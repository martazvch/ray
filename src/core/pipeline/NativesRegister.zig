const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const cffi = @import("../ffi/cffi.zig");
const zffi = @import("../ffi/zffi.zig");
const MapNameType = @import("../analyzer/types.zig").MapNameType;
const Type = @import("../analyzer/types.zig").Type;
const TypeInterner = @import("../analyzer/types.zig").TypeInterner;
const LexScope = @import("../analyzer/LexicalScope.zig");
const Symbol = LexScope.Symbol;
const SymbolMap = LexScope.SymbolMap;
const Variable = LexScope.Variable;
const VariableMap = LexScope.VariableMap;

const Module = @import("ModuleManager.zig").Module;
const Value = @import("../runtime/values.zig").Value;
const Obj = @import("../runtime/Obj.zig");
const Vm = @import("../runtime/Vm.zig");

const misc = @import("misc");
const Interner = misc.Interner;
const oom = misc.oom;

pub const NativeModule = struct {
    path: []const u8,
    index: usize,

    // Globals
    globals: ArrayList(Value) = .empty,
    globals_meta: VariableMap = .empty,

    /// Native Zig functions used at runtime
    zig_fns: ArrayList(*Obj.ZigFn) = .empty,
    /// Native Zig functions translated to Ray's type system for compilation
    zig_fns_meta: Meta = .empty,

    /// Extern functions used at runtime
    extern_fns: ArrayList(*Obj.ExternFn) = .empty,
    /// Extern functions translated to Ray's type system for compilation
    extern_fns_meta: Meta = .empty,

    /// Native structures used at runtime
    zig_structs: ArrayList(Module.Structure) = .empty,
    /// Native structures translated to Ray's type system for compilation
    zig_structs_meta: Meta = .empty,
    /// Native structures translated to Ray's type system used here for self references
    scratch_structs: Meta = .empty,
};

mods: std.AutoArrayHashMapUnmanaged(Interner.Index, NativeModule),
current: *NativeModule,

/// Intrinsic functions called during Analyzis pass
intrinsics: std.AutoHashMapUnmanaged(Interner.Index, zffi.IntrinsicFn),
intrinsics_meta: Meta,

const Self = @This();
pub const Meta = SymbolMap;

pub fn init(self: *Self, alloc: Allocator, interner: *Interner) void {
    self.mods = .empty;
    self.mods.put(alloc, interner.intern("@global"), .{ .path = "@global", .index = 0 }) catch oom();
    self.current = undefined;
    self.intrinsics = .empty;
    self.intrinsics_meta = .empty;
}

pub fn registerMod(self: *Self, alloc: Allocator, interner: *Interner, ti: *TypeInterner, Mod: type) void {
    if (!@hasDecl(Mod, "module")) {
        @compileError("Native Zig files must declare a module");
    }

    const mod = @field(Mod, "module");
    if (@TypeOf(mod) != zffi.Module) {
        @compileError("Native Zig module's 'module' variable must be of type " ++ @typeName(zffi.Module));
    }

    if (mod.name) |name| {
        const gop = self.mods.getOrPut(alloc, interner.intern(name)) catch oom();

        if (gop.found_existing) {
            // TODO: error
            @panic("Already declared native module");
        }

        gop.value_ptr.* = .{ .path = name, .index = self.mods.count() - 1 };
        self.current = gop.value_ptr;
    } else {
        self.current = self.getGlobalScope();
    }

    self.current.zig_structs.ensureUnusedCapacity(alloc, mod.structures.len) catch oom();
    self.current.zig_structs_meta.ensureUnusedCapacity(alloc, mod.structures.len) catch oom();
    inline for (mod.structures) |s| {
        self.registerStruct(alloc, s, interner, ti);
    }

    inline for (mod.functions) |func| {
        _ = self.registerZigFn(alloc, &func, interner, ti);
    }

    inline for (mod.globals) |cte| {
        _ = self.registerGlobal(alloc, cte, interner, ti);
    }
}

pub fn getGlobalScope(self: *Self) *NativeModule {
    return &self.mods.values()[0];
}

// TODO: Errors
fn registerStruct(self: *Self, alloc: Allocator, comptime zstruct: zffi.StructMeta, interner: *Interner, ti: *TypeInterner) void {
    // TODO: handle container name properly
    const struct_name = interner.intern(zstruct.name);
    var s: Type.Structure = .{
        .loc = .{ .name = struct_name, .container = interner.intern("std") },
        .fields = .empty,
        .functions = .empty,
        .traits = .empty,
        .native = true,
    };
    s.fields.ensureTotalCapacity(alloc, zstruct.fields.len) catch oom();
    s.functions.ensureTotalCapacity(alloc, zstruct.functions.len) catch oom();

    const ty = ti.intern(.{ .structure = s });
    const sym: Symbol = .{
        .name = struct_name,
        .type = ty,
        .index = self.current.zig_structs_meta.count(),
        .module = .toIndex(self.current.index),
    };
    self.current.scratch_structs.put(
        alloc,
        interner.intern(zstruct.type_name),
        sym,
    ) catch oom();

    inline for (zstruct.functions) |*func| {
        const interned_name = interner.intern(func.name);
        if (ty.structure.functions.contains(interned_name)) {
            @panic("Already declared function");
        }

        const reg = self.registerZigFn(alloc, func, interner, ti);
        ty.structure.functions.putAssumeCapacity(
            interned_name,
            .{
                .name = interned_name,
                .index = reg.index,
                .type = reg.type,
                .module = .toIndex(self.current.index),
            },
        );
    }

    inline for (zstruct.fields) |field| {
        const interned_name = interner.intern(field.name);
        if (ty.structure.fields.contains(interned_name)) {
            @panic("Already declared field");
        }
        ty.structure.fields.putAssumeCapacity(interned_name, .{
            .default = null,
            .type = self.zigToRay(alloc, field.type, interner, ti),
        });
    }

    self.current.zig_structs.appendAssumeCapacity(.{
        .name = zstruct.name,
        .type_id = ti.typeId(ty),
        .field_count = zstruct.fields.len,
    });

    const gop = self.current.zig_structs_meta.getOrPutAssumeCapacity(struct_name);
    if (gop.found_existing) {
        @panic("Already declared with same name");
    }
    gop.value_ptr.* = sym;
}

const Registered = struct {
    index: usize,
    type: *const Type,
};

/// Used by embedded, always registers in global scope when not registerating via a module
pub fn registerZigFnInGlobal(self: *Self, alloc: Allocator, comptime func: *const zffi.FnMeta, interner: *Interner, ti: *TypeInterner) Registered {
    self.current = self.getGlobalScope();
    return self.registerZigFn(alloc, func, interner, ti);
}

// We can use pointers here because we refer to comptime declarations in Module
fn registerZigFn(self: *Self, alloc: Allocator, comptime func: *const zffi.FnMeta, interner: *Interner, ti: *TypeInterner) Registered {
    const fn_type = self.fnZigToRay(alloc, func, interner, ti);
    const fn_name = interner.intern(func.name);
    const gop = self.current.zig_fns_meta.getOrPut(alloc, fn_name) catch oom();

    // TODO: Error
    if (gop.found_existing) {
        @panic("Already defined one with same name");
    }
    gop.value_ptr.* = .{
        .name = fn_name,
        .type = fn_type,
        .index = self.current.zig_fns.items.len,
        .module = .toIndex(self.current.index),
    };

    self.current.zig_fns.append(alloc, .create(alloc, func.name, func.function)) catch oom();

    return .{ .index = self.current.zig_fns.items.len - 1, .type = fn_type };
}

// TODO: Errors
fn fnZigToRay(self: *Self, alloc: Allocator, comptime func: *const zffi.FnMeta, interner: *Interner, ti: *TypeInterner) *const Type {
    var params: Type.Function.ParamsMap = .empty;

    // We don't take into account param *Vm and if it's in second place, it means 'self' is in first and we skip it too
    const offset = if (func.info.params.len > 1 and func.info.params[1].type.? == *Vm) 2 else 1;
    params.ensureTotalCapacity(alloc, func.info.params.len - offset) catch oom();

    inline for (func.info.params[offset..], 0..) |*p, i| {
        const param_ty = self.zigToRay(alloc, p.type.?, interner, ti);

        const gop = params.getOrPutAssumeCapacity(interner.intern(func.params[i].name));
        if (gop.found_existing) {
            @panic("Already declared param with same name");
        }
        gop.value_ptr.* = .{
            .type = param_ty,
            .default = null,
            .captured = false,
        };
    }

    // TODO: handle container name properly
    const ty: Type.Function = .{
        .kind = if (offset == 2) .zig_method else .zig,
        .loc = .{ .name = interner.intern(func.name), .container = interner.intern("std") },
        .return_type = self.zigToRay(alloc, func.info.return_type.?, interner, ti),
        .params = params,
    };

    return ti.intern(.{ .function = ty });
}

// TODO: Errors
pub fn registerIntrinsics(self: *Self, alloc: Allocator, interner: *Interner, ti: *TypeInterner, Mod: type) void {
    const funcs = @field(Mod, "functions");
    if (@TypeOf(funcs) != []const zffi.Intrinsic) {
        @compileError("Intrinsic functions variable must be of type []const " ++ @typeName(zffi.Intrinsic));
    }

    self.intrinsics.ensureUnusedCapacity(alloc, funcs.len) catch oom();
    self.intrinsics_meta.ensureUnusedCapacity(alloc, funcs.len) catch oom();

    inline for (funcs) |*func| {
        const fn_name = interner.intern(func.name);

        const gop = self.intrinsics_meta.getOrPutAssumeCapacity(fn_name);
        if (gop.found_existing) {
            @panic("Already decalred with same name");
        }

        const fn_type = self.fnIntrinsicToRay(alloc, func, interner, ti);
        self.intrinsics.putAssumeCapacity(fn_name, func.func);
        gop.value_ptr.* = .{
            .name = fn_name,
            .type = fn_type,
            .index = self.intrinsics.count() - 1,
            .module = .toIndex(self.current.index),
        };
    }
}

// TODO: Errors
fn fnIntrinsicToRay(
    self: *Self,
    alloc: Allocator,
    comptime func: *const zffi.Intrinsic,
    interner: *Interner,
    ti: *TypeInterner,
) *const Type {
    var params: Type.Function.ParamsMap = .empty;
    params.ensureTotalCapacity(alloc, func.params.len) catch oom();

    inline for (func.params) |p| {
        const param_ty = self.zigToRay(alloc, p.type, interner, ti);

        const gop = params.getOrPutAssumeCapacity(interner.intern(p.name));
        if (gop.found_existing) {
            @panic("Already declared parameter");
        }
        gop.value_ptr.* = .{
            .type = param_ty,
            .default = null,
            .captured = false,
        };
    }

    // TODO: handle container name properly
    const ty: Type.Function = .{
        .kind = .intrinsic,
        .loc = .{ .name = interner.intern(func.name), .container = interner.intern("compiler") },
        .return_type = self.zigToRay(alloc, func.return_type, interner, ti),
        .params = params,
    };

    return ti.intern(.{ .function = ty });
}

fn zigToRay(self: *Self, alloc: Allocator, T: type, interner: *Interner, ti: *TypeInterner) *const Type {
    return switch (T) {
        // `anyopaque` is used to represent `any` in Ray
        anyopaque => ti.getCached(.any),
        bool => ti.getCached(.bool),
        i64 => ti.getCached(.int),
        f64 => ti.getCached(.float),
        void => ti.getCached(.void),
        []const u8 => ti.getCached(.str),
        else => switch (@typeInfo(T)) {
            .@"union" => |u| {
                var childs = ArrayList(*const Type).initCapacity(alloc, u.fields.len) catch oom();
                inline for (u.fields) |f| {
                    childs.appendAssumeCapacity(self.zigToRay(alloc, f.type, interner, ti));
                }
                return ti.intern(.{ .inline_union = .{ .types = childs.toOwnedSlice(alloc) catch oom() } });
            },
            .pointer => |ptr| switch (ptr.child) {
                Obj.EnumInstance => ti.getCached(.IsEnum),
                else => self.zigToRay(alloc, ptr.child, interner, ti),
            },
            .@"struct" => {
                if (self.current.scratch_structs.get(interner.intern(@typeName(T)))) |t| {
                    return t.type;
                }

                // TODO: error, occurs when everything isn't correctly registered inside the file
                // like a static function returning a type not declared in the structures of the module
                unreachable;
            },
            else => @compileError("Zig to Ray type conversion not supported for type: " ++ @typeName(T)),
        },
    };
}

/// Declares in global scope, used when not declaring via a module
pub fn registerExternFnInGlobal(self: *Self, alloc: Allocator, proto: *const cffi.FnProto, interner: *Interner, ti: *TypeInterner) Registered {
    self.current = self.getGlobalScope();
    return self.registerExternFn(alloc, proto, interner, ti);
}

fn registerExternFn(self: *Self, alloc: Allocator, proto: *const cffi.FnProto, interner: *Interner, ti: *TypeInterner) Registered {
    const fn_type = externFnToRay(alloc, proto, interner, ti);
    const name_str = std.mem.span(proto.name);
    const fn_name = interner.intern(name_str);
    self.current.extern_fns_meta.put(alloc, fn_name, .{
        .name = fn_name,
        .type = fn_type,
        .index = self.current.extern_fns_meta.count(),
        .module = .toIndex(self.current.index),
    }) catch oom();
    const native = Obj.ExternFn.create(alloc, name_str, proto.func, proto.return_type != .void);

    self.current.extern_fns.append(alloc, native) catch oom();

    return .{ .index = self.current.extern_fns.items.len - 1, .type = fn_type };
}

pub fn externFnToRay(alloc: Allocator, proto: *const cffi.FnProto, interner: *Interner, ti: *TypeInterner) *const Type {
    var params: Type.Function.ParamsMap = .empty;
    params.ensureTotalCapacity(alloc, proto.params.len - 1) catch oom();

    for (proto.params[0..@as(usize, @intCast(proto.arity))]) |*p| {
        const param_ty = cTypeToRay(p.ty, ti);
        const param_name = interner.intern(std.mem.span(p.name));

        params.putAssumeCapacity(
            param_name,
            .{
                .type = param_ty,
                .default = null,
                .captured = false,
            },
        );
    }

    // TODO: handle container name properly
    const ty: Type.Function = .{
        .kind = .@"extern",
        .loc = .{
            .name = interner.intern(std.mem.span(proto.name)),
            .container = interner.intern("std"),
        },
        .return_type = cTypeToRay(proto.return_type, ti),
        .params = params,
    };

    return ti.intern(.{ .function = ty });
}

fn cTypeToRay(ty: cffi.cType, ti: *TypeInterner) *const Type {
    return switch (ty) {
        .void => ti.getCached(.void),
        .int => ti.getCached(.int),
        .float => ti.getCached(.float),
        .bool => ti.getCached(.bool),
    };
}

fn registerGlobal(self: *Self, alloc: Allocator, global: zffi.Global, interner: *Interner, ti: *TypeInterner) void {
    const name = interner.intern(global.name);
    const gop = self.current.globals_meta.getOrPut(alloc, name) catch oom();
    if (gop.found_existing) {
        @panic("Already declared global in module");
    }

    gop.value_ptr.* = .{
        .name = name,
        .type = switch (global.value) {
            .bool => ti.getCached(.bool),
            .float => ti.getCached(.float),
            .int => ti.getCached(.int),
            else => unreachable,
        },
        .index = self.current.globals_meta.count() - 1,
        .comp_time = true,
        .constant = true,
        .initialized = true,
        .kind = .global,
        .captured = false,
        .ext_mod = .toIndex(self.current.index),
    };

    self.current.globals.append(alloc, global.value) catch oom();
}
