const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const zffi = @import("../ffi/zffi.zig");
const ffi = @import("../ffi/ffi.zig");
const MapNameType = @import("../analyzer/types.zig").MapNameType;
const Type = @import("../analyzer/types.zig").Type;
const TypeInterner = @import("../analyzer/types.zig").TypeInterner;

const Value = @import("../runtime/values.zig").Value;
const Obj = @import("../runtime/Obj.zig");
const Vm = @import("../runtime/Vm.zig");

const misc = @import("misc");
const Interner = misc.Interner;
const oom = misc.oom;

/// Native Zig functions used at runtime
zig_fns: ArrayList(*Obj.ZigFn),
/// Native Zig functions translated to Ray's type system for compilation
zig_fns_meta: std.AutoArrayHashMapUnmanaged(Interner.Index, *const Type),
/// Native structures used at runtime
structs: ArrayList(struct { name: []const u8, func: Value }),
/// Native structures translated to Ray's type system for compilation
structs_meta: std.AutoArrayHashMapUnmanaged(Interner.Index, *const Type),
/// Native structures translated to Ray's type system used here for self references
scratch_structs: std.AutoArrayHashMapUnmanaged(Interner.Index, *const Type),

/// Foreign functions used at runtime
foreign_fns: ArrayList(*Obj.ForeignFn),
/// Foreign functions translated to Ray's type system for compilation
foreign_fns_meta: std.AutoArrayHashMapUnmanaged(Interner.Index, *const Type),

const Self = @This();

pub const empty: Self = .{
    .zig_fns = .empty,
    .zig_fns_meta = .empty,
    .structs = .empty,
    .structs_meta = .empty,
    .scratch_structs = .empty,
    .foreign_fns = .empty,
    .foreign_fns_meta = .empty,
};

pub fn registerMod(self: *Self, allocator: Allocator, interner: *Interner, ti: *TypeInterner, Module: type) void {
    if (!@hasDecl(Module, "module")) {
        @compileError("Native Zig files must declare a module");
    }

    const mod = @field(Module, "module");
    if (@TypeOf(mod) != zffi.Module) {
        @compileError("Native Zig module's 'module' variable must be of type " ++ @typeName(zffi.Module));
    }

    inline for (mod.structures) |s| {
        self.registerStruct(allocator, s, interner, ti);
    }

    inline for (mod.functions) |func| {
        _ = self.registerZigFn(allocator, &func, interner, ti);
    }
}

fn registerStruct(self: *Self, allocator: Allocator, S: type, interner: *Interner, ti: *TypeInterner) void {
    if (!@hasDecl(S, "zig_struct")) {
        @compileError("Native structures must declare a public 'zig_struct' constant");
    }

    const zig_struct = @field(S, "zig_struct");
    if (@TypeOf(zig_struct) != zffi.StructMeta) {
        @compileError("zig_struct constant must be of type: " ++ @typeName(zffi.StructMeta));
    }

    // TODO: handle container name properly
    const struct_name = interner.intern(zig_struct.name);
    var s: Type.Structure = .{
        .loc = .{ .name = struct_name, .container = interner.intern("std") },
        .fields = .empty,
        .functions = .empty,
        .traits = .empty,
    };
    s.fields.ensureTotalCapacity(allocator, zig_struct.fields.len) catch oom();
    s.functions.ensureTotalCapacity(allocator, zig_struct.functions.len) catch oom();

    const ty = ti.intern(.{ .structure = s });
    self.scratch_structs.put(allocator, interner.intern(@typeName(S)), ty) catch oom();

    inline for (zig_struct.functions) |*func| {
        const interned_name = interner.intern(func.name);
        if (ty.structure.functions.contains(interned_name)) {
            // TODO: error
            @panic("Already declared function");
        }

        const reg = self.registerZigFn(allocator, func, interner, ti);
        ty.structure.functions.putAssumeCapacity(
            interned_name,
            .{ .name = interned_name, .index = reg.index, .type = reg.type },
        );
    }

    // TODO: use assume capacity (check all the 'put')
    self.structs_meta.put(allocator, struct_name, ty) catch oom();
}

const Registered = struct {
    index: usize,
    type: *const Type,
};
// We can use pointers here because we refer to comptime declarations in Module
// TODO: no check on already defined with same name?
pub fn registerZigFn(self: *Self, allocator: Allocator, func: *const zffi.FnMeta, interner: *Interner, ti: *TypeInterner) Registered {
    const fn_type = self.fnZigToRay(allocator, func, interner, ti);
    self.zig_fns_meta.put(allocator, interner.intern(func.name), fn_type) catch oom();
    const native = Obj.ZigFn.create(allocator, func.name, func.function);

    self.zig_fns.append(allocator, native) catch oom();

    return .{ .index = self.zig_fns.items.len - 1, .type = fn_type };
}

fn fnZigToRay(self: *Self, allocator: Allocator, func: *const zffi.FnMeta, interner: *Interner, ti: *TypeInterner) *const Type {
    var params: Type.Function.ParamsMap = .empty;

    // We don't take into account param *Vm and if it's in second place, it means 'self' is in first and we skip it too
    const offset: usize = if (func.info.params.len > 1 and func.info.params[1].type.? == *Vm) 2 else 1;

    params.ensureTotalCapacity(allocator, func.info.params.len - offset) catch oom();

    inline for (func.info.params[offset..], 0..) |*p, i| {
        const param_ty = self.zigToRay(allocator, p.type.?, interner, ti);

        params.putAssumeCapacity(
            interner.intern(func.params[i].name),
            .{ .name = null, .type = param_ty, .default = null, .captured = false },
        );
    }

    // TODO: handle container name properly
    const ty: Type.Function = .{
        .kind = if (offset == 2) .zig_method else .zig,
        .loc = .{ .name = interner.intern(func.name), .container = interner.intern("std") },
        .return_type = self.zigToRay(allocator, func.info.return_type.?, interner, ti),
        .params = params,
    };

    return ti.intern(.{ .function = ty });
}

fn zigToRay(self: *Self, allocator: Allocator, ty: type, interner: *Interner, ti: *TypeInterner) *const Type {
    return switch (ty) {
        bool => ti.getCached(.bool),
        i64 => ti.getCached(.int),
        f64 => ti.getCached(.float),
        void => ti.getCached(.void),
        []const u8 => ti.getCached(.str),
        else => switch (@typeInfo(ty)) {
            .@"union" => |u| {
                var childs = ArrayList(*const Type).initCapacity(allocator, u.fields.len) catch oom();
                inline for (u.fields) |f| {
                    childs.appendAssumeCapacity(self.zigToRay(allocator, f.type, interner, ti));
                }
                return ti.intern(.{ .inline_union = .{ .types = childs.toOwnedSlice(allocator) catch oom() } });
            },
            .pointer => |ptr| switch (ptr.child) {
                Obj => unreachable,
                else => |C| self.zigToRay(allocator, C, interner, ti),
            },
            .@"struct" => {
                if (self.scratch_structs.get(interner.intern(@typeName(ty)))) |t| {
                    return t;
                }

                // TODO: error, occurs when everything isn't correctly registered inside the file
                // like a static function returning a type not declared in the structures of the module
                unreachable;
            },
            else => @compileError("Zig to Ray type conversion not supported for type: " ++ @typeName(ty)),
        },
    };
}

pub fn registerForeignFn(self: *Self, allocator: Allocator, proto: *const ffi.FnProto, interner: *Interner, ti: *TypeInterner) Registered {
    const fn_type = foreignFnToRay(allocator, proto, interner, ti);
    const name = std.mem.span(proto.name);
    self.foreign_fns_meta.put(allocator, interner.intern(name), fn_type) catch oom();
    const native = Obj.ForeignFn.create(allocator, name, proto.func, proto.return_type != .void);

    self.foreign_fns.append(allocator, native) catch oom();

    return .{ .index = self.foreign_fns.items.len - 1, .type = fn_type };
}

pub fn foreignFnToRay(allocator: Allocator, proto: *const ffi.FnProto, interner: *Interner, ti: *TypeInterner) *const Type {
    var params: Type.Function.ParamsMap = .empty;
    params.ensureTotalCapacity(allocator, proto.params.len - 1) catch oom();

    for (proto.params[0..@as(usize, @intCast(proto.arity))]) |*p| {
        const param_ty = cTypeToRay(p.ty, ti);
        const param_name = interner.intern(std.mem.span(p.name));

        params.putAssumeCapacity(
            param_name,
            .{
                .name = param_name,
                .type = param_ty,
                .default = null,
                .captured = false,
            },
        );
    }

    // TODO: handle container name properly
    const ty: Type.Function = .{
        .kind = .foreign_glob,
        .loc = .{
            .name = interner.intern(std.mem.span(proto.name)),
            .container = interner.intern("std"),
        },
        .return_type = cTypeToRay(proto.return_type, ti),
        .params = params,
    };

    return ti.intern(.{ .function = ty });
}

fn cTypeToRay(ty: ffi.cType, ti: *TypeInterner) *const Type {
    return switch (ty) {
        .void => ti.getCached(.void),
        .int => ti.getCached(.int),
        .float => ti.getCached(.float),
        .bool => ti.getCached(.bool),
    };
}
