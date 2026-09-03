const std = @import("std");
const Allocator = std.mem.Allocator;

const LexScope = @import("../analyzer/LexicalScope.zig");
const SymbolMap = LexScope.SymbolMap;
const VariableMap = LexScope.VariableMap;
const Value = @import("../runtime/values.zig").Value;
const Obj = @import("../runtime/Obj.zig");
const State = @import("../pipeline/State.zig");
const TypeId = @import("../analyzer/types.zig").TypeId;
const NativeMod = @import("NativesRegister.zig").NativeModule;

const misc = @import("misc");
const InternerIndex = misc.Interner.Index;
const oom = misc.oom;

const Self = @This();

pub const Module = struct {
    path: InternerIndex,
    name: InternerIndex,
    index: Index,
    native: bool,

    /// Compiled values used at runtime
    globals: []Value = &.{},
    /// Compiled constants used at runtime
    constants: []Value = &.{},

    /// Type infos gathered by the analyzer used when importing a module
    /// It has all the analyzis-time data to type check
    sym_infos: SymbolMap = .empty,
    globals_infos: VariableMap = .empty,

    /// Compiled objects
    enums: []Enum = &.{},
    unions: []Union = &.{},
    funcs: []*Obj.Function = &.{},
    structs: []Structure = &.{},

    zig_funcs: []*Obj.ZigFn = &.{},
    c_funcs: std.ArrayList(*Obj.CFn) = .empty,
    vtables: []VTable = &.{},

    pub const Enum = struct {
        name: []const u8,
        tags: []const []const u8,
        discriminants: []const i64,
        type_id: TypeId,
    };

    pub const Union = struct {
        name: []const u8,
        tags: []const []const u8,
        type_id: TypeId,
        is_err: bool,
    };

    pub const Structure = struct {
        name: []const u8,
        type_id: TypeId,
        field_count: usize,
    };

    pub const VTable = struct {
        name: []const u8,
        functions: []*Obj.Function,
    };
};

pub const Index = enum(usize) {
    _,

    pub fn toIndex(i: usize) Index {
        return @enumFromInt(i);
    }

    pub fn toInt(index: Index) usize {
        return @intFromEnum(index);
    }
};

modules: std.AutoArrayHashMapUnmanaged(InternerIndex, Module),

pub const empty: Self = .{
    .modules = .empty,
};

pub fn open(self: *Self, allocator: Allocator, path: InternerIndex, name: InternerIndex, native: bool) Index {
    const gop = self.modules.getOrPut(allocator, path) catch oom();
    if (gop.found_existing) {
        return gop.value_ptr.index;
    }

    const index: Index = .toIndex(self.modules.count() - 1);
    gop.value_ptr.* = .{
        .name = name,
        .path = path,
        .native = native,
        .index = index,
    };

    return index;
}

/// Adds symbols informations to module so that other module can have type informations when importing
/// symbols from this one
pub fn registerSymsInfo(self: *Self, allocator: Allocator, index: Index, symbols: *const SymbolMap) void {
    var mod = self.getFromIndex(index);
    mod.sym_infos.ensureUnusedCapacity(allocator, symbols.count()) catch oom();

    var it = symbols.iterator();
    while (it.next()) |entry| {
        mod.sym_infos.putAssumeCapacity(entry.value_ptr.name, entry.value_ptr.*);
    }
}

/// Adds symbols informations to module so that other module can have type informations when importing
/// symbols from this one
pub fn registerGlobalsInfo(self: *Self, allocator: Allocator, index: Index, globals: *const VariableMap) void {
    var mod = self.getFromIndex(index);
    mod.globals_infos.ensureUnusedCapacity(allocator, @intCast(globals.count())) catch oom();

    var it = globals.iterator();
    while (it.next()) |entry| {
        mod.globals_infos.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.*);
    }
}

/// After creating a native module, we have both compiled functions and symbols informations
/// Adds the informations and the compiled objects
pub fn registerSymsFromNativeMod(self: *Self, allocator: Allocator, index: Index, native_mod: *const NativeMod) void {
    self.registerSymsInfo(allocator, index, &native_mod.zig_funcs_meta);
    self.registerGlobalsInfo(allocator, index, &native_mod.globals_meta);

    const mod = self.getFromIndex(index);
    mod.globals = native_mod.globals.items;
    mod.zig_funcs = native_mod.zig_funcs.items;
    mod.c_funcs = native_mod.c_funcs;
}

/// Used between analyzis and compilation as we know the exact number of symbols
pub fn ensureCompileSizes(self: *Self, allocator: Allocator, index: Index, state: *const State) void {
    const mod = self.getFromIndex(index);

    errdefer oom();
    // We use realloc because of REPL mode that keeps defining symbols in current module
    mod.globals = try allocator.realloc(mod.globals, state.lex_scope.current.variables.count());
    mod.constants = try allocator.realloc(mod.constants, state.const_interner.constants.items.len);
    mod.enums = try allocator.realloc(mod.enums, state.lex_scope.enum_count);
    mod.unions = try allocator.realloc(mod.unions, state.lex_scope.union_count);
    mod.funcs = try allocator.realloc(mod.funcs, state.lex_scope.func_count);
    mod.structs = try allocator.realloc(mod.structs, state.lex_scope.struct_count);
    mod.vtables = try allocator.realloc(mod.vtables, state.lex_scope.vtable_count);
}

pub fn setGlobal(self: *Self, module_index: Index, value_index: usize, value: Value) void {
    self.getFromIndex(module_index).globals[value_index] = value;
}

pub fn getGlobal(self: *const Self, mod: Index, index: usize) Value {
    return self.getFromIndex(mod).globals[index];
}

pub fn setSymbol(self: *Self, module_index: Index, sym_index: usize, value: anytype) void {
    const module = self.getFromIndex(module_index);
    const array = switch (@TypeOf(value)) {
        Module.Enum => module.enums,
        Module.Union => module.unions,
        *Obj.Function => module.funcs,
        Module.Structure => module.structs,
        else => @compileError("Can only add symbols defined in compiled module, found " ++ @typeName(@TypeOf(value))),
    };
    array[sym_index] = value;
}

pub fn addCFn(self: *Self, alloc: Allocator, mod_index: Index, value: *Obj.CFn) void {
    const module = self.getFromIndex(mod_index);
    module.c_funcs.append(alloc, value) catch oom();
}

pub fn getSymbol(
    self: *const Self,
    mod_index: Index,
    sym_index: usize,
    comptime kind: enum {
        @"enum",
        function,
        c_func,
        zig_func,
        structure,
        @"union",
    },
) switch (kind) {
    .@"enum" => *const Module.Enum,
    .function => *Obj.Function,
    .c_func => *Obj.CFn,
    .zig_func => *Obj.ZigFn,
    .structure => *const Module.Structure,
    .@"union" => *const Module.Union,
} {
    const mod = self.getFromIndex(mod_index);
    return switch (kind) {
        .@"enum" => &mod.enums[sym_index],
        .function => mod.funcs[sym_index],
        .c_func => mod.c_funcs.items[sym_index],
        .zig_func => mod.zig_funcs[sym_index],
        .structure => &mod.structs[sym_index],
        .@"union" => &mod.unions[sym_index],
    };
}

pub fn setConstant(self: *Self, mod: Index, index: usize, value: Value) void {
    self.getFromIndex(mod).constants[index] = value;
}

pub fn getConstant(self: *const Self, mod: Index, index: usize) Value {
    return self.getFromIndex(mod).constants[index];
}

pub fn getFromIndex(self: *const Self, index: Index) *Module {
    return &self.modules.values()[index.toInt()];
}

pub fn getFromPath(self: *Self, path: InternerIndex) ?*Module {
    return self.modules.getPtr(path);
}

pub fn has(self: *const Self, name: InternerIndex) bool {
    return self.modules.contains(name);
}
