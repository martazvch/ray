const std = @import("std");
const Allocator = std.mem.Allocator;

const SymbolArrMap = @import("../analyzer/LexicalScope.zig").SymbolArrMap;
const Value = @import("../runtime/values.zig").Value;
const Obj = @import("../runtime/Obj.zig");
const State = @import("../pipeline/State.zig");
const TypeId = @import("../analyzer/types.zig").TypeId;

const misc = @import("misc");
const InternerIndex = misc.Interner.Index;
const oom = misc.oom;

const Self = @This();

pub const Module = struct {
    path: InternerIndex,
    name: InternerIndex,
    /// Type infos gathered by the analyzer
    // TODO: used for what?
    sym_infos: SymbolArrMap,
    /// Compiled values/symbols used at runtime
    globals: []Value,
    constants: []Value,

    enums: []Enum,
    unions: []Union,
    functions: []*Obj.Function,
    foreign_funcs: []*Obj.CFn,
    structures: []Structure,

    pub const Enum = struct {
        name: []const u8,
        tags: []const []const u8,
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

    pub const empty: Module = .{
        .path = undefined,
        .name = undefined,
        .sym_infos = .empty,
        .globals = &.{},
        .constants = &.{},
        .enums = &.{},
        .unions = &.{},
        .functions = &.{},
        .foreign_funcs = &.{},
        .structures = &.{},
    };
};

// TODO: make a utils tool because it's a common pattern
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

pub fn open(self: *Self, allocator: Allocator, path: InternerIndex, name: InternerIndex) Index {
    if (self.getIndex(path)) |index| {
        return index;
    }

    var mod: Module = .empty;
    mod.path = path;
    mod.name = name;
    self.modules.put(allocator, path, mod) catch oom();

    return .toIndex(self.modules.count() - 1);
}

pub fn updateWithSymsInfo(self: *Self, allocator: Allocator, index: Index, symbols: SymbolArrMap) void {
    var mod = self.getFromIndex(index);
    mod.sym_infos.ensureUnusedCapacity(allocator, symbols.count()) catch oom();

    for (symbols.values()) |sym| {
        mod.sym_infos.putAssumeCapacity(sym.name, sym);
    }
}

pub fn ensureCompileSizes(self: *Self, allocator: Allocator, index: Index, state: *const State) void {
    errdefer oom();
    const mod = self.getFromIndex(index);

    // We use realloc because of REPL mode that keeps defining symbols in current module
    mod.globals = try allocator.realloc(mod.globals, state.lex_scope.current.variables.count());
    mod.constants = try allocator.realloc(mod.constants, state.const_interner.constants.items.len);
    mod.enums = try allocator.realloc(mod.enums, state.lex_scope.enum_count);
    mod.unions = try allocator.realloc(mod.unions, state.lex_scope.union_count);
    mod.functions = try allocator.realloc(mod.functions, state.lex_scope.func_count);
    mod.structures = try allocator.realloc(mod.structures, state.lex_scope.struct_count);
}

pub fn addGlobal(self: *Self, mod: Index, index: usize, value: Value) void {
    self.getFromIndex(mod).globals[index] = value;
}

pub fn addSymbol(self: *Self, mod_index: Index, sym_index: usize, value: anytype) void {
    const module = self.getFromIndex(mod_index);
    const array = switch (@TypeOf(value)) {
        Module.Enum => module.enums,
        Module.Union => module.unions,
        *Obj.Function => module.functions,
        Module.Structure => module.structures,
        else => @compileError("Can only add symbols defined in compiled module, found " ++ @typeName(@TypeOf(value))),
    };
    array[sym_index] = value;
}

pub fn getSymbol(self: *const Self, mod_index: Index, sym_index: usize, comptime kind: enum { @"enum", structure, @"union" }) *const switch (kind) {
    .@"enum" => Module.Enum,
    .structure => Module.Structure,
    .@"union" => Module.Union,
} {
    const mod = self.getFromIndex(mod_index);
    return switch (kind) {
        .@"enum" => &mod.enums[sym_index],
        .structure => &mod.structures[sym_index],
        .@"union" => &mod.unions[sym_index],
    };
}

pub fn addConstant(self: *Self, mod: Index, index: usize, value: Value) void {
    self.getFromIndex(mod).constants[index] = value;
}

pub fn getFromIndex(self: *const Self, index: Index) *Module {
    return &self.modules.values()[index.toInt()];
}

pub fn getFromName(self: *Self, name: InternerIndex) ?*Module {
    return self.modules.getPtr(name);
}

pub fn getIndex(self: *const Self, name: InternerIndex) ?Index {
    const index = self.modules.getIndex(name) orelse return null;
    return Index.toIndex(index);
}

pub fn has(self: *const Self, name: InternerIndex) bool {
    return self.modules.contains(name);
}
