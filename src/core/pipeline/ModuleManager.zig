const std = @import("std");
const Allocator = std.mem.Allocator;

const SymbolArrMap = @import("../analyzer/LexicalScope.zig").SymbolArrMap;
const Value = @import("../runtime/values.zig").Value;

const misc = @import("misc");
const InternerIndex = misc.Interner.Index;
const oom = misc.oom;

const Self = @This();

pub const Module = struct {
    path: InternerIndex,
    name: InternerIndex,
    /// Type infos gathered by the analyzer
    sym_infos: SymbolArrMap,
    /// Compiled values/symbols used at runtime
    globals: []Value,
    symbols: []Value,
    constants: []Value,

    pub const empty: Module = .{
        .path = undefined,
        .name = undefined,
        .sym_infos = .empty,
        .globals = &.{},
        .symbols = &.{},
        .constants = &.{},
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

pub fn ensureCompileSizes(
    self: *Self,
    allocator: Allocator,
    index: Index,
    globals: usize,
    symbols: usize,
    constants: usize,
) void {
    errdefer oom();
    const mod = self.getFromIndex(index);

    mod.globals = try allocator.realloc(mod.globals, globals);
    mod.symbols = try allocator.realloc(mod.symbols, symbols);
    mod.constants = try allocator.realloc(mod.constants, constants);
}

pub fn addGlobal(self: *Self, mod: Index, index: usize, value: Value) void {
    self.getFromIndex(mod).globals[index] = value;
}

pub fn addSymbol(self: *Self, mod: Index, index: usize, value: Value) void {
    self.getFromIndex(mod).symbols[index] = value;
}

pub fn addConstant(self: *Self, mod: Index, index: usize, value: Value) void {
    self.getFromIndex(mod).constants[index] = value;
}

pub fn getSymbol(self: *const Self, mod: Index, sym_index: usize) Value {
    return self.getFromIndex(mod).symbols[sym_index];
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
