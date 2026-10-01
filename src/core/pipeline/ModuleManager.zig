const std = @import("std");
const Allocator = std.mem.Allocator;

const LexScope = @import("../analyzer/LexicalScope.zig");
const SymbolMap = LexScope.SymbolMap;
const VariableMap = LexScope.VariableMap;
const Value = @import("../runtime/values.zig").Value;
const Obj = @import("../runtime/Obj.zig");
const NativeMod = @import("NativesRegister.zig").NativeModule;

const misc = @import("misc");
const InternerIndex = misc.Interner.Index;
const oom = misc.oom;

modules: std.AutoArrayHashMapUnmanaged(InternerIndex, Module),

const Self = @This();

pub const Module = struct {
    path: InternerIndex,
    name: InternerIndex,
    index: Index,
    native: bool,

    /// Type infos gathered by the analyzer used when importing a module
    /// It has all the analyzis-time data to type check
    sym_infos: SymbolMap = .empty,
    globals_infos: VariableMap = .empty,
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
    self.registerSymsInfo(allocator, index, &native_mod.zig_funcs);
    self.registerGlobalsInfo(allocator, index, &native_mod.globals);
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
