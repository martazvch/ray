const std = @import("std");
const Allocator = std.mem.Allocator;
const LexScope = @import("../analyzer/LexicalScope.zig");
const Symbol = LexScope.Symbol;
const SymbolTypeMap = LexScope.SymbolTypeMap;
const Type = @import("../analyzer/types.zig").Type;
const oom = @import("misc").oom;

symbols: std.AutoHashMapUnmanaged(*const Type, Symbol),

/// Symbol table registers all the accessible symbols from other analyzed modules
/// It is useful in cases where we just have the expected type, for example when
/// calling an imported function, its arguments' type might be symbols declared
/// in their modules and thus we have to be able to reach for symbol information
/// (mostly symbol and module index) to be able to compile a load_symbol instruction
const Self = @This();

pub const empty: Self = .{
    .symbols = .empty,
};

pub fn addFrom(self: *Self, alloc: Allocator, other: *const SymbolTypeMap) void {
    self.symbols.ensureUnusedCapacity(alloc, other.count()) catch oom();
    var it = other.iterator();
    while (it.next()) |entry| {
        const sym = entry.value_ptr.*;
        self.symbols.putAssumeCapacity(sym.type, sym);
    }
}

pub fn get(self: *const Self, ty: *const Type) ?Symbol {
    return self.symbols.get(ty);
}
