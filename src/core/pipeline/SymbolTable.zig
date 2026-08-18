const std = @import("std");
const Allocator = std.mem.Allocator;
const LexScope = @import("../analyzer/LexicalScope.zig");
const Symbol = LexScope.Symbol;
const SymbolMap = LexScope.SymbolMap;
const Type = @import("../analyzer/types.zig").Type;
const oom = @import("misc").oom;

symbols: std.AutoHashMapUnmanaged(*const Type, Symbol),

const Self = @This();

pub const empty: Self = .{
    .symbols = .empty,
};

pub fn addFrom(self: *Self, alloc: Allocator, other: *const SymbolMap) void {
    var it = other.iterator();
    while (it.next()) |entry| {
        const sym = entry.value_ptr.*;
        const gop = self.symbols.getOrPut(alloc, sym.type) catch oom();

        // TODO: real error
        if (gop.found_existing) {
            return;
        }

        gop.value_ptr.* = sym;
    }
}

pub fn get(self: *const Self, ty: *const Type) ?Symbol {
    return self.symbols.get(ty);
}
