const std = @import("std");
const Allocator = std.mem.Allocator;
const oom = @import("misc").oom;
const Artifacts = @import("Artifacts.zig");

table: Table,

pub const Key = struct {
    tag: Artifacts.Tag,
    module: usize,
    symbol: usize,
};
pub const Table = std.AutoHashMapUnmanaged(Key, usize);

pub const Self = @This();
pub const empty: Self = .{ .table = .empty };

pub fn put(self: *Self, alloc: Allocator, key: Key, value: usize) void {
    self.table.put(alloc, key, value) catch oom();
}

pub fn get(self: *const Self, key: Key) usize {
    return self.table.get(key).?;
}
