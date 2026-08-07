const std = @import("std");
const ray = @import("ray");
const Tester = @import("tester.zig");

fn isLess(_: *ray.Vm, a: i64, b: i64) bool {
    return a < b;
}

pub fn main(init: std.process.Init) !void {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    defer {
        std.debug.assert(debug_allocator.deinit() == .ok);
    }

    var arena = std.heap.ArenaAllocator.init(debug_allocator.allocator());
    defer arena.deinit();

    try Tester.testDir(init.io, arena.allocator(), "../cases");
}

test {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    try Tester.testDir(std.testing.io, arena.allocator(), "../cases");
}
