const std = @import("std");
const ray = @import("ray");
const Tester = @import("tester.zig");

pub fn main() !void {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    defer {
        std.debug.assert(debug_allocator.deinit() == .ok);
    }

    try Tester.testMod(debug_allocator.allocator(), @import("tests/test1.zig"));
}

test {
    try Tester.testMod(std.testing.allocator, @import("tests/test1.zig"));
}
