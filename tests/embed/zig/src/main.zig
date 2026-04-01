const std = @import("std");
const ray = @import("ray");
const Tester = @import("tester.zig");

fn isLess(_: *ray.Vm, a: i64, b: i64) bool {
    return a < b;
}

pub fn main() !void {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    defer {
        std.debug.assert(debug_allocator.deinit() == .ok);
    }

    var vm = ray.create(debug_allocator.allocator());
    defer vm.deinit();

    vm.init(
        .{
            .embedded = true,
            .print_ir = true,
            .print_bytecode = true,
        },
    );

    try vm.run("print 14");
    try vm.run("print float(14)");
    try vm.run("print isLess(14, 15)");
    try vm.run("print isLess(15, 14)");
}

test {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    try Tester.testDir(arena.allocator(), ".." ++ std.fs.path.sep_str ++ "cases");
}
