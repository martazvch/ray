const std = @import("std");
const ffi = @import("ffi.zig");
const Value = @import("../runtime/values.zig").Value;
const Vm = @import("../runtime/Vm.zig");

pub const module: ffi.ZigModule = .{
    .is_module = false,
    .functions = &.{
        .init("int", int, "", &.{
            .{ .name = "value" },
        }),
        .init("float", float, "", &.{
            .{ .name = "value" },
        }),
        .init("str", str, "", &.{
            .{ .name = "value" },
        }),
    },
};

fn int(_: *Vm, value: ffi.Union(&.{ .int, .float, .str })) ffi.Int {
    return switch (value) {
        .int => |i| i,
        .float => |f| @intFromFloat(f),
        .str => |s| std.fmt.parseInt(ffi.Int, s, 10) catch unreachable,
    };
}

fn float(_: *Vm, value: ffi.Union(&.{ .int, .float, .str })) ffi.Float {
    return switch (value) {
        .int => |i| @floatFromInt(i),
        .float => |f| f,
        .str => |s| std.fmt.parseFloat(ffi.Float, s) catch unreachable,
    };
}

fn str(vm: *Vm, value: ffi.Union(&.{ .int, .float })) ffi.Str {
    errdefer unreachable;

    return switch (value) {
        .int => |i| try std.fmt.allocPrint(vm.gc_alloc, "{}", .{i}),
        .float => |f| try std.fmt.allocPrint(vm.gc_alloc, "{}", .{f}),
    };
}
