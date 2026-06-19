const std = @import("std");
const zffi = @import("../ffi/zffi.zig");
const Value = @import("../runtime/values.zig").Value;
const Vm = @import("../runtime/Vm.zig");

const Module = @This();

pub const module: zffi.Module = .{
    .is_module = false,
    .functions = &.{
        .init(Module, "int", "", &.{
            .{ .name = "value" },
        }),
        .init(Module, "float", "", &.{
            .{ .name = "value" },
        }),
        .init(Module, "str", "", &.{
            .{ .name = "value" },
        }),
    },
};

pub fn int(_: *Vm, value: zffi.Union(&.{ .int, .float, .str, .@"enum" })) zffi.Int {
    return switch (value) {
        .int => |i| i,
        .float => |f| @intFromFloat(f),
        .str => |s| std.fmt.parseInt(zffi.Int, s, 10) catch unreachable,
        .@"enum" => |e| e.tag_id,
    };
}

pub fn float(_: *Vm, value: zffi.Union(&.{ .int, .float, .str })) zffi.Float {
    return switch (value) {
        .int => |i| @floatFromInt(i),
        .float => |f| f,
        .str => |s| std.fmt.parseFloat(zffi.Float, s) catch unreachable,
    };
}

pub fn str(vm: *Vm, value: zffi.Union(&.{ .int, .float })) zffi.Str {
    errdefer unreachable;

    return switch (value) {
        .int => |i| try std.fmt.allocPrint(vm.gc_alloc, "{}", .{i}),
        .float => |f| try std.fmt.allocPrint(vm.gc_alloc, "{}", .{f}),
    };
}
