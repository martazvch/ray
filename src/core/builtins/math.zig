const std = @import("std");
const zffi = @import("../ffi/zffi.zig");
const Value = @import("../runtime/values.zig").Value;
const Vm = @import("../runtime/Vm.zig");

const Module = @This();

pub const module: zffi.Module = .{
    .name = "math",
    .functions = &.{
        .init(Module, "cos", "", &.{.{ .name = "value" }}),
    },
};

pub fn cos(_: *Vm, value: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (value) {
        .int => |i| @cos(@floatFromInt(i)),
        .float => |f| @cos(f),
    };
}
