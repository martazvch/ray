const std = @import("std");
const zffi = @import("../ffi/zffi.zig");
const Value = @import("../runtime/values.zig").Value;
const Vm = @import("../runtime/Vm.zig");

const Module = @This();

pub const module: zffi.Module = .{
    .name = "std",
    .modules = &.{
        @import("math.zig").module,
    },
};
