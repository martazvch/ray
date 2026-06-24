const std = @import("std");
const Analyzer = @import("../analyzer/Analyzer.zig");
const Type = @import("../analyzer/types.zig").Type;
const zffi = @import("../ffi/zffi.zig");

pub const functions: []const zffi.Intrinsic = &.{
    .{
        .name = "typeName",
        .func = typeName,
        .params = &.{
            .{ .name = "value", .type = anyopaque },
        },
        .return_type = []const u8,
    },
};

pub fn typeName(ana: *Analyzer, ty: *const Type, offset: usize) usize {
    return ana.addConstant(
        .{ .string = ana.interner.intern(ana.typeName(ty)) },
        offset,
    );
}
