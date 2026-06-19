const ray = @import("ray");

const Module = @This();

pub const functions: []const ray.RayFn = &.{
    .init(@This(), "isLess", "", &.{
        .{ .name = "a" },
        .{ .name = "b" },
    }),
};

pub fn isLess(_: *ray.Vm, a: i64, b: i64) bool {
    return a < b;
}
