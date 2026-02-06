const Body = @import("../tester.zig").Body;
const ray = @import("ray");

fn isLess(_: *ray.Vm, a: i64, b: i64) bool {
    return a < b;
}

pub const natives: []const ray.RayFn = &.{
    .init("isLess", isLess, "", &.{
        .{ .name = "a" },
        .{ .name = "b" },
    }),
};

pub const body1: Body = .{
    .code = "print isLess(14, 15)",
    .res = "true",
};

pub const body2: Body = .{
    .code = "print isLess(15, 14)",
    .res = "false",
};
