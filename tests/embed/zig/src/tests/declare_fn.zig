const Body = @import("../tester.zig").Body;

pub const body1: Body = .{
    .code =
    \\fn add(x, y: int = 8) -> int {
    \\    return x + y
    \\}
    ,
};

pub const body2: Body = .{
    .code = "print add(1, 2)",
    .res = "3",
};

pub const body3: Body = .{
    .code = "print add()",
    .res = "16",
};
