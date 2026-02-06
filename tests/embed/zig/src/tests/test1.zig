const Body = @import("../tester.zig").Body;

pub const body1: Body = .{
    .code = "print 1",
    .res = "1",
};

pub const body2: Body = .{
    .code = "print -2",
    .res = "-2",
};

pub const body3: Body = .{
    .code = "let a = 65",
};

pub const body4: Body = .{
    .code = "print a",
    .res = "65",
};

pub const body5: Body = .{
    .code =
    \\fn add(x, y: int = 8) -> int {
    \\    return x + y
    \\}
    ,
};

pub const body6: Body = .{
    .code = "print add(1, 2)",
    .res = "3",
};

pub const body7: Body = .{
    .code = "print add()",
    .res = "16",
};
