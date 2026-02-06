const Body = @import("../tester.zig").Body;

pub const body1: Body = .{
    .code = "let a = 65",
};

pub const body2: Body = .{
    .code = "print a",
    .res = "65",
};
