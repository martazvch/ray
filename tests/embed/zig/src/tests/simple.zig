const Body = @import("../tester.zig").Body;

pub const body1: Body = .{
    .code = "print 1",
    .res = "1",
};

pub const body2: Body = .{
    .code = "print -2",
    .res = "-2",
};
