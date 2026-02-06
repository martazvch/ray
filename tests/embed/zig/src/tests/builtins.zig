const Body = @import("../tester.zig").Body;

pub const body1: Body = .{
    .code = "print int(5.6)",
    .res = "5",
};

pub const body2: Body = .{
    .code = "print int(\"5\")",
    .res = "5",
};

pub const body3: Body = .{
    .code = "print float(5)",
    .res = "5",
};

pub const body4: Body = .{
    .code = "print float(\"5.8\")",
    .res = "5.8",
};

pub const body5: Body = .{
    .code = "print str(68)",
    .res = "68",
};

pub const body6: Body = .{
    .code = "print str(68.12)",
    .res = "68.12",
};
