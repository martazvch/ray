pub const CompilerMsg = union(enum) {
    const Self = @This();

    pub fn getMsg(self: Self, writer: anytype) !void {
        _ = self;
        _ = writer;
        // try switch (self) {
        //     .implicit_cast => writer.print("implicit cast", .{}),
        // };
    }

    pub fn getHelp(self: Self, writer: anytype) !void {
        _ = self;
        _ = writer;
        // try switch (self) {
        //     .implicit_cast => writer.print("unterminated string", .{}),
        //     else => {},
        // };
    }
};
