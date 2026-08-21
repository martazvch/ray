const Writer = @import("std").Io.Writer;

pub const LexerMsg = union(enum) {
    leading_zeroes,
    invalid_float_digit: struct { digit: u8 },
    invalid_int_digit: struct { digit: u8 },
    unterminated_str,
    unexpected_char,

    const Self = @This();

    pub fn getMsg(self: Self, writer: *Writer) !void {
        try switch (self) {
            .leading_zeroes => writer.writeAll("leading zeros in integer literals are not allowed"),
            .invalid_float_digit => |e| writer.print("invalid float digit '{c}'", .{e.digit}),
            .invalid_int_digit => |e| writer.print("invalid int digit '{c}'", .{e.digit}),
            .unterminated_str => writer.writeAll("unterminated string"),
            .unexpected_char => writer.writeAll("unexpected character"),
        };
    }

    pub fn getHelp(self: Self, writer: *Writer) !void {
        try switch (self) {
            .leading_zeroes => writer.writeAll("remove the leading zeros"),
            .unterminated_str => writer.writeAll("close the opening quote"),
            else => {},
        };
    }
};
