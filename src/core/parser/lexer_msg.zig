const Writer = @import("std").Io.Writer;

pub const LexerMsg = union(enum) {
    base_prefix_uppercase: struct { base: u8 },
    expect_digit_after_base,
    leading_zeroes,
    invalid_float_digit: struct { digit: u8 },
    invalid_int_digit: struct { digit: u8 },
    invalid_int_binary: struct { digit: u8 },
    invalid_int_hexa: struct { digit: u8 },
    invalid_int_octal: struct { digit: u8 },
    repeated_digit_separator,
    trailing_digit_separator,
    unterminated_str,
    unexpected_char,

    const Self = @This();

    pub fn getMsg(self: Self, writer: *Writer) !void {
        try switch (self) {
            .base_prefix_uppercase => |e| writer.print(
                "base prefix must be lower case, change {c} to {c}",
                .{ e.base, e.base - 32 },
            ),
            .expect_digit_after_base => writer.writeAll("expect a digit after base prefix"),
            .leading_zeroes => writer.writeAll("leading zeros in integer literals are not allowed"),
            .invalid_float_digit => |e| writer.print("invalid float digit '{c}'", .{e.digit}),
            .invalid_int_digit => |e| writer.print("invalid int digit '{c}'", .{e.digit}),
            .invalid_int_binary => |e| writer.print("invalid digit '{c}' for binary base", .{e.digit}),
            .invalid_int_hexa => |e| writer.print("invalid digit '{c}' for hexadecimal base", .{e.digit}),
            .invalid_int_octal => |e| writer.print("invalid digit '{c}' for octal base", .{e.digit}),
            .repeated_digit_separator => writer.writeAll("found multiple '_', only one is allowed"),
            .trailing_digit_separator => writer.writeAll("no trailing digit separator '_' allowed"),
            .unterminated_str => writer.writeAll("unterminated string"),
            .unexpected_char => writer.writeAll("unexpected character"),
        };
    }

    pub fn getHelp(self: Self, writer: *Writer) !void {
        try switch (self) {
            .leading_zeroes => writer.writeAll("remove the leading zeros"),
            .invalid_int_binary => writer.writeAll("only digits 0 and 1 are allowed in binary lierals"),
            .invalid_int_hexa => writer.writeAll("only digits from 0 to 9 and characters from A to F are allowed in hexadecimal lierals"),
            .invalid_int_octal => writer.writeAll("only digits from 0 to 7 are allowed in octal lierals"),
            .unterminated_str => writer.writeAll("close the opening quote"),
            else => {},
        };
    }
};
