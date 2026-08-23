const std = @import("std");
const ArrayList = std.ArrayList;
const MultiArrayList = std.MultiArrayList;
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

const LexerMsg = @import("lexer_msg.zig").LexerMsg;

const misc = @import("misc");
const GenReport = misc.reporter.GenReport;
const oom = misc.oom;

source: [:0]const u8,
index: usize,
tokens: std.MultiArrayList(Token),
errs: ArrayList(LexerReport),
allocator: Allocator,

const Self = @This();
pub const LexerReport = GenReport(LexerMsg);

pub const Span = struct {
    start: usize,
    end: usize,

    pub const zero: Span = .{ .start = 0, .end = 0 };

    pub fn text(self: *const Span, source: []const u8) []const u8 {
        return source[self.start..self.end];
    }
};

pub const Token = struct {
    tag: Tag,
    span: Span,

    const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "and", .@"and" },
        .{ "as", .as },
        .{ "break", .@"break" },
        .{ "continue", .@"continue" },
        .{ "defer", .@"defer" },
        .{ "do", .do },
        .{ "else", .@"else" },
        .{ "enum", .@"enum" },
        .{ "error", .@"error" },
        .{ "extern", .@"extern" },
        .{ "fail", .fail },
        .{ "false", .false },
        .{ "fn", .@"fn" },
        .{ "for", .@"for" },
        .{ "if", .@"if" },
        .{ "impl", .impl },
        .{ "in", .in },
        .{ "is", .is },
        .{ "let", .let },
        .{ "match", .match },
        .{ "not", .not },
        .{ "null", .null },
        .{ "or", .@"or" },
        .{ "print", .print },
        .{ "return", .@"return" },
        .{ "self", .self },
        .{ "struct", .@"struct" },
        .{ "trait", .trait },
        .{ "trap", .trap },
        .{ "true", .true },
        .{ "union", .@"union" },
        .{ "use", .use },
        .{ "var", .@"var" },
        .{ "while", .@"while" },
    });

    pub const Tag = enum {
        ampersand,
        ampersand_equal,
        @"and",
        arrow_small,
        arrow_big,
        as,
        at,
        bang,
        bang_bang,
        bang_equal,
        @"break",
        colon,
        comma,
        @"continue",
        @"defer",
        do,
        dot,
        dot_dot,
        dot_dot_dot,
        dot_question_mark,
        dot_star,
        @"else",
        @"enum",
        eof,
        equal,
        equal_equal,
        @"error",
        @"extern",
        fail,
        false,
        float,
        @"fn",
        @"for",
        greater,
        greater_equal,
        greater_greater,
        greater_greater_equal,
        hat,
        hat_equal,
        identifier,
        @"if",
        impl,
        in,
        int,
        is,
        left_brace,
        left_bracket,
        left_paren,
        less,
        less_equal,
        less_less,
        less_less_equal,
        let,
        match,
        minus,
        minus_equal,
        modulo,
        modulo_equal,
        new_line,
        not,
        null,
        @"or",
        pipe,
        pipe_equal,
        plus,
        plus_equal,
        print,
        question_mark,
        question_mark_question_mark,
        @"return",
        right_brace,
        right_bracket,
        right_paren,
        self,
        slash,
        slash_equal,
        star,
        star_equal,
        string,
        @"struct",
        tilde,
        trait,
        trap,
        true,
        underscore,
        @"union",
        use,
        @"var",
        @"while",

        base_prefix_uppercase,
        expect_digit_before_dot,
        expect_digit_before_separator,
        expect_digit_after_base,
        leading_zeroes,
        invalid_float_digit,
        invalid_int_digit,
        invalid_int_binary,
        invalid_int_hexa,
        invalid_int_octal,
        repeated_digit_separator,
        trailing_digit_separator,
        unterminated_str,
        unexpected_char,
    };

    pub fn getKeyword(ident: []const u8) ?Tag {
        return keywords.get(ident);
    }
};

const State = enum {
    bang,
    comment,
    dot,
    dot_dot,
    equal,
    float,
    float_scient,
    greater,
    identifier,
    int,
    int_binary,
    int_hexa,
    int_octal,
    invalid,
    less,
    question_mark,
    slash,
    start,
    string,
    string_escape,
};

pub fn init(allocator: Allocator) Self {
    return .{
        .source = undefined,
        .index = 0,
        .tokens = .{},
        .errs = .empty,
        .allocator = allocator,
    };
}

pub fn deinit(self: *Self) void {
    self.tokens.deinit(self.allocator);
    self.errs.deinit(self.allocator);
}

pub fn lex(self: *Self, source: [:0]const u8) void {
    self.source = source;

    while (true) {
        const tk = self.next();

        // TODO: redo this part. As we lex every thing at once, use arraylist for
        // errors like parser, analyzer, ...? Or use compitme to associate both sides
        switch (tk.tag) {
            .base_prefix_uppercase => self.errorAt(.{ .base_prefix_uppercase = .{ .base = source[tk.span.start] } }, tk),
            .expect_digit_before_dot => self.errorAt(.expect_digit_before_dot, tk),
            .expect_digit_before_separator => self.errorAt(.expect_digit_before_separator, tk),
            .expect_digit_after_base => self.errorAt(.expect_digit_after_base, tk),
            .leading_zeroes => self.errorAt(.leading_zeroes, tk),

            .invalid_float_digit => self.errorAt(.{ .invalid_float_digit = .{ .digit = source[tk.span.start] } }, tk),
            .invalid_int_digit => self.errorAt(.{ .invalid_int_digit = .{ .digit = source[tk.span.start] } }, tk),
            .invalid_int_binary => self.errorAt(.{ .invalid_int_binary = .{ .digit = source[tk.span.start] } }, tk),
            .invalid_int_hexa => self.errorAt(.{ .invalid_int_hexa = .{ .digit = source[tk.span.start] } }, tk),
            .invalid_int_octal => self.errorAt(.{ .invalid_int_octal = .{ .digit = source[tk.span.start] } }, tk),
            .repeated_digit_separator => self.errorAt(.repeated_digit_separator, tk),
            .trailing_digit_separator => self.errorAt(.trailing_digit_separator, tk),
            .unterminated_str => self.errorAt(.unterminated_str, tk),
            .unexpected_char => self.errorAt(.unexpected_char, tk),
            else => self.tokens.append(self.allocator, tk) catch oom(),
        }

        if (tk.tag == .eof) break;
    }
}

fn errorAt(self: *Self, tag: LexerMsg, token: Token) void {
    const report = LexerReport.err(tag, token.span.start, token.span.end);
    self.errs.append(self.allocator, report) catch oom();
}

pub fn next(self: *Self) Token {
    var res = Token{
        .tag = undefined,
        .span = .{
            .start = self.index,
            .end = undefined,
        },
    };

    state: switch (State.start) {
        .start => {
            switch (self.current()) {
                'a'...'z', 'A'...'Z' => {
                    res.tag = .identifier;
                    continue :state .identifier;
                },
                ' ', '\t', '\r' => {
                    self.advance();
                    res.span.start = self.index;
                    continue :state .start;
                },
                '(' => {
                    res.tag = .left_paren;
                    self.advance();
                },
                ')' => {
                    res.tag = .right_paren;
                    self.advance();
                },
                '{' => {
                    res.tag = .left_brace;
                    self.advance();
                },
                '}' => {
                    res.tag = .right_brace;
                    self.advance();
                },
                '[' => {
                    res.tag = .left_bracket;
                    self.advance();
                },
                ']' => {
                    res.tag = .right_bracket;
                    self.advance();
                },
                ',' => {
                    res.tag = .comma;
                    self.advance();
                },
                '+' => {
                    self.advance();
                    if (self.current() == '=') {
                        self.advance();
                        res.tag = .plus_equal;
                    } else res.tag = .plus;
                },
                '-' => {
                    self.advance();

                    switch (self.current()) {
                        '>' => {
                            self.advance();
                            res.tag = .arrow_small;
                        },
                        '=' => {
                            self.advance();
                            res.tag = .minus_equal;
                        },
                        else => res.tag = .minus,
                    }
                },
                '*' => {
                    self.advance();

                    if (self.current() == '=') {
                        self.advance();
                        res.tag = .star_equal;
                    } else res.tag = .star;
                },
                '%' => {
                    self.advance();

                    switch (self.current()) {
                        '=' => {
                            self.advance();
                            res.tag = .modulo_equal;
                        },
                        else => res.tag = .modulo,
                    }
                },
                '/' => continue :state .slash,
                '\n' => {
                    res.tag = .new_line;
                    self.advance();
                },
                ':' => {
                    res.tag = .colon;
                    self.advance();
                },
                '<' => continue :state .less,
                '>' => continue :state .greater,
                '!' => continue :state .bang,
                '=' => continue :state .equal,
                '.' => continue :state .dot,
                '?' => continue :state .question_mark,
                '"' => {
                    res.tag = .string;
                    continue :state .string;
                },
                '&' => {
                    self.advance();
                    if (self.current() == '=') {
                        self.advance();
                        res.tag = .ampersand_equal;
                    } else res.tag = .ampersand;
                },
                '|' => {
                    self.advance();
                    if (self.current() == '=') {
                        self.advance();
                        res.tag = .pipe_equal;
                    } else res.tag = .pipe;
                },
                '~' => {
                    res.tag = .tilde;
                    self.advance();
                },
                '^' => {
                    self.advance();
                    if (self.current() == '=') {
                        self.advance();
                        res.tag = .hat_equal;
                    } else res.tag = .hat;
                },
                '0' => {
                    if (self.checkAt(1, '.')) {
                        if (self.checkAt(2, '.')) {
                            // Range syntax like: 0..5
                            res.tag = .int;
                            self.advance();
                        } else {
                            self.index += 2;
                            continue :state .float;
                        }
                    } else if (std.ascii.isAlphabetic((self.source[self.index + 1]))) {
                        self.advance();

                        switch (self.current()) {
                            'b' => {
                                res.tag = .int;
                                self.advance();
                                continue :state .int_binary;
                            },
                            'x' => {
                                res.tag = .int;
                                self.advance();
                                continue :state .int_hexa;
                            },
                            'o' => {
                                res.tag = .int;
                                self.advance();
                                continue :state .int_octal;
                            },
                            'B', 'X', 'O' => return self.currentToken(.base_prefix_uppercase),
                            else => return self.currentToken(.invalid_int_digit),
                        }
                    } else {
                        self.advance();
                        switch (self.current()) {
                            '0'...'9' => return tokenAt(.leading_zeroes, .{
                                .start = self.index - 1,
                                .end = self.index,
                            }),
                            else => res.tag = .int,
                        }
                    }
                },
                '1'...'9' => {
                    res.tag = .int;
                    self.advance();
                    continue :state .int;
                },
                '_' => {
                    self.advance();
                    switch (self.current()) {
                        'a'...'z', 'A'...'Z', '0'...'9', '_' => {
                            res.tag = .identifier;
                            continue :state .identifier;
                        },
                        else => res.tag = .underscore,
                    }
                },
                '@' => {
                    self.advance();
                    res.tag = .at;
                },
                0 => {
                    if (self.index == self.source.len) {
                        return self.currentToken(.eof);
                    } else continue :state .invalid;
                },
                else => {
                    res.tag = .unexpected_char;
                    self.advance();
                },
            }
        },
        .bang => {
            self.advance();

            switch (self.current()) {
                '=' => {
                    res.tag = .bang_equal;
                    self.advance();
                },
                '!' => {
                    res.tag = .bang_bang;
                    self.advance();
                },
                else => res.tag = .bang,
            }
        },
        .question_mark => {
            self.advance();

            switch (self.current()) {
                '?' => {
                    res.tag = .question_mark_question_mark;
                    self.advance();
                },
                else => res.tag = .question_mark,
            }
        },
        .comment => {
            self.advance();

            switch (self.current()) {
                0 => res.tag = .eof,
                '\n' => continue :state .start,
                else => continue :state .comment,
            }
        },
        .dot => {
            self.advance();

            switch (self.current()) {
                '0'...'9' => {
                    self.advance();
                    continue :state .float;
                },
                '.' => continue :state .dot_dot,
                '*' => {
                    res.tag = .dot_star;
                    self.advance();
                },
                '?' => {
                    res.tag = .dot_question_mark;
                    self.advance();
                },
                else => res.tag = .dot,
            }
        },
        .dot_dot => {
            self.advance();

            switch (self.current()) {
                '.' => {
                    res.tag = .dot_dot_dot;
                    self.advance();
                },
                else => res.tag = .dot_dot,
            }
        },
        .equal => {
            self.advance();

            switch (self.current()) {
                '=' => {
                    res.tag = .equal_equal;
                    self.advance();
                },
                '>' => {
                    self.advance();
                    res.tag = .arrow_big;
                },
                else => res.tag = .equal,
            }
        },
        .float => {
            if (self.skipDigitSep()) |err| {
                return err;
            }

            switch (self.current()) {
                '0'...'9' => {
                    self.advance();
                    continue :state .float;
                },
                'e' => {
                    self.advance();
                    if (self.current() == '-' or self.current() == '+') {
                        self.advance();
                    }
                    continue :state .float_scient;
                },
                'E' => return self.currentToken(.base_prefix_uppercase),
                'a'...'d', 'f'...'z', 'A'...'D', 'F'...'Z' => return self.currentToken(.invalid_float_digit),
                else => res.tag = .float,
            }

            if (self.checkTrailingDigitSep()) |err| {
                return err;
            }
        },
        .float_scient => {
            switch (self.current()) {
                '0'...'9' => {
                    self.advance();
                    continue :state .float;
                },
                'a'...'z', 'A'...'Z' => return self.currentToken(.invalid_float_digit),
                else => res.tag = .float,
            }

            if (self.prev() == 'e' or self.prev() == '+' or self.prev() == '-') {
                return self.prevToken(.expect_digit_after_base);
            }

            if (self.checkTrailingDigitSep()) |err| {
                return err;
            }
        },
        .greater => {
            self.advance();

            switch (self.current()) {
                '=' => {
                    res.tag = .greater_equal;
                    self.advance();
                },
                '>' => {
                    self.advance();
                    res.tag = .greater_greater;

                    if (self.current() == '=') {
                        self.advance();
                        res.tag = .greater_greater_equal;
                    }
                },
                else => res.tag = .greater,
            }
        },
        .identifier => {
            self.advance();

            switch (self.current()) {
                'a'...'z', 'A'...'Z', '_', '0'...'9' => continue :state .identifier,
                else => {
                    const ident = self.source[res.span.start..self.index];

                    if (Token.getKeyword(ident)) |kw| {
                        res.tag = kw;
                    }
                },
            }
        },
        .int => {
            if (self.skipDigitSep()) |err| {
                return err;
            }

            switch (self.current()) {
                '0'...'9' => {
                    self.advance();
                    continue :state .int;
                },
                'e' => {
                    self.advance();
                    if (self.current() == '-' or self.current() == '+') {
                        self.advance();
                    }
                    continue :state .float_scient;
                },
                'E' => return self.currentToken(.base_prefix_uppercase),
                '.' => {
                    if (self.checkAt(1, '.')) {
                        // Range syntax: 1..3
                    } else {
                        if (self.prev() == '_') {
                            return self.prevToken(.expect_digit_before_dot);
                        }
                        self.advance();
                        continue :state .float;
                    }
                },
                'a'...'d', 'f'...'z', 'A'...'D', 'F'...'Z' => return self.currentToken(.invalid_int_digit),
                else => {},
            }

            if (self.checkTrailingDigitSep()) |err| {
                return err;
            }
        },
        .int_binary => {
            if (self.skipDigitSep()) |err| {
                return err;
            }

            switch (self.current()) {
                '0', '1' => {
                    self.advance();
                    continue :state .int_binary;
                },
                '2'...'9', 'a'...'z', 'A'...'Z' => return self.currentToken(.invalid_int_binary),
                else => {},
            }

            if (self.checkDigitAfterBaseAndTrailingSep('b')) |err| {
                return err;
            }
        },
        .int_hexa => {
            if (self.skipDigitSep()) |err| {
                return err;
            }

            switch (self.current()) {
                '0'...'9', 'a'...'f', 'A'...'F' => {
                    self.advance();
                    continue :state .int_hexa;
                },
                'g'...'z', 'G'...'Z' => return self.currentToken(.invalid_int_hexa),
                else => {},
            }

            if (self.checkDigitAfterBaseAndTrailingSep('x')) |err| {
                return err;
            }
        },
        .int_octal => {
            if (self.skipDigitSep()) |err| {
                return err;
            }

            switch (self.current()) {
                '0'...'7' => {
                    self.advance();
                    continue :state .int_octal;
                },
                '8'...'9', 'a'...'z', 'A'...'Z' => return self.currentToken(.invalid_int_octal),
                else => {},
            }

            if (self.checkDigitAfterBaseAndTrailingSep('o')) |err| {
                return err;
            }
        },
        .invalid => {
            self.advance();

            switch (self.current()) {
                0 => {
                    if (self.index == self.source.len) {
                        res.tag = .eof;
                    } else continue :state .invalid;
                },
                ' ' => res.tag = .@"error",
                else => continue :state .invalid,
            }
        },
        .less => {
            self.advance();

            switch (self.current()) {
                '=' => {
                    res.tag = .less_equal;
                    self.advance();
                },
                '<' => {
                    self.advance();
                    res.tag = .less_less;

                    if (self.current() == '=') {
                        self.advance();
                        res.tag = .less_less_equal;
                    }
                },
                else => res.tag = .less,
            }
        },
        .slash => {
            self.advance();

            switch (self.current()) {
                '/' => continue :state .comment,
                '=' => {
                    self.advance();
                    res.tag = .slash_equal;
                },
                else => res.tag = .slash,
            }
        },
        .string => {
            self.advance();

            switch (self.current()) {
                0 => {
                    if (self.index == self.source.len) {
                        // For error reporting, one byte length
                        return tokenAt(.unterminated_str, .{
                            .start = res.span.start,
                            .end = res.span.start + 1,
                        });
                    }
                },
                '"' => self.advance(),
                '\\' => continue :state .string_escape,
                else => continue :state .string,
            }
        },
        .string_escape => {
            self.advance();

            switch (self.current()) {
                0 => res.tag = .eof,
                '\\' => {
                    self.advance();
                    continue :state .string;
                },
                else => continue :state .string,
            }
        },
    }

    res.span.end = self.index;
    return res;
}

inline fn advance(self: *Self) void {
    self.index += 1;
}

inline fn current(self: *const Self) u8 {
    return self.source[self.index];
}

inline fn prev(self: *const Self) u8 {
    return self.source[self.index - 1];
}

/// Creates specified token with current character's span
fn currentToken(self: *const Self, tag: Token.Tag) Token {
    return .{
        .tag = tag,
        .span = self.currentSpan(),
    };
}

/// Creates specified token with current character's span
fn prevToken(self: *const Self, tag: Token.Tag) Token {
    return .{
        .tag = tag,
        .span = .{ .start = self.index - 1, .end = self.index - 1 },
    };
}

/// Returns span of current character
fn currentSpan(self: *const Self) Span {
    return .{
        .start = self.index,
        .end = self.index,
    };
}

/// Creates specified token with current character's span
fn tokenAt(tag: Token.Tag, span: Span) Token {
    return .{
        .tag = tag,
        .span = span,
    };
}

fn checkAt(self: *const Self, deepth: usize, char: u8) bool {
    return self.index < self.source.len + deepth and self.source[self.index + deepth] == char;
}

fn skipDigitSep(self: *Self) ?Token {
    if (self.current() == '_') {
        if (!std.ascii.isAlphanumeric(self.prev())) {
            return self.prevToken(.expect_digit_before_separator);
        }
        self.advance();
    }

    if (self.source[self.index] == '_') {
        return self.currentToken(.repeated_digit_separator);
    }

    return null;
}

fn checkDigitAfterBaseAndTrailingSep(self: *const Self, base: u8) ?Token {
    if (self.checkDigitAfterBase(base)) |err| {
        return err;
    }
    if (self.checkTrailingDigitSep()) |err| {
        return err;
    }
    return null;
}

fn checkDigitAfterBase(self: *const Self, base: u8) ?Token {
    if (self.prev() == base) {
        return self.currentToken(.expect_digit_after_base);
    }

    return null;
}

fn checkTrailingDigitSep(self: *const Self) ?Token {
    if (self.prev() == '_') {
        return self.prevToken(.trailing_digit_separator);
    }

    return null;
}

// ------------
//  Tests
// ------------
test "ident and strings" {
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex("foo bar variable  truth");

    const res = [_]Token{
        .{ .tag = .identifier, .span = .{ .start = 0, .end = 3 } },
        .{ .tag = .identifier, .span = .{ .start = 4, .end = 7 } },
        .{ .tag = .identifier, .span = .{ .start = 8, .end = 16 } },
        .{ .tag = .identifier, .span = .{ .start = 18, .end = 23 } },
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        const span = lexer.tokens.items(.span)[i];

        try expect(tag == res[i].tag);
        try expect(span.start == res[i].span.start);
        try expect(span.end == res[i].span.end);
    }
}

test "numbers" {
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex("123 45.6 7. .86");

    const res = [_]Token{
        .{ .tag = .int, .span = .{ .start = 0, .end = 3 } },
        .{ .tag = .float, .span = .{ .start = 4, .end = 8 } },
        .{ .tag = .float, .span = .{ .start = 9, .end = 11 } },
        .{ .tag = .float, .span = .{ .start = 12, .end = 15 } },
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        const span = lexer.tokens.items(.span)[i];

        try expect(tag == res[i].tag);
        try expect(span.start == res[i].span.start);
        try expect(span.end == res[i].span.end);
    }
}

test "tokens" {
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex("(){}.:,=!< ><= >= !=+-*/ += -= *= /= [] @ % %= !! ? ?? & | ~ ^ << >> &= |= ^= <<= >>=");

    const res = [_]Token.Tag{
        .left_paren,      .right_paren, .left_brace,   .right_brace,     .dot,                   .colon,
        .comma,           .equal,       .bang,         .less,            .greater,               .less_equal,
        .greater_equal,   .bang_equal,  .plus,         .minus,           .star,                  .slash,
        .plus_equal,      .minus_equal, .star_equal,   .slash_equal,     .left_bracket,          .right_bracket,
        .at,              .modulo,      .modulo_equal, .bang_bang,       .question_mark,         .question_mark_question_mark,
        .ampersand,       .pipe,        .tilde,        .hat,             .less_less,             .greater_greater,
        .ampersand_equal, .pipe_equal,  .hat_equal,    .less_less_equal, .greater_greater_equal,
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        try expect(tag == res[i]);
    }
}

test "keywords" {
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex(
        \\\and else false for fn if null or print return 
        \\\self struct true var while not do use break 
        \\\as enum match let in fail trap is continue trait impl
        \\\union extern defer
    );

    const res = [_]Token.Tag{
        .@"and",    .@"else",  .false,       .@"for",    .@"fn",   .@"if",    .null,     .@"or",     .print,
        .@"return", .new_line, .self,        .@"struct", .true,    .@"var",   .@"while", .not,       .do,
        .use,       .@"break", .new_line,    .as,        .@"enum", .match,    .let,      .in,        .fail,
        .trap,      .is,       .@"continue", .trait,     .impl,    .new_line, .@"union", .@"extern", .@"defer",
        .eof,
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        try expect(tag == res[i]);
    }
}

test "unterminated string" {
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex("\"blabla bli blop");

    const err = lexer.errs.items[0];
    try expect(err.report == .unterminated_str);
}

test "leading zeros" {
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex("var e = 01\n var b = 00002");

    try expect(lexer.errs.items[0].report == .leading_zeroes);
    try expect(lexer.errs.items[1].report == .leading_zeroes);
}

test "underscore" {
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex("var _under   _=1   var _1art   var ___yo");

    const res = [_]Token.Tag{
        .@"var", .identifier, .underscore, .equal,      .int,
        .@"var", .identifier, .@"var",     .identifier, .eof,
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        try expect(tag == res[i]);
    }
}

test "arrow" {
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex("- > -5> >- -< -> =>");

    const res = [_]Token.Tag{
        .minus, .greater, .minus,       .int,       .greater, .greater, .minus,
        .minus, .less,    .arrow_small, .arrow_big,
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        try expect(tag == res[i]);
    }
}

test "dot" {
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex(". .. ... .? .* ...");

    const res = [_]Token.Tag{
        .dot,      .dot_dot,     .dot_dot_dot, .dot_question_mark,
        .dot_star, .dot_dot_dot,
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        try expect(tag == res[i]);
    }
}

test "range" {
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex("1..2 -4.5..56.7");

    const res = [_]Token.Tag{ .int, .dot_dot, .int, .minus, .float, .dot_dot, .float };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        try expect(tag == res[i]);
    }
}
