const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const assert = std.debug.assert;
const Writer = std.Io.Writer;
const builtin = @import("builtin");

const BoxChar = enum {
    bottom_left,
    bottom_right,
    horitzontal,
    left_t,
    under_t,
    upper_left,
    upper_right,
    vertical,
};

fn boxChar(kind: BoxChar) []const u8 {
    return switch (kind) {
        .bottom_left => "╰",
        .bottom_right => "╯",
        .horitzontal => "─",
        .left_t => "├",
        .under_t => "┬",
        .upper_left => "╭",
        .upper_right => "╮",
        .vertical => "│",
    };
}

const Color = enum {
    blue,
    cyan,
    green,
    no_color,
    red,
    yellow,
};

fn color(clr: Color) []const u8 {
    return switch (clr) {
        .blue => "\x1b[34m",
        .cyan => "\x1b[96m",
        .green => "\x1b[32m",
        .no_color => "\x1b[0m",
        .red => "\x1b[31m",
        .yellow => "\x1b[33m",
    };
}

fn generateMsg(comptime msg: []const u8, comptime clr: Color) []const u8 {
    return color(clr) ++ msg ++ color(.no_color);
}

const err_msg = generateMsg("Error:", .red);
const help_msg = generateMsg("help:", .green);
const warning_msg = generateMsg("Warning:", .yellow);
const corner_to_hint = boxChar(.bottom_left) ++ boxChar(.horitzontal) ** 4;
const corner_to_end = boxChar(.bottom_left) ++ boxChar(.horitzontal) ** 2;

extern "kernel32" fn GetConsoleOutputCP() std.os.windows.UINT;
extern "kernel32" fn SetConsoleOutputCP(std.os.windows.UINT) void;

/// Reports all the reports of type *Report*
pub fn reportAll(
    io: Io,
    Report: type,
    reports: []const GenReport(Report),
    verbose: bool,
    file_name: []const u8,
    source: [:0]const u8,
) !void {
    const prev_cp = if (builtin.os.tag == .windows) cp: {
        const prev = GetConsoleOutputCP();
        _ = SetConsoleOutputCP(65001);
        break :cp prev;
    } else 0;

    defer if (builtin.os.tag == .windows) {
        _ = SetConsoleOutputCP(prev_cp);
    };

    var stderr_buf: [2048]u8 = undefined;
    var stderr_writer = std.Io.File.stderr().writer(io, &stderr_buf);
    const stderr = &stderr_writer.interface;

    if (verbose) {
        for (reports) |*report| {
            try display(Report, report, stderr, file_name, source);
        }
    } else {
        for (reports) |report| {
            try report.toStr(stderr);
            try stderr.writeAll("\n");
        }
    }

    try stderr.flush();
}

fn display(Report: type, report: *const GenReport(Report), writer: *Writer, file_name: []const u8, source: [:0]const u8) !void {
    // Prints the error part
    //  Error: <err-msg>
    try writer.print("{s} ", .{report.level.getLevelMsg()});
    try report.getMsg(writer);
    _ = try writer.write("\n");

    // If there is visual indication on text
    if (report.end > 0) {
        var current: usize = 0;
        var line_start: usize = 0;
        var line_count: usize = 0;
        var previous_line: ?[]const u8 = null;

        // Looking for current line where it occured and buffers the previous one
        // for context
        while (true) : (current += 1) {
            if (current >= source.len) break;

            if (source[current] == '\n') {
                if (current >= report.start) break;

                // line_start > 0 otherwise if first line of file is \n, current - 1 crashes
                if (line_start > 0) {
                    const end = if (source[current - 1] == '\r') current - 1 else current;
                    previous_line = source[line_start..end];
                }

                line_count += 1;
                // Skip the \n
                line_start = current + 1;
            }
        }

        // Line index start to 1
        line_count += 1;

        var buf: [10]u8 = undefined;
        // We consider the maximum line number being 99 999. The extra space
        // is for space between line number and gutter and the one at the beginning
        const buf2: [7]u8 = [_]u8{' '} ** 7;

        // Gets line number digit count
        const written = try std.fmt.bufPrint(&buf, "{}", .{line_count});
        const line_digit_count = written.len;
        const left_padding = buf2[0 .. written.len + 2];

        // Prints file name and location infos
        //  ╭─[file_name.ray:1:5]
        try writer.print(
            "{s}{s}{s}[{s}{s}{s}:{}:{}]\n",
            .{
                left_padding,
                boxChar(.upper_left),
                boxChar(.horitzontal),
                color(.blue),
                file_name,
                color(.no_color),
                line_count,
                report.end - line_start + 1,
            },
        );
        // Prints previous line number, separation and line itself
        //  56 | var a = 3
        if (previous_line) |pl| {
            try printLine(writer, line_count - 1, pl, line_digit_count);
        }

        // Prints current line number, separation and line
        //  57 | fn add(a, b c)
        try printLine(writer, line_count, source[line_start..current], line_digit_count);

        // Underlines the problem
        // Takes padding into account + separator + space
        //  <space><space> |
        try writer.print("{s}{s} ", .{ left_padding, boxChar(.vertical) });

        // We get the length of the error code and the half to underline it
        var space_buf: [1024]u8 = [_]u8{' '} ** 1024;
        const start_space = report.start - line_start;
        const lexeme_len = @max(report.end - report.start, 1);

        // Prints ^^^^
        _ = try writer.write(space_buf[0..start_space]);
        _ = try writer.write(color(.yellow));
        for (0..lexeme_len) |_| {
            _ = try writer.write("^");
        }
        _ = try writer.write("\n");
        _ = try writer.write(color(.no_color));
    }

    //  help: <help-msg>
    try writer.print(" {s} ", .{help_msg});
    try report.getHelp(writer);
    _ = try writer.write("\n\n");
}

fn printLine(writer: *Writer, line_nb: usize, line: []const u8, padding: usize) !void {
    try writer.print(
        " {[line_nb]:>[padding]} {[box_char]s} {[line]s}\n",
        .{ .line_nb = line_nb, .padding = padding, .box_char = boxChar(.vertical), .line = line },
    );
}

/// Error report used en each step of the Ray language:
/// lexing, parsing, compiling, executing, ...
/// It has:
///  - report: structure that has the message data
///  - level: warning or error
///  - start: starting byte offset from source of the error
///  - end: ending byte offset from source of the error
pub fn GenReport(comptime T: type) type {
    assert(@typeInfo(T) == .@"union");
    assert(@hasDecl(T, "getMsg"));
    assert(@hasDecl(T, "getHelp"));

    return struct {
        report: T,
        level: Level,
        start: usize,
        end: usize,

        pub const Level = enum {
            @"error",
            info,
            warning,

            pub fn getLevelMsg(self: Level) []const u8 {
                return switch (self) {
                    .@"error" => err_msg,
                    .info => @panic("not implemented yet"),
                    .warning => warning_msg,
                };
            }
        };

        const Self = @This();

        pub fn init(report: T, level: Level, start: usize, end: usize) Self {
            return .{
                .report = report,
                .level = level,
                .start = start,
                .end = end,
            };
        }

        /// Creates an error associated with the tag
        pub fn err(report: T, start: usize, end: usize) Self {
            return Self.init(report, .@"error", start, end);
        }

        /// Creates warning associated with the tag
        pub fn warn(report: T, start: usize, end: usize) Self {
            return Self.init(report, .warning, start, end);
        }

        pub fn getMsg(self: *const Self, writer: anytype) !void {
            return self.report.getMsg(writer);
        }

        pub fn getHelp(self: *const Self, writer: anytype) !void {
            return self.report.getHelp(writer);
        }

        /// Used in test mode when we only want error name and associated data
        pub fn toStr(self: *const Self, writer: anytype) !void {
            const name = @tagName(self.report);
            try writer.writeAll(name);

            inline for (std.meta.fields(T)) |field| {
                if (field.type != void and std.mem.eql(u8, field.name, name)) {
                    const field_info = @field(self.report, field.name);

                    inline for (std.meta.fields(field.type)) |subf| {
                        const subv = @field(field_info, subf.name);

                        switch (@typeInfo(@TypeOf(subv))) {
                            .int => |i| {
                                if (i.signedness == .unsigned and i.bits == 8) {
                                    try writer.print(", {c}", .{subv});
                                } else {
                                    try writer.print(", {}", .{subv});
                                }
                            },
                            .@"enum" => try writer.print(", {t}", .{subv}),

                            else => try writer.print(", {s}", .{subv}),
                        }
                    }
                }
            }
        }
    };
}
