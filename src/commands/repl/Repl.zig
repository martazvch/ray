const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const builtin = @import("builtin");

const Terminal = @import("terminal/Terminal.zig");
const WinTerm = @import("terminal/WinTerm.zig");
const UnixTerm = @import("terminal/UnixTerm.zig");
const Pipeline = @import("../../core/pipeline/pipeline.zig");
const Vm = @import("../../core/runtime/Vm.zig");
const State = @import("../../core/pipeline/State.zig");
const oom = @import("misc").oom;

arena: std.heap.ArenaAllocator,
allocator: Allocator,
terminal: Terminal,
stdout: std.fs.File.Writer,
stdout_writer: *std.io.Writer,
prompts: Prompts,
cursor_pos: Vec2,
indent_level: usize,
state: State,
vm: Vm,

const Self = @This();
const Prompts = ArrayList([:0]const u8);
const Error = error{ BadRead, BadWrite, TooManyIndents, Empty, EndOfFile } || std.io.Writer.Error;

const MAX_IDENT = 64;
const INDENT_SIZE = 4;
const SPACES = " " ** (INDENT_SIZE * MAX_IDENT);

const Vec2 = struct {
    x: usize,
    y: usize,

    pub const zero: Vec2 = .{ .x = 0, .y = 0 };
};

var stdout_buf: [1024]u8 = undefined;

pub fn run(allocator: Allocator, config: State.Config) !void {
    var repl: Self = undefined;
    repl.init(allocator, config);
    defer repl.deinit();

    try repl.execute();
}

fn init(self: *Self, allocator: Allocator, config: State.Config) void {
    self.arena = .init(allocator);
    self.allocator = self.arena.allocator();

    var terminal = terminal: {
        // Windows
        if (builtin.os.tag == .windows) {
            var term = self.allocator.create(WinTerm) catch oom();
            term.* = WinTerm.init() catch unreachable;
            break :terminal term.terminal();
        }
        // Unix
        else if (builtin.os.tag == .macos or builtin.os.tag == .linux) {
            var term = self.allocator.create(UnixTerm) catch oom();
            term.* = UnixTerm.init() catch unreachable;
            break :terminal term.terminal();
        }
        // Unsupported
        else @panic("unsupported OS for REPL");
    };
    terminal.enableRawMode() catch unreachable;

    self.terminal = terminal;

    self.stdout = std.fs.File.stdout().writer(&stdout_buf);
    self.stdout_writer = &self.stdout.interface;

    self.prompts = .{};
    self.cursor_pos = .zero;
    self.indent_level = 0;

    self.state = .new(self.allocator, config);
    self.vm = undefined;
    self.vm.init(allocator, &self.state);

    self.logInfos();
}

fn deinit(self: *Self) void {
    self.terminal.disableRawMode();
    self.arena.deinit();
}

fn logInfos(self: *Self) void {
    errdefer oom();
    defer self.stdout_writer.flush() catch oom();
    _ = try self.stdout_writer.write(
        \\        Ray language REPL
        \\
        ,
    );
}

fn execute(self: *Self) !void {
    while (true) {
        const prompt = self.getPrompt(self.allocator) catch |e| switch (e) {
            error.Empty => continue,
            error.EndOfFile => break,
            else => {
                std.debug.print("REPL error: {s}\n", .{@errorName(e)});
                return;
            },
        };

        const entry_point = Pipeline.run(self.allocator, &self.state, false, "stdin", ".", prompt) catch |e| switch (e) {
            error.ExitOnPrint => continue,
            else => return e,
        };
        try self.vm.run(entry_point, self.state.modules.modules.values());
    }
}

fn getPrompt(self: *Self, allocator: Allocator) Error![:0]const u8 {
    var input: ArrayList(u8) = .{};
    errdefer input.deinit(allocator);

    var line_offset: usize = 0;
    self.cursor_pos.x = 0;

    self.printPs1();

    while (true) {
        defer self.stdout_writer.flush() catch oom();

        const key = self.terminal.getKey() catch |e| switch (e) {
            error.NonAsciiChar => {
                self.stdout_writer.writeAll("Error: accept only ascii characters\n") catch unreachable;
                self.printPs1();
                continue;
            },
            else => unreachable,
        };

        switch (key.value) {
            .up => {
                if (self.cursor_pos.y == 0) continue;
                self.cursor_pos.y -= 1;
                line_offset = self.restoreHistory(allocator, self.cursor_pos.y, &input, line_offset);
            },
            .down => {
                // If end of history, reset line
                if (self.cursor_pos.y == self.prompts.items.len - 1) {
                    self.cursor_pos.y += 1;
                    self.resetLine(&input, 0);
                    line_offset = 0;
                    continue;
                } else if (self.cursor_pos.y == self.prompts.items.len) {
                    continue;
                } else {
                    self.cursor_pos.y += 1;
                    line_offset = self.restoreHistory(allocator, self.cursor_pos.y, &input, line_offset);
                }
            },
            .left => {
                if (self.cursor_pos.x > 0) {
                    self.moveCursor(.left, 1);
                }
            },
            .right => {
                if (self.cursor_pos.x < input.items.len) {
                    self.moveCursor(.right, 1);
                }
            },
            .back => {
                if (self.cursor_pos.x == 0) continue;

                const line = input.items[line_offset + self.cursor_pos.x - 1 ..];

                std.mem.copyForwards(u8, line[0..], line[1..]);
                _ = input.pop();
                self.moveCursor(.left, 1);
                var len = self.stdout_writer.write(input.items[line_offset + self.cursor_pos.x ..]) catch unreachable;
                // Erase the last character
                len += self.stdout_writer.write(" ") catch unreachable;

                self.cursor_pos.x += len;
                self.moveCursor(.left, len);
            },
            .delete => {},
            .tab => {
                self.stdout_writer.writeAll(SPACES[0..INDENT_SIZE]) catch unreachable;
                input.appendSlice(allocator, SPACES[0..INDENT_SIZE]) catch oom();
                self.cursor_pos.x += INDENT_SIZE;
            },
            .enter => {
                const trimmed = std.mem.trimRight(u8, input.items[line_offset..], " ");

                if (trimmed.len == 0) {
                    try self.stdout_writer.writeAll("\n");

                    if (self.indent_level > 0) {
                        input.appendSlice(allocator, "\n") catch oom();
                    }

                    self.printPs1();
                    line_offset = input.items.len;
                    self.cursor_pos.x = self.indent(allocator, &input);
                    self.stdout_writer.writeAll(input.items[line_offset..]) catch unreachable;
                    continue;
                }

                if (std.mem.eql(u8, trimmed, "quit")) {
                    return error.EndOfFile;
                }

                try self.checkIndent(input.items);

                if (self.indent_level == 0) {
                    self.lineReturn(allocator, &input, true);
                    self.prompts.append(allocator, input.toOwnedSliceSentinel(allocator, 0) catch oom()) catch oom();
                    self.cursor_pos.y += 1;
                    return self.prompts.getLast();
                } else {
                    line_offset = self.lineReturnContinue(allocator, &input);
                    continue;
                }
            },
            .char => |char| {
                if (char == 'c' and key.ctrl) {
                    return error.EndOfFile;
                }

                input.insert(allocator, line_offset + self.cursor_pos.x, char) catch oom();
                self.writeAndMoveCursor(char);
            },
        }

        if (self.cursor_pos.x != input.items.len - line_offset) {
            // Rewrite tail after cursor
            try self.stdout_writer.writeAll(input.items[line_offset + self.cursor_pos.x ..]);
            // Move cursor back to original position
            const diff = input.items.len - line_offset - self.cursor_pos.x;
            try self.stdout_writer.print("\x1b[{}D", .{diff});
        }
    }

    unreachable;
}

fn writeAndMoveCursor(self: *Self, char: u8) void {
    defer self.stdout_writer.flush() catch oom();
    self.stdout_writer.writeByte(char) catch unreachable;
    self.cursor_pos.x += 1;
}

fn printPs1(self: *Self) void {
    defer self.stdout_writer.flush() catch oom();
    _ = self.stdout_writer.write(if (self.indent_level == 0) "> " else "| ") catch unreachable;
}

fn indent(self: *Self, allocator: Allocator, line: *ArrayList(u8)) usize {
    const indent_len = self.indent_level * INDENT_SIZE;
    const indent_chars = SPACES[0..indent_len];
    line.appendSlice(allocator, indent_chars) catch oom();

    return indent_len;
}

fn moveCursor(self: *Self, dir: enum { left, right }, amount: usize) void {
    defer self.stdout_writer.flush() catch oom();

    if (dir == .left) {
        self.cursor_pos.x -= amount;
        self.stdout_writer.print("\x1b[{}D", .{amount}) catch unreachable;
    } else {
        self.cursor_pos.x += amount;
        self.stdout_writer.print("\x1b[{}C", .{amount}) catch unreachable;
    }
}

fn moveCursorEndOfLine(self: *Self, line_len: usize) void {
    if (self.cursor_pos.x < line_len) {
        self.moveCursor(.right, line_len - self.cursor_pos.x);
    }
}

fn clearLineMoveStartOfLine(self: *Self) void {
    defer self.stdout_writer.flush() catch oom();
    self.stdout_writer.writeAll("\x1b[2K\r") catch unreachable;
}

fn lineReturn(self: *Self, allocator: Allocator, line: *ArrayList(u8), write: bool) void {
    defer self.stdout_writer.flush() catch oom();

    self.moveCursorEndOfLine(line.items.len);
    self.stdout_writer.writeAll("\n") catch unreachable;

    if (write) {
        line.appendSlice(allocator, "\n") catch oom();
    }
}

fn lineReturnContinue(self: *Self, allocator: Allocator, line: *ArrayList(u8)) usize {
    defer self.stdout_writer.flush() catch oom();

    self.moveCursorEndOfLine(line.items.len);
    line.appendSlice(allocator, "\n") catch oom();
    _ = self.stdout_writer.writeAll("\n") catch unreachable;
    self.printPs1();

    const line_offset = line.items.len;
    const indent_len = self.indent(allocator, line);
    _ = self.stdout_writer.writeAll(line.items[line_offset..]) catch unreachable;
    self.cursor_pos.x = indent_len;

    return line_offset;
}

fn checkIndent(self: *Self, input: []const u8) Error!void {
    self.indent_level = 0;

    for (input) |c| {
        if (c == '{')
            self.indent_level += 1
        else if (c == '}')
            // TODO: protect?
            self.indent_level -= 1;
    }

    if (self.indent_level > MAX_IDENT) return error.TooManyIndents;
}

fn restoreHistory(self: *Self, allocator: Allocator, index: usize, line: *ArrayList(u8), offset: usize) usize {
    defer self.stdout_writer.flush() catch oom();

    self.resetLine(line, offset);
    const prev = self.prompts.items[index];
    const prev_no_return = prev[0 .. prev.len - 1];
    line.appendSlice(allocator, prev_no_return) catch oom();
    self.stdout_writer.writeAll(prev_no_return) catch unreachable;
    const start_of_last_line = std.mem.lastIndexOf(u8, prev_no_return, "\n") orelse 0;
    self.cursor_pos.x = prev_no_return.len - start_of_last_line;

    return start_of_last_line;
}

/// Resets the line by erasing content from console and line buffer and prints Ps1 back
fn resetLine(self: *Self, line: *ArrayList(u8), offset: usize) void {
    defer self.stdout_writer.flush() catch oom();

    if (line.items.len != offset) {
        const line_count = std.mem.count(u8, line.items, "\n");

        self.clearLineMoveStartOfLine();
        for (0..line_count) |_| {
            // Move up
            self.stdout_writer.writeAll("\x1b[A") catch unreachable;
            self.clearLineMoveStartOfLine();
        }

        self.printPs1();

        line.shrinkRetainingCapacity(offset);
        self.cursor_pos.x = 0;
    }
}
