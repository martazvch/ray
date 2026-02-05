const std = @import("std");
const posix = std.posix;
const Terminal = @import("Terminal.zig");

const Self = @This();

original_termios: posix.termios,
stdin: std.fs.File,

pub fn init() Terminal.Error!Self {
    const stdin = std.fs.File.stdin();

    const original_termios = posix.tcgetattr(stdin.handle) catch {
        return error.InitFail;
    };

    return .{
        .original_termios = original_termios,
        .stdin = stdin,
    };
}

pub fn enableRawMode(ctx: *anyopaque) Terminal.Error!void {
    const self: *const Self = @ptrCast(@alignCast(ctx));

    var raw = self.original_termios;

    // Disable canonical mode, echo, signals, and special processing
    raw.lflag.ECHO = false;
    raw.lflag.ICANON = false;
    raw.lflag.ISIG = false;
    raw.lflag.IEXTEN = false;

    // Disable input processing
    raw.iflag.IXON = false;
    raw.iflag.ICRNL = false;
    raw.iflag.BRKINT = false;
    raw.iflag.INPCK = false;
    raw.iflag.ISTRIP = false;

    // Set character size to 8 bits
    raw.cflag.CSIZE = .CS8;

    // Set read timeout
    raw.cc[@intFromEnum(posix.V.TIME)] = 0;
    raw.cc[@intFromEnum(posix.V.MIN)] = 1;

    posix.tcsetattr(self.stdin.handle, .FLUSH, raw) catch {
        return error.RawModeFail;
    };
}

pub fn disableRawMode(ctx: *anyopaque) void {
    const self: *const Self = @ptrCast(@alignCast(ctx));
    posix.tcsetattr(self.stdin.handle, .FLUSH, self.original_termios) catch {};
}

fn readByte(self: *const Self) Terminal.Error!u8 {
    var buf: [1]u8 = undefined;
    const n = self.stdin.read(&buf) catch {
        return error.ReadInputError;
    };
    if (n == 0) return error.ReadInputError;
    return buf[0];
}

pub fn getKey(ctx: *anyopaque) Terminal.Error!Terminal.Key {
    const self: *const Self = @ptrCast(@alignCast(ctx));

    const first_byte = try self.readByte();

    // Handle escape sequences
    if (first_byte == 0x1B) { // ESC
        // Try to read next byte with a short timeout
        const second_byte = self.readByte() catch {
            // Just ESC key pressed
            return .{ .value = .{ .char = 0x1B }, .ctrl = false };
        };

        if (second_byte == '[') {
            const third_byte = try self.readByte();

            return switch (third_byte) {
                'A' => .{ .value = .up, .ctrl = false },
                'B' => .{ .value = .down, .ctrl = false },
                'C' => .{ .value = .right, .ctrl = false },
                'D' => .{ .value = .left, .ctrl = false },
                '3' => blk: {
                    // Delete key sends ESC[3~
                    const tilde = try self.readByte();
                    if (tilde == '~') {
                        break :blk .{ .value = .delete, .ctrl = false };
                    }
                    break :blk error.ReadInputError;
                },
                else => error.ReadInputError,
            };
        } else if (second_byte == 'O') {
            // Some terminals send ESC O for special keys
            const third_byte = try self.readByte();
            return switch (third_byte) {
                'A' => .{ .value = .up, .ctrl = false },
                'B' => .{ .value = .down, .ctrl = false },
                'C' => .{ .value = .right, .ctrl = false },
                'D' => .{ .value = .left, .ctrl = false },
                else => error.ReadInputError,
            };
        }

        // Unknown escape sequence
        return error.ReadInputError;
    }

    // Check for control characters
    // const ctrl = first_byte < 32 and first_byte != '\t' and first_byte != '\r' and first_byte != '\n';

    return switch (first_byte) {
        '\r', '\n' => .{ .value = .enter, .ctrl = false },
        '\t' => .{ .value = .tab, .ctrl = false },
        0x7F, 0x08 => .{ .value = .back, .ctrl = false }, // DEL or backspace
        0x03 => .{ .value = .{ .char = 'c' }, .ctrl = true }, // Ctrl-C
        0x01...0x02, 0x04...0x07, 0x0B...0x0C, 0x0E...0x1A => .{ // Ctrl+A through Ctrl+Z (excluding tab, backspace, newline, carriage return)
            .value = .{ .char = first_byte + 'a' - 1 },
            .ctrl = true,
        },
        0x20...0x7E => .{ // Printable ASCII
            .value = .{ .char = first_byte },
            .ctrl = false,
        },
        else => error.NonAsciiChar,
    };
}

pub fn terminal(self: *Self) Terminal {
    return .{
        .ptr = self,
        .vtable = &.{
            .enableRawMode = enableRawMode,
            .disableRawMode = disableRawMode,
            .getKey = getKey,
        },
    };
}
