const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const oom = @import("utils.zig").oom;

const Self = @This();

string: ArrayList([]const u8),

pub const RenderOpts = struct {
    sep: ?[]const u8 = null,
    start_offset: usize = 0,
    end_offset: usize = 0,
};
pub const empty: Self = .{ .string = .empty };

pub fn deinit(self: *Self, alloc: Allocator) void {
    for (self.string.items) |s| {
        alloc.free(s);
    }
    self.string.deinit(alloc);
}

/// Duplicates the string to own it
pub fn append(self: *Self, alloc: Allocator, s: []const u8) void {
    self.string.append(alloc, alloc.dupe(u8, s) catch oom()) catch oom();
}

/// Duplicates the string to own it
pub fn appendSlice(self: *Self, alloc: Allocator, s: []const []const u8) void {
    for (s) |chunk| {
        self.append(alloc, chunk);
    }
}

pub fn pop(self: *Self) ?[]const u8 {
    return self.string.pop();
}

pub fn popMany(self: *Self, count: usize) void {
    for (0..count) |_| _ = self.string.pop();
}

pub fn render(self: *const Self, buf: []u8, opts: RenderOpts) []const u8 {
    var w = std.Io.Writer.fixed(buf);
    const end = self.string.items.len - opts.end_offset;

    for (self.string.items[opts.start_offset..end], 0..) |s, i| {
        if (opts.sep) |sep| {
            if (i != 0) w.writeAll(sep) catch oom();
        }
        w.writeAll(s) catch oom();
    }

    return w.buffered();
}

/// Caller owns the memory
pub fn renderAlloc(self: *const Self, alloc: Allocator, opts: RenderOpts) []const u8 {
    var path: ArrayList(u8) = .empty;
    const end = self.string.items.len - opts.end_offset;

    for (self.string.items[opts.start_offset..end], 0..) |s, i| {
        if (opts.sep) |sep| {
            if (i != 0) path.appendSlice(alloc, sep) catch oom();
        }
        path.appendSlice(alloc, s) catch oom();
    }

    return path.toOwnedSlice(alloc) catch oom();
}

/// Get current count of string chunks
pub fn len(self: *const Self) usize {
    return self.string.items.len;
}

/// Shrinks the number of string chunks
pub fn shrink(self: *Self, alloc: Allocator, length: usize) void {
    for (self.string.items[length..]) |chunk| {
        alloc.free(chunk);
    }
    self.string.shrinkRetainingCapacity(length);
}

/// Creates a copy of this instance which owns its memory
pub fn dup(self: *Self, alloc: Allocator) Self {
    var new: Self = .empty;
    new.appendSlice(alloc, self.string.items);
    return new;
}
