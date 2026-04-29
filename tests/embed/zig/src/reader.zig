const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const eql = std.mem.eql;
const startsWith = std.mem.startsWith;

pub const Part = struct {
    body: [:0]const u8,
    res: ?[]const u8,
};

pub const Case = ArrayList(Part);
pub const Cases = ArrayList(Case);
pub const Error = error{ MissingFile, FileTooLong };

const State = enum {
    none,
    case,
    part,
    pre_code,
    code,
    pre_res,
    res,
};

pub fn read(io: Io, allocator: Allocator, cwd: *std.Io.Dir, file_name: []const u8) Error!Cases {
    const content = cwd.readFileAlloc(io, file_name, allocator, .unlimited) catch return error.MissingFile;
    defer allocator.free(content);

    var it = std.mem.splitScalar(u8, content, '\n');

    var state: State = .none;
    var case: Case = .empty;
    var cases: Cases = .empty;
    var body: ArrayList(u8) = .empty;
    var res: ArrayList(u8) = .empty;

    errdefer unreachable;

    while (it.next()) |raw_line| {
        const line = std.mem.trimEnd(u8, raw_line, "\r");

        switch (state) {
            .none => if (startsWith(u8, line, "# case")) {
                state = .case;
                continue;
            },
            .case => if (startsWith(u8, line, "## part")) {
                state = .part;
                continue;
            },
            .part => if (startsWith(u8, line, "- code")) {
                state = .pre_code;
                continue;
            } else if (startsWith(u8, line, "# case")) {
                state = .case;
                try cases.append(allocator, case);
                case = .empty;
                continue;
            },
            .pre_code => if (startsWith(u8, line, "```")) {
                state = .code;
                continue;
            },
            .code => if (startsWith(u8, line, "```")) {
                state = .pre_res;
                continue;
            },
            .pre_res => if (startsWith(u8, line, "## part")) {
                state = .part;
                try case.append(allocator, .{
                    .body = try body.toOwnedSliceSentinel(allocator, 0),
                    .res = if (res.items.len > 0) try res.toOwnedSlice(allocator) else null,
                });
                body = .empty;
                res = .empty;
                continue;
            } else if (startsWith(u8, line, "```")) {
                state = .res;
                continue;
            },
            .res => if (startsWith(u8, line, "```")) {
                state = .part;
                try case.append(allocator, .{
                    .body = try body.toOwnedSliceSentinel(allocator, 0),
                    .res = if (res.items.len > 0) try res.toOwnedSlice(allocator) else null,
                });
                body = .empty;
                res = .empty;
                continue;
            },
        }

        if (eql(u8, line, "")) continue;

        switch (state) {
            .code => try body.appendSlice(allocator, try allocator.dupe(u8, line)),
            .res => try res.appendSlice(allocator, try allocator.dupe(u8, line)),
            else => {},
        }
    }

    if (case.items.len > 0) {
        try cases.append(allocator, case);
    }

    return cases;
}
