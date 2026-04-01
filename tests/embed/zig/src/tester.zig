const std = @import("std");
const Allocator = std.mem.Allocator;
const ray = @import("ray");
const Reader = @import("reader.zig");

pub const Error = error{
    TestFailed,
    ExpectButNoOut,
    ExpectNothingButGot,
};

var output: ?[]const u8 = null;

pub fn printFn(text: []const u8) void {
    output = text;
}

fn isLess(_: *ray.Vm, a: i64, b: i64) bool {
    return a < b;
}

pub fn testDir(allocator: Allocator, path: []const u8) !void {
    var vm = ray.create(allocator);
    defer vm.deinit();

    vm.init(
        .{
            .embedded = true,
            .printFn = printFn,
        },
        &.{
            .init("isLess", isLess, "", &.{
                .{ .name = "a" },
                .{ .name = "b" },
            }),
        },
    );

    var cwd = std.fs.cwd();
    var test_dir = try cwd.openDir(path, .{ .iterate = true });
    var walker = try test_dir.walk(allocator);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        if (entry.kind != .file) continue;
        const cases = try Reader.read(allocator, &test_dir, entry.basename);

        for (cases.items) |case| {
            for (case.items) |part| {
                try runTest(&vm, part);
            }
        }
    }
}

/// `vm` must be of type ray.Vm, but it isn't marked as public
fn runTest(vm: anytype, part: Reader.Part) Error!void {
    output = null;

    vm.run(part.body) catch {
        @panic("Error while running code through VM");
    };

    if (part.res) |expect| {
        if (output) |out| {
            if (!std.mem.eql(u8, expect, out)) {
                std.debug.print(
                    \\Mismatch between expected and output
                    \\ Expected:
                    \\ ---------
                    \\{s}
                    \\
                    \\ Got:
                    \\ ----
                    \\{s}
                    \\
                ,
                    .{ expect, out },
                );
                return error.TestFailed;
            }
        } else {
            std.debug.print(
                \\Expected output but got nothing
                \\ Expected:
                \\ ---------
                \\{s}
                \\
            ,
                .{expect},
            );
            return error.ExpectButNoOut;
        }
    } else {
        if (output) |out| {
            std.debug.print(
                \\Expected nothing but got
                \\ Got:
                \\ ---------
                \\{s}
                \\
            ,
                .{out},
            );
            return error.ExpectNothingButGot;
        }
    }
}
