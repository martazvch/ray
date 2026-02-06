const std = @import("std");
const Allocator = std.mem.Allocator;
const ray = @import("ray");

pub const Error = error{
    TestFailed,
    ExpectButNoOut,
    ExpectNothingButGot,
};

pub const Body = struct {
    code: [:0]const u8,
    res: ?[]const u8 = null,
};

var output: ?[]const u8 = null;

pub fn printFn(text: []const u8) void {
    output = text;
}

pub fn testMod(allocator: Allocator, Mod: type) Error!void {
    var vm = ray.create(allocator);
    defer vm.deinit();

    vm.init(
        .{
            .embedded = true,
            .printFn = printFn,
        },
        if (@hasDecl(Mod, "natives")) @field(Mod, "natives") else &.{},
    );

    inline for (@typeInfo(Mod).@"struct".decls) |decl| {
        const field = @field(Mod, decl.name);

        if (@TypeOf(field) == Body) {
            try runTest(&vm, field);
        }
    }
}

/// `vm` must be of type ray.Vm, but it isn't marked as public
fn runTest(vm: anytype, body: Body) Error!void {
    output = null;

    vm.run(body.code) catch {
        @panic("Error while running code through VM");
    };

    if (body.res) |expect| {
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
