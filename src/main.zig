const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const clarg = @import("clarg");
const Arg = clarg.Arg;

const oom = @import("misc").oom;
const State = @import("core/pipeline/State.zig");
const ray = @import("commands/ray.zig");
const Repl = @import("commands/repl/Repl.zig");
const compile = @import("commands/compile.zig");

const Args = struct {
    file: Arg(.string) = .{ .desc = "Path to the file to run", .positional = true },
    print_ast: Arg(bool) = .{ .desc = "Prints the AST" },
    print_ir: Arg(bool) = .{ .desc = "Prints the IR" },
    print_bytecode: Arg(bool) = .{ .desc = "Prints the compiled bytecode" },
    static_analyzis: Arg(bool) = .{ .desc = "Statically checks the file without running it (shows warnings)", .short = 's' },
    path: Arg(.string) = .{ .desc = "Path to fetch additional modules", .short = 'p' },

    compile: Arg(compile.Args) = .{ .desc = "Compiles to a native executable (WIP)", .short = 'c' },

    help: Arg(bool) = .{ .desc = "Prints this help and exit", .short = 'h' },

    pub const description: []const u8 =
        \\Interpreter for Ray language. You can either run a file with the available options
        \\or run a command.
        \\If no arguments are provided, runs the REPL.
    ;
};

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

pub fn main(init: std.process.Init) !void {
    const allocator, const is_debug = gpa: {
        break :gpa switch (builtin.mode) {
            .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
            .ReleaseFast, .ReleaseSmall => .{ std.heap.smp_allocator, false },
        };
    };
    defer if (is_debug) {
        std.debug.assert(debug_allocator.deinit() == .ok);
    };

    const args = try init.minimal.args.toSlice(init.arena.allocator());

    var diag: clarg.Diag = undefined;
    const parsed = clarg.parse(Args, args, &diag, .{}) catch |e| {
        try diag.reportToFile(init.io, .stderr());
        return e;
    };

    if (parsed.help) {
        return clarg.helpToFile(Args, init.io, .stderr());
    }

    const config: State.Config = .{
        .print_ast = parsed.print_ast,
        .print_bytecode = parsed.print_bytecode,
        .static_analyzis = parsed.static_analyzis,
        .print_ir = parsed.print_ir,
        .embedded = parsed.file == null,
        .path = parsed.path,
    };

    if (parsed.compile) |cmd| {
        try compile.run(init.io, allocator, cmd);
    } else if (parsed.file) |f| {
        try ray.run(init.io, allocator, f, config);
    } else {
        try Repl.run(init.io, allocator, config);
    }
}

test {
    _ = @import("core/parser/Lexer.zig");
    _ = @import("core/analyzer/types.zig");
}
