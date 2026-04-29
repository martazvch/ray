const std = @import("std");
const Allocator = std.mem.Allocator;

const Pipeline = @import("../core/pipeline/pipeline.zig");
const Vm = @import("../core/runtime/Vm.zig");
const State = @import("../core/pipeline/State.zig");

pub fn run(io: std.Io, allocator: Allocator, file_path: []const u8, config: State.Config) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    const arena_alloc = arena.allocator();
    defer arena.deinit();

    var state: State = .new(arena_alloc, config);

    const entry_point = mod: {
        const file_content = std.Io.Dir.cwd().readFileAllocOptions(io, file_path, allocator, .unlimited, .of(u8), 0) catch |err| {
            // TODO: Ray error
            std.debug.print("Error: {}, unable to open file at: {s}\n", .{ err, file_path });
            std.process.exit(0);
        };
        defer allocator.free(file_content);

        const entry_point = Pipeline.run(io, arena_alloc, &state, false, file_path, ".", file_content) catch |e| switch (e) {
            error.ExitOnPrint => return,
            else => return e,
        };

        break :mod entry_point;
    };

    var vm: Vm = undefined;
    vm.init(io, allocator, &state);
    defer vm.deinit();

    try vm.run(entry_point, state.modules.modules.values());
}
