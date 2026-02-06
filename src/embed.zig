const std = @import("std");
const Allocator = std.mem.Allocator;

const State = @import("core/pipeline/State.zig");
const Config = State.Config;
const Pipeline = @import("core/pipeline/pipeline.zig");
const Vm = @import("core/runtime/Vm.zig");

arena: std.heap.ArenaAllocator,
allocator: Allocator,
state: State,
vm: Vm,

const Self = @This();
pub const Error = error{ CompileErr, RuntimeErr };

pub fn create(allocator: Allocator) Self {
    return .{
        .arena = .init(allocator),
        .allocator = undefined,
        .state = undefined,
        .vm = undefined,
    };
}

pub fn init(self: *Self, config: Config) void {
    self.allocator = self.arena.allocator();
    self.state = .new(self.allocator, config);
    self.vm.init(self.allocator, &self.state);
}

pub fn run(self: *Self, code: [:0]const u8) Error!void {
    const entry_point = Pipeline.run(self.allocator, &self.state, false, "host", ".", code) catch |e| switch (e) {
        error.ExitOnPrint => return,
        else => return error.CompileErr,
    };
    self.vm.runRepl(entry_point, self.state.modules.modules.values()) catch return error.RuntimeErr;
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
}
