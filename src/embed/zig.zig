const std = @import("std");
const Allocator = std.mem.Allocator;

const core = @import("core");
const State = core.State;
pub const Config = State.Config;
const Pipeline = core.Pipeline;
pub const Vm = core.Vm;
const zffi = core.zffi;
const RayFn = zffi.FnMeta;

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
}

pub fn registerFn(self: *Self, func: RayFn) void {
    self.state.registerFn(self.allocator, func);
}

pub fn initGlobalScope(self: *Self) void {
    self.state.initGlobalScope(self.allocator);
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
