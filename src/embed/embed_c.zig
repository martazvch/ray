const std = @import("std");
const Allocator = std.mem.Allocator;

const core = @import("core");
const State = core.State;
pub const Config = State.Config;
const Pipeline = core.Pipeline;
pub const Vm = core.Vm;
pub const FnProto = core.cffi.FnProto;

var arena: std.heap.ArenaAllocator = .init(std.heap.smp_allocator);
var allocator = arena.allocator();
var state: State = undefined;
var vm: Vm = undefined;

pub const Error = error{ CompileErr, RuntimeErr };

const ConfigC = extern struct {
    embedded: bool = false,
    print_ast: bool = false,
    print_bytecode: bool = false,
    static_analyzis: bool = false,
    print_ir: bool = false,
    dbg_infos: bool = false,
    path: [*c]const u8 = null,
    printFn: ?*const fn ([*c]const u8) callconv(.c) void = null,
};

// Module-level storage for the C callback
var c_print_fn: ?*const fn ([*c]const u8) callconv(.c) void = null;

fn cPrintWrapper(text: []const u8) void {
    std.log.debug("In Zig print wrapper", .{});
    const f = c_print_fn.?;
    // `text` is not null-terminated, so copy it to a stack buffer
    var buf: [4096]u8 = undefined;
    const len = @min(text.len, buf.len - 1);
    @memcpy(buf[0..len], text[0..len]);
    buf[len] = 0;
    f(&buf);
}

pub export fn rayCreate(config: ConfigC) void {
    const print_fn: *const fn ([]const u8) void = if (config.printFn) |f| blk: {
        c_print_fn = f;
        break :blk cPrintWrapper;
    } else State.defaultPrint;

    state = .new(allocator, .{
        .embedded = config.embedded,
        .print_ast = config.print_ast,
        .print_bytecode = config.print_bytecode,
        .static_analyzis = config.static_analyzis,
        .print_ir = config.print_ir,
        .dbg_infos = config.dbg_infos,
        .path = null,
        .printFn = print_fn,
    });
}

pub export fn rayRegisterFn(func: FnProto) void {
    state.registerCFn(allocator, func);
}

pub export fn rayInitGlobalScope() void {
    state.initGlobalScope(allocator);
    vm.init(allocator, &state);
}

pub export fn rayRun(code: [*c]const u8) c_int {
    const entry_point = Pipeline.run(allocator, &state, false, "host", ".", std.mem.span(code)) catch |e| switch (e) {
        error.ExitOnPrint => return 0,
        else => return @intFromError(error.CompileErr),
    };
    vm.runRepl(entry_point, state.modules.modules.values()) catch return @intFromError(error.RuntimeErr);
    return 0;
}

pub export fn rayDeinit() void {
    vm.deinit();
    arena.deinit();
}
