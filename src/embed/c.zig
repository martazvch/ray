const std = @import("std");
const Allocator = std.mem.Allocator;

const core = @import("core");
const State = core.State;
pub const Config = State.Config;
const Pipeline = core.Pipeline;
pub const Vm = core.Vm;
pub const FnProto = core.cffi.FnProto;

const oom = @import("misc").oom;

var arena: std.heap.ArenaAllocator = .init(std.heap.smp_allocator);
var allocator = arena.allocator();

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

var c_print_fn: ?*const fn ([*c]const u8) callconv(.c) void = null;

fn cPrintWrapper(text: []const u8) void {
    const f = c_print_fn.?;
    // `text` is not null-terminated, so copy it to a stack buffer
    var buf: [4096]u8 = undefined;
    const len = @min(text.len, buf.len - 1);
    @memcpy(buf[0..len], text[0..len]);
    buf[len] = 0;
    f(&buf);
}

const OpaqueVm = opaque {};
pub export fn rayNewVm(config: ConfigC) *OpaqueVm {
    const print_fn: *const fn ([]const u8) void = if (config.printFn) |f| blk: {
        c_print_fn = f;
        break :blk cPrintWrapper;
    } else State.defaultPrint;

    const heap_state = allocator.create(State) catch oom();
    heap_state.* = .new(allocator, .{
        .embedded = config.embedded,
        .print_ast = config.print_ast,
        .print_bytecode = config.print_bytecode,
        .static_analyzis = config.static_analyzis,
        .print_ir = config.print_ir,
        .dbg_infos = config.dbg_infos,
        .path = null,
        .printFn = print_fn,
    });

    var heap_vm = allocator.create(Vm) catch oom();
    heap_vm.init(allocator, heap_state);

    return @ptrCast(heap_vm);
}

pub export fn rayDeinitVm(opaque_vm: *OpaqueVm) void {
    const vm: *Vm = @ptrCast(@alignCast(opaque_vm));
    allocator.destroy(vm.state);
    vm.deinit();
    allocator.destroy(vm);
}

pub export fn rayRegisterFn(opaque_vm: *OpaqueVm, func: FnProto) void {
    const vm: *Vm = @ptrCast(@alignCast(opaque_vm));
    vm.state.registerCFn(allocator, func);
}

pub export fn rayInitGlobalScope(opaque_vm: *OpaqueVm) void {
    const vm: *Vm = @ptrCast(@alignCast(opaque_vm));
    vm.state.initGlobalScope(allocator);
    vm.init(allocator, vm.state);
}

pub export fn rayRun(opaque_vm: *OpaqueVm, code: [*c]const u8) c_int {
    const vm: *Vm = @ptrCast(@alignCast(opaque_vm));
    const entry_point = Pipeline.run(allocator, vm.state, false, "host", ".", std.mem.span(code)) catch |e| switch (e) {
        error.ExitOnPrint => return 0,
        else => return @intFromError(error.CompileErr),
    };
    vm.runRepl(entry_point, vm.state.modules.modules.values()) catch return @intFromError(error.RuntimeErr);
    return 0;
}

const Index = usize;

pub export fn raySetInt(opaque_vm: *OpaqueVm, index: Index, value: i64) void {
    const vm: *Vm = @ptrCast(@alignCast(opaque_vm));
    vm.frame.slots[index] = .makeInt(value);
}

pub export fn raySetBool(opaque_vm: *OpaqueVm, index: Index, value: bool) void {
    const vm: *Vm = @ptrCast(@alignCast(opaque_vm));
    vm.frame.slots[index] = .makeBool(value);
}

pub export fn rayGetInt(opaque_vm: *OpaqueVm, index: Index) i64 {
    const vm: *Vm = @ptrCast(@alignCast(opaque_vm));
    return vm.frame.slots[index].int;
}
