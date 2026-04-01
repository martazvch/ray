const std = @import("std");
const misc = @import("misc");
const SymbolArrMap = @import("../analyzer/LexicalScope.zig").SymbolArrMap;
const TypeInterner = @import("../analyzer/types.zig").TypeInterner;
const NativeReg = @import("../pipeline/NativesRegister.zig");
const Type = @import("../analyzer/types.zig").Type;
const Value = @import("../runtime/values.zig").Value;
const Vm = @import("../runtime/Vm.zig");
const Interner = misc.Interner;
const oom = misc.oom;

const cVm = opaque {};
const cRegister = opaque {};
pub const Register = struct {
    allocator: std.mem.Allocator,
    funcs: std.ArrayList(struct {
        name: Interner.Index,
        type: *const Type,
        index: usize,
        func: Fn,
        returns: bool,
    }),
    interner: *Interner,
    ti: *TypeInterner,
};

pub const Handcheck = *const fn (*cRegister, *const cApi) callconv(.c) void;
pub const Fn = *const fn (*cVm) callconv(.c) void;
const Index = usize;

const cApi = extern struct {
    register_fn: *const fn (*cRegister, Fn, FnProto) callconv(.c) void,
    get_float: *const fn (*cVm, Index) callconv(.c) f64,
    set_float: *const fn (*cVm, Index, f64) callconv(.c) void,
};

pub const api: cApi = .{
    .register_fn = registerFn,
    .get_float = getFloat,
    .set_float = setFloat,
};

pub const cType = enum(c_int) {
    void,
    int,
    float,
    bool,
};

pub const FnProto = extern struct {
    name: [*c]const u8,
    arity: c_int,
    params: [max_param]Param,
    return_type: cType,
    func: Fn,

    const max_param = 256;
    const Param = extern struct {
        name: [*c]const u8,
        ty: cType,
    };
};

fn registerFn(c_register: *cRegister, proto: FnProto) callconv(.c) void {
    var register: *Register = @ptrCast(@alignCast(c_register));

    register.funcs.append(
        register.allocator,
        .{
            .name = register.interner.intern(std.mem.span(proto.name)),
            .type = NativeReg.fnCToRay(register, proto),
            .index = register.funcs.items.len,
            .func = @ptrCast(proto.func),
            .returns = proto.return_type != .void,
        },
    ) catch oom();
}

fn setFloat(c_vm: *cVm, index: Index, value: f64) callconv(.c) void {
    const vm: *Vm = @ptrCast(@alignCast(c_vm));
    vm.frame.slots[index] = .makeFloat(value);
}

fn getFloat(c_vm: *cVm, index: Index) callconv(.c) f64 {
    const vm: *Vm = @ptrCast(@alignCast(c_vm));
    return vm.frame.slots[index].float;
}
