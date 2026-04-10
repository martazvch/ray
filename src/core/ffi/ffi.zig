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

pub const cVm = opaque {};
pub const Handcheck = *const fn (*const cApi) callconv(.c) void;
pub const Fn = *const fn (*cVm) callconv(.c) void;
const Index = usize;

const cApi = extern struct {
    get_float: *const fn (*cVm, Index) callconv(.c) f64,
    set_float: *const fn (*cVm, Index, f64) callconv(.c) void,
};

pub const api: cApi = .{
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

fn setFloat(c_vm: *cVm, index: Index, value: f64) callconv(.c) void {
    const vm: *Vm = @ptrCast(@alignCast(c_vm));
    vm.frame.slots[index] = .makeFloat(value);
}

fn getFloat(c_vm: *cVm, index: Index) callconv(.c) f64 {
    const vm: *Vm = @ptrCast(@alignCast(c_vm));
    return vm.frame.slots[index].float;
}
