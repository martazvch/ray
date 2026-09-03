const std = @import("std");
const misc = @import("misc");
const TypeInterner = @import("../analyzer/types.zig").TypeInterner;
const NativeReg = @import("../pipeline/NativesRegister.zig");
const Type = @import("../analyzer/types.zig").Type;
const Obj = @import("../runtime/Obj.zig");
const Value = @import("../runtime/values.zig").Value;
const Vm = @import("../runtime/Vm.zig");
const Interner = misc.Interner;
const oom = misc.oom;

pub const cVm = opaque {};
pub const CStruct = opaque {};
pub const Handcheck = *const fn (*const cApi, Index) callconv(.c) void;
pub const Fn = *const fn (*cVm) callconv(.c) void;
const Index = usize;

const cApi = extern struct {
    set_float: *const fn (*cVm, Index, f64) callconv(.c) void,
    get_float: *const fn (*const cVm, Index) callconv(.c) f64,
    set_int: *const fn (*cVm, Index, i64) callconv(.c) void,
    get_int: *const fn (*const cVm, Index) callconv(.c) i64,
    set_bool: *const fn (*cVm, Index, bool) callconv(.c) void,
    get_bool: *const fn (*const cVm, Index) callconv(.c) bool,
    set_str: *const fn (*cVm, Index, [*c]const u8) callconv(.c) void,
    get_str: *const fn (*const cVm, Index) callconv(.c) [*c]const u8,

    new_struct: *const fn (*cVm, Index, Index) callconv(.c) *CStruct,
    struct_bytes: *const fn (*CStruct) callconv(.c) [*c]u8,
    set_struct: *const fn (*cVm, Index, *CStruct) callconv(.c) void,
    get_struct: *const fn (*cVm, Index) callconv(.c) *CStruct,

    get_enum_tag: *const fn (*const cVm, Index) callconv(.c) i64,
};

pub const api: cApi = .{
    .set_float = setFloat,
    .get_float = getFloat,
    .set_int = setInt,
    .get_int = getInt,
    .set_bool = setBool,
    .get_bool = getBool,
    .set_str = setStr,
    .get_str = getStr,

    .new_struct = newStruct,
    .struct_bytes = structBytes,
    .set_struct = setStruct,
    .get_struct = getStruct,

    .get_enum_tag = getEnumTag,
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

fn getFloat(c_vm: *const cVm, index: Index) callconv(.c) f64 {
    const vm: *const Vm = @ptrCast(@alignCast(c_vm));
    return vm.frame.slots[index].float;
}

fn setBool(c_vm: *cVm, index: Index, value: bool) callconv(.c) void {
    const vm: *Vm = @ptrCast(@alignCast(c_vm));
    vm.frame.slots[index] = .makeBool(value);
}

fn getBool(c_vm: *const cVm, index: Index) callconv(.c) bool {
    const vm: *const Vm = @ptrCast(@alignCast(c_vm));
    return vm.frame.slots[index].bool;
}

fn setInt(c_vm: *cVm, index: Index, value: i64) callconv(.c) void {
    const vm: *Vm = @ptrCast(@alignCast(c_vm));
    vm.frame.slots[index] = .makeInt(value);
}

fn getInt(c_vm: *const cVm, index: Index) callconv(.c) i64 {
    const vm: *const Vm = @ptrCast(@alignCast(c_vm));
    return vm.frame.slots[index].int;
}

fn setStr(c_vm: *cVm, index: Index, value: [*c]const u8) callconv(.c) void {
    const vm: *Vm = @ptrCast(@alignCast(c_vm));
    vm.frame.slots[index] = .makeObj(Obj.String.takeCopy(vm, std.mem.span(value)).asObj());
}

fn getStr(c_vm: *const cVm, index: Index) callconv(.c) [*c]const u8 {
    const vm: *const Vm = @ptrCast(@alignCast(c_vm));
    return vm.frame.slots[index].obj.as(Obj.String).chars.ptr;
}

fn newStruct(c_vm: *cVm, module: Index, symbol: Index) callconv(.c) *CStruct {
    const vm: *Vm = @ptrCast(@alignCast(c_vm));
    return @ptrCast(Obj.CStructure.create(vm, vm.modules[module].c_structs[symbol].layout));
}

fn structBytes(cstruct: *CStruct) callconv(.c) [*c]u8 {
    const obj: *Obj.CStructure = @ptrCast(@alignCast(cstruct));
    return @ptrCast(obj.bytes);
}

fn setStruct(c_vm: *cVm, index: Index, value: *CStruct) callconv(.c) void {
    const vm: *Vm = @ptrCast(@alignCast(c_vm));
    vm.frame.slots[index] = .makeObj(@as(*Obj.CStructure, @ptrCast(@alignCast(value))).asObj());
}

fn getStruct(c_vm: *const cVm, index: Index) callconv(.c) *CStruct {
    const vm: *const Vm = @ptrCast(@alignCast(c_vm));
    return @ptrCast(vm.frame.slots[index].obj.as(Obj.CStructure));
}

fn getFieldU8(c_struct: *const CStruct, index: Index) callconv(.c) u8 {
    const s: *const Obj.Structure = @ptrCast(@alignCast(c_struct));
    return @intCast(s.fields[index].int);
}

fn getEnumTag(c_vm: *const cVm, index: Index) callconv(.c) i64 {
    const vm: *const Vm = @ptrCast(@alignCast(c_vm));
    return vm.frame.slots[index].obj.as(Obj.Enum).payload;
}
