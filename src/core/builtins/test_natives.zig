const std = @import("std");
const zffi = @import("../ffi/zffi.zig");
const Value = @import("../runtime/values.zig").Value;
const Obj = @import("../runtime/Obj.zig");
const Vm = @import("../runtime/Vm.zig");

const Module = @This();

pub const module: zffi.Module = .{
    .is_module = false,
    .functions = &.{
        .init(Module, "test_native_new", "", &.{
            .{ .name = "value1" },
            .{ .name = "value2" },
            .{ .name = "value3" },
            .{ .name = "name" },
        }),
        .init(Module, "test_native_newDefault", "", &.{}),
    },
    .structures = &.{
        .init(Data, "Data"),
        .init(DataNoInit, "DataNoInit"),
    },
};

const Data = struct {
    value1: i64 = 8,
    value2: f64 = 5.6,
    value3: bool = true,
    name: []const u8 = "default name",

    const Self = @This();

    pub const functions: []const zffi.FnMeta = &.{
        .init(Self, "init", "", &.{
            .{ .name = "value1" },
            .{ .name = "value2" },
            .{ .name = "value3" },
            .{ .name = "name" },
        }),
        .init(Self, "clone", "", &.{}),
        .init(Self, "newFromMethod", "", &.{
            .{ .name = "value1" },
            .{ .name = "value2" },
            .{ .name = "value3" },
            .{ .name = "name" },
        }),
        .init(Self, "new", "", &.{
            .{ .name = "value1" },
            .{ .name = "value2" },
            .{ .name = "value3" },
            .{ .name = "name" },
        }),
        .init(Self, "newDefault", "", &.{}),
    };

    pub const fields: []const zffi.StructMeta.Field = &.{
        .init(Self, "value1", ""),
        .init(Self, "value2", ""),
        .init(Self, "value3", ""),
        .init(Self, "name", ""),
    };

    const accessors = zffi.makeAccessors(Self);

    pub fn init(vm: *Vm, value1: i64, value2: f64, value3: bool, name: []const u8) *Self {
        return new(vm, value1, value2, value3, name);
    }

    pub fn deinit(self: *anyopaque, vm: *Vm) void {
        const s: *Self = @ptrCast(@alignCast(self));
        vm.gc_alloc.destroy(s);
    }

    pub fn getField(self: *anyopaque, vm: *Vm, index: usize) Value {
        return accessors[index].get(self, vm);
    }

    // From method
    pub fn clone(self: *Self, vm: *Vm) *Self {
        const cloned = vm.gc_alloc.create(Self) catch unreachable;
        cloned.* = self.*;

        return zffi.makeObj(Self, "Data", cloned, vm);
    }

    pub fn newFromMethod(_: *Self, vm: *Vm, value1: i64, value2: f64, value3: bool, name: []const u8) *Self {
        const cloned = vm.gc_alloc.create(Self) catch unreachable;
        cloned.* = .{
            .value1 = value1,
            .value2 = value2,
            .value3 = value3,
            .name = name,
        };

        return zffi.makeObj(Self, "Data", cloned, vm);
    }

    // From static functions
    pub fn new(vm: *Vm, value1: i64, value2: f64, value3: bool, name: []const u8) *Self {
        const self = vm.gc_alloc.create(Self) catch unreachable;
        self.* = .{
            .value1 = value1,
            .value2 = value2,
            .value3 = value3,
            .name = name,
        };

        return zffi.makeObj(Self, "Data", self, vm);
    }

    pub fn newDefault(vm: *Vm) *Self {
        const self = vm.gc_alloc.create(Self) catch unreachable;
        self.* = .{};

        return zffi.makeObj(Self, "Data", self, vm);
    }
};

pub fn test_native_new(vm: *Vm, value1: i64, value2: f64, value3: bool, name: []const u8) *Data {
    const self = vm.gc_alloc.create(Data) catch unreachable;
    self.* = .{
        .value1 = value1,
        .value2 = value2,
        .value3 = value3,
        .name = name,
    };

    return zffi.makeObj(Data, "Data", self, vm);
}

pub fn test_native_newDefault(vm: *Vm) *Data {
    const self = vm.gc_alloc.create(Data) catch unreachable;
    self.* = .{};

    return zffi.makeObj(Data, "Data", self, vm);
}

// No constructor
pub const DataNoInit = struct {
    const Self = @This();

    pub const functions: []const zffi.FnMeta = &.{};

    pub const fields: []const zffi.StructMeta.Field = &.{};

    const accessors = zffi.makeAccessors(Self);

    pub fn deinit(self: *anyopaque, vm: *Vm) void {
        const s: *Self = @ptrCast(@alignCast(self));
        vm.gc_alloc.destroy(s);
    }

    pub fn getField(self: *anyopaque, vm: *Vm, index: usize) Value {
        return accessors[index].get(self, vm);
    }
};
