const std = @import("std");
const zffi = @import("../ffi/zffi.zig");
const Value = @import("../runtime/values.zig").Value;
const Obj = @import("../runtime/Obj.zig");
const Vm = @import("../runtime/Vm.zig");

// TODO: handle errors
const Module = @This();

pub const module: zffi.Module = .{
    .is_module = false,
    .functions = &.{
        .init(Module, "open", "", &.{.{ .name = "path" }}),
    },
    .structures = &.{
        .init(File, "File"),
    },
};

const File = struct {
    fd: std.Io.File,
    path: []const u8,

    const Self = @This();

    // TODO: make helper functions so we don't even need to specify types
    pub const functions: []const zffi.FnMeta = &.{
        .init(Self, "readAll", "", &.{}),
    };

    pub const fields: []const zffi.StructMeta.Field = &.{
        .init(Self, "path", ""),
    };

    const accessors = zffi.makeAccessors(Self);

    pub fn readAll(self: *Self, vm: *Vm) []const u8 {
        var reader = self.fd.reader(vm.io, &.{});
        const interface = &reader.interface;

        return interface.readAlloc(
            vm.gc_alloc,
            (self.fd.stat(vm.io) catch unreachable).size,
        ) catch unreachable;
    }

    pub fn deinit(self: *anyopaque, vm: *Vm) void {
        const s: *Self = @ptrCast(@alignCast(self));
        s.fd.close(vm.io);
        vm.gc_alloc.destroy(s);
    }

    pub fn getField(self: *anyopaque, vm: *Vm, index: usize) Value {
        return accessors[index].get(self, vm);
    }
};

pub fn open(vm: *Vm, path: []const u8) *File {
    const self = vm.gc_alloc.create(File) catch unreachable;
    self.* = .{
        .fd = std.Io.Dir.cwd().openFile(vm.io, path, .{}) catch unreachable,
        .path = path,
    };

    return zffi.makeObj(File, "File", self, vm);
}
