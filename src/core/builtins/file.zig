const std = @import("std");
const zffi = @import("../ffi/zffi.zig");
const Value = @import("../runtime/values.zig").Value;
const Obj = @import("../runtime/Obj.zig");
const Vm = @import("../runtime/Vm.zig");

// TODO: handle errors

pub const module: zffi.Module = .{
    .is_module = false,
    .functions = &.{
        .init("open", open, "", &.{.{ .name = "path" }}),
    },
    .structures = &.{
        .init(File, "File"),
    },
};

const File = struct {
    fd: std.Io.File,

    const Self = @This();

    pub const functions: []const zffi.FnMeta = &.{
        .init("readAll", readAll, "", &.{}),
    };

    pub const fields: []const zffi.StructMeta.Field = &.{
        .{ .name = "path", .desc = "" },
    };

    fn readAll(self: *Self, vm: *Vm) []const u8 {
        var reader = self.fd.reader(vm.io, &.{});
        const interface = &reader.interface;

        return interface.readAlloc(
            vm.gc_alloc,
            (self.fd.stat(vm.io) catch unreachable).size,
        ) catch unreachable;
    }

    fn deinit(self: *anyopaque, vm: *Vm) void {
        const s: *Self = @ptrCast(@alignCast(self));
        vm.gc_alloc.destroy(s);
    }
};

fn open(vm: *Vm, path: []const u8) *File {
    const self = vm.gc_alloc.create(File) catch unreachable;
    self.* = .{
        .fd = std.Io.Dir.cwd().openFile(vm.io, path, .{}) catch unreachable,
    };

    const obj = Obj.NativeObj.create(vm, "File", self, File.deinit);

    return @ptrCast(@alignCast(&obj.child));
}
