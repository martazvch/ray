const std = @import("std");
const ffi = @import("ffi.zig");
const Value = @import("../runtime/values.zig").Value;
const Obj = @import("../runtime/Obj.zig");
const Vm = @import("../runtime/Vm.zig");

// TODO: handle errors

pub const module: ffi.ZigModule = .{
    .is_module = false,
    .functions = &.{
        .init("open", open, "", &.{.{ .name = "path" }}),
    },
    .structures = &.{
        File,
    },
};

const File = struct {
    fd: std.fs.File,

    const Self = @This();

    pub const zig_struct: ffi.ZigStructMeta = .{
        .name = "File",
        .functions = &.{
            .init("readAll", readAll, "", &.{}),
        },
        .deinit_fn = deinit,
    };

    fn readAll(self: *Self, vm: *Vm) []const u8 {
        var reader = self.fd.reader(&.{});
        const interface = &reader.interface;

        return interface.readAlloc(
            vm.gc_alloc,
            (self.fd.stat() catch unreachable).size,
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
        .fd = std.fs.cwd().openFile(path, .{}) catch unreachable,
    };

    const obj = Obj.NativeObj.create(vm, "File", self, File.zig_struct.deinit_fn);

    return @ptrCast(@alignCast(&obj.child));
}
