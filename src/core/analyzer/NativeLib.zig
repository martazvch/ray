const std = @import("std");
const Allocator = std.mem.Allocator;
const DynLib = std.DynLib;
const builtin = @import("builtin");
const oom = @import("misc").oom;

const HMODULE = *anyopaque;
const FARPROC = *anyopaque;

extern "kernel32" fn LoadLibraryExW(
    lpLibFileName: [*:0]const u16,
    hFile: ?*anyopaque,
    dwFlags: u32,
) callconv(.winapi) ?HMODULE;

extern "kernel32" fn FreeLibrary(hLibModule: HMODULE) callconv(.winapi) i32;
extern "kernel32" fn GetProcAddress(hModule: HMODULE, lpProcName: [*:0]const u8) callconv(.winapi) ?FARPROC;
extern "kernel32" fn GetLastError() callconv(.winapi) u32;

lib: Lib,

const os = builtin.os.tag;
const LOAD_LIBRARY_SEARCH_DEFAULT_DIRS = 0x1000;

const Self = @This();
const Lib = if (os == .windows) HMODULE else DynLib;
const Error = error{ UnsupportedOS, LoadFailed };

pub fn open(alloc: Allocator, path: []const u8, name: []const u8) Error!Self {
    const full_path = std.fmt.allocPrint(alloc, "{s}{s}{s}", .{
        path,
        std.Io.Dir.path.sep_str,
        name,
    }) catch oom();

    const lib_handle = switch (os) {
        .windows => win: {
            var path_utf16: [std.fs.max_path_bytes:0]u16 = undefined;
            const n = std.unicode.utf8ToUtf16Le(&path_utf16, full_path) catch return error.LoadFailed;
            path_utf16[n] = 0;

            break :win LoadLibraryExW(
                @ptrCast(path_utf16[0..n].ptr),
                null,
                LOAD_LIBRARY_SEARCH_DEFAULT_DIRS,
            ) orelse return error.LoadFailed;
        },
        else => DynLib.open(full_path) catch return error.LoadFailed,
    };

    return .{
        .lib = lib_handle,
    };
}

fn libName(alloc: Allocator, name: []const u8) Error![]const u8 {
    return switch (os) {
        .linux => std.fmt.allocPrint(alloc, "lib{s}.so", .{name}) catch oom(),
        .macos => std.fmt.allocPrint(alloc, "lib{s}.dylib", .{name}) catch oom(),
        .windows => std.fmt.allocPrit(alloc, "{s}.dll", .{name}) catch oom(),
        else => error.UnsupportedOS,
    };
}

pub fn lookup(self: *Self, alloc: Allocator, T: type, name: []const u8) ?T {
    return switch (os) {
        .windows => win: {
            const name_sentinel = alloc.dupeZ(u8, name) catch oom();
            defer alloc.free(name_sentinel);

            const proc = GetProcAddress(self.lib, name_sentinel.ptr) orelse return null;
            break :win @ptrCast(proc);
        },
        else => self.lib.lookup(T, name),
    };
}

pub fn close(self: *Self) void {
    switch (os) {
        .windows => _ = FreeLibrary(self.lib),
        else => self.lib.close(),
    }
}
