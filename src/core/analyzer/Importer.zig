const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

const Ast = @import("../parser/Ast.zig");

const misc = @import("misc");
const Sb = misc.StringBuilder;
const oom = misc.oom;

const Self = @This();
pub const Result = union(enum) {
    rayfile: struct {
        name: []const u8,
        path: []const u8,
        content: [:0]const u8,
    },
    dynlib: struct {
        name: []const u8,
        path: []const u8,
        rayn_content: [:0]const u8,
        lib: std.DynLib,
        token: usize,
    },
    missing_file: usize,
    missing_dynlib_file: usize,
    unknown_mod: usize,
    unsupported_os,
};
const Error = error{UnsupportedOS};

/// Import rules and order
/// - If path starts with a '.', consider it as a relative path and fails if not found
/// - If path starts with an identifier, consider it as an absolute path from where the process was invoked
///     If fails, tries to fetch from `path` cli option if provided, otherwise consider it as an imported package
/// - If considered as a package, look for it in package place TODO:
///
/// Naming rules
/// - Last identifier is the file to import
///
/// **Caller owns memory of result**
pub fn fetchImportedFile(
    allocator: Allocator,
    ast: *const Ast,
    path_chunks: []const Ast.TokenIndex,
    path: ?[]const u8,
    sb: *Sb,
) Result {
    if (ast.token_tags[path_chunks[0]] == .dot) {
        var buf_path: [std.fs.max_path_bytes]u8 = undefined;
        const buf_written = sb.render(&buf_path);
        const cwd = std.fs.openDirAbsolute(buf_written, .{}) catch unreachable;

        // TODO: could it be only a dot? And thus it would break at the [1..]
        return fetchFrom(allocator, cwd, ast, path_chunks[1..], sb);
    }

    // TODO: error
    if (path) |p| {
        const cwd = cwd: {
            if (std.fs.path.isAbsolute(p)) {
                break :cwd std.fs.openDirAbsolute(p, .{}) catch unreachable;
            } else {
                var cwd = std.fs.cwd();
                break :cwd cwd.openDir(p, .{}) catch unreachable;
            }
        };

        // TODO: won't work with absolute path
        sb.append(allocator, p);
        defer _ = sb.pop();

        return fetchFrom(allocator, cwd, ast, path_chunks, sb);
    }

    @panic("Absolute imports not yet implemented");
}

fn fetchFrom(allocator: Allocator, init_dir: std.fs.Dir, ast: *const Ast, path_chunks: []const Ast.TokenIndex, sb: *Sb) Result {
    var cwd = init_dir;

    for (path_chunks, 0..) |part, i| {
        const name = ast.toSource(part);

        if (i == path_chunks.len - 1) {
            // Ray module
            {
                sb.append(allocator, name);
                defer _ = sb.pop();
                const file_name = std.fmt.allocPrint(allocator, "{s}.{s}", .{ name, "ray" }) catch oom();

                if (cwd.access(file_name, .{})) {
                    return .{ .rayfile = .{
                        .name = file_name,
                        .path = sb.renderAlloc(allocator),
                        .content = readFile(allocator, &cwd, file_name),
                    } };
                } else |_| {}
            }

            // Native module
            {
                const file_name = std.fmt.allocPrint(allocator, "{s}.{s}", .{ name, "rayn" }) catch oom();

                if (cwd.access(file_name, .{})) {
                    const lib = dynLib(allocator, name, sb) catch |e| switch (e) {
                        error.UnsupportedOS => return .unsupported_os,
                        else => return .{ .missing_dynlib_file = part },
                    };

                    // We add the name after fetching the lib to avoid duplicate name
                    sb.append(allocator, name);
                    return .{ .dynlib = .{
                        .name = file_name,
                        .path = sb.renderAlloc(allocator),
                        .rayn_content = readFile(allocator, &cwd, file_name),
                        .lib = lib,
                        .token = part,
                    } };
                } else |_| {}
            }
        } else {
            cwd = cwd.openDir(name, .{}) catch return .{ .unknown_mod = part };
            sb.append(allocator, name);
        }
    }

    return .{ .missing_file = path_chunks[path_chunks.len - 1] };
}

fn readFile(allocator: Allocator, cwd: *std.fs.Dir, file_name: []const u8) [:0]const u8 {
    return cwd.readFileAllocOptions(allocator, file_name, 100_000, null, .of(u8), 0) catch unreachable;
}

fn dynLib(allocator: Allocator, file_name: []const u8, sb: *Sb) !std.DynLib {
    const dynlib_name = try libName(allocator, file_name);
    sb.append(allocator, dynlib_name);
    defer _ = sb.pop();

    return std.DynLib.open(sb.renderWithSepAlloc(allocator, std.fs.path.sep_str));
}

fn libName(allocator: Allocator, name: []const u8) Error![]const u8 {
    return switch (builtin.os.tag) {
        .linux => std.fmt.allocPrint(allocator, "lib{s}.so", .{name}) catch oom(),
        .macos => std.fmt.allocPrint(allocator, "lib{s}.dylib", .{name}) catch oom(),
        .windows => std.fmt.allocPrint(allocator, "{s}.dll", .{name}) catch oom(),
        else => error.UnsupportedOS,
    };
}
