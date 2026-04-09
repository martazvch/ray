const std = @import("std");
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
        lib: std.DynLib,
    },
    unknown_mod: usize,
    missing_file: usize,
};

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

    if (path) |p| {
        const cwd = cwd: {
            if (std.fs.path.isAbsolute(p)) {
                break :cwd std.fs.openDirAbsolute(p, .{}) catch unreachable;
            } else {
                var cwd = std.fs.cwd();
                break :cwd cwd.openDir(p, .{}) catch unreachable;
            }
        };
        return fetchFrom(allocator, cwd, ast, path_chunks, sb);
    }

    @panic("Absolute imports not yet implemented");
}

fn fetchFrom(allocator: Allocator, init_dir: std.fs.Dir, ast: *const Ast, path_chunks: []const Ast.TokenIndex, sb: *Sb) Result {
    var cwd = init_dir;

    for (path_chunks, 0..) |part, i| {
        const name = ast.toSource(part);

        if (i == path_chunks.len - 1) {
            inline for (.{ ".ray", ".dylib" }, .{ rayFile, dynLib }) |ext, func| {
                const file_name = allocator.alloc(u8, name.len + ext.len) catch oom();
                @memcpy(file_name[0..name.len], name);
                @memcpy(file_name[name.len..], ext);

                if (cwd.access(file_name, .{})) {
                    return func(allocator, &cwd, file_name, sb);
                } else |_| {}

                // If not found, we free the buffer
                allocator.free(file_name);
            }
        } else {
            cwd = cwd.openDir(name, .{}) catch return .{ .unknown_mod = part };
            sb.append(allocator, std.fs.path.sep_str);
            sb.append(allocator, name);
        }
    }

    return .{ .missing_file = path_chunks[path_chunks.len - 1] };
}

fn rayFile(allocator: Allocator, cwd: *std.fs.Dir, file_name: []const u8, sb: *Sb) Result {
    const file = cwd.openFile(file_name, .{}) catch unreachable;
    defer file.close();

    // The file has a new line inserted by default
    const size = file.getEndPos() catch @panic("Ray internal error: wrong import file end position");
    const buf = allocator.allocSentinel(u8, size, 0) catch oom();
    _ = file.readAll(buf) catch @panic("Ray internal error: error while reading imported file");

    sb.append(allocator, ".");
    sb.append(allocator, file_name);
    defer sb.popMany(2);

    return .{ .rayfile = .{
        .name = file_name,
        .path = sb.renderAlloc(allocator),
        .content = buf,
    } };
}

fn dynLib(allocator: Allocator, _: *std.fs.Dir, file_name: []const u8, sb: *Sb) Result {
    sb.append(allocator, std.fs.path.sep_str);
    sb.append(allocator, file_name);
    std.log.debug("Path: {s}", .{sb.renderAlloc(allocator)});

    const dynlib = std.DynLib.open(sb.renderAlloc(allocator)) catch unreachable;
    sb.popMany(2);

    sb.append(allocator, ".");
    sb.append(allocator, file_name);
    defer sb.popMany(2);

    return .{ .dynlib = .{
        .name = file_name,
        .path = sb.renderAlloc(allocator),
        .lib = dynlib,
    } };
}
