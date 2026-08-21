const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const NativeLib = @import("NativeLib.zig");
const NativeModule = @import("../pipeline/NativesRegister.zig").NativeModule;
const State = @import("../pipeline/State.zig");

const Ast = @import("../parser/Ast.zig");

const misc = @import("misc");
const InternerIdx = misc.Interner.Index;
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
        lib: NativeLib,
        token: usize,
    },
    module: struct {
        path: InternerIdx,
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
    io: std.Io,
    alloc: Allocator,
    ast: *const Ast,
    path_chunks: []const Ast.TokenIndex,
    state: *State,
) Result {
    // Relative imports
    if (ast.token_tags[path_chunks[0]] == .dot) {
        // TODO: could it be only a dot? And thus it would break at the [1..]
        return fetchFrom(io, alloc, &state.cwd, ast, path_chunks[1..], &state.path_builder);
    }

    // Absolute imports (from std for example)
    if (state.modules.getFromPath(state.interner.intern(ast.toSource(path_chunks[0])))) |mod| {
        return .{ .module = .{ .path = mod.path } };
    }

    // Import from CLI additional path
    // TODO: error
    if (state.config.path) |p| {
        state.cwd = cwd: {
            if (std.fs.path.isAbsolute(p)) {
                break :cwd std.Io.Dir.openDirAbsolute(io, p, .{}) catch unreachable;
            } else {
                var new_cwd = std.Io.Dir.cwd();
                break :cwd new_cwd.openDir(io, p, .{}) catch unreachable;
            }
        };

        // TODO: won't work with absolute path
        state.path_builder.append(alloc, p);

        return fetchFrom(io, alloc, &state.cwd, ast, path_chunks, &state.path_builder);
    }

    // Import from native std modules
    if (path_chunks.len > 1) {
        @panic("Absolute import of length > 1 are not implemented yet");
    }

    @panic("Absolute imports not yet implemented");
}

fn fetchFrom(
    io: std.Io,
    alloc: Allocator,
    cwd: *std.Io.Dir,
    ast: *const Ast,
    path_chunks: []const Ast.TokenIndex,
    path: *Sb,
) Result {
    for (path_chunks, 0..) |part, i| {
        if (ast.token_tags[part] == .hat) {
            _ = path.pop();
            cwd.* = cwd.openDir(io, "..", .{}) catch unreachable;
            continue;
        }

        const name = ast.toSource(part);

        if (i == path_chunks.len - 1) {
            // Ray module
            {
                path.append(alloc, name);
                defer _ = path.pop();
                const file_name = std.fmt.allocPrint(alloc, "{s}.{s}", .{ name, "ray" }) catch oom();

                if (cwd.access(io, file_name, .{})) {
                    return .{ .rayfile = .{
                        .name = file_name,
                        .path = path.renderAlloc(alloc, .{ .sep = std.fs.path.sep_str }),
                        .content = readFile(io, alloc, cwd, file_name),
                    } };
                } else |_| {}
            }

            // Native module
            {
                const file_name = std.fmt.allocPrint(alloc, "{s}.{s}", .{ name, "rayn" }) catch oom();

                if (cwd.access(io, file_name, .{})) {
                    const lib = NativeLib.open(
                        alloc,
                        path.renderAlloc(alloc, .{ .sep = std.Io.Dir.path.sep_str }),
                        name,
                    ) catch |e| switch (e) {
                        error.UnsupportedOS => return .unsupported_os,
                        error.LoadFailed => return .{ .missing_dynlib_file = part },
                    };

                    // We add the name after fetching the lib to avoid duplicate name
                    path.append(alloc, name);

                    return .{ .dynlib = .{
                        .name = file_name,
                        .path = path.renderAlloc(alloc, .{ .sep = std.fs.path.sep_str }),
                        .rayn_content = readFile(io, alloc, cwd, file_name),
                        .lib = lib,
                        .token = part,
                    } };
                } else |_| {}
            }
        } else {
            cwd.* = cwd.openDir(io, name, .{}) catch return .{ .unknown_mod = part };
            path.append(alloc, name);
        }
    }

    return .{ .missing_file = path_chunks[path_chunks.len - 1] };
}

fn readFile(io: std.Io, alloc: Allocator, cwd: *std.Io.Dir, file_name: []const u8) [:0]const u8 {
    return cwd.readFileAllocOptions(io, file_name, alloc, .unlimited, .of(u8), 0) catch unreachable;
}
