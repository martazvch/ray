const std = @import("std");
const Translator = @import("translate_c").Translator;

pub fn build(b: *std.Build) !void {
    const options = b.addOptions();

    const print_instr = b.option(bool, "print-instr", "prints the current instruction") orelse false;
    options.addOption(bool, "print_instr", print_instr);

    const print_stack = b.option(bool, "print-stack", "prints the stack on each instruction") orelse false;
    options.addOption(bool, "print_stack", print_stack);

    const log_gc = b.option(bool, "log-gc", "logs each GC actions (alloc and free)") orelse false;
    options.addOption(bool, "log_gc", log_gc);

    const stress_gc = b.option(bool, "stress-gc", "logs each GC actions (alloc, free, mark, sweep, ...)") orelse false;
    options.addOption(bool, "stress_gc", stress_gc);

    const test_mode = b.option(bool, "test-mode", "Compiles in test mode to enable certain behaviors") orelse false;
    options.addOption(bool, "test_mode", test_mode);

    const gen_lib = b.option(bool, "gen-embed", "Generates the embedabble dynamic library") orelse false;
    options.addOption(bool, "gen_lib", gen_lib);

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // --------------
    //  Dependencies
    // --------------
    const clarg = b.dependency("clarg", .{
        .target = target,
        .optimize = optimize,
    });

    const libffi = b.dependency("libffi", .{
        .target = target,
        .optimize = optimize,
    });
    const ffi = libffi.artifact("ffi");

    const translate_c = b.dependency("translate_c", .{});
    const translator = Translator.init(translate_c, .{
        .c_source_file = b.path("src/core/ffi/libffi.h"),
        .optimize = optimize,
        .target = target,
    });

    // ffi.h is generated + installed by libffi package
    translator.addIncludePath(ffi.getEmittedIncludeTree());

    // ---------
    //  Modules
    // ---------
    const ray_mod = b.addModule("ray", .{
        .optimize = optimize,
        .target = target,
        .root_source_file = b.path("src/main.zig"),
    });

    const misc_mod = b.createModule(.{
        .optimize = optimize,
        .target = target,
        .root_source_file = b.path("src/misc/misc.zig"),
    });

    const libffi_mod = b.createModule(.{
        .optimize = optimize,
        .target = target,
        .root_source_file = b.path("src/core/libffi.zig"),
    });
    libffi_mod.addImport("ffi", translator.mod);
    libffi_mod.linkLibrary(ffi);

    const core_mod = b.createModule(.{
        .optimize = optimize,
        .target = target,
        .root_source_file = b.path("src/core/core.zig"),
        .imports = &.{
            .{ .name = "misc", .module = misc_mod },
            .{ .name = "options", .module = options.createModule() },
        },
    });

    // ------------
    //  Executable
    // ------------
    const exe = b.addExecutable(.{
        .name = "ray",
        .root_module = ray_mod,
    });
    exe.root_module.addImport("clarg", clarg.module("clarg"));
    exe.root_module.addImport("misc", misc_mod);
    exe.root_module.addImport("libffi", libffi_mod);
    exe.root_module.addOptions("options", options);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    run_cmd.addPassthruArgs();

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // -------
    //  Embed
    // -------
    const embed_mod = b.addModule("embed", .{
        .target = target,
        .optimize = optimize,
        .root_source_file = b.path("src/embed/zig.zig"),
        .imports = &.{
            .{ .name = "core", .module = core_mod },
            .{ .name = "misc", .module = misc_mod },
            .{ .name = "options", .module = options.createModule() },
        },
    });
    _ = embed_mod;

    const embed_c_lib = b.addLibrary(.{
        .name = "ray",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/embed/c.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "core", .module = core_mod },
                .{ .name = "misc", .module = misc_mod },
                .{ .name = "options", .module = options.createModule() },
            },
        }),
        .linkage = .dynamic,
    });

    if (gen_lib) {
        b.installArtifact(embed_c_lib);
    }

    // --------
    // For ZLS
    // --------
    const exe_check = b.addExecutable(.{
        .name = "foo",
        .root_module = ray_mod,
    });
    exe_check.root_module.addImport("clarg", clarg.module("clarg"));
    exe_check.root_module.addImport("misc", misc_mod);
    exe_check.root_module.addImport("libffi", libffi_mod);
    exe_check.root_module.addOptions("options", options);

    const check = b.step("check", "Check if foo compiles");
    check.dependOn(&exe_check.step);

    // -------
    //  Tests
    // -------
    const test_step = b.step("test", "Run unit tests");

    // Unit tests
    const exe_tests = b.addTest(.{ .root_module = exe.root_module });
    const run_exe_tests = b.addRunArtifact(exe_tests);
    test_step.dependOn(&run_exe_tests.step);

    // Unit tests on misc module
    const misc_tests = b.addTest(.{ .root_module = misc_mod });
    const run_misc_tests = b.addRunArtifact(misc_tests);
    test_step.dependOn(&run_misc_tests.step);

    // Custom unit tests runner for language
    const tester_mod = b.createModule(.{
        .target = target,
        .optimize = optimize,
        .root_source_file = b.path("tests/tester.zig"),
    });
    const tester_exe = b.addExecutable(.{
        .name = "ray-tester",
        .root_module = tester_mod,
    });

    tester_exe.root_module.addImport("clarg", clarg.module("clarg"));
    const install_tester = b.addInstallArtifact(tester_exe, .{});
    const run_tester = b.addRunArtifact(tester_exe);
    run_tester.step.dependOn(&install_tester.step);
    run_tester.step.dependOn(b.getInstallStep());

    // C module needs to build dynamic library
    buildC(b, test_step, "cmodule", "module.c", target, optimize);
    buildC(b, test_step, "not_cmodule", "not_module.c", target, optimize);
    buildC(b, test_step, "invalid_cmodule", "invalid_module.c", target, optimize);

    run_tester.addPassthruArgs();
    test_step.dependOn(&run_tester.step);

    // Zig embedded tests
    var embed_tests = b.addSystemCommand(
        &.{ "zig", "build", "test" },
    );
    embed_tests.setCwd(b.path(b.pathJoin(&.{ "tests", "embed", "zig" })));
    test_step.dependOn(&embed_tests.step);

    // C embedded tests
    const embed_c_static = b.addLibrary(.{
        .name = "ray",
        .root_module = embed_c_lib.root_module,
        .linkage = .static,
    });

    const c_embed_test_exe = b.addExecutable(.{
        .name = "c-tester",
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    c_embed_test_exe.root_module.addCSourceFiles(.{
        .root = b.path("tests/embed/c"),
        .files = &.{ "main.c", "reader.c", "tester.c" },
        .flags = &.{ "-Wall", "-Wextra", "-std=c99" },
    });
    c_embed_test_exe.root_module.addIncludePath(b.path("tests/embed/c"));
    c_embed_test_exe.root_module.addIncludePath(b.path("src/embed"));
    c_embed_test_exe.root_module.linkLibrary(embed_c_static);

    const run_c_embed_test = b.addRunArtifact(c_embed_test_exe);
    run_c_embed_test.setCwd(b.path("tests/embed/c"));
    test_step.dependOn(&run_c_embed_test.step);
}

fn buildC(
    b: *std.Build,
    test_step: *std.Build.Step,
    name: []const u8,
    file_name: []const u8,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
) void {
    const cmodule_lib = b.addLibrary(.{
        .name = name,
        .linkage = .dynamic,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    cmodule_lib.root_module.addCSourceFiles(.{
        .root = b.path("tests/data/cmodule/"),
        .files = &.{file_name},
        .flags = &.{ "-Wall", "-Wextra", "-std=c99" },
    });
    cmodule_lib.root_module.addIncludePath(b.path("tests/embed/c"));
    const install_c_module = b.addInstallArtifact(cmodule_lib, .{
        .dest_dir = .{ .override = .{ .custom = "../tests/data/cmodule" } },
    });
    test_step.dependOn(&install_c_module.step);
}
