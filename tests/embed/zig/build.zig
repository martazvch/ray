const std = @import("std");

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

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const ray_embedded_mod = b.addModule("ray_embedded", .{
        .optimize = optimize,
        .target = target,
        .root_source_file = b.path("src/main.zig"),
    });

    const exe = b.addExecutable(.{
        .name = "ray_embedded",
        .root_module = ray_embedded_mod,
    });

    // -----
    //  Ray
    // -----
    const ray_dep = b.dependency("ray", .{
        .target = target,
        .optimize = optimize,
        .@"print-instr" = print_instr,
        .@"print-stack" = print_stack,
        .@"log-gc" = log_gc,
        .@"stress-gc" = stress_gc,
    });
    const ray_mod = ray_dep.module("embed");

    exe.root_module.addImport("ray", ray_mod);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // --------
    // For ZLS
    // --------
    const exe_check = b.addExecutable(.{
        .name = "foo",
        .root_module = ray_embedded_mod,
    });
    exe_check.root_module.addImport("ray", ray_mod);

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
}
