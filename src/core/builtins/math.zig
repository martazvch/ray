const std = @import("std");
const math = std.math;
const zffi = @import("../ffi/zffi.zig");
const Value = @import("../runtime/values.zig").Value;
const Vm = @import("../runtime/Vm.zig");

const Module = @This();

pub const module: zffi.Module = .{
    .name = "math",
    .functions = &.{
        .init(Module, "abs", "", &.{.{ .name = "x" }}),
        .init(Module, "ceil", "", &.{.{ .name = "x" }}),
        .init(Module, "floor", "", &.{.{ .name = "x" }}),
        .init(Module, "round", "", &.{.{ .name = "x" }}),
        .init(Module, "sign", "", &.{.{ .name = "x" }}),
        .init(Module, "sqrt", "", &.{.{ .name = "x" }}),
        .init(Module, "powi", "", &.{
            .{ .name = "x" },
            .{ .name = "y" },
        }),
        .init(Module, "powf", "", &.{
            .{ .name = "x" },
            .{ .name = "y" },
        }),
        .init(Module, "exp", "", &.{.{ .name = "x" }}),
        .init(Module, "exp2", "", &.{.{ .name = "x" }}),
        .init(Module, "log", "", &.{.{ .name = "x" }}),
        .init(Module, "log2", "", &.{.{ .name = "x" }}),
        .init(Module, "log10", "", &.{.{ .name = "x" }}),
        .init(Module, "cos", "", &.{.{ .name = "x" }}),
        .init(Module, "sin", "", &.{.{ .name = "x" }}),
        .init(Module, "tan", "", &.{.{ .name = "x" }}),
        .init(Module, "acos", "", &.{.{ .name = "x" }}),
        .init(Module, "asin", "", &.{.{ .name = "x" }}),
        .init(Module, "atan", "", &.{.{ .name = "x" }}),
        .init(Module, "atan2", "", &.{
            .{ .name = "x" },
            .{ .name = "y" },
        }),
        .init(Module, "degToRad", "", &.{.{ .name = "x" }}),
        .init(Module, "radToDeg", "", &.{.{ .name = "x" }}),
    },
    .globals = &.{
        .init(Module, "e", ""),
        .init(Module, "pi", ""),
        .init(Module, "tau", ""),
        .init(Module, "sqrt2", ""),
        .init(Module, "sqrt1_2", ""),
        .init(Module, "degPerRad", ""),
        .init(Module, "radPerDeg", ""),
    },
};

pub const e: zffi.Float = math.e;
pub const pi: zffi.Float = math.pi;
pub const tau: zffi.Float = math.tau;
pub const sqrt2: zffi.Float = math.sqrt2;
pub const sqrt1_2: zffi.Float = math.sqrt1_2;
pub const degPerRad: zffi.Float = math.deg_per_rad;
pub const radPerDeg: zffi.Float = math.rad_per_deg;

pub fn abs(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |i| @floatFromInt(@abs(i)),
        .float => |f| @abs(f),
    };
}

pub fn ceil(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |i| @ceil(@floatFromInt(i)),
        .float => |f| @ceil(f),
    };
}

pub fn floor(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |i| @floor(@floatFromInt(i)),
        .float => |f| @floor(f),
    };
}

pub fn round(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |i| @round(@floatFromInt(i)),
        .float => |f| @round(f),
    };
}

pub fn sign(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Int {
    return switch (x) {
        .int => |i| math.sign(i),
        .float => |f| math.sign(f),
    };
}

pub fn sqrt(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |i| @sqrt(@floatFromInt(i)),
        .float => |f| @sqrt(f),
    };
}

pub fn powi(_: *Vm, x: zffi.Int, y: zffi.Int) zffi.Int {
    return math.pow(i64, x, y);
}

pub fn powf(_: *Vm, x: zffi.Float, y: zffi.Float) zffi.Float {
    return math.pow(f64, x, y);
}

pub fn exp(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |i| @exp(@floatFromInt(i)),
        .float => |f| @exp(f),
    };
}

pub fn exp2(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |i| @exp2(@floatFromInt(i)),
        .float => |f| @exp2(f),
    };
}

pub fn log(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |i| @log(@floatFromInt(i)),
        .float => |f| @log(f),
    };
}

pub fn log2(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |i| @log2(@floatFromInt(i)),
        .float => |f| @log2(f),
    };
}

pub fn log10(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |i| @log10(@floatFromInt(i)),
        .float => |f| @log10(f),
    };
}

pub fn cos(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |i| @cos(@floatFromInt(i)),
        .float => |f| @cos(f),
    };
}

pub fn sin(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |i| @sin(@floatFromInt(i)),
        .float => |f| @sin(f),
    };
}

pub fn tan(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |i| @tan(@floatFromInt(i)),
        .float => |f| @tan(f),
    };
}

pub fn acos(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |i| math.acos(@as(f64, @floatFromInt(i))),
        .float => |f| math.acos(f),
    };
}

pub fn asin(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |i| math.asin(@as(f64, @floatFromInt(i))),
        .float => |f| math.asin(f),
    };
}

pub fn atan(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |i| math.atan(@as(f64, @floatFromInt(i))),
        .float => |f| math.atan(f),
    };
}

pub fn atan2(_: *Vm, y: zffi.Union(&.{ .int, .float }), x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |xi| switch (y) {
            .int => |yi| math.atan2(@as(f64, @floatFromInt(yi)), @as(f64, @floatFromInt(xi))),
            .float => |yf| math.atan2(yf, @as(f64, @floatFromInt(xi))),
        },
        .float => |xf| switch (y) {
            .int => |yi| math.atan2(@as(f64, @floatFromInt(yi)), xf),
            .float => |yf| math.atan2(yf, xf),
        },
    };
}

pub fn degToRad(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |i| math.degreesToRadians(@as(f64, @floatFromInt(i))),
        .float => |f| math.degreesToRadians(f),
    };
}

pub fn radToDeg(_: *Vm, x: zffi.Union(&.{ .int, .float })) zffi.Float {
    return switch (x) {
        .int => |i| math.radiansToDegrees(@as(f64, @floatFromInt(i))),
        .float => |f| math.radiansToDegrees(f),
    };
}
