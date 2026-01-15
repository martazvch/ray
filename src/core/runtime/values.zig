const std = @import("std");
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;

const Obj = @import("Obj.zig");
const Vm = @import("Vm.zig");
const oom = @import("misc").oom;

pub const Value = union(enum) {
    bool: bool,
    float: f64,
    int: i64,
    null,
    range_float: RangeFloat,
    range_int: RangeInt,
    obj: *Obj,

    const Self = @This();
    pub const true_: Self = .{ .bool = true };
    pub const false_: Self = .{ .bool = false };
    pub const null_: Self = .{ .null = undefined };
    pub const RangeFloat = struct { start: f64, end: f64 };
    pub const RangeInt = struct { start: i64, end: i64 };

    pub fn makeBool(value: bool) Self {
        return .{ .bool = value };
    }

    pub fn makeFloat(value: f64) Self {
        return .{ .float = value };
    }

    pub fn makeInt(value: i64) Self {
        return .{ .int = value };
    }

    pub fn makeRangeFloat(start: f64, end: f64) Self {
        return .{ .range_float = .{ .start = start, .end = end } };
    }

    pub fn makeRangeInt(start: i64, end: i64) Self {
        return .{ .range_int = .{ .start = start, .end = end } };
    }

    pub fn makeObj(object: *Obj) Self {
        return .{ .obj = object };
    }

    // Safety garenteed by the analyzer
    pub fn not(self: *Self) void {
        self.bool = !self.bool;
    }

    pub fn asObj(self: *const Self) ?*Obj {
        return switch (self.*) {
            .obj => |v| v,
            else => null,
        };
    }

    pub fn deepCopy(self: Self, vm: *Vm) Self {
        return switch (self) {
            .bool, .float, .int, .null, .range_float, .range_int => self,
            .obj => |obj| Self.makeObj(obj.deepCopy(vm)),
        };
    }

    pub fn print(self: *const Self, writer: *Writer) void {
        switch (self.*) {
            .bool => |v| writer.print("{}", .{v}) catch oom(),
            .float => |v| writer.print("{d}", .{v}) catch oom(),
            .int => |v| writer.print("{}", .{v}) catch oom(),
            .null => writer.print("null", .{}) catch oom(),
            .range_float => |v| writer.print("{}..{}", .{ v.start, v.end }) catch oom(),
            .range_int => |v| writer.print("{}..{}", .{ v.start, v.end }) catch oom(),
            .obj => |v| v.print(writer) catch oom(),
        }
    }
};
