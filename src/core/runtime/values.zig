const std = @import("std");
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;

const Artifacts = @import("../compiler/Artifacts.zig");
const Obj = @import("Obj.zig");
const Vm = @import("Vm.zig");
const oom = @import("misc").oom;

pub const Value = union(enum) {
    bool: bool,
    float: f64,
    int: i64,
    null,
    range: Range,
    obj: *Obj,

    const Self = @This();
    pub const true_: Self = .{ .bool = true };
    pub const false_: Self = .{ .bool = false };
    pub const null_: Self = .{ .null = undefined };
    pub const Range = struct { start: i64, end: i64 };

    pub fn makeBool(value: bool) Self {
        return .{ .bool = value };
    }

    pub fn makeFloat(value: f64) Self {
        return .{ .float = value };
    }

    pub fn makeInt(value: i64) Self {
        return .{ .int = value };
    }

    pub fn makeRange(start: i64, end: i64) Self {
        return .{ .range = .{ .start = start, .end = end } };
    }

    pub fn makeObj(object: *Obj) Self {
        return .{ .obj = object };
    }

    pub fn eq(self: Value, other: Value) bool {
        if (std.meta.activeTag(self) != std.meta.activeTag(other)) return false;

        return switch (self) {
            .bool => |v| v == other.bool,
            .int => |v| v == other.int,
            .float => |v| v == other.float,
            .null => true,
            .range => |v| v.start == other.range.start and v.end == other.range.end,
            .obj => |v| v == other.obj,
        };
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
            .obj => |obj| .makeObj(obj.deepCopy(vm)),
            else => self,
        };
    }

    pub fn print(self: *const Self, artifacts: *const Artifacts, writer: *Writer) void {
        switch (self.*) {
            .bool => |v| writer.print("{}", .{v}) catch oom(),
            .float => |v| writer.print("{d}", .{v}) catch oom(),
            .int => |v| writer.print("{}", .{v}) catch oom(),
            .null => writer.print("null", .{}) catch oom(),
            .range => |v| writer.print("{}..{}", .{ v.start, v.end }) catch oom(),
            .obj => |v| v.print(artifacts, writer) catch oom(),
        }
    }
};
