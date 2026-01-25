const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const ir = @import("ir.zig");
const Instruction = ir.Instruction;
const misc = @import("misc");
const oom = misc.oom;

hashes: misc.Set(u64),
constants: ArrayList(Constant),

const Self = @This();

pub const Constant = union(enum) {
    int: i64,
    float: f64,
    bool: bool,
    enum_instance: Instruction.EnumCreate,
    null,
    string: misc.Interner.Index,
};

// u16 is enough for now because maximum is u8 in VM
pub const ConstIdx = enum(u16) {
    true,
    false,
    null,
    _,

    pub fn toInt(self: ConstIdx) usize {
        return @intFromEnum(self);
    }

    pub fn fromInt(i: usize) ConstIdx {
        return @enumFromInt(i);
    }
};

pub const empty: Self = .{
    .hashes = .empty,
    .constants = .empty,
};

pub fn init(allocator: Allocator) Self {
    var self: Self = .empty;
    _ = self.add(allocator, .{ .bool = true });
    _ = self.add(allocator, .{ .bool = false });
    _ = self.add(allocator, .{ .null = {} });

    return self;
}

pub fn add(self: *Self, allocator: Allocator, cte: Constant) ConstIdx {
    const hashed = hash(cte);

    if (self.hashes.getIndex(hashed)) |index| {
        return .fromInt(index);
    }

    self.hashes.add(allocator, hashed) catch oom();
    self.constants.append(allocator, cte) catch oom();

    return .fromInt(self.hashes.count() - 1);
}

fn hash(data: Constant) u64 {
    var hasher = std.hash.Wyhash.init(0);
    const asBytes = std.mem.asBytes;

    hasher.update(asBytes(&@intFromEnum(data)));

    switch (data) {
        .bool => |*i| hasher.update(asBytes(i)),
        .int => |*i| hasher.update(asBytes(i)),
        .float => |*f| hasher.update(asBytes(f)),
        .string => |*s| hasher.update(asBytes(s)),
        .null => {},
        .enum_instance => |e| {
            hasher.update(asBytes(&e.sym.module_index));
            hasher.update(asBytes(&e.sym.symbol_index));
            hasher.update(asBytes(&e.tag_index));
        },
    }

    return hasher.final();
}
