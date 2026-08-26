const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const ir = @import("ir.zig");
const Instruction = ir.Instruction;
const TypeId = @import("types.zig").TypeId;
const ModIdx = @import("../pipeline/ModuleManager.zig").Index;
const misc = @import("misc");
const oom = misc.oom;

hashes: misc.Set(u64),
constants: ArrayList(Constant),

const Self = @This();

pub const Constant = union(enum) {
    int: i64,
    float: f64,
    bool: bool,
    array: struct { type_id: TypeId, values: []const ConstIdx },
    enum_lit: TagLit,
    struct_lit: struct {
        parent: struct { symbol: usize, module: ModIdx },
        values: []const ConstIdx,
    },
    union_lit: TagLit,
    null,
    string: misc.Interner.Index,

    pub const TagLit = struct {
        sym: Instruction.LoadSymbol,
        tag_index: usize,
    };
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

const empty: Self = .{
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

pub fn get(self: *Self, index: ConstIdx) Constant {
    return self.constants.items[index.toInt()];
}

fn hash(data: Constant) u64 {
    var hasher = std.hash.Wyhash.init(0);
    const asBytes = std.mem.asBytes;

    hasher.update(asBytes(&@intFromEnum(data)));

    switch (data) {
        .array => |arr| {
            hasher.update(asBytes(&arr.type_id));
            for (arr.values) |v| {
                hasher.update(asBytes(&v));
            }
        },
        .bool => |*i| hasher.update(asBytes(i)),
        .int => |*i| hasher.update(asBytes(i)),
        .float => |*f| hasher.update(asBytes(f)),
        .string => |*s| hasher.update(asBytes(s)),
        .null => {},
        .enum_lit, .union_lit => |e| {
            hasher.update(asBytes(&e.sym.module.toInt()));
            hasher.update(@tagName(data));
            hasher.update(asBytes(&e.sym.symbol));
            hasher.update(asBytes(&e.tag_index));
        },
        .struct_lit => |s| {
            hasher.update(asBytes(&s.parent.symbol));
            hasher.update(asBytes(&s.parent.module));
            for (s.values) |v| {
                hasher.update(asBytes(&v));
            }
        },
    }

    return hasher.final();
}
