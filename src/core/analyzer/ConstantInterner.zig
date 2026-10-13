const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const ir = @import("ir.zig");
const Instruction = ir.Instruction;
const TypeId = @import("types.zig").TypeId;
const Language = @import("types.zig").Language;
const ModIdx = @import("../pipeline/ModuleManager.zig").Index;
const misc = @import("misc");
const oom = misc.oom;

hashes: misc.Set(u64),
constants: ArrayList(Constant),

const Self = @This();

pub const Constant = union(enum) {
    null,
    bool: bool,
    int: i64,
    float: f64,
    raw_ptr: usize,
    array: struct { type_id: TypeId, values: []const ConstIdx },
    enum_lit: TagLit,
    struct_lit: StructLit,
    union_lit: TagLit,
    string: misc.Interner.Index,

    pub const StructLit = struct {
        symbol: Instruction.LoadSymbol,
        values: []const ConstIdx,
        lang: Language,
    };

    pub const TagLit = struct {
        symbol: Instruction.LoadSymbol,
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
        return @backingInt(self);
    }

    pub fn fromInt(i: usize) ConstIdx {
        return @fromBackingInt(@intCast(i));
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

    hasher.update(asBytes(&@backingInt(data)));

    switch (data) {
        .null => {},
        .bool => |*i| hasher.update(asBytes(i)),
        .int => |*i| hasher.update(asBytes(i)),
        .float => |*f| hasher.update(asBytes(f)),
        .raw_ptr => |*addr| hasher.update(asBytes(addr)),
        .string => |*s| hasher.update(asBytes(s)),
        .array => |arr| {
            hasher.update(asBytes(&arr.type_id));
            for (arr.values) |v| {
                hasher.update(asBytes(&v));
            }
        },
        .enum_lit, .union_lit => |e| {
            hasher.update(asBytes(&e.symbol.module.toInt()));
            hasher.update(@tagName(data));
            hasher.update(asBytes(&e.symbol.symbol));
            hasher.update(asBytes(&e.tag_index));
        },
        .struct_lit => |s| {
            hasher.update(asBytes(&s.lang));
            hasher.update(asBytes(&s.symbol.symbol));
            hasher.update(asBytes(&s.symbol.module));
            for (s.values) |v| {
                hasher.update(asBytes(&v));
            }
        },
    }

    return hasher.final();
}
