const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayListUnmanaged;
const oom = @import("misc").oom;
const Obj = @import("../runtime/Obj.zig");
const Value = @import("../runtime/values.zig").Value;
const TypeId = @import("../analyzer/types.zig").TypeId;
const Linker = @import("Linker.zig");
const ModIndex = @import("../pipeline/ModuleManager.zig").Index;

/// Compiled values used at runtime
globals: ArrayList(Value) = .empty,
/// Compiled constants used at runtime
constants: ArrayList(Value) = .empty,

funcs: ArrayList(*Obj.Function) = .empty,
zig_funcs: ArrayList(*Obj.ZigFn) = .empty,
c_funcs: ArrayList(*Obj.CFn) = .empty,
structs: ArrayList(Structure) = .empty,
c_structs: ArrayList(CStructure) = .empty,
enums: ArrayList(Enum) = .empty,
unions: ArrayList(Union) = .empty,
vtables: ArrayList(VTable) = .empty,

linker: Linker = .empty,

pub const Enum = struct {
    name: []const u8,
    tags: []const []const u8,
    discriminants: []const i64,
    type_id: TypeId,
};

pub const Union = struct {
    name: []const u8,
    tags: []const []const u8,
    type_id: TypeId,
    is_err: bool,
};

pub const Structure = struct {
    name: []const u8,
    type_id: TypeId,
    fields: []const []const u8,
};

pub const CStructure = struct {
    name: []const u8,
    type_id: TypeId,
    layout: Layout,

    pub const Layout = struct {
        size: usize,
        alignment: usize,
        fields: []const Field,

        pub const Field = struct {
            offset: usize,
            kind: Kind,

            pub const Kind = enum { u8 };
        };
    };
};

pub const VTable = struct {
    name: []const u8,
    functions: []*Obj.Function,
};

pub const Tag = enum {
    constant,
    global,
    function,
    c_function,
    zig_function,
    structure,
    c_structure,
    @"enum",
    @"union",
    vtable,
};
pub const Self = @This();

pub fn add(self: *Self, alloc: Allocator, comptime tag: Tag, module: ModIndex, symbol: usize, value: SymbolType(tag)) void {
    self.linker.put(
        alloc,
        .{ .tag = tag, .module = module.toInt(), .symbol = symbol },
        self.newIndex(tag),
    );

    self.getSymbolList(tag).append(alloc, value) catch oom();
}

pub fn addPlaceholder(self: *Self, alloc: Allocator, comptime tag: Tag, module: ModIndex, symbol: usize) usize {
    const index = self.newIndex(tag);

    self.linker.put(
        alloc,
        .{ .tag = tag, .module = module.toInt(), .symbol = symbol },
        index,
    );

    self.getSymbolList(tag).append(alloc, undefined) catch oom();

    return index;
}

pub fn get(self: *const Self, comptime tag: Tag, index: usize) *SymbolType(tag) {
    return &self.getSymbolListConst(tag).items[index];
}

pub fn set(self: *const Self, comptime tag: Tag, index: usize, value: SymbolType(tag)) void {
    self.getSymbolListConst(tag).items[index] = value;
}

pub fn getFromKey(self: *const Self, comptime tag: Tag, module: ModIndex, symbol: usize) *SymbolType(tag) {
    const index = self.linker.get(.{ .tag = tag, .module = module.toInt(), .symbol = symbol });
    return &self.getSymbolListConst(tag).items[index];
}

pub fn getIndex(self: *const Self, comptime tag: Tag, module: ModIndex, symbol: usize) usize {
    return self.linker.get(.{ .tag = tag, .module = module.toInt(), .symbol = symbol });
}

fn SymbolType(comptime tag: Tag) type {
    return switch (tag) {
        .constant => Value,
        .global => Value,
        .function => *Obj.Function,
        .c_function => *Obj.CFn,
        .zig_function => *Obj.ZigFn,
        .structure => Structure,
        .c_structure => CStructure,
        .@"enum" => Enum,
        .@"union" => Union,
        .vtable => VTable,
    };
}

pub fn newIndex(self: *const Self, comptime tag: Tag) usize {
    return self.getSymbolListConst(tag).items.len;
}

fn getSymbolList(self: *Self, comptime tag: Tag) *ArrayList(SymbolType(tag)) {
    return switch (tag) {
        .constant => &self.constants,
        .global => &self.globals,
        .function => &self.funcs,
        .c_function => &self.c_funcs,
        .zig_function => &self.zig_funcs,
        .structure => &self.structs,
        .c_structure => &self.c_structs,
        .@"enum" => &self.enums,
        .@"union" => &self.unions,
        .vtable => &self.vtables,
    };
}

fn getSymbolListConst(self: *const Self, comptime tag: Tag) *const ArrayList(SymbolType(tag)) {
    return switch (tag) {
        .constant => &self.constants,
        .global => &self.globals,
        .function => &self.funcs,
        .c_function => &self.c_funcs,
        .zig_function => &self.zig_funcs,
        .structure => &self.structs,
        .c_structure => &self.c_structs,
        .@"enum" => &self.enums,
        .@"union" => &self.unions,
        .vtable => &self.vtables,
    };
}
