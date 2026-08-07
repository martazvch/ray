const std = @import("std");
const options = @import("options");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;

const Value = @import("../runtime/values.zig").Value;
const Obj = @import("../runtime/Obj.zig");
const oom = @import("misc").oom;
const Chunk = @import("Chunk.zig");
const OpCode = Chunk.OpCode;
const CompiledMod = @import("../pipeline/ModuleManager.zig").Module;
const NativeMod = @import("../pipeline/NativesRegister.zig").NativeModule;

chunk: *const Chunk,
/// Current module's zig functions
zig_fns: []const *Obj.ZigFn,
/// Current module's zig structures
zig_structs: []const CompiledMod.Structure,
/// Current module's foreign functions
foreign_fns: []const *Obj.ForeignFn,
render_mode: RenderMode,
module: *const CompiledMod,
wide: bool,

prev_line: usize = 0,

const Self = @This();
pub const RenderMode = enum { normal, @"test" };

pub fn init(
    chunk: *const Chunk,
    module: *const CompiledMod,
    zig_fns: []const *Obj.ZigFn,
    zig_structs: []const CompiledMod.Structure,
    foreign_fns: []const *Obj.ForeignFn,
) Self {
    return .{
        .chunk = chunk,
        .render_mode = if (options.test_mode) .@"test" else .normal,
        .zig_fns = zig_fns,
        .zig_structs = zig_structs,
        .foreign_fns = foreign_fns,
        .module = module,
        .wide = false,
    };
}

pub fn disChunk(self: *Self, writer: *Writer, name: []const u8) void {
    self.disSlice(writer, name, 0);
}

pub fn disSlice(self: *Self, writer: *Writer, name: []const u8, start: usize) void {
    writer.print("-- {s} --\n", .{name}) catch oom();

    var i: usize = start;
    while (i < self.chunk.code.items.len) {
        i = self.disInstruction(writer, i);
    }
}

fn lineHeader(self: *Self, writer: *Writer, offset: usize) !void {
    if (self.render_mode == .normal) {
        writer.print(" {:0>4}  ", .{offset}) catch oom();

        const line = self.chunk.offsets.items[offset];
        if (line > self.prev_line) {
            writer.print("{:>4}  ", .{line}) catch oom();
            self.prev_line = line;
        } else {
            writer.writeAll("   |  ") catch oom();
        }
    }
}

pub fn disInstruction(self: *Self, writer: *Writer, base_offset: usize) usize {
    var offset = base_offset;
    try self.lineHeader(writer, offset);

    self.wide = false;
    var op: OpCode = @enumFromInt(self.chunk.code.items[offset]);

    if (op == .wide) {
        self.wide = true;
        offset += 1;
        op = @enumFromInt(self.chunk.code.items[offset]);
        _ = writer.writeAll("wide\n") catch unreachable;
        try self.lineHeader(writer, offset);
    }

    const name = @tagName(op);
    return switch (op) {
        .add_float => self.simpleInstruction(writer, name, offset),
        .add_int => self.simpleInstruction(writer, name, offset),
        .array_new => self.arrayNew(writer, offset),
        .array_set => self.simpleInstruction(writer, name, offset),
        .bound_method => self.indexInstruction(writer, name, offset),
        .box => self.simpleInstruction(writer, name, offset),
        .call => self.call(writer, offset),
        .call_dyn => self.indexInstruction(writer, name, offset),
        .call_array, .call_string => self.callIndexArity(writer, op, offset),
        .call_ext => self.callExt(writer, false, offset),
        .call_foreign => self.callForeign(writer, name, offset),
        .call_foreign_ext => self.callExt(writer, true, offset),
        .call_virtual => self.callIndexArity(writer, op, offset),
        .call_zig => self.callZig(writer, name, offset),
        .closure => self.indexInstruction(writer, name, offset),
        .def_global => self.indexInstruction(writer, name, offset),
        .div_float => self.simpleInstruction(writer, name, offset),
        .div_int => self.simpleInstruction(writer, name, offset),
        .dup => self.simpleInstruction(writer, name, offset),
        .eq_bool => self.simpleInstruction(writer, name, offset),
        .eq_float => self.simpleInstruction(writer, name, offset),
        .eq_int => self.simpleInstruction(writer, name, offset),
        .eq_null => self.simpleInstruction(writer, name, offset),
        .eq_str => self.simpleInstruction(writer, name, offset),
        .exit_repl => self.simpleInstruction(writer, name, offset),
        .fallback_err => self.simpleInstruction(writer, name, offset),
        .fallback_opt => self.simpleInstruction(writer, name, offset),
        .ge_float => self.simpleInstruction(writer, name, offset),
        .ge_int => self.simpleInstruction(writer, name, offset),
        .get_capt_frame => self.indexInstruction(writer, name, offset),
        .get_capt_local => self.indexInstruction(writer, name, offset),
        .get_field => self.getMember(writer, name, offset),
        .get_field_cow => self.getMember(writer, name, offset),
        .get_field_native => self.getMember(writer, name, offset),
        .get_global => self.getGlobal(writer, false, offset),
        .get_global_cow => self.getGlobal(writer, true, offset),
        .get_local => self.indexInstruction(writer, name, offset),
        .get_local_cow => self.indexInstruction(writer, name, offset),
        .get_enum_tag => self.simpleInstruction(writer, name, offset),
        .get_union_tag => self.simpleInstruction(writer, name, offset),
        .gt_float => self.simpleInstruction(writer, name, offset),
        .gt_int => self.simpleInstruction(writer, name, offset),
        .incr_ref => self.simpleInstruction(writer, name, offset),
        .index_arr => self.simpleInstruction(writer, name, offset),
        .index_range_arr => self.simpleInstruction(writer, name, offset),
        .index_arr_cow => self.simpleInstruction(writer, name, offset),
        .index_range_str => self.simpleInstruction(writer, name, offset),
        .index_str => self.simpleInstruction(writer, name, offset),
        .in_array => self.simpleInstruction(writer, name, offset),
        .in_range_float => self.simpleInstruction(writer, name, offset),
        .in_range_int => self.simpleInstruction(writer, name, offset),
        .in_str => self.simpleInstruction(writer, name, offset),
        .is_bool => self.simpleInstruction(writer, name, offset),
        .is_float => self.simpleInstruction(writer, name, offset),
        .is_int => self.simpleInstruction(writer, name, offset),
        .is_str => self.simpleInstruction(writer, name, offset),
        .is_type => self.isType(writer, offset),
        .int_to_float => self.simpleInstruction(writer, name, offset),
        .iter_new_arr => self.simpleInstruction(writer, name, offset),
        .iter_new_range => self.simpleInstruction(writer, name, offset),
        .iter_new_str => self.simpleInstruction(writer, name, offset),
        .iter_next => self.simpleInstruction(writer, name, offset),
        .iter_next_index => self.simpleInstruction(writer, name, offset),
        .jump => self.jumpInstruction(writer, name, 1, offset),
        .jump_false => self.jumpInstruction(writer, name, 1, offset),
        .jump_true => self.jumpInstruction(writer, name, 1, offset),
        .jump_no_err => self.jumpInstruction(writer, name, 1, offset),
        .jump_null => self.jumpInstruction(writer, name, 1, offset),
        .le_float => self.simpleInstruction(writer, name, offset),
        .le_int => self.simpleInstruction(writer, name, offset),
        .lt_float => self.simpleInstruction(writer, name, offset),
        .lt_int => self.simpleInstruction(writer, name, offset),
        .load_blk_val => self.simpleInstruction(writer, name, offset),
        .load_const => self.constantInstruction(writer, name, offset),
        .load_const_ext => self.extConstantInstruction(writer, name, offset),
        .load_fn => self.loadSymbol(writer, offset),
        .load_fn_ext => self.indexExternInstruction(writer, name, offset),
        .load_fn_zig => self.indexExternInstruction(writer, name, offset),
        .loop => self.jumpInstruction(writer, name, -1, offset),
        .mod_float => self.simpleInstruction(writer, name, offset),
        .mod_int => self.simpleInstruction(writer, name, offset),
        .mul_float => self.simpleInstruction(writer, name, offset),
        .mul_int => self.simpleInstruction(writer, name, offset),
        .ne_bool => self.simpleInstruction(writer, name, offset),
        .ne_float => self.simpleInstruction(writer, name, offset),
        .ne_int => self.simpleInstruction(writer, name, offset),
        .ne_null => self.simpleInstruction(writer, name, offset),
        .ne_null_push => self.simpleInstruction(writer, name, offset),
        .ne_str => self.simpleInstruction(writer, name, offset),
        .neg_float => self.simpleInstruction(writer, name, offset),
        .neg_int => self.simpleInstruction(writer, name, offset),
        .not => self.simpleInstruction(writer, name, offset),
        .pop => self.simpleInstruction(writer, name, offset),
        .pop2 => self.simpleInstruction(writer, name, offset),
        .pop3 => self.simpleInstruction(writer, name, offset),
        .popn => self.indexInstruction(writer, name, offset),
        .print => self.simpleInstruction(writer, name, offset),
        .push_false => self.simpleInstruction(writer, name, offset),
        .push_null => self.simpleInstruction(writer, name, offset),
        .push_true => self.simpleInstruction(writer, name, offset),
        .range_new_float => self.simpleInstruction(writer, name, offset),
        .range_new_int => self.simpleInstruction(writer, name, offset),
        .ret => self.simpleInstruction(writer, name, offset),
        .ret_naked => self.simpleInstruction(writer, name, offset),
        .set_field => self.indexInstruction(writer, name, offset),
        .set_global => self.indexInstruction(writer, name, offset),
        .set_local => self.indexInstruction(writer, name, offset),
        .set_local_box => self.indexInstruction(writer, name, offset),
        .store_blk_val => self.simpleInstruction(writer, name, offset),
        .str_cat => self.simpleInstruction(writer, name, offset),
        .str_mul => self.simpleInstruction(writer, name, offset),
        .struct_lit => self.structLiteral(writer, false, offset),
        .struct_lit_ext => self.structLiteralExt(writer, offset),
        .struct_lit_zig => self.structLiteral(writer, true, offset),
        .sub_float => self.simpleInstruction(writer, name, offset),
        .sub_int => self.simpleInstruction(writer, name, offset),
        .swap_pop => self.simpleInstruction(writer, name, offset),
        .trait_obj => self.indexInstruction(writer, name, offset),
        .unbox => self.simpleInstruction(writer, name, offset),
        .union_constr => self.unionConstr(writer, offset),
        .union_constr_ext => self.unionConstrExt(writer, offset),
        .union_unwrap => self.indexInstruction(writer, name, offset),
        .wide => unreachable,
    } catch oom();
}

fn getIndex(self: *const Self, offset: usize) struct { value: usize, bytes: usize } {
    if (self.wide) {
        return .{ .value = self.readShort(offset), .bytes = 2 };
    } else {
        return .{ .value = self.chunk.code.items[offset + 1], .bytes = 1 };
    }
}

fn readShort(self: *const Self, offset: usize) u16 {
    var index = @as(u16, self.chunk.code.items[offset + 1]) << 8;
    index |= self.chunk.code.items[offset + 2];
    return index;
}

fn simpleInstruction(self: *Self, writer: *Writer, name: []const u8, offset: usize) Writer.Error!usize {
    if (self.render_mode == .@"test") {
        try writer.print("{s}\n", .{name});
    } else {
        try writer.print("{s:<20}\n", .{name});
    }

    return offset + 1;
}

fn indexInstruction(self: *Self, writer: *Writer, name: []const u8, offset: usize) Writer.Error!usize {
    const index = self.getIndex(offset);

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}\n", .{ name, index.value });
    } else {
        try writer.print("{s:<20} index {:>4}\n", .{ name, index.value });
    }

    return offset + 1 + index.bytes;
}

fn indexExternInstruction(self: *Self, writer: *Writer, name: []const u8, offset: usize) Writer.Error!usize {
    const module = self.chunk.code.items[offset + 1];
    const index = self.chunk.code.items[offset + 2];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, module {}\n", .{ name, module, index });
    } else {
        try writer.print("{s:<20} index {:>4}, module {:>4}\n", .{ name, module, index });
    }

    return offset + 3;
}

fn isType(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "is_type";
    const index = self.getIndex(offset);

    if (self.render_mode == .@"test") {
        // Not printing the type id value because it changes every time I add a builtin/std function or type
        try writer.print("{s} x\n", .{text});
    } else {
        try writer.print("{s:<20} {:>4}\n", .{ text, index.value });
    }

    return offset + 1 + index.bytes;
}

fn arrayNew(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const len = self.getIndex(offset);
    const type_id = self.readShort(offset + len.bytes);

    if (self.render_mode == .@"test") {
        // Not printing the type id value because it changes every time I add a builtin/std function or type
        try writer.print("array_new length {}, type_id x\n", .{len.value});
    } else {
        try writer.print("array_new length {:>4}, type_id {:>4}\n", .{ len.value, type_id });
    }

    return offset + 1 + len.bytes + 2;
}

fn getGlobal(self: *Self, writer: *Writer, cow: bool, offset: usize) Writer.Error!usize {
    const index = self.chunk.code.items[offset + 1];
    const text = if (cow) "get_global_cow" else "get_global";

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}", .{ text, index });
    } else {
        try writer.print("{s:<20} index {:>4}", .{ text, index });
    }

    if (self.module.globals[index].asObj()) |obj| {
        try writer.writeAll(", ");
        try obj.print(writer);
    }
    try writer.writeAll("\n");

    return offset + 2;
}

fn constantInstruction(self: *Self, writer: *Writer, name: []const u8, offset: usize) Writer.Error!usize {
    const index = self.getIndex(offset);
    const value = self.module.constants[index.value];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, value ", .{ name, index.value });
    } else {
        try writer.print("{s:<20} index {:>4}, value ", .{ name, index.value });
    }

    value.print(writer);
    try writer.print("\n", .{});
    return offset + 1 + index.bytes;
}

fn extConstantInstruction(self: *Self, writer: *Writer, name: []const u8, offset: usize) Writer.Error!usize {
    const constant = self.chunk.code.items[offset + 1];
    const mod = self.chunk.code.items[offset + 2];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, module {}\n", .{ name, constant, mod });
    } else {
        try writer.print("{s:<20} index {:>4}, module {:>4}\n", .{ name, constant, mod });
    }

    return offset + 3;
}

fn jumpInstruction(self: *Self, writer: *Writer, name: []const u8, sign: isize, offset: usize) Writer.Error!usize {
    const jump = self.readShort(offset);
    const target = @as(isize, jump) * sign + @as(isize, @intCast(offset)) + 3;

    if (self.render_mode == .@"test") {
        try writer.print("{s} {} -> {}\n", .{ name, offset, target });
    } else {
        try writer.print("{s:<20} {:>4} -> {}\n", .{ name, offset, target });
    }

    return offset + 3;
}

fn loadSymbol(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "load_fn";
    const idx = self.chunk.code.items[offset + 1];
    const func = self.module.functions[idx];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, {s}\n", .{ text, idx, func.name });
    } else {
        try writer.print("{s:<20} index {:>4}, {s}\n", .{ text, idx, func.name });
    }

    return offset + 2;
}

fn getMember(self: *Self, writer: *Writer, name: []const u8, offset: usize) Writer.Error!usize {
    const idx = self.chunk.code.items[offset + 1];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}\n", .{ name, idx });
    } else {
        try writer.print("{s:<20} index {:>4}\n", .{ name, idx });
    }

    return offset + 2;
}

fn call(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "call";
    const index = self.chunk.code.items[offset + 1];
    const arity = self.chunk.code.items[offset + 2];
    const func = self.module.functions[index];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, arity {}, {s}\n", .{ text, index, arity, func.name });
    } else {
        try writer.print("{s:<20} index {:>4}, arity {:>4}, {s}\n", .{ text, index, arity, func.name });
    }

    return offset + 3;
}

fn callExt(self: *Self, writer: *Writer, native: bool, offset: usize) Writer.Error!usize {
    const text = if (native) "call_foreign_ext" else "call_ext";
    const index = self.chunk.code.items[offset + 1];
    const module = self.chunk.code.items[offset + 2];
    const arity = self.chunk.code.items[offset + 3];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, module {}, arity {}\n", .{ text, index, module, arity });
    } else {
        try writer.print("{s:<20} index {:>4}, module {:>4}, arity {:>4}\n", .{ text, index, module, arity });
    }

    return offset + 4;
}

fn callForeign(self: *Self, writer: *Writer, text: []const u8, offset: usize) Writer.Error!usize {
    const index = self.chunk.code.items[offset + 1];
    const arity = self.chunk.code.items[offset + 2];
    const name = self.module.foreign_funcs.items[index].name;

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, arity {}, {s}\n", .{ text, index, arity, name });
    } else {
        try writer.print("{s:<20} index {:>4}, arity {:>4}, {s}\n", .{ text, index, arity, name });
    }

    return offset + 3;
}

fn callZig(self: *Self, writer: *Writer, text: []const u8, offset: usize) Writer.Error!usize {
    const index = self.chunk.code.items[offset + 1];
    const module = self.chunk.code.items[offset + 2];
    const arity = self.chunk.code.items[offset + 3];
    const name = self.zig_fns[index].name;

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, module {}, arity {}, {s}\n", .{ text, index, module, arity, name });
    } else {
        try writer.print("{s:<20} index {:>4}, module {:>4}, arity {:>4}, {s}\n", .{ text, index, module, arity, name });
    }

    return offset + 4;
}

fn callIndexArity(self: *Self, writer: *Writer, op: OpCode, offset: usize) Writer.Error!usize {
    const index = self.chunk.code.items[offset + 1];
    const arity = self.chunk.code.items[offset + 2];

    if (self.render_mode == .@"test") {
        try writer.print("{t} index {}, arity {}\n", .{ op, index, arity });
    } else {
        try writer.print("{t:<20} index {:>4}, arity {:>4}\n", .{ op, index, arity });
    }

    return offset + 3;
}

fn enumLiteral(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "enum_lit";
    const index = self.chunk.code.items[offset + 1];
    const tag = self.chunk.code.items[offset + 2];
    const sym = self.module.unions[index];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, tag {}, {s}\n", .{ text, index, tag, sym.name });
    } else {
        try writer.print("{s:<20} index {:>4}, tag {:>4}, {s}\n", .{ text, index, tag, sym.name });
    }

    return offset + 3;
}

fn enumLiteralExt(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "enum_lit_ext";
    const index = self.chunk.code.items[offset + 1];
    const module = self.chunk.code.items[offset + 2];
    const tag = self.chunk.code.items[offset + 3];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, module {}, tag {}\n", .{ text, index, module, tag });
    } else {
        try writer.print("{s:<20} index {:>4}, module {:>4}, tag {:>4}\n", .{ text, index, module, tag });
    }

    return offset + 4;
}

fn structLiteral(self: *Self, writer: *Writer, native: bool, offset: usize) Writer.Error!usize {
    const text = if (native) "struct_lit_zig" else "struct_lit";
    const index = self.chunk.code.items[offset + 1];
    const arity = self.chunk.code.items[offset + 2];
    const sym = if (native) self.zig_structs[index] else self.module.structures[index];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, arity {}, {s}\n", .{ text, index, arity, sym.name });
    } else {
        try writer.print("{s:<20} index {:>4}, arity {:>4}, {s}\n", .{ text, index, arity, sym.name });
    }

    return offset + 3;
}

fn structLiteralExt(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "struct_lit_ext";
    const index = self.chunk.code.items[offset + 1];
    const module = self.chunk.code.items[offset + 2];
    const arity = self.chunk.code.items[offset + 3];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, module {}, arity {}\n", .{ text, index, module, arity });
    } else {
        try writer.print("{s:<20} index {:>4}, module {:>4}, arity {:>4}\n", .{ text, index, module, arity });
    }

    return offset + 4;
}

fn unionConstr(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "union_constr";
    const index = self.chunk.code.items[offset + 1];
    const tag = self.chunk.code.items[offset + 2];
    const sym = self.module.unions[index];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, tag {}, {s}\n", .{ text, index, tag, sym.name });
    } else {
        try writer.print("{s:<20} index {:>4}, tag {:>4}, {s}\n", .{ text, index, tag, sym.name });
    }

    return offset + 3;
}

fn unionConstrExt(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "union_lit_constr";
    const index = self.chunk.code.items[offset + 1];
    const module = self.chunk.code.items[offset + 2];
    const tag = self.chunk.code.items[offset + 3];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, module {}, tag {}\n", .{ text, index, module, tag });
    } else {
        try writer.print("{s:<20} index {:>4}, module {:>4}, tag {:>4}\n", .{ text, index, module, tag });
    }

    return offset + 4;
}
