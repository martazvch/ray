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
const Module = @import("../pipeline/ModuleManager.zig").Module;

chunk: *const Chunk,
natives: []const *Obj.NativeFunction,
render_mode: RenderMode,
module: *const Module,
wide: bool,

prev_line: usize = 0,

const Self = @This();
pub const RenderMode = enum { normal, @"test" };

pub fn init(chunk: *const Chunk, module: *const Module, natives: []const *Obj.NativeFunction) Self {
    return .{
        .chunk = chunk,
        .render_mode = if (options.test_mode) .@"test" else .normal,
        .natives = natives,
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

    return switch (op) {
        .add_float => self.simpleInstruction(writer, "add_float", offset),
        .add_int => self.simpleInstruction(writer, "add_int", offset),
        .array_new => self.arrayNew(writer, offset),
        .array_set => self.simpleInstruction(writer, "array_set", offset),
        .bound_method => self.indexInstruction(writer, "bound_method", offset),
        .box => self.simpleInstruction(writer, "box", offset),
        .call_any => self.indexInstruction(writer, "call", offset),
        .call_array, .call_string => self.callObjFn(writer, op, offset),
        .call => self.call(writer, offset),
        .call_ext => self.callExt(writer, offset),
        .call_native => self.callNative(writer, offset),
        .closure => self.indexInstruction(writer, "closure", offset),
        .def_global => self.indexInstruction(writer, "def_global", offset),
        .div_float => self.simpleInstruction(writer, "div_float", offset),
        .div_int => self.simpleInstruction(writer, "div_int", offset),
        .dup => self.simpleInstruction(writer, "dup", offset),
        .enum_lit => self.enumLiteral(writer, offset),
        .enum_lit_ext => self.enumLiteralExt(writer, offset),
        .eq_bool => self.simpleInstruction(writer, "eq_bool", offset),
        .eq_float => self.simpleInstruction(writer, "eq_float", offset),
        .eq_int => self.simpleInstruction(writer, "eq_int", offset),
        .eq_null => self.simpleInstruction(writer, "eq_null", offset),
        .eq_str => self.simpleInstruction(writer, "eq_str", offset),
        .exit_repl => self.simpleInstruction(writer, "exit_repl", offset),
        .fallback_err => self.simpleInstruction(writer, "fallback_err", offset),
        .fallback_opt => self.simpleInstruction(writer, "fallback_opt", offset),
        .ge_float => self.simpleInstruction(writer, "ge_float", offset),
        .ge_int => self.simpleInstruction(writer, "ge_int", offset),
        .get_capt_frame => self.indexInstruction(writer, "get_capt_frame", offset),
        .get_capt_local => self.indexInstruction(writer, "get_capt_local", offset),
        .get_field => self.getMember(writer, "get_field", offset),
        .get_field_cow => self.getMember(writer, "get_field_cow", offset),
        .get_global => self.getGlobal(writer, false, offset),
        .get_global_cow => self.getGlobal(writer, true, offset),
        .get_local => self.indexInstruction(writer, "get_local", offset),
        .get_local_cow => self.indexInstruction(writer, "get_local_cow", offset),
        .get_tag => self.simpleInstruction(writer, "get_tag", offset),
        .gt_float => self.simpleInstruction(writer, "gt_float", offset),
        .gt_int => self.simpleInstruction(writer, "gt_int", offset),
        .incr_ref => self.simpleInstruction(writer, "incr_ref", offset),
        .index_arr => self.simpleInstruction(writer, "index_arr", offset),
        .index_range_arr => self.simpleInstruction(writer, "index_range_arr", offset),
        .index_arr_cow => self.simpleInstruction(writer, "index_arr_cow", offset),
        .index_range_str => self.simpleInstruction(writer, "index_range_str", offset),
        .index_str => self.simpleInstruction(writer, "index_str", offset),
        .in_array => self.simpleInstruction(writer, "in_array", offset),
        .in_range_float => self.simpleInstruction(writer, "in_range_float", offset),
        .in_range_int => self.simpleInstruction(writer, "in_range_int", offset),
        .in_str => self.simpleInstruction(writer, "in_str", offset),
        .is_bool => self.simpleInstruction(writer, "is_bool", offset),
        .is_float => self.simpleInstruction(writer, "is_float", offset),
        .is_int => self.simpleInstruction(writer, "is_int", offset),
        .is_str => self.simpleInstruction(writer, "is_str", offset),
        .is_type => self.indexInstruction(writer, "is_type", offset),
        .iter_new_arr => self.simpleInstruction(writer, "iter_new_arr", offset),
        .iter_new_range => self.simpleInstruction(writer, "iter_new_range", offset),
        .iter_new_str => self.simpleInstruction(writer, "iter_new_str", offset),
        .iter_next => self.simpleInstruction(writer, "iter_next", offset),
        .iter_next_index => self.simpleInstruction(writer, "iter_next_index", offset),
        .jump => self.jumpInstruction(writer, "jump", 1, offset),
        .jump_false => self.jumpInstruction(writer, "jump_false", 1, offset),
        .jump_true => self.jumpInstruction(writer, "jump_true", 1, offset),
        .jump_no_err => self.jumpInstruction(writer, "jump_no_err", 1, offset),
        .jump_null => self.jumpInstruction(writer, "jump_null", 1, offset),
        .le_float => self.simpleInstruction(writer, "le_float", offset),
        .le_int => self.simpleInstruction(writer, "le_int", offset),
        .lt_float => self.simpleInstruction(writer, "lt_float", offset),
        .lt_int => self.simpleInstruction(writer, "lt_int", offset),
        .load_blk_val => self.simpleInstruction(writer, "load_blk_val", offset),
        .load_constant => self.constantInstruction(writer, "load_constant", offset),
        .load_ext_constant => self.extConstantInstruction(writer, "load_ext_constant", offset),
        .load_fn => self.loadSymbol(writer, offset),
        .load_fn_ext => self.indexExternInstruction(writer, "load_fn_ext", offset),
        .load_fn_builtin => self.indexInstruction(writer, "load_fn_builtin", offset),
        .loop => self.jumpInstruction(writer, "loop", -1, offset),
        .mod_float => self.simpleInstruction(writer, "mod_float", offset),
        .mod_int => self.simpleInstruction(writer, "mod_int", offset),
        .mul_float => self.simpleInstruction(writer, "mul_float", offset),
        .mul_int => self.simpleInstruction(writer, "mul_int", offset),
        .ne_bool => self.simpleInstruction(writer, "ne_bool", offset),
        .ne_float => self.simpleInstruction(writer, "ne_float", offset),
        .ne_int => self.simpleInstruction(writer, "ne_int", offset),
        .ne_null => self.simpleInstruction(writer, "ne_null", offset),
        .ne_null_push => self.simpleInstruction(writer, "ne_null_push", offset),
        .ne_str => self.simpleInstruction(writer, "ne_str", offset),
        .neg_float => self.simpleInstruction(writer, "neg_float", offset),
        .neg_int => self.simpleInstruction(writer, "neg_int", offset),
        .not => self.simpleInstruction(writer, "not", offset),
        .pop => self.simpleInstruction(writer, "pop", offset),
        .pop2 => self.simpleInstruction(writer, "pop2", offset),
        .pop3 => self.simpleInstruction(writer, "pop3", offset),
        .popn => self.indexInstruction(writer, "popn", offset),
        .print => self.simpleInstruction(writer, "print", offset),
        .push_false => self.simpleInstruction(writer, "push_false", offset),
        .push_null => self.simpleInstruction(writer, "push_null", offset),
        .push_true => self.simpleInstruction(writer, "push_true", offset),
        .range_new_float => self.simpleInstruction(writer, "range_new_float", offset),
        .range_new_int => self.simpleInstruction(writer, "range_new_int", offset),
        .ret => self.simpleInstruction(writer, "ret", offset),
        .ret_naked => self.simpleInstruction(writer, "ret_naked", offset),
        .set_field => self.indexInstruction(writer, "set_field", offset),
        .set_global => self.indexInstruction(writer, "set_global", offset),
        .set_local => self.indexInstruction(writer, "set_local", offset),
        .set_local_box => self.indexInstruction(writer, "set_local_box", offset),
        .store_blk_val => self.simpleInstruction(writer, "store_blk_val", offset),
        .str_cat => self.simpleInstruction(writer, "str_cat", offset),
        .str_mul => self.simpleInstruction(writer, "str_mul", offset),
        .struct_lit => self.structLiteral(writer, offset),
        .struct_lit_ext => self.structLiteralExt(writer, offset),
        .sub_float => self.simpleInstruction(writer, "sub_float", offset),
        .sub_int => self.simpleInstruction(writer, "sub_int", offset),
        .swap_pop => self.simpleInstruction(writer, "swap_pop", offset),
        .unbox => self.simpleInstruction(writer, "unbox", offset),
        .union_lit => self.unionLiteral(writer, offset),
        .union_lit_ext => self.unionLiteralExt(writer, offset),
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

fn arrayNew(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const len = self.getIndex(offset);
    const type_id = self.readShort(offset + len.bytes);

    if (self.render_mode == .@"test") {
        try writer.print("array_new length {}, type_id {}\n", .{ len.value, type_id });
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

fn callExt(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "call_ext";
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

fn callNative(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "call_native";
    const index = self.chunk.code.items[offset + 1];
    const arity = self.chunk.code.items[offset + 2];
    const native = self.natives[index];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, arity {}, {s}\n", .{ text, index, arity, native.name });
    } else {
        try writer.print("{s:<20} index {:>4}, arity {:>4}, {s}\n", .{ text, index, arity, native.name });
    }

    return offset + 3;
}

fn callObjFn(self: *Self, writer: *Writer, op: OpCode, offset: usize) Writer.Error!usize {
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

fn structLiteral(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "struct_lit";
    const index = self.chunk.code.items[offset + 1];
    const arity = self.chunk.code.items[offset + 2];
    const sym = self.module.structures[index];

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

fn unionLiteral(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "union_lit";
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

fn unionLiteralExt(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "union_lit_ext";
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
