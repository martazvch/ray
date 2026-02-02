const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;

const Value = @import("../runtime/values.zig").Value;
const oom = @import("misc").oom;
const Chunk = @import("Chunk.zig");
const OpCode = Chunk.OpCode;
const Module = @import("compiler.zig").CompiledModule;

chunk: *const Chunk,
globals: []const Value,
symbols: []const Value,
constants: []const Value,
natives: []const Value,
render_mode: RenderMode,
wide: bool,

prev_line: usize = 0,

const Self = @This();
pub const RenderMode = enum { none, normal, @"test" };

pub fn init(chunk: *const Chunk, module: *const Module, natives: []const Value, render_mode: RenderMode) Self {
    return .{
        .chunk = chunk,
        .globals = module.globals,
        .symbols = module.symbols,
        .constants = module.constants,
        .render_mode = render_mode,
        .natives = natives,
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
        .array_new => self.indexInstruction(writer, "array_new", offset),
        .array_set => self.simpleInstruction(writer, "array_set", offset),
        .bound_method => self.indexInstruction(writer, "bound_method", offset),
        .box => self.simpleInstruction(writer, "box", offset),
        .call => self.indexInstruction(writer, "call", offset),
        .call_array_fn => self.callObjFn(writer, .array, offset),
        .call_sym => self.callSym(writer, offset),
        .call_sym_ext => self.callExtSym(writer, offset),
        .call_native => self.callNativeSym(writer, offset),
        .closure => self.indexInstruction(writer, "closure", offset),
        .def_global => self.indexInstruction(writer, "def_global", offset),
        .div_float => self.simpleInstruction(writer, "div_float", offset),
        .div_int => self.simpleInstruction(writer, "div_int", offset),
        .dup => self.simpleInstruction(writer, "dup", offset),
        .enum_create => self.indexInstruction(writer, "enum_create", offset),
        .eq_bool => self.simpleInstruction(writer, "eq_bool", offset),
        .eq_float => self.simpleInstruction(writer, "eq_float", offset),
        .eq_int => self.simpleInstruction(writer, "eq_int", offset),
        .eq_null => self.simpleInstruction(writer, "eq_null", offset),
        .eq_str => self.simpleInstruction(writer, "eq_str", offset),
        .exit_repl => self.simpleInstruction(writer, "exit_repl", offset),
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
        .load_ext_sym => self.indexExternInstruction(writer, "load_extern_sym", offset),
        .load_builtin => self.indexInstruction(writer, "load_builtin", offset),
        .load_sym => self.loadSymbol(writer, offset),
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
        .wide => unreachable,
    } catch oom();
}

fn getIndex(self: *const Self, offset: usize) struct { value: usize, bytes: usize } {
    if (self.wide) {
        var index = @as(u16, self.chunk.code.items[offset + 1]) << 8;
        index |= self.chunk.code.items[offset + 2];
        return .{ .value = index, .bytes = 2 };
    } else {
        return .{ .value = self.chunk.code.items[offset + 1], .bytes = 1 };
    }
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
    // const index = self.chunk.code.items[offset + 1];
    const index = self.getIndex(offset);

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}\n", .{ name, index.value });
    } else {
        try writer.print("{s:<20} index {:>4}\n", .{ name, index.value });
    }

    return offset + 1 + index.bytes;
}

fn indexExternInstruction(self: *Self, writer: *Writer, name: []const u8, offset: usize) Writer.Error!usize {
    const index = self.chunk.code.items[offset + 1];
    const module = self.chunk.code.items[offset + 2];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, module {}\n", .{ name, module, index });
    } else {
        try writer.print("{s:<20} index {:>4}, module {:>4}\n", .{ name, module, index });
    }

    return offset + 3;
}

fn getGlobal(self: *Self, writer: *Writer, cow: bool, offset: usize) Writer.Error!usize {
    const index = self.chunk.code.items[offset + 1];
    const text = if (cow) "get_global_cow" else "get_global";

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}", .{ text, index });
    } else {
        try writer.print("{s:<20} index {:>4}", .{ text, index });
    }

    if (self.globals[index].asObj()) |obj| {
        try writer.writeAll(", ");
        try obj.print(writer);
    }
    try writer.writeAll("\n");

    return offset + 2;
}

fn constantInstruction(self: *Self, writer: *Writer, name: []const u8, offset: usize) Writer.Error!usize {
    // const constant = self.chunk.code.items[offset + 1];
    const index = self.getIndex(offset);
    const value = self.constants[index.value];

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
    var jump: u16 = @as(u16, self.chunk.code.items[offset + 1]) << 8;
    jump |= self.chunk.code.items[offset + 2];
    const target = @as(isize, jump) * sign + @as(isize, @intCast(offset)) + 3;

    if (self.render_mode == .@"test") {
        try writer.print("{s} {} -> {}\n", .{ name, offset, target });
    } else {
        try writer.print("{s:<20} {:>4} -> {}\n", .{ name, offset, target });
    }

    return offset + 3;
}

fn loadSymbol(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "load_sym";
    const idx = self.chunk.code.items[offset + 1];
    const sym = self.symbols[idx].obj;

    var buf: [512]u8 = undefined;
    var bw = std.Io.Writer.fixed(&buf);
    sym.print(&bw) catch oom();

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, {s}\n", .{ text, idx, bw.buffered() });
    } else {
        try writer.print("{s:<20} index {:>4}, {s}\n", .{ text, idx, bw.buffered() });
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

fn callSym(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "call_sym";
    const index = self.chunk.code.items[offset + 1];
    const arity = self.chunk.code.items[offset + 2];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, arity {}, ", .{ text, index, arity });
    } else {
        try writer.print("{s:<20} index {:>4}, arity {:>4}, ", .{ text, index, arity });
    }

    const symbol = self.symbols[index];
    symbol.print(writer);
    try writer.print("\n", .{});

    return offset + 3;
}

fn callExtSym(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "call_sym_ext";
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

fn callNativeSym(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "call_sym_native";
    const index = self.chunk.code.items[offset + 1];
    const arity = self.chunk.code.items[offset + 2];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, arity {}, ", .{ text, index, arity });
    } else {
        try writer.print("{s:<20} index {:>4}, arity {:>4}, ", .{ text, index, arity });
    }

    const symbol = self.natives[index];
    symbol.print(writer);
    try writer.print("\n", .{});

    return offset + 3;
}

fn callObjFn(self: *Self, writer: *Writer, kind: enum { array }, offset: usize) Writer.Error!usize {
    const text = "call_" ++ @tagName(kind) ++ "_fn";
    const index = self.chunk.code.items[offset + 1];
    const arity = self.chunk.code.items[offset + 2];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, arity {}\n", .{ text, index, arity });
    } else {
        try writer.print("{s:<20} index {:>4}, arity {:>4}\n", .{ text, index, arity });
    }

    return offset + 3;
}

fn structLiteral(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "struct_lit";
    const index = self.chunk.code.items[offset + 1];
    const arity = self.chunk.code.items[offset + 2];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, arity {}, ", .{ text, index, arity });
    } else {
        try writer.print("{s:<20} index {:>4}, arity {:>4}, ", .{ text, index, arity });
    }

    const symbol = self.symbols[index];
    symbol.print(writer);
    try writer.print("\n", .{});

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
