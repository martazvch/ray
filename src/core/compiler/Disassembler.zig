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
const ModManager = @import("../pipeline/ModuleManager.zig");
const CompiledMod = ModManager.Module;
const ModIndex = ModManager.Index;
const NativeMod = @import("../pipeline/NativesRegister.zig").NativeModule;

chunk: *const Chunk,
module: ModIndex,
modules: *const ModManager,
wide: bool,
render_mode: RenderMode,

prev_line: usize = 0,

const Self = @This();
pub const RenderMode = enum { normal, @"test" };

pub fn init(
    chunk: *const Chunk,
    module: ModIndex,
    modules: *const ModManager,
) Self {
    return .{
        .chunk = chunk,
        .module = module,
        .modules = modules,
        .wide = false,
        .render_mode = if (options.test_mode) .@"test" else .normal,
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

        .binary_and => self.simpleInstruction(writer, name, offset),
        .binary_or => self.simpleInstruction(writer, name, offset),
        .binary_xor => self.simpleInstruction(writer, name, offset),
        .binary_neg => self.simpleInstruction(writer, name, offset),
        .shift_left => self.simpleInstruction(writer, name, offset),
        .shift_right => self.simpleInstruction(writer, name, offset),

        .bound_method => self.indexInstruction(writer, name, offset),
        .box => self.simpleInstruction(writer, name, offset),

        .call => self.call(writer, name, false, false, offset),
        .call_dyn => self.indexInstruction(writer, name, offset),
        .call_array, .call_string => self.callIndexArity(writer, op, offset),
        .call_ext => self.call(writer, name, false, true, offset),
        .call_extern => self.callExtern(writer, name, false, offset),
        .call_extern_ext => self.callExtern(writer, name, true, offset),
        .call_virtual => self.callIndexArity(writer, op, offset),
        .call_zig => self.call(writer, name, true, true, offset),

        .closure => self.indexInstruction(writer, name, offset),
        .deref => self.simpleInstruction(writer, name, offset),
        .div_float => self.simpleInstruction(writer, name, offset),
        .div_int => self.simpleInstruction(writer, name, offset),
        .dup => self.simpleInstruction(writer, name, offset),
        .eq_bool => self.simpleInstruction(writer, name, offset),
        .eq_float => self.simpleInstruction(writer, name, offset),
        .eq_int => self.simpleInstruction(writer, name, offset),
        .eq_null => self.simpleInstruction(writer, name, offset),
        .eq_ptr => self.simpleInstruction(writer, name, offset),
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
        .get_global => self.getGlobal(writer, false, false, offset),
        .get_global_cow => self.getGlobal(writer, true, false, offset),
        .get_global_ext => self.getGlobal(writer, false, true, offset),
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
        .load_const => self.constantInstruction(writer, name, false, offset),
        .load_const_ext => self.constantInstruction(writer, name, true, offset),
        .load_fn => self.loadSymbol(writer, name, offset),
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
        .ne_ptr => self.simpleInstruction(writer, name, offset),
        .ne_str => self.simpleInstruction(writer, name, offset),
        .neg_float => self.simpleInstruction(writer, name, offset),
        .neg_int => self.simpleInstruction(writer, name, offset),
        .not => self.simpleInstruction(writer, name, offset),
        .pop => self.simpleInstruction(writer, name, offset),
        .pop2 => self.simpleInstruction(writer, name, offset),
        .pop3 => self.simpleInstruction(writer, name, offset),
        .popn => self.indexInstruction(writer, name, offset),
        .print => self.simpleInstruction(writer, name, offset),
        .ptr_local => self.indexInstruction(writer, name, offset),
        .ptr_global => self.indexInstruction(writer, name, offset),
        .ptr_field => self.indexInstruction(writer, name, offset),
        .ptr_store => self.simpleInstruction(writer, name, offset),
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
        .struct_lit => self.structLiteral(writer, name, false, offset),
        .struct_lit_ext => self.structLiteral(writer, name, true, offset),
        .struct_lit_zig => self.structLiteral(writer, name, false, offset),
        .sub_float => self.simpleInstruction(writer, name, offset),
        .sub_int => self.simpleInstruction(writer, name, offset),
        .swap_pop => self.simpleInstruction(writer, name, offset),
        .trait_obj => self.indexInstruction(writer, name, offset),
        .unbox => self.simpleInstruction(writer, name, offset),
        .union_constr => self.unionConstr(writer, name, false, offset),
        .union_constr_ext => self.unionConstr(writer, name, true, offset),
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

fn getGlobal(self: *Self, writer: *Writer, cow: bool, ext: bool, offset: usize) Writer.Error!usize {
    const index = self.chunk.code.items[offset + 1];
    const module = if (ext) self.chunk.code.items[offset + 2] else self.module.toInt();
    const text = if (cow)
        "get_global_cow"
    else if (ext)
        "get_global_ext"
    else
        "get_global";

    if (self.render_mode == .@"test") {
        if (ext) {
            try writer.print("{s} index {}, module {}, value ", .{ text, index, module });
        } else {
            try writer.print("{s} index {}, value ", .{ text, index });
        }
    } else {
        if (ext) {
            try writer.print("{s:<20} index {:>4}, module {:>4}, value ", .{ text, index, module });
        } else {
            try writer.print("{s:<20} index {:>4}, value ", .{ text, index });
        }
    }

    self.modules.getGlobal(.toIndex(module), index).print(writer);
    try writer.writeAll("\n");

    return offset + 2 + @intFromBool(ext);
}

fn constantInstruction(self: *Self, writer: *Writer, name: []const u8, ext: bool, offset: usize) Writer.Error!usize {
    const index = self.getIndex(offset);
    const module = if (ext) self.chunk.code.items[offset + 2] else self.module.toInt();

    if (self.render_mode == .@"test") {
        if (ext) {
            try writer.print("{s} index {}, module {}, value ", .{ name, index.value, module });
        } else {
            try writer.print("{s} index {}, value ", .{ name, index.value });
        }
    } else {
        if (ext) {
            try writer.print("{s:<20} index {:>4}, module {:>4}, value ", .{ name, index.value, module });
        } else {
            try writer.print("{s:<20} index {:>4}, value ", .{ name, index.value });
        }
    }

    const value = self.modules.getConstant(.toIndex(module), index.value);
    value.print(writer);
    try writer.print("\n", .{});
    return offset + 1 + index.bytes + @intFromBool(ext);
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

fn loadSymbol(self: *Self, writer: *Writer, name: []const u8, offset: usize) Writer.Error!usize {
    const index = self.chunk.code.items[offset + 1];
    const func = self.modules.getSymbol(self.module, index, .function);

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, {s}\n", .{ name, index, func.name });
    } else {
        try writer.print("{s:<20} index {:>4}, {s}\n", .{ name, index, func.name });
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

fn call(self: *Self, writer: *Writer, name: []const u8, native: bool, ext: bool, offset: usize) Writer.Error!usize {
    const ext_offset = @intFromBool(ext);

    const index = self.chunk.code.items[offset + 1];
    const module = if (ext) self.chunk.code.items[offset + 2] else self.module.toInt();
    const arity = self.chunk.code.items[offset + 2 + ext_offset];

    const fn_name = if (native)
        self.modules.getSymbol(.toIndex(module), index, .function_zig).name
    else
        self.modules.getSymbol(.toIndex(module), index, .function).name;

    if (self.render_mode == .@"test") {
        if (ext) {
            try writer.print("{s} index {}, module {}, arity {}, {s}\n", .{ name, index, module, arity, fn_name });
        } else {
            try writer.print("{s} index {}, arity {}, {s}\n", .{ name, index, arity, fn_name });
        }
    } else {
        if (ext) {
            try writer.print("{s:<20} index {:>4}, module {:>4}, arity {:>4}, {s}\n", .{ name, index, module, arity, fn_name });
        } else {
            try writer.print("{s:<20} index {:>4}, arity {:>4}, {s}\n", .{ name, index, arity, fn_name });
        }
    }

    return offset + 3 + ext_offset;
}

fn callExtern(self: *Self, writer: *Writer, name: []const u8, ext: bool, offset: usize) Writer.Error!usize {
    const ext_offset = @intFromBool(ext);

    const index = self.chunk.code.items[offset + 1];
    const module = if (ext) self.chunk.code.items[offset + 2] else self.module.toInt();
    const arity = self.chunk.code.items[offset + 2 + ext_offset];
    const fn_name = self.modules.getSymbol(.toIndex(module), index, .function_extern).name;

    if (self.render_mode == .@"test") {
        if (ext) {
            try writer.print("{s} index {}, module {}, arity {}, {s}\n", .{ name, index, module, arity, fn_name });
        } else {
            try writer.print("{s} index {}, arity {}, {s}\n", .{ name, index, arity, fn_name });
        }
    } else {
        if (ext) {
            try writer.print("{s:<20} index {:>4}, module {:>4}, arity {:>4}, {s}\n", .{ name, index, module, arity, fn_name });
        } else {
            try writer.print("{s:<20} index {:>4}, arity {:>4}, {s}\n", .{ name, index, arity, fn_name });
        }
    }

    return offset + 3 + ext_offset;
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

fn structLiteral(self: *Self, writer: *Writer, name: []const u8, ext: bool, offset: usize) Writer.Error!usize {
    const ext_offset = @intFromBool(ext);

    const index = self.chunk.code.items[offset + 1];
    const module = if (ext) self.chunk.code.items[offset + 2] else self.module.toInt();
    const arity = self.chunk.code.items[offset + 2 + ext_offset];
    const sym = self.modules.getSymbol(.toIndex(module), index, .structure);

    if (self.render_mode == .@"test") {
        if (ext) {
            try writer.print("{s} index {}, module {}, arity {}, {s}\n", .{ name, index, module, arity, sym.name });
        } else {
            try writer.print("{s} index {}, arity {}, {s}\n", .{ name, index, arity, sym.name });
        }
    } else {
        if (ext) {
            try writer.print("{s:<20} index {:>4}, module {:>4}, arity {:>4}, {s}\n", .{ name, index, module, arity, sym.name });
        } else {
            try writer.print("{s:<20} index {:>4}, arity {:>4}, {s}\n", .{ name, index, arity, sym.name });
        }
    }

    return offset + 3 + ext_offset;
}

fn unionConstr(self: *Self, writer: *Writer, name: []const u8, ext: bool, offset: usize) Writer.Error!usize {
    const ext_offset = @intFromBool(ext);

    const index = self.chunk.code.items[offset + 1];
    const module = if (ext) self.chunk.code.items[offset + 2] else self.module.toInt();
    const tag = self.chunk.code.items[offset + 2 + ext_offset];
    const sym = self.modules.getSymbol(.toIndex(module), index, .@"union");

    if (self.render_mode == .@"test") {
        if (ext) {
            try writer.print("{s} index {}, module {}, tag {}\n", .{ name, index, module, tag });
        } else {
            try writer.print("{s} index {}, tag {}, {s}\n", .{ name, index, tag, sym.name });
        }
    } else {
        if (ext) {
            try writer.print("{s:<20} index {:>4}, module {:>4}, tag {:>4}\n", .{ name, index, module, tag });
        } else {
            try writer.print("{s:<20} index {:>4}, tag {:>4}, {s}\n", .{ name, index, tag, sym.name });
        }
    }

    return offset + 3 + ext_offset;
}
