const std = @import("std");
const Io = std.Io;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const FieldEnum = std.meta.FieldEnum;

const Disassembler = @import("Disassembler.zig");
const ir = @import("../analyzer/ir.zig");
const Instruction = ir.Instruction;
const State = @import("../pipeline/State.zig");
const ModIndex = @import("../pipeline/ModuleManager.zig").Index;
const Obj = @import("../runtime/Obj.zig");
const Value = @import("../runtime/values.zig").Value;
const Chunk = @import("Chunk.zig");
const OpCode = Chunk.OpCode;
const CompilerMsg = @import("compiler_msg.zig").CompilerMsg;
const ConstInterner = @import("../analyzer/ConstantInterner.zig");
const Constant = ConstInterner.Constant;
const ConstIdx = ConstInterner.ConstIdx;
const Artifacts = @import("Artifacts.zig");
const Linker = @import("Linker.zig");

const misc = @import("misc");
const Interner = misc.Interner;
const GenReport = misc.reporter.GenReport;
const oom = misc.oom;

pub const CompilationUnit = struct {
    io: Io,
    alloc: Allocator,
    state: *State,
    mod_index: ModIndex,
    compiler: Compiler,
    errs: ArrayList(CompilerReport),
    instr_data: []const Instruction.Data,
    instr_lines: []const usize,
    constants: []const Constant,
    line: usize,
    compiled_constants: misc.Set(usize),
    render: bool,

    const Self = @This();
    const Error = error{ Err, TooManyConst } || std.Io.Writer.Error;
    const CompilerReport = GenReport(CompilerMsg);

    pub fn init(io: Io, alloc: Allocator, state: *State, mod_index: ModIndex, render: bool) Self {
        return .{
            .io = io,
            .alloc = alloc,
            .state = state,
            .mod_index = mod_index,
            .compiler = undefined,
            .errs = .empty,
            .instr_data = undefined,
            .instr_lines = undefined,
            .constants = state.const_interner.constants.items,
            .line = 0,
            .compiled_constants = .empty,
            .render = render,
        };
    }

    pub fn compile(
        self: *Self,
        instr_data: []const Instruction.Data,
        roots: []const ir.Index,
        instr_lines: []const usize,
        main_index: ?usize,
    ) !*Obj.Function {
        self.instr_data = instr_data;
        self.instr_lines = instr_lines;

        if (self.render) {
            var buf: [256]u8 = undefined;
            var stdout = std.Io.File.stdout().writer(self.io, &buf);
            const mod_name = self.state.interner.getKey(self.state.modules.getFromIndex(self.mod_index).name).?;
            stdout.interface.print("//---- {s} ----\n\n", .{mod_name}) catch oom();
            stdout.interface.flush() catch oom();
        }

        // TODO: protect cast
        self.compiler = Compiler.init(self, "global scope", 0, @intCast(self.mod_index.toInt()));

        for (roots) |root| {
            try self.compiler.compileInstr(root);
        }

        if (main_index) |idx| {
            // TODO: protect
            const index = self.state.artifacts.getIndex(.function, self.mod_index, idx);
            self.compiler.writeOpAndByte(.call, @intCast(index));
            self.compiler.writeByte(0);
        } else {
            self.compiler.writeOp(.exit_repl);
        }

        return self.compiler.end();
    }
};

const Compiler = struct {
    alloc: Allocator,
    manager: *CompilationUnit,
    interner: *const Interner,
    artifacts: *Artifacts,
    module: ModIndex,
    function: *Obj.Function,
    block_stack: BlockStack,
    state: Context = .{},

    const Self = @This();
    const BlockStack = struct {
        stack: ArrayList(Block),

        const Block = struct {
            jumps: ArrayList(Jump),
        };
        const Jump = struct {
            kind: Kind,
            instr: usize,

            const Kind = enum { @"break", @"continue" };
        };

        pub const empty: BlockStack = .{ .stack = .empty };

        pub fn open(self: *BlockStack, alloc: Allocator) void {
            self.stack.append(alloc, .{ .jumps = .empty }) catch oom();
        }

        pub fn close(self: *BlockStack) Block {
            return self.stack.pop().?;
        }

        /// Adds a jump in the block at `depth`. `depth` is generated from bottom to top in *Analyzer*
        pub fn add(self: *BlockStack, alloc: Allocator, kind: Jump.Kind, instr: usize, depth: usize) void {
            self.stack.items[self.stack.items.len - 1 - depth].jumps.append(alloc, .{ .kind = kind, .instr = instr }) catch oom();
        }
    };
    const Error = CompilationUnit.Error;

    const CompilerReport = GenReport(CompilerMsg);
    // TODO: could make a generic construct with only flags like this (share with other contexts)
    const Context = struct {
        in_cf: bool = false,

        // No duplication for:
        // all lvalue:  lvalue = ...
        // all invoke:  p.speak()          ---   a = p.speak()
        // all bound:   p.speak            ---   a = p.speak
        // all pointer: *foo.a == *foo.a
        dup: bool = true,

        pub fn setAndGetPrev(self: *Context, comptime f: FieldEnum(Context), value: @FieldType(Context, @tagName(f))) @TypeOf(value) {
            const prev = @field(self, @tagName(f));
            @field(self, @tagName(f)) = value;

            return prev;
        }
    };
    const FnKind = enum { global, @"fn", method };

    pub fn init(manager: *CompilationUnit, name: []const u8, type_id: ir.TypeId, module_index: usize) Self {
        return .{
            .alloc = manager.alloc,
            .interner = &manager.state.interner,
            .artifacts = &manager.state.artifacts,
            .manager = manager,
            .module = manager.mod_index,
            .function = Obj.Function.create(
                manager.alloc,
                name,
                type_id,
                module_index,
            ),
            .block_stack = .empty,
        };
    }

    fn at(self: *const Self, index: usize) Instruction.Data {
        return self.manager.instr_data[index];
    }

    fn eof(self: *const Self) bool {
        return self.manager.instr_idx == self.manager.instr_data.len;
    }

    fn setInstrIndexGetPrev(self: *Self, index: usize) usize {
        const variable_instr = self.manager.instr_idx;
        self.manager.instr_idx = index;
        return variable_instr;
    }

    /// Writes an OpCode to the current chunk
    fn writeOp(self: *Self, op: OpCode) void {
        self.function.chunk.writeOp(self.alloc, op, self.manager.line);
    }

    /// Writes a byte to the current chunk
    fn writeByte(self: *Self, byte: u8) void {
        self.function.chunk.writeByte(self.alloc, byte, self.manager.line);
    }

    /// Writes an OpCode and a byte to the current chunk
    fn writeOpAndByte(self: *Self, op: OpCode, byte: u8) void {
        self.writeOp(op);
        self.writeByte(byte);
    }

    /// Writes a u16 value in two separate bytes
    fn writeShort(self: *Self, value: u16) void {
        self.writeByte(@as(u8, @intCast(value >> 8)));
        self.writeByte(@intCast(value & 0xff));
    }

    fn writeOpAndMaybeShort(self: *Self, op: OpCode, value: usize) Error!void {
        if (value >= std.math.maxInt(u16) + 1) {
            return error.TooManyConst;
        } else if (value >= std.math.maxInt(u8) + 1) {
            self.writeOp(.wide);
            self.writeOp(op);
            self.writeShort(@intCast(value));
        } else {
            self.writeOpAndByte(op, @intCast(value));
        }
    }

    fn emitJump(self: *Self, kind: OpCode) usize {
        const chunk = &self.function.chunk;
        chunk.writeOp(self.alloc, kind, self.manager.line);
        chunk.writeByte(self.alloc, 0xff, self.manager.line);
        chunk.writeByte(self.alloc, 0xff, self.manager.line);

        return chunk.code.items.len - 2;
    }

    fn patchJump(self: *Self, offset: usize) Error!void {
        const chunk = &self.function.chunk;
        // -2 for the two 8bits jump value (cf emit jump)
        const jump = chunk.code.items.len - offset - 2;

        // TODO: proper error handling
        if (jump > std.math.maxInt(u16)) {
            std.debug.print("Too much code to jump over", .{});
            return error.Err;
        }

        chunk.code.items[offset] = @as(u8, @intCast(jump >> 8));
        chunk.code.items[offset + 1] = @intCast(jump & 0xff);
    }

    fn emitLoop(self: *Self, loop_start: usize) Error!void {
        self.writeOp(.loop);
        // +2 for loop own operands (jump offset on 16bits)
        const jump_offset = self.function.chunk.code.items.len - loop_start + 2;

        // TODO: Error handling
        if (jump_offset > std.math.maxInt(u16)) {
            @panic("loop body too large\n");
        }

        self.writeByte(@intCast(jump_offset >> 8));
        self.writeByte(@intCast(jump_offset & 0xff));
    }

    fn patchLoop(self: *Self, offset: usize, loop_start: usize) Error!void {
        const chunk = &self.function.chunk;
        // +2 for the two 8bits jump value (cf emit jump)
        const jump_offset = offset - loop_start + 2;

        // TODO: Error handling
        if (jump_offset > std.math.maxInt(u16)) {
            @panic("loop body too large\n");
        }

        chunk.code.items[offset] = @as(u8, @intCast(jump_offset >> 8));
        chunk.code.items[offset + 1] = @intCast(jump_offset & 0xff);
    }

    pub fn end(self: *Self) *Obj.Function {
        if (self.manager.render) {
            var alloc_writer: std.Io.Writer.Allocating = .init(self.alloc);
            defer alloc_writer.deinit();

            var dis = Disassembler.init(&self.function.chunk, self.artifacts);
            dis.disChunk(&alloc_writer.writer, self.function.name);

            var buf: [1024]u8 = undefined;
            var stdout_writer = std.Io.File.stdout().writer(self.manager.io, &buf);
            const stdout = &stdout_writer.interface;

            stdout.print("{s}\n", .{alloc_writer.writer.buffered()}) catch oom();
            stdout.flush() catch oom();
        }

        return self.function;
    }

    fn writePops(self: *Self, count: usize) void {
        // TODO: error
        switch (count) {
            0 => {},
            1 => self.writeOp(.pop),
            2 => self.writeOp(.pop2),
            3 => self.writeOp(.pop3),
            else => |c| self.writeOpAndByte(.popn, @intCast(c)),
        }
    }

    /// Creates a symbol based on the opcode. If module index isn't null, uses the `_ext` version of the opcode
    fn symbolAccess(self: *Self, comptime op: OpCode, sym_data: Instruction.LoadSymbol) void {
        const index = switch (op) {
            .load_fn => self.artifacts.getIndex(.function, sym_data.module, sym_data.symbol),
            .load_fn_zig => self.artifacts.getIndex(.zig_function, sym_data.module, sym_data.symbol),
            .struct_lit => self.artifacts.getIndex(.structure, sym_data.module, sym_data.symbol),
            .struct_lit_c => self.artifacts.getIndex(.c_structure, sym_data.module, sym_data.symbol),
            .union_constr => self.artifacts.getIndex(.@"union", sym_data.module, sym_data.symbol),
            else => unreachable,
        };

        self.writeOpAndByte(op, @intCast(index));
    }

    fn compileInstr(self: *Self, instr: ir.Index) Error!void {
        self.manager.line = self.manager.instr_lines[instr];

        try switch (self.manager.instr_data[instr]) {
            .array => |*data| self.array(data),
            .assignment => |*data| self.assignment(data),
            .binop => |*data| self.binop(data),
            .block => |*data| self.block(data),
            .box => |index| self.wrappedInstr(.box, index),
            .bound_method => |data| self.boundMethod(data),
            .@"break" => |data| self.breakInstr(data),
            .call => |*data| self.call(data),

            .constant => |data| self.constant(data.index, self.module, true),
            .@"continue" => |data| self.continueInstr(data),
            .deref => |index| self.wrappedInstr(.deref, index),
            .discard => |index| self.wrappedInstrNoDup(.pop, index),

            .enum_decl => |*data| self.enumDecl(data),
            .enum_tag => |index| self.getTag(index, .@"enum"),
            .fail => |data| self.returnInstr(data),
            .field => |*data| self.field(data),
            .fn_decl => |*data| self.fnDecl(data),
            .cfn_decl => |*data| self.cFnDecl(data),
            .for_loop => |data| self.forLoop(data),
            .identifier => |*data| self.identifier(data),
            .@"if" => |*data| self.ifInstr(data),
            .in => |data| self.in(data),
            .indexing => |data| self.indexing(data),
            .int_to_float => |index| self.wrappedInstr(.int_to_float, index),

            // Standalone 'load_symbol' can only mean that we're loading a function to bind it to a runtime value
            .load_symbol => |data| switch (data.lang) {
                .ray => self.symbolAccess(.load_fn, data),
                .zig => self.symbolAccess(.load_fn_zig, data),
                .c => @panic("TODO"),
            },

            .match => |*data| self.match(data),
            .match_type => |data| self.matchType(data),
            .multiple_var_decl => |*data| self.multipleVarDecl(data),

            // Used in `call`, not meant to be accessed directly
            .obj_func => unreachable,

            // In case of nullable pattern, we don't replace top of stack with the bool result of
            // comparison because if it's true, it's gonna be popped and so last value on stack
            // will be the one extracted, it acts as if we just declared the value in scope
            .pat_nullable => |index| self.wrappedInstr(.ne_null_push, index),

            .pointer => |index| self.pointer(index),
            .pop => |index| self.wrappedInstrNoDup(.pop, index),
            .range => |data| self.range(data),
            .@"return" => |data| self.returnInstr(data),
            .string_interp => |data| self.stringInterp(data),
            .struct_decl => |*data| self.structDecl(data),
            .cstruct_decl => |*data| self.cStructDecl(data),
            .struct_literal => |data| self.structLiteral(data),
            .trait_decl => |data| self.traitDecl(data),
            .trait_obj => |data| self.traitObj(data),
            .trap => |data| self.trap(data),
            .unary => |*data| self.unary(data),
            .unbox => |index| self.wrappedInstrNoDup(.unbox, index),
            .union_constr => |data| self.unionConstr(data),
            .union_decl => |*data| self.unionDecl(data),
            .union_tag => |index| self.getTag(index, .@"union"),
            .union_unwrap => |data| self.unionUnwrap(data),
            .var_decl => |*data| self.varDecl(data),
            .@"while" => |data| self.whileInstr(data),

            .noop => {},
        };
    }

    /// Compiles an instruction while desactivating duplication state
    fn compileInstrNoDup(self: *Self, instr: usize) Error!void {
        const prev_dup = self.state.setAndGetPrev(.dup, false);
        defer self.state.dup = prev_dup;
        try self.compileInstr(instr);
    }

    fn wrappedInstr(self: *Self, op: OpCode, index: usize) Error!void {
        try self.compileInstr(index);
        self.writeOp(op);
    }

    fn wrappedInstrNoDup(self: *Self, op: OpCode, index: usize) Error!void {
        try self.compileInstrNoDup(index);
        self.writeOp(op);
    }

    fn array(self: *Self, data: *const Instruction.Array) Error!void {
        for (data.values) |value| {
            try self.compileInstr(value);
        }
        try self.writeOpAndMaybeShort(.array_new, data.values.len);
        self.writeShort(data.type_id);
    }

    fn arrayAssign(self: *Self, data: Instruction.Indexing) Error!void {
        try self.compileInstr(data.expr);
        try self.compileInstr(data.index);
        self.writeOp(.array_set);
    }

    fn assignment(self: *Self, data: *const Instruction.Assignment) Error!void {
        try self.compileInstr(data.value);

        self.state.dup = false;
        defer self.state.dup = true;
        const variable_data, const unbox = switch (self.at(data.assigne)) {
            .deref => |deref| {
                try self.compileInstr(deref);
                self.writeOp(.ptr_store);
                return;
            },
            .identifier => |variable| .{ variable, false },
            .indexing => |indexing_data| return self.arrayAssign(indexing_data),
            .field => |*field_data| return self.fieldAssignment(field_data),
            .unbox => |index| .{ self.at(index).identifier, true },
            else => unreachable,
        };

        // BUG: Protect the cast, we can't have more than 256 variable to lookup for now
        switch (variable_data.kind) {
            .local => self.writeOpAndByte(
                if (unbox) .set_local_box else .set_local,
                @intCast(variable_data.index),
            ),

            .global => |glob| self.writeOpAndByte(
                .set_global,
                @intCast(self.artifacts.getIndex(.global, glob.module orelse self.module, variable_data.index)),
            ),
        }
    }

    fn fieldAssignment(self: *Self, data: *const Instruction.Field) Error!void {
        try self.compileInstr(data.structure);
        if (data.kind == .c) {
            self.writeOpAndByte(.set_field_c, @intCast(data.index));
        } else {
            self.writeOpAndByte(.set_field, @intCast(data.index));
        }
    }

    fn binop(self: *Self, data: *const Instruction.Binop) Error!void {
        if (data.op == .@"and" or data.op == .@"or") return self.logicalBinop(data);
        if (data.op == .eq_null or data.op == .ne_null) return self.nullBinop(data);

        try self.compileInstr(data.lhs);
        try self.compileInstr(data.rhs);

        self.writeOp(
            switch (data.op) {
                .add_float => .add_float,
                .add_int => .add_int,
                .add_str => .str_cat,
                .binary_and => .binary_and,
                .binary_or => .binary_or,
                .binary_xor => .binary_xor,
                .bang_bang => .fallback_err,
                .div_float => .div_float,
                .div_int => .div_int,
                .eq_bool => .eq_bool,
                .eq_float => .eq_float,
                .eq_int => .eq_int,
                .eq_ptr => .eq_ptr,
                .eq_str => .eq_str,
                .ge_float => .ge_float,
                .ge_int => .ge_int,
                .gt_float => .gt_float,
                .gt_int => .gt_int,
                .le_float => .le_float,
                .le_int => .le_int,
                .lt_float => .lt_float,
                .lt_int => .lt_int,
                .mod_float => .mod_float,
                .mod_int => .mod_int,
                .mul_float => .mul_float,
                .mul_int => .mul_int,
                .mul_str => .str_mul,
                .ne_bool => .ne_bool,
                .ne_float => .ne_float,
                .ne_int => .ne_int,
                .ne_ptr => .ne_ptr,
                .ne_str => .ne_str,
                .question_mark_question_mark => .fallback_opt,
                .shift_left => .shift_left,
                .shift_right => .shift_right,
                .sub_float => .sub_float,
                .sub_int => .sub_int,
                else => unreachable,
            },
        );
    }

    fn logicalBinop(self: *Self, data: *const Instruction.Binop) Error!void {
        switch (data.op) {
            .@"and" => {
                try self.compileInstr(data.lhs);
                const end_jump = self.emitJump(.jump_false);
                // If true, pop the value, else the 'false' remains on top of stack
                self.writeOp(.pop);
                try self.compileInstr(data.rhs);
                try self.patchJump(end_jump);
            },
            .@"or" => {
                try self.compileInstr(data.lhs);
                const else_jump = self.emitJump(.jump_true);
                self.writeOp(.pop);
                try self.compileInstr(data.rhs);
                try self.patchJump(else_jump);
            },
            else => unreachable,
        }
    }

    fn nullBinop(self: *Self, data: *const Instruction.Binop) Error!void {
        try self.compileInstr(data.lhs);
        self.writeOp(if (data.op == .eq_null) .eq_null else .ne_null);
    }

    fn getTag(self: *Self, instr: usize, kind: enum { @"enum", @"union" }) Error!void {
        try self.compileInstrNoDup(instr);
        self.writeOp(if (kind == .@"enum") .get_enum_tag else .get_union_tag);
    }

    fn block(self: *Self, data: *const Instruction.Block) Error!void {
        self.block_stack.open(self.alloc);

        for (data.instrs) |instr| {
            try self.compileInstr(instr);
        }

        self.writePops(data.pop_count);
        try self.closeAndPatchBlock();

        if (data.is_expr) self.writeOp(.load_blk_val);
    }

    // TODO: protect cast
    fn boundMethod(self: *Self, data: Instruction.BoundMethod) Error!void {
        try self.compileInstrNoDup(data.structure);

        const index = self.artifacts.getIndex(.function, self.module, data.index);
        self.writeOpAndByte(.bound_method, @intCast(index));
    }

    fn breakInstr(self: *Self, data: Instruction.Break) Error!void {
        if (data.instr) |instr| {
            try self.compileInstr(instr);
            // Load is done by end of `block`
            self.writeOp(.store_blk_val);
        }

        self.writePops(data.pop_count);
        self.block_stack.add(self.alloc, .@"break", self.emitJump(.jump), data.depth);
    }

    fn call(self: *Self, data: *const Instruction.Call) Error!void {
        switch (self.at(data.callee)) {
            .field => |f| switch (f.kind) {
                .function => return self.invoke(data, f),
                .virtual => return self.virtualCall(data, f),
                // If we call a field holding a function it is dynamically resolved by 'call_dyn'
                .ray => {},
                // No methods on those
                .c, .zig => unreachable,
            },
            .load_symbol => |sym| {
                return self.callSymbol(data, 0, sym.module, sym.symbol);
            },
            .obj_func => |obj_data| {
                return self.callObjFn(obj_data, data.args);
            },
            // Dynamic call resolved at runtime
            else => {},
        }

        // For functions bounded to runtime values (including structure fields)
        try self.compileInstrNoDup(data.callee);
        try self.compileArgs(data.args);
        // TODO: protect cast
        self.writeOpAndByte(.call_dyn, @intCast(data.args.len));
    }

    fn invoke(self: *Self, data: *const Instruction.Call, callee: Instruction.Field) Error!void {
        try self.compileInstrNoDup(callee.structure);
        try self.callSymbol(data, 1, data.module, callee.index);
    }

    fn virtualCall(self: *Self, data: *const Instruction.Call, callee: Instruction.Field) Error!void {
        // Trait object don't have to be cloned they don't contain anything
        try self.compileInstrNoDup(callee.structure);
        try self.compileArgs(data.args);
        self.writeOpAndByte(.call_virtual, @intCast(callee.index));
        // We add one because we invoke the virtual on the trait object so as `callSymbol`, we add 1
        self.writeByte(@intCast(data.args.len + 1));
    }

    // TODO: protect casts
    fn callSymbol(
        self: *Self,
        data: *const Instruction.Call,
        arity_offset: usize,
        mod_index: ModIndex,
        sym_index: usize,
    ) Error!void {
        try self.compileArgs(data.args);

        const index = switch (data.kind) {
            .normal, .method, .bound => self.artifacts.getIndex(.function, mod_index, sym_index),
            .c => self.artifacts.getIndex(.c_function, mod_index, sym_index),
            .zig, .zig_method => self.artifacts.getIndex(.zig_function, mod_index, sym_index),
            .intrinsic => unreachable,
        };
        const op: OpCode = switch (data.kind) {
            .c => .call_c,
            .zig, .zig_method => .call_zig,
            .normal, .method, .bound => .call,
            // Only called at analyzis time
            .intrinsic => unreachable,
        };
        self.writeOpAndByte(op, @intCast(index));
        self.writeByte(@intCast(data.args.len + arity_offset));
    }

    fn callObjFn(self: *Self, data: Instruction.ObjFn, args: []const Instruction.Arg) Error!void {
        const prev_dup = self.state.setAndGetPrev(.dup, false);
        defer self.state.dup = prev_dup;

        try self.compileInstr(data.obj);
        try self.compileArgs(args);
        self.writeOpAndByte(if (data.kind == .array) .call_array else .call_string, @intCast(data.fn_index));
        self.writeByte(@intCast(args.len));
    }

    fn compileArgs(self: *Self, args: []const Instruction.Arg) Error!void {
        for (args) |arg| {
            switch (arg) {
                .default => |def| try self.constant(def.constant, def.module, true),
                .instr => |instr| try self.compileInstr(instr),
            }
        }
    }

    /// Compiles any callable (free functions, members functions, ...)
    fn compileFnBody(self: *Self, name: []const u8, data: *const Instruction.FnDecl) Error!*Obj.Function {
        var compiler = Compiler.init(self.manager, name, data.type_id, self.function.module_index);
        const index = self.artifacts.addPlaceholder(self.alloc, .function, self.module, data.sym_index);

        try self.defaults(data.defaults);

        for (data.body) |instr| {
            try compiler.compileInstr(instr);
        }

        // If the function doesn't return by itself, we emit one naked return
        if (!data.returns) {
            compiler.writeOp(.ret_naked);
        }

        self.artifacts.set(.function, index, compiler.function);

        return compiler.end();
    }

    fn fnDecl(self: *Self, data: *const Instruction.FnDecl) Error!void {
        const fn_name = if (data.name) |idx| self.interner.getKey(idx).? else "anonymus";
        _ = try self.compileFnBody(fn_name, data);

        if (data.captures.len > 0) {
            try self.compileClosure(data);
        }
    }

    fn compileClosure(self: *Self, data: *const Instruction.FnDecl) Error!void {
        self.symbolAccess(.load_fn, .{ .module = self.module, .symbol = @intCast(data.sym_index) });

        for (data.captures) |*capt| {
            try self.capture(capt);
        }

        self.writeOpAndByte(.closure, @intCast(data.captures.len));
    }

    fn cFnDecl(self: *Self, data: *const Instruction.CFnDecl) Error!void {
        const fn_name = self.interner.getKey(data.name).?;
        const func = Obj.CFn.create(self.alloc, fn_name, data.func, data.returns);
        self.artifacts.add(self.alloc, .c_function, self.module, data.sym_index, func);
    }

    fn containerFnDecls(self: *Self, decls: []const ir.Index) Error!void {
        for (decls) |decl| {
            const fn_data = self.manager.instr_data[decl].fn_decl;
            // Structures and enums' functions have a name
            // TODO: not all the time
            const fn_name = self.interner.getKey(fn_data.name orelse unreachable).?;
            _ = try self.compileFnBody(fn_name, &fn_data);
        }
    }

    fn containerTraitDecls(self: *Self, decls: []const Instruction.Trait) Error!void {
        for (decls) |decl| {
            var vtable: Artifacts.VTable = .{
                .name = self.alloc.dupe(u8, self.interner.getKey(decl.name).?) catch oom(),
                .functions = self.alloc.alloc(*Obj.Function, decl.funcs.len) catch oom(),
            };

            for (decl.funcs) |func| {
                switch (func.func) {
                    .compiled => |compiled| {
                        vtable.functions[func.index] = self.artifacts.getFromKey(
                            .function,
                            compiled.mod_index orelse self.module,
                            compiled.sym_index,
                        ).*;
                    },
                    .instr => |instr| {
                        const fn_data = self.manager.instr_data[instr].fn_decl;
                        // Structures and enums' functions have a name
                        // TODO: not all the time
                        const fn_name = self.interner.getKey(fn_data.name orelse unreachable).?;
                        const body = try self.compileFnBody(fn_name, &fn_data);
                        vtable.functions[func.index] = body;
                    },
                }
            }

            self.artifacts.add(self.alloc, .vtable, self.module, decl.vtable_index, vtable);
        }
    }

    fn defaults(self: *Self, instrs: []const ir.Index) Error!void {
        for (instrs) |instr| {
            // TODO: protect this
            const const_data = self.at(instr).constant;
            try self.constant(const_data.index, self.module, false);
        }
    }

    // TODO: protect cast
    fn capture(self: *Self, data: *const Instruction.FnDecl.Capture) Error!void {
        self.writeOpAndByte(if (data.local) .get_capt_local else .get_capt_frame, @intCast(data.index));
    }

    fn cast(self: *Self, typ: ir.Type) Error!void {
        switch (typ) {
            .float => self.writeOp(.cast_to_float, self.getLineNumber()),
            .int => unreachable,
        }
    }

    fn compileConstant(self: *Self, index: ConstIdx) Error!void {
        const idx = index.toInt();
        const gop = self.manager.compiled_constants.getOrPut(self.alloc, idx) catch oom();
        if (gop.found_existing) {
            return;
        }

        const cte = self.manager.constants[idx];
        const value = switch (cte) {
            .array => |arr| arr: {
                var vals = ArrayList(Value).initCapacity(self.alloc, arr.values.len) catch oom();
                for (arr.values) |val| {
                    try self.compileConstant(val);
                    vals.appendAssumeCapacity(self.artifacts.getFromKey(.constant, self.module, val.toInt()).*);
                }

                break :arr Value.makeObj(Obj.Array.createComptime(
                    self.alloc,
                    @intCast(arr.type_id),
                    vals.toOwnedSlice(self.alloc) catch oom(),
                ).asObj());
            },
            .bool => |c| Value.makeBool(c),
            .int => |val| Value.makeInt(val),
            .float => |val| Value.makeFloat(val),
            .enum_lit => |e| Value.makeObj(Obj.Enum.create(
                self.alloc,
                self.artifacts.getFromKey(.@"enum", e.symbol.module, e.symbol.symbol),
                @intCast(e.tag_index),
            ).asObj()),
            .union_lit => |u| Value.makeObj(Obj.Union.createComptime(
                self.alloc,
                self.artifacts.getFromKey(.@"union", u.symbol.module, u.symbol.symbol),
                @intCast(u.tag_index),
                .null_,
            ).asObj()),
            .struct_lit => |s| s: {
                var vals = ArrayList(Value).initCapacity(self.alloc, s.values.len) catch oom();
                for (s.values) |val| {
                    try self.compileConstant(val);
                    vals.appendAssumeCapacity(
                        self.artifacts.getFromKey(.constant, self.module, val.toInt()).*,
                    );
                }

                const obj = if (s.lang == .ray)
                    Obj.Structure.createComptime(
                        self.alloc,
                        self.artifacts,
                        self.artifacts.getIndex(.structure, s.symbol.module, s.symbol.symbol),
                        vals.toOwnedSlice(self.alloc) catch oom(),
                    ).asObj()
                else
                    Obj.CStructure.createComptime(
                        self.alloc,
                        self.artifacts.getFromKey(.c_structure, s.symbol.module, s.symbol.symbol),
                        vals.toOwnedSlice(self.alloc) catch oom(),
                    ).asObj();

                break :s Value.makeObj(obj);
            },

            .null => Value.null_,
            .string => |val| Value.makeObj(Obj.String.comptimeCopy(
                self.alloc,
                &self.manager.state.strings,
                self.interner.getKey(val).?,
            ).asObj()),
        };

        self.artifacts.add(self.alloc, .constant, self.module, idx, value);
    }

    // TODO: protect casts
    fn constant(self: *Self, index: ConstIdx, mod: ModIndex, push_to_stack: bool) Error!void {
        try self.compileConstant(index);

        if (!push_to_stack) return;

        switch (index) {
            .true => self.writeOp(.push_true),
            .false => self.writeOp(.push_false),
            .null => self.writeOp(.push_null),
            else => |i| try self.writeOpAndMaybeShort(
                .load_const,
                self.artifacts.getIndex(.constant, mod, i.toInt()),
            ),
        }
    }

    fn continueInstr(self: *Self, data: Instruction.Continue) Error!void {
        self.writePops(data.pop_count);
        self.block_stack.add(self.alloc, .@"continue", self.emitJump(.loop), data.depth);
    }

    fn enumDecl(self: *Self, data: *const Instruction.EnumDecl) Error!void {
        self.artifacts.add(self.alloc, .@"enum", self.module, data.sym_index, .{
            .name = self.alloc.dupe(u8, self.interner.getKey(data.name).?) catch oom(),
            .tags = data.tags,
            .discriminants = data.discriminants,
            .type_id = data.type_id,
        });

        try self.containerFnDecls(data.functions);
        try self.containerTraitDecls(data.traits);
    }

    fn field(self: *Self, data: *const Instruction.Field) Error!void {
        try self.compileInstrNoDup(data.structure);

        self.writeOpAndByte(
            switch (data.kind) {
                .ray => if (self.state.dup) .get_field_dup else .get_field,
                .zig => .get_field_zig,
                .c => .get_field_c,
                else => unreachable,
            },
            @intCast(data.index),
        );
    }

    fn forLoop(self: *Self, data: Instruction.For) Error!void {
        // Values are copied by `next` function on iterator
        try self.compileInstrNoDup(data.expr);
        self.writeOp(switch (data.kind) {
            .array => .iter_new_arr,
            .array_ptr => .iter_new_arr_ptr,
            .range => .iter_new_range,
            .str => .iter_new_str,
        });

        self.block_stack.open(self.alloc);
        const loop_start = self.function.chunk.code.items.len;

        self.writeOp(if (data.use_index) .iter_next_index else .iter_next);
        const iter_end = self.emitJump(.jump_null);

        const body = self.manager.instr_data[data.body].block;

        for (body.instrs) |instr| {
            try self.compileInstr(instr);
        }

        // We patch them before cleaning the scope and then looping so that each continue don't do it
        try self.patchContinueInBlock(loop_start);

        self.writePops(body.pop_count);

        try self.emitLoop(loop_start);
        try self.patchJump(iter_end);

        // Null value and iterator
        self.writeOp(if (data.use_index) .pop3 else .pop2);

        try self.closeAndPatchBlock();
    }

    fn closeAndPatchBlock(self: *Self) Error!void {
        const popped = self.block_stack.close();

        for (popped.jumps.items) |jump| {
            if (jump.kind == .@"break") {
                try self.patchJump(jump.instr);
            }
        }
    }

    fn patchContinueInBlock(self: *Self, loop_start: usize) Error!void {
        for (self.block_stack.stack.getLast().jumps.items) |jump| {
            if (jump.kind == .@"continue") {
                try self.patchLoop(jump.instr, loop_start);
            }
        }
    }

    fn identifier(self: *Self, data: *const Instruction.Variable) Error!void {
        // BUG: Protect the cast, we can't have more than 256 variable to lookup for now
        switch (data.kind) {
            .local => |d| self.writeOpAndByte(
                if (d.duplicable and self.state.dup) .get_local_dup else .get_local,
                @intCast(data.index),
            ),
            .global => |d| self.writeOpAndByte(
                if (self.state.dup) .get_global_dup else .get_global,
                @intCast(self.artifacts.getIndex(.global, d.module orelse self.module, data.index)),
            ),
        }
    }

    fn ifInstr(self: *Self, data: *const Instruction.If) Error!void {
        const is_null_pat = self.manager.instr_data[data.cond] == .pat_nullable;
        try self.compileInstr(data.cond);

        const then_jump = self.emitJump(.jump_false);
        // Pops the condition
        self.writeOp(.pop);

        // Then body
        try self.compileInstr(data.then);

        // Exits the if expression
        const else_jump = self.emitJump(.jump);
        try self.patchJump(then_jump);

        // If we go in the else branch, we pop the condition too
        // If the condition was a nullable pattern, the variable tested against `null` is still on
        // top of stack so we have to remove it
        self.writeOp(if (is_null_pat) .pop2 else .pop);

        // We insert a jump in the then body to be able to jump over the else branch
        // Otherwise, we just patch the then_jump
        if (data.@"else") |instr| {
            try self.compileInstr(instr);
        }

        try self.patchJump(else_jump);
    }

    fn in(self: *Self, data: Instruction.In) Error!void {
        try self.compileInstr(data.needle);
        try self.compileInstr(data.haystack);

        self.writeOp(switch (data.kind) {
            .array => .in_array,
            .range_int => .in_range_int,
            .range_float => .in_range_float,
            .string => .in_str,
        });
    }

    fn indexing(self: *Self, data: Instruction.Indexing) Error!void {
        try self.compileInstrNoDup(data.expr);
        try self.compileInstrNoDup(data.index);

        const op: OpCode = switch (data.index_kind) {
            .scalar => switch (data.kind) {
                .array => if (self.state.dup) .index_arr_dup else .index_arr,
                .str => .index_str,
            },
            .range => switch (data.kind) {
                .array => .index_range_arr,
                .str => .index_range_str,
            },
        };
        self.writeOp(op);
    }

    fn match(self: *Self, data: *const Instruction.Match) Error!void {
        if (data.kind == .@"enum") {
            try self.getTag(data.expr, .@"enum");
        } else if (data.kind == .@"union") {
            try self.getTag(data.expr, .@"union");
        } else {
            try self.compileInstrNoDup(data.expr);
        }

        var exit_jumps = ArrayList(usize).initCapacity(self.alloc, data.arms.len) catch oom();

        for (data.arms) |arm| {
            self.writeOp(.dup);

            switch (data.kind) {
                .bool => {
                    try self.compileInstr(arm.expr);
                    self.writeOp(.eq_bool);
                },
                .@"enum" => {
                    try self.getTag(arm.expr, .@"enum");
                    self.writeOp(.eq_int);
                },
                .float => {
                    try self.compileInstr(arm.expr);
                    if (self.at(arm.expr) == .range) {
                        self.writeOp(.in_range_float);
                    } else {
                        self.writeOp(.eq_float);
                    }
                },
                .int => {
                    try self.compileInstr(arm.expr);
                    if (self.at(arm.expr) == .range) {
                        self.writeOp(.in_range_int);
                    } else {
                        self.writeOp(.eq_int);
                    }
                },
                .string => {
                    try self.compileInstr(arm.expr);
                    self.writeOp(.eq_str);
                },
                .@"union" => {
                    try self.getTag(arm.expr, .@"union");
                    self.writeOp(.eq_int);
                },
            }

            const arm_jump = self.emitJump(.jump_false);
            // Pops the condition
            self.writeOp(.pop);
            // Arm body
            try self.compileInstr(arm.body);

            // Exits the when after arm body
            exit_jumps.appendAssumeCapacity(self.emitJump(.jump));

            // Skips to next arm
            try self.patchJump(arm_jump);
            // Pops the condition in case of false
            self.writeOp(.pop);
        }

        if (data.wildcard) |wc| {
            try self.compileInstr(wc);
        }

        for (exit_jumps.items) |jump| {
            try self.patchJump(jump);
        }

        // If we return a value, we pop the value matched on and leave the result on stack
        // equivalent to swapping the two first values then popping
        if (data.is_expr) {
            self.writeOp(.swap_pop);
        } else {
            self.writeOp(.pop);
        }
    }

    // TODO: do as Parser, provide a `armFn` and make a common `matchArm` for both value and type match
    fn matchType(self: *Self, data: Instruction.MatchType) Error!void {
        try self.compileInstrNoDup(data.expr);

        var exit_jumps = ArrayList(usize).initCapacity(self.alloc, data.arms.len) catch oom();

        for (data.arms) |arm| {
            self.writeOp(.dup);

            // Specialized because only objects hold a type id
            switch (arm.kind) {
                .int => self.writeOp(.is_int),
                .float => self.writeOp(.is_float),
                .bool => self.writeOp(.is_bool),
                .str => self.writeOp(.is_str),
                .obj => try self.writeOpAndMaybeShort(.is_type, arm.type_id),
            }

            const arm_jump = self.emitJump(.jump_false);
            // Pops the condition
            self.writeOp(.pop);
            // Arm body
            try self.compileInstr(arm.body);

            // Exits the when after arm body
            exit_jumps.appendAssumeCapacity(self.emitJump(.jump));

            // Skips to next arm
            try self.patchJump(arm_jump);
            // Pops the condition in case of false
            self.writeOp(.pop);
        }

        if (data.wildcard) |wc| {
            try self.compileInstr(wc);
        }

        for (exit_jumps.items) |jump| {
            try self.patchJump(jump);
        }

        // If we return a value, we pop the value matched on and leave the result on stack
        // equivalent to swapping the two first values then popping
        if (data.is_expr) {
            self.writeOp(.swap_pop);
        } else {
            self.writeOp(.pop);
        }
    }

    fn multipleVarDecl(self: *Self, data: *const Instruction.MultiVarDecl) Error!void {
        for (data.decls) |decl| {
            try self.compileInstr(decl);
        }
    }

    fn range(self: *Self, data: Instruction.Range) Error!void {
        try self.compileInstr(data.end);
        try self.compileInstr(data.start);
        self.writeOp(if (data.kind == .int) .range_new_int else .range_new_float);
    }

    fn pointer(self: *Self, instr: Instruction.Pointer) Error!void {
        switch (instr) {
            .array => |a| {
                try self.compileInstrNoDup(a.expr);
                try self.compileInstr(a.index);
                self.writeOp(.ptr_array);
            },
            .field => |f| {
                try self.compileInstrNoDup(f.structure);
                self.writeOpAndByte(.ptr_field, @intCast(f.index));
            },
            .variable => |v| switch (v.kind) {
                .local => self.writeOpAndByte(.ptr_local, @intCast(v.index)),
                .global => self.writeOpAndByte(.ptr_global, @intCast(v.index)),
            },
        }
    }

    fn returnInstr(self: *Self, data: Instruction.Return) Error!void {
        if (data.value) |val| {
            try self.compileInstr(val);
            self.writeOp(.ret);
        } else self.writeOp(.ret_naked);
    }

    fn stringInterp(self: *Self, data: Instruction.StringInterp) Error!void {
        // Compiles [literal, expr, literal, expr, ...]
        for (0..data.exprs.len) |i| {
            try self.constant(data.literals[i], self.module, true);
            try self.compileInstr(data.exprs[i]);
        }

        // Always one additional literal due to parsing
        try self.constant(data.literals[data.literals.len - 1], self.module, true);

        self.writeOpAndByte(.string_interp, @intCast(data.exprs.len));
    }

    fn structDecl(self: *Self, data: *const Instruction.StructDecl) Error!void {
        self.artifacts.add(self.alloc, .structure, self.module, data.sym_index, .{
            .name = self.alloc.dupe(u8, self.interner.getKey(data.name).?) catch oom(),
            .type_id = data.type_id,
            .fields = data.fields,
        });

        try self.defaults(data.default_fields);
        try self.containerFnDecls(data.functions);
        try self.containerTraitDecls(data.traits);
    }

    fn cStructDecl(self: *Self, data: *const Instruction.CStructDecl) Error!void {
        self.artifacts.add(self.alloc, .c_structure, self.module, data.sym_index, .{
            .name = self.alloc.dupe(u8, self.interner.getKey(data.name).?) catch oom(),
            .type_id = data.type_id,
            .layout = data.layout,
        });
    }

    // TODO: protect cast
    fn structLiteral(self: *Self, data: Instruction.StructLiteral) Error!void {
        try self.compileArgs(data.values);
        const load_sym = data.structure;

        if (load_sym.lang == .c) {
            self.symbolAccess(.struct_lit_c, load_sym);
        } else {
            self.symbolAccess(.struct_lit, load_sym);
        }
        self.writeByte(@intCast(data.values.len));
    }

    fn traitDecl(self: *Self, data: Instruction.TraitDecl) Error!void {
        try self.containerFnDecls(data.functions);
    }

    fn traitObj(self: *Self, data: Instruction.TraitObj) Error!void {
        // No need to duplicate because it's gonna be a pointer to an object
        try self.compileInstrNoDup(data.variable);
        // TODO: error
        self.writeOpAndByte(.trait_obj, @intCast(data.vtable_index));
    }

    fn trap(self: *Self, data: Instruction.Trap) Error!void {
        try self.compileInstr(data.lhs);
        const ok_jump = self.emitJump(.jump_no_err);
        try self.compileInstr(data.rhs);
        try self.patchJump(ok_jump);

        // If we use match directly on return value, it sits on top of stack, so we have to remove
        // it manually while leaving the result of the match on top of stack
        switch (data.kind) {
            .match => self.writeOp(if (self.at(data.rhs).match.is_expr) .swap_pop else .pop),
            .match_is => self.writeOp(if (self.at(data.rhs).match_type.is_expr) .swap_pop else .pop),
            else => {},
        }
    }

    fn unary(self: *Self, data: *const Instruction.Unary) Error!void {
        try self.compileInstr(data.instr);

        if (data.op == .minus) {
            self.writeOp(if (data.typ == .int) .neg_int else .neg_float);
        } else if (data.op == .tilde) {
            self.writeOp(.binary_neg);
        } else {
            self.writeOp(.not);
        }
    }

    fn unionDecl(self: *Self, data: *const Instruction.UnionDecl) Error!void {
        self.artifacts.add(self.alloc, .@"union", self.module, data.sym_index, .{
            .name = self.alloc.dupe(u8, self.interner.getKey(data.name).?) catch oom(),
            .tags = data.tags,
            .type_id = data.type_id,
            .is_err = data.is_err,
        });

        try self.containerFnDecls(data.functions);
        try self.containerTraitDecls(data.traits);
    }

    fn unionConstr(self: *Self, data: Instruction.UnionConstr) Error!void {
        // TODO: Error
        if (data.tag_lit.tag_index >= std.math.maxInt(u8)) {
            @panic("Union is to big, not implemented yet");
        }

        try self.compileInstr(data.arg);
        self.symbolAccess(.union_constr, data.tag_lit.symbol);
        self.writeByte(@intCast(data.tag_lit.tag_index));
    }

    fn unionUnwrap(self: *Self, data: Instruction.UnionUnwrap) Error!void {
        // TODO: Error
        if (data.tag_index >= std.math.maxInt(u8)) {
            @panic("Union is to big, not implemented yet");
        }

        try self.compileInstr(data.@"union");
        self.writeOpAndByte(.union_unwrap, @intCast(data.tag_index));
    }

    fn varDecl(self: *Self, data: *const Instruction.VarDecl) Error!void {
        // TODO: Protect the cast, we can't have more than 256 variable to lookup for now
        if (data.variable.kind == .global) {
            const value: Value = value: {
                const value_instr = data.value orelse break :value .null_;
                const const_index = self.at(value_instr).constant.index;
                try self.compileConstant(const_index);

                break :value self.artifacts.getFromKey(.constant, self.module, const_index.toInt()).*;
            };

            self.artifacts.add(self.alloc, .global, self.module, data.variable.index, value);
        }
        // Local value left on stack
        else {
            if (data.value) |val| {
                try self.compileInstr(val);
            } else {
                self.writeOp(.push_null);
            }

            if (data.box) {
                self.writeOp(.box);
            }
        }
    }

    fn whileInstr(self: *Self, data: Instruction.While) Error!void {
        const is_null_pat = self.manager.instr_data[data.cond] == .pat_nullable;

        self.block_stack.open(self.alloc);
        const loop_start = self.function.chunk.code.items.len;

        try self.compileInstr(data.cond);
        const exit_jump = self.emitJump(.jump_false);

        // If true
        self.writeOp(.pop);

        const body = self.manager.instr_data[data.body].block;

        for (body.instrs) |instr| {
            try self.compileInstr(instr);
        }

        // We patch them before cleaning the scope and then looping so that each continue don't do it
        try self.patchContinueInBlock(loop_start);

        self.writePops(body.pop_count);

        try self.emitLoop(loop_start);
        try self.patchJump(exit_jump);
        // If false
        // If the condition was null pattern, the variable tested against `null` is still on
        // top of stack so we have to remove it
        self.writeOp(if (is_null_pat) .pop2 else .pop);

        try self.closeAndPatchBlock();
    }
};
