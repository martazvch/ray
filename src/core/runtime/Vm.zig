const std = @import("std");
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;

const options = @import("options");

const TypeId = @import("../analyzer/types.zig").TypeId;
const OpCode = @import("../compiler/Chunk.zig").OpCode;
const Module = @import("../pipeline/ModuleManager.zig").Module;
const Disassembler = @import("../compiler/Disassembler.zig");
const oom = @import("misc").oom;
const Gc = @import("Gc.zig");
const Obj = @import("Obj.zig");
const Value = @import("values.zig").Value;
const State = @import("../pipeline/State.zig");

gc: Gc,
stack: Stack,
frame: *CallFrame,
frame_stack: FrameStack,
ip: [*]u8,
allocator: Allocator,
arena_comptime: std.heap.ArenaAllocator,
gc_alloc: Allocator,
strings: *std.AutoHashMapUnmanaged(usize, *Obj.String),
objects: ?*Obj,
modules: []Module,
c_fns: []*Obj.CFn,
zig_fns: []*Obj.ZigFn,
state: *State,

// Used ti-ype ids at runtime
arr_str_type_id: TypeId,

const Self = @This();
const Error = error{
    StackOverflow,
    OutOfBound,
    ModuloWith0,
    RangeIndexDecrease,
} || Allocator.Error;

pub fn init(self: *Self, allocator: Allocator, state: *State) void {
    self.arena_comptime = .init(allocator);
    self.allocator = self.arena_comptime.allocator();

    // TODO: pass an ObjectPoolAlloc?
    self.gc = .init(self, allocator);
    self.gc_alloc = self.gc.allocator();
    self.stack.init();
    self.strings = &state.strings;

    self.stack = .empty;
    self.stack.init();
    self.frame = undefined;
    self.frame_stack = .empty;
    self.objects = null;
    self.state = state;
    self.c_fns = state.native_reg.c_fns.items;
    self.zig_fns = state.native_reg.zig_fns.items;

    self.arr_str_type_id = state.type_interner.typeId(
        state.type_interner.intern(.{ .array = .{ .child = state.type_interner.getCached(.str) } }),
    );

    // In REPL/embedded mode, we won't call the main function (there is not)
    // so we increment ourself the frame stack (discaring the first one)
    // because the first function we would exit would cause the VM to stop
    // as there would not have any other frame
    if (state.config.embedded) {
        self.frame_stack.count += 1;
    }
}

pub fn deinit(self: *Self) void {
    self.arena_comptime.deinit();
    self.gc.deinit();
    self.freeObjects();
}

fn freeObjects(self: *Self) void {
    var object = self.objects;
    while (object) |obj| {
        const next = obj.next;
        obj.destroy(self);
        object = next;
    }
}

/// Returns the error gave in `kind` parameter and prints backtrace
fn err(self: *Self, kind: Error) Error {
    var buf: [1024]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&buf);
    const stderr = &stderr_writer.interface;
    defer stderr.flush() catch oom();

    const msg = switch (kind) {
        error.OutOfBound => "out of bounds access",
        error.ModuloWith0 => "modulo with 0",
        error.RangeIndexDecrease => "using a decreasing range for collection indexing",
        error.StackOverflow => "stack overflow",
        error.OutOfMemory => "no more memory available",
    };
    stderr.print("Runtime error: {s}\n", .{msg}) catch oom();

    // Ignore global scope, thus -1
    for (0..self.frame_stack.count - 1) |i| {
        const idx = self.frame_stack.count - i - 1;
        const frame = &self.frame_stack.frames[idx];
        const function = frame.function;
        // At this point, we point to next instruction so we take the previous one
        const instr = frame.instructionNb() - 1;

        stderr.print(
            "    [line {}] in {s}\n",
            .{ function.chunk.offsets.items[instr], function.name },
        ) catch oom();
    }

    return kind;
}

pub fn run(self: *Self, entry_point: *Obj.Function, modules: []Module) !void {
    self.modules = modules;
    self.gc.active = true;

    self.frame = try self.frame_stack.new();
    self.frame.call(entry_point, &self.stack, 0, modules);

    try self.execute();
}

pub fn runRepl(self: *Self, entry_point: *Obj.Function, modules: []Module) !void {
    self.modules = modules;
    self.gc.active = true;

    self.frame = try self.frame_stack.new();
    // Reset stack pointer to start of stack
    self.frame.slots = self.stack.values[0..].ptr;
    self.frame.module = &modules[0];
    self.frame.function = entry_point;
    self.frame.ip = entry_point.chunk.code.items.ptr;

    try self.execute();
}

fn execute(self: *Self) !void {
    while (true) {
        if (comptime options.print_stack) {
            // TODO: return an internal error?
            var buf: [1024]u8 = undefined;
            var stdout_writer = std.fs.File.stdout().writer(&buf);
            const stdout = &stdout_writer.interface;
            defer stdout.flush() catch oom();

            self.stack.print(stdout, self.frame) catch oom();
        }

        if (comptime options.print_instr) {
            var buf: [1024]u8 = undefined;
            var stdout_writer = std.fs.File.stdout().writer(&buf);
            const stdout = &stdout_writer.interface;
            defer stdout.flush() catch oom();

            var dis = Disassembler.init(&self.frame.function.chunk, self.frame.module, self.zig_fns);
            const instr_nb = self.frame.instructionNb();
            _ = dis.disInstruction(stdout, instr_nb);
        }

        const instruction = self.frame.readByte();
        var op: OpCode = @enumFromInt(instruction);
        var wide = false;

        if (op == .wide) {
            wide = true;
            op = @enumFromInt(self.frame.readByte());
        }

        switch (op) {
            .add_float => {
                const rhs = self.stack.pop().float;
                self.stack.peekRef(0).float += rhs;
            },
            .add_int => {
                const rhs = self.stack.pop().int;
                self.stack.peekRef(0).int += rhs;
            },
            .array_new => {
                const len = self.frame.readMaybeShort(wide);
                const type_id = self.frame.readShort();
                const array = Obj.Array.create(self, type_id, (self.stack.top - len)[0..len]);
                self.stack.top -= len;
                self.stack.push(Value.makeObj(array.asObj()));
            },
            .call_array => {
                const index = self.frame.readByte();
                const arity = self.frame.readByte();
                const array = self.stack.peekRef(arity).obj.as(Obj.Array);
                const result = array.funcs[index](array, self, (self.stack.top - arity)[0..arity]);

                self.stack.top -= arity + 1;
                if (result) |res| self.stack.push(res);
            },
            .call_string => {
                const index = self.frame.readByte();
                const arity = self.frame.readByte();
                const string = self.stack.peekRef(arity).obj.as(Obj.String);
                const result = string.funcs[index](string, self, (self.stack.top - arity)[0..arity]);

                self.stack.top -= arity + 1;
                if (result) |res| self.stack.push(res);
            },
            .array_set => {
                const index = self.stack.pop().int;
                const array = self.stack.pop().obj.as(Obj.Array);
                const value = self.stack.pop();

                const final = try self.normalizeIndex(array.values.items.len, index);
                array.values.items[final] = value;
            },
            .bound_method => {
                const sym_index = self.frame.readByte();

                const closure = Obj.Closure.create(
                    self,
                    self.frame.module.functions[sym_index],
                    (self.stack.top - 1)[0..1],
                );
                // Discard the function
                self.stack.top -= 1;
                self.stack.push(Value.makeObj(closure.asObj()));
            },
            .box => {
                const to_box = self.stack.pop();
                const boxed = Value.makeObj(Obj.Box.create(self, to_box).asObj());
                self.stack.push(boxed);
            },
            .call_any => {
                const args_count = self.frame.readByte();
                const callee = self.stack.peekRef(args_count).obj;

                switch (callee.kind) {
                    .native_zfn => {
                        const native = callee.as(Obj.ZigFn).function;
                        const result = native(self, (self.stack.top - args_count)[0..args_count]);

                        self.stack.top -= args_count + 1;
                        if (result) |res| self.stack.push(res);
                    },
                    else => {
                        @branchHint(.likely);
                        self.frame = try self.frame_stack.newKeepMod();
                        self.frame.runtimeCall(callee, &self.stack, args_count, self.modules);
                    },
                }
            },
            .call => {
                const index = self.frame.readByte();
                const arity = self.frame.readByte();
                self.frame = try self.frame_stack.newKeepMod();
                self.frame.call(self.frame.module.functions[index], &self.stack, arity, self.modules);
            },
            .call_ext => {
                const index = self.frame.readByte();
                const module = self.frame.readByte();
                const arity = self.frame.readByte();
                self.frame = try self.frame_stack.newKeepMod();
                self.frame.call(self.modules[module].functions[index], &self.stack, arity, self.modules);
            },
            .call_c => {
                const index = self.frame.readByte();
                const arity = self.frame.readByte();
                const base = self.stack.top - arity;
                const obj = self.c_fns[index];
                obj.function(@ptrCast(self));

                if (obj.returns) {
                    const res = self.stack.pop();
                    self.stack.top = base;
                    self.stack.push(res);
                } else {
                    self.stack.top = base;
                }
            },
            .call_zig => {
                const index = self.frame.readByte();
                const args_count = self.frame.readByte();
                const f = self.zig_fns[index].function;
                const result = f(self, (self.stack.top - args_count)[0..args_count]);

                self.stack.top -= args_count;
                if (result) |res| self.stack.push(res);
            },
            .closure => {
                const captures_count = self.frame.readByte();
                const closure = Obj.Closure.create(
                    self,
                    self.stack.peekRef(captures_count).obj.as(Obj.Function),
                    (self.stack.top - captures_count)[0..captures_count],
                );
                // Discard the function
                self.stack.top -= captures_count + 1;
                self.stack.push(Value.makeObj(closure.asObj()));
            },
            .def_global => {
                const idx = self.frame.readByte();
                self.frame.module.globals[idx] = self.stack.pop();
            },
            .div_float => {
                const rhs = self.stack.pop().float;
                self.stack.peekRef(0).float /= rhs;
            },
            .div_int => {
                const rhs = self.stack.pop().int;
                const lhs = self.stack.pop().int;
                self.stack.push(Value.makeInt(@divTrunc(lhs, rhs)));
            },
            .dup => self.stack.push(self.stack.peek(0)),
            .enum_lit => {
                const index = self.frame.readByte();
                const tag = self.frame.readByte();
                self.stack.push(.makeObj(Obj.EnumInstance.create(self, &self.frame.module.enums[index], tag, .null).asObj()));
            },
            .enum_lit_ext => {
                const index = self.frame.readByte();
                const module = self.frame.readByte();
                const tag = self.frame.readByte();
                self.stack.push(.makeObj(Obj.EnumInstance.create(self, &self.modules[module].enums[index], tag, .null).asObj()));
            },
            .eq_bool => self.stack.push(Value.makeBool(self.stack.pop().bool == self.stack.pop().bool)),
            .eq_float => self.stack.push(Value.makeBool(self.stack.pop().float == self.stack.pop().float)),
            .eq_int => self.stack.push(Value.makeBool(self.stack.pop().int == self.stack.pop().int)),
            .eq_null => self.stack.push(Value.makeBool(self.stack.pop() == .null)),
            .eq_str => self.stack.push(Value.makeBool(self.stack.pop().obj.as(Obj.String) == self.stack.pop().obj.as(Obj.String))),
            .exit_repl => {
                // Just deletes the current call self.frame
                self.frame_stack.count -= 1;
                break;
            },
            .fallback_err => {
                const fallback = self.stack.pop();
                const maybe_err = self.stack.peekRef(0);

                switch (maybe_err.*) {
                    .obj => |o| switch (o.kind) {
                        .@"error" => maybe_err.* = fallback,
                        else => {},
                    },
                    else => {},
                }
            },
            .fallback_opt => {
                const fallback = self.stack.pop();
                const maybe_opt = self.stack.peekRef(0);
                if (maybe_opt.* == .null) {
                    maybe_opt.* = fallback;
                }
            },
            .ge_float => self.stack.push(.makeBool(self.stack.pop().float <= self.stack.pop().float)),
            .ge_int => self.stack.push(.makeBool(self.stack.pop().int <= self.stack.pop().int)),
            .get_capt_frame => {
                // Get a capture in the current call self.frame, not on stack
                const index = self.frame.readByte();
                self.stack.push(self.frame.captures[index]);
            },
            .get_capt_local => {
                // Local captured are determined by their index relative to start of call self.frame
                const index = self.frame.readByte();
                self.stack.push((self.frame.slots + index)[0]);
            },
            .get_field => {
                const field_idx = self.frame.readByte();
                self.stack.peekRef(0).* = self.stack.peekRef(0).obj.as(Obj.Instance).fields[field_idx];
            },
            .get_field_cow => {
                const field_idx = self.frame.readByte();
                const field = &self.stack.peekRef(0).obj.as(Obj.Instance).fields[field_idx];
                field.obj = self.cow(field.obj);
                self.stack.peekRef(0).* = field.*;
            },
            .get_global => {
                const idx = self.frame.readByte();
                self.stack.push(self.frame.module.globals[idx]);
            },
            .get_global_cow => {
                const idx = self.frame.readByte();
                const value = &self.frame.module.globals[idx];
                value.obj = self.cow(value.obj);
                self.stack.push(value.*);
            },
            // TODO: see if same compiler bug as get_global
            .get_local => self.stack.push(self.frame.slots[self.frame.readByte()]),
            .get_local_cow => {
                const index = self.frame.readByte();
                const value = &self.frame.slots[index];
                value.obj = self.cow(value.obj);
                self.stack.push(value.*);
            },
            .get_tag => self.stack.peekRef(0).* = .makeInt(self.stack.peek(0).obj.as(Obj.EnumInstance).tag_id),
            .gt_float => self.stack.push(Value.makeBool(self.stack.pop().float < self.stack.pop().float)),
            .gt_int => self.stack.push(Value.makeBool(self.stack.pop().int < self.stack.pop().int)),
            .incr_ref => self.stack.peekRef(0).obj.ref_count += 1,
            .index_arr => {
                const index = self.stack.pop().int;
                const array = self.stack.pop().obj.as(Obj.Array);
                const final = try self.normalizeIndex(array.values.items.len, index);
                self.stack.push(array.values.items[final]);
            },
            .index_arr_cow => {
                const index = self.stack.pop().int;
                const array = self.stack.pop().obj.as(Obj.Array);
                const final = try self.normalizeIndex(array.values.items.len, index);

                var value = array.values.items[final];
                value.obj = self.cow(value.obj);
                self.stack.push(value);
            },
            .index_range_arr => {
                const index = self.stack.pop().range_int;
                const array = self.stack.pop().obj.as(Obj.Array);
                const start, const end = try self.checkRangeIndex(array.values.items.len, index);
                self.stack.push(.makeObj(Obj.Array.create(self, array.obj.type_id, array.values.items[start..end]).asObj()));
            },
            .index_str => {
                const index = self.stack.pop().int;
                const string = self.stack.pop().obj.as(Obj.String);
                const final = try self.normalizeIndex(string.chars.len, index);
                self.stack.push(.makeObj(Obj.String.takeCopy(self, &.{string.chars[final]}).asObj()));
            },
            .index_range_str => {
                const index = self.stack.pop().range_int;
                const string = self.stack.pop().obj.as(Obj.String);
                const start, const end = try self.checkRangeIndex(string.chars.len, index);
                self.stack.push(.makeObj(Obj.String.takeCopy(self, string.chars[start..end]).asObj()));
            },
            .in_array => {
                const array = self.stack.pop();
                const value = self.stack.pop();
                check: {
                    for (array.obj.as(Obj.Array).values.items) |v| {
                        if (value.eq(v)) {
                            self.stack.push(.true_);
                            break :check;
                        }
                    }
                    self.stack.push(.false_);
                }
            },
            .in_range_float => {
                const range = self.stack.pop().range_float;
                const value = self.stack.pop().float;
                self.stack.push(.makeBool(value >= range.start and value <= range.end));
            },
            .in_range_int => {
                const range = self.stack.pop().range_int;
                const value = self.stack.pop().int;
                self.stack.push(.makeBool(value >= range.start and value <= range.end));
            },
            .in_str => {
                const string = self.stack.pop().obj.as(Obj.String);
                const value = self.stack.pop().obj.as(Obj.String);
                self.stack.push(.makeBool(std.mem.containsAtLeast(u8, string.chars, 1, value.chars)));
            },
            .is_bool => self.stack.push(.makeBool(self.stack.pop() == .bool)),
            .is_float => self.stack.push(.makeBool(self.stack.pop() == .float)),
            .is_int => self.stack.push(.makeBool(self.stack.pop() == .int)),
            .is_str => {
                const top = self.stack.pop();
                self.stack.push(.makeBool(if (top.asObj()) |o| o.kind == .string else false));
            },
            .is_type => {
                const type_id = self.frame.readByte();
                const value = self.stack.pop();

                if (value == .obj) {
                    self.stack.push(.makeBool(value.obj.type_id == type_id));
                } else {
                    self.stack.push(.false_);
                }
            },
            .iter_new_arr => self.stack.peekRef(0).* = .makeObj(Obj.ArrIterator.create(self, self.stack.peek(0))),
            .iter_new_range => self.stack.peekRef(0).* = .makeObj(Obj.RangeIterator.create(self, self.stack.peek(0).range_int)),
            .iter_new_str => self.stack.peekRef(0).* = .makeObj(Obj.StrIterator.create(self, self.stack.peek(0).obj.as(Obj.String))),
            .iter_next => self.stack.push(self.stack.peekRef(0).obj.as(Obj.Iterator).next(self)),
            .iter_next_index => {
                const index, const value = self.stack.peekRef(0).obj.as(Obj.Iterator).nextWithIndex(self);
                self.stack.push(.makeInt(index));
                self.stack.push(value);
            },
            .jump => {
                const jump = self.frame.readShort();
                self.frame.ip += jump;
            },
            .jump_false => {
                const jump = self.frame.readShort();
                if (!self.stack.peek(0).bool) self.frame.ip += jump;
            },
            .jump_true => {
                const jump = self.frame.readShort();
                if (self.stack.peek(0).bool) self.frame.ip += jump;
            },
            .jump_no_err => {
                const jump = self.frame.readShort();
                err: {
                    if (self.stack.peek(0).asObj()) |obj| {
                        if (obj.kind == .@"error") {
                            break :err;
                        }
                    }
                    self.frame.ip += jump;
                }
            },
            .jump_null => {
                const jump = self.frame.readShort();
                if (self.stack.peek(0) == .null) self.frame.ip += jump;
            },
            .lt_float => self.stack.push(Value.makeBool(self.stack.pop().float > self.stack.pop().float)),
            .lt_int => self.stack.push(Value.makeBool(self.stack.pop().int > self.stack.pop().int)),
            .le_float => self.stack.push(Value.makeBool(self.stack.pop().float >= self.stack.pop().float)),
            .le_int => self.stack.push(Value.makeBool(self.stack.pop().int >= self.stack.pop().int)),
            .load_blk_val => self.stack.push(self.frame.blk_val),
            .load_constant => self.stack.push(self.frame.readConstant(wide)),
            .load_ext_constant => {
                const const_index = self.frame.readByte();
                const mod_index = self.frame.readByte();
                self.stack.push(self.modules[mod_index].constants[const_index]);
            },
            .load_fn_builtin => {
                const symbol_idx = self.frame.readByte();
                self.stack.push(.makeObj(self.zig_fns[symbol_idx].asObj()));
            },
            .load_fn => {
                const symbol_idx = self.frame.readByte();
                self.stack.push(.makeObj(self.frame.module.functions[symbol_idx].asObj()));
            },
            .load_fn_ext => {
                const symbol_index = self.frame.readByte();
                const module_index = self.frame.readByte();
                const module = self.modules[module_index];
                self.stack.push(.makeObj(module.functions[symbol_index].asObj()));
            },
            .loop => {
                const jump = self.frame.readShort();
                self.frame.ip -= jump;
            },
            // TODO: modulo errors
            .mod_float => {
                const rhs = self.stack.pop().float;
                const lhs = self.stack.pop().float;
                if (rhs == 0) {
                    return self.err(error.ModuloWith0);
                }
                self.stack.push(.makeFloat(@mod(lhs, rhs)));
            },
            .mod_int => {
                const rhs = self.stack.pop().int;
                const lhs = self.stack.pop().int;
                if (rhs == 0) {
                    return self.err(error.ModuloWith0);
                }
                self.stack.push(.makeInt(@mod(lhs, rhs)));
            },
            .mul_float => {
                const rhs = self.stack.pop().float;
                self.stack.peekRef(0).float *= rhs;
            },
            .mul_int => {
                const rhs = self.stack.pop().int;
                self.stack.peekRef(0).int *= rhs;
            },
            // PERF: no push/pop, only pointer manipulation (same for eq_)
            .ne_bool => self.stack.push(Value.makeBool(self.stack.pop().bool != self.stack.pop().bool)),
            .ne_int => self.stack.push(Value.makeBool(self.stack.pop().int != self.stack.pop().int)),
            .ne_float => self.stack.push(Value.makeBool(self.stack.pop().float != self.stack.pop().float)),
            .ne_null => self.stack.push(Value.makeBool(self.stack.pop() != .null)),
            .ne_null_push => self.stack.push(Value.makeBool(self.stack.peek(0) != .null)),
            .ne_str => self.stack.push(Value.makeBool(self.stack.pop().obj.as(Obj.String) != self.stack.pop().obj.as(Obj.String))),
            .neg_float => self.stack.peekRef(0).float *= -1,
            .neg_int => self.stack.peekRef(0).int *= -1,
            .not => self.stack.peekRef(0).not(),
            .pop => self.stack.top -= 1,
            .pop2 => self.stack.top -= 2,
            .pop3 => self.stack.top -= 3,
            .popn => {
                const count = self.frame.readByte();
                self.stack.top -= count;
            },
            .print => {
                var wa = std.io.Writer.Allocating.init(self.allocator);
                var writer = &wa.writer;
                self.stack.pop().print(writer);
                self.state.config.printFn(writer.buffered());
            },
            .push_false => self.stack.push(Value.false_),
            .push_null => self.stack.push(Value.null_),
            .push_true => self.stack.push(Value.true_),
            .range_new_float => self.stack.push(.makeRangeFloat(self.stack.pop().float, self.stack.pop().float)),
            .range_new_int => self.stack.push(.makeRangeInt(self.stack.pop().int, self.stack.pop().int)),
            .ret => {
                const result = self.stack.pop();
                self.frame_stack.count -= 1;

                // The last standing self.frame is the artificial one created when we run
                // the global scope at the very beginning
                // TODO: avoid logic at runtime, just emit a special OpCode for `main` return
                if (self.frame_stack.count == 1) {
                    _ = self.stack.pop();
                    break;
                }

                self.stack.top = self.frame.slots;
                self.stack.push(result);

                self.frame = &self.frame_stack.frames[self.frame_stack.count - 1];
            },
            .ret_naked => {
                self.frame_stack.count -= 1;

                // The last standing self.frame is the artificial one created when we run
                // the global scope at the very beginning
                // TODO: avoid logic at runtime, just emit a special OpCode for `main` naked return
                if (self.frame_stack.count == 1) {
                    _ = self.stack.pop();
                    break;
                }

                self.stack.top = self.frame.slots;
                self.frame = &self.frame_stack.frames[self.frame_stack.count - 1];
            },
            .set_field => {
                const field_idx = self.frame.readByte();
                const instance = self.stack.pop().obj.as(Obj.Instance);
                const value = self.stack.pop();
                instance.fields[field_idx] = value;
            },
            .set_global => {
                const idx = self.frame.readByte();
                self.frame.module.globals[idx] = self.stack.pop();
            },
            .set_local => self.frame.slots[self.frame.readByte()] = self.stack.pop(),
            .set_local_box => {
                const index = self.frame.readByte();
                self.frame.slots[index].obj.as(Obj.Box).value = self.stack.pop();
            },
            .store_blk_val => self.frame.blk_val = self.stack.pop(),
            .str_cat => self.strConcat(),
            .str_mul => self.strMul(self.stack.peekRef(0).obj.as(Obj.String), self.stack.peekRef(1).int),
            .struct_lit => {
                const index = self.frame.readByte();
                const arity = self.frame.readByte();
                const instance = Obj.Instance.create(self, &self.frame.module.structures[index]);
                structLit(instance, arity, &self.stack);
            },
            .struct_lit_ext => {
                const index = self.frame.readByte();
                const module = self.frame.readByte();
                const arity = self.frame.readByte();
                const instance = Obj.Instance.create(self, &self.modules[module].structures[index]);
                structLit(instance, arity, &self.stack);
            },
            .sub_float => {
                const rhs = self.stack.pop().float;
                self.stack.peekRef(0).float -= rhs;
            },
            .sub_int => {
                const rhs = self.stack.pop().int;
                self.stack.peekRef(0).int -= rhs;
            },
            .swap_pop => {
                self.stack.peekRef(1).* = self.stack.peek(0);
                self.stack.top -= 1;
            },
            .unbox => self.stack.peekRef(0).* = self.stack.peekRef(0).obj.as(Obj.Box).value,
            .union_lit => {
                const index = self.frame.readByte();
                const tag = self.frame.readByte();
                self.stack.push(.makeObj(Obj.UnionInstance.create(self, &self.frame.module.unions[index], tag, self.stack.pop()).asObj()));
            },
            .union_lit_ext => {
                const index = self.frame.readByte();
                const module = self.frame.readByte();
                const tag = self.frame.readByte();
                self.stack.push(.makeObj(Obj.UnionInstance.create(self, &self.modules[module].unions[index], tag, self.stack.pop()).asObj()));
            },
            .wide => unreachable,
        }
    }
}

fn structLit(instance: *Obj.Instance, arity: usize, stack: *Stack) void {
    for (0..arity) |i| {
        instance.fields[i] = stack.peek(arity - i - 1);
    }

    stack.top -= arity;
    stack.push(Value.makeObj(instance.asObj()));
}

/// Checks clone on write
/// TODO: move this to Obj
fn cow(self: *Self, obj: *Obj) *Obj {
    if (obj.ref_count > 0) {
        obj.ref_count -= 1;
        return obj.deepCopy(self);
    }

    return obj;
}

// PERF: check if a length is 0 and just return the string?
fn strConcat(self: *Self) void {
    const s2 = self.stack.peekRef(0).obj.as(Obj.String);
    const s1 = self.stack.peekRef(1).obj.as(Obj.String);

    const res = self.gc_alloc.alloc(u8, s1.chars.len + s2.chars.len) catch oom();
    @memcpy(res[0..s1.chars.len], s1.chars);
    @memcpy(res[s1.chars.len..], s2.chars);

    self.stack.peekRef(1).* = Value.makeObj(Obj.String.take(self, res).asObj());
    self.stack.top -= 1;
}

// PERF: check if factor is 0 or 1 and return empty or self string
fn strMul(self: *Self, str: *const Obj.String, factor: i64) void {
    // BUG: Check if factor is positive
    const f = @as(usize, @intCast(factor));
    const res = self.gc_alloc.alloc(u8, str.chars.len * f) catch oom();
    for (0..f) |i| {
        @memcpy(res[i * str.chars.len .. (i + 1) * str.chars.len], str.chars);
    }

    self.stack.peekRef(1).* = Value.makeObj(Obj.String.take(self, res).asObj());
    self.stack.top -= 1;
}

// // TODO: runtime error desactivable with release fast mode
fn normalizeIndex(self: *Self, len: usize, index: i64) Error!usize {
    if (index >= 0) {
        const i: usize = @intCast(index);
        if (i > len) {
            return self.err(error.OutOfBound);
        }
        return i;
    } else {
        const abs = @abs(index);
        if (abs > len) {
            return self.err(error.OutOfBound);
        }
        return len - @as(usize, @intCast(abs));
    }
}

// // TODO: runtime error desactivable with release fast mode
fn checkRangeIndex(self: *Self, len: usize, range: Value.RangeInt) Error!struct { usize, usize } {
    const s = try self.normalizeIndex(len, range.start);
    const e = try self.normalizeIndex(len, range.end);

    if (s > e) {
        return self.err(error.RangeIndexDecrease);
    }

    return .{ s, e };
}

// PERF: inline methods?
const Stack = struct {
    values: [STACK_SIZE]Value,
    top: [*]Value,

    const STACK_SIZE: u16 = @as(u16, FrameStack.FRAMES_MAX) * @as(u16, std.math.maxInt(u8));
    pub const empty: Stack = .{ .values = undefined, .top = undefined };

    pub fn init(self: *Stack) void {
        self.top = self.values[0..].ptr;
    }

    pub fn push(self: *Stack, value: Value) void {
        self.top[0] = value;
        self.top += 1;
    }

    pub fn pop(self: *Stack) Value {
        self.top -= 1;
        return self.top[0];
    }

    fn peek(self: *const Stack, distance: usize) Value {
        return (self.top - 1 - distance)[0];
    }

    pub fn peekRef(self: *Stack, distance: usize) *Value {
        return &(self.top - 1 - distance)[0];
    }

    pub fn print(self: *const Stack, writer: *Writer, frame: *CallFrame) Writer.Error!void {
        try writer.writeAll("          ");
        var value = self.values[0..].ptr;

        while (value != self.top) : (value += 1) {
            // Start of call frame
            if (value == frame.slots) try writer.writeAll(">");

            try writer.writeAll("[");
            value[0].print(writer);
            try writer.writeAll("] ");
        }
        try writer.writeAll("\n");
        try writer.flush();
    }
};

pub const CallFrame = struct {
    function: *Obj.Function,
    module: *Module,
    ip: [*]u8,
    slots: [*]Value,
    captures: []Value,
    blk_val: Value,

    pub fn instructionNb(self: *const CallFrame) usize {
        const addr1 = @intFromPtr(self.ip);
        const addr2 = @intFromPtr(self.function.chunk.code.items.ptr);
        return addr1 - addr2;
    }

    pub fn readByte(self: *CallFrame) u8 {
        defer self.ip += 1;
        return self.ip[0];
    }

    pub fn readConstant(self: *CallFrame, wide: bool) Value {
        // TODO: Compiler bug: https://github.com/ziglang/zig/issues/13938?
        const index = self.readMaybeShort(wide);
        return self.module.constants[index];
    }

    pub fn readShort(self: *CallFrame) u16 {
        const part1 = self.readByte();
        const part2 = self.readByte();

        return (@as(u16, part1) << 8) | part2;
    }

    pub fn readMaybeShort(self: *CallFrame, wide: bool) u16 {
        return if (wide) self.readShort() else self.readByte();
    }

    /// Sets the call to the provided function
    pub fn call(self: *CallFrame, func: *Obj.Function, stack: *Stack, args_count: usize, modules: []Module) void {
        self.slots = stack.top - args_count;
        self.module = &modules[func.module_index];
        self.function = func;
        self.ip = func.chunk.code.items.ptr;
    }

    /// Calls a function bounded to a runtime value. Checks what kind of function it is before calling
    pub fn runtimeCall(self: *CallFrame, callee: *Obj, stack: *Stack, args_count: usize, modules: []Module) void {
        // As it's a runtime value containing the function, it's on top of stack. We got one slot behind to override it
        self.slots = stack.top - args_count - 1;

        const function = switch (callee.kind) {
            .closure => b: {
                const closure = callee.as(Obj.Closure);
                const capt_len = closure.captures.len;

                // Moves arguments after captures that are in the first local slots of the call frame
                std.mem.copyBackwards(Value, self.slots[capt_len .. capt_len + args_count], self.slots[1 .. 1 + args_count]);
                // Copy the captures at the beginning of the call frame
                @memcpy(self.slots, closure.captures);
                // -1 because we moved the slots back 1 cell already
                if (capt_len > 0) stack.top += capt_len - 1;
                self.captures = closure.captures;
                break :b closure.function;
            },
            .function => b: {
                const function = callee.as(Obj.Function);
                self.module = &modules[function.module_index];
                // Moves all arguments one slot back to override the runtime variable containing the function
                // Not ideal regarding performance but allow the return address to be in the right place
                // It comes from the fact that runtime calls are handled the same way as comptime resolved
                // symbol calls and as they don't live on stack the return address is the first argument's slot
                std.mem.copyForwards(Value, self.slots[0..args_count], self.slots[1 .. 1 + args_count]);
                stack.top -= 1;
                break :b function;
            },
            else => unreachable,
        };

        self.function = function;
        self.ip = function.chunk.code.items.ptr;
    }
};

const FrameStack = struct {
    frames: [FRAMES_MAX]CallFrame,
    count: usize,

    const FRAMES_MAX: u8 = 64;

    pub const empty: FrameStack = .{ .frames = undefined, .count = 0 };

    pub fn new(self: *FrameStack) Error!*CallFrame {
        if (self.count == FRAMES_MAX) {
            return error.StackOverflow;
        }

        const new_frame = &self.frames[self.count];
        self.count += 1;

        return new_frame;
    }

    /// Opens a new frame while keeping the same module at the one before
    /// Assumes that there is one before
    pub fn newKeepMod(self: *FrameStack) Error!*CallFrame {
        if (self.count == FRAMES_MAX) {
            return error.StackOverflow;
        }

        const new_frame = &self.frames[self.count];
        new_frame.module = self.frames[self.count - 1].module;
        self.count += 1;

        return new_frame;
    }
};
