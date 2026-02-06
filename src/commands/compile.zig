const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const clarg = @import("clarg");
const Arg = clarg.Arg;
const misc = @import("misc");
const oom = misc.oom;
const RevIterator = misc.RevIterator;
const ConstIdx = @import("../core/analyzer/ConstantInterner.zig").ConstIdx;

const ir = @import("../core/analyzer/ir.zig");
const Instr = ir.Instruction;
const IrBuilder = @import("../core/analyzer/IrBuilder.zig");
const Scope = @import("../core/analyzer/LexicalScope.zig").Scope;
const Variable = @import("../core/analyzer/LexicalScope.zig").Variable;
const Symbol = @import("../core/analyzer/LexicalScope.zig").Symbol;
const Ast = @import("../core/parser/Ast.zig");
const Pipeline = @import("../core/pipeline/pipeline.zig");
const State = @import("../core/pipeline/State.zig");

allocator: Allocator,
indent_level: usize,
writer: *std.Io.Writer,
ast: Ast,
irb: IrBuilder,
lex_scope: LexScope,
constants: ArrayList(usize),

const LexScope = struct {
    scopes: []const Scope,
    stack: std.ArrayList(Scope),
    symbols: []const Symbol,
    index: usize,

    pub fn init(scopes: []const Scope, symbols: []const Symbol) LexScope {
        return .{ .scopes = scopes, .stack = .empty, .index = 0, .symbols = symbols };
    }

    pub fn deinit(self: *LexScope, allocator: Allocator) void {
        self.stack.deinit(allocator);
    }

    pub fn open(self: *LexScope, allocator: Allocator) void {
        self.stack.append(allocator, self.scopes[self.index]) catch oom();
        self.index += 1;
    }

    pub fn close(self: *LexScope) void {
        _ = self.stack.pop().?;
    }

    /// Tries to retreive a variable from scopes and the local offset of its scope
    pub fn getVariable(self: *const LexScope, kind: enum { local, global }, index: usize) *const Variable {
        if (kind == .global) {
            return &self.stack.items[0].variables.values()[index];
        }

        var it = self.iterator();
        var offset: usize = 0;

        while (it.next()) |scope| {
            if (index > offset + scope.variables.count()) {
                offset += scope.offset;
                continue;
            }

            const local_index = index - offset;
            return &scope.variables.values()[local_index];
        }

        unreachable;
    }

    pub fn getSymbol(self: *const LexScope, index: usize) Symbol {
        return self.symbols[index];
    }

    fn iterator(self: *const LexScope) RevIterator(Scope) {
        return .init(self.stack.items);
    }
};

const Self = @This();
const Error = std.ArrayList(u8).Writer.Error;
const spaces: []const u8 = " " ** 1024;
const INDENT_SIZE = 4;

pub const Args = struct {
    file: Arg(.string) = .{ .desc = "Path to the file to compile", .positional = true },
    out: Arg("out.e") = .{ .desc = "Output's name", .short = 'o' },
    help: Arg(bool) = .{ .desc = "Prints this help and exit", .short = 'h' },

    pub const description: []const u8 = "Compiles to a native executable";
};

pub fn run(allocator: Allocator, args: clarg.ParsedArgs(Args)) !void {
    var transpiler: Self = .{
        .allocator = allocator,
        .writer = undefined,
        .ast = undefined,
        .irb = undefined,
        .lex_scope = undefined,
        .constants = .empty,
        .indent_level = 0,
    };
    try transpiler.runPipeline(args);
}

pub fn runPipeline(self: *Self, args: clarg.ParsedArgs(Args)) !void {
    if (args.help) return clarg.helpToFile(Args, .stderr());

    const file_path = args.file orelse {
        std.debug.print("Error: Expected a file to compile.\n", .{});
        return;
    };

    var arena = std.heap.ArenaAllocator.init(self.allocator);
    const arena_alloc = arena.allocator();
    defer arena.deinit();

    var state: State = .new(arena_alloc, .{}, &.{});

    const file_content = std.fs.cwd().readFileAllocOptions(self.allocator, file_path, 100_000, null, .of(u8), 0) catch |err| {
        // TODO: Ray error
        std.debug.print("Error: {}, unable to open file at: {s}\n", .{ err, file_path });
        std.process.exit(0);
    };
    defer self.allocator.free(file_content);

    const scopes, const symbols, const irb = Pipeline.runFrontend(arena_alloc, &state, false, file_path, file_content) catch |e| switch (e) {
        error.ExitOnPrint => return,
        else => return e,
    };

    self.irb = irb;
    self.lex_scope = .init(scopes, symbols);

    var tmp = std.Io.Writer.Allocating.init(self.allocator);
    self.writer = &tmp.writer;

    const out_file = "test_compiled.zig";
    try self.compile(&state.interner, out_file);
    std.log.debug("Transpiled:\n{s}", .{self.writer.buffered()});

    const process = try std.process.Child.run(.{
        .allocator = self.allocator,
        .argv = &.{
            "zig",
            "build-exe",
            out_file,
        },
    });
    defer self.allocator.free(process.stdout);
    defer self.allocator.free(process.stderr);

    tmp.deinit();
    self.lex_scope.deinit(self.allocator);
    self.constants.deinit(self.allocator);
}

fn compile(self: *Self, interner: *const misc.Interner, output: []const u8) !void {
    // Global scope
    self.lex_scope.open(self.allocator);

    self.appendSlice("const std = @import(\"std\");\n", .all);

    for (self.irb.roots.items) |instr| {
        try self.transpileInstr(instr, interner);
        self.appendSlice("\n", .none);
    }

    const file = try std.fs.cwd().createFile(output, .{});
    defer file.close();

    try file.writeAll(self.writer.buffered());
}

fn transpileInstr(self: *Self, instr: usize, interner: *const misc.Interner) !void {
    switch (self.irb.instructions.items(.data)[instr]) {
        .block => |n| {
            // std.log.debug("Block: {any}", .{n});
            self.appendSlice("{", .all);
            {
                self.openScope();
                defer self.closeScope();

                for (n.instrs) |block_instr| {
                    try self.transpileInstr(block_instr, interner);
                }
            }
            self.appendSlice("}", .all);
        },
        .binop => |n| {
            // std.log.debug("Binop: {any}", .{n});
            try self.transpileInstr(n.lhs, interner);
            self.printSlice(
                " {s} ",
                .{switch (n.op) {
                    .add_int, .add_float => "+",
                    else => unreachable,
                }},
                .none,
            );
            try self.transpileInstr(n.rhs, interner);
        },
        .call => |n| {
            std.log.debug("Call: {any}", .{n});

            self.indent();
            try self.transpileInstr(n.callee, interner);
            if (n.args.len > 0) {
                self.appendSlice("(", .none);
                for (n.args, 0..) |arg, i| {
                    try self.transpileInstr(arg.instr, interner);
                    if (i < n.args.len - 1) self.appendSlice(", ", .none);
                }
                self.appendSlice(");", .{ .indent = false });
            } else {
                self.appendSlice("();", .{ .indent = false });
            }
        },
        .identifier => |n| {
            const variable = self.lex_scope.getVariable(if (n.scope == .global) .global else .local, n.index);
            self.appendSlice(interner.getKey(variable.name).?, .none);
        },
        .load_symbol => |n| {
            const symbol = self.lex_scope.getSymbol(n.symbol_index);
            const sym_name = interner.getKey(symbol.name).?;
            self.appendSlice(sym_name, .none);
        },
        .field => |n| {
            std.log.debug("Field: {any}", .{n});
            try self.transpileInstr(n.structure, interner);

            const variable = switch (self.irb.instructions.items(.data)[n.structure]) {
                .identifier => |ident| self.lex_scope.getVariable(if (ident.scope == .global) .global else .local, ident.index),
                else => |e| {
                    std.log.debug("Got: {any}", .{e});
                    @panic("Not yet supported");
                },
            };
            const struct_type = variable.type.structure;

            const name = switch (n.kind) {
                .field => interner.getKey(struct_type.fields.keys()[n.index]).?,
                .function => interner.getKey(self.lex_scope.getSymbol(n.index).name).?,
            };

            self.printSlice(".{s}", .{name}, .none);
        },
        .fn_decl => |n| {
            // std.log.debug("FnDecl: {any}", .{n});
            try self.transpilefn(n, interner);
        },
        .print => |n| {
            // std.log.debug("Print: {any}", .{n});
            self.appendSlice("std.debug.print(\"{any}\\n\", .{", .{ .new_line = false });
            try self.transpileInstr(n, interner);
            self.appendSlice("});", .{ .indent = false });
        },
        .struct_decl => |n| {
            std.log.debug("Struct decl: {any}", .{n});

            const sym = self.lex_scope.getSymbol(n.sym_index);
            const struct_type = sym.type.structure;
            const struct_name = interner.getKey(n.name).?;

            if (struct_type.fields.count() == 0 and struct_type.functions.count() == 0) {
                self.printSlice("pub const {s} = struct {{}};", .{struct_name}, .all);
                self.openScope();
                defer self.closeScope();
            } else {
                self.printSlice("pub const {s} = struct {{", .{struct_name}, .all);
                self.openScope();

                std.log.debug("Syms: {any}", .{self.lex_scope.stack.getLast().symbols.values()});

                // TODO: maybe handle defaults better, see when doing the same for functions
                var def_count: usize = 0;

                for (struct_type.fields.keys(), struct_type.fields.values()) |name, field| {
                    if (field.default) |_| {
                        self.printSlice("{s}: {s} = ", .{ interner.getKey(name).?, getType(field.type, interner) }, .{ .new_line = false });
                        try self.transpileInstr(n.default_fields[def_count], interner);
                        def_count += 1;
                        self.appendSlice(",", .{ .indent = false });
                    } else {
                        self.printSlice("{s}: {s}", .{ interner.getKey(name).?, getType(field.type, interner) }, .all);
                    }
                }
                std.log.debug("Syms: {any}", .{self.lex_scope.stack.getLast().symbols.values()});

                for (n.functions) |func| {
                    try self.transpilefn(self.irb.instructions.items(.data)[func].fn_decl, interner);
                }

                self.closeScope();
                self.appendSlice("};", .all);
            }
        },
        .struct_literal => |n| {
            std.log.debug("Struct lit: {any}", .{n});

            try self.transpileInstr(n.structure, interner);

            const sym = switch (self.irb.instructions.items(.data)[n.structure]) {
                .load_symbol => |sym| self.lex_scope.getSymbol(sym.symbol_index),
                else => unreachable,
            };
            const struct_type = sym.type.structure;

            if (n.values.len == 0) {
                self.appendSlice("{{}};", .{ .indent = false });
            } else {
                self.appendSlice("{", .{ .indent = false });
                self.indent_level += 1;

                for (n.values, 0..) |value, i| {
                    self.indent();
                    self.printSlice(".{s} = ", .{interner.getKey(struct_type.fields.keys()[i]).?}, .none);
                    switch (value) {
                        .instr => |val_instr| try self.transpileInstr(val_instr, interner),
                        .default => |def_val| try self.transpileConstant(def_val.const_index, interner),
                    }
                    self.appendSlice(",", .{ .indent = false });
                }

                self.indent_level -= 1;
                self.appendSlice("}", .{ .new_line = false });
            }
        },
        .var_decl => |n| {
            // std.log.debug("VarDecl: {any}", .{n});

            const variable = self.lex_scope.getVariable(
                if (n.variable.scope == .global) .global else .local,
                n.variable.index,
            );
            const var_name = interner.getKey(variable.name).?;

            self.printSlice(
                "{s} {s}: {s} = ",
                .{
                    if (variable.constant) "const" else "var",
                    var_name,
                    getType(variable.type, interner),
                },
                .{ .new_line = false },
            );

            if (n.value) |val| {
                try self.transpileInstr(val, interner);
            }

            self.appendSlice(";", .{ .indent = false });

            // Don't do it in global scopeù
            if (!variable.used and self.lex_scope.stack.items.len > 1) {
                self.printSlice("_ = &{s};", .{var_name}, .all);
            }
        },
        .constant => |n| {
            try self.transpileConstant(n.index, interner);
        },
        else => |n| {
            std.log.debug("Got: {any}", .{n});
            unreachable;
        },
    }
}

fn transpileConstant(self: *Self, const_index: ConstIdx, interner: *const misc.Interner) anyerror!void {
    try self.transpileInstr(self.constants.items[const_index.toInt()], interner);
    switch (const_index) {
        .true => self.appendSlice("true", .none),
        .false => self.appendSlice("false", .none),
        .null => self.appendSlice("null", .none),
        else => {
            const idx = const_index.toInt();
            try self.transpileInstr(self.constants.items[if (idx > 2) idx - 3 else idx], interner);
        },
    }
}

fn transpilefn(self: *Self, instr_data: ir.Instruction.FnDecl, interner: *const misc.Interner) anyerror!void {
    const fn_name = instr_data.name orelse @panic("Anonymus function not yet supported");
    const sym = self.lex_scope.getSymbol(instr_data.sym_index);

    self.printSlice("pub fn {s}(", .{interner.getKey(fn_name).?}, .{ .new_line = false });

    const fn_type = sym.type.function;

    for (fn_type.params.values(), 0..) |p, i| {
        std.log.debug("Param: {any}", .{p});
        const p_name = p.name orelse @panic("Anonymus param not yet supported");
        const p_text = interner.getKey(p_name).?;

        if (std.mem.eql(u8, p_text, "self")) {
            self.printSlice("{s}: *{s}", .{ p_text, getType(p.type, interner) }, .none);
        } else {
            self.printSlice("{s}: {s}", .{ p_text, getType(p.type, interner) }, .none);
        }

        if (i < fn_type.params.count() - 1) self.appendSlice(", ", .none);
    }

    self.printSlice(
        ") {s} {{",
        .{getType(fn_type.return_type, interner)},
        .{ .indent = false, .new_line = instr_data.body.len > 0 },
    );

    {
        self.openScope();
        defer self.closeScope();

        // Unused params
        for (0..fn_type.params.count()) |i| {
            const variable = self.lex_scope.getVariable(.local, i);
            const var_name = interner.getKey(variable.name).?;

            if (!variable.used) {
                self.printSlice("_ = &{s};", .{var_name}, .all);
            }
        }

        for (instr_data.body) |body_instr| {
            try self.transpileInstr(body_instr, interner);
        }
    }

    self.appendSlice("}", .all);
}

fn openScope(self: *Self) void {
    self.lex_scope.open(self.allocator);
    self.indent_level += 1;
}

fn closeScope(self: *Self) void {
    self.lex_scope.close();
    self.indent_level -= 1;
}

fn getType(ty: *const @import("../core/analyzer/types.zig").Type, interner: *const misc.Interner) []const u8 {
    return switch (ty.*) {
        .int => "i64",
        .float => "f64",
        .bool => "bool",
        .str => "[]const u8",
        .void => "void",
        .structure => |s| interner.getKey(s.loc.?.name).?,
        else => @panic("Type not supported yet"),
    };
}

fn indent(self: *Self) void {
    std.debug.assert(self.indent_level * 2 < 1024);
    self.writer.writeAll(spaces[0 .. self.indent_level * INDENT_SIZE]) catch oom();
}

const PrintConfig = struct {
    indent: bool = true,
    new_line: bool = true,

    pub const all: PrintConfig = .{ .indent = true, .new_line = true };
    pub const none: PrintConfig = .{ .indent = false, .new_line = false };
};

fn appendSlice(self: *Self, text: []const u8, config: PrintConfig) void {
    if (config.indent) self.indent();
    self.writer.writeAll(text) catch oom();
    if (config.new_line) self.writer.writeAll("\n") catch oom();
}

fn printSlice(self: *Self, comptime fmt: []const u8, args: anytype, config: PrintConfig) void {
    if (config.indent) self.indent();
    self.writer.print(fmt, args) catch oom();
    if (config.new_line) self.writer.writeAll("\n") catch oom();
}
