const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const MultiArrayList = std.MultiArrayList;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;
const FieldEnum = std.meta.FieldEnum;

const Pipeline = @import("../pipeline/Pipeline.zig");
const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;
const Ast = @import("../parser/Ast.zig");
const Node = Ast.Node;
const Expr = Ast.Expr;
const Importer = @import("Importer.zig");
const IrBuilder = @import("IrBuilder.zig");
const LexScope = @import("LexicalScope.zig");
const ir = @import("ir.zig");
const InstrIndex = ir.Index;
const Instr = ir.Instruction;
const Span = @import("../parser/Lexer.zig").Span;
const TokenTag = @import("../parser/Lexer.zig").Token.Tag;
const ConstIdx = @import("ConstantInterner.zig").ConstIdx;

const type_mod = @import("types.zig");
const Type = type_mod.Type;
const TypeInterner = type_mod.TypeInterner;
const TypeIds = type_mod.TypeIds;

const Matcher = @import("Matcher.zig");
const matchers = @import("matchers.zig");

const misc = @import("misc");
const Interner = misc.Interner;
const InternerIdx = Interner.Index;
const GenReport = misc.reporter.GenReport;
const Sb = misc.StringBuilder;
const Set = misc.Set;
const oom = misc.oom;

pub const AnalyzedModule = struct {
    name: []const u8,
    globals: LexScope.VariableMap,
    symbols: LexScope.SymbolArrMap,
};

pub const Context = struct {
    decl_type: ?*const Type,
    fn_type: ?*const Type,
    self_type: ?*const Type,
    in_call: bool,
    in_for: bool,

    pub const empty: Context = .{
        .decl_type = null,
        .fn_type = null,
        .self_type = null,
        .in_call = false,
        .in_for = false,
    };

    const ContextSnapshot = struct {
        saved: Context,
        ctx: *Context,

        pub fn restore(self: ContextSnapshot) void {
            self.ctx.* = self.saved;
        }
    };

    pub fn setAndGetPrevious(self: *Context, comptime f: FieldEnum(Context), value: @FieldType(Context, @tagName(f))) @TypeOf(value) {
        const prev = @field(self, @tagName(f));
        @field(self, @tagName(f)) = value;

        return prev;
    }

    pub fn snapshot(self: *Context) ContextSnapshot {
        return .{ .saved = self.*, .ctx = self };
    }

    pub fn reset(self: *Context) void {
        self.* = .empty;
    }
};

const Self = @This();
pub const Error = error{ Err, NotSymbol };
const TypeResult = Error!struct { type: *const Type, instr: InstrIndex };
const StmtResult = Error!InstrIndex;

const TypeInfos = struct {
    heap: bool = false,
    is_sym: bool = false,
    comp_time: bool = false,
    /// External module index where the symbol comes from
    ext_mod: ?usize = null,
};
pub const InstrInfos = struct {
    type: *const Type,
    ti: TypeInfos = .{},
    cf: ControlFlow = .none,
    instr: InstrIndex,

    pub const ControlFlow = enum {
        @"break",
        @"return",
        none,

        pub fn exitScope(self: ControlFlow) bool {
            return self == .@"return" or self == .@"break";
        }
    };
};

const Result = Error!InstrInfos;
pub const AnalyzerReport = GenReport(AnalyzerMsg);

allocator: Allocator,
pipeline: *Pipeline,
interner: *Interner,
path: *Sb,
containers: Sb,

errs: ArrayList(AnalyzerReport),
warns: ArrayList(AnalyzerReport),
ast: *const Ast,
scope: LexScope,
ti: *TypeInterner,
irb: IrBuilder,
main: ?usize,

mod_name: InternerIdx,
mod_index: usize,
cached_names: struct { empty: usize, main: usize, std: usize, self: usize, Self: usize, init: usize },

pub fn init(allocator: Allocator, pipeline: *Pipeline, save_dbg_infos: bool) Self {
    var scope: LexScope = .empty;
    scope.save = save_dbg_infos;
    scope.initGlobalScope(allocator, pipeline.state);

    return .{
        .allocator = allocator,
        .pipeline = pipeline,
        .interner = &pipeline.state.interner,
        .path = &pipeline.state.path_builder,
        .containers = .empty,
        .ti = &pipeline.state.type_interner,
        .ast = undefined,
        .errs = .empty,
        .warns = .empty,
        .scope = scope,
        .irb = .init(allocator),
        .main = null,

        .mod_name = undefined,
        .mod_index = pipeline.state.module_interner.analyzed.count(),
        .cached_names = .{
            .empty = pipeline.state.interner.intern(""),
            .main = pipeline.state.interner.intern("main"),
            .std = pipeline.state.interner.intern("std"),
            .self = pipeline.state.interner.intern("self"),
            .Self = pipeline.state.interner.intern("Self"),
            .init = pipeline.state.interner.intern("init"),
        },
    };
}

pub fn err(self: *Self, kind: AnalyzerMsg, span: Span) Error {
    self.errs.append(self.allocator, AnalyzerReport.err(kind, span.start, span.end)) catch oom();
    return error.Err;
}

pub fn warn(self: *Self, kind: AnalyzerMsg, span: Span) void {
    self.warns.append(self.allocator, AnalyzerReport.warn(kind, span.start, span.end)) catch oom();
}

fn replacePrevErr(self: *Self, new_err: AnalyzerMsg, span: Span) Error {
    const last = &self.errs.items[self.errs.items.len - 1];
    last.* = AnalyzerReport.err(new_err, span.start, span.end);

    return error.Err;
}

pub fn analyze(self: *Self, ast: *const Ast, module_name: []const u8, expect_main: bool) AnalyzedModule {
    self.ast = ast;
    var ctx: Context = .empty;

    const mod_no_ext = module_name[0 .. module_name.len - 3];
    self.mod_name = self.interner.intern(mod_no_ext);
    self.containers.append(self.allocator, mod_no_ext);

    for (ast.nodes) |*node| {
        const res = self.analyzeNode(node, .none, &ctx) catch continue;
        self.irb.addRootInstr(res.instr);
    }

    if (expect_main and self.main == null) {
        self.err(.no_main, .{ .start = 0, .end = 0 }) catch {};
    }

    self.scope.closeGlobalScope();

    return .{
        .name = module_name,
        .globals = self.scope.current.variables,
        .symbols = self.scope.current.symbols,
    };
}

pub fn analyzeNode(self: *Self, node: *const Node, expect: ExprResKind, ctx: *Context) Result {
    const instr = switch (node.*) {
        .assignment => |*n| try self.assignment(n, ctx),
        .@"continue" => |n| try self.continueStmt(n, ctx),
        .discard => |n| try self.discard(n, ctx),
        .enum_decl => |*n| try self.enumDeclaration(n, ctx),
        .for_loop => |*n| try self.forLoop(n, ctx),
        .fn_decl => |*n| (try self.fnDeclaration(n, ctx)).instr,
        .multi_var_decl => |*n| try self.multiVarDecl(n, ctx),
        .print => |n| try self.print(n, ctx),
        .struct_decl => |*n| try self.structDecl(n, ctx),
        .use => |*n| b: {
            try self.use(n);
            // TODO: replace with a error.Noop to skip it?
            break :b self.irb.addInstr(.noop, 0);
        },
        .var_decl => |*n| try self.varDeclaration(n, ctx),
        .@"while" => |*n| try self.whileStmt(n, ctx),
        .expr => |n| return try self.analyzeExpr(n, expect, ctx),
    };

    return .{ .type = self.ti.getCached(.void), .instr = instr };
}

fn assignment(self: *Self, node: *const Ast.Assignment, ctx: *Context) StmtResult {
    const span = self.ast.getSpan(node.assigne);

    const maybe_assigne: ?InstrInfos = switch (node.assigne.*) {
        .identifier => |e| b: {
            var assigne = try self.expectVariableIdentifier(e);
            if (assigne.variable.constant) return self.err(.{ .assign_to_constant = .{ .name = self.ast.toSource(e) } }, span);

            assigne.variable.initialized = true;
            break :b .{ .type = assigne.variable.type, .instr = assigne.instr };
        },
        .field => |*e| b: {
            const field_result = try self.field(e, ctx);
            if (field_result.ti.is_sym) return self.err(.assign_to_struct_fn, span);

            // Resolving methods without call result in a bound method
            if (field_result.type.* == .function and field_result.type.function.kind == .bound) {
                return self.err(.assign_to_struct_fn, span);
            }
            break :b .{ .type = field_result.type, .instr = field_result.instr };
        },
        .fn_call => return self.err(.invalid_assign_target, span),
        .indexing => |*expr| b: {
            const res = try self.indexing(expr, ctx);

            if (res.type.* == .str) {
                return self.err(.index_assign_str, self.ast.getSpan(expr.expr));
            }
            break :b res;
        },
        else => return self.err(.invalid_assign_target, span),
    };

    const assigne = maybe_assigne orelse return self.err(.invalid_assign_target, span);

    ctx.decl_type = assigne.type;

    var value_res = try self.analyzeExpr(node.value, .value, ctx);

    _ = try self.performTypeCoercion(assigne.type, value_res.type, false, self.ast.getSpan(node.value));

    self.checkWrap(&value_res.instr, value_res.ti.heap);

    return self.irb.addInstr(
        .{ .assignment = .{ .assigne = assigne.instr, .value = value_res.instr, .cow = assigne.type.isHeap() } },
        span.start,
    );
}

fn continueStmt(self: *Self, node: Ast.Continue, ctx: *const Context) StmtResult {
    const span = self.ast.getSpan(node);

    const scope, const depth = self.scope.getScopeContinuable(self.internLabel(node.label)) catch |e| {
        return switch (e) {
            error.CantContinue => self.err(
                .{ .cant_continue_scope = .{ .name = self.ast.toSource(node.label.?) } },
                self.ast.getSpan(node.label.?),
            ),
            error.NoContinueScope => self.err(.no_continuable_scope, span),
            error.UnknownLabel => self.err(
                .{ .undeclared_block_label = .{ .name = self.ast.toSource(node.label.?) } },
                self.ast.getSpan(node.label.?),
            ),
        };
    };

    // If we are in a `for` loop, we don't discard the added iterator
    return self.irb.addInstr(
        .{ .@"continue" = .{
            .depth = depth,
            .pop_count = self.scope.stackDiffWithCurrent(scope) - @intFromBool(ctx.in_for),
        } },
        span.start,
    );
}

fn discard(self: *Self, expr: *const Expr, ctx: *Context) StmtResult {
    const res = try self.analyzeExpr(expr, .value, ctx);
    return self.irb.wrapInstr(.discard, res.instr);
}

fn enumDeclaration(self: *Self, node: *const Ast.EnumDecl, ctx: *Context) StmtResult {
    const snapshot = ctx.snapshot();
    defer snapshot.restore();

    var buf: [1024]u8 = undefined;
    const container_name = self.interner.internKeepRef(self.allocator, self.containers.renderWithSep(&buf, "."));

    // TODO: anonymus enums
    const name_tk = node.name orelse @panic("anonymus enums aren't supported yet");
    const name = try self.internIfNotInCurrentScope(name_tk);

    const interned = self.ti.newEnum(.{ .name = name, .container = container_name }, node.is_err);
    const ty = &interned.@"enum";

    const sym = self.scope.forwardDeclareSymbol(self.allocator, name);
    sym.type = interned;

    ctx.self_type = interned;
    defer ctx.self_type = null;

    try self.openContainer(name_tk);
    defer self.closeContainer();

    try self.enumTags(node.tags, ty, ctx);
    const funcs = try self.containerFnDecls(node.functions, &ty.functions, ctx);

    return self.irb.addInstr(
        .{ .enum_decl = .{
            .name = name,
            .sym_index = sym.index,
            .functions = funcs,
            .is_err = node.is_err,
        } },
        self.ast.getSpan(node).start,
    );
}

fn enumTags(self: *Self, tags: []const Ast.EnumDecl.Tag, ty: *Type.Enum, ctx: *Context) Error!void {
    ty.tags.ensureTotalCapacity(self.allocator, @intCast(tags.len)) catch oom();

    for (tags) |tag| {
        const tag_res = try self.enumTag(tag, ctx);
        const gop = ty.tags.getOrPut(self.allocator, tag_res.name) catch oom();

        if (gop.found_existing) {
            return self.err(
                .{ .enum_dup_tag = .{ .name = self.ast.toSource(tag.name) } },
                self.ast.getSpan(tag.name),
            );
        } else {
            gop.value_ptr.* = tag_res.ty;
        }
    }
}

fn enumTag(self: *Self, tag: Ast.EnumDecl.Tag, ctx: *const Context) Error!struct { name: InternerIdx, ty: *const Type } {
    const name = self.interner.intern(self.ast.toSource(tag.name));
    const ty = if (tag.payload) |ty| try self.checkAndGetType(ty, ctx) else self.ti.cache.void;
    return .{ .name = name, .ty = ty };
}

/// Analyzes function declarations in a container (enum or structure)
fn containerFnDecls(
    self: *Self,
    decls: []const Ast.FnDecl,
    funcs: *AutoArrayHashMapUnmanaged(InternerIdx, LexScope.Symbol),
    ctx: *Context,
) Error![]const InstrIndex {
    funcs.ensureTotalCapacity(self.allocator, @intCast(decls.len)) catch oom();

    var func_instrs: ArrayList(InstrIndex) = .empty;
    func_instrs.ensureTotalCapacity(self.allocator, decls.len) catch oom();

    for (decls) |*f| {
        const fn_name = self.interner.intern(self.ast.toSource(f.name));
        const fn_res = try self.fnDeclaration(f, ctx);
        func_instrs.appendAssumeCapacity(fn_res.instr);
        funcs.putAssumeCapacity(fn_name, fn_res.sym);
    }

    return func_instrs.toOwnedSlice(self.allocator) catch oom();
}

fn forLoop(self: *Self, node: *const Ast.For, ctx: *Context) StmtResult {
    const binding = try self.internIfNotInCurrentScope(node.binding);
    const index_interned = if (node.index_binding) |index| try self.internIfNotInCurrentScope(index) else null;

    const res = try self.analyzeExpr(node.expr, .value, ctx);

    // Virtual scope to declare the iterator
    _ = try self.forwardDeclareVariable(0, undefined, false, undefined);

    const kind: Instr.For.Kind, const elem_type = switch (res.type.*) {
        .array => |t| .{ .array, t.child },
        .range => |r| b: {
            if (r != self.ti.getCached(.int)) {
                return self.err(.for_iter_non_int_range, self.ast.getSpan(node.expr));
            }

            break :b .{ .range, self.ti.getCached(.int) };
        },
        .str => .{ .str, res.type },
        else => |*t| return self.err(.{ .iter_non_iterable = .{ .found = self.typeName(t) } }, self.ast.getSpan(node.expr)),
    };

    if (index_interned) |interned| {
        try self.forwardDeclareVariable(interned, self.ti.getCached(.int), false, self.ast.getSpan(node.index_binding.?));
    }

    try self.forwardDeclareVariable(binding, elem_type, false, self.ast.getSpan(node.binding));

    const prev_in_for = ctx.setAndGetPrevious(.in_for, true);
    defer ctx.in_for = prev_in_for;
    const body_res = try self.block(&node.body, 1, .{ .can_continue = true }, ctx);

    return self.irb.addInstr(
        .{ .for_loop = .{
            .expr = res.instr,
            .body = body_res.instr,
            .kind = kind,
            .use_index = node.index_binding != null,
        } },
        self.ast.getSpan(node).start,
    );
}

const FnDeclRes = struct { instr: usize, sym: LexScope.Symbol };

fn fnDeclaration(self: *Self, node: *const Ast.FnDecl, ctx: *Context) Error!FnDeclRes {
    const snapshot = ctx.snapshot();
    defer snapshot.restore();

    const span = self.ast.getSpan(node);
    const name = try self.internIfNotInCurrentScope(node.name);

    var buf: [1024]u8 = undefined;
    const container_name = self.interner.internKeepRef(self.allocator, self.containers.renderWithSep(&buf, "."));

    // Forward declaration in outer scope for recursion
    var sym = self.scope.forwardDeclareSymbol(self.allocator, name);

    self.scope.open(self.allocator, null, .{ .barrier = true });
    errdefer _ = self.scope.close();

    self.containers.append(self.allocator, self.ast.toSource(node.name));
    defer _ = self.containers.pop();

    const captures = try self.loadFunctionCaptures(&node.meta.captures);
    const param_res = try self.fnParams(node.params, ctx);

    var fn_type: Type.Function = .{
        .loc = .{ .name = name, .container = container_name },
        .params = param_res.decls,
        .return_type = try self.checkAndGetType(node.return_type, ctx),
        .kind = if (param_res.is_method) .method else .normal,
    };
    const interned_type = self.ti.intern(.{ .function = fn_type });
    sym.type = interned_type;
    ctx.fn_type = interned_type;

    ctx.decl_type = fn_type.return_type;
    defer ctx.decl_type = null;

    const body_instrs, const returns = try self.fnBody(node.body.nodes, &fn_type, span, ctx);
    _ = self.scope.close();

    // If it's a closure, it lives on the stack at runtime
    if (captures.len > 0) {
        _ = try self.declareVariable(name, interned_type, false, true, true, false, null, span);
    }

    if (name == self.cached_names.main and self.scope.isGlobal()) {
        self.main = sym.index;
    }

    return .{
        .instr = self.irb.addInstr(
            .{ .fn_decl = .{
                .sym_index = sym.index,
                .type_id = self.ti.typeId(interned_type),
                .name = name,
                .body = body_instrs,
                .defaults = param_res.defaults,
                .captures = captures,
                .returns = returns,
            } },
            span.start,
        ),
        .sym = sym.*,
    };
}

fn loadFunctionCaptures(self: *Self, captures_meta: *const Ast.FnDecl.Meta.Captures) Error![]const Instr.FnDecl.Capture {
    var captures: ArrayList(Instr.FnDecl.Capture) = .empty;
    captures.ensureTotalCapacity(self.allocator, captures_meta.count()) catch oom();

    var it = captures_meta.iterator();
    while (it.next()) |capt| {
        const name = capt.key_ptr.*;
        const capt_infos = capt.value_ptr.*;
        const variable, _ = self.scope.getVariable(name) orelse unreachable;
        _ = try self.declareVariable(name, variable.type, true, true, false, false, null, .zero);
        captures.appendAssumeCapacity(.{ .index = capt_infos.index, .local = capt_infos.is_local });
    }

    return captures.toOwnedSlice(self.allocator) catch oom();
}

const Params = struct {
    decls: AutoArrayHashMapUnmanaged(InternerIdx, Type.Function.Parameter),
    defaults: []const InstrIndex,
    is_method: bool,
};

fn fnParams(self: *Self, params: []Ast.VarDecl, ctx: *Context) Error!Params {
    var decls: AutoArrayHashMapUnmanaged(InternerIdx, Type.Function.Parameter) = .empty;
    decls.ensureTotalCapacity(self.allocator, params.len) catch oom();

    var is_method = false;
    var defaults: ArrayList(InstrIndex) = .empty;

    for (params, 0..) |*p, i| {
        const span = self.ast.getSpan(p.name);
        const param_name = self.interner.intern(self.ast.toSource(p.name));

        if (i == 0 and param_name == self.cached_names.self) {
            const self_type = ctx.self_type orelse return self.err(.self_outside_decl, span);

            is_method = true;
            _ = try self.declareVariable(param_name, self_type, p.meta.captured, true, true, false, null, .zero);
            decls.putAssumeCapacity(param_name, .{
                .name = param_name,
                .type = self_type,
                .default = null,
                .captured = false,
            });
            continue;
        }

        if (self.scope.isVarOrSymInCurrentScope(param_name)) {
            return self.err(.{ .already_declared_param = .{ .name = self.ast.toSource(p.name) } }, span);
        }

        var const_index: ?ConstIdx = null;
        var param_type = try self.checkAndGetType(p.typ, ctx);

        if (p.value) |val| {
            const value_res, const constant = try self.defaultValue(param_type, val, .parameter, ctx);

            const_index = constant;
            param_type = value_res.type;
            defaults.append(self.allocator, value_res.instr) catch oom();
        }

        if (param_type.is(.void)) return self.err(.void_param, span);

        _ = try self.declareVariable(param_name, param_type, p.meta.captured, true, true, false, null, span);
        decls.putAssumeCapacity(param_name, .{
            .name = param_name,
            .type = param_type,
            .default = const_index,
            .captured = p.meta.captured,
        });
    }

    return .{ .decls = decls, .defaults = defaults.toOwnedSlice(self.allocator) catch oom(), .is_method = is_method };
}

/// Analyses function's body returning all of the instructions and a flag indicating if the function returns
fn fnBody(self: *Self, body: []Node, fn_type: *const Type.Function, name_span: Span, ctx: *Context) Error!struct { []const InstrIndex, bool } {
    const err_count = self.errs.items.len;
    var final_type: *const Type = self.ti.getCached(.void);
    var returns = false;
    const len = body.len;

    var instrs: ArrayList(InstrIndex) = .empty;
    instrs.ensureTotalCapacity(self.allocator, body.len) catch oom();

    for (body, 0..) |*n, i| {
        const last = i == len - 1;

        // We try to analyze the whole body
        const res = self.analyzeNode(n, .maybe, ctx) catch continue;

        // Type checking is done in return expression analyzis
        final_type = res.type;
        returns = res.cf == .@"return";

        // If last expression produced a value and that it wasn't the last one we pop it
        if (fn_type.return_type == self.ti.getCached(.void) and !res.type.is(.void) and !res.type.is(.never)) {
            instrs.appendAssumeCapacity(self.irb.wrapPreviousInstr(.pop));
        } else {
            instrs.appendAssumeCapacity(res.instr);
        }

        if (returns and !last) {
            self.warn(.dead_code, self.ast.getSpan(n));
            break;
        }
    }

    // If you had an error, we don't even check this
    if (err_count == self.errs.items.len and !returns and !fn_type.return_type.is(.void)) {
        const span = if (body.len == 0) name_span else self.ast.getSpan(body[body.len - 1]);
        return self.err(.{ .fn_expect_value = .{ .expect = self.typeName(fn_type.return_type) } }, span);
    }

    return .{ instrs.toOwnedSlice(self.allocator) catch oom(), returns };
}

/// Kind has to be either `param` or `field`
fn defaultValue(self: *Self, decl_type: *const Type, val: *const Expr, kind: anytype, ctx: *Context) Error!struct { InstrInfos, ?ConstIdx } {
    var value_res = try self.analyzeExpr(val, .value, ctx);
    value_res.type = try self.performTypeCoercion(decl_type, value_res.type, false, self.ast.getSpan(val));

    if (!value_res.ti.comp_time) {
        return self.err(.{ .non_comptime_default = .new(kind) }, self.ast.getSpan(val));
    }

    // TODO: see if this is safe enough with comp_time check above
    const const_index = self.irb.getInstr(value_res.instr).constant.index;
    return .{ value_res, const_index };
}

fn print(self: *Self, expr: *const Expr, ctx: *Context) StmtResult {
    const res = try self.analyzeExpr(expr, .any, ctx);
    return self.irb.wrapInstr(.print, res.instr);
}

fn use(self: *Self, node: *const Ast.Use) Error!void {
    const name_token = if (node.alias) |alias| alias else node.names[node.names.len - 1];
    const module_name = self.interner.intern(self.ast.toSource(name_token));

    if (self.scope.isModuleImported(module_name)) {
        return self.err(
            .{ .already_declared = .{ .name = self.ast.toSource(name_token) } },
            self.ast.getSpan(name_token),
        );
    }

    const old_path_length = self.path.len();
    defer self.path.shrink(self.allocator, old_path_length);

    const result = Importer.fetchImportedFile(self.allocator, self.ast, node.names, self.path);
    const file_infos = switch (result) {
        .ok => |f| f,
        .err => |e| {
            self.errs.append(self.allocator, e) catch oom();
            return error.Err;
        },
    };

    // TODO: don't check only path but path + name?
    const interned = self.interner.intern(file_infos.path);

    if (!self.pipeline.state.module_interner.analyzed.contains(interned)) {
        var pipeline = self.pipeline.createSubPipeline();
        // TODO: proper error handling, for now just print errors and exit
        _ = pipeline.run(file_infos.name, file_infos.path, file_infos.content) catch {
            std.process.exit(0);
        };
    }

    if (node.items) |items| {
        const mod = self.pipeline.state.module_interner.analyzed.get(interned).?;
        const mod_index = self.pipeline.state.module_interner.analyzed.getIndex(interned).?;

        for (items) |item| {
            const item_name = self.interner.intern(self.ast.toSource(item.item));
            const sym = mod.symbols.get(item_name) orelse return self.err(
                .{ .missing_symbol_in_module = .{
                    .module = self.ast.toSource(node.names[node.names.len - 1]),
                    .symbol = self.ast.toSource(item.item),
                } },
                self.ast.getSpan(item.item),
            );

            // TODO: error
            if (!sym.type.is(.function) and !sym.type.is(.structure)) {
                @panic("Import not supported yet");
            }

            const item_token = if (item.alias) |alias| alias else item.item;
            const item_interned = self.interner.intern(self.ast.toSource(item_token));
            self.scope.declareExternSymbol(self.allocator, item_interned, mod_index, sym);
        }
    } else {
        self.scope.declareModule(self.allocator, module_name, self.ti.intern(.{ .module = interned }));
    }
}

fn expectAssignableValue(self: *Self, expr: *const Ast.Expr, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);

    const value_res: ?InstrInfos = switch (expr.*) {
        .identifier => |e| b: {
            const value = self.resolveIdentifier(e, true, ctx) catch break :b null;
            if (value.kind == .symbol and value.type.* != .function) break :b null;
            break :b .{ .type = value.type, .ti = .{ .heap = value.type.isHeap() }, .instr = value.instr };
        },
        .field => |*e| b: {
            const field_res = try self.field(e, ctx);
            if (field_res.ti.is_sym and field_res.type.* != .function) break :b null;
            break :b field_res;
        },
        else => try self.analyzeExpr(expr, .value, ctx),
    };

    return value_res orelse self.err(.assign_type, span);
}

/// Wraps the instruction inside `incr_rc` if `heap` is true
fn checkWrap(self: *Self, instr: *InstrIndex, heap: bool) void {
    if (heap) {
        instr.* = self.irb.wrapInstr(.incr_rc, instr.*);
    }
}

fn varDeclaration(self: *Self, node: *const Ast.VarDecl, ctx: *Context) StmtResult {
    const span = self.ast.getSpan(node.name);
    const name = try self.internIfNotInCurrentScope(node.name);
    var checked_type = try self.checkAndGetType(node.typ, ctx);

    ctx.decl_type = if (!checked_type.is(.void)) checked_type else null;
    defer ctx.decl_type = null;

    const value_res = if (node.value) |value| v: {
        var value_res = try self.expectAssignableValue(value, ctx);
        checked_type = try self.performTypeCoercion(checked_type, value_res.type, false, self.ast.getSpan(value));
        self.checkWrap(&value_res.instr, value_res.ti.heap);

        break :v value_res;
    } else null;

    const decl_index = try self.declareVariable(
        name,
        checked_type,
        node.meta.captured,
        value_res != null,
        node.is_const,
        if (value_res) |v| v.ti.comp_time else false,
        if (value_res) |v| v.ti.ext_mod else null,
        span,
    );

    return self.irb.addInstr(
        .{ .var_decl = .{
            .box = node.meta.captured,
            .value = if (value_res) |v| v.instr else null,
            .variable = .{ .index = decl_index, .scope = if (self.scope.isGlobal()) .global else .local },
        } },
        span.start,
    );
}

fn multiVarDecl(self: *Self, node: *const Ast.MultiVarDecl, ctx: *Context) StmtResult {
    var decls: ArrayList(InstrIndex) = .empty;
    decls.ensureTotalCapacity(self.allocator, node.decls.len) catch oom();

    for (node.decls) |*n| {
        decls.appendAssumeCapacity(try self.varDeclaration(n, ctx));
    }

    return self.irb.addInstr(
        .{ .multiple_var_decl = .{ .decls = decls.toOwnedSlice(self.allocator) catch oom() } },
        self.ast.getSpan(node).start,
    );
}

fn structDecl(self: *Self, node: *const Ast.StructDecl, ctx: *Context) StmtResult {
    const span = self.ast.getSpan(node);
    const name = try self.internIfNotInCurrentScope(node.name);

    var buf: [1024]u8 = undefined;
    const container_name = self.interner.internKeepRef(self.allocator, self.containers.renderWithSep(&buf, "."));

    const interned = self.ti.newStruct(.{ .name = name, .container = container_name });
    const ty = &interned.structure;

    const sym = self.scope.forwardDeclareSymbol(self.allocator, name);
    sym.type = interned;

    ctx.self_type = interned;
    defer ctx.self_type = null;

    try self.openContainer(node.name);
    defer self.closeContainer();

    const default_fields = try self.structureFields(node.fields, ty, ctx);
    const funcs = try self.containerFnDecls(node.functions, &ty.functions, ctx);

    return self.irb.addInstr(
        .{ .struct_decl = .{
            .name = name,
            .sym_index = sym.index,
            .type_id = self.ti.typeId(sym.type),
            .fields_count = node.fields.len,
            .default_fields = default_fields,
            .functions = funcs,
        } },
        span.start,
    );
}

fn structureFields(self: *Self, fields: []const Ast.VarDecl, ty: *Type.Structure, ctx: *Context) Error![]const InstrIndex {
    var default_fields: ArrayList(InstrIndex) = .empty;
    ty.fields.ensureTotalCapacity(self.allocator, fields.len) catch oom();

    for (fields) |*f| {
        const span = self.ast.getSpan(f.name);
        const field_name = self.interner.intern(self.ast.toSource(f.name));

        if (ty.fields.get(field_name) != null) return self.err(
            .{ .already_declared_field = .{ .name = self.ast.toSource(f.name) } },
            span,
        );

        var struct_field: Type.Structure.Field = .{
            .type = try self.checkAndGetType(f.typ, ctx),
            .default = null,
        };

        ctx.decl_type = struct_field.type;

        if (f.value) |value| {
            const value_res, const constant = try self.defaultValue(struct_field.type, value, .field, ctx);
            if (struct_field.type.is(.void)) struct_field.type = value_res.type;

            struct_field.default = constant;
            default_fields.append(self.allocator, value_res.instr) catch oom();
        }

        ty.fields.putAssumeCapacity(field_name, struct_field);
    }

    return default_fields.toOwnedSlice(self.allocator) catch oom();
}

fn whileStmt(self: *Self, node: *const Ast.While, ctx: *Context) StmtResult {
    const span = self.ast.getSpan(node.pattern);
    const cond_res = try self.pattern(node.pattern, ctx);

    if (!cond_res.type.is(.bool)) return self.err(
        .{ .non_bool_cond = .{ .what = "while", .found = self.typeName(cond_res.type) } },
        span,
    );
    const body_res = try self.block(&node.body, 0, .{ .can_continue = true }, ctx);

    return self.irb.addInstr(.{ .@"while" = .{ .cond = cond_res.instr, .body = body_res.instr } }, span.start);
}

pub const ExprResKind = enum {
    any,
    value,
    maybe,
    symbol,
    none,

    pub fn expects(self: ExprResKind) bool {
        return self == .any or self == .value or self == .symbol;
    }
};

pub fn analyzeExpr(self: *Self, expr: *const Expr, expect: ExprResKind, ctx: *Context) Result {
    const res = try switch (expr.*) {
        .array => |*e| self.array(e, ctx),
        .block => |*e| self.block(e, 0, .{ .exp_val = expect == .value }, ctx),
        .binop => |e| self.binop(e, ctx),
        .bool => |e| self.boolLit(e),
        .@"break" => |*e| self.breakExpr(e, ctx),
        .closure => |*e| self.closure(e, ctx),
        .enum_lit => |e| self.enumLit(e, ctx),
        .fail => |e| self.fail(e, ctx),
        .field => |*e| self.field(e, ctx),
        .float => |e| self.floatLit(e, false),
        .fn_call => |*e| self.call(e, ctx),
        .grouping => |*e| self.analyzeExpr(e.expr, expect, ctx),
        .identifier => |e| self.identifier(e, ctx),
        .@"if" => |*e| self.ifExpr(e, expect, ctx),
        .indexing => |*e| self.indexing(e, ctx),
        .int => |e| self.intLit(e, false),
        .match => |e| self.match(e, expect, ctx),
        .null => |e| self.nullLit(e),
        .pattern => |e| self.pattern(e, ctx),
        .@"return" => |*e| self.returnExpr(e, ctx),
        .self => |e| self.identifier(e, ctx),
        .string => |e| self.string(e),
        .struct_literal => |*e| self.structLiteral(e, ctx),
        .ternary => |*e| self.ternary(e, ctx),
        .trap => |e| self.trap(e, ctx),
        .unary => |*e| self.unary(e, ctx),
    };

    const span = self.ast.getSpan(expr);
    const ty = res.type;

    switch (expect) {
        .any => try self.checkNotVoid(ty, span),
        .value => {
            try self.checkNotVoid(ty, span);

            // Functions are first class object and treated like values
            if (res.ti.is_sym and !ty.is(.function)) return self.err(
                .{ .expect_value_found_type = .{ .found = self.typeName(ty) } },
                self.ast.getSpan(expr),
            );
        },
        .maybe => {},
        .symbol => if (!res.ti.is_sym) return error.NotSymbol,
        // Either used by blocks or asked by top level statement but blocks consume `.none`
        .none => return self.err(.expect_statement, self.ast.getSpan(expr)),
    }

    if (self.scope.isGlobal() and !res.ti.comp_time) {
        return self.err(.non_comptime_in_global, self.ast.getSpan(expr));
    }

    return res;
}

fn checkNotVoid(self: *Self, ty: *const Type, span: Span) Error!void {
    if (ty.is(.void)) return self.err(.void_value, span);
}

fn array(self: *Self, expr: *const Ast.Array, ctx: *Context) Result {
    var pure = true;
    var values = ArrayList(InstrIndex).initCapacity(self.allocator, expr.values.len) catch oom();
    var types: Set(*const Type) = .empty;

    for (expr.values) |val| {
        const val_res = try self.analyzeExpr(val, .value, ctx);
        var val_instr = val_res.instr;
        types.add(self.allocator, val_res.type) catch oom();

        self.checkWrap(&val_instr, val_res.ti.heap);
        pure = pure and val_res.ti.comp_time;
        values.appendAssumeCapacity(val_instr);
    }

    return .{
        .type = self.ti.intern(.{ .array = .{ .child = self.mergeTypes(types.toOwned()) } }),
        .ti = .{ .comp_time = pure },
        .instr = self.irb.addInstr(
            .{ .array = .{ .values = values.toOwnedSlice(self.allocator) catch oom() } },
            self.ast.getSpan(expr).start,
        ),
    };
}

/// `pop_offset` is used to pop that amount less variables at scope closure
fn block(self: *Self, expr: *const Ast.Block, pop_offset: usize, opts: LexScope.Scope.Options, ctx: *Context) Result {
    self.scope.open(self.allocator, self.internLabel(expr.label), opts);

    var instrs: ArrayList(InstrIndex) = .empty;
    instrs.ensureTotalCapacity(self.allocator, expr.nodes.len) catch oom();

    var pure = false;
    var cf: InstrInfos.ControlFlow = .none;

    for (expr.nodes) |*node| {
        errdefer _ = self.scope.close();

        var res = try self.analyzeNode(node, .maybe, ctx);
        pure = pure and res.ti.comp_time;

        // Type checking is done in break expression analyzis
        if (!res.type.is(.void) and !res.type.is(.never)) {
            instrs.appendAssumeCapacity(self.irb.wrapPreviousInstr(.pop));
        } else {
            instrs.appendAssumeCapacity(res.instr);
        }

        if (res.cf.exitScope()) {
            cf = res.cf;
            break;
        }
    }

    const pop_count, const breaks = self.scope.close();
    const final = ty: {
        // If the block returned and we have no breaks, it means we returned with 'return',
        // so we exited scope complytely
        if (cf == .@"return" and breaks.len == 0) break :ty self.ti.getCached(.never);

        // If the block partially or doesn't return, if we expect a value it's an error otherwise we
        // choose the safe option to return void
        if (cf == .none) {
            if (opts.exp_val) {
                return self.err(.block_all_path_dont_return, self.ast.getSpan(expr));
            }

            break :ty self.ti.getCached(.void);
        }

        // Else we merge all possibilities
        var types = Set(*const Type).fromSlice(self.allocator, breaks) catch oom();
        break :ty self.mergeTypes(types.toOwned());
    };

    // TODO: protect cast
    return .{
        .type = final,
        .cf = if (cf == .@"return") .@"return" else .none,
        .instr = self.irb.addInstr(
            .{ .block = .{
                .instrs = instrs.toOwnedSlice(self.allocator) catch oom(),
                .pop_count = @intCast(pop_count - pop_offset),
                .is_expr = !final.is(.void) and !final.is(.never),
            } },
            self.ast.getSpan(expr).start,
        ),
    };
}

fn internLabel(self: *Self, label: ?Ast.TokenIndex) ?InternerIdx {
    const lbl = label orelse return null;
    return self.interner.intern(self.ast.toSource(lbl));
}

fn binop(self: *Self, expr: Ast.Binop, ctx: *Context) Result {
    if (expr.op == .dot_dot) {
        return self.range(expr, ctx);
    } else if (expr.op == .in) {
        return self.in(expr, ctx);
    }

    const lhs = try self.analyzeExpr(expr.lhs, .value, ctx);

    // For enum literals
    ctx.decl_type = if (!lhs.type.is(.void)) lhs.type else null;

    const rhs = try self.analyzeExpr(expr.rhs, .value, ctx);

    const lhs_type = lhs.type;
    const rhs_type = rhs.type;

    const lhs_span = self.ast.getSpan(expr.lhs);
    const rhs_span = self.ast.getSpan(expr.rhs);

    if (isStringConcat(expr.op, lhs_type, rhs_type)) {
        return .{
            .type = self.ti.cache.str,
            .instr = self.irb.addInstr(.{ .binop = .{ .op = .add_str, .lhs = lhs.instr, .rhs = rhs.instr } }, lhs_span.start),
        };
    } else if (isStringRepeat(expr.op, lhs_type, rhs_type)) {
        return .{
            .type = self.ti.cache.str,
            .instr = self.irb.addInstr(
                .{ .binop = .{
                    .op = .mul_str,
                    .lhs = if (lhs_type.is(.int)) lhs.instr else rhs.instr,
                    .rhs = if (lhs_type.is(.int)) rhs.instr else lhs.instr,
                } },
                lhs_span.start,
            ),
        };
    }

    const op: Instr.Binop.Op, const lhs_instr, const rhs_instr, const ty = instr: {
        switch (expr.op) {
            .plus, .slash, .star, .minus, .modulo => {
                const lhs_instr, const rhs_instr, const ty = binopArithmeticCoercion(lhs, rhs) catch |e| return switch (e) {
                    error.NonNumLsh => self.err(.{ .invalid_arithmetic = .{ .found = self.typeName(lhs.type) } }, lhs_span),
                    error.NonNumRhs => self.err(.{ .invalid_arithmetic = .{ .found = self.typeName(rhs.type) } }, rhs_span),
                };
                break :instr .{ getArithmeticOp(expr.op, ty), lhs_instr, rhs_instr, ty };
            },
            .greater_equal, .greater, .less_equal, .less => {
                const lhs_instr, const rhs_instr, const ty = binopArithmeticCoercion(lhs, rhs) catch |e| return switch (e) {
                    error.NonNumLsh => self.err(.{ .invalid_arithmetic = .{ .found = self.typeName(lhs.type) } }, lhs_span),
                    error.NonNumRhs => self.err(.{ .invalid_arithmetic = .{ .found = self.typeName(rhs.type) } }, rhs_span),
                };

                if (lhs_type.is(.float) or rhs_type.is(.float)) self.warn(.float_equal, self.ast.getSpan(expr));

                break :instr .{ getArithmeticOp(expr.op, ty), lhs_instr, rhs_instr, self.ti.getCached(.bool) };
            },
            .equal_equal, .bang_equal => {
                const lhs_instr, const rhs_instr, const ty = binopComparisonCoercion(lhs, rhs) catch |e| return switch (e) {
                    error.NonNullLhs => self.err(.{ .non_null_comp_optional = .{ .found = self.typeName(lhs_type) } }, lhs_span),
                    error.NonNullRhs => self.err(.{ .non_null_comp_optional = .{ .found = self.typeName(rhs_type) } }, rhs_span),
                    error.Invalid => self.err(
                        .{ .invalid_comparison = .{ .found1 = self.typeName(lhs_type), .found2 = self.typeName(rhs_type) } },
                        self.ast.getSpan(expr),
                    ),
                };

                if (lhs_type.is(.float) or rhs_type.is(.float)) self.warn(.float_equal, self.ast.getSpan(expr));

                break :instr .{ getComparisonOp(expr.op, ty), lhs_instr, rhs_instr, self.ti.getCached(.bool) };
            },
            .@"and", .@"or" => {
                if (!lhs_type.is(.bool)) return self.err(.{ .invalid_logical = .{ .found = self.typeName(lhs_type) } }, lhs_span);
                if (!rhs_type.is(.bool)) return self.err(.{ .invalid_logical = .{ .found = self.typeName(rhs_type) } }, rhs_span);

                break :instr .{ if (expr.op == .@"and") .@"and" else .@"or", lhs.instr, rhs.instr, self.ti.getCached(.bool) };
            },
            else => unreachable,
        }
    };

    return .{
        .type = ty,
        .ti = .{ .comp_time = lhs.ti.comp_time and rhs.ti.comp_time },
        .instr = self.irb.addInstr(.{ .binop = .{ .op = op, .lhs = lhs_instr, .rhs = rhs_instr } }, lhs_span.start),
    };
}

fn isStringConcat(op: TokenTag, lhs: *const Type, rhs: *const Type) bool {
    return op == .plus and lhs.is(.str) and rhs.is(.str);
}

fn isStringRepeat(op: TokenTag, lhs: *const Type, rhs: *const Type) bool {
    return op == .star and ((lhs.is(.str) and rhs.is(.int)) or (lhs.is(.int) and rhs.is(.str)));
}

fn binopArithmeticCoercion(
    lhs: InstrInfos,
    rhs: InstrInfos,
) error{ NonNumLsh, NonNumRhs }!struct { InstrIndex, InstrIndex, *const Type } {
    const lhs_type = lhs.type;
    const rhs_type = rhs.type;

    if (!lhs_type.isNumeric()) return error.NonNumLsh;
    if (!rhs_type.isNumeric()) return error.NonNumRhs;

    return .{ lhs.instr, rhs.instr, lhs_type };
}

fn getArithmeticOp(op: TokenTag, ty: *const Type) Instr.Binop.Op {
    return switch (op) {
        .plus => if (ty.is(.int)) .add_int else .add_float,
        .slash => if (ty.is(.int)) .div_int else .div_float,
        .star => if (ty.is(.int)) .mul_int else .mul_float,
        .minus => if (ty.is(.int)) .sub_int else .sub_float,
        .modulo => if (ty.is(.int)) .mod_int else .mod_float,

        .less => if (ty.is(.int)) .lt_int else .lt_float,
        .less_equal => if (ty.is(.int)) .le_int else .le_float,
        .greater => if (ty.is(.int)) .gt_int else .gt_float,
        .greater_equal => if (ty.is(.int)) .ge_int else .ge_float,
        else => unreachable,
    };
}

fn binopComparisonCoercion(
    lhs: InstrInfos,
    rhs: InstrInfos,
) error{ NonNullLhs, NonNullRhs, Invalid }!struct { InstrIndex, InstrIndex, *const Type } {
    const lhs_type = lhs.type;
    const rhs_type = rhs.type;

    if (lhs_type == rhs_type) return .{ lhs.instr, rhs.instr, lhs_type };

    arithmetic: {
        return binopArithmeticCoercion(lhs, rhs) catch break :arithmetic;
    }

    if (lhs_type.is(.optional) or rhs_type.is(.optional)) {
        if (lhs_type.is(.null) or rhs_type.is(.null)) {
            if (lhs_type.is(.optional)) {
                return .{ lhs.instr, rhs.instr, lhs_type };
            } else {
                return .{ rhs.instr, lhs.instr, rhs_type };
            }
        }

        if (lhs_type.is(.optional)) return error.NonNullRhs;
        if (rhs_type.is(.optional)) return error.NonNullLhs;
    }

    return error.Invalid;
}

fn getComparisonOp(op: TokenTag, ty: *const Type) Instr.Binop.Op {
    return switch (ty.*) {
        .bool => if (op == .equal_equal) .eq_bool else .ne_bool,
        .int => if (op == .equal_equal) .eq_int else .ne_int,
        .float => if (op == .equal_equal) .eq_float else .ne_float,
        .str => if (op == .equal_equal) .eq_str else .ne_str,
        .null, .optional => if (op == .equal_equal) .eq_null else .ne_null,
        .@"enum" => if (op == .equal_equal) .eq_tag else .ne_tag,
        else => unreachable,
    };
}

fn breakExpr(self: *Self, expr: *const Ast.Break, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);

    // We take scope's index because evaluating the possible `break` expression can invalidate a pointer to scope
    const scope_index, const depth = self.scope.getScope(self.internLabel(expr.label)) catch return self.err(
        .{ .undeclared_block_label = .{ .name = self.ast.toSource(expr.label.?) } },
        self.ast.getSpan(expr.label.?),
    );
    const scope_exp_val = self.scope.scopes.items[scope_index].opts.exp_val;

    const ty, const expr_instr = brk: {
        const e = expr.expr orelse break :brk .{ self.ti.getCached(.void), null };

        if (!scope_exp_val) {
            return self.err(.break_val_in_non_val_block, self.ast.getSpan(e));
        }

        const res = try self.analyzeExpr(e, if (scope_exp_val) .value else .none, ctx);
        break :brk .{ res.type, res.instr };
    };

    const scope = &self.scope.scopes.items[scope_index];

    const instr = self.irb.addInstr(
        .{ .@"break" = .{
            .instr = expr_instr,
            .depth = depth,
            .pop_count = self.scope.stackDiffWithCurrent(scope),
        } },
        span.start,
    );
    scope.breaks.append(self.allocator, ty) catch oom();

    // Break always return void because its value escapes scope
    return .{
        .type = self.ti.getCached(.void),
        .cf = .@"break",
        .instr = instr,
    };
}

fn closure(self: *Self, expr: *const Ast.FnDecl, ctx: *Context) Result {
    // TODO: create an anonymus name generator mechanism
    var sym = self.scope.forwardDeclareSymbol(self.allocator, self.interner.intern("azert"));

    self.scope.open(self.allocator, null, .{ .barrier = true });
    defer _ = self.scope.close();

    const captures = try self.loadFunctionCaptures(&expr.meta.captures);
    const param_res = try self.fnParams(expr.params, ctx);

    // Update type for resolution in function's body
    const closure_type: Type.Function = .{
        .loc = null,
        .params = param_res.decls,
        .return_type = try self.checkAndGetType(expr.return_type, ctx),
        .kind = .normal,
    };
    const interned_type = self.ti.intern(.{ .function = closure_type });
    sym.type = interned_type;

    const span = self.ast.getSpan(expr);
    const offset = span.start;

    ctx.fn_type = interned_type;
    const body_instrs, const returns = try self.fnBody(expr.body.nodes, &closure_type, span, ctx);

    // TODO: protect the cast
    return .{
        .type = interned_type,
        .instr = self.irb.addInstr(
            .{ .fn_decl = .{
                .sym_index = sym.index,
                .type_id = self.ti.typeId(interned_type),
                .name = null,
                .body = body_instrs,
                .defaults = param_res.defaults,
                .captures = captures,
                .returns = returns,
            } },
            offset,
        ),
    };
}

pub fn enumLit(self: *Self, tag: Ast.TokenIndex, ctx: *Context) Result {
    const span = self.ast.getSpan(tag);
    const decl = ctx.decl_type orelse return self.err(.enum_lit_no_type, span);

    const enum_ty = decl.as(.@"enum") orelse return self.err(
        .{ .enum_lit_non_enum = .{ .found = self.typeName(decl) } },
        span,
    );
    const tag_name = self.interner.intern(self.ast.toSource(tag));

    const tag_index = enum_ty.tags.getIndex(tag_name) orelse return self.err(
        .{ .enum_unknown_decl = .{ .@"enum" = self.typeName(decl), .field = self.ast.toSource(tag) } },
        span,
    );

    // It must exist because type parsed before expression, so if not existing it would have error already
    const name = self.scope.getSymbolName(decl).?;
    const sym = self.symbolIdentifier(name, span).?;

    return .{
        .type = decl,
        .ti = .{ .comp_time = enum_ty.tags.get(tag_name).?.is(.void) },
        .instr = self.irb.addConstant(
            // TODO: protect the cast
            .{ .enum_instance = .{
                .sym = .{ .module_index = 0, .symbol_index = @intCast(sym.sym.index) },
                .tag_index = @intCast(tag_index),
            } },
            span.start,
        ),
    };
}

fn fail(self: *Self, expr: Ast.Fail, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);
    const fn_type = ctx.fn_type orelse return self.err(.fail_outside_fn, span);
    const ty = fn_type.function.return_type;

    var value_res = try self.analyzeExpr(expr.expr, .value, ctx);

    // We can't return an error with 'return' unless we're defining a method on the error's enum
    if (!value_res.type.isErr()) return self.err(
        .{ .fail_no_err = .{ .found = self.typeName(value_res.type) } },
        span,
    );

    value_res.type = try self.performTypeCoercion(ty, value_res.type, true, self.ast.getSpan(expr.expr));

    return .{
        .type = value_res.type,
        .cf = .@"return",
        .instr = self.irb.addInstr(.{ .fail = .{ .value = value_res.instr } }, span.start),
    };
}

pub fn field(self: *Self, expr: *const Ast.Field, ctx: *Context) Result {
    const span = self.ast.getSpan(expr.structure);
    const struct_res = try self.analyzeExpr(expr.structure, .any, ctx);

    const field_res = switch (struct_res.type.*) {
        .@"enum" => |ty| b: {
            const res = try self.enumAccess(struct_res, ty, expr.field);

            switch (res) {
                .tag => |tag| return tag,
                .decl => |decl| break :b decl,
            }
        },
        .structure => |*ty| try self.structureAccess(expr.field, ty, struct_res.ti.is_sym, ctx.in_call),
        .module => |ty| return self.moduleAccess(expr.field, ty),
        .array => |ty| return self.arrayFnAccess(struct_res.instr, expr.field, ty),
        else => return self.err(
            .{ .non_struct_field_access = .{ .found = self.typeName(struct_res.type) } },
            span,
        ),
    };

    const is_static = field_res.kind == .function and struct_res.ti.is_sym;

    if (!is_static and field_res.kind != .field and !ctx.in_call) {
        return self.boundMethod(field_res.type, field_res.index, struct_res.instr, span);
    }

    // Lhs must be heap_ref too. It's used to allow calls to break chains like: getVec().point1
    // If the object returned by getVec() is a literal for example, the rest of the chain will be a stack allocated object
    return .{
        .type = field_res.type,
        .ti = .{
            .heap = struct_res.ti.heap and field_res.type.isHeap(),
            .is_sym = field_res.kind == .function,
            .ext_mod = struct_res.ti.ext_mod,
        },
        // If it's a static function, we just call it
        .instr = if (is_static)
            self.irb.addInstr(
                .{ .load_symbol = .{ .symbol_index = @intCast(field_res.index), .module_index = struct_res.ti.ext_mod } },
                span.start,
            )
            // Other wise we still want to compute 'self' to call the method on it
        else
            self.irb.addInstr(
                .{ .field = .{ .structure = struct_res.instr, .index = field_res.index, .kind = field_res.kind } },
                span.start,
            ),
    };
}

fn arrayFnAccess(self: *Self, array_instr: InstrIndex, fn_tk: Ast.TokenIndex, array_type: Type.Array) Result {
    const func_name = self.ast.toSource(fn_tk);
    const func = self.pipeline.state.array_fns.get(func_name) orelse return self.err(
        .{ .undeclared_field_access = .{ .name = func_name } },
        self.ast.getSpan(fn_tk),
    );

    const ty = self.runtimeFnToType(func, array_type.child);

    return .{
        .type = ty,
        .instr = self.irb.addInstr(
            .{ .obj_func = .{
                .obj = array_instr,
                .fn_index = self.pipeline.state.array_fns.getIndex(func_name).?,
                .kind = .array,
            } },
            self.ast.getSpan(fn_tk).start,
        ),
    };
}

fn runtimeFnToType(self: *Self, func: type_mod.ObjFnInfos, current_generic: *const Type) *const Type {
    var params: Type.Function.ParamsMap = .empty;
    params.ensureTotalCapacity(self.allocator, func.params.len + 1) catch oom();
    params.putAssumeCapacity(
        self.interner.intern("self"),
        .{ .name = self.interner.intern("self"), .type = current_generic, .default = null, .captured = false },
    );

    for (func.params) |p| {
        params.putAssumeCapacity(
            0,
            .{ .name = null, .type = self.runtimeObjToType(p, current_generic), .default = null, .captured = false },
        );
    }

    const ty: Type = .{ .function = .{
        .kind = .method,
        .loc = null,
        .params = params,
        .return_type = self.runtimeObjToType(func.return_type, current_generic),
    } };

    return self.ti.intern(ty);
}

fn runtimeObjToType(self: *Self, ty: type_mod.ObjFnType, current_generic: *const Type) *const Type {
    return switch (ty) {
        .bool => self.ti.getCached(.bool),
        .int => self.ti.getCached(.int),
        .float => self.ti.getCached(.float),
        .str => self.ti.getCached(.str),
        .void => self.ti.getCached(.void),
        .generic => current_generic,
    };
}

const AccessResult = struct {
    type: *const Type,
    kind: Instr.Field.Kind,
    index: usize,
};

const EnumResult = union(enum) {
    tag: InstrInfos,
    decl: AccessResult,
};

fn enumAccess(self: *Self, enum_info: InstrInfos, ty: Type.Enum, tag_tk: Ast.TokenIndex) Error!EnumResult {
    const span = self.ast.getSpan(tag_tk);
    const text = self.ast.toSource(tag_tk);
    const tag_name = self.interner.intern(text);

    if (ty.tags.getIndex(tag_name)) |index| {
        // Can't access a tag on an instance
        if (!enum_info.ti.is_sym) {
            return self.err(.enum_tag_access, span);
        }

        return .{
            .tag = .{
                .type = self.ti.intern(.{ .@"enum" = ty }),
                .ti = .{ .comp_time = ty.tags.get(tag_name).?.is(.void) },
                .instr = self.irb.addConstant(
                    .{ .enum_instance = .{
                        .sym = self.irb.data(enum_info.instr).load_symbol,
                        .tag_index = index,
                    } },
                    self.ast.getSpan(tag_tk).start,
                ),
            },
        };
    } else if (ty.functions.get(tag_name)) |func| {
        return .{ .decl = .{ .type = func.type, .kind = .function, .index = func.index } };
    }

    return self.err(
        .{ .enum_unknown_decl = .{ .@"enum" = self.typeName(enum_info.type), .field = self.ast.toSource(tag_tk) } },
        span,
    );
}

fn structureAccess(self: *Self, field_tk: Ast.TokenIndex, ty: *const Type.Structure, is_symbol: bool, in_call: bool) Error!AccessResult {
    const text = self.ast.toSource(field_tk);
    const field_name = self.interner.intern(text);

    if (ty.fields.getPtr(field_name)) |f| {
        return .{ .type = f.type, .kind = .field, .index = ty.fields.getIndex(field_name).? };
    } else if (ty.functions.get(field_name)) |f| {
        const function = &f.type.function;

        if (in_call) {
            // Call method on type
            if (is_symbol and function.kind == .method) {
                return self.err(.{ .call_method_on_type = .{ .name = text } }, self.ast.getSpan(field_tk));
            }
            // Call static on instance
            else if (!is_symbol and function.kind != .method and function.kind != .native_method) {
                return self.err(.{ .call_static_on_instance = .{ .name = text } }, self.ast.getSpan(field_tk));
            }
        }

        // TODO: can remove the 'Array' hashmap for a hashmap
        return .{ .type = f.type, .kind = .function, .index = f.index };
    }

    return self.err(.{ .undeclared_field_access = .{ .name = text } }, self.ast.getSpan(field_tk));
}

fn moduleAccess(self: *Self, field_tk: Ast.TokenIndex, module_idx: InternerIdx) Result {
    const span = self.ast.getSpan(field_tk);
    const text = self.ast.toSource(field_tk);

    const field_name = self.interner.intern(text);
    const module = self.pipeline.state.module_interner.getAnalyzed(module_idx).?;
    const sym = module.symbols.get(field_name) orelse return self.err(
        .{ .missing_symbol_in_module = .{ .module = module.name, .symbol = text } },
        span,
    );
    const index = self.pipeline.state.module_interner.analyzed.getIndex(module_idx).?;

    // TODO: protect the cast
    return .{
        .type = sym.type,
        .ti = .{ .is_sym = true, .ext_mod = index },
        .instr = self.irb.addInstr(
            .{ .load_symbol = .{ .module_index = index, .symbol_index = @intCast(sym.index) } },
            span.start,
        ),
    };
}

fn boundMethod(self: *Self, func_type: *const Type, field_index: usize, structure: InstrIndex, span: Span) Result {
    const bounded_type = func_type.function.toBoundMethod(self.allocator);
    const ty = self.ti.intern(.{ .function = bounded_type });

    return .{
        .type = ty,
        .instr = self.irb.addInstr(.{ .bound_method = .{ .structure = structure, .index = field_index } }, span.start),
    };
}

fn call(self: *Self, expr: *const Ast.FnCall, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);

    const ctx_call = ctx.setAndGetPrevious(.in_call, true);
    const callee = try self.analyzeExpr(expr.callee, .any, ctx);

    if (!callee.type.is(.function)) return self.err(.invalid_call_target, span);

    // Restore state before arguments analyzis
    ctx.in_call = ctx_call;
    const args_res = try self.fnArgsList(expr.args, &callee.type.function, callee.ti.ext_mod, span, ctx);

    return .{
        .type = callee.type.function.return_type,
        .ti = .{ .ext_mod = callee.ti.ext_mod },
        .instr = self.irb.addInstr(
            .{ .call = .{
                .callee = callee.instr,
                .args = args_res,
                .ext_mod = callee.ti.ext_mod,
                .native = callee.type.function.kind == .native or callee.type.function.kind == .native_method,
            } },
            span.start,
        ),
    };
}

fn fnArgsList(
    self: *Self,
    args: []const Ast.FnCall.Arg,
    ty: *const Type.Function,
    ext_mod: ?usize,
    err_span: Span,
    ctx: *Context,
) Error![]const Instr.Arg {
    var proto = ty.proto(self.allocator);
    const param_count = proto.count();
    const params = ty.params.values()[@intFromBool(ty.kind == .method)..];

    if (args.len > param_count) return self.err(.{ .too_many_fn_args = .{ .expect = param_count, .found = args.len } }, err_span);

    var instrs = self.allocator.alloc(Instr.Arg, params.len) catch oom();
    var proto_values = proto.values();

    for (proto_values, 0..) |val, i| {
        if (val.default) |def| {
            instrs[i] = .{ .default = .{ .const_index = def, .mod = ext_mod } };
        }
    }

    for (args, 0..) |arg, i| {
        var param_info: *const Type.Function.Parameter = undefined;
        const span = self.ast.getSpan(arg.value);

        const index = value: {
            if (arg.name) |param_name| {
                if (ty.kind == .bound) return self.err(.named_arg_in_bounded, self.ast.getSpan(param_name));

                const name = self.interner.intern(self.ast.toSource(param_name));

                param_info = ty.params.getPtr(name) orelse return self.err(
                    .{ .unknown_param = .{ .name = self.ast.toSource(param_name) } },
                    self.ast.getSpan(param_name),
                );

                const gop = proto.getOrPutAssumeCapacity(name);

                if (gop.value_ptr.done) return self.err(
                    .{ .duplicate_param = .{ .name = self.ast.toSource(param_name) } },
                    self.ast.getSpan(param_name),
                );
                gop.value_ptr.done = true;

                break :value gop.index;
            } else {
                param_info = &params[i];
                proto_values[i].done = true;

                break :value i;
            }
        };

        ctx.decl_type = param_info.type;
        var value = try self.analyzeExpr(arg.value, .value, ctx);

        _ = try self.performTypeCoercion(param_info.type, value.type, false, span);

        self.checkWrap(&value.instr, false);
        if (param_info.captured) value.instr = self.irb.wrapPreviousInstr(.box);

        instrs[index] = .{ .instr = value.instr };
    }

    // Check if any missing non-default parameter
    const err_count = self.errs.items.len;

    for (proto.keys(), proto_values) |k, v| {
        if (v.done or v.default != null) continue;
        self.err(.{ .missing_function_param = .{ .name = self.interner.getKey(k).? } }, err_span) catch {};
    }

    return if (err_count < self.errs.items.len) error.Err else instrs;
}

const IdentRes = struct {
    type: *const Type,
    kind: enum { variable, symbol },
    comp_time: bool = true,
    instr: InstrIndex,
    module: ?usize = null,
};

/// Tries to find a match from variables and symbols and returns its type while emitting an instruction
fn resolveIdentifier(self: *Self, token_name: Ast.TokenIndex, initialized: bool, ctx: *const Context) Error!IdentRes {
    const span = self.ast.getSpan(token_name);
    const text = self.ast.toSource(token_name);
    const name = self.interner.intern(text);

    if (self.variableIdentifier(name, span)) |res| {
        if (initialized and !res.variable.initialized) {
            return self.err(.{ .use_uninit_var = .{ .name = text } }, self.ast.getSpan(token_name));
        }
        res.variable.used = true;

        return .{
            .type = res.variable.type,
            .kind = .variable,
            .comp_time = res.variable.comp_time,
            .instr = res.instr,
            .module = res.variable.ext_mod,
        };
    }

    const sym_name = if (name == self.cached_names.Self) b: {
        const ty = ctx.self_type orelse return self.err(.big_self_outside_decl, span);
        break :b switch (ty.*) {
            .@"enum" => |t| t.loc.?.name,
            .structure => |t| t.loc.?.name,
            else => unreachable,
        };
    } else name;

    if (self.symbolIdentifier(sym_name, span)) |res| {
        return .{ .type = res.sym.type, .kind = .symbol, .instr = res.instr };
    }

    if (self.externSymbolIdentifier(sym_name, span)) |res| {
        return .{ .type = res.sym.type, .kind = .symbol, .instr = res.instr, .module = res.mod_index };
    }

    if (self.builtinSymbol(sym_name, span)) |res| {
        return .{ .type = res.sym.type, .kind = .symbol, .instr = res.instr };
    }

    if (self.scope.getModule(name)) |mod| {
        return .{ .type = mod, .kind = .symbol, .instr = 0 };
    }

    return self.err(.{ .undeclared_var = .{ .name = text } }, self.ast.getSpan(token_name));
}

const VariableInstr = struct { variable: *LexScope.Variable, instr: InstrIndex };

fn expectVariableIdentifier(self: *Self, token_name: Ast.TokenIndex) Error!VariableInstr {
    const span = self.ast.getSpan(token_name);
    const text = self.ast.toSource(token_name);
    const name = self.interner.intern(text);

    return self.variableIdentifier(name, span) orelse return self.err(
        .{ .undeclared_var = .{ .name = self.interner.getKey(name).? } },
        span,
    );
}

/// Tries to find a variable in scopes and returns it while emitting an instruction
fn variableIdentifier(self: *Self, name: InternerIdx, span: Span) ?VariableInstr {
    const variable, const scope_offset = self.scope.getVariable(name) orelse return null;

    // TODO: use scope directly instead of this shenanigan
    var instr = self.irb.addInstr(
        .{ .identifier = .{ .index = variable.index + scope_offset, .scope = switch (variable.kind) {
            .local => .local,
            .global => .global,
        } } },
        span.start,
    );

    self.checkWrap(&instr, false);
    if (variable.captured) instr = self.irb.wrapPreviousInstr(.unbox);

    return .{ .variable = variable, .instr = instr };
}

/// Tries to find a symbol in scopes and returns it while emitting an instruction
fn symbolIdentifier(self: *Self, name: InternerIdx, span: Span) ?struct { sym: *LexScope.Symbol, instr: InstrIndex } {
    const sym = self.scope.getSymbol(name) orelse return null;

    // TODO: protect cast
    return .{
        .sym = sym,
        .instr = self.irb.addInstr(
            .{ .load_symbol = .{ .module_index = null, .symbol_index = @intCast(sym.index) } },
            span.start,
        ),
    };
}

/// Tries to find a symbol in scopes and returns it while emitting an instruction
fn externSymbolIdentifier(self: *Self, name: InternerIdx, span: Span) ?struct { sym: *LexScope.Symbol, mod_index: usize, instr: InstrIndex } {
    const ext = self.scope.getExternSymbol(name) orelse return null;

    // TODO: protect cast
    return .{
        .sym = &ext.symbol,
        .mod_index = ext.module_index,
        .instr = self.irb.addInstr(
            .{ .load_symbol = .{ .module_index = ext.module_index, .symbol_index = @intCast(ext.symbol.index) } },
            span.start,
        ),
    };
}

/// Tries to find a symbol in scopes and returns it while emitting an instruction
fn builtinSymbol(self: *Self, name: InternerIdx, span: Span) ?struct { sym: *LexScope.Symbol, instr: InstrIndex } {
    const sym = self.scope.getBuiltinSymbol(name) orelse return null;

    // TODO: protect cast
    return .{
        .sym = sym,
        .instr = self.irb.addInstr(.{ .load_builtin = @intCast(sym.index) }, span.start),
    };
}

// For if, we have to check coehrence of the branches because it can be used as a direct expression like:
//  var a = if true do 4 else null // results in a ?int
// When the branches are scopes, types are checked by the block expression
fn ifExpr(self: *Self, expr: *const Ast.If, expect: ExprResKind, ctx: *Context) Result {
    const span = self.ast.getSpan(expr.pattern);

    const cond_res = try self.pattern(expr.pattern, ctx);
    var pure = cond_res.ti.comp_time;

    // We can continue to analyze if the condition isn't a bool
    if (!cond_res.type.is(.bool)) self.err(
        .{ .non_bool_cond = .{ .what = "if", .found = self.typeName(cond_res.type) } },
        span,
    ) catch {};

    // Analyze then branch
    var then_res = try self.analyzeNode(&expr.then, expect, ctx);
    self.checkWrap(&then_res.instr, then_res.ti.heap);

    pure = pure and then_res.ti.comp_time;

    var else_cf: ?InstrInfos.ControlFlow = null;
    var else_ty: ?*const Type = null;
    var else_instr: ?InstrIndex = null;

    if (expr.@"else") |*n| {
        const else_res = try self.analyzeNode(n, expect, ctx);
        else_instr = if (else_res.ti.heap) self.irb.wrapInstr(.incr_rc, else_res.instr) else else_res.instr;

        pure = pure and else_res.ti.comp_time;
        else_cf = else_res.cf;
        else_ty = else_res.type;
    } else if (expect == .value) {
        return self.err(
            .{ .missing_else_clause = .{ .if_type = self.typeName(then_res.type) } },
            self.ast.getSpan(expr),
        );
    }

    const branch_res = try self.checkIfBranches(then_res.type, then_res.cf, else_ty, else_cf, expr, expect);

    return .{
        .type = branch_res,
        .ti = .{ .comp_time = pure },
        .cf = if (branch_res.is(.never)) .@"return" else .none,
        .instr = self.irb.addInstr(
            .{ .@"if" = .{ .cond = cond_res.instr, .then = then_res.instr, .@"else" = else_instr } },
            span.start,
        ),
    };
}

fn checkIfBranches(
    self: *Self,
    then_ty: *const Type,
    then_cf: InstrInfos.ControlFlow,
    else_ty: ?*const Type,
    else_cf: ?InstrInfos.ControlFlow,
    expr: *const Ast.If,
    expect: ExprResKind,
) Error!*const Type {
    const then_res = try self.checkBranch(then_ty, then_cf, expect, self.ast.getSpan(expr.then));

    const else_type = else_ty orelse return then_res;
    const else_res = try self.checkBranch(else_type, else_cf.?, expect, self.ast.getSpan(expr.@"else".?));

    // No need to get cast information as 'break' and 'return' already handles it
    if (then_cf.exitScope() and else_cf.?.exitScope()) return self.ti.getCached(.never);

    return self.mergeTypes(&.{ then_res, else_res });
}

fn checkBranch(self: *Self, ty: *const Type, cf: InstrInfos.ControlFlow, expect: ExprResKind, span: Span) Error!*const Type {
    if (cf.exitScope()) return self.ti.getCached(.void);
    if (expect == .value and ty.is(.void)) return self.err(.void_value, span);

    return ty;
}

fn in(self: *Self, expr: Ast.Binop, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);
    const needle_res = try self.analyzeExpr(expr.lhs, .value, ctx);
    const haystack_res = try self.analyzeExpr(expr.rhs, .value, ctx);

    const exp_type, const kind: Instr.In.Kind = switch (haystack_res.type.*) {
        .array => |t| .{ t.child, .array },
        .range => |t| .{ t, if (t == self.ti.getCached(.int)) .range_int else .range_float },
        .str => .{ self.ti.getCached(.str), .string },
        else => |*t| return self.err(
            .{ .invalid_in_type = .{ .found = self.typeName(t) } },
            self.ast.getSpan(expr.rhs),
        ),
    };

    if (needle_res.type != exp_type) return self.err(
        .{ .type_mismatch = .{
            .expect = self.typeName(exp_type),
            .found = self.typeName(needle_res.type),
        } },
        span,
    );

    return .{
        .type = needle_res.type,
        .instr = self.irb.addInstr(
            .{ .in = .{
                .needle = needle_res.instr,
                .haystack = haystack_res.instr,
                .kind = kind,
            } },
            span.start,
        ),
    };
}

fn indexing(self: *Self, expr: *const Ast.Indexing, ctx: *Context) Result {
    const index, const index_kind = try self.expectIndex(expr.index, ctx);
    const expr_res = try self.analyzeExpr(expr.expr, .value, ctx);

    // NOTE: when implementing maps, disallow range for it
    const elem_type = switch (expr_res.type.*) {
        .array => |t| t.child,
        .str => expr_res.type,
        else => return self.err(
            .{ .non_indexable_type = .{ .found = self.typeName(expr_res.type) } },
            self.ast.getSpan(expr.expr),
        ),
    };

    return .{
        .type = elem_type,
        .ti = .{ .heap = elem_type.isHeap() },
        .instr = self.irb.addInstr(
            .{ .indexing = .{
                .expr = expr_res.instr,
                .index = index,
                .index_kind = index_kind,
                .kind = if (expr_res.type.* == .array) .array else .str,
            } },
            self.ast.getSpan(expr).start,
        ),
    };
}

/// Analyze the expression and return an error if the type isn't an integer
fn expectIndex(self: *Self, expr: *const Expr, ctx: *Context) Error!struct { InstrIndex, Instr.Indexing.IndexKind } {
    const res = try self.analyzeExpr(expr, .value, ctx);

    return switch (res.type.*) {
        .int => .{ res.instr, .scalar },
        .range => .{ res.instr, .range },
        else => self.err(
            .{ .invalid_index = .{ .found = self.typeName(res.type) } },
            self.ast.getSpan(expr),
        ),
    };
}

pub fn boolLit(self: *Self, expr: Ast.Bool) Result {
    return .{
        .type = self.ti.cache.bool,
        .ti = .{ .comp_time = true },
        .instr = self.irb.addConstant(
            .{ .bool = self.ast.token_tags[expr] == .true },
            self.ast.getSpan(expr).start,
        ),
    };
}

pub fn floatLit(self: *Self, expr: Ast.Float, negate: bool) Result {
    const value = std.fmt.parseFloat(f64, self.ast.toSource(expr)) catch blk: {
        // TODO: error handling, only one possible it's invalid char
        std.debug.print("Error parsing float\n", .{});
        break :blk 0;
    };

    return .{
        .type = self.ti.cache.float,
        .ti = .{ .comp_time = true },
        .instr = self.irb.addConstant(.{ .float = if (negate) -value else value }, self.ast.getSpan(expr).start),
    };
}

pub fn intLit(self: *Self, expr: Ast.Int, negate: bool) Result {
    const value = std.fmt.parseInt(i64, self.ast.toSource(expr), 10) catch blk: {
        // TODO: error handling, only one possible it's invalid char
        std.debug.print("Error parsing integer\n", .{});
        break :blk 0;
    };

    return .{
        .type = self.ti.cache.int,
        .ti = .{ .comp_time = true },
        .instr = self.irb.addConstant(.{ .int = if (negate) -value else value }, self.ast.getSpan(expr).start),
    };
}

pub fn identifier(self: *Self, expr: Ast.Identifier, ctx: *Context) Result {
    const res = try self.resolveIdentifier(expr, true, ctx);
    return .{
        .type = res.type,
        .ti = .{ .heap = res.type.isHeap(), .is_sym = res.kind == .symbol, .comp_time = res.comp_time, .ext_mod = res.module },
        .instr = res.instr,
    };
}

pub fn nullLit(self: *Self, expr: Ast.Null) Result {
    return .{
        .type = self.ti.cache.null,
        .ti = .{ .comp_time = true },
        .instr = self.irb.addConstant(.null, self.ast.getSpan(expr).start),
    };
}

pub fn string(self: *Self, expr: Ast.String) Result {
    const text = self.ast.toSource(expr);
    const span = self.ast.getSpan(expr);

    const no_quotes = text[1 .. text.len - 1];
    var final: ArrayList(u8) = .empty;
    var i: usize = 0;

    while (i < no_quotes.len) : (i += 1) {
        const c = no_quotes[i];

        if (c == '\\') {
            i += 1;

            // Safe access here because lexer checked if the string and termianted
            switch (no_quotes[i]) {
                'n' => final.append(self.allocator, '\n') catch oom(),
                't' => final.append(self.allocator, '\t') catch oom(),
                '"' => final.append(self.allocator, '"') catch oom(),
                'r' => final.append(self.allocator, '\r') catch oom(),
                '\\' => final.append(self.allocator, '\\') catch oom(),
                else => return self.err(
                    .{ .unknow_char_escape = .{ .found = no_quotes[i .. i + 1] } },
                    span,
                ),
            }
        } else final.append(self.allocator, c) catch oom();
    }

    const value = self.interner.intern(final.toOwnedSlice(self.allocator) catch oom());

    return .{
        .type = self.ti.cache.str,
        .ti = .{ .comp_time = true },
        .instr = self.irb.addConstant(.{ .string = value }, span.start),
    };
}

fn match(self: *Self, expr: Ast.Match, expect: ExprResKind, ctx: *Context) Result {
    const value = try self.analyzeExpr(expr.expr, .value, ctx);

    const kind: Instr.Match.Kind, var matcher = ana: switch (value.type.*) {
        .bool => {
            var matcher = matchers.Bool.init();
            break :ana .{ .bool, matcher.matcher() };
        },
        .@"enum" => |t| {
            var matcher = matchers.Enum.init(t.proto(self.allocator));
            break :ana .{ .@"enum", matcher.matcher() };
        },
        .float => {
            var matcher = matchers.Num(f64).init();
            break :ana .{ .float, matcher.matcher() };
        },
        .int => {
            var matcher = matchers.Num(i64).init();
            break :ana .{ .int, matcher.matcher() };
        },
        .str => {
            var matcher = matchers.String.init();
            break :ana .{ .string, matcher.matcher() };
        },
        else => |t| {
            std.log.debug("Found: {any}", .{t});
            @panic("Match on that type is not implemented yet");
        },
    };

    const res = switch (expr.body) {
        .value => |arms| try self.matchValueArms(&matcher, value.type, arms, expr.kw, expect, ctx),
        .type => @panic("TODO"),
    };

    return .{
        .type = self.mergeTypes(res.types),
        .instr = self.irb.addInstr(
            .{ .match = .{
                .expr = value.instr,
                .arms = res.arms,
                .wildcard = res.wildcard,
                .is_expr = expect.expects(),
                .kind = kind,
            } },
            self.ast.getSpan(expr).start,
        ),
    };
}

fn matchValueArms(
    self: *Self,
    analyzer: *Matcher,
    ty: *const Type,
    expr: Ast.Match.ValueMatch,
    kw: Ast.TokenIndex,
    expect: ExprResKind,
    ctx: *Context,
) Error!Matcher.MatchArms {
    var types: Set(*const Type) = .empty;
    const len = expr.arms.len + @intFromBool(expr.wildcard != null);
    var arms_instr = ArrayList(Instr.Match.Arm).initCapacity(self.allocator, len) catch oom();

    const prev_decl_type = ctx.setAndGetPrevious(.decl_type, ty);
    defer ctx.decl_type = prev_decl_type;

    for (expr.arms) |*arm| {
        const arm_res, const body_res = try analyzer.arm(self, arm, expect, ctx);

        if (arm_res.type != ty) return self.err(
            .{ .type_mismatch = .{ .expect = self.typeName(ty), .found = self.typeName(arm_res.type) } },
            self.ast.getSpan(arm.expr),
        );

        types.add(self.allocator, body_res.type) catch oom();
        arms_instr.appendAssumeCapacity(.{ .expr = arm_res.instr, .body = body_res.instr });
    }

    const wildcard = if (expr.wildcard) |*w| w: {
        const res = try analyzer.wildcard(self, w, expect, ctx);
        types.add(self.allocator, res.type) catch oom();
        break :w res.instr;
    } else null;

    try analyzer.validate(self, self.ast.getSpan(kw));

    return .{
        .types = types.toOwned(),
        .arms = arms_instr.toOwnedSlice(self.allocator) catch oom(),
        .wildcard = wildcard,
    };
}

const Pattern = struct {
    bindings: []const struct { name: InternerIdx, type: *const Type },
};

fn pattern(self: *Self, pat: Ast.Pattern, ctx: *Context) Result {
    switch (pat) {
        .value => |v| {
            const value_res = try self.analyzeExpr(v.expr, .value, ctx);
            if (v.alias) |alias| {
                const binding = try self.internIfNotInCurrentScope(alias);
                _ = try self.forwardDeclareVariable(binding, value_res.type, false, self.ast.getSpan(alias));
            }

            return value_res;
        },
        .nullable => |v| {
            return self.nullablePattern(v, ctx);
        },
    }
}

fn nullablePattern(self: *Self, pat: Ast.Pattern.Nullable, ctx: *Context) Result {
    const span = self.ast.getSpan(pat.expr);
    const expr_ty = try self.analyzeExpr(pat.expr, .value, ctx);

    const ty = expr_ty.type.as(.optional) orelse return self.err(
        .{ .pat_null_non_optional = .{ .found = self.typeName(expr_ty.type) } },
        span,
    );

    // TODO: be sure that it's in the correct scope
    const binding = try self.internIfNotInCurrentScope(pat.binding);
    _ = try self.forwardDeclareVariable(binding, ty, false, self.ast.getSpan(pat.binding));

    return .{
        .type = self.ti.cache.bool,
        .instr = self.irb.addInstr(.{ .pat_nullable = expr_ty.instr }, span.start),
    };
}

pub fn range(self: *Self, expr: Ast.Binop, ctx: *Context) Result {
    const start = try self.analyzeExpr(expr.lhs, .value, ctx);
    const end = try self.analyzeExpr(expr.rhs, .value, ctx);

    try self.validateRange(start.type, end.type, self.ast.getSpan(expr.lhs));

    return .{
        .type = self.ti.intern(.{ .range = start.type }),
        .ti = .{ .comp_time = start.ti.comp_time and end.ti.comp_time },
        .instr = self.irb.addInstr(
            .{ .range = .{
                .start = start.instr,
                .end = end.instr,
                .kind = if (start.type.is(.int)) .int else .float,
            } },
            self.ast.getSpan(expr).start,
        ),
    };
}

fn validateRange(self: *Self, low: *const Type, high: *const Type, span: Span) Error!void {
    if (!low.is(.int) and !low.is(.float)) {
        return self.err(.{ .range_non_num = .{ .found = self.typeName(low) } }, span);
    }
    if (!high.is(.int) and !high.is(.float)) {
        return self.err(.{ .range_non_num = .{ .found = self.typeName(high) } }, span);
    }
    if (low != high) {
        return self.err(.range_mix_int_float, span);
    }
}

fn returnExpr(self: *Self, expr: *const Ast.Return, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);
    const fn_type = ctx.fn_type orelse return self.err(.return_outside_fn, span);
    const ty = fn_type.function.return_type;

    const exp = expr.expr orelse return .{
        .type = self.ti.getCached(.never),
        .cf = .@"return",
        .instr = self.irb.addInstr(.{ .@"return" = .{ .value = null } }, span.start),
    };

    var value_res = try self.analyzeExpr(exp, .value, ctx);

    // We can't return an error with 'return' unless we're defining a method on the error's enum
    check: {
        if (!value_res.type.isErr()) break :check;
        if (ctx.self_type) |self_type| if (self_type == value_res.type) break :check;

        return self.err(
            .{ .error_with_return = .{ .found = self.typeName(value_res.type) } },
            span,
        );
    }

    if (ty != value_res.type) {
        const err_span = if (expr.expr) |e| self.ast.getSpan(e) else span;
        value_res.type = try self.performTypeCoercion(ty, value_res.type, true, err_span);
    }

    return .{
        .type = value_res.type,
        .cf = .@"return",
        .instr = self.irb.addInstr(.{ .@"return" = .{ .value = value_res.instr } }, span.start),
    };
}

fn structLiteral(self: *Self, expr: *const Ast.StructLiteral, ctx: *Context) Result {
    const span = self.ast.getSpan(expr.structure);
    const struct_res = self.analyzeExpr(expr.structure, .symbol, ctx) catch |e| switch (e) {
        error.NotSymbol => return self.err(.non_struct_struct_literal, span),
        else => return e,
    };
    var comp_time = struct_res.ti.comp_time;

    const struct_type = struct_res.type.as(.structure) orelse return self.err(.non_struct_struct_literal, span);

    var proto = struct_type.proto(self.allocator);
    defer proto.deinit(self.allocator);

    var values = self.allocator.alloc(Instr.Arg, struct_type.fields.count()) catch oom();

    for (struct_type.fields.values(), 0..) |f, i| {
        if (f.default) |def| {
            values[i] = .{ .default = .{ .const_index = def, .mod = struct_res.ti.ext_mod } };
        }
    }

    for (expr.fields) |*fv| {
        const field_span = self.ast.getSpan(fv.name);
        const field_name = self.interner.intern(self.ast.toSource(fv.name));

        const f = struct_type.fields.get(field_name) orelse return self.err(
            .{ .unknown_struct_field = .{ .name = self.ast.toSource(fv.name) } },
            field_span,
        );
        const field_index = struct_type.fields.getIndex(field_name).?;

        const gop = proto.getOrPutAssumeCapacity(field_name);
        if (gop.value_ptr.done) {
            return self.err(.{ .duplicate_field = .{ .name = self.interner.getKey(field_name).? } }, field_span);
        }
        gop.value_ptr.done = true;

        var res: InstrInfos = if (fv.value) |value|
            try self.analyzeExpr(value, .value, ctx)
        else b: {
            // Syntax: { x } instead of { x = x }
            const res = try self.expectVariableIdentifier(fv.name);
            break :b .{
                .type = res.variable.type,
                .ti = .{ .heap = res.variable.type.isHeap(), .comp_time = res.variable.comp_time },
                .instr = res.instr,
            };
        };

        comp_time = comp_time and res.ti.comp_time;
        const value_span = if (fv.value) |val| self.ast.getSpan(val) else field_span;
        _ = try self.performTypeCoercion(f.type, res.type, false, value_span);

        self.checkWrap(&res.instr, res.ti.heap);
        values[field_index] = .{ .instr = res.instr };
    }

    // Check for non-completed prototype
    const err_count = self.errs.items.len;

    for (proto.keys(), proto.values()) |k, v| {
        if (v.done or v.default != null) continue;
        self.err(.{ .missing_field_struct_literal = .{ .name = self.interner.getKey(k).? } }, span) catch {};
    }

    if (self.errs.items.len > err_count) return error.Err;

    return .{
        .type = struct_res.type,
        .ti = .{ .comp_time = comp_time, .ext_mod = struct_res.ti.ext_mod },
        .instr = self.irb.addInstr(
            .{ .struct_literal = .{ .structure = struct_res.instr, .values = values } },
            span.start,
        ),
    };
}

fn ternary(self: *Self, expr: *const Ast.Ternary, ctx: *Context) Result {
    const condition = try self.analyzeExpr(expr.condition, .value, ctx);

    if (!condition.type.is(.bool)) return self.err(
        .{ .ternary_cond_non_bool = .{ .found = self.typeName(condition.type) } },
        self.ast.getSpan(expr.condition),
    );

    const then = try self.analyzeExpr(expr.then, .value, ctx);
    const @"else" = try self.analyzeExpr(expr.@"else", .value, ctx);

    return .{
        .type = self.mergeTypes(&.{ then.type, @"else".type }),
        .instr = self.irb.addInstr(
            .{ .@"if" = .{ .cond = condition.instr, .then = then.instr, .@"else" = @"else".instr } },
            self.ast.getSpan(expr).start,
        ),
    };
}

fn trap(self: *Self, expr: Ast.Trap, ctx: *Context) Result {
    const lhs = try self.analyzeExpr(expr.lhs, .value, ctx);
    const err_type = lhs.type.as(.error_union) orelse return self.err(
        .{ .trap_on_non_error = .{ .found = self.typeName(lhs.type) } },
        self.ast.getSpan(expr.lhs),
    );

    const rhs = switch (expr.rhs) {
        .match => |m| try self.trapMatch(m, err_type, ctx),
        .binding => |b| try self.trapIdent(b, err_type, ctx),
    };

    return .{
        .type = rhs.type,
        .instr = self.irb.addInstr(.{ .trap = .{ .lhs = lhs.instr, .rhs = rhs.instr } }, self.ast.getSpan(expr).start),
    };
}

fn trapIdent(self: *Self, binding: Ast.Trap.Binding, err_type: Type.ErrorUnion, ctx: *Context) Result {
    if (binding.token) |token| {
        const err_name = try self.internIfNotInCurrentScope(token);
        const binding_span = self.ast.getSpan(token);
        _ = try self.forwardDeclareVariable(err_name, err_type.err, false, binding_span);
    }
    return self.analyzeExpr(binding.body, .value, ctx);
}

fn trapMatch(self: *Self, expr: *Expr, err_type: Type.ErrorUnion, ctx: *Context) Result {
    const match_expr = expr.match.expr;

    if (match_expr.* != .identifier) {
        return self.err(.trap_match_not_ident, self.ast.getSpan(match_expr));
    }

    const err_name = try self.internIfNotInCurrentScope(match_expr.identifier);
    const binding_span = self.ast.getSpan(match_expr.identifier);

    // If it's a `trap match`, we open another scope to discard the error at the end
    self.scope.open(self.allocator, null, .{ .exp_val = true });
    defer _ = self.scope.close();

    _ = try self.declareVariable(err_name, err_type.err, false, true, true, false, null, binding_span);
    return self.analyzeExpr(expr, .value, ctx);
}

fn unary(self: *Self, expr: *const Ast.Unary, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);
    const op = self.ast.token_tags[expr.op];

    const rhs = switch (expr.expr.*) {
        .int => |i| return self.intLit(i, true),
        .float => |f| return self.floatLit(f, true),
        else => try self.analyzeExpr(expr.expr, .value, ctx),
    };
    const ty = rhs.type;

    if (op == .not and !ty.is(.bool)) {
        return self.err(.{ .invalid_unary = .{ .found = self.typeName(ty) } }, span);
    }
    if (op == .minus and !ty.isNumeric()) {
        return self.err(.{ .invalid_arithmetic = .{ .found = self.typeName(ty) } }, span);
    }

    return .{
        .type = rhs.type,
        .ti = rhs.ti,
        .instr = self.irb.addInstr(
            .{ .unary = .{
                .op = if (op == .not) .bang else .minus,
                .typ = if (ty.is(.int)) .int else .float,
                .instr = rhs.instr,
            } },
            span.start,
        ),
    };
}

fn when(self: *Self, expr: *const Ast.When, expect: ExprResKind, ctx: *Context) Result {
    const value = try self.analyzeExpr(expr.expr, .value, ctx);

    const value_type = value.type.as(.@"union") orelse return self.err(
        .{ .when_with_non_union = .{ .found = self.typeName(value.type) } },
        self.ast.getSpan(expr.expr),
    );
    var proto = value_type.proto(self.allocator);

    const types, const arms = try self.whenArms(expr.arms, expr.alias, value.type, &proto, expect, ctx);
    try self.whenValidation(&proto, self.ast.getSpan(expr.kw));

    return .{
        .type = self.mergeTypes(types),
        .instr = self.irb.addInstr(
            .{ .when = .{ .expr = value.instr, .arms = arms, .is_expr = expect.expects() } },
            self.ast.getSpan(expr).start,
        ),
    };
}

const WhenArmsRes = struct { []const *const Type, []const Instr.When.Arm };

fn whenArms(
    self: *Self,
    arm_exprs: []const Ast.When.Arm,
    glob_alias: ?Ast.TokenIndex,
    ty: *const Type,
    proto: *Type.Union.Proto,
    expect: ExprResKind,
    ctx: *Context,
) Error!WhenArmsRes {
    var had_err = false;
    var types: Set(*const Type) = .empty;
    var arms = ArrayList(Instr.When.Arm).initCapacity(self.allocator, arm_exprs.len) catch oom();

    for (arm_exprs) |*arm| {
        const arm_span = self.ast.getSpan(arm.type);
        const arm_ty = try self.checkAndGetType(arm.type, ctx);

        const resolved_ty = self.findTypeInProto(proto, ty, arm_ty, arm_span) catch continue;

        // Implicit scope for aliases
        self.scope.open(self.allocator, null, false, expect == .value);
        defer _ = self.scope.close();

        alias: {
            const alias = glob_alias orelse break :alias;
            const binding = try self.internIfNotInCurrentScope(alias);
            _ = try self.declareVariable(binding, resolved_ty, false, true, true, false, null, self.ast.getSpan(alias));
        }

        const body = self.analyzeNode(&arm.body, expect, ctx) catch {
            had_err = true;
            continue;
        };

        types.add(self.allocator, body.type) catch oom();
        arms.appendAssumeCapacity(.{ .type_id = self.ti.typeId(resolved_ty), .body = body.instr });
    }

    return if (had_err) error.Err else .{ types.toOwned(), arms.toOwnedSlice(self.allocator) catch oom() };
}

fn findTypeInProto(self: *Self, proto: *Type.Union.Proto, union_ty: *const Type, ty: *const Type, span: Span) Error!*const Type {
    const gop = proto.getEntry(ty) orelse gop: {
        // We might not be able to find the entry if we're comparing anonymus function with a declared one
        if (ty.is(.function)) {
            var it = proto.iterator();
            while (it.next()) |entry| {
                if (entry.value_ptr.*) continue;

                const fn_ty = entry.key_ptr.*;
                if (fn_ty.* == .function) {
                    _ = self.checkFunctionEq(fn_ty, ty) catch continue;
                    break :gop entry;
                }
            }
        }

        return self.err(
            .{ .when_arm_not_in_union = .{ .found = self.typeName(ty), .expect = self.typeName(union_ty) } },
            span,
        );
    };

    if (gop.value_ptr.*) {
        return self.err(.when_arm_duplicate, span);
    }
    gop.value_ptr.* = true;

    return gop.key_ptr.*;
}

fn whenValidation(self: *Self, proto: *const Type.Union.Proto, span: Span) Error!void {
    var missing: ArrayList(u8) = .empty;

    var it = proto.iterator();
    while (it.next()) |entry| {
        if (!entry.value_ptr.*) {
            if (missing.items.len > 0) {
                missing.appendSlice(self.allocator, ", ") catch oom();
            }
            missing.appendSlice(self.allocator, self.typeName(entry.key_ptr.*)) catch oom();
        }
    }

    if (missing.items.len > 0) return self.err(
        .{ .match_non_exhaustive = .{ .kind = "when", .missing = missing.toOwnedSlice(self.allocator) catch oom() } },
        span,
    );
}

/// Checks if identifier name is already declared, otherwise interns it and returns the key
fn internIfNotInCurrentScope(self: *Self, token: usize) Error!usize {
    const name = self.interner.intern(self.ast.toSource(token));

    if (self.scope.isVarOrSymInCurrentScope(name)) return self.err(
        .{ .already_declared = .{ .name = self.interner.getKey(name).? } },
        self.ast.getSpan(token),
    );

    return name;
}

/// Checks that the node is a declared type and return it's value. If node is `.empty`, returns `void`
fn checkAndGetType(self: *Self, ty: ?*const Ast.Type, ctx: *const Context) Error!*const Type {
    const t = ty orelse return self.ti.getCached(.void);

    return switch (t.*) {
        .array => |arr_type| {
            const child = try self.checkAndGetType(arr_type.child, ctx);

            if (child.is(.void)) {
                return self.err(.void_array, self.ast.getSpan(arr_type.child));
            }

            return self.ti.intern(.{ .array = .{ .child = child } });
        },
        .error_union => |err_union| {
            const ok = try self.checkAndGetType(err_union.ok, ctx);

            var errs = ArrayList(*const Type).initCapacity(self.allocator, err_union.errs.len) catch oom();
            for (err_union.errs) |e| {
                errs.appendAssumeCapacity(try self.checkAndGetType(&.{ .scalar = e }, ctx));
            }

            return self.ti.intern(.{ .error_union = .{
                .ok = ok,
                .err = self.ti.intern(.{ .@"union" = .{ .types = errs.toOwnedSlice(self.allocator) catch oom() } }),
            } });
        },
        .fields => |fields| {
            // TODO: Error
            if (fields.len > 2) @panic("Nested types are not supported yet");

            const module_token = fields[0];
            const module_infos = try self.resolveIdentifier(module_token, true, ctx);
            const module_type = module_infos.type;

            if (!module_type.is(.module)) return self.err(
                .{ .dot_type_on_non_mod = .{ .found = self.typeName(module_type) } },
                self.ast.getSpan(module_token),
            );

            // If `identifier` returned no error and it's a module, safe unwrap
            const module = self.pipeline.state.module_interner.getAnalyzed(module_type.module).?;

            const symbol_token = fields[1];
            const symbol_name = self.interner.intern(self.ast.toSource(symbol_token));
            const final = module.symbols.get(symbol_name) orelse return self.err(
                .{ .missing_symbol_in_module = .{
                    .module = self.ast.toSource(module_token),
                    .symbol = self.ast.toSource(symbol_token),
                } },
                self.ast.getSpan(symbol_token),
            );

            return final.type;
        },
        .function => |func| {
            var params: AutoArrayHashMapUnmanaged(InternerIdx, Type.Function.Parameter) = .{};
            for (func.params, 0..) |p, i| {
                const p_type = try self.checkAndGetType(p, ctx);
                params.put(self.allocator, i, .{ .name = null, .type = p_type, .default = null, .captured = false }) catch oom();
            }

            return self.ti.intern(.{ .function = .{
                .loc = null,
                .params = params,
                .return_type = try self.checkAndGetType(func.return_type, ctx),
                .kind = .normal,
            } });
        },
        .optional => |opt| {
            const child = try self.checkAndGetType(opt.child, ctx);
            return self.ti.intern(.{ .optional = child });
        },
        .scalar => {
            const interned = self.interner.intern(self.ast.toSource(t));

            const found_type = self.scope.getType(interned) orelse {
                if (interned == self.cached_names.Self) {
                    if (ctx.self_type) |struct_type| {
                        return struct_type;
                    } else {
                        return self.err(.big_self_outside_decl, self.ast.getSpan(t));
                    }
                } else {
                    return self.err(.{ .undeclared_type = .{ .found = self.ast.toSource(t) } }, self.ast.getSpan(t));
                }
            };

            return found_type;
        },
        .@"union" => |u| {
            var types = ArrayList(*const Type).initCapacity(self.allocator, u.len) catch oom();

            types.ensureTotalCapacity(self.allocator, u.len) catch oom();
            for (u) |child| {
                types.appendAssumeCapacity(try self.checkAndGetType(child, ctx));
            }

            return self.ti.intern(.{ .@"union" = .{ .types = types.toOwnedSlice(self.allocator) catch oom() } });
        },
        .self => if (ctx.self_type) |struct_type| struct_type else self.err(.self_outside_decl, self.ast.getSpan(t)),
    };
}

/// Given a slice a types, creates an union or return a scalar type if slice length is one
fn mergeTypes(self: *Self, types: []const *const Type) *const Type {
    var set = Set(*const Type).fromSlice(self.allocator, types) catch oom();

    // Void can be in union, it just means a path, branch or value didn't produce anything
    _ = set.remove(self.ti.getCached(.void));

    if (set.count() == 0) return self.ti.getCached(.void);

    const optional = set.remove(self.ti.getCached(.null));
    const ty = if (set.count() == 1) set.keys()[0] else self.ti.intern(.{ .@"union" = .{ .types = set.toOwned() } });

    return if (optional) self.ti.intern(.{ .optional = ty }) else ty;
}

/// Checks for `void` values, array inference, cast and function type generation
/// The goal is to see if the two types are equivalent and if so, make the transformations needed
fn performTypeCoercion(self: *Self, decl: *const Type, value: *const Type, decl_explicit_void: bool, span: Span) Error!*const Type {
    if (decl == value) return decl;

    if (decl.is(.error_union)) {
        return self.performErrorCoercion(decl, value, span);
    }

    if (value.is(.null)) {
        return if (decl.is(.optional))
            decl
        else
            self.err(.{ .null_assign_to_non_optional = .{ .expect = self.typeName(decl) } }, span);
    }

    // If this is 'never', it means we ended with a control flow in which all branches returned
    // In that case, the type has already been tested against function's type
    if (value.is(.never)) return decl;

    if (decl.is(.optional)) {
        _ = try self.performTypeCoercion(decl.optional, if (value.is(.optional)) value.optional else value, decl_explicit_void, span);
        return decl;
    }

    check: {
        if (value.is(.array)) {
            return self.checkArrayType(decl, value, span) catch |e| switch (e) {
                error.mismatch => break :check,
                else => |narrowed| return narrowed,
            };
        } else if (decl.is(.@"union")) {
            return checkUnionType(decl, value) catch |e| return switch (e) {
                error.Mismatch => break :check,
                error.NotInUnion => self.err(
                    .{ .type_not_in_union = .{ .expect = self.typeName(decl), .found = self.typeName(value) } },
                    span,
                ),
            };
        } else if (value.is(.function)) {
            return self.checkFunctionEq(decl, value) catch break :check;
        }

        // We check after the other because above checks need the information about a potential void declaration
        if (decl.is(.void)) {
            // If a void in declaration is an explicit expected type, we can't allow any value
            // Used for example when a function doesn't declare any return type
            if (decl_explicit_void and !value.is(.void)) break :check;
            return value;
        }
    }

    return self.err(
        .{ .type_mismatch = .{ .expect = self.typeName(decl), .found = self.typeName(value) } },
        span,
    );
}

fn performErrorCoercion(self: *Self, decl: *const Type, value: *const Type, span: Span) Error!*const Type {
    const error_union = decl.error_union;

    if (value.isErr()) {
        switch (error_union.err.*) {
            .@"union" => |u| {
                for (u.types) |union_err| {
                    if (union_err == value) return decl;
                }
            },
            else => |*t| if (t == value) return decl,
        }

        return self.err(.{ .error_not_in_union = .{
            .found = self.typeName(value),
            .expect = self.typeName(error_union.err),
        } }, span);
    } else {
        return self.performTypeCoercion(error_union.ok, value, false, span);
    }
}

/// Checks if two different pointers to function type are equal, due to anonymus ones
/// Assumes that types are functions
fn checkFunctionEq(self: *Self, decl: *const Type, value: *const Type) Error!*const Type {
    // Functions function's return types like: 'fn add() -> fn(int) -> int' don't have a declaration
    // There is also the case when assigning to a variable and infering type like: var bound = foo.method
    // Here, we want `bound` to be an anonymus function, it loses all declaration infos because it's a runtime value
    // TODO: put outside to centralize all the void decl -> infer from value mechanism
    if (decl.is(.void)) return if (value.function.loc != null)
        self.ti.intern(.{ .function = value.function.toAnon(self.allocator) })
    else
        value;

    const f1 = decl.function;
    const f2 = value.function;

    check: {
        if (f1.loc != null and f2.loc != null or f1.params.count() != f2.params.count()) break :check;
        if (f1.return_type != f2.return_type) break :check;

        for (f1.params.values(), f2.params.values()) |p1, p2| {
            if (p1.type != p2.type) break :check;
        }

        return decl;
    }

    return error.Err;
}
/// Checks if two different types (with at least one of them being an unoion) fits in one or the other
/// Assumes `decl` is an union
fn checkUnionType(decl: *const Type, value: *const Type) error{ Mismatch, NotInUnion }!*const Type {
    // TODO: put the check void on decl in the caller
    if (decl.is(.void)) {
        return value;
    }

    // Only declaration is an union
    if (!value.is(.@"union")) {
        if (decl.@"union".contains(value)) {
            return decl;
        }
    }
    // Value is an union but not declaration
    else if (!decl.is(.@"union")) {
        return error.Mismatch;
    }
    // Both are unions
    else if (decl.@"union".containsSubset(&value.@"union")) {
        return decl;
    }

    return error.NotInUnion;
}

/// Try to infer array value type from variable's declared type
/// Assumes `value` is an array
fn checkArrayType(self: *Self, decl: *const Type, value: *const Type, span: Span) (Error || error{mismatch})!*const Type {
    const depth_value, const child_value = value.array.depthAndChild();

    check: {
        if (decl.is(.void)) {
            // Empty array like: []
            if (child_value.is(.void)) {
                // No type declared and empty array like: var a = [], else infer from declaration
                return self.err(.cant_infer_array_type, span);
            }
        } else {
            if (!decl.is(.array)) break :check;
            const depth_decl, const child_decl = decl.array.depthAndChild();

            const current_decl = if (child_decl.is(.optional)) child_decl.optional else child_decl;

            if (depth_value != depth_decl) break :check;

            if (!child_value.is(.void)) {
                // Additional check for cases like: var a: []int|float = [1, 2, 3]
                if (current_decl.as(.@"union")) |*u| {
                    return if (self.checkArrayOfUnion(u, value)) decl else error.mismatch;
                }
                if (child_value != current_decl) break :check;
            }

            return decl;
        }

        return value;
    }

    return error.mismatch;
}

fn checkArrayOfUnion(self: *Self, decl: *const Type.Union, value: *const Type) bool {
    for (decl.types) |ty| {
        if (self.ti.intern(.{ .array = .{ .child = ty } }) == value) return true;
    }
    return false;
}

pub fn typeName(self: *const Self, ty: *const Type) []const u8 {
    return ty.toString(self.allocator, self.interner, self.mod_name);
}

fn declareVariable(
    self: *Self,
    name: InternerIdx,
    ty: *const Type,
    captured: bool,
    initialized: bool,
    constant: bool,
    comp_time: bool,
    ext_mod: ?usize,
    span: Span,
) Error!usize {
    return self.scope.declareVar(
        self.allocator,
        name,
        ty,
        captured,
        initialized,
        constant,
        comp_time,
        ext_mod,
    ) catch self.err(.too_many_locals, span);
}

fn forwardDeclareVariable(self: *Self, name: InternerIdx, ty: *const Type, captured: bool, span: Span) Error!void {
    return self.scope.declareVarInFutureScope(self.allocator, name, ty, captured) catch self.err(.too_many_locals, span);
}

fn openContainer(self: *Self, name: Ast.TokenIndex) Error!void {
    self.scope.open(self.allocator, null, .{ .barrier = true });
    self.containers.append(self.allocator, self.ast.toSource(name));
}

fn closeContainer(self: *Self) void {
    _ = self.scope.close();
    _ = self.containers.pop();
}

/// Tries to fetch a constant value from either a `constant` instruction or a compile time literal bound to an identifier
pub fn tryGetConstant(self: *Self, index: ir.Index) ?Instr.Data {
    return switch (self.irb.getInstr(index)) {
        .constant => self.irb.getConstant(index),
        .identifier => |i| {
            const scope = switch (i.scope) {
                .local => self.scope.current,
                .global => &self.scope.scopes.items[0],
                .builtin => unreachable,
            };
            const ident = scope.variables.values()[i.index];

            std.log.debug("Comptime: {}", .{ident.comp_time});

            @panic("TODO");
        },
        else => null,
    };
}
