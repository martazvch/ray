const std = @import("std");
const Allocator = std.mem.Allocator;

const options = @import("options");

const State = @import("State.zig");
const Compiler = @import("../compiler/compiler.zig").Compiler;
const CompiledModule = @import("../compiler/compiler.zig").CompiledModule;
const CompilationUnit = @import("../compiler/compiler.zig").CompilationUnit;
const Analyzer = @import("../analyzer/Analyzer.zig");
const AnalyzerMsg = @import("../analyzer/analyzer_msg.zig").AnalyzerMsg;
const Ast = @import("../parser/Ast.zig");
const AstRender = @import("../parser/AstRender.zig");
const Walker = @import("../parser/AstWalker.zig");
const Lexer = @import("../parser/Lexer.zig");
const LexerMsg = @import("../parser/lexer_msg.zig").LexerMsg;
const LexicalScope = @import("../analyzer/LexicalScope.zig");
const Parser = @import("../parser/Parser.zig");
const ParserMsg = @import("../parser/parser_msg.zig").ParserMsg;
const IrRenderer = @import("../analyzer/IrRenderer.zig");
const Irb = @import("../analyzer/IrBuilder.zig");
const Obj = @import("../runtime/Obj.zig");

const misc = @import("misc");
const reportAll = misc.reporter.reportAll;
const oom = misc.oom;

const Error = error{ExitOnPrint};

/// Runs the pipeline
// TODO: could only need full path
pub fn run(
    allocator: Allocator,
    state: *State,
    is_sub: bool,
    file_name: []const u8,
    path: []const u8,
    source: [:0]const u8,
) !*Obj.Function {
    // Initiliaze the path builder
    state.path_builder.append(allocator, std.fs.cwd().realpathAlloc(allocator, ".") catch oom());

    const ast = try parse(allocator, state, file_name, source);

    const mod_name = file_name[0 .. file_name.len - 4];
    const mod_index = state.modules.open(
        allocator,
        state.interner.intern(path),
        state.interner.intern(mod_name),
    );

    var analyzer: Analyzer = .init(allocator, state);
    analyzer.analyze(&ast, mod_name, !is_sub);

    // Analyzed Ast printer
    if (analyzer.warns.items.len > 0) {
        try reportAll(AnalyzerMsg, analyzer.warns.items, !options.test_mode, file_name, source);
    }
    if (analyzer.errs.items.len > 0) {
        try reportAll(AnalyzerMsg, analyzer.errs.items, !options.test_mode, file_name, source);
        return error.ExitOnPrint;
    }
    if (state.config.print_ir) {
        try printIr(allocator, state, file_name, &analyzer);
        if (options.test_mode and !is_sub) return error.ExitOnPrint;
    }

    state.updateModWithScope(allocator, mod_index);
    state.modules.ensureCompileSizes(
        allocator,
        mod_index,
        state.lex_scope.current.variables.count(),
        state.lex_scope.symbol_count,
        analyzer.irb.const_interner.constants.items.len,
    );

    // Compiler
    var compiler = CompilationUnit.init(
        allocator,
        state,
        mod_index,
        analyzer.irb.const_interner.constants.items,
        state.native_reg.funcs.items,
        if (!state.config.print_bytecode) .none else if (options.test_mode) .@"test" else .normal,
    );

    const entry_point = try compiler.compile(
        analyzer.irb.instructions.items(.data),
        analyzer.irb.roots.items,
        analyzer.irb.computeLineFromOffsets(source),
        analyzer.main,
    );

    return if (options.test_mode and state.config.print_bytecode and !is_sub)
        error.ExitOnPrint
    else
        entry_point;
}

pub fn runFrontend(allocator: Allocator, state: *State, is_sub: bool, file_name: []const u8, source: [:0]const u8) !struct {
    []const LexicalScope.Scope,
    []const LexicalScope.Symbol,
    Irb,
} {
    // Initiliaze the path builder
    state.path_builder.append(allocator, std.fs.cwd().realpathAlloc(allocator, ".") catch oom());

    const ast = try parse(allocator, state, file_name, source);

    var analyzer: Analyzer = .init(allocator, state);
    _ = analyzer.analyze(&ast, file_name, !is_sub);

    // Analyzed Ast printer
    if (analyzer.warns.items.len > 0) {
        try reportAll(AnalyzerMsg, analyzer.warns.items, !options.test_mode, file_name, source);
    }
    if (analyzer.errs.items.len > 0) {
        try reportAll(AnalyzerMsg, analyzer.errs.items, !options.test_mode, file_name, source);
        return error.ExitOnPrint;
    }
    if (state.config.print_ir) {
        try printIr(allocator, state, file_name, &analyzer);
        if (options.test_mode and !is_sub) return error.ExitOnPrint;
    }

    var scopes = std.ArrayList(LexicalScope.Scope).initCapacity(allocator, analyzer.scope.saved.items.len) catch oom();

    for (analyzer.scope.saved.items, 0..) |scope, i| {
        // TODO: error
        if (scope == .tombstone) {
            std.log.debug("Found a tombstone at: {}", .{i});
            @panic("Invalid state");
        }
        scopes.appendAssumeCapacity(scope.scope);
    }

    var symbols = std.ArrayList(LexicalScope.Symbol).initCapacity(allocator, analyzer.scope.saved_syms.items.len) catch oom();

    for (analyzer.scope.saved_syms.items) |symbol| {
        symbols.appendAssumeCapacity(symbol.*);
    }

    return .{
        scopes.toOwnedSlice(allocator) catch oom(),
        symbols.toOwnedSlice(allocator) catch oom(),
        analyzer.irb,
    };
}

pub fn parse(allocator: Allocator, state: *State, file_name: []const u8, source: [:0]const u8) !Ast {
    var lexer = Lexer.init(allocator);
    lexer.lex(source);
    defer lexer.deinit();

    if (lexer.errs.items.len > 0) {
        try reportAll(LexerMsg, lexer.errs.items, !options.test_mode, file_name, source);
        return error.ExitOnPrint;
    }

    // Parser
    const token_slice = lexer.tokens.toOwnedSlice();
    var parser: Parser = .init(allocator);
    var ast = parser.parse(source, token_slice.items(.tag), token_slice.items(.span));

    var walker: Walker = .init(allocator, &state.interner, &ast);
    walker.walk();

    if (parser.errs.items.len > 0) {
        try reportAll(ParserMsg, parser.errs.items, !options.test_mode, file_name, source);
        return error.ExitOnPrint;
    } else if (state.config.print_ast) {
        try printAst(allocator, &ast);
        if (options.test_mode) return error.ExitOnPrint;
    }

    return ast;
}

pub fn runSubPipeline(allocator: Allocator, state: *State, file_name: []const u8, path: []const u8, source: [:0]const u8) void {
    const prev_scope = state.lex_scope;
    state.lex_scope = .empty;
    state.lex_scope.initGlobalScope(allocator, state);

    _ = run(allocator, state, true, file_name, path, source) catch {
        std.process.exit(0);
    };

    state.lex_scope = prev_scope;
}

fn printAst(allocator: Allocator, ast: *const Ast) !void {
    var buf: [2048]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&buf);
    const stdout = &stdout_writer.interface;
    defer stdout.flush() catch unreachable;

    var renderer: AstRender = .init(allocator, ast);
    try stdout.writeAll(try renderer.render());
}

fn printIr(allocator: Allocator, state: *const State, file_name: []const u8, analyzer: *const Analyzer) !void {
    var buf: [2048]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&buf);
    const stdout = &stdout_writer.interface;
    defer stdout.flush() catch unreachable;

    var ir_renderer = IrRenderer.init(
        allocator,
        analyzer.irb.instructions.items(.data),
        analyzer.irb.const_interner.constants.items,
        &state.interner,
    );
    try stdout.writeAll(try ir_renderer.renderIr(file_name, analyzer.irb.roots.items));
}
