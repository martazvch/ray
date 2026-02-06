const std = @import("std");
const Allocator = std.mem.Allocator;

const type_mod = @import("../analyzer/types.zig");
const TypeInterner = type_mod.TypeInterner;
const ObjFns = type_mod.ObjFns;
const Obj = @import("../runtime/Obj.zig");
const ModuleManager = @import("ModuleManager.zig");
const NativeRegister = @import("NativesRegister.zig");
const LexScope = @import("../analyzer/LexicalScope.zig");
const ModIndex = @import("../pipeline/ModuleManager.zig").Index;
const ConstInterner = @import("../analyzer/ConstantInterner.zig");
const ConstIdx = ConstInterner.ConstIdx;
const Constant = ConstInterner.Constant;
const ffi = @import("../builtins/ffi.zig");

const misc = @import("misc");
const Interner = misc.Interner;
const Sb = misc.StringBuilder;

config: Config,
interner: Interner,
type_interner: TypeInterner,
const_interner: ConstInterner,
path_builder: Sb,
lex_scope: LexScope,
modules: ModuleManager,
native_reg: NativeRegister,
strings: std.AutoHashMapUnmanaged(usize, *Obj.String),
array_fns: ObjFns,
string_fns: ObjFns,

const Self = @This();

pub const Config = struct {
    embedded: bool = false,
    print_ast: bool = false,
    print_bytecode: bool = false,
    static_analyzis: bool = false,
    print_ir: bool = false,
    dbg_infos: bool = false,

    printFn: *const fn ([]const u8) void = defaultPrint,
    // errorFn: *const fn ([]const u8) void = defaultErr,
};

fn defaultPrint(text: []const u8) void {
    errdefer @panic("failed to write to stdout");

    var buf: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&buf);
    const stdout = &stdout_writer.interface;

    try stdout.print("{s}\n", .{text});
    try stdout.flush();
}

// fn defaultErr(text: []const u8) void {
//     errdefer @panic("failed to write to stderr");
//
//     var buf: [1024]u8 = undefined;
//     var stderr_writer = std.fs.File.stderr().writer(&buf);
//     const stderr = &stderr_writer.interface;
//
//     try stderr.print("{s}\n", .{text});
//     try stderr.flush();
// }

pub fn new(allocator: Allocator, config: Config, comptime natives: []const ffi.ZigFnMeta) Self {
    var ctx: Self = .{
        .config = config,
        .interner = .init(allocator),
        .type_interner = .init(allocator),
        .const_interner = .init(allocator),
        .path_builder = .empty,
        .lex_scope = .empty,
        .modules = .empty,
        .native_reg = .empty,
        .strings = .empty,
        .array_fns = Obj.Array.getFns(),
        .string_fns = Obj.String.getFns(),
    };

    ctx.type_interner.cacheFrequentTypes();
    ctx.registerMod(allocator, @import("../builtins/builtins.zig"));
    ctx.registerMod(allocator, @import("..//builtins/file.zig"));

    inline for (natives) |*native| {
        _ = ctx.native_reg.registerFn(allocator, native, &ctx.interner, &ctx.type_interner);
    }

    ctx.lex_scope.save = config.dbg_infos;
    ctx.lex_scope.initGlobalScope(allocator, &ctx);

    // If embedded/Repl, all code is treated as local code to allow impur code
    if (ctx.config.embedded) {
        ctx.lex_scope.open(allocator, null, .{ .barrier = true });
    }

    return ctx;
}

pub fn registerMod(self: *Self, allocator: Allocator, Module: type) void {
    self.native_reg.registerMod(allocator, &self.interner, &self.type_interner, Module);
}

pub fn registerFn(self: *Self, allocator: Allocator, func: *const ffi.ZigFnMeta) void {
    _ = self.native_reg.registerFn(allocator, func, &self.interner, &self.type_interner);
}

pub fn updateModWithScope(self: *Self, allocator: Allocator, index: ModIndex) void {
    self.modules.updateWithSymsInfo(allocator, index, self.lex_scope.current.symbols);
}

pub fn addConstant(self: *Self, allocator: Allocator, constant: Constant) ConstIdx {
    return self.const_interner.add(allocator, constant);
}

pub fn getConstant(self: *const Self, index: ConstIdx) Constant {
    return self.const_interner.constants.items[index.toInt()];
}
