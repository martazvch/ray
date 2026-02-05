const std = @import("std");
const Allocator = std.mem.Allocator;

const type_mod = @import("../analyzer/types.zig");
const TypeInterner = type_mod.TypeInterner;
const ObjFns = type_mod.ObjFns;
const Obj = @import("../runtime/Obj.zig");
const ModuleInterner = @import("ModuleInterner.zig");
const NativeRegister = @import("NativesRegister.zig");
const LexScope = @import("../analyzer/LexicalScope.zig");

const misc = @import("misc");
const Interner = misc.Interner;
const Sb = misc.StringBuilder;

config: Config,
interner: Interner,
type_interner: TypeInterner,
path_builder: Sb,
lex_scope: LexScope,
module_interner: ModuleInterner,
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
};

pub fn new(allocator: Allocator, config: Config) Self {
    var ctx: Self = .{
        .config = config,
        .interner = .init(allocator),
        .type_interner = .init(allocator),
        .path_builder = .empty,
        .lex_scope = .empty,
        .module_interner = .init(allocator),
        .native_reg = .empty,
        .strings = .empty,
        .array_fns = Obj.Array.getFns(),
        .string_fns = Obj.String.getFns(),
    };

    ctx.type_interner.cacheFrequentTypes();
    ctx.registerNatives(allocator, @import("../builtins/builtins.zig"));
    ctx.registerNatives(allocator, @import("..//builtins/file.zig"));

    ctx.lex_scope.initGlobalScope(allocator, &ctx);

    // If embedded/Repl, all code is treated as local code to allow impur code
    if (config.embedded) {
        ctx.lex_scope.open(allocator, null, .{ .barrier = true });
    }

    // TODO: implement this only for compile command
    // if (config.compile) {
    //     ctx.lex_scope.save = true;
    // }

    return ctx;
}

pub fn registerNatives(self: *Self, allocator: Allocator, Module: type) void {
    self.native_reg.register(allocator, &self.interner, &self.type_interner, Module);
}
