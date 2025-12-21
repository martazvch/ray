const std = @import("std");
const Allocator = std.mem.Allocator;

const type_mod = @import("../analyzer/types.zig");
const TypeInterner = type_mod.TypeInterner;
const ObjFns = type_mod.ObjFns;
const Obj = @import("../runtime/Obj.zig");
const ModuleInterner = @import("ModuleInterner.zig");
const NativeRegister = @import("NativesRegister.zig");

const misc = @import("misc");
const Interner = misc.Interner;
const Sb = misc.StringBuilder;

config: Config,
interner: Interner,
type_interner: TypeInterner,
path_builder: Sb,
module_interner: ModuleInterner,
native_reg: NativeRegister,
array_fns: ObjFns,
string_fns: ObjFns,

const Self = @This();

pub fn new(allocator: Allocator, config: Config) Self {
    var ctx: Self = .{
        .config = config,
        .interner = .init(allocator),
        .type_interner = .init(allocator),
        .path_builder = .empty,
        .module_interner = .init(allocator),
        .native_reg = .empty,
        .array_fns = Obj.Array.getFns(allocator),
        .string_fns = Obj.String.getFns(allocator),
    };
    ctx.type_interner.cacheFrequentTypes();

    return ctx;
}

pub fn registerNatives(self: *Self, allocator: Allocator, Module: type) void {
    self.native_reg.register(allocator, &self.interner, &self.type_interner, Module);
}

pub const Config = struct {
    embedded: bool = false,
    print_ast: bool = false,
    print_bytecode: bool = false,
    static_analyzis: bool = false,
    print_ir: bool = false,
};
