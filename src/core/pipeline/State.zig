const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const options = @import("options");

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
const zffi = @import("../ffi/zffi.zig");
const ffi = @import("../ffi/ffi.zig");
const NativeLib = @import("../analyzer/NativeLib.zig");
const SymbolTable = @import("SymbolTable.zig");

const misc = @import("misc");
const Interner = misc.Interner;
const Sb = misc.StringBuilder;
const oom = misc.oom;

config: Config,
interner: Interner,
type_interner: TypeInterner,
const_interner: ConstInterner,
path_builder: Sb,
cwd: Io.Dir,
lex_scope: LexScope,
/// Registers all the importable symbols from all modules in a flat table
symbol_table: SymbolTable,
/// Registers all the symbols per module
modules: ModuleManager,
native_reg: NativeRegister,
strings: std.AutoHashMapUnmanaged(usize, *Obj.String),
array_fns: ObjFns,
string_fns: ObjFns,
/// Associated dynamic library to this module. When importing a native module, we open
/// a subpipeline with the associated library to fetch symbols
dynlib: ?*NativeLib,

const Self = @This();

pub const Config = struct {
    embedded: bool = false,
    print_ast: bool = false,
    print_bytecode: bool = false,
    static_analyzis: bool = false,
    print_ir: bool = false,
    dbg_infos: bool = false,
    path: ?[]const u8 = null,

    printFn: *const fn (Io, []const u8) void = defaultPrint,
    // errorFn: *const fn ([]const u8) void = defaultErr,
};

pub fn defaultPrint(io: Io, text: []const u8) void {
    errdefer @panic("failed to write to stdout");

    var buf: [1024]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(io, &buf);
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

pub fn new(io: Io, allocator: Allocator, cwd: Io.Dir, config: Config) Self {
    // Initiliaze the path builder
    var path_builder: Sb = .empty;
    {
        const path = cwd.realPathFileAlloc(io, ".", allocator) catch oom();
        var it = std.mem.splitScalar(u8, path, std.fs.path.sep);
        while (it.next()) |part| {
            path_builder.append(allocator, part);
        }
    }

    var ctx: Self = .{
        .config = config,
        .interner = .init(allocator),
        .type_interner = .init(allocator),
        .const_interner = .init(allocator),
        .path_builder = path_builder,
        .cwd = cwd,
        .lex_scope = .empty,
        .symbol_table = .empty,
        .modules = .empty,
        .native_reg = undefined,
        .strings = .empty,
        .array_fns = Obj.Array.getFns(),
        .string_fns = Obj.String.getFns(),
        .dynlib = null,
    };

    ctx.native_reg.init(allocator, &ctx.interner);

    ctx.type_interner.cacheFrequentTypes(&ctx.interner);
    ctx.registerMod(allocator, @import("../builtins/builtins.zig"));
    ctx.registerMod(allocator, @import("../builtins/math.zig"));
    ctx.registerMod(allocator, @import("../builtins/file.zig"));
    ctx.registerIntrinsics(allocator, @import("../builtins/intrinsics.zig"));

    if (options.test_mode) {
        ctx.registerMod(allocator, @import("../builtins/test_natives.zig"));
    }

    {
        var it = ctx.native_reg.mods.iterator();
        while (it.next()) |*mod| {
            const index = ctx.modules.open(
                allocator,
                ctx.interner.intern(mod.value_ptr.path),
                ctx.interner.intern(mod.value_ptr.path),
                true,
            );
            ctx.modules.registerSymsFromNativeMod(allocator, index, mod.value_ptr);
        }
    }

    ctx.lex_scope.save = config.dbg_infos;

    // If we're not embedded, we won't add native functions so we can init the global scope
    if (!config.embedded) {
        ctx.lex_scope.initGlobalScope(allocator, &ctx);
    }

    return ctx;
}

/// Should only be called when embedded to end native functions registration to initialize global scope
pub fn initGlobalScope(self: *Self, allocator: Allocator) void {
    if (self.config.embedded) {
        self.lex_scope.initGlobalScope(allocator, self);
        // If embedded/Repl, all code is treated as local code to allow impur code
        self.lex_scope.open(allocator, null, .{ .barrier = true });
        self.modules.registerSymsFromNativeMod(allocator, .toIndex(0), self.native_reg.getGlobalScope());
    }
}

pub fn registerMod(self: *Self, allocator: Allocator, Module: type) void {
    self.native_reg.registerMod(allocator, &self.interner, &self.type_interner, Module);
}

pub fn registerIntrinsics(self: *Self, allocator: Allocator, Module: type) void {
    self.native_reg.registerIntrinsics(allocator, &self.interner, &self.type_interner, Module);
}

/// Used by embedded
pub fn registerFn(self: *Self, allocator: Allocator, func: zffi.FnMeta) void {
    _ = self.native_reg.registerZigFnInGlobal(allocator, &func, &self.interner, &self.type_interner);
}

/// Used by embedded
pub fn registerCFn(self: *Self, allocator: Allocator, func: ffi.FnProto) void {
    _ = self.native_reg.registerExternFnInGlobal(allocator, &func, &self.interner, &self.type_interner);
}

/// Used after analyzer to register module's public symbols' information
/// They correspond to the symbols that can be imported
pub fn registerModPubSymbols(self: *Self, allocator: Allocator, index: ModIndex) void {
    self.modules.registerSymsInfo(allocator, index, &self.lex_scope.current.symbols);
    self.modules.registerGlobalsInfo(allocator, index, &self.lex_scope.current.variables);
    self.symbol_table.addFrom(allocator, &self.lex_scope.current.symbols_type);
}

pub fn addConstant(self: *Self, allocator: Allocator, constant: Constant) ConstIdx {
    return self.const_interner.add(allocator, constant);
}

pub fn getConstant(self: *const Self, index: ConstIdx) Constant {
    return self.const_interner.constants.items[index.toInt()];
}
