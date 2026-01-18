const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;

const Type = @import("types.zig").Type;
const InstrIndex = @import("ir.zig").Index;
const Span = @import("../parser/Lexer.zig").Span;
const State = @import("../pipeline/State.zig");

const misc = @import("misc");
const InternerIdx = misc.Interner.Index;
const RevIterator = misc.RevIterator;
const oom = misc.oom;

const Self = @This();

pub const Variable = struct {
    name: InternerIdx,
    type: *const Type,
    kind: enum { local, global },
    initialized: bool,
    used: bool = false,
    index: Index,
    captured: bool,
    constant: bool,
    comp_time: bool,
    ext_mod: ?usize,

    pub const Index = usize;
};

pub const Break = *const Type;
pub const Symbol = struct { name: InternerIdx, type: *const Type, index: usize };
pub const ExternSymbol = struct { module_index: usize, symbol: Symbol };

pub const VariableMap = AutoArrayHashMapUnmanaged(InternerIdx, Variable);
pub const SymbolArrMap = AutoArrayHashMapUnmanaged(InternerIdx, Symbol);
pub const ExternMap = AutoHashMapUnmanaged(InternerIdx, ExternSymbol);

scopes: ArrayList(Scope),
current: *Scope,
builtins: AutoHashMapUnmanaged(InternerIdx, *const Type),
natives: AutoHashMapUnmanaged(InternerIdx, Symbol),
symbol_count: usize,

// Dbg
save: bool,
saved: ArrayList(SaveSlot),
saved_syms: ArrayList(*Symbol),

pub const SaveSlot = union(enum) { tombstone, scope: Scope };

pub const empty: Self = .{
    .scopes = .empty,
    .current = undefined,
    .builtins = .empty,
    .natives = .empty,
    .symbol_count = 0,

    .save = false,
    .saved = .empty,
    .saved_syms = .empty,
};

pub const Scope = struct {
    name: ?InternerIdx,
    variables: VariableMap = .empty,
    forwarded: VariableMap = .empty,
    symbols: SymbolArrMap = .empty,
    extern_symbols: ExternMap = .empty,
    /// First is the interned identifier and second is the interned module's path key of module interner
    modules: AutoHashMapUnmanaged(InternerIdx, *const Type) = .empty,
    breaks: ArrayList(Break) = .empty,
    /// Offset to apply to any index in this scope. Correspond to the numbers of locals
    /// in parent scopes (represents stack at runtime)
    offset: usize,
    opts: Options,

    pub const Options = struct {
        /// It means you can't go to upper scope from this one
        barrier: bool = false,
        /// This scopes is expected to return a value
        exp_val: bool = false,
        /// Can use `continue` statments inside this block
        can_continue: bool = false,
    };
};

/// Opens a new scope. It can be the scope of a symbol like a function, closure or structure
/// declaration. In that case, **barrier** should be `true` as you can access outter scope from
/// those and the name should be `null`.
/// Otherwise, you can name the scope as it might just be a regular block
pub fn open(self: *Self, allocator: Allocator, name: ?InternerIdx, opts: Scope.Options) void {
    const offset = if (opts.barrier) 0 else self.current.variables.count() + self.current.offset;

    var scope: Scope = .{ .name = name, .offset = offset, .opts = opts };

    // If variables have been forwarded, for declare them now
    if (self.scopes.items.len > 0) {
        scope.variables.ensureUnusedCapacity(allocator, self.current.forwarded.count()) catch oom();

        var it = self.current.forwarded.iterator();

        while (it.next()) |entry| {
            scope.variables.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.*);
        }

        self.current.forwarded.clearRetainingCapacity();
    }

    self.scopes.append(allocator, scope) catch oom();
    self.updateCurrent();

    if (self.save) {
        self.saved.append(allocator, .tombstone) catch oom();
    }
}

pub fn close(self: *Self) struct { usize, []const Break } {
    const popped = self.scopes.pop().?;
    self.updateCurrent();

    // Saves the scope at the latest tombstone to reproduce runtime scope order
    if (self.save) {
        var it: RevIterator(SaveSlot) = .init(self.saved.items);

        save: {
            while (it.next()) |slot| {
                if (slot.* == .tombstone) {
                    slot.* = .{ .scope = popped };
                    break :save;
                }
            }

            unreachable;
        }
    }

    return .{ popped.variables.count(), popped.breaks.items };
}

pub fn initGlobalScope(self: *Self, allocator: Allocator, state: *State) void {
    self.open(allocator, null, .{ .barrier = true });
    const builtins = std.meta.fields(@TypeOf(state.type_interner.cache));
    self.builtins.ensureUnusedCapacity(allocator, builtins.len) catch oom();

    inline for (builtins) |builtin| {
        self.builtins.putAssumeCapacity(state.interner.intern(builtin.name), @field(state.type_interner.cache, builtin.name));
    }

    self.natives.ensureTotalCapacity(allocator, @intCast(state.native_reg.funcs_meta.count())) catch oom();
    var it = state.native_reg.funcs_meta.iterator();
    while (it.next()) |entry| {
        self.natives.putAssumeCapacity(entry.key_ptr.*, .{
            .name = entry.key_ptr.*,
            .index = self.natives.count(),
            .type = entry.value_ptr.*,
        });
    }
}

pub fn closeGlobalScope(self: *Self) void {
    const popped = self.scopes.pop().?;
    if (self.save) {
        self.saved.items[0] = .{ .scope = popped };
    }
}

/// Update `current` field to last scope. **Assumes** that there is at least one scope
fn updateCurrent(self: *Self) void {
    self.current = &self.scopes.items[self.scopes.items.len - 1];
}

pub fn isGlobal(self: *Self) bool {
    return self.scopes.items.len == 1;
}

pub fn declareVar(
    self: *Self,
    allocator: Allocator,
    name: InternerIdx,
    ty: *const Type,
    captured: bool,
    initialized: bool,
    constant: bool,
    comp_time: bool,
    ext_mode: ?usize,
) error{TooManyLocals}!usize {
    const index = self.current.variables.count();
    if (index == 255 and !self.isGlobal()) return error.TooManyLocals;

    self.current.variables.put(allocator, name, .{
        .name = name,
        .type = ty,
        .kind = if (self.isGlobal()) .global else .local,
        .initialized = initialized,
        .index = index,
        .captured = captured,
        .constant = constant,
        .comp_time = comp_time,
        .ext_mod = ext_mode,
    }) catch oom();

    return index;
}

pub fn declareVarInFutureScope(self: *Self, allocator: Allocator, name: InternerIdx, ty: *const Type, captured: bool) error{TooManyLocals}!void {
    const index = self.current.forwarded.count();
    if (index == 255) return error.TooManyLocals;

    self.current.forwarded.put(allocator, name, .{
        .name = name,
        .type = ty,
        .kind = .local,
        .initialized = true,
        .index = index,
        .captured = captured,
        .constant = true,
        .comp_time = false,
        .ext_mod = null,
    }) catch oom();
}

/// Tries to retreive a variable from scopes and the local offset of its scope
pub fn getVariable(self: *const Self, name: InternerIdx) ?struct { *Variable, usize } {
    var it = self.iterator();
    while (it.next()) |scope| {
        if (scope.variables.getPtr(name)) |variable| {
            return .{ variable, scope.offset };
        }
    }

    return null;
}

/// Forward declares a symbol without incrementing global symbol count
pub fn forwardDeclareSymbol(self: *Self, allocator: Allocator, name: InternerIdx) *Symbol {
    self.current.symbols.put(allocator, name, .{ .name = name, .type = undefined, .index = self.symbol_count }) catch oom();
    self.symbol_count += 1;

    const sym = self.current.symbols.getPtr(name).?;

    if (self.save) {
        self.saved_syms.append(allocator, sym) catch oom();
    }

    return sym;
}

pub fn getSymbol(self: *const Self, name: InternerIdx) ?*Symbol {
    var it = self.iterator();
    while (it.next()) |scope| {
        if (scope.symbols.getPtr(name)) |sym| {
            return sym;
        }
    }

    return null;
}

pub fn getSymbolName(self: *const Self, sym_type: *const Type) ?InternerIdx {
    var it = self.iterator();
    while (it.next()) |scope| {
        var sym_it = scope.symbols.iterator();
        while (sym_it.next()) |sym| {
            if (sym.value_ptr.type == sym_type) {
                return sym.key_ptr.*;
            }
        }
    }

    return null;
}

pub fn declareExternSymbol(self: *Self, allocator: Allocator, name: InternerIdx, module_index: usize, symbol: Symbol) void {
    self.current.extern_symbols.put(allocator, name, .{ .module_index = module_index, .symbol = symbol }) catch oom();
}

pub fn getExternSymbol(self: *const Self, name: InternerIdx) ?*ExternSymbol {
    var it = self.iterator();
    while (it.next()) |scope| {
        if (scope.extern_symbols.getPtr(name)) |ext| {
            return ext;
        }
    }

    return null;
}

pub fn getBuiltinSymbol(self: *const Self, name: InternerIdx) ?*Symbol {
    return self.natives.getPtr(name);
}

pub fn declareModule(self: *Self, allocator: Allocator, name: InternerIdx, ty: *const Type) void {
    self.current.modules.put(allocator, name, ty) catch oom();
}

pub fn getModule(self: *const Self, name: InternerIdx) ?*const Type {
    var it = self.iterator();
    while (it.next()) |scope| {
        if (scope.modules.get(name)) |ty| {
            return ty;
        }
    }

    return null;
}

/// Searches a scope by its name or return the last one if `label` is null
/// Returns the index (start from 0) and the depth (start from scopes.len) of the scope if found
pub fn getScope(self: *const Self, label: ?InstrIndex) error{UnknownLabel}!struct { usize, usize } {
    const lbl = label orelse return .{ self.scopes.items.len - 1, 0 };

    var depth: usize = 0;
    var it = self.iterator();
    while (it.next()) |scope| : (depth += 1) {
        // We hit a function or structure's declaration scope
        if (scope.opts.barrier) break;

        if (scope.name) |sn| {
            if (sn != lbl) continue;
            return .{ self.scopes.items.len - 1 - depth, depth };
        }
    }

    return error.UnknownLabel;
}

/// Searches a scope by its name or return the last one if `label` is null
/// Returns the index (start from 0) and the depth (start from scopes.len) of the scope if found
pub const ContinueScopeErr = error{
    /// Didn't find any scope corresponding to provided label
    UnknownLabel,
    /// Found a labelled scope but it isn't continuable
    CantContinue,
    /// Didn't find any continuable scope
    NoContinueScope,
};
pub fn getScopeContinuable(self: *const Self, label: ?InstrIndex) ContinueScopeErr!struct { *const Scope, usize } {
    var depth: usize = 0;
    var it = self.iterator();
    while (it.next()) |scope| : (depth += 1) {
        // We hit a function or structure's declaration scope
        if (scope.opts.barrier) break;

        if (label) |lbl| {
            if (scope.name) |sn| {
                if (sn != lbl) continue;
                if (!scope.opts.can_continue) return error.CantContinue;
                return .{ scope, depth };
            }
        } else if (scope.opts.can_continue) {
            return .{ scope, depth };
        }
    }

    return if (label != null) error.UnknownLabel else error.NoContinueScope;
}

pub fn stackDiffWithCurrent(self: *const Self, other: *const Scope) usize {
    return self.current.offset + self.current.variables.count() - other.offset;
}

// TODO: shouldn't it be in type interner?
pub fn getType(self: *Self, name: InternerIdx) ?*const Type {
    if (self.builtins.get(name)) |builtin| {
        return builtin;
    } else if (self.getSymbol(name)) |sym| {
        return sym.type;
    }

    return null;
}

pub fn isVarOrSymInCurrentScope(self: *const Self, name: InternerIdx) bool {
    return self.current.variables.get(name) != null or self.current.symbols.get(name) != null;
}

pub fn isModuleImported(self: *const Self, name: InternerIdx) bool {
    return self.getModule(name) != null;
}

fn iterator(self: *const Self) RevIterator(Scope) {
    return .init(self.scopes.items);
}
