const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Analyzer = @import("Analyzer.zig");
const Variable = @import("LexicalScope.zig").Variable;
const Matcher = @import("Matcher.zig");
const ExprResKind = Analyzer.ExprResKind;
const Error = Analyzer.Error;
const Context = Analyzer.Context;
const InstrInfos = Analyzer.InstrInfos;
const Type = @import("types.zig").Type;
const TypeId = @import("types.zig").TypeId;
const Ast = @import("../parser/Ast.zig");
const Span = @import("../parser/Lexer.zig").Span;
const misc = @import("misc");
const oom = misc.oom;
const Interner = misc.Interner;
const RangeSet = misc.RangeSet;
const Set = misc.Set;

pub const Enum = struct {
    proto: Type.Enum.Proto,
    has_wildcard: bool = false,

    const Self = @This();

    pub fn init(proto: Type.Enum.Proto) Self {
        return .{
            .proto = proto,
        };
    }

    fn arm(self_opaque: *anyopaque, ana: *Analyzer, value_arm: *const Ast.Match.ValueArm, expect: ExprResKind, ctx: *Context) Matcher.ArmRes {
        var self: *Self = @ptrCast(@alignCast(self_opaque));

        const span = ana.ast.getSpan(value_arm.expr);

        const tag, const arm_res = switch (value_arm.expr.*) {
            .enum_lit => |e| .{ e, try ana.enumLit(e, ctx) },
            .field => |*e| .{ e.field, try ana.field(e, ctx) },
            else => return ana.err(.match_enum_invalid_pat, span),
        };

        if (self.proto.getPtr(ana.interner.intern(ana.ast.toSource(tag)))) |found| {
            if (found.*) {
                return ana.err(.match_duplicate_arm, span);
            }
            found.* = true;
        }

        const body = try ana.analyzeNode(&value_arm.body, expect, ctx);

        return .{ arm_res, body };
    }

    fn wildcard(self_opaque: *anyopaque, ana: *Analyzer, wc: Ast.Match.Wildcard, expect: ExprResKind, ctx: *Context) Error!InstrInfos {
        var self: *Self = @ptrCast(@alignCast(self_opaque));

        check: {
            var it = self.proto.iterator();
            while (it.next()) |entry| {
                if (!entry.value_ptr.*) {
                    break :check;
                }
            }

            return ana.err(.match_wildcard_exhaustive, ana.ast.getSpan(wc));
        }

        self.has_wildcard = true;

        return ana.analyzeNode(&wc.body, expect, ctx);
    }

    fn validate(self_opaque: *anyopaque, ana: *Analyzer, span: Span) Error!void {
        var self: *Self = @ptrCast(@alignCast(self_opaque));

        if (self.has_wildcard) return;

        var missing: ArrayList(u8) = .empty;

        var it = self.proto.iterator();
        while (it.next()) |entry| {
            if (!entry.value_ptr.*) {
                if (missing.items.len > 0) {
                    missing.appendSlice(ana.allocator, ", ") catch oom();
                }
                missing.appendSlice(ana.allocator, ana.interner.getKey(entry.key_ptr.*).?) catch oom();
            }
        }

        if (missing.items.len > 0) return ana.err(
            .{ .match_non_exhaustive = .{ .missing = missing.toOwnedSlice(ana.allocator) catch oom() } },
            span,
        );
    }

    pub fn matcher(self: *Self) Matcher {
        return .{
            .ptr = self,
            .vtable = &.{
                .armFn = arm,
                .wildcardFn = wildcard,
                .validateFn = validate,
            },
        };
    }
};

pub fn Num(T: type) type {
    if (T != i64 and T != f64) {
        @compileError("Numeric matcher can only accept i64 or f64 types");
    }

    return struct {
        covered: RangeSetType,
        has_wildcard: bool = false,

        const Self = @This();
        const RangeSetType = RangeSet(T);
        const RangeType = RangeSetType.InnerRange;

        pub fn init() Self {
            return .{
                .covered = .empty,
            };
        }

        pub fn deinit(self: *Self, allocator: Allocator) void {
            self.ranges.deinit(allocator);
        }

        fn arm(self_opaque: *anyopaque, ana: *Analyzer, value_arm: *const Ast.Match.ValueArm, expect: ExprResKind, ctx: *Context) Matcher.ArmRes {
            var self: *Self = @ptrCast(@alignCast(self_opaque));

            const span = ana.ast.getSpan(value_arm.expr);

            const arm_res, const range: ?RangeType = b: {
                switch (value_arm.expr.*) {
                    .float => |e| {
                        const res = try ana.floatLit(e, false);
                        break :b .{ res, .unit(constantValue(ana, res.instr)) };
                    },
                    .int => |e| {
                        const res = try ana.intLit(e, false);
                        break :b .{ res, .unit(constantValue(ana, res.instr)) };
                    },
                    .binop => |e| {
                        if (e.op == .dot_dot) {
                            var res = try ana.range(e, ctx);
                            res.type = res.type.range;

                            // If range is made of comptime known literals, we check the range
                            if (res.ti.comp_time) {
                                const instr = ana.irb.getInstr(res.instr).range;

                                if (ana.irb.getInstr(instr.start) == .constant and ana.irb.getInstr(instr.end) == .constant) {
                                    const range: RangeType = .{
                                        .low = constantValue(ana, instr.start),
                                        .high = constantValue(ana, instr.end),
                                    };

                                    break :b .{ res, range };
                                }
                            }
                        }
                    },
                    .unary => |*e| {
                        if (ana.ast.token_tags[e.op] != .minus) {
                            return ana.err(.match_num_invalid_unary, span);
                        }

                        const res = try switch (e.expr.*) {
                            .int => |i| res: {
                                if (T != i64) return ana.err(
                                    .{ .type_mismatch = .{ .expect = "float", .found = "int" } },
                                    span,
                                );

                                break :res ana.intLit(i, true);
                            },
                            .float => |f| res: {
                                if (T != f64) return ana.err(
                                    .{ .type_mismatch = .{ .expect = "int", .found = "float" } },
                                    span,
                                );

                                break :res ana.floatLit(f, true);
                            },
                            else => return ana.err(.match_num_invalid_unary, span),
                        };

                        if (res.ti.comp_time) {
                            break :b .{ res, .unit(constantValue(ana, res.instr)) };
                        }
                    },
                    else => |*e| {
                        const res = try ana.analyzeExpr(e, expect, ctx);

                        // analyzeExpr checks for result's kind expectation but if we ask for a value
                        // and the return symbol is a function, it by-pass the .value tag, so we have to make
                        // sure ourselves here that it isn't a symbol, otherwise `int` for example (which is a builtin)
                        // is a valid value as a `match` arm
                        if (res.ti.comp_time and !res.ti.is_sym) {
                            break :b .{ res, null };
                        }
                    },
                }

                return ana.err(.match_non_literal, span);
            };

            if (range) |r| {
                if (!r.isIncreasing()) {
                    return ana.err(.match_num_decrease_range, span);
                }

                try self.checkRange(ana, r, span);
            }

            const body = try ana.analyzeNode(&value_arm.body, expect, ctx);

            return .{ arm_res, body };
        }

        fn checkRange(self: *Self, ana: *Analyzer, range: RangeType, span: Span) Error!void {
            self.covered.checkRange(ana.allocator, range) catch |err| switch (err) {
                error.partial => ana.warn(.match_partial_overlap, span),
                error.unreached => return ana.err(.match_unreachable_arm, span),
            };
        }

        fn constantValue(ana: *const Analyzer, instr: usize) T {
            return switch (T) {
                f64 => ana.getConstant(instr).float,
                i64 => ana.getConstant(instr).int,
                else => unreachable,
            };
        }

        fn wildcard(self_opaque: *anyopaque, ana: *Analyzer, wc: Ast.Match.Wildcard, expect: ExprResKind, ctx: *Context) Error!InstrInfos {
            var self: *Self = @ptrCast(@alignCast(self_opaque));
            self.has_wildcard = true;

            return ana.analyzeNode(&wc.body, expect, ctx);
        }

        fn validate(self_opaque: *anyopaque, ana: *Analyzer, span: Span) Error!void {
            const self: *Self = @ptrCast(@alignCast(self_opaque));

            if (!self.has_wildcard) {
                return ana.err(.match_no_wildcard, span);
            }
        }

        pub fn matcher(self: *Self) Matcher {
            return .{
                .ptr = self,
                .vtable = &.{
                    .armFn = arm,
                    .wildcardFn = wildcard,
                    .validateFn = validate,
                },
            };
        }
    };
}

pub const Bool = struct {
    has_true: bool,
    has_false: bool,
    has_wildcard: bool,

    const Self = @This();

    pub fn init() Self {
        return .{
            .has_true = false,
            .has_false = false,
            .has_wildcard = false,
        };
    }

    pub fn deinit(_: *Self, _: Allocator) void {}

    fn arm(self_opaque: *anyopaque, ana: *Analyzer, value_arm: *const Ast.Match.ValueArm, expect: ExprResKind, ctx: *Context) Matcher.ArmRes {
        var self: *Self = @ptrCast(@alignCast(self_opaque));

        const span = ana.ast.getSpan(value_arm.expr);

        const arm_res = b: switch (value_arm.expr.*) {
            .bool => |e| {
                const res = try ana.boolLit(e);
                const constant = ana.getConstant(res.instr).bool;

                if (constant) {
                    if (self.has_true) return ana.err(.match_duplicate_arm, span);
                    self.has_true = true;
                } else {
                    if (self.has_false) return ana.err(.match_duplicate_arm, span);
                    self.has_false = true;
                }

                break :b res;
            },
            else => return ana.err(.match_non_literal, span),
        };

        const body = try ana.analyzeNode(&value_arm.body, expect, ctx);

        return .{ arm_res, body };
    }

    fn wildcard(self_opaque: *anyopaque, ana: *Analyzer, wc: Ast.Match.Wildcard, expect: ExprResKind, ctx: *Context) Error!InstrInfos {
        var self: *Self = @ptrCast(@alignCast(self_opaque));

        if (self.has_true and self.has_false) {
            return ana.err(.match_wildcard_exhaustive, ana.ast.getSpan(wc));
        }

        self.has_wildcard = true;

        return ana.analyzeNode(&wc.body, expect, ctx);
    }

    fn validate(self_opaque: *anyopaque, ana: *Analyzer, span: Span) Error!void {
        const self: *Self = @ptrCast(@alignCast(self_opaque));

        if (self.has_wildcard or (self.has_true and self.has_false)) return;

        const missing = if (self.has_true) "false" else if (self.has_false) "true" else "true, false";
        return ana.err(.{ .match_non_exhaustive = .{ .missing = missing } }, span);
    }

    pub fn matcher(self: *Self) Matcher {
        return .{
            .ptr = self,
            .vtable = &.{
                .armFn = arm,
                .wildcardFn = wildcard,
                .validateFn = validate,
            },
        };
    }
};

pub const String = struct {
    covered: Set(Interner.Index),
    has_wildcard: bool = false,

    const Self = @This();

    pub fn init() Self {
        return .{
            .covered = .empty,
        };
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.set.deinit(allocator);
    }

    fn arm(self_opaque: *anyopaque, ana: *Analyzer, value_arm: *const Ast.Match.ValueArm, expect: ExprResKind, ctx: *Context) Matcher.ArmRes {
        var self: *Self = @ptrCast(@alignCast(self_opaque));

        const span = ana.ast.getSpan(value_arm.expr);

        const arm_res = switch (value_arm.expr.*) {
            .string => |s| b: {
                const res = try ana.string(s);
                const constant = ana.getConstant(res.instr).string;

                if (self.covered.has(constant)) {
                    return ana.err(.match_duplicate_arm, span);
                }
                self.covered.add(ana.allocator, constant) catch oom();

                break :b res;
            },
            else => return ana.err(.match_non_literal, span),
        };

        const body = try ana.analyzeNode(&value_arm.body, expect, ctx);

        return .{ arm_res, body };
    }

    fn wildcard(self_opaque: *anyopaque, ana: *Analyzer, wc: Ast.Match.Wildcard, expect: ExprResKind, ctx: *Context) Error!InstrInfos {
        var self: *Self = @ptrCast(@alignCast(self_opaque));
        self.has_wildcard = true;

        return ana.analyzeNode(&wc.body, expect, ctx);
    }

    fn validate(self_opaque: *anyopaque, ana: *Analyzer, span: Span) Error!void {
        const self: *Self = @ptrCast(@alignCast(self_opaque));

        if (!self.has_wildcard) {
            return ana.err(.match_no_wildcard, span);
        }
    }

    pub fn matcher(self: *Self) Matcher {
        return .{
            .ptr = self,
            .vtable = &.{
                .armFn = arm,
                .wildcardFn = wildcard,
                .validateFn = validate,
            },
        };
    }
};

pub const Union = struct {
    proto: Type.Union.Proto,
    alias: ?usize,
    in_trap: bool,
    value: InstrInfos,
    value_var: ?*Variable,
    value_span: Span,
    value_base_type: *const Type,
    match_kw: Ast.TokenIndex,
    has_wildcard: bool,

    const Self = @This();

    pub fn init(
        ana: *Analyzer,
        ty: Type.Union,
        alias: ?usize,
        in_trap: bool,
        value: InstrInfos,
        value_span: Span,
        kw: Ast.TokenIndex,
    ) Self {
        const value_instr = ana.irb.getInstr(value.instr);
        const is_ident = value_instr == .identifier;

        return .{
            .proto = ty.proto(ana.allocator),
            .alias = alias,
            .in_trap = in_trap,
            .value = value,
            // If we are in `trap` expression, the identifier doesn't refer to a variable,
            // it's the binding name for the error
            .value_var = if (is_ident and !in_trap)
                ana.scope.getVarInCurrentScopeAt(value_instr.identifier.index)
            else
                null,
            .value_span = value_span,
            .value_base_type = value.type,
            .match_kw = kw,
            .has_wildcard = false,
        };
    }

    const ArmTypeRes = Analyzer.Error!struct { *const Type, InstrInfos };

    pub fn arm(self: *Self, ana: *Analyzer, type_arm: Ast.Match.TypeArm, expect: ExprResKind, ctx: *Context) ArmTypeRes {
        const arm_type = try ana.checkAndGetType(type_arm.type, ctx);

        const gop = self.proto.getOrPut(ana.allocator, arm_type) catch oom();
        if (!gop.found_existing) return ana.err(
            .{ .type_not_in_union = .{
                .expect = ana.typeName(self.value_base_type),
                .found = ana.typeName(arm_type),
            } },
            ana.ast.getSpan(type_arm),
        );

        if (gop.value_ptr.*) {
            return ana.err(.match_duplicate_arm, ana.ast.getSpan(type_arm));
        }
        gop.value_ptr.* = true;

        // Defines alias' type inside this arm
        if (!self.in_trap) if (self.alias) |al| {
            const alias_variable = ana.scope.getVarInCurrentScopeAt(al);
            alias_variable.type = arm_type;
        };

        // Same if matched on value is an identifier
        if (self.value_var) |v| {
            v.type = arm_type;
        }

        const body_res = switch (type_arm.body) {
            .value => |*v| try ana.analyzeNode(v, expect, ctx),
            .patmat => |v| pat: {
                var local_value = self.value;
                local_value.type = arm_type;
                break :pat try ana.matchValue(v, local_value, self.value_span, self.match_kw, expect, ctx);
            },
        };

        return .{ arm_type, body_res };
    }

    pub fn wildcard(self: *Self, ana: *Analyzer, wc: Ast.Match.Wildcard, expect: ExprResKind, ctx: *Context) Error!InstrInfos {
        check: {
            var it = self.proto.iterator();
            while (it.next()) |entry| {
                if (!entry.value_ptr.*) {
                    break :check;
                }
            }

            return ana.err(.match_wildcard_exhaustive, ana.ast.getSpan(wc));
        }

        self.has_wildcard = true;

        return ana.analyzeNode(&wc.body, expect, ctx);
    }

    pub fn validate(self: *Self, ana: *Analyzer, span: Span) Error!void {
        if (self.has_wildcard) return;

        var missing: ArrayList(u8) = .empty;

        var it = self.proto.iterator();
        while (it.next()) |entry| {
            if (!entry.value_ptr.*) {
                if (missing.items.len > 0) {
                    missing.appendSlice(ana.allocator, ", ") catch oom();
                }
                missing.appendSlice(ana.allocator, ana.typeName(entry.key_ptr.*)) catch oom();
            }
        }

        if (missing.items.len > 0) return ana.err(
            .{ .match_non_exhaustive = .{ .missing = missing.toOwnedSlice(ana.allocator) catch oom() } },
            span,
        );
    }

    pub fn terminate(self: *Self) void {
        if (self.value_var) |v| {
            v.type = self.value_base_type;
        }
    }
};
