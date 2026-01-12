const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Analyzer = @import("Analyzer.zig");
const Matcher = @import("Matcher.zig");
const ExprResKind = Analyzer.ExprResKind;
const Error = Analyzer.Error;
const Context = Analyzer.Context;
const InstrInfos = Analyzer.InstrInfos;
const Type = @import("types.zig").Type;
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
        return .{ .proto = proto };
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

    fn wildcard(self_opaque: *anyopaque, ana: *Analyzer, wc: *const Ast.Match.Wildcard, expect: ExprResKind, ctx: *Context) Error!InstrInfos {
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

pub const Int = struct {
    covered: RangeSet,
    has_wildcard: bool = false,

    const Self = @This();

    pub fn init() Self {
        return .{ .covered = .empty };
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.ranges.deinit(allocator);
    }

    fn arm(self_opaque: *anyopaque, ana: *Analyzer, value_arm: *const Ast.Match.ValueArm, expect: ExprResKind, ctx: *Context) Matcher.ArmRes {
        var self: *Self = @ptrCast(@alignCast(self_opaque));

        const span = ana.ast.getSpan(value_arm.expr);

        const arm_res, const range: ?RangeSet.Range = b: {
            switch (value_arm.expr.*) {
                .int => |e| {
                    const res = try ana.intLit(e, false);
                    break :b .{ res, .unit(ana.irb.getConstant(res.instr).int) };
                },
                .range => |e| {
                    var res = try ana.range(e, ctx);
                    res.type = ana.ti.cache.int;

                    // If range is made of comptime known literals, we check the range
                    if (res.ti.comp_time) {
                        const instr = ana.irb.getInstr(res.instr).range;

                        if (ana.irb.getInstr(instr.start) == .constant and ana.irb.getInstr(instr.end) == .constant) {
                            const range: RangeSet.Range = .{
                                .low = ana.irb.getConstant(instr.start).int,
                                .high = ana.irb.getConstant(instr.end).int,
                            };

                            break :b .{ res, range };
                        }
                    }
                },
                .unary => |*e| {
                    const res = try switch (e.expr.*) {
                        .int => |i| ana.intLit(i, true),
                        else => ana.analyzeExpr(e.expr, .value, ctx),
                    };

                    if (res.ti.comp_time) {
                        break :b .{ res, .unit(ana.irb.getConstant(res.instr).int) };
                    }
                },
                else => |*e| {
                    const res = try ana.analyzeExpr(e, expect, ctx);

                    if (res.ti.comp_time) {
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

    fn checkRange(self: *Self, ana: *Analyzer, range: RangeSet.Range, span: Span) Error!void {
        self.covered.checkRange(ana.allocator, range) catch |err| switch (err) {
            error.partial => ana.warn(.match_partial_overlap, span),
            error.unreached => return ana.err(.match_unreachable_arm, span),
        };
    }

    fn wildcard(self_opaque: *anyopaque, ana: *Analyzer, wc: *const Ast.Match.Wildcard, expect: ExprResKind, ctx: *Context) Error!InstrInfos {
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

pub const Bool = struct {
    has_true: bool = false,
    has_false: bool = false,
    has_wildcard: bool = false,

    const Self = @This();

    pub fn init() Self {
        return .{};
    }

    pub fn deinit(_: *Self, _: Allocator) void {}

    fn arm(self_opaque: *anyopaque, ana: *Analyzer, value_arm: *const Ast.Match.ValueArm, expect: ExprResKind, ctx: *Context) Matcher.ArmRes {
        var self: *Self = @ptrCast(@alignCast(self_opaque));

        const span = ana.ast.getSpan(value_arm.expr);

        const arm_res = b: switch (value_arm.expr.*) {
            .bool => |e| {
                const res = try ana.boolLit(e);
                const constant = ana.irb.getConstant(res.instr).bool;

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

    fn wildcard(self_opaque: *anyopaque, ana: *Analyzer, wc: *const Ast.Match.Wildcard, expect: ExprResKind, ctx: *Context) Error!InstrInfos {
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
        return .{ .covered = .empty };
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
                const constant = ana.irb.getConstant(res.instr).string;

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

    fn wildcard(self_opaque: *anyopaque, ana: *Analyzer, wc: *const Ast.Match.Wildcard, expect: ExprResKind, ctx: *Context) Error!InstrInfos {
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
