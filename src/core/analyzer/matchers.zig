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
const RangeSet = misc.RangeSet;

pub const Enum = struct {
    proto: Type.Enum.Proto,
    has_wildcard: bool = false,

    const Self = @This();

    pub fn init(proto: Type.Enum.Proto) Self {
        return .{ .proto = proto };
    }

    fn arm(self_opaque: *anyopaque, ana: *Analyzer, value_arm: *const Ast.Match.ValueArm, expect: ExprResKind, ctx: *Context) Matcher.ArmRes {
        var self: *Self = @ptrCast(@alignCast(self_opaque));

        const arm_span = ana.ast.getSpan(value_arm.expr);

        const tag, const arm_res = switch (value_arm.expr.*) {
            .enum_lit => |e| .{ e, try ana.enumLit(e, ctx) },
            .field => |*e| .{ e.field, try ana.field(e, ctx) },
            else => return ana.err(.match_enum_invalid_pat, arm_span),
        };

        if (self.proto.getPtr(ana.interner.intern(ana.ast.toSource(tag)))) |found| {
            if (found.*) {
                return ana.err(.{ .match_duplicate_arm = .{ .name = ana.ast.toSource(tag) } }, arm_span);
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

        const arm_span = ana.ast.getSpan(value_arm.expr);

        const arm_res = b: switch (value_arm.expr.*) {
            .int => |e| {
                const res = try ana.intLit(e);
                try self.checkRange(ana, .unit(ana.irb.getConstant(res.instr).int), arm_span);
                break :b res;
            },
            .range => |e| {
                var res = try ana.range(e, ctx);
                res.type = ana.ti.cache.int;

                // If range is made of comptime known literals, we check the range
                if (e.start.* == .int and e.end.* == .int) {
                    const instr = ana.irb.getInstr(res.instr).range;

                    const range: RangeSet.Range = .{
                        .low = ana.irb.getConstant(instr.start).int,
                        .high = ana.irb.getConstant(instr.end).int,
                    };
                    try self.checkRange(ana, range, arm_span);
                }

                break :b res;
            },
            else => |*e| try ana.analyzeExpr(e, expect, ctx),
        };

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
            return ana.err(.match_num_no_wildcard, span);
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
