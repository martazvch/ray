const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Analyzer = @import("Analyzer.zig");
const ExprResKind = Analyzer.ExprResKind;
const Error = Analyzer.Error;
const Context = Analyzer.Context;
const InstrInfos = Analyzer.InstrInfos;
const ir = @import("ir.zig");
const InstrIndex = ir.Index;
const Instr = ir.Instruction;
const Type = @import("types.zig").Type;
const Ast = @import("../parser/Ast.zig");
const Span = @import("../parser/Lexer.zig").Span;
const misc = @import("misc");
const oom = misc.oom;
const RangeSet = misc.RangeSet;

pub const MatchArms = struct {
    types: []const *const Type,
    arms: []const Instr.Match.Arm,
    wildcard: ?InstrIndex,
};

const VTable = struct {
    ptr: *anyopaque,
    armFn: *const fn (*anyopaque, *Analyzer, *const Ast.Match.ValueArm, ExprResKind, *Context) ArmRes,
    wildcardFn: *const fn (*anyopaque, *Analyzer, *const Ast.Match.Wildcard, ExprResKind, *Context) Error!InstrInfos,
    validateFn: *const fn (*anyopaque, *Analyzer, Span) Error!void,

    const ArmRes = Error!struct { InstrInfos, InstrInfos };

    pub fn arm(self: *VTable, ana: *Analyzer, arm_expr: *const Ast.Match.ValueArm, expect: ExprResKind, ctx: *Context) ArmRes {
        return self.armFn(self.ptr, ana, arm_expr, expect, ctx);
    }

    pub fn wildcard(self: *VTable, ana: *Analyzer, arm_expr: *const Ast.Match.Wildcard, expect: ExprResKind, ctx: *Context) Error!InstrInfos {
        return self.wildcardFn(self.ptr, ana, arm_expr, expect, ctx);
    }

    pub fn validate(self: *VTable, ana: *Analyzer, span: Span) Error!void {
        return self.validateFn(self.ptr, ana, span);
    }
};

pub const Enum = struct {
    proto: Type.Enum.Proto,
    has_wildcard: bool = false,

    const Self = @This();

    pub fn init(proto: Type.Enum.Proto) Self {
        return .{ .proto = proto };
    }

    fn arm(self_opaque: *anyopaque, ana: *Analyzer, value_arm: *const Ast.Match.ValueArm, expect: ExprResKind, ctx: *Context) VTable.ArmRes {
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

    pub fn analyzer(self: *Self) VTable {
        return .{
            .ptr = self,
            .armFn = arm,
            .wildcardFn = wildcard,
            .validateFn = validate,
        };
    }
};

pub const Int = struct {
    covered: RangeSet,
    /// Whenever we encounter a runtime expression, we can no longer statically check ranges
    is_runtime: bool = false,
    has_wildcard: bool = false,

    const Self = @This();

    pub fn init() Self {
        return .{ .covered = .empty };
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.ranges.deinit(allocator);
    }

    fn arm(self_opaque: *anyopaque, ana: *Analyzer, value_arm: *const Ast.Match.ValueArm, expect: ExprResKind, ctx: *Context) VTable.ArmRes {
        var self: *Self = @ptrCast(@alignCast(self_opaque));

        const arm_span = ana.ast.getSpan(value_arm.expr);
        _ = arm_span; // autofix

        const range: ?RangeSet.Range, const arm_res = b: switch (value_arm.expr.*) {
            .int => |e| {
                const res = try ana.intLit(e, ctx);
                break :b .unit(ana.irb.getInstr(res.instr).int);
            },
            else => |e| {
                self.is_runtime = true;
                const res = try ana.analyzeExpr(e, expect, ctx);

                if (!res.type.is(.int)) @panic("wrong type");

                break :b .{ null, res };
            },
        };

        if (!self.is_runtime) if (range) |r| {
            try self.covered.checkRange(ana.allocator, r);
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

    pub fn analyzer(self: *Self) VTable {
        return .{
            .ptr = self,
            .armFn = arm,
            .wildcardFn = wildcard,
            .validateFn = validate,
        };
    }
};
