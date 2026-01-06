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
const oom = @import("misc").oom;

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

const Range = struct {
    start: i64,
    end: i64,

    const Self = @This();
    pub const Err = error{OverLap};
    pub const empty: Self = .{ .start = 0, .end = 0 };

    pub fn unit(value: i64) Self {
        return .{ .start = value, .end = value };
    }
};

const IntervalSet = struct {
    ranges: ArrayList(Range),

    const Self = @This();
    pub const empty: Self = .empty;

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.ranges.deinit(allocator);
    }

    /// Computes uncovered parts of a range as a sorted list
    /// Given r, return all parts of r not yet covered
    pub fn subtract(self: *Self, r: Range, alloc: Allocator) !ArrayList(Range) {
        // Uncovered fragments of r
        var out = ArrayList(Range).init(alloc);

        // We start with the entire range [cur_lo, cur_hi]
        // r:  [=========================]
        //     ^cur_lo                   ^cur_hi
        var cur_lo = r.lo;
        const cur_hi = r.hi;

        // self.ranges is sorted and non-overlapping
        for (self.ranges.items) |c| {
            // Skip ranges that end before our current window
            //  c:  [2---4]
            //  r:          [6-----------]
            if (c.hi < cur_lo) continue;

            // Stop if covered range starts after our window
            //  c:                  [10---12]
            //  r:  [3-----------8]
            if (c.lo > cur_hi) break;

            // From here, covered range overlaps our window

            // Case 1 — uncovered gap before c
            //  c:        [4---6]
            //  cur: [1------------10]
            //  uncovered:  [1---3]
            if (c.lo > cur_lo) {
                try out.append(.{ .lo = cur_lo, .hi = c.lo - 1 });
            }

            // Case 2 — shrink the window past the covered range
            //  c:      [4---6]
            //  cur_lo:         ^
            // Values ≤ c.hi are now known to be covered
            cur_lo = c.hi + 1;

            // Stop if nothing left
            if (cur_lo > cur_hi) break;
        }

        // After the loop: tail uncovered segment
        //  covered: [2---4]
        //  r:        [1-----------8]
        //  remaining: [5---8]
        if (cur_lo <= cur_hi) {
            try out.append(.{ .lo = cur_lo, .hi = cur_hi });
        }

        return out;
    }

    /// Merge a new range into covered
    /// Insert r into covered while preserving invariants
    pub fn add(self: *Self, r: Range) !void {
        var lo = r.lo;
        var hi = r.hi;

        var i: usize = 0;
        while (i < self.ranges.items.len) {
            const cur = self.ranges.items[i];

            // Case 1 — new range is strictly before current
            //  cur:          [5---7]
            //  new: [1---3]
            if (hi + 1 < cur.lo) break;

            // Case 2 — new range is strictly after current
            //  cur: [1---3]
            //  new:          [5---7]
            if (lo > cur.hi + 1) {
                i += 1;
                continue;
            }

            // Overlap or adjacency → merge
            //  cur: [3---5]
            //  new:      [5---8]
            //  merged: [3-------8]
            lo = @min(lo, cur.lo);
            hi = @max(hi, cur.hi);
            _ = self.ranges.orderedRemove(i);
        }

        try self.ranges.insert(i, .{ .lo = lo, .hi = hi });
    }

    pub fn addAll(self: *Self, ranges: []const Range) !void {
        for (ranges) |r| {
            try self.add(r);
        }
    }
};

pub const Int = struct {
    covered: IntervalSet,
    /// Whenever we encounter a runtime expression, we can no longer statically check ranges
    is_runtime: bool = false,
    has_wildcard: bool = false,

    const Self = @This();

    // pub fn init(allocator: Allocator, arm_count: usize) Self {
    pub fn init() Self {
        // return .{ .range = ArrayList(Range).initCapacity(allocator, arm_count) catch oom() };
        return .{ .covered = .empty };
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.ranges.deinit(allocator);
    }

    fn arm(self_opaque: *anyopaque, ana: *Analyzer, value_arm: *const Ast.Match.ValueArm, expect: ExprResKind, ctx: *Context) VTable.ArmRes {
        var self: *Self = @ptrCast(@alignCast(self_opaque));

        const arm_span = ana.ast.getSpan(value_arm.expr);
        _ = arm_span; // autofix

        const range: ?Range, const arm_res = b: switch (value_arm.expr.*) {
            .int => |e| {
                const res = try ana.intLit(e, ctx);
                break :b Range.unit(ana.irb.getInstr(res.instr).int);
            },
            else => |e| {
                self.is_runtime = true;
                const res = try ana.analyzeExpr(e, expect, ctx);

                if (!res.type.is(.int)) @panic("wrong type");

                break :b .{ null, res };
            },
        };

        var any_uncovered = false;
        var partial = false;
        check: {
            if (!self.is_runtime) break :check;

            if (range) |r| {
                const uncovered = try self.covered.subtract(r, ana.allocator);
                if (uncovered.items.len == 0) break :check;

                any_uncovered = true;
                if (uncovered.items.len != 1 or
                    uncovered.items[0].lo != r.lo or
                    uncovered.items[0].hi != r.hi)
                {
                    partial = true;
                }

                try self.covered.addAll(uncovered.items);
            }
        }

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
