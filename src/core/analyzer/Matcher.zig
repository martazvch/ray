const std = @import("std");
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

pub const MatchArms = struct {
    types: []const *const Type,
    arms: []const Instr.Match.Arm,
    wildcard: ?InstrIndex,
};

ptr: *anyopaque,
vtable: *const VTable,

const Self = @This();
pub const ArmRes = Error!struct { InstrInfos, InstrInfos };

const VTable = struct {
    armFn: *const fn (*anyopaque, *Analyzer, *const Ast.Match.ValueArm, ExprResKind, *Context) ArmRes,
    wildcardFn: *const fn (*anyopaque, *Analyzer, *const Ast.Match.Wildcard, ExprResKind, *Context) Error!InstrInfos,
    validateFn: *const fn (*anyopaque, *Analyzer, Span) Error!void,
};

pub fn arm(self: *Self, ana: *Analyzer, arm_expr: *const Ast.Match.ValueArm, expect: ExprResKind, ctx: *Context) ArmRes {
    return self.vtable.armFn(self.ptr, ana, arm_expr, expect, ctx);
}

pub fn wildcard(self: *Self, ana: *Analyzer, arm_expr: *const Ast.Match.Wildcard, expect: ExprResKind, ctx: *Context) Error!InstrInfos {
    return self.vtable.wildcardFn(self.ptr, ana, arm_expr, expect, ctx);
}

pub fn validate(self: *Self, ana: *Analyzer, span: Span) Error!void {
    return self.vtable.validateFn(self.ptr, ana, span);
}
