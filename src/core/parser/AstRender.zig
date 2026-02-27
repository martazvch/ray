const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const assert = std.debug.assert;

const Ast = @import("Ast.zig");

allocator: Allocator = undefined,
ast: *const Ast,
output: std.ArrayList(u8),
writer: std.ArrayList(u8).Writer,
indent_level: usize,

const Self = @This();
const Error = std.ArrayList(u8).Writer.Error;
const spaces: []const u8 = " " ** 1024;
const INDENT_SIZE = 4;

pub fn init(allocator: Allocator, ast: *const Ast) Self {
    return .{
        .allocator = allocator,
        .ast = ast,
        .output = .empty,
        .indent_level = 1,
        .writer = undefined,
    };
}

pub fn render(self: *Self) Error![]const u8 {
    self.writer = self.output.writer(self.allocator);
    try self.writer.writeAll("{\n");

    for (self.ast.nodes, 0..) |*node, i| {
        try self.renderNode(node, i != self.ast.nodes.len - 1);
    }

    self.indent_level -= 1;
    try self.writer.writeAll("}\n");

    return self.output.items;
}

fn renderNode(self: *Self, node: *const Ast.Node, comma: bool) Error!void {
    switch (node.*) {
        .assignment => |*n| {
            try self.openKey(@tagName(node.*), .block);
            try self.renderSingleExpr("assigne", n.assigne, .block, true);
            try self.renderSingleExpr("value", n.value, .block, false);
            try self.closeKey(.block, comma);
        },
        .@"continue" => |n| {
            try self.openKey(@tagName(node.*), .block);
            try self.pushKeyValue("label", if (n.label) |l| self.ast.toSource(l) else "null", false);
            try self.closeKey(.block, comma);
        },
        .discard => |n| {
            try self.renderSingleExpr(@tagName(node.*), n, .block, comma);
        },
        .enum_decl => |n| {
            try self.openKey(if (n.is_err) "error" else "enum", .block);
            if (n.name) |name| try self.pushKeyValue("name", self.ast.toSource(name), true);

            if (n.tags.len > 0) {
                try self.openKey("tags", .list);
                for (n.tags, 0..) |tag, i| {
                    const ty = if (tag.payload) |payload| try self.renderType(payload) else "";
                    try self.pushKeyValue(self.ast.toSource(tag.name), ty, i < n.tags.len - 1);
                }
                try self.closeKey(.list, false);
            }

            try self.renderFnDecls(n.functions);
            try self.closeKey(.block, comma);
        },
        .for_loop => |n| {
            try self.openKey(@tagName(node.*), .block);
            try self.pushKeyValue("name", self.ast.toSource(n.binding), true);
            try self.pushKeyValue("index", if (n.index_binding) |index| self.ast.toSource(index) else "null", true);
            try self.renderSingleExpr("value", n.expr, .block, true);
            try self.renderBlock(&n.body, "body", false);
            try self.closeKey(.block, comma);
        },
        .fn_decl => |*n| try self.renderFnDecl(self.ast.toSource(n.name), n, comma),
        .multi_var_decl => |n| {
            try self.openKey(@tagName(node.*), .list);
            for (n.decls, 0..) |*decl, i| {
                try self.openKey("var_decl", .block);
                try self.renderNameTypeValue(decl, i != n.decls.len - 1);
                try self.closeKey(.block, comma);
            }
            try self.closeKey(.list, comma);
        },
        .print => |n| {
            try self.renderSingleExpr(@tagName(node.*), n, .block, comma);
        },
        .struct_decl => |n| {
            try self.openKey(@tagName(node.*), .block);
            try self.pushKeyValue("name", self.ast.toSource(n.name), true);

            if (n.fields.len == 0) {
                try self.emptyKey("fields", .list, true);
            } else {
                try self.openKey("fields", .list);
                for (n.fields, 0..) |*v, i| {
                    const last = i != n.fields.len - 1;
                    try self.openAnonKey(.block);
                    try self.renderNameTypeValue(v, false);
                    try self.closeKey(.block, last);
                }
                try self.closeKey(.list, true);
            }

            try self.renderFnDecls(n.functions);

            try self.closeKey(.block, comma);
        },
        .use => |n| {
            try self.openKey("use", .block);

            try self.openKey("path", .list);
            for (n.names, 0..) |name, i| {
                const last = i != n.names.len - 1;
                try self.indent();
                try self.writer.print("\"{s}\"", .{self.ast.toSource(name)});
                try self.finishPush(last);
            }
            try self.closeKey(.list, true);

            if (n.items) |items| {
                try self.openKey("items", .list);
                for (items, 0..) |item, i| {
                    const last = i != items.len - 1;
                    try self.openAnonKey(.block);
                    try self.pushKeyValue("item", self.ast.toSource(item.item), true);
                    try self.pushKeyValue("alias", if (item.alias) |alias| self.ast.toSource(alias) else "", false);
                    try self.closeKey(.block, last);
                }
                try self.closeKey(.list, true);
            } else try self.emptyKey("items", .list, true);

            try self.pushKeyValue("alias", if (n.alias) |alias| self.ast.toSource(alias) else "", false);
            try self.closeKey(.block, comma);
        },
        .var_decl => |*n| {
            try self.openKey("var_decl", .block);
            try self.renderNameTypeValue(n, true);
            try self.pushKeyValue("const", if (n.is_const) "true" else "false", false);
            try self.closeKey(.block, comma);
        },
        .@"while" => |*n| {
            try self.openKey("while", .block);
            try self.pattern(n.pattern, true);
            try self.renderBlock(&n.body, "body", false);
            try self.closeKey(.block, comma);
        },
        .expr => |n| try self.renderExpr(n, comma),
    }
}

fn renderFnDecls(self: *Self, decls: []const Ast.FnDecl) !void {
    if (decls.len == 0) {
        try self.emptyKey("functions", .list, false);
    } else {
        try self.openKey("functions", .list);
        for (decls, 0..) |*f, i| {
            const last = i != decls.len - 1;
            try self.renderFnDecl(self.ast.toSource(f.name), f, last);
        }
        try self.closeKey(.list, false);
    }
}

fn renderSingleNode(self: *Self, name: ?[]const u8, node: *const Ast.Node, tag: KeyTag, comma: bool) !void {
    if (name) |n| {
        try self.openKey(n, tag);
    } else {
        try self.openAnonKey(tag);
    }
    try self.renderNode(node, false);
    try self.closeKey(tag, comma);
}

fn renderFnDecl(self: *Self, name: []const u8, decl: *const Ast.FnDecl, comma: bool) !void {
    try self.openKey(if (decl.is_closure) "closure_decl" else "fn_decl", .block);
    try self.pushKeyValue("name", name, true);

    if (decl.params.len == 0) {
        try self.emptyKey("params", .list, true);
    } else {
        try self.openKey("params", .list);
        for (decl.params, 0..) |p, i| {
            const last = i != decl.params.len - 1;
            try self.openAnonKey(.block);
            try self.pushKeyValue("name", self.ast.toSource(p.name), true);
            if (p.typ) |typ| {
                try self.pushKeyValue("type", try self.renderType(typ), true);
            } else try self.pushKeyValue("type", "void", true);
            if (p.value) |val| {
                try self.renderSingleExpr("value", val, .block, false);
            } else try self.emptyKey("value", .block, false);
            try self.closeKey(.block, last);
        }
        try self.closeKey(.list, true);
    }

    try self.pushKeyValue("return_type", if (decl.return_type) |ret| try self.renderType(ret) else "void", true);
    try self.renderAnonBlock(&decl.body, "body", false);
    try self.closeKey(.block, comma);
}

fn renderNameTypeValue(self: *Self, decl: *const Ast.VarDecl, comma: bool) !void {
    try self.pushKeyValue("name", self.ast.toSource(decl.name), true);

    if (decl.typ) |t| {
        try self.pushKeyValue("type", try self.renderType(t), true);
    } else try self.emptyKey("type", .block, true);

    if (decl.value) |val| {
        try self.renderSingleExpr("value", val, .block, comma);
    } else try self.emptyKey("value", .block, comma);
}

fn renderType(self: *Self, typ: ?*Ast.Type) Error![]const u8 {
    const ty = typ orelse return "";

    var buf: std.ArrayList(u8) = .empty;

    switch (ty.*) {
        .array => |t| {
            try buf.appendSlice(self.allocator, "[]");
            try buf.appendSlice(self.allocator, try self.renderType(t.child));
        },
        .error_union => |t| {
            try buf.appendSlice(self.allocator, try self.renderType(t.ok));
            try buf.appendSlice(self.allocator, "|");
            for (t.errs, 0..) |err, i| {
                try buf.appendSlice(self.allocator, self.ast.toSource(err));
                if (i < t.errs.len - 1) try buf.appendSlice(self.allocator, "|");
            }
        },
        .fields => |fields| {
            for (fields, 0..) |f, i| {
                try buf.appendSlice(self.allocator, self.ast.toSource(f));
                if (i < fields.len - 1) {
                    try buf.appendSlice(self.allocator, ".");
                }
            }
        },
        .function => |t| {
            try buf.appendSlice(self.allocator, "fn(");

            if (t.params.len != 0) {
                for (t.params, 0..) |p, i| {
                    try buf.appendSlice(self.allocator, try self.renderType(p));
                    if (i != t.params.len - 1) {
                        try buf.appendSlice(self.allocator, ", ");
                    }
                }
            }

            try buf.appendSlice(self.allocator, ") -> ");
            if (t.return_type) |ret| {
                try buf.appendSlice(self.allocator, try self.renderType(ret));
            } else try buf.appendSlice(self.allocator, "void");
        },
        .optional => |t| try buf.print(self.allocator, "?{s}", .{try self.renderType(t.child)}),
        .scalar => |t| try buf.appendSlice(self.allocator, self.ast.toSource(t)),
        .@"union" => |types| {
            for (types, 0..) |t, i| {
                try buf.appendSlice(self.allocator, try self.renderType(t));
                if (i < types.len - 1) try buf.appendSlice(self.allocator, "|");
            }
        },
        .self => try buf.appendSlice(self.allocator, "Self"),
    }

    return try buf.toOwnedSlice(self.allocator);
}

fn renderExpr(self: *Self, expr: *const Ast.Expr, comma: bool) Error!void {
    switch (expr.*) {
        .array => |*e| {
            if (e.values.len == 0)
                try self.emptyKey("array", .list, comma)
            else {
                try self.openKey("array", .list);
                for (e.values, 0..) |val, i| {
                    try self.renderExpr(val, i != e.values.len - 1);
                }
                try self.closeKey(.list, comma);
            }
        },
        .block => |*e| try self.renderBlock(e, null, comma),
        .binop => |*e| {
            try self.openKey(@tagName(expr.*), .block);
            try self.renderSingleExpr("lhs", e.lhs, .block, true);
            try self.renderSingleExpr("rhs", e.rhs, .block, true);
            try self.pushKeyValue("op", switch (e.op) {
                .dot_dot => "..",
                .in => "in",
                .greater => ">",
                .greater_equal => ">=",
                .less => "<",
                .less_equal => "<=",
                .bang_bang => "!!",
                .bang_equal => "!=",
                .equal_equal => "==",
                .@"and", .@"or" => |tag| @tagName(tag),
                .plus => "+",
                .modulo => "%",
                .minus => "-",
                .question_mark_question_mark => "??",
                .star => "*",
                .slash => "/",
                else => unreachable,
            }, false);
            try self.closeKey(.block, comma);
        },
        .bool => |e| try self.pushKeyValue(@tagName(expr.*), self.ast.toSource(e), comma),
        .@"break" => |e| {
            if (e.label == null and e.expr == null) {
                try self.emptyKey("break", .block, comma);
                return;
            }

            try self.openKey(@tagName(expr.*), .block);
            if (e.label) |label| try self.pushKeyValue("label", self.ast.toSource(label), e.expr != null);
            if (e.expr) |data| try self.renderExpr(data, false);
            try self.closeKey(.block, comma);
        },
        .closure => |*e| try self.renderFnDecl("", e, comma),
        .enum_lit => |e| try self.pushKeyValue("enum_literal", self.ast.toSource(e), comma),
        .fail => |e| try self.renderSingleExpr(@tagName(expr.*), e.expr, .block, comma),
        .float => |e| try self.pushKeyValue(@tagName(expr.*), self.ast.toSource(e), comma),
        .field => |e| {
            try self.openKey(@tagName(expr.*), .block);
            try self.renderSingleExpr("structure", e.structure, .block, true);
            try self.pushKeyValue("field_name", self.ast.toSource(e.field), false);
            try self.closeKey(.block, comma);
        },
        .fn_call => |e| {
            try self.openKey(@tagName(expr.*), .block);
            try self.renderSingleExpr("callee", e.callee, .block, true);

            if (e.args.len == 0) {
                try self.emptyKey("args", .list, false);
            } else {
                try self.openKey("args", .list);
                for (e.args, 0..) |arg, i| {
                    if (arg.name) |name| {
                        try self.openAnonKey(.block);
                        try self.pushKeyValue("name", self.ast.toSource(name), true);
                        try self.renderExpr(arg.value, false);
                        try self.closeKey(.block, i != e.args.len - 1);
                    } else {
                        try self.renderExpr(arg.value, i != e.args.len - 1);
                    }
                }
                try self.closeKey(.list, false);
            }

            try self.closeKey(.block, comma);
        },
        .grouping => |e| {
            try self.renderSingleExpr(@tagName(expr.*), e.expr, .block, comma);
        },
        .identifier => |e| try self.pushKeyValue(@tagName(expr.*), self.ast.toSource(e), comma),
        .@"if" => |e| {
            try self.openKey(@tagName(expr.*), .block);
            try self.pattern(e.pattern, true);
            try self.renderSingleNode("then", &e.then, .block, true);
            if (e.@"else") |*data| {
                try self.renderSingleNode("else", data, .block, comma);
            }
            try self.closeKey(.block, comma);
        },
        .indexing => |*e| {
            try self.openKey("indexing", .block);
            try self.renderSingleExpr("expr", e.expr, .block, true);
            try self.renderSingleExpr("index", e.index, .block, false);
            try self.closeKey(.block, comma);
        },
        .int => |e| try self.pushKeyValue(@tagName(expr.*), self.ast.toSource(e), comma),
        .self => try self.pushKeyValue(@tagName(expr.*), "{}", comma),
        .string => |e| {
            const text = self.ast.toSource(e);
            try self.pushKeyValue(@tagName(expr.*), text[1 .. text.len - 1], comma);
        },
        .null => try self.pushKeyValue(@tagName(expr.*), "{}", comma),
        .match => |n| try self.match(n, comma),
        .pattern => |n| try self.pattern(n, comma),
        .@"return" => |e| {
            if (e.expr) |data| {
                try self.renderSingleExpr(@tagName(expr.*), data, .block, comma);
            } else try self.emptyKey("return", .block, comma);
        },
        .struct_literal => |e| {
            try self.openKey(@tagName(expr.*), .block);
            try self.renderSingleExpr("structure", e.structure, .block, true);

            if (e.fields.len == 0) {
                try self.emptyKey("fields_values", .list, false);
            } else {
                try self.openKey("fields_values", .list);
                for (e.fields, 0..) |fv, i| {
                    const last = i != e.fields.len - 1;
                    try self.openAnonKey(.block);
                    try self.pushKeyValue("name", self.ast.toSource(fv.name), true);
                    if (fv.value) |v| {
                        try self.renderSingleExpr("value", v, .block, false);
                    } else try self.emptyKey("value", .block, false);

                    try self.closeKey(.block, last);
                }
                try self.closeKey(.list, false);
            }
            try self.closeKey(.block, comma);
        },
        .ternary => |e| {
            try self.openKey(@tagName(expr.*), .block);
            try self.renderSingleExpr("condition", e.condition, .block, true);
            try self.renderSingleExpr("then", e.then, .block, true);
            try self.renderSingleExpr("else", e.@"else", .block, false);
            try self.closeKey(.block, comma);
        },
        .trap => |e| {
            try self.openKey(@tagName(expr.*), .block);
            try self.renderSingleExpr("lhs", e.lhs, .block, true);
            try self.openKey("rhs", .block);
            switch (e.rhs) {
                .match => |m| try self.match(m.match, false),
                .binding => |b| {
                    try self.openKey("binding", .block);
                    try self.pushKeyValue("name", if (b.token) |t| self.ast.toSource(t) else "_", true);
                    try self.renderSingleExpr("body", b.body, .block, false);
                    try self.closeKey(.block, false);
                },
            }
            try self.closeKey(.block, false);
            try self.closeKey(.block, comma);
        },
        .unary => |e| {
            try self.openKey(@tagName(expr.*), .block);
            try self.pushKeyValue("op", self.ast.toSource(e.op), true);
            try self.renderSingleExpr("expr", e.expr, .block, false);
            try self.closeKey(.block, comma);
        },
    }
}

fn renderSingleExpr(self: *Self, name: ?[]const u8, expr: *const Ast.Expr, tag: KeyTag, comma: bool) !void {
    if (name) |n| {
        try self.openKey(n, tag);
    } else {
        try self.openAnonKey(tag);
    }
    try self.renderExpr(expr, false);
    try self.closeKey(tag, comma);
}

fn renderBlock(self: *Self, block: *const Ast.Block, name: ?[]const u8, comma: bool) !void {
    if (block.nodes.len == 0) {
        if (block.label) |label| {
            try self.openKey(name orelse "block", .block);
            try self.pushKeyValue("label", self.ast.toSource(label), false);
            try self.closeKey(.block, comma);
        } else try self.emptyKey(name orelse "block", .block, comma);
    } else {
        try self.openKey(name orelse "block", .block);
        if (block.label) |label| try self.pushKeyValue("label", self.ast.toSource(label), true);
        try self.openKey("exprs", .list);
        for (block.nodes, 0..) |*data, i| {
            try self.renderNode(data, i != block.nodes.len - 1);
        }
        try self.closeKey(.list, false);
        try self.closeKey(.block, comma);
    }
}

fn renderAnonBlock(self: *Self, block: *const Ast.Block, name: ?[]const u8, comma: bool) !void {
    if (block.nodes.len == 0) {
        try self.emptyKey(name orelse "block", .list, comma);
    } else {
        try self.openKey(name orelse "block", .list);
        for (block.nodes, 0..) |*data, i| {
            try self.renderNode(data, i != block.nodes.len - 1);
        }
        try self.closeKey(.list, comma);
    }
}

fn match(self: *Self, expr: Ast.Match, comma: bool) !void {
    try self.openKey("match", .block);
    try self.renderSingleExpr("expression", expr.expr, .block, true);

    try self.openKey("arms", .list);
    switch (expr.body) {
        .value => |arms| try self.matchValue(arms),
        .type => |arms| try self.matchType(arms),
    }
    try self.closeKey(.list, false);

    try self.closeKey(.block, comma);
}

fn matchValue(self: *Self, expr: Ast.Match.ValueMatch) !void {
    for (expr.arms, 0..) |arm, i| {
        try self.matchValueArm(arm, expr.wildcard != null or i < expr.arms.len - 1);
    }
    if (expr.wildcard) |w| {
        try self.matchWildcard(w, false);
    }
}

fn matchValueArm(self: *Self, arm: Ast.Match.ValueArm, comma: bool) !void {
    try self.openAnonKey(.block);
    try self.renderSingleExpr("expr", arm.expr, .block, true);
    if (arm.alias) |alias| {
        try self.pushKeyValue("alias", self.ast.toSource(alias), true);
    }
    try self.renderSingleNode("body", &arm.body, .block, false);
    try self.closeKey(.block, comma);
}

fn matchWildcard(self: *Self, arm: Ast.Match.Wildcard, comma: bool) !void {
    try self.openKey("wildcard", .block);
    if (arm.alias) |alias| {
        try self.pushKeyValue("alias", self.ast.toSource(alias), true);
    }
    try self.renderSingleNode("body", &arm.body, .block, false);
    try self.closeKey(.block, comma);
}

fn matchType(self: *Self, expr: Ast.Match.TypeMatch) !void {
    for (expr.arms, 0..) |arm, i| {
        try self.matchTypeArm(arm, expr.wildcard != null or i < expr.arms.len - 1);
    }
    if (expr.wildcard) |w| {
        try self.matchWildcard(w, false);
    }
}

fn matchTypeArm(self: *Self, arm: Ast.Match.TypeArm, comma: bool) !void {
    try self.openAnonKey(.block);
    try self.pushKeyValue("type", try self.renderType(arm.type), true);
    switch (arm.body) {
        .value => |*n| try self.renderSingleNode("body", n, .block, false),
        .patmat => |sub_arms| {
            try self.openKey("arms", .list);
            try self.matchValue(sub_arms);
            try self.closeKey(.list, false);
        },
    }
    try self.closeKey(.block, comma);
}

fn pattern(self: *Self, pat: Ast.Pattern, comma: bool) !void {
    switch (pat) {
        .value => |v| {
            try self.openKey("pattern: value", .block);

            try self.renderSingleExpr("expr", v.expr, .block, v.alias != null);
            if (v.alias) |alias| {
                try self.pushKeyValue("alias", self.ast.toSource(alias), false);
            }
        },
        .nullable => |v| {
            try self.openKey("pattern: nullable", .block);
            try self.pushKeyValue("binding", self.ast.toSource(v.binding), true);
            try self.renderSingleExpr("expr", v.expr, .block, true);
            try self.pushKeyValue("constant", if (self.ast.token_tags[v.token] == .let) "true" else "false", false);
        },
    }

    try self.closeKey(.block, comma);
}

const KeyTag = enum {
    block,
    list,

    pub fn toOpenStr(self: KeyTag) []const u8 {
        return switch (self) {
            .block => "{",
            .list => "[",
        };
    }

    pub fn toCloseStr(self: KeyTag) []const u8 {
        return switch (self) {
            .block => "}",
            .list => "]",
        };
    }
};

fn openKey(self: *Self, key: []const u8, tag: KeyTag) !void {
    try self.indent();
    try self.writer.print("\"{s}\": {s}\n", .{ key, tag.toOpenStr() });
    self.indent_level += 1;
}

fn openAnonKey(self: *Self, tag: KeyTag) !void {
    try self.indent();
    try self.writer.print("{s}\n", .{tag.toOpenStr()});
    self.indent_level += 1;
}

fn closeKey(self: *Self, tag: KeyTag, comma: bool) !void {
    self.indent_level -= 1;
    try self.indent();
    try self.writer.print("{s}", .{tag.toCloseStr()});
    try self.finishPush(comma);
}

fn emptyKey(self: *Self, key: []const u8, tag: KeyTag, comma: bool) !void {
    try self.indent();
    try self.writer.print("\"{s}\": {s}{s}", .{ key, tag.toOpenStr(), tag.toCloseStr() });
    try self.finishPush(comma);
}

fn pushKeyValue(self: *Self, key: []const u8, value: []const u8, comma: bool) !void {
    try self.indent();
    try self.writer.print("\"{s}\": \"{s}\"", .{ key, value });
    try self.finishPush(comma);
}

fn finishPush(self: *Self, comma: bool) !void {
    try self.writer.print("{s}\n", .{if (comma) "," else ""});
}

fn indent(self: *Self) !void {
    assert(self.indent_level * 2 < 1024);
    try self.output.appendSlice(self.allocator, spaces[0 .. self.indent_level * INDENT_SIZE]);
}
