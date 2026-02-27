const ConstIdx = @import("ConstantInterner.zig").ConstIdx;
const ModIndex = @import("../pipeline/ModuleManager.zig").Index;

pub const Scope = enum { builtin, global, local };
pub const Type = enum(u2) { float, int };

pub const Index = usize;

pub const TypeId = @import("types.zig").TypeId;
pub const SymbolIndex = usize;

pub const Instruction = struct {
    data: Data,
    offset: usize,

    pub const Data = union(enum) {
        array: Array,
        assignment: Assignment,
        binop: Binop,
        block: Block,
        box: Index,
        bound_method: BoundMethod,
        @"break": Break,
        call: Call,
        @"continue": Continue,
        constant: Constant,
        discard: Index,
        enum_create: EnumCreate,
        enum_decl: EnumDecl,
        fail: Return,
        field: Field,
        fn_decl: FnDecl,
        for_loop: For,
        identifier: Variable,
        @"if": If,
        in: In,
        incr_rc: Index,
        indexing: Indexing,
        load_symbol: LoadSymbol,
        load_builtin: usize,
        match: Match,
        multiple_var_decl: MultiVarDecl,
        obj_func: ObjFn,
        pat_nullable: Index,
        pop: Index,
        print: Index,
        range: Range,
        @"return": Return,
        struct_decl: StructDecl,
        struct_literal: StructLiteral,
        trap: Trap,
        unary: Unary,
        unbox: Index,
        var_decl: VarDecl,
        when: When,
        @"while": While,

        noop, // Used only by 'use' statements as they don't produce any instructions
    };

    pub const Indexing = struct {
        expr: Index,
        index: Index,
        kind: Kind,
        index_kind: IndexKind,

        pub const Kind = enum { array, str };
        pub const IndexKind = enum { scalar, range };
    };
    pub const Binop = struct {
        lhs: Index,
        rhs: Index,
        op: Op,

        pub const Op = enum {
            add_float,
            add_int,
            add_str,
            @"and",
            bang_bang,
            div_float,
            div_int,
            eq_bool,
            eq_float,
            eq_int,
            eq_null,
            eq_str,
            eq_tag,
            ge_float,
            ge_int,
            gt_float,
            gt_int,
            le_float,
            le_int,
            lt_float,
            lt_int,
            mod_float,
            mod_int,
            mul_float,
            mul_int,
            mul_str,
            ne_bool,
            ne_float,
            ne_int,
            ne_null,
            ne_str,
            ne_tag,
            @"or",
            sub_float,
            sub_int,
        };
    };

    pub const Array = struct {
        values: []const Index,
    };
    pub const Assignment = struct {
        assigne: Index,
        value: Index,
        cow: bool,
    };
    pub const Block = struct {
        instrs: []const Index,
        pop_count: u8,
        is_expr: bool,
    };
    pub const BoundMethod = struct { structure: Index, index: usize };
    pub const Break = struct { instr: ?Index, depth: usize, pop_count: usize };
    pub const Call = struct {
        callee: Index,
        args: []const Arg,
        ext_mod: ?ModIndex,
        native: bool,
    };
    pub const Arg = union(enum) {
        instr: Index,
        default: struct {
            const_index: ConstIdx,
            mod: ?ModIndex,
        },
    };
    pub const Continue = struct { depth: usize, pop_count: usize };
    pub const Constant = struct {
        index: ConstIdx,
    };
    pub const EnumCreate = struct {
        sym: LoadSymbol,
        tag_index: usize,
    };
    pub const EnumDecl = struct {
        name: usize,
        sym_index: SymbolIndex,
        functions: []const Index,
        is_err: bool,
    };
    pub const Field = struct {
        structure: Index,
        index: usize,
        kind: Kind,

        pub const Kind = enum { field, function };
    };
    pub const FnDecl = struct {
        sym_index: SymbolIndex,
        type_id: TypeId,
        name: ?usize,
        body: []const Index,
        defaults: []const Index,
        captures: []const Capture,
        returns: bool,

        pub const Capture = struct { index: usize, local: bool };
    };
    pub const For = struct {
        expr: Index,
        body: Index,
        kind: Kind,
        use_index: bool,

        pub const Kind = enum { array, str, range };
    };
    pub const If = struct {
        cond: Index,
        then: Index,
        @"else": ?Index,
    };
    pub const In = struct {
        needle: Index,
        haystack: Index,
        kind: Kind,

        pub const Kind = enum { array, range_int, range_float, string };
    };
    pub const LoadSymbol = struct {
        module_index: ?ModIndex,
        symbol_index: u8,
    };
    pub const Match = struct {
        expr: Index,
        arms: []const Arm,
        wildcard: ?Index,
        is_expr: bool,
        kind: Kind,

        pub const Arm = struct { expr: Index, body: Index };
        pub const Kind = enum { bool, float, int, @"enum", string };
    };
    pub const MultiVarDecl = struct { decls: []const Index };
    pub const ObjFn = struct {
        obj: Index,
        fn_index: usize,
        kind: Kind,

        pub const Kind = enum { array, string };
    };
    pub const Range = struct {
        start: Index,
        end: Index,
        kind: Kind,

        pub const Kind = enum { int, float };
    };
    pub const Return = struct { value: ?Index };
    pub const StructDecl = struct {
        // Interner index
        name: usize,
        sym_index: SymbolIndex,
        type_id: TypeId,
        fields_count: usize,
        default_fields: []const Index,
        functions: []const Index,
    };
    pub const StructLiteral = struct {
        structure: Index,
        values: []const Arg,
    };
    pub const Trap = struct {
        lhs: Index,
        rhs: Index,
    };
    pub const Unary = struct {
        op: Op,
        typ: Type,
        instr: Index,

        pub const Op = enum { minus, bang };
    };
    pub const VarDecl = struct {
        box: bool,
        variable: Variable,
        value: ?Index,
    };
    pub const Variable = struct {
        index: u64,
        scope: Scope,
    };
    pub const When = struct {
        expr: Index,
        arms: []const Arm,
        is_expr: bool,

        pub const Arm = struct { type_id: TypeId, body: Index };
    };
    pub const While = struct { cond: Index, body: Index };
};
