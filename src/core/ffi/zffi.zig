const std = @import("std");
const Value = @import("../runtime/values.zig").Value;
const Obj = @import("../runtime/Obj.zig");
const Vm = @import("../runtime/Vm.zig");

// -------
//  Types
// -------
pub const Int = i64;
pub const Float = f64;
pub const Bool = bool;
pub const Str = []const u8;

pub const All = union(enum) {
    bool: Bool,
    int: Int,
    float: Float,
    str: Str,
};

pub fn Union(types: []const std.meta.FieldEnum(All)) type {
    var enum_field_values: [types.len]usize = undefined;

    var fields_names: [types.len][]const u8 = undefined;
    var fields_types: [types.len]type = undefined;

    inline for (types, 0..) |t, i| {
        const name = @tagName(t);
        const T = @FieldType(All, name);
        fields_names[i] = name;
        fields_types[i] = T;
        enum_field_values[i] = i;
    }

    return @Union(
        .auto,
        @Enum(usize, .exhaustive, &fields_names, &enum_field_values),
        &fields_names,
        &fields_types,
        &@splat(.{}),
    );
}

pub const Module = struct {
    name: ?[]const u8 = null,
    is_module: bool = true,
    functions: []const FnMeta = &.{},
    structures: []const StructMeta = &.{},
    traits: []const TraitMeta = &.{},
};

fn checkNonEmptyName(comptime name: []const u8) void {
    if (name.len == 0) {
        @compileError("Name can't be empty");
    }
}

const MemberKind = enum {
    fields,
    functions,

    pub fn Type(self: MemberKind) type {
        return switch (self) {
            .fields => []const StructMeta.Field,
            .functions => []const FnMeta,
        };
    }
};

pub const FnMeta = struct {
    name: []const u8,
    params: []const Param,
    desc: []const u8,
    info: std.builtin.Type.Fn,
    function: Fn,

    pub const Param = struct {
        name: [:0]const u8,
        desc: []const u8 = "",
    };

    pub fn init(T: type, name: []const u8, desc: []const u8, params: []const Param) FnMeta {
        checkNonEmptyName(name);

        const func = @field(T, name);
        const info = @typeInfo(@TypeOf(func));
        if (info != .@"fn") {
            @compileError("Trying to declare a non-function, found: " ++ @typeName(func));
        }

        return .{
            .name = name,
            .desc = desc,
            .params = params,
            .info = info.@"fn",
            .function = makeNative(func),
        };
    }
};

pub const Fn = *const fn (*Vm, []const Value) ?Value;

fn getMember(T: type, comptime kind: MemberKind) kind.Type() {
    const info = @typeInfo(T);
    if (info != .@"struct") {
        @compileError("Trying to declare a non-structure, found: " ++ @typeName(T));
    }

    const tag = @tagName(kind);
    const Member = kind.Type();

    comptime var member: ?Member = null;

    inline for (info.@"struct".decls) |decl| {
        if (comptime std.mem.eql(u8, decl.name, tag)) {
            const field = @field(T, decl.name);

            if (@TypeOf(field) != Member) {
                @compileError("Expect member '" ++ tag ++ "' to be of type: " ++ @typeName(Member));
            }

            member = field;
        }
    }

    return member orelse {
        @compileError("Structure must define a '" ++ tag ++ "' member to register them");
    };
}

pub const StructMeta = struct {
    name: []const u8,
    type_name: []const u8,
    fields: []const Field,
    functions: []const FnMeta,

    pub const Field = struct {
        name: []const u8,
        desc: []const u8,
        type: type,

        pub fn init(T: type, name: []const u8, desc: []const u8) Field {
            return .{
                .name = name,
                .desc = desc,
                .type = @FieldType(T, name),
            };
        }
    };

    pub fn init(T: type, name: []const u8) StructMeta {
        checkNonEmptyName(name);

        return .{
            .name = name,
            .type_name = @typeName(T),
            .fields = getMember(T, .fields),
            .functions = getMember(T, .functions),
        };
    }
};

pub const VTable = struct {
    get_field: *const fn (*anyopaque, *Vm, usize) Value,
    deinit_fn: *const fn (*anyopaque, *Vm) void,

    pub fn init(T: type) *const VTable {
        return &.{
            .get_field = @field(T, "getField"),
            .deinit_fn = @field(T, "deinit"),
        };
    }
};

pub const FieldAccessor = struct {
    get: *const fn (*anyopaque, *Vm) Value,
};

pub fn makeAccessors(T: type) []const FieldAccessor {
    const fields = @field(T, "fields");
    comptime var accessors: [fields.len]FieldAccessor = undefined;

    inline for (fields, 0..) |field, i| {
        accessors[i] = .{
            .get = struct {
                fn get(self: *anyopaque, vm: *Vm) Value {
                    const s: *T = @ptrCast(@alignCast(self));
                    return toValue(vm, @field(s, field.name), .{ .copy_str = true });
                }
            }.get,
        };
    }

    const copy = accessors;
    return &copy;
}

pub fn makeObj(T: type, name: []const u8, value: *anyopaque, vm: *Vm) *T {
    const obj = Obj.NativeObj.create(vm, name, value, .init(T));
    return @ptrCast(@alignCast(&obj.child));
}

pub const TraitMeta = struct {
    name: []const u8,
    type_name: []const u8,
    functions: []const FnMeta,

    pub fn init(T: type, name: []const u8) TraitMeta {
        checkNonEmptyName(name);

        return .{
            .name = name,
            .type_name = @typeName(T),
            .functions = getMember(T, .functions),
        };
    }
};

pub fn makeNative(func: anytype) Fn {
    return struct {
        pub fn call(vm: *Vm, stack: []const Value) ?Value {
            const ArgsType, const vm_index = ArgsTuple(@TypeOf(func));
            var args: ArgsType = undefined;

            const fields = @typeInfo(ArgsType).@"struct".fields;

            comptime var offset: usize = 0;

            inline for (fields, 0..) |f, i| {
                if (i == vm_index) {
                    args[vm_index] = vm;
                    offset = 1;
                } else {
                    args[i] = fromValue(f.type, stack[i - offset]);
                }
            }

            if (@typeInfo(@TypeOf(func)).@"fn".return_type.? != void) {
                const res = @call(.auto, func, args);
                return toValue(vm, res, .{});
            } else {
                @call(.auto, func, args);
                return null;
            }
        }

        fn fromValue(T: type, value: Value) T {
            return switch (@typeInfo(T)) {
                .float => value.float,
                .int => value.int,
                .bool => value.bool,
                .@"union" => handleUnion(T, value),
                .pointer => |ptr| {
                    return switch (ptr.child) {
                        u8 => value.obj.as(Obj.String).chars,
                        anyopaque => @compileError("Can't use *anyopaque in functions"),
                        // All other native zig structures are wrapped in NativeObj
                        else => @ptrCast(@alignCast(value.asObj().?.as(Obj.NativeObj).child)),
                    };
                },
                else => @compileError("FFI: Unsupported type in auto conversion: " ++ @typeName(T)),
            };
        }

        fn handleUnion(U: type, value: Value) U {
            // Unwrap is same because type checking is done in Analyzer
            return switch (value) {
                .bool => |v| createUnionIfField(U, "bool", v),
                .int => |v| createUnionIfField(U, "int", v),
                .float => |v| createUnionIfField(U, "float", v),
                .obj => |v| createUnionIfField(U, "str", v.as(Obj.String).chars),
                else => unreachable,
            } orelse {
                std.log.debug("Found '{t}' kind", .{value});
                unreachable;
            };
        }

        fn createUnionIfField(U: type, comptime name: []const u8, value: anytype) ?U {
            if (@hasField(U, name)) {
                return @unionInit(U, name, value);
            }

            return null;
        }
    }.call;
}

const Config = struct {
    /// When dealing with value on stack (through native function calls), we always want
    /// `take` the value, meaning that if it already exists, we free the argument
    /// and take the existing interned one
    /// When dealing with native structure fields, we don't want to `take` it because it
    /// would free the value owned by the native structure
    copy_str: bool = false,
};
fn toValue(vm: *Vm, value: anytype, config: Config) Value {
    return switch (@typeInfo(@TypeOf(value))) {
        .float => .{ .float = value },
        .int => .{ .int = value },
        .bool => .{ .bool = value },
        .pointer => |ptr| return switch (ptr.child) {
            // TODO: check memory managment
            u8 => {
                const str = if (config.copy_str)
                    Obj.String.takeCopy(vm, value)
                else
                    Obj.String.take(vm, value);

                return .makeObj(str.asObj());
            },
            else => {
                const native = @as(
                    *Obj.NativeObj,
                    @alignCast(@fieldParentPtr(
                        "child",
                        @as(**anyopaque, @ptrCast(@alignCast(value))),
                    )),
                );

                return .makeObj(native.asObj());
            },
        },
        else => @compileError("FFI: Unsupported type in auto conversion: " ++ @typeName(@TypeOf(value))),
    };
}

pub fn ArgsTuple(comptime FnType: type) struct { type, usize } {
    const infos = @typeInfo(FnType);
    if (infos != .@"fn") {
        @compileError("FFI: Can't generate native function for a non-function");
    }

    const fn_infos = infos.@"fn";
    if (fn_infos.is_var_args) {
        @compileError("FFI: Can't generate native function for variadic function");
    }

    const vm_index = if (fn_infos.params[0].type.? == *Vm)
        0
    else if (fn_infos.params[1].type.? == *Vm)
        1
    else
        @compileError("Either first or second argument of functions must be of type *Vm");

    var args_type: [fn_infos.params.len]type = undefined;

    inline for (fn_infos.params, 0..) |arg, i| {
        args_type[i] = arg.type.?;
    }

    var field_types: [fn_infos.params.len]type = undefined;

    for (args_type, 0..) |T, i| {
        field_types[i] = T;
    }

    return .{
        @Tuple(&field_types),
        vm_index,
    };
}
