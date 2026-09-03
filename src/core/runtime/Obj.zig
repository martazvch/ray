const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Writer = std.Io.Writer;

const options = @import("options");

const type_mod = @import("../analyzer/types.zig");
const TypeId = type_mod.TypeId;
const ObjFnInfos = type_mod.ObjFnInfos;
const ObjFnTypeInfo = type_mod.ObjFnTypeInfo;
const ObjFns = type_mod.ObjFns;

const Chunk = @import("../compiler/Chunk.zig");
const Module = @import("../pipeline/ModuleManager.zig").Module;
const zffi = @import("../ffi/zffi.zig");
const cffi = @import("../ffi/cffi.zig");
const oom = @import("misc").oom;
const Value = @import("values.zig").Value;
const Vm = @import("Vm.zig");

kind: Kind,
next: ?*Obj,
type_id: TypeId,
is_marked: bool = false,

const Obj = @This();

const Kind = enum {
    array,
    box,
    closure,
    @"enum",
    @"error",
    function,
    cfunction,
    zig_function,
    iterator,
    pointer,
    string,
    structure,
    c_structure,
    zig_structure,
    trait_obj,
    @"union",

    pub fn fromType(T: type) Kind {
        return switch (T) {
            Array => .array,
            Box => .box,
            Closure => .closure,
            Enum => .@"enum",
            Function => .function,
            CFn => .cfunction,
            ZigFn => .zig_function,
            Iterator => .iterator,
            Pointer => .pointer,
            String => .string,
            Structure => .structure,
            CStructure => .c_structure,
            ZigStructure => .zig_structure,
            TraitObj => .trait_obj,
            Union => .@"union",
            else => @compileError(@typeName(T) ++ " isn't a runtime object type"),
        };
    }
};

// TODO: report a runtime error for OOM
pub fn allocate(vm: *Vm, comptime T: type, type_id: TypeId) *T {
    comptime assert(@hasField(T, "obj"));
    comptime assert(@hasDecl(T, "asObj"));

    const ptr = vm.gc_alloc.create(T) catch oom();
    ptr.obj = .{ .kind = .fromType(T), .next = vm.objects, .type_id = type_id };

    vm.objects = &ptr.obj;

    if (comptime options.log_gc) {
        std.debug.print("{*} allocate {} bytes for: ", .{ ptr, @sizeOf(T) });
    }

    return ptr;
}

/// Another version of allocation but don't register the object into the VM linked list.
/// Used for objects that live for ever like symbols which are created at compile time
/// Dedicated function so that we don't add a bool check at runtime when allocating with `allocate`
// TODO: report a runtime error for OOM
fn allocateComptime(allocator: Allocator, comptime T: type, type_id: TypeId) *T {
    comptime assert(@hasField(T, "obj"));
    comptime assert(@hasDecl(T, "asObj"));

    const ptr = allocator.create(T) catch oom();
    ptr.obj = .{ .kind = .fromType(T), .next = null, .type_id = type_id };

    return ptr;
}

pub inline fn as(self: *Obj, comptime T: type) *T {
    comptime assert(@hasField(T, "obj"));

    return @alignCast(@fieldParentPtr("obj", self));
}

// ----
// Api
fn BuiltinFn(T: type) type {
    return *const fn (*T, *Vm, []Value) ?Value;
}

fn getApiFns(T: type) []const BuiltinFn(T) {
    const info = @typeInfo(T).@"struct";
    comptime var fns: []const BuiltinFn(T) = &.{};

    inline for (info.decls) |decl| {
        if (comptime std.mem.startsWith(u8, decl.name, "_api_")) {
            comptime fns = fns ++ .{@field(T, decl.name)};
        }
    }

    return fns;
}

const KV = struct { []const u8, ObjFnInfos };

fn getDefApiFns(T: type) std.StaticStringMap(ObjFnInfos) {
    const info = @typeInfo(T).@"struct";
    comptime var i: usize = 0;
    comptime var kvs: []const KV = &.{};

    inline for (info.decls) |decl| {
        if (comptime std.mem.startsWith(u8, decl.name, "_api_")) {
            const name = decl.name[5..];
            const def_name = "_defapi_" ++ name;

            if (!@hasDecl(T, def_name)) {
                @compileError("Missing API type definition for function: " ++ name);
            }

            kvs = kvs ++ .{KV{ name, .{ .index = i, .type_info = @field(T, def_name) } }};
            i += 1;
        }
    }

    return .initComptime(kvs);
}

// ---------
//  Objects
pub const Array = struct {
    obj: Obj,
    values: ArrayList(Value),
    funcs: []const BuiltinFn(Self),

    const Self = @This();

    pub fn create(vm: *Vm, type_id: TypeId, values: []Value) *Self {
        const obj = Obj.allocate(vm, Self, type_id);
        obj.values = .empty;

        if (options.log_gc) std.debug.print("<array>\n", .{});

        vm.gc.pushTmpRoot(obj.asObj());
        defer vm.gc.popTmpRoot();

        obj.values.ensureTotalCapacity(vm.gc_alloc, values.len) catch oom();
        obj.funcs = getApiFns(Self);

        for (values) |val| {
            obj.values.appendAssumeCapacity(val);
        }

        return obj;
    }

    pub fn createComptime(allocator: Allocator, type_id: TypeId, values: []Value) *Self {
        const obj = Obj.allocateComptime(allocator, Self, type_id);
        obj.values = .fromOwnedSlice(values);
        obj.funcs = getApiFns(Self);

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deepCopy(self: *const Self, vm: *Vm) *Self {
        var values: ArrayList(Value) = .empty;
        values.ensureTotalCapacity(vm.allocator, self.values.items.len) catch oom();

        const index = vm.gc.tmp_roots.items.len;
        defer vm.gc.tmp_roots.shrinkRetainingCapacity(index);

        vm.gc.tmp_roots.ensureUnusedCapacity(vm.allocator, self.values.items.len) catch oom();

        for (self.values.items) |val| {
            const value = val.deepCopy(vm);
            values.appendAssumeCapacity(value);
            if (value.asObj()) |obj| vm.gc.tmp_roots.appendAssumeCapacity(obj);
        }

        return Self.create(vm, self.obj.type_id, values.toOwnedSlice(vm.allocator) catch oom());
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        // We don't own the values, just the array
        self.values.deinit(vm.gc_alloc);
        vm.gc_alloc.destroy(self);
    }

    // Api
    pub fn call(self: *Self, vm: *Vm, stack: []Value, fn_index: usize) ?Value {
        return self.funcs[fn_index](self, vm, stack);
    }

    pub fn getFns() ObjFns {
        return getDefApiFns(Self);
    }

    pub const _defapi_push: ObjFnTypeInfo = .{
        .params = &.{.{ .name = "value", .ty = .generic }},
        .return_type = .void,
    };
    pub fn _api_push(self: *Self, vm: *Vm, stack: []Value) ?Value {
        self.values.append(vm.gc_alloc, stack[0]) catch oom();
        return null;
    }

    pub const _defapi_pop: ObjFnTypeInfo = .{ .params = &.{}, .return_type = .generic };
    pub fn _api_pop(self: *Self, _: *Vm, _: []Value) ?Value {
        return self.values.pop();
    }

    pub const _defapi_len: ObjFnTypeInfo = .{ .params = &.{}, .return_type = .int };
    pub fn _api_len(self: *Self, _: *Vm, _: []Value) ?Value {
        return .makeInt(@intCast(self.values.items.len));
    }
};

pub const String = struct {
    obj: Obj,
    chars: []const u8,
    hash: u32,
    funcs: []const BuiltinFn(Self),

    const Self = @This();

    fn create(vm: *Vm, str: []const u8, hash: u32) *String {
        var obj = Obj.allocate(vm, Self, undefined);
        obj.chars = str;
        obj.hash = hash;
        obj.funcs = getApiFns(Self);

        // The set method can trigger a GC to grow hashmap before
        // inserting. We put the value on the stack so that it is marked
        // as a root
        vm.gc.pushTmpRoot(obj.asObj());
        defer vm.gc.popTmpRoot();
        vm.strings.put(vm.allocator, hash, obj) catch oom();

        if (options.log_gc) obj.asObj().log();

        return obj;
    }

    /// **Warning**: Meant to be used at compile time only
    pub fn comptimeCopy(allocator: Allocator, interned: *std.AutoHashMapUnmanaged(usize, *Obj.String), str: []const u8) *String {
        const hash = String.hashString(str);
        const gop = interned.getOrPut(allocator, hash) catch oom();
        // TODO: as we intern constants at compile time, we are sure to not find an existing one
        if (gop.found_existing) {
            return gop.value_ptr.*;
        }

        const chars = allocator.alloc(u8, str.len) catch oom();
        @memcpy(chars, str);

        var obj = Obj.allocateComptime(allocator, Self, undefined);
        obj.chars = chars;
        obj.hash = hash;
        obj.funcs = getApiFns(Self);

        // Comptime constants have maximum lifetime
        obj.obj.is_marked = true;

        gop.value_ptr.* = obj;

        return obj;
    }

    /// Take a string allocated by calling Vm. If interned already, free
    /// the memory and return the interned one
    pub fn take(vm: *Vm, str: []const u8) *String {
        const hash = String.hashString(str);
        if (vm.strings.get(hash)) |interned| {
            vm.gc_alloc.free(str);
            return interned;
        }

        return String.create(vm, str, hash);
    }

    /// Take a string. If interned already, returns the interned one
    pub fn takeCopy(vm: *Vm, str: []const u8) *String {
        const hash = String.hashString(str);
        if (vm.strings.get(hash)) |interned| {
            return interned;
        }

        return String.create(vm, vm.gc_alloc.dupe(u8, str) catch oom(), hash);
    }

    pub fn asObj(self: *String) *Obj {
        return &self.obj;
    }

    fn hashString(chars: []const u8) u32 {
        var hash: u32 = 2166136261;
        for (chars) |c| {
            hash ^= c;
            hash *%= 16777619;
        }

        return hash;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.free(self.chars);
        allocator.destroy(self);
    }

    // Api
    pub fn call(self: *Self, vm: *Vm, stack: []Value, fn_index: usize) ?Value {
        return self.funcs[fn_index](self, vm, stack);
    }

    pub fn getFns() ObjFns {
        return getDefApiFns(Self);
    }

    pub const _defapi_len: ObjFnTypeInfo = .{ .params = &.{}, .return_type = .int };
    pub fn _api_len(self: *Self, _: *Vm, _: []Value) ?Value {
        return .makeInt(@intCast(self.chars.len));
    }

    pub const _defapi_split: ObjFnTypeInfo = .{
        .params = &.{.{ .name = "splitter", .ty = .str }},
        .return_type = .array_str,
    };
    pub fn _api_split(self: *Self, vm: *Vm, stack: []Value) ?Value {
        const sep = stack[0].obj.as(String).chars;
        var split = std.mem.splitSequence(u8, self.chars, sep);

        var chunks: ArrayList(Value) = .empty;
        defer chunks.deinit(vm.allocator);

        while (split.next()) |chunk| {
            const s = String.takeCopy(vm, chunk).asObj();
            chunks.append(vm.allocator, .makeObj(s)) catch oom();
            vm.gc.pushTmpBuf(chunks.items);
        }
        defer vm.gc.popTmpBuf();

        return .makeObj(Array.create(vm, vm.arr_str_type_id, chunks.items).asObj());
    }
};

pub const Function = struct {
    obj: Obj,
    chunk: Chunk,
    name: []const u8,
    module_index: usize,

    const Self = @This();

    pub fn create(allocator: Allocator, name: []const u8, type_id: TypeId, module_index: usize) *Self {
        const obj = Obj.allocateComptime(allocator, Self, type_id);
        obj.chunk = .empty;
        obj.name = allocator.dupe(u8, name) catch oom();
        obj.module_index = module_index;

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        self.chunk.deinit(vm.allocator);
        // Name already in the linked list, don't free manually
        vm.gc_alloc.destroy(self);
    }
};

pub const Closure = struct {
    obj: Obj,
    function: *Function,
    captures: []Value,

    const Self = @This();

    pub fn create(vm: *Vm, function: *Function, captures: []Value) *Self {
        const obj = Obj.allocate(vm, Self, undefined);
        vm.gc.pushTmpRoot(&obj.obj);
        defer vm.gc.popTmpRoot();

        // Fix the size to zero to avoid GC bug
        obj.captures.len = 0;
        obj.function = function;
        obj.captures = vm.gc_alloc.alloc(Value, captures.len) catch oom();
        @memcpy(obj.captures, captures);

        if (options.log_gc) obj.asObj().log();

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        vm.gc_alloc.free(self.captures);
        vm.gc_alloc.destroy(self);
    }
};

pub const Box = struct {
    obj: Obj,
    value: Value,

    const Self = @This();

    pub fn create(vm: *Vm, value: Value) *Self {
        const obj = Obj.allocate(vm, Self, undefined);
        obj.value = value;

        if (options.log_gc) obj.asObj().log();

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        vm.gc_alloc.destroy(self);
    }
};

pub const ZigFn = struct {
    obj: Obj,
    name: []const u8,
    function: zffi.Fn,

    const Self = @This();

    pub fn create(allocator: Allocator, name: []const u8, function: zffi.Fn) *Self {
        const obj = Obj.allocateComptime(allocator, Self, undefined);
        obj.name = name;
        obj.function = function;

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.destroy(self);
    }
};

pub const CFn = struct {
    obj: Obj,
    name: []const u8,
    function: cffi.Fn,
    returns: bool,

    const Self = @This();

    pub fn create(allocator: Allocator, name: []const u8, function: cffi.Fn, returns: bool) *Self {
        const obj = Obj.allocateComptime(allocator, Self, undefined);
        obj.name = name;
        obj.function = function;
        obj.returns = returns;

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.destroy(self);
    }
};

pub const Structure = struct {
    obj: Obj,
    parent: *const Module.Structure,
    fields: []Value,

    const Self = @This();

    pub fn create(vm: *Vm, parent: *const Module.Structure) *Self {
        // Fields first for GC because other wise allocating fields after creation
        // of the instance may trigger GC in between
        const alloc_fields = vm.gc_alloc.alloc(Value, parent.field_count) catch oom();
        const obj = Obj.allocate(vm, Self, parent.type_id);

        obj.parent = parent;
        obj.fields = alloc_fields;

        if (options.log_gc) obj.asObj().log();

        return obj;
    }

    pub fn createComptime(alloc: Allocator, parent: *const Module.Structure, fields: []const Value) *Self {
        const obj = Obj.allocateComptime(alloc, Self, parent.type_id);
        obj.parent = parent;
        obj.fields = alloc.dupe(Value, fields) catch oom();

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deepCopy(self: *Self, vm: *Vm) *Self {
        var obj = Self.create(vm, self.parent);
        vm.gc.pushTmpRoot(obj.asObj());
        defer vm.gc.popTmpRoot();

        // TODO: Still needed?
        obj.fields.len = 0;
        for (self.fields, 0..) |*field, i| {
            const value = field.deepCopy(vm);
            obj.fields.len += 1;
            obj.fields[i] = value;
        }

        return obj;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.free(self.fields);
        allocator.destroy(self);
    }
};

pub const CStructure = struct {
    obj: Obj,
    name: []const u8,
    bytes: []u8,
    layout: Module.CStructure.Layout,

    const Self = @This();

    pub fn create(vm: *Vm, layout: Module.CStructure.Layout) *Self {
        const obj = Obj.allocate(vm, Self, undefined);
        obj.name = undefined;
        obj.layout = layout;
        // obj.bytes = vm.gc_alloc.alignedAlloc(u8, layout.alignment, layout.size) catch oom();
        obj.bytes = vm.gc_alloc.alloc(u8, layout.size) catch oom();

        return obj;
    }

    pub fn createComptime(alloc: Allocator, parent: *const Module.CStructure, fields: []const Value) *Self {
        const obj = Obj.allocateComptime(alloc, Self, parent.type_id);
        obj.name = parent.name;
        obj.layout = parent.layout;
        // obj.bytes = alloc.alignedAlloc(u8, parent.layout.alignment, parent.layout.size) catch oom();
        obj.bytes = alloc.alloc(u8, parent.layout.size) catch oom();

        for (fields, 0..) |field, i| {
            obj.setField(i, field);
        }

        return obj;
    }

    pub fn getField(self: *const Self, index: usize) Value {
        const f = self.layout.fields[index];
        const p = self.bytes.ptr + f.offset;

        return switch (f.tag) {
            .u8 => .makeInt(@as(*const u8, @ptrCast(p)).*),
            // .i32 => .makeInt(@as(*const i32, @ptrCast(@alignCast(p))).*),
            // .f32 => .makeFloat(@as(*const f32, @ptrCast(@alignCast(p))).*),
            // .ptr => .makeInt(@intFromPtr(@as(*const *anyopaque, @ptrCast(@alignCast(p))).*)),
        };
    }

    pub fn setField(self: *Self, index: usize, value: Value) void {
        const f = self.layout.fields[index];
        const p = self.bytes.ptr + f.offset;

        switch (f.tag) {
            .u8 => @as(*u8, @ptrCast(p)).* = @intCast(value.int),
            // .i32 => @as(*i32, @ptrCast(@alignCast(p))).* = @intCast(value.int),
            // .f32 => @as(*f32, @ptrCast(@alignCast(p))).* = @floatCast(value.float),
            // .ptr => @as(**anyopaque, @ptrCast(@alignCast(p))).* = @ptrFromInt(@as(usize, @intCast(value.int))),
        }
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deepCopy(self: *Self, vm: *Vm) *Self {
        var obj = Self.create(vm, self.layout);
        vm.gc.pushTmpRoot(obj.asObj());
        defer vm.gc.popTmpRoot();
        @memcpy(obj.bytes, self.bytes);

        return obj;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.free(self.bytes);
        allocator.destroy(self);
    }
};

pub const Enum = struct {
    obj: Obj,
    parent: *const Module.Enum,
    tag_id: u8,
    payload: i64,

    const Self = @This();

    /// Creates an enum instance
    pub fn create(allocator: Allocator, parent: *const Module.Enum, tag_id: u8) *Self {
        const obj = Obj.allocateComptime(allocator, Self, parent.type_id);
        obj.parent = parent;
        obj.tag_id = tag_id;
        obj.payload = parent.discriminants[tag_id];
        obj.obj.type_id = parent.type_id;

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        vm.gc_alloc.destroy(self);
    }
};

pub const Union = struct {
    obj: Obj,
    parent: *const Module.Union,
    tag_id: u8,
    payload: Value,

    const Self = @This();

    /// Creates a compile time constant that is a naked enum field
    pub fn create(vm: *Vm, parent: *const Module.Union, tag_id: u8, payload: Value) *Self {
        vm.gc.pushTmpBuf(&.{payload});
        defer vm.gc.popTmpBuf();

        const obj = Obj.allocate(vm, Self, parent.type_id);
        obj.parent = parent;
        obj.tag_id = tag_id;
        obj.payload = payload;
        obj.obj.kind = if (parent.is_err) .@"error" else .@"union";
        obj.obj.type_id = parent.type_id;

        return obj;
    }

    /// Creates a compile time constant that is a naked union field
    pub fn createComptime(allocator: Allocator, parent: *const Module.Union, tag_id: u8, payload: Value) *Self {
        const obj = Obj.allocateComptime(allocator, Self, parent.type_id);
        obj.parent = parent;
        obj.tag_id = tag_id;
        obj.payload = payload;
        obj.obj.kind = if (parent.is_err) .@"error" else .@"union";
        obj.obj.type_id = parent.type_id;

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        vm.gc_alloc.destroy(self);
    }

    pub fn deepCopy(self: *Self, vm: *Vm) *Self {
        return create(vm, self.parent, self.tag_id, self.payload);
    }
};

pub const Iterator = struct {
    obj: Obj,
    parent: ?Value,
    ptr: *anyopaque,
    vtable: *const VTable,
    count: i64,

    const Self = @This();
    pub const VTable = struct {
        nextFn: *const fn (*anyopaque, *Vm) Value,
        deinitFn: *const fn (*anyopaque, Allocator) void,
    };

    pub fn create(vm: *Vm, ptr: *anyopaque, vtable: *const VTable, parent: ?Value) *Self {
        const obj = Obj.allocate(vm, Self, undefined);
        obj.ptr = ptr;
        obj.parent = parent;
        obj.vtable = vtable;
        obj.count = 0;

        if (options.log_gc) std.debug.print("<iterator>\n", .{});

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.vtable.deinitFn(self.ptr, allocator);
        allocator.destroy(self);
    }

    pub fn next(self: *Self, vm: *Vm) Value {
        return self.vtable.nextFn(self.ptr, vm);
    }

    pub fn nextWithIndex(self: *Self, vm: *Vm) struct { i64, Value } {
        defer self.count += 1;
        return .{ self.count, self.vtable.nextFn(self.ptr, vm) };
    }
};

pub const ArrIterator = struct {
    values: []Value,
    index: usize,

    const Self = @This();

    pub fn create(vm: *Vm, parent: Value) *Obj {
        var self: *Self = vm.gc_alloc.create(Self) catch oom();
        self.values = parent.obj.as(Array).values.items;
        self.index = 0;

        return self.iterator(vm, parent).asObj();
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn next(self: *anyopaque, vm: *Vm) Value {
        const s: *Self = @ptrCast(@alignCast(self));
        if (s.index == s.values.len) {
            return .null;
        }

        defer s.index += 1;
        return s.values[s.index].deepCopy(vm);
    }

    pub fn deinit(self: *anyopaque, allocator: Allocator) void {
        const s: *Self = @ptrCast(@alignCast(self));
        allocator.destroy(s);
    }

    pub fn iterator(self: *Self, vm: *Vm, parent: Value) *Iterator {
        return Iterator.create(vm, self, &.{
            .nextFn = next,
            .deinitFn = deinit,
        }, parent);
    }
};

pub const ArrPtrIterator = struct {
    values: []Value,
    index: usize,

    const Self = @This();

    // When creating a pointer iteraor 'for x in *arr {}', it gets a pointer
    // to 'arr' onto the stack (avoiding a ccopy btw), so we have to access
    // pointer's child
    pub fn create(vm: *Vm, parent: Value) *Obj {
        var self: *Self = vm.gc_alloc.create(Self) catch oom();
        const ptr = parent.obj.as(Pointer).child;
        self.values = ptr.obj.as(Array).values.items;
        self.index = 0;

        return self.iterator(vm, parent).asObj();
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn next(self: *anyopaque, vm: *Vm) Value {
        const s: *Self = @ptrCast(@alignCast(self));
        if (s.index == s.values.len) {
            return .null;
        }

        defer s.index += 1;
        return .makeObj(Pointer.create(vm, &s.values[s.index]).asObj());
    }

    pub fn deinit(self: *anyopaque, allocator: Allocator) void {
        const s: *Self = @ptrCast(@alignCast(self));
        allocator.destroy(s);
    }

    pub fn iterator(self: *Self, vm: *Vm, parent: Value) *Iterator {
        return Iterator.create(vm, self, &.{
            .nextFn = next,
            .deinitFn = deinit,
        }, parent);
    }
};

pub const StrIterator = struct {
    string: *String,
    index: usize,

    const Self = @This();
    pub const empty: Self = .{ .string = undefined, .index = 0 };

    pub fn create(vm: *Vm, string: *String) *Obj {
        var self: *Self = vm.gc_alloc.create(Self) catch oom();
        self.string = string;
        self.index = 0;

        return self.iterator(vm).asObj();
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn next(self: *anyopaque, vm: *Vm) Value {
        const s: *Self = @ptrCast(@alignCast(self));
        if (s.index == s.string.chars.len) {
            return .null;
        }

        defer s.index += 1;
        return .makeObj(Obj.String.takeCopy(vm, &.{s.string.chars[s.index]}).asObj());
    }

    pub fn deinit(self: *anyopaque, allocator: Allocator) void {
        const s: *Self = @ptrCast(@alignCast(self));
        allocator.destroy(s);
    }

    pub fn iterator(self: *Self, vm: *Vm) *Iterator {
        return Iterator.create(vm, self, &.{
            .nextFn = next,
            .deinitFn = deinit,
        }, null);
    }
};

pub const RangeIterator = struct {
    obj: Obj,
    end: i64,
    current: i64,
    incr: i64,

    const Self = @This();

    pub fn create(vm: *Vm, range: Value.RangeInt) *Obj {
        var self: *Self = vm.gc_alloc.create(Self) catch oom();
        self.end = range.end;
        self.current = range.start;
        self.incr = if (range.start < range.end) 1 else -1;

        return self.iterator(vm).asObj();
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn next(self: *anyopaque, _: *Vm) Value {
        const s: *Self = @ptrCast(@alignCast(self));
        if (s.current == s.end) {
            return .null;
        }

        defer s.current += s.incr;
        return .makeInt(s.current);
    }

    pub fn deinit(self: *anyopaque, allocator: Allocator) void {
        const s: *Self = @ptrCast(@alignCast(self));
        allocator.destroy(s);
    }

    pub fn iterator(self: *Self, vm: *Vm) *Iterator {
        return Iterator.create(vm, self, &.{
            .nextFn = next,
            .deinitFn = deinit,
        }, null);
    }
};

pub const ZigStructure = struct {
    obj: Obj,
    name: []const u8,
    child: *anyopaque,
    vtable: *const zffi.VTable,

    const Self = @This();

    pub fn create(vm: *Vm, name: []const u8, child: *anyopaque, vtable: *const zffi.VTable) *Self {
        // Fields first for GC because other wise allocating fields after creation
        // of the instance may trigger GC in between
        const obj = Obj.allocate(vm, Self, undefined);
        obj.name = name;
        obj.child = child;
        obj.vtable = vtable;

        return obj;
    }

    pub fn getField(self: *Self, vm: *Vm, index: usize) Value {
        return self.vtable.get_field(self.child, vm, index);
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        self.vtable.deinit_fn(self.child, vm);
        vm.gc_alloc.destroy(self);
    }
};

pub const TraitObj = struct {
    obj: Obj,
    data: *Obj,
    vtable: *const Module.VTable,

    const Self = @This();

    pub fn create(vm: *Vm, data: *Obj, vtable: *const Module.VTable) *Self {
        const obj = Obj.allocate(vm, Self, undefined);
        obj.data = data;
        obj.vtable = vtable;

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        vm.gc_alloc.destroy(self);
    }
};

pub const Pointer = struct {
    obj: Obj,
    child: *Value,

    const Self = @This();

    pub fn create(vm: *Vm, child: *Value) *Self {
        const obj = Obj.allocate(vm, Self, undefined);
        obj.child = child;

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        vm.gc_alloc.destroy(self);
    }
};

pub fn deepCopy(self: *Obj, vm: *Vm) *Obj {
    return switch (self.kind) {
        .array => self.as(Array).deepCopy(vm).asObj(),
        .@"error", .@"union" => self.as(Union).deepCopy(vm).asObj(),
        .structure => self.as(Structure).deepCopy(vm).asObj(),
        .c_structure => self.as(CStructure).deepCopy(vm).asObj(),
        // Immutable, shallow copy ok
        .box, .closure, .@"enum", .function, .iterator, .cfunction, .zig_function, .zig_structure, .string, .trait_obj, .pointer => self,
    };
}

pub fn destroy(self: *Obj, vm: *Vm) void {
    switch (self.kind) {
        .array => self.as(Array).deinit(vm),
        .box => self.as(Box).deinit(vm),
        .closure => self.as(Closure).deinit(vm),
        .@"enum" => self.as(Enum).deinit(vm),
        .function => {
            const function = self.as(Function);
            function.deinit(vm);
        },
        .cfunction => {
            const function = self.as(CFn);
            function.deinit(vm.gc_alloc);
        },
        .zig_function => {
            const function = self.as(ZigFn);
            function.deinit(vm.gc_alloc);
        },
        .iterator => {
            const iterator = self.as(Iterator);
            iterator.deinit(vm.gc_alloc);
        },
        .pointer => self.as(Pointer).deinit(vm),
        .string => self.as(String).deinit(vm.gc_alloc),
        .structure => {
            const instance = self.as(Structure);
            instance.deinit(vm.gc_alloc);
        },
        .c_structure => {
            const object = self.as(CStructure);
            object.deinit(vm.gc_alloc);
        },
        .zig_structure => {
            const object = self.as(ZigStructure);
            object.deinit(vm);
        },
        .trait_obj => self.as(TraitObj).deinit(vm),
        .@"error", .@"union" => self.as(Union).deinit(vm),
    }
}
pub fn print(self: *Obj, writer: *Writer) Writer.Error!void {
    switch (self.kind) {
        .array => {
            const array = self.as(Array);
            try writer.writeAll("[");
            for (array.values.items, 0..) |val, i| {
                val.print(writer);
                if (i < array.values.items.len - 1) try writer.writeAll(", ");
            }
            try writer.writeAll("]");
        },
        .box => {
            const box = self.as(Box);
            try writer.writeAll("Box ");
            box.value.print(writer);
        },
        .closure => {
            const closure = self.as(Closure);

            if (comptime @import("builtin").mode == .Debug) {
                try writer.print("<closure {s}>", .{closure.function.name});
            } else {
                try writer.print("<fn {s}>", .{closure.function.name});
            }
        },
        .@"enum" => {
            const instance = self.as(Enum);
            try writer.print("<enum {s}.{s}>", .{
                instance.parent.name,
                instance.parent.tags[instance.tag_id],
            });
        },
        .function => {
            const function = self.as(Function);
            try writer.print("<function {s}>", .{function.name});
        },
        .cfunction => try writer.print("<c function {s}>", .{self.as(CFn).name}),
        .zig_function => try writer.print("<zig function {s}>", .{self.as(ZigFn).name}),
        .iterator => try writer.writeAll("<iterator>"),
        .pointer => try writer.print("<pointer 0x{x}>", .{@intFromPtr(self.as(Pointer).child)}),
        .string => try writer.print("{s}", .{self.as(String).chars}),
        .structure => try writer.print("<structure {s}>", .{self.as(Structure).parent.name}),
        .c_structure => try writer.print("<c structure {s}>", .{self.as(CStructure).name}),
        .zig_structure => try writer.print("<zig structure {s}>", .{self.as(ZigStructure).name}),
        .trait_obj => try writer.print("<trait obj {s}>", .{self.as(TraitObj).vtable.name}),
        .@"union", .@"error" => {
            const instance = self.as(Union);
            try writer.print("<{s} {s}.{s}>", .{
                if (instance.parent.is_err) "error" else "union",
                instance.parent.name,
                instance.parent.tags[instance.tag_id],
            });
        },
    }
}

pub fn log(self: *Obj) void {
    switch (self.kind) {
        .array => std.debug.print("<array>", .{}),
        .box => std.debug.print("box", .{}),
        .closure => std.debug.print("<closure {s}>", .{self.as(Closure).function.name}),
        .@"enum", .@"error" => std.debug.print(
            "<{s} {s}>",
            .{ if (self.kind == .@"error") "error" else "enum", self.as(Enum).parent.name },
        ),
        .function => std.debug.print("<function {s}>", .{self.as(Function).name}),
        .structure => std.debug.print("<structure {s}>", .{self.as(Structure).parent.name}),
        .iterator => std.debug.print("<iterator>", .{}),
        .cfunction => std.debug.print("<c function {s}>", .{self.as(CFn).name}),
        .zig_function => std.debug.print("<zig function {s}>", .{self.as(ZigFn).name}),
        .zig_structure => std.debug.print("<zig structure {s}>", .{self.as(ZigStructure).name}),
        .pointer => std.debug.print("<pointer 0x{x}>", .{@intFromPtr(self.as(Pointer).child)}),
        .string => std.debug.print("{s}", .{self.as(String).chars}),
        .trait_obj => std.debug.print("<trait obj {s}>", .{self.as(TraitObj).vtable.name}),
        .@"union" => std.debug.print("<union {s}>", .{self.as(Enum).parent.name}),
    }
}
