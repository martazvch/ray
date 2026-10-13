const std = @import("std");
const Allocator = std.mem.Allocator;
const libffi = @import("libffi");

fn oom() noreturn {
    @panic("oom");
}

pub const Fn = *const fn () callconv(.c) void;
pub const Cif = libffi.ffi_cif;
pub const FfiType = [*c]libffi.ffi_type;
const arg = libffi.ffi_arg;

pub const ok = libffi.FFI_OK;
pub const call = libffi.ffi_call;
pub const prepCif = libffi.ffi_prep_cif;
pub const getDefaultAbi = libffi.ffi_get_default_abi;
pub const getStructOffsets = libffi.ffi_get_struct_offsets;

pub const Void = &libffi.ffi_type_void;
pub const Bool = Uint8;
pub const Uint8 = &libffi.ffi_type_uint8;
pub const Uint = &libffi.ffi_type_uint32;
pub const Int = &libffi.ffi_type_sint32;
pub const Float = &libffi.ffi_type_float;
pub const Double = &libffi.ffi_type_double;
pub const Pointer = &libffi.ffi_type_pointer;

pub const Type = union(enum) {
    void,
    bool,
    u8,
    uint,
    int,
    f32,
    f64,
    string,
    enum_tag,
    pointer,
    structure: []const Type,

    pub fn ffi(self: Type, alloc: Allocator) FfiType {
        return switch (self) {
            .void => Void,
            .bool => Bool,
            .u8 => Uint8,
            .uint => Uint,
            .int => Int,
            .f32 => Float,
            .f64 => Double,
            .string => Pointer,
            .enum_tag => Int,
            .pointer => Pointer,
            .structure => |fields| {
                const ffi_type = alloc.create(libffi.ffi_type) catch oom();

                // Has to be a null-terminated array
                var elements = alloc.alloc(FfiType, fields.len + 1) catch oom();
                for (fields, 0..) |field, i| {
                    elements[i] = field.ffi(alloc);
                }
                elements[fields.len] = null;

                ffi_type.* = .{
                    .size = 0,
                    .alignment = 0,
                    .type = libffi.FFI_TYPE_STRUCT,
                    .elements = @ptrCast(elements),
                };

                if (getStructOffsets(getDefaultAbi(), ffi_type, null) != ok) {
                    @panic("Failed to get libffi structure offset");
                }

                return ffi_type;
            },
        };
    }

    pub fn returnSize(self: Type, alloc: Allocator) usize {
        return @max(self.ffi(alloc).*.size, @sizeOf(arg));
    }
};

pub fn sliceToFfi(alloc: Allocator, types: []const Type) []FfiType {
    var ffi_types = std.ArrayList(FfiType).initCapacity(alloc, types.len) catch oom();
    for (types) |ty| {
        ffi_types.appendAssumeCapacity(ty.ffi(alloc));
    }

    return ffi_types.toOwnedSlice(alloc) catch oom();
}
