const libffi = @import("libffi");

pub const Fn = *const fn () callconv(.c) void;
pub const Type = [*c]libffi.ffi_type;
pub const Params = []Type;
pub const Cif = libffi.ffi_cif;

pub const Void = &libffi.ffi_type_void;
pub const Bool = &libffi.ffi_type_sint32;
pub const Int32 = &libffi.ffi_type_sint32;
pub const Float = &libffi.ffi_type_float;

pub fn eqType(ty1: Type, ty2: Type) bool {
    return ty1.*.type == ty2.*.type;
}

pub const call = libffi.ffi_call;
pub const prepCif = libffi.ffi_prep_cif;
pub const getDefaultAbi = libffi.ffi_get_default_abi;
