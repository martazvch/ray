pub const cVm = opaque {};
pub const Fn = *const fn (*cVm) callconv(.c) void;

pub const cType = enum(c_int) {
    void,
    int,
    float,
    bool,
};

pub const FnProto = extern struct {
    name: [*c]const u8,
    arity: c_int,
    params: [*c]Param,
    return_type: cType,
    func: Fn,

    const Param = extern struct {
        name: [*c]const u8,
        ty: cType,
    };
};
