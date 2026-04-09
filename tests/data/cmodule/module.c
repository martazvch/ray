#include "ray_ext.h"
#include <math.h>
#include <stdio.h>

static const RayApi *api;

void my_ret_arg(RayVm *vm) {
    double tmp = api->ray_get_float(vm, 0);
    api->ray_set_float(vm, 0, sqrt(tmp));
}

void my_ret_args(RayVm *vm) {
    double tmp1 = api->ray_get_float(vm, 0);
    double tmp2 = api->ray_get_float(vm, 1);
    api->ray_set_float(vm, 0, sqrt(tmp1));
}

void my_empty(RayVm *vm) {}

void my_empty_args(RayVm *vm) {
    double arg1 = api->ray_get_float(vm, 0);
    double arg2 = api->ray_get_float(vm, 1);
}

void handcheck(RayReg *reg, RayApi *const in_api) {
    api = in_api;
    api->ray_register_fn(
        reg,
        (RayFnProto){
            .name = "ret_arg",
            .func = my_ret_arg,
            .arity = 1,
            .params = {{.name = "value", .type = TYPE_FLOAT}},
            .return_type = TYPE_FLOAT,
        });

    api->ray_register_fn(
        reg,
        (RayFnProto){
            .name = "ret_args",
            .func = my_ret_args,
            .arity = 2,
            .params = {
                {.name = "arg1", .type = TYPE_FLOAT},
                {.name = "arg2", .type = TYPE_FLOAT},
            },
            .return_type = TYPE_FLOAT,
        });

    api->ray_register_fn(
        reg,
        (RayFnProto){
            .name = "empty",
            .func = my_empty,
            .arity = 0,
            .params = {},
            .return_type = TYPE_VOID,
        });

    api->ray_register_fn(
        reg,
        (RayFnProto){
            .name = "empty_args",
            .func = my_empty_args,
            .arity = 2,
            .params = {
                {.name = "arg1", .type = TYPE_FLOAT},
                {.name = "arg2", .type = TYPE_FLOAT},
            },
            .return_type = TYPE_VOID,
        });
}
