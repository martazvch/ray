#include "../../../src/core/ffi/ray_ext.h"
#include <math.h>
#include <stdio.h>

static const RayApi *api;

void ret_no_arg(RayVm *vm) {
    api->set_float(vm, 0, 8);
}

void ret_arg(RayVm *vm) {
    double tmp = api->get_float(vm, 0);
    api->set_float(vm, 0, sqrt(tmp));
}

void ret_args(RayVm *vm) {
    double tmp1 = api->get_float(vm, 0);
    double tmp2 = api->get_float(vm, 1);
    api->set_float(vm, 0, sqrt(tmp1));
}

void void_no_arg(RayVm *vm) {}

void void_arg(RayVm *vm) {
    double arg1 = api->get_float(vm, 0);
}

void void_args(RayVm *vm) {
    double arg1 = api->get_float(vm, 0);
    double arg2 = api->get_float(vm, 1);
}
