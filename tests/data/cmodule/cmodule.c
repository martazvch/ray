#include <math.h>
#include <stdio.h>

float ret_no_arg() {
    return 8.f;
}

float ret_arg(float arg) {
    return sqrtf(arg);
}

float ret_args(float arg1, float arg2) {
    return sqrtf(arg1);
}

void void_no_arg() {}

void void_arg(float arg) {
    (void)arg;
}

void void_args(float arg1, float arg2) {
    (void)arg1;
    (void)arg2;
}
