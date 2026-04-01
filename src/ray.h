#ifndef RAY_H
#define RAY_H

#include <stdbool.h>

typedef void (*RayPrintFn)(const char *);

typedef struct {
    bool embedded;
    bool print_ast;
    bool print_bytecode;
    bool static_analyzis;
    bool print_ir;
    bool dbg_infos;
    const char *path;
    RayPrintFn printFn;
} Config;

typedef enum {
    RES_OK,
    RES_COMPILE_ERR,
    RES_RUNTIME_ERR,
} Result;

typedef enum {
    TYPE_VOID,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_BOOL,
} RayType;

typedef struct {
    char *name;
    RayType type;
} RayParam;

#define MAX_PARAM 256
typedef struct RayVm RayVm;
typedef void (*RayFn)(RayVm *);

typedef struct {
    char *name;
    int arity;
    RayParam params[MAX_PARAM];
    RayType return_type;
    RayFn func;
} RayFnProto;

extern void rayCreate(Config);
extern void rayRegisterFn(RayFnProto);
extern void rayInitGlobalScope();
extern Result rayRun(const char *source);
extern void rayDeinit();

#endif // RAY_H
