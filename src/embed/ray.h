#ifndef RAY_H
#define RAY_H

#include <stdbool.h>
#include <stddef.h>

// ---------
//  Opaques
typedef struct RayVm RayVm;
typedef struct RayReg RayReg;

// ----------
//  Typedefs
typedef void (*RayFn)(RayVm *);
typedef void (*RayPrintFn)(const char *);

typedef size_t Index;

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
typedef struct {
    char *name;
    int arity;
    RayParam params[MAX_PARAM];
    RayType return_type;
    RayFn func;
} RayFnProto;

// -----------
//  Functions
extern RayVm *rayNewVm(Config);
extern void rayRegisterFn(RayVm *, RayFnProto);
extern void rayInitGlobalScope(RayVm *);
extern Result rayRun(RayVm *, const char *source);
extern void rayDeinitVm(RayVm *);

// ---------
//  Getters
extern double rayGetFloat(RayVm *, Index);
extern int rayGetInt(RayVm *, Index);

// ---------
//  Setters
extern void raySetFloat(RayVm *, Index, double);
extern void raySetInt(RayVm *, Index, int);
extern void raySetBool(RayVm *, Index, bool);

#endif // RAY_H
