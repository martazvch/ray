#ifndef RAY_EXT_H
#define RAY_EXT_H

#include <stddef.h>

// #ifdef _WIN32
//     // #ifdef RAY_BUILDING_DLL
//     //     #define RAY_API __declspec(dllexport)
//     // #else
//         #define RAY_API __declspec(dllimport)
//     // #endif
// #else
//     #define RAY_API
// #endif

// ----------
//  Opaques
typedef struct RayReg RayReg;
typedef struct RayVm RayVm;

// -----------
//  Typedefs
typedef void (*RayFn)(RayVm *);

// ----------
//  Types
typedef enum {
    TYPE_VOID,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_BOOL,
} RayType;

typedef size_t Index;

typedef struct {
    char *name;
    RayType type;
} Param;

#define MAX_PARAM 256
typedef struct {
    char *name;
    RayFn func;
    int arity;
    Param params[MAX_PARAM];
    int return_type;
} RayFnProto;

typedef void (*RayRegisterFn)(RayReg *, RayFnProto);

// ----------
//  Accessors
typedef double (*RayGetFloat)(RayVm *, Index);

// ----------
//  Setters
typedef void (*RaySetFloat)(RayVm *, Index, double);

// ----------------
//  Function table
typedef struct {
    RayRegisterFn ray_register_fn;
    RayGetFloat ray_get_float;
    RaySetFloat ray_set_float;
} RayApi;

#endif // RAY_EXT_H
