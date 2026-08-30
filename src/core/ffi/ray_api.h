#ifndef RAY_EXT_H
#define RAY_EXT_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define UNUSED(expr) ((void)expr)

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
typedef struct RayVm RayVm;
typedef struct CStruct CStruct;

// -----------
//  Typedefs
typedef void (*RayFn)(RayVm *);
typedef size_t Index;

// ----------
//  Accessors
typedef double (*RayGetFloat)(const RayVm *const, Index);
typedef int64_t (*RayGetInt)(const RayVm *const, Index);
typedef bool (*RayGetBool)(const RayVm *const, Index);
typedef char *(*RayGetStr)(const RayVm *const, Index);

typedef CStruct *(*RayGetStruct)(const RayVm *const, Index);
typedef unsigned char (*RayGetU8)(const CStruct *const, Index);

typedef int64_t (*RayGetEnumTag)(const RayVm *const, Index);

// ----------
//  Setters
typedef void (*RaySetFloat)(RayVm *const, Index, double);
typedef void (*RaySetInt)(RayVm *const, Index, int64_t);
typedef void (*RaySetBool)(RayVm *const, Index, bool);
typedef void (*RaySetStr)(RayVm *const, Index, const char *);

// ----------------
//  Function table
typedef struct {
    RayGetFloat get_float;
    RaySetFloat set_float;
    RayGetInt get_int;
    RaySetInt set_int;
    RayGetBool get_bool;
    RaySetBool set_bool;
    RayGetStr get_str;
    RaySetStr set_str;

    RayGetStruct get_struct;
    RayGetU8 get_field_u8;

    RayGetEnumTag get_enum_tag;
} RayApi;

#endif // RAY_EXT_H
