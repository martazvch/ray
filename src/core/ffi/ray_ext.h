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
typedef struct RayVm RayVm;

// -----------
//  Typedefs
typedef void (*RayFn)(RayVm *);
typedef size_t Index;

// ----------
//  Accessors
typedef double (*RayGetFloat)(RayVm *, Index);

// ----------
//  Setters
typedef void (*RaySetFloat)(RayVm *, Index, double);

// ----------------
//  Function table
typedef struct {
    RayGetFloat get_float;
    RaySetFloat set_float;
} RayApi;

#endif // RAY_EXT_H
