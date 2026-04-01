#ifndef H_DYNARRAY
#define H_DYNARRAY

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define DA_INIT_CAP 256

#define da_reserve(da, expected_capacity)                                      \
    do {                                                                       \
        if ((expected_capacity) > (da)->capacity) {                            \
            if ((da)->capacity == 0) {                                         \
                (da)->capacity = DA_INIT_CAP;                                  \
            }                                                                  \
        }                                                                      \
        while ((expected_capacity) > (da)->capacity) {                         \
            (da)->capacity *= 2;                                               \
        }                                                                      \
        (da)->items =                                                          \
            realloc((da)->items, (da)->capacity * sizeof(*(da)->items));       \
        assert((da)->items && "Need more RAM");                                \
    } while (0)

#define da_append(da, item)                                                    \
    do {                                                                       \
        da_reserve((da), (da)->count + 1);                                     \
        (da)->items[(da)->count++] = (item);                                   \
    } while (0)

#define da_append_array(da, new_items)                                         \
    do {                                                                       \
        size_t items_count = sizeof((new_items)) / sizeof(*(da)->items);       \
        da_reserve((da), (da)->count + (items_count));                         \
        memcpy((da)->items + (da)->count, (new_items),                         \
               (items_count) * sizeof(*(da)->items));                          \
        (da)->count += (items_count);                                          \
    } while (0)

#define da_append_many(da, new_items, items_count)                             \
    do {                                                                       \
        da_reserve((da), (da)->count + (items_count));                         \
        memcpy((da)->items + (da)->count, (new_items),                         \
               (items_count) * sizeof(*(da)->items));                          \
        (da)->count += (items_count);                                          \
    } while (0)

#define da_reset(da) ((da)->count = 0)

#define da_free(da) free((da)->items)

#define da_foreach(Type, it, da)                                               \
    for (Type *it = (da)->items; it < (da)->items + (da)->count; ++it)

#endif // H_DYNARRAY
