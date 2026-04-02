#include "tester.h"
#include "dynarray.h"
#include "ray.h"
#include "reader.h"
#include "tinydir.h"
#include <stdarg.h>
#include <stdio.h>

void printCases(Cases *cases) {
    da_foreach(Case, c, cases) {
        printf("-- Case\n");
        da_foreach(Part, part, c) {
            printf("  -- Part\n");
            printf("    -- body\n%s\n", part->body);
            printf("    -- res\n%s\n", part->res);
        }
    }
}

void isLess(RayVm *vm) {
    int a = rayGetInt(vm, 0);
    int b = rayGetInt(vm, 1);
    raySetBool(vm, 0, a < b);
}

static const char *output = NULL;

void print(const char *text) {
    output = strdup(text);
}

void logError(const char *file_name, int part_id, const char *fmt, ...) {
    fprintf(stderr,
            "Error in embedded C tests in file: %s, part n°%d\n",
            file_name, part_id);

    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
}

bool runTest(RayVm *vm, const char *file_name, Part *part, int part_id) {
    output = NULL;
    Result result = rayRun(vm, part->body);

    if (part->res) {
        if (output) {
            if (strcmp(part->res, output) != 0) {
                logError(file_name, part_id,
                         "Mismatch between expected and output\n"
                         " Expected:\n"
                         "----------\n"
                         "%s\n\n"
                         "Got:\n"
                         "----\n"
                         "%s\n\n",
                         part->res, output);

                return false;
            }
        } else {
            logError(file_name, part_id,
                     "Expected output but got nothing\n"
                     " Expected:\n"
                     " ---------\n"
                     "%s\n\n",
                     part->res);

            return false;
        }

    } else if (output) {
        logError(file_name, part_id,
                 "Expect nothing but got\n"
                 " Got:\n"
                 " ----\n"
                 "%s\n\n",
                 output);

        return false;
    }

    return true;
}

bool testDir(const char *path) {
    tinydir_dir dir;
    if (tinydir_open(&dir, path) == -1) {
        return false;
    }

    while (dir.has_next) {
        tinydir_file file;

        if (tinydir_readfile(&dir, &file) == -1) {
            goto next;
        }

        // Skip . and ..
        if (file.name[0] == '.' &&
            (file.name[1] == '\0' ||
             (file.name[1] == '.' && file.name[2] == '\0'))) {
            goto next;
        }

        if (!file.is_reg) {
            continue;
        }

        Cases cases = {0};
        FILE *f = fopen(file.path, "r");
        readMd(f, &cases);
        fclose(f);

        da_foreach(Case, c, &cases) {
            RayVm *vm = rayNewVm((Config){
                .embedded = true,
                .printFn = print,
            });
            rayRegisterFn(
                vm,
                (RayFnProto){
                    .name = "isLess",
                    .arity = 2,
                    .params =
                        {
                            {.name = "a", .type = TYPE_INT},
                            {.name = "b", .type = TYPE_INT},
                        },
                    .return_type = TYPE_BOOL,
                    .func = isLess,
                });
            rayInitGlobalScope(vm);

            int part_id = 0;
            da_foreach(Part, part, c) {
                if (!runTest(vm, file.name, part, part_id)) {
                    rayDeinitVm(vm);
                    return false;
                }
                part_id++;
            }

            rayDeinitVm(vm);
        }

    next:
        tinydir_next(&dir);
    }

    tinydir_close(&dir);

    return true;
}
