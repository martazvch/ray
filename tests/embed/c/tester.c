#include "tester.h"
#include "dynarray.h"
#include "ray.h"
#include "reader.h"
#include "tinydir.h"
#include <stdio.h>

void isLess(RayVm *vm) {
    printf("CALLED\n");
}

static const char *output = NULL;

void print(const char *text) {
    printf("Gonna print: %s\n", text);
    output = strdup(text);
    printf("Printed: %s\n", output);
}

bool runTest(Part *part) {
    output = NULL;
    Result result = rayRun(part->body);
    printf("Printed: %s\n", output);

    if (part->res) {
        if (output) {
            if (strcmp(part->res, output) != 0) {
                printf("Mismatch between exoected and output\n"
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
            printf("Expected output but got nothing\n"
                   " Expected:\n"
                   " ---------\n"
                   "%s\n\n",
                   part->res);

            return false;
        }

    } else if (output) {
        printf("Expect nothing but got\n"
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
        printf("Oui\n");
        read(f, &cases);
        fclose(f);

        printf("Testing file: %s\n", file.name);
        da_foreach(Case, c, &cases) {
            printf("-- Case\n");
            da_foreach(Part, part, c) {
                printf("  -- Part\n");
                printf("    -- body\n%s\n", part->body);
                printf("    -- res\n%s\n", part->res);
            }
        }

        da_foreach(Case, c, &cases) {
            rayCreate((Config){
                .embedded = true,
                // .print_ir = true,
                .printFn = print,
            });
            rayRegisterFn((RayFnProto){
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
            rayInitGlobalScope();

            da_foreach(Part, part, c) {
                if (!runTest(part)) {
                    rayDeinit();
                    return false;
                }
            }

            rayDeinit();
        }

    next:
        tinydir_next(&dir);
    }

    tinydir_close(&dir);

    return true;
}
