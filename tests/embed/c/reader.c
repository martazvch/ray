#include "reader.h"
#include "dynarray.h"
#include <stdio.h>
#include <string.h>

bool startsWith(const char *str, const char *pattern) {
    return strncmp(str, pattern, strlen(pattern)) == 0;
}

bool read(FILE *file, Cases *cases) {
    State state = STATE_NONE;
    Case cur_case = {0};
    String body = {0};
    String res = {0};

    char line[1024];
    while (fgets(line, sizeof(line), file)) {
        // Strip trailing newline (\n on Linux, \r\n on Windows)
        line[strcspn(line, "\r\n")] = 0;

        switch (state) {
        case STATE_NONE:
            if (startsWith(line, "# case")) {
                state = STATE_CASE;
                continue;
            }
            break;
        case STATE_CASE:
            if (startsWith(line, "## part")) {
                state = STATE_PART;
                continue;
            }
            break;
        case STATE_PART:
            if (startsWith(line, "- code")) {
                state = STATE_PRE_CODE;
                continue;
            } else if (startsWith(line, "# case")) {
                state = STATE_CASE;
                da_append(cases, cur_case);
                da_reset(&cur_case);
                continue;
            }
            break;
        case STATE_PRE_CODE:
            if (startsWith(line, "```")) {
                state = STATE_CODE;
                continue;
            }
            break;
        case STATE_CODE:
            if (startsWith(line, "```")) {
                state = STATE_PRE_RES;
                continue;
            }
            break;
        case STATE_PRE_RES:
            if (startsWith(line, "## part")) {
                state = STATE_PART;
                Part part = {
                    .body = strndup(body.items, body.count),
                    .res = (res.count > 0) ? strndup(res.items, res.count) : NULL,
                };
                da_append(&cur_case, part);
                da_reset(&body);
                da_reset(&res);
                continue;
            } else if (startsWith(line, "```")) {
                state = STATE_RES;
                continue;
            }
            break;
        case STATE_RES:
            if (startsWith(line, "```")) {
                state = STATE_PART;
                Part part = {
                    .body = strndup(body.items, body.count),
                    .res = (res.count > 0) ? strndup(res.items, res.count) : NULL,
                };
                da_append(&cur_case, part);
                da_reset(&body);
                da_reset(&res);
                continue;
            }
            break;
        }

        printf("Line: -%s-\n", line);
        if (strlen(line) == 0) {
            continue;
        }

        switch (state) {
        case STATE_CODE:
            da_append_many(&body, line, strlen(line));
            da_append(&body, '\n');
            break;
        case STATE_RES:
            da_append_many(&res, line, strlen(line));
            break;
        default:
            break;
        }
    }

    if (cur_case.count > 0) {
        da_append(cases, cur_case);
    }

    da_free(&body);
    da_free(&res);

    return false;
}
