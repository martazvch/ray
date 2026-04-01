#ifndef READER_H
#define READER_H

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum {
    STATE_NONE,
    STATE_CASE,
    STATE_PART,
    STATE_PRE_CODE,
    STATE_CODE,
    STATE_PRE_RES,
    STATE_RES
} State;

typedef struct {
    char *body;
    char *res;
} Part;

typedef struct {
    size_t count;
    size_t capacity;
    Part *items;
} Case;

typedef struct {
    size_t count;
    size_t capacity;
    Case *items;
} Cases;

typedef struct {
    size_t count;
    size_t capacity;
    char *items;
} String;

bool read(FILE *file, Cases *cases);

#endif // READER_H
