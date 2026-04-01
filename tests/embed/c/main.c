#include "tester.h"
#include <stdio.h>

int main() {
    const char *path = "../cases";
    if (testDir(path)) {
        return 0;
    } else {
        return 1;
    }
}
