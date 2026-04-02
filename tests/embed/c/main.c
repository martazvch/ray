#include "tester.h"

int main() {
    if (testDir("../cases")) {
        return 0;
    } else {
        return 1;
    }
}
