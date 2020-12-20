#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define STOP 30000000

int main() {
    clock_t start = clock();
    int input[] = { 20, 9, 11, 0, 1, 2 };
    int* prevs = (int*) malloc(STOP * sizeof(int));
    for (int i = 0; i < STOP; i++) {
        prevs[i] = 0;
    }
    for (int i = 0; i < 6; i++) {
        prevs[input[i]] = i + 1;
    }
    int cur = 2;
    for (int i = 6; i < STOP; i++) {
        int last_said = prevs[cur];
        prevs[cur] = i;
        if (last_said == 0) {
            cur = 0;
        } else {
            cur = i - last_said;
        }
    }
    printf("%d\n", cur);
    printf("%ld\n", (1000 * clock() - start) / CLOCKS_PER_SEC);
}
