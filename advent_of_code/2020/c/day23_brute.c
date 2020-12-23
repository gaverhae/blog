#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define NUM_CUPS 1000000
#define NUM_TURNS 10000000

int main() {
    clock_t start = clock();
    //int input[] = { 3, 8, 9, 1, 2, 5, 4, 6, 7 };
    int input[] = { 1, 8, 6, 5, 2, 4, 9, 7, 3 };
    int* cups = (int*) malloc(NUM_CUPS * sizeof(int));
    for (int i = 0; i < 9; i++) {
        cups[i] = input[i];
    }
    for (int i = 9; i < NUM_CUPS; i++) {
        cups[i] = i + 1;
    }
    for (int n = 0; n < NUM_TURNS; n++) {
        int current = cups[n % NUM_CUPS];
        int p1 = cups[(n + 1) % NUM_CUPS];
        int p2 = cups[(n + 2) % NUM_CUPS];
        int p3 = cups[(n + 3) % NUM_CUPS];
        int lbl = current - 1;
        while (lbl == p1 || lbl == p2 || lbl == p3 || lbl == 0) {
            if (lbl == 0) {
                lbl = NUM_CUPS;
            } else {
                lbl--;
            }
        }
        int tom = n + 4;
        while (cups[tom % NUM_CUPS] != lbl) {
            tom++;
        }
        tom = (tom - n - 3) % NUM_CUPS;

        for (int i = 0; i < tom; i++) {
            cups[(n + i + 1) % NUM_CUPS] = cups[(n + i + 4) % NUM_CUPS];
        }
        cups[(n + tom + 1) % NUM_CUPS] = p1;
        cups[(n + tom + 2) % NUM_CUPS] = p2;
        cups[(n + tom + 3) % NUM_CUPS] = p3;
    }

    int one_idx;
    for (one_idx = 0; one_idx < NUM_CUPS; one_idx++) {
        if (cups[one_idx] == 1) {
            break;
        }
    }
    printf("%ld\n", ((long)cups[(one_idx + 1) % NUM_CUPS]) * ((long)cups[(one_idx + 2) % NUM_CUPS]));

    printf("%ld\n", (1000 * clock() - start) / CLOCKS_PER_SEC);
}
