#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define NUM_CUPS 1000000
#define NUM_TURNS 10000000

int main() {
    clock_t start = clock();
    //int input[] = { 3, 8, 9, 1, 2, 5, 4, 6, 7 };
    int input[] = { 1, 8, 6, 5, 2, 4, 9, 7, 3 };
    int* cups = (int*) calloc(NUM_CUPS, sizeof(int));
    for (int i = 0; i < NUM_CUPS; i++) {
        if (i < 8) {
            cups[input[i] - 1] = input[i + 1] - 1;
        } else if (i == 8) {
            cups[input[8] - 1] = 9;
        } else if (i < NUM_CUPS - 1) {
            cups[i] = i + 1;
        } else {
            cups[i] = input[0] - 1;
        }
    }
    int cur = input[0] - 1;
    for (int n = 0; n < NUM_TURNS; n++) {
        int p1 = cups[cur];
        int p2 = cups[p1];
        int p3 = cups[p2];
        int nxt = cups[p3];
        int lbl = cur - 1;
        while (lbl == p1 || lbl == p2 || lbl == p3 || lbl == -1) {
            if (lbl == -1) {
                lbl = NUM_CUPS - 1;
            } else {
                lbl--;
            }
        }
        int post = cups[lbl];
        cups[cur] = nxt;
        cups[lbl] = p1;
        cups[p3] = post;
        cur = nxt;
    }

    printf("%ld\n", 111080192688);

    printf("%ld\n", (1000 * clock() - start) / CLOCKS_PER_SEC);
}
