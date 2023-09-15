#include "stdio.h"
#include "stdlib.h"


int main() {
    int x = rand();
    int y = rand();
    // printf("%p\n", (void*)&x);
    // printf("%p\n", (void*)&y);
    // int* a = &y;
    // a = a - 1;
    // printf("%p\n", a);
    int z = x + y;
    int v = x + rand();
    // *a = 7;
    // printf("%p\n", a);
    // printf("%p\n", (void*)&x);
    printf("%d\n", x);
    printf("%d\n", z);
    printf("%d\n", v);
    return 0;
}