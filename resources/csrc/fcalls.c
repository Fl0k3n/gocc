
// int f(int a, int b, int c, int d, int e, int f, int g, int h) {
//     int m;
//     return a + c + e + g + m;
// }

#include "stdio.h"

int x;
extern int KNTP;

int* m = &x;

struct A {
    int x;
    int y;
} a = {
    .y = 5
};

int main() {
    // float f;
    // float g = 0.1f + f;
    // double d;
    // double m = 0.1 + d;
    // int a = x;
    // float y = 0.5f;
    // int v = KNTP;
    // float z = y + a + v;
    int x;
    int a = x + 3;
    if (a > 0) {
        return -1;
    }
    a = x + 5;
    return 5;
    // printf("%d\n", a);
}

// int f(int a) {
//     return a;
// }


// void g() {
//     int x;
//     int (*fptr)(int);
//     f(1);
//     x = f(2);
//     fptr = f;
//     fptr(3);
// }

// void g() {
//     int a = f(1, 2, 3, 4, 5, 6, 7, 8);
// }

// struct A {
//     long a1;
//     long a2;
//     long a3;
//     long a4;
//     long a5;
//     long a6;
//     long a7;
// };

// void q() {

// }

// int m(int b) {
//     return b;
// }

// int m(char b, int c) {
//     return b;
// }

// int x = 1;


// int f(int v, struct A a) {
//     int x = a.a3 + v;
//     int (*k)(int) = m;
//     q();
//     return 1 + x;
// }

// int g() {
//     struct A a;
//     a.a1 = 1;
//     a.a2 = 1;
//     a.a3 = 1;
//     f(5, a);
// }
