
// int g(int a, int b, int e1, int e2, int e3, int e4, int e5, int e6, int e7) {
//     int c;
//     c = a + 235 * b;
//     return c + e1 + e2 + e3 + e4 + e5 + e6 + e7;
// }

int A;

int B = 3;

static int C;

static int D = 5;

// extern int E;

static int F() {
    return 1;
}

struct KNTP {
    int x;
};

struct KNTP kntp;

struct KNTP kntp2 = {
    .x = 1
};

// extern int c;

void (*kntp3)(int, int);

// extern int G();

static int g(int a, int b) {
    int c;
    c = a + 235 * b + A + F() + D + C + B;
    // c = a + 270 * b;
    return c;
}

// int g(int a, int b, int e1, int e2, int e3, int e4, int e5, int e6, int e7);

void f() {
    int a;
    int b;
    int i;
    for (i = 0; i < 10; i++) {
        int m;
        m = i / 345642;
        if (m == 3) {
            b = g(a, b);
        }
        else {
            a++;
        }
    }
}

// void f() {
//     char b0;
//     char b1;
//     char b2;
//     short w0;
//     short w1;
//     short w;
//     int x;
//     int y;
//     int z;
//     long a;
//     long b;
//     long c;
//     z = x + y;
//     c = a + b;
// }