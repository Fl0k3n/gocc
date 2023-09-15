static int D = 4;
static int C;

int b;
int m = 3;

extern int q;

extern int f2();

int f(int a, int b) {
    C = 5;
    return a + b + C + D + m + f2();
}
