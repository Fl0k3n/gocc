int q = 6;

static int C;

extern int W;

static int f() {
    return 0;
}

int f2() {
    C = 7;
    return q + C + W + f();
}
