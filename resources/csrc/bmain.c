int A_INIT = 2;
int A_NOT_INIT;

static int B_INIT = 5;
static int B_NOT_INIT;

extern int f(int, int);

int g(int a, int b) {
    B_NOT_INIT = 3;
    A_NOT_INIT = 4;
    return a + b + A_INIT + B_INIT + f(A_NOT_INIT, B_NOT_INIT);
}

int main() {
    int a = 2;
    int b = 4;
    int i;
    for (i = 0; i < 5; i++) {
        int m;
        m = i;
        if (m < 2) {
            b = g(a, b);
        }
        else {
            a++;
        }
    }
    return a;
}
