// void f() {
//     a + b;
// }

int f0(int, int, int);


typedef int TEST;

struct A {
    int x;
    char y;
};

typedef struct B {
    int z;
} B;

int f1(int a, int b) {
    int x;
    int y = 1;
    int A[10];
    struct A st;
    struct A* ptr;

    A[0] = 1;
    A[y + 0 + 1 * 0] = 2;

    st.x = 1;
    st.y = A[y + 0 + 1 * 0];

    ptr = &st;
    ptr->x--;


    x = 5;
    if (x > 1) {
        x = 6;
    }
    else {
        x = 7;
    }

    while (x > 0) {
        x++;
    }

    for (i = 0; i < 10; i++) {
        f(1, 2);
        for (j = 1; j == 1; j *= -1) {
            if (j % 2) {
                f(0);
            }
            else if (j * 3 % 3) {
                g(1);
            }
            else if (0) {
                x++;
            }
            else {
                x--;
            }
        }
    }

    return x;
}
