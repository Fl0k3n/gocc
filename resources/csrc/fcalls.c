
// int f(int a, int b, int c, int d, int e, int f, int g, int h) {
//     int m;
//     return a + c + e + g + m;
// }


// void g() {
//     int a = f(1, 2, 3, 4, 5, 6, 7, 8);
// }

struct A {
    long a1;
    long a2;
    long a3;
    long a4;
    long a5;
    long a6;
    long a7;
};

void q() {

}

int x = 1;


int f(int v, struct A a) {
    int x = a.a3 + v;
    q();
    return 1 + x;
}

int g() {
    struct A a;
    a.a1 = 1;
    a.a2 = 1;
    a.a3 = 1;
    f(5, a);
}
