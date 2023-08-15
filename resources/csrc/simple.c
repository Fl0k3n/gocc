
int f1(int a, int b) {
    int v;
    v = -1;
    return a + b + v;
}


int f2(int x) {
    return f1(x, 1) + 2;
}

char f3() {
    return 'a';
}

void f4() {
    float a = f3();
}