extern int f(int, int);

int a = 1;
int W = 8;
extern int b;

extern int f2();
int funcWithManyArgs(int a1, int a2, int a3, int a4, int a5,
    int a6, int a7, int a8, int a9)
{
    int res = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;
    a1 = 7;
    a = 2;
    b = -2;
    return res;
}

int main() {
    int fres = funcWithManyArgs(a, 2, 3, 4, 5, 6, 7, 8, 9);
    // return f(a, b);
    return fres + a + b;
}
