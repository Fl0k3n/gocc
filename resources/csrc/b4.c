struct D {
    int m;
};

struct C {
    struct D q[4];
};

struct B {
    int v;
    int m;
    struct C* c;
};

struct A {
    int x;
    int y;
    struct B* b;
};

struct A* identity(struct A* ptr) {
    return ptr;
}

int main() {
    struct A a;
    struct B b;
    struct C c;
    struct A* aptr;
    aptr = &a;
    a.b = &b;
    a.b->c = &c;
    a.b->c->q[2].m = 2;
    return identity(&a)->b->c->q[2].m;
}
