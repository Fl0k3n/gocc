int fib(int n) {
    if (n == 1 || n == 0) {
        return 1;
    }
    if (n < 0) {
        return -1;
    }
    return fib(n - 1) + fib(n - 2);
}

int fibIter(int n) {
    int i;
    int a, b;
    int temp;
    if (n < 0) {
        return -1;
    }
    a = 0;
    b = 1;
    for (i = 0; i < n; i++) {
        temp = b;
        b = a + b;
        a = temp;
    }
    return b;
}

void prepareDynamicFibArr(int* arr, int size) {
    int i;
    if (size > 0) {
        arr[0] = 1;
    }
    if (size > 1) {
        arr[1] = 1;
    }
    for (i = 2; i < size; i++) {
        arr[i] = -1;
    }
}

int fibDynamic(int* arr, int n) {
    if (n < 0) {
        return -1;
    }
    if (arr[n] != -1) {
        return arr[n];
    }
    arr[n] = fibDynamic(arr, n - 1) + fibDynamic(arr, n - 2);
    return arr[n];
}

int main() {
    int a[9];
    int fibNum = 8;
    int ok = 0;
    int recursive, iterative, dynamic;

    recursive = fib(fibNum);
    iterative = fibIter(fibNum);

    prepareDynamicFibArr(a, (sizeof a) / sizeof(int));
    dynamic = fibDynamic(a, fibNum);

    if (recursive == iterative && iterative == dynamic) {
        ok = 1;
    }

    return ok;
}
