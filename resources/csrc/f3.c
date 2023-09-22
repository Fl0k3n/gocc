
int main() {
    float arr[5];
    int res;
    float acc = 0.0f;
    int i;
    for (i = 0; i < 5; i++) {
        arr[i] = i;
        arr[i] *= 1.5f;
    }
    for (i = 0; i < 5; i++) {
        if (arr[i] > 5) {
            acc += arr[i];
        }
        else {
            int j;
            for (j = 0; j < i; j++) {
                acc += arr[j] * 0.5f;
            }
            acc /= 2;
        }
    }
    res = acc;
    return res;
}