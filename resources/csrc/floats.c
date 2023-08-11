#include "stdio.h"

int main() {
    float x = 1421.123;
    float y = 123;
    float z = 123.;
    float a1 = 123.f;
    float a2 = 123.1f;
    double a4 = 123.0f;
    printf("%f\n", x + y + z + a1 + a2);
    return 0;
}