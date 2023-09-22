// // float x = 1;

// float y = 0.5f;

// extern void init_z();

// extern float do_floating_ops(float);

// float float_arithm() {
//     // float x = 1;
//     // char* x = "abc";
//     float a = 1.1f;
//     float b = 2.2f;
//     float z;
//     z = a + b;
//     init_z();
//     z = do_floating_ops(z);
//     return z;
// }

// char* string_stuff() {
//     char* res = "abcd";
//     return res;
// }

float x = 3.0f;

extern float do_floating_ops(float);

float float_arithm() {
    float f = 2.0f;
    return do_floating_ops(f + x);
}
