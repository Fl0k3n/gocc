BITS 64

.text:
ucomiss xmm0, xmm1
ucomisd xmm0, xmm1
ucomiss xmm8, xmm9
ucomisd xmm8, xmm9
ucomiss xmm8, xmm1
ucomisd xmm8, xmm1
ucomiss xmm8, xmm9
ucomisd xmm0, xmm1
ucomisd xmm0, xmm1
movss xmm0, [r12d]
movsd xmm0, [r12d]
movss xmm0, [rax]
movsd xmm0, [rax]
movss xmm8, [r12d]
movss xmm8, [rax]
