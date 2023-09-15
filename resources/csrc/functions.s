	.file	"functions.c"
	.intel_syntax noprefix
# GNU C17 (Ubuntu 11.4.0-1ubuntu1~22.04) version 11.4.0 (x86_64-linux-gnu)
#	compiled by GNU C version 11.4.0, GMP version 6.2.1, MPFR version 4.1.0, MPC version 1.2.1, isl version isl-0.24-GMP

# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed: -mno-red-zone -masm=intel -mtune=generic -march=x86-64 -O0 -fpic -fasynchronous-unwind-tables -fstack-protector-strong -fstack-clash-protection -fcf-protection
	.text
	.globl	A
	.bss
	.align 4
	.type	A, @object
	.size	A, 4
A:
	.zero	4
	.globl	B
	.data
	.align 4
	.type	B, @object
	.size	B, 4
B:
	.long	3
	.local	C
	.comm	C,4,4
	.align 4
	.type	D, @object
	.size	D, 4
D:
	.long	5
	.text
	.type	F, @function
F:
.LFB0:
	.cfi_startproc
	endbr64	
	push	rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp	#,
	.cfi_def_cfa_register 6
# functions.c:19:     return 1;
	mov	eax, 1	# _1,
# functions.c:20: }
	pop	rbp	#
	.cfi_restore 6
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE0:
	.size	F, .-F
	.globl	kntp
	.bss
	.align 4
	.type	kntp, @object
	.size	kntp, 4
kntp:
	.zero	4
	.globl	kntp2
	.data
	.align 4
	.type	kntp2, @object
	.size	kntp2, 4
kntp2:
# x:
	.long	1
	.globl	kntp3
	.bss
	.align 8
	.type	kntp3, @object
	.size	kntp3, 8
kntp3:
	.zero	8
	.text
	.globl	g
	.type	g, @function
g:
.LFB1:
	.cfi_startproc
	endbr64	
	push	rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp	#,
	.cfi_def_cfa_register 6
	sub	rsp, 48	#,
	mov	DWORD PTR -36[rbp], edi	# a, a
	mov	DWORD PTR -40[rbp], esi	# b, b
# functions.c:36: int g(int a, int b) {
	mov	rax, QWORD PTR fs:40	# tmp88, MEM[(<address-space-1> long unsigned int *)40B]
	mov	QWORD PTR -8[rbp], rax	# D.1983, tmp88
	xor	eax, eax	# tmp88
# functions.c:41:     x = &c; // lea [addr of x], [addr of c]
	lea	rax, -20[rbp]	# tmp85,
	mov	QWORD PTR -16[rbp], rax	# x, tmp85
# functions.c:42:     c = *x; // mov eax, [addr of x]; mov eax, [eax]; mov [c], eax
	mov	rax, QWORD PTR -16[rbp]	# tmp86, x
	mov	eax, DWORD PTR [rax]	# _1, *x_2
# functions.c:42:     c = *x; // mov eax, [addr of x]; mov eax, [eax]; mov [c], eax
	mov	DWORD PTR -20[rbp], eax	# c, _1
# functions.c:54:     return c;
	mov	eax, DWORD PTR -20[rbp]	# _5, c
# functions.c:55: }
	mov	rdx, QWORD PTR -8[rbp]	# tmp89, D.1983
	sub	rdx, QWORD PTR fs:40	# tmp89, MEM[(<address-space-1> long unsigned int *)40B]
	je	.L5	#,
	call	__stack_chk_fail@PLT	#
.L5:
	leave	
	.cfi_restore 6
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE1:
	.size	g, .-g
	.globl	f
	.type	f, @function
f:
.LFB2:
	.cfi_startproc
	endbr64	
	push	rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp	#,
	.cfi_def_cfa_register 6
	sub	rsp, 16	#,
# functions.c:64:     for (i = 0; i < 10; i++) {
	mov	DWORD PTR -4[rbp], 0	# i,
# functions.c:64:     for (i = 0; i < 10; i++) {
	jmp	.L7	#
.L10:
# functions.c:65:         if ((i % 2) == 0) {
	mov	eax, DWORD PTR -4[rbp]	# i.0_1, i
	and	eax, 1	# _2,
# functions.c:65:         if ((i % 2) == 0) {
	test	eax, eax	# _2
	jne	.L8	#,
# functions.c:66:             b = g(a, b);
	mov	edx, DWORD PTR -8[rbp]	# tmp84, b
	mov	eax, DWORD PTR -12[rbp]	# tmp85, a
	mov	esi, edx	#, tmp84
	mov	edi, eax	#, tmp85
	call	g@PLT	#
	mov	DWORD PTR -8[rbp], eax	# b, tmp86
	jmp	.L9	#
.L8:
# functions.c:69:             a++;
	add	DWORD PTR -12[rbp], 1	# a,
.L9:
# functions.c:64:     for (i = 0; i < 10; i++) {
	add	DWORD PTR -4[rbp], 1	# i,
.L7:
# functions.c:64:     for (i = 0; i < 10; i++) {
	cmp	DWORD PTR -4[rbp], 9	# i,
	jle	.L10	#,
# functions.c:72: }
	nop	
	nop	
	leave	
	.cfi_restore 6
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE2:
	.size	f, .-f
	.ident	"GCC: (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0"
	.section	.note.GNU-stack,"",@progbits
	.section	.note.gnu.property,"a"
	.align 8
	.long	1f - 0f
	.long	4f - 1f
	.long	5
0:
	.string	"GNU"
1:
	.align 8
	.long	0xc0000002
	.long	3f - 2f
2:
	.long	0x3
3:
	.align 8
4:
