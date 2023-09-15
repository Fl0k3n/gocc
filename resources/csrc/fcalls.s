	.file	"fcalls.c"
	.intel_syntax noprefix
# GNU C17 (Ubuntu 11.4.0-1ubuntu1~22.04) version 11.4.0 (x86_64-linux-gnu)
#	compiled by GNU C version 11.4.0, GMP version 6.2.1, MPFR version 4.1.0, MPC version 1.2.1, isl version isl-0.24-GMP

# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed: -masm=intel -mtune=generic -march=x86-64 -O0 -fpic -fasynchronous-unwind-tables -fstack-protector-strong -fstack-clash-protection -fcf-protection
	.text
	.globl	x
	.bss
	.align 4
	.type	x, @object
	.size	x, 4
x:
	.zero	4
	.globl	m
	.section	.data.rel,"aw"
	.align 8
	.type	m, @object
	.size	m, 8
m:
	.quad	x
	.globl	a
	.data
	.align 8
	.type	a, @object
	.size	a, 8
a:
# y:
	.zero	4
	.long	5
	.text
	.globl	main
	.type	main, @function
main:
.LFB0:
	.cfi_startproc
	endbr64	
	push	rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp	#,
	.cfi_def_cfa_register 6
# fcalls.c:31:     int a = x + 3;
	mov	eax, DWORD PTR -8[rbp]	# tmp87, x
	add	eax, 3	# tmp86,
	mov	DWORD PTR -4[rbp], eax	# a, tmp86
# fcalls.c:32:     if (a > 0) {
	cmp	DWORD PTR -4[rbp], 0	# a,
	jle	.L2	#,
# fcalls.c:33:         return -1;
	mov	eax, -1	# _1,
	jmp	.L3	#
.L2:
# fcalls.c:35:     a = x + 5;
	mov	eax, DWORD PTR -8[rbp]	# tmp91, x
	add	eax, 5	# tmp90,
	mov	DWORD PTR -4[rbp], eax	# a, tmp90
# fcalls.c:36:     return 5;
	mov	eax, 5	# _1,
.L3:
# fcalls.c:38: }
	pop	rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE0:
	.size	main, .-main
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
