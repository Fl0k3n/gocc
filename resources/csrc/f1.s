	.file	"f1.c"
	.intel_syntax noprefix
# GNU C17 (Ubuntu 11.4.0-1ubuntu1~22.04) version 11.4.0 (x86_64-linux-gnu)
#	compiled by GNU C version 11.4.0, GMP version 6.2.1, MPFR version 4.1.0, MPC version 1.2.1, isl version isl-0.24-GMP

# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed: -mno-red-zone -masm=intel -mtune=generic -march=x86-64 -O0 -fpic -fasynchronous-unwind-tables -fstack-protector-strong -fstack-clash-protection -fcf-protection
	.text
	.section	.rodata
.LC0:
	.string	"abc"
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
	sub	rsp, 48	#,
# f1.c:2:     char* x = "abc";
	lea	rax, .LC0[rip]	# tmp84,
	mov	QWORD PTR -32[rbp], rax	# x, tmp84
# f1.c:3:     float a = 1;
	movss	xmm0, DWORD PTR .LC1[rip]	# tmp85,
	movss	DWORD PTR -44[rbp], xmm0	# a, tmp85
# f1.c:11:     z = a + b;
	movss	xmm0, DWORD PTR -44[rbp]	# tmp87, a
	addss	xmm0, DWORD PTR -40[rbp]	# tmp86, b
	movss	DWORD PTR -36[rbp], xmm0	# z, tmp86
# f1.c:12:     d3 = d1 + d2;
	movsd	xmm0, QWORD PTR -24[rbp]	# tmp89, d1
	addsd	xmm0, QWORD PTR -16[rbp]	# tmp88, d2
	movsd	QWORD PTR -8[rbp], xmm0	# d3, tmp88
# f1.c:13:     return 0;
	mov	eax, 0	# _8,
# f1.c:14: }
	leave	
	.cfi_restore 6
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.section	.rodata
	.align 4
.LC1:
	.long	1065353216
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
