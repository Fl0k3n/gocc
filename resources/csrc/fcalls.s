	.file	"fcalls.c"
	.intel_syntax noprefix
# GNU C17 (Ubuntu 11.4.0-1ubuntu1~22.04) version 11.4.0 (x86_64-linux-gnu)
#	compiled by GNU C version 11.4.0, GMP version 6.2.1, MPFR version 4.1.0, MPC version 1.2.1, isl version isl-0.24-GMP

# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed: -masm=intel -mtune=generic -march=x86-64 -O0 -fasynchronous-unwind-tables -fstack-protector-strong -fstack-clash-protection -fcf-protection
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
# fcalls.c:10:     int a = 3;
	mov	DWORD PTR -12[rbp], 3	# a,
# fcalls.c:11:     int b = 4;
	mov	DWORD PTR -8[rbp], 4	# b,
# fcalls.c:12:     int c = a = b;
	mov	eax, DWORD PTR -8[rbp]	# tmp84, b
	mov	DWORD PTR -12[rbp], eax	# a, tmp84
# fcalls.c:12:     int c = a = b;
	mov	eax, DWORD PTR -12[rbp]	# tmp85, a
	mov	DWORD PTR -4[rbp], eax	# c, tmp85
	mov	eax, 0	# _5,
# fcalls.c:13: }
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
