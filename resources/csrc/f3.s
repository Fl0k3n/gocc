	.file	"f3.c"
	.intel_syntax noprefix
# GNU C17 (Ubuntu 11.4.0-1ubuntu1~22.04) version 11.4.0 (x86_64-linux-gnu)
#	compiled by GNU C version 11.4.0, GMP version 6.2.1, MPFR version 4.1.0, MPC version 1.2.1, isl version isl-0.24-GMP

# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed: -mno-red-zone -masm=intel -mtune=generic -march=x86-64 -O0 -fpic -fasynchronous-unwind-tables -fstack-protector-strong -fstack-clash-protection -fcf-protection
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
	sub	rsp, 32	#,
# f3.c:2:     float a = 0.3f;
	movss	xmm0, DWORD PTR .LC0[rip]	# tmp84,
	movss	DWORD PTR -20[rbp], xmm0	# a, tmp84
# f3.c:3:     float b = 0.5f;
	movss	xmm0, DWORD PTR .LC1[rip]	# tmp85,
	movss	DWORD PTR -16[rbp], xmm0	# b, tmp85
# f3.c:5:     int res = 0;
	mov	DWORD PTR -24[rbp], 0	# res,
# f3.c:8:     c = d;
	pxor	xmm0, xmm0	# tmp87
	cvtsd2ss	xmm0, QWORD PTR -8[rbp]	# tmp87, d
	movss	DWORD PTR -12[rbp], xmm0	# c, tmp87
# f3.c:9:     d = c;
	pxor	xmm0, xmm0	# tmp89
	cvtss2sd	xmm0, DWORD PTR -12[rbp]	# tmp89, c
	movsd	QWORD PTR -8[rbp], xmm0	# d, tmp89
# f3.c:10:     if (a > b) {
	movss	xmm0, DWORD PTR -20[rbp]	# tmp90, a
	comiss	xmm0, DWORD PTR -16[rbp]	# tmp90, b
	jbe	.L7	#,
# f3.c:11:         res = 1;
	mov	DWORD PTR -24[rbp], 1	# res,
	jmp	.L4	#
.L7:
# f3.c:14:         res = 2;
	mov	DWORD PTR -24[rbp], 2	# res,
.L4:
# f3.c:30:     return res;
	mov	eax, DWORD PTR -24[rbp]	# _10, res
# f3.c:31: }
	leave	
	.cfi_restore 6
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.section	.rodata
	.align 4
.LC0:
	.long	1050253722
	.align 4
.LC1:
	.long	1056964608
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
