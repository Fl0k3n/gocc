	.file	"b5.c"
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
# b5.c:2:     int a0 = 0;
	mov	DWORD PTR -28[rbp], 0	# a0,
# b5.c:3:     int a1 = 1;
	mov	DWORD PTR -24[rbp], 1	# a1,
# b5.c:4:     int a2 = 2;
	mov	DWORD PTR -20[rbp], 2	# a2,
# b5.c:5:     int a3 = 3;
	mov	DWORD PTR -16[rbp], 3	# a3,
# b5.c:6:     int a4 = 4;
	mov	DWORD PTR -12[rbp], 4	# a4,
# b5.c:7:     int a5 = 5;
	mov	DWORD PTR -8[rbp], 5	# a5,
# b5.c:8:     int z = a0 && a1;
	cmp	DWORD PTR -28[rbp], 0	# a0,
	je	.L2	#,
# b5.c:8:     int z = a0 && a1;
	cmp	DWORD PTR -24[rbp], 0	# a1,
	je	.L2	#,
# b5.c:8:     int z = a0 && a1;
	mov	eax, 1	# iftmp.0_1,
	jmp	.L3	#
.L2:
# b5.c:8:     int z = a0 && a1;
	mov	eax, 0	# iftmp.0_1,
.L3:
# b5.c:8:     int z = a0 && a1;
	mov	DWORD PTR -4[rbp], eax	# z, iftmp.0_1
# b5.c:9:     return z;
	mov	eax, DWORD PTR -4[rbp]	# _11, z
# b5.c:10: }
	leave	
	.cfi_restore 6
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
