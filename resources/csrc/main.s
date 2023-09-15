	.file	"main.c"
	.intel_syntax noprefix
# GNU C17 (Ubuntu 11.4.0-1ubuntu1~22.04) version 11.4.0 (x86_64-linux-gnu)
#	compiled by GNU C version 11.4.0, GMP version 6.2.1, MPFR version 4.1.0, MPC version 1.2.1, isl version isl-0.24-GMP

# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed: -masm=intel -mtune=generic -march=x86-64 -O0 -fasynchronous-unwind-tables -fstack-protector-strong -fstack-clash-protection -fcf-protection
	.text
	.section	.rodata
.LC0:
	.string	"%d\n"
	.text
	.globl	main
	.type	main, @function
main:
.LFB6:
	.cfi_startproc
	endbr64	
	push	rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp	#,
	.cfi_def_cfa_register 6
	sub	rsp, 16	#,
# main.c:6:     int x = rand();
	call	rand@PLT	#
	mov	DWORD PTR -16[rbp], eax	# x, tmp85
# main.c:7:     int y = rand();
	call	rand@PLT	#
	mov	DWORD PTR -12[rbp], eax	# y, tmp86
# main.c:13:     int z = x + y;
	mov	edx, DWORD PTR -16[rbp]	# tmp91, x
	mov	eax, DWORD PTR -12[rbp]	# tmp92, y
	add	eax, edx	# tmp90, tmp91
	mov	DWORD PTR -8[rbp], eax	# z, tmp90
# main.c:14:     int v = x + rand();
	call	rand@PLT	#
# main.c:14:     int v = x + rand();
	mov	edx, DWORD PTR -16[rbp]	# tmp96, x
	add	eax, edx	# tmp95, tmp96
	mov	DWORD PTR -4[rbp], eax	# v, tmp95
# main.c:18:     printf("%d\n", x);
	mov	eax, DWORD PTR -16[rbp]	# tmp97, x
	mov	esi, eax	#, tmp97
	lea	rax, .LC0[rip]	# tmp98,
	mov	rdi, rax	#, tmp98
	mov	eax, 0	#,
	call	printf@PLT	#
# main.c:19:     printf("%d\n", z);
	mov	eax, DWORD PTR -8[rbp]	# tmp99, z
	mov	esi, eax	#, tmp99
	lea	rax, .LC0[rip]	# tmp100,
	mov	rdi, rax	#, tmp100
	mov	eax, 0	#,
	call	printf@PLT	#
# main.c:20:     printf("%d\n", v);
	mov	eax, DWORD PTR -4[rbp]	# tmp101, v
	mov	esi, eax	#, tmp101
	lea	rax, .LC0[rip]	# tmp102,
	mov	rdi, rax	#, tmp102
	mov	eax, 0	#,
	call	printf@PLT	#
# main.c:21:     return 0;
	mov	eax, 0	# _13,
# main.c:22: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE6:
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
