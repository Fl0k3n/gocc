	.file	"main.c"
	.intel_syntax noprefix
# GNU C17 (Ubuntu 11.4.0-1ubuntu1~22.04) version 11.4.0 (x86_64-linux-gnu)
#	compiled by GNU C version 11.4.0, GMP version 6.2.1, MPFR version 4.1.0, MPC version 1.2.1, isl version isl-0.24-GMP

# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed: -masm=intel -mtune=generic -march=x86-64 -O0 -fasynchronous-unwind-tables -fstack-protector-strong -fstack-clash-protection -fcf-protection
	.text
	.section	.rodata
.LC0:
	.string	"%p\n"
.LC1:
	.string	"%d\n"
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
# main.c:3: int main() {
	mov	rax, QWORD PTR fs:40	# tmp91, MEM[(<address-space-1> long unsigned int *)40B]
	mov	QWORD PTR -8[rbp], rax	# D.2354, tmp91
	xor	eax, eax	# tmp91
# main.c:4:     int x = 5;
	mov	DWORD PTR -24[rbp], 5	# x,
# main.c:5:     int y = 3;
	mov	DWORD PTR -20[rbp], 3	# y,
# main.c:8:     int* a = &y;
	lea	rax, -20[rbp]	# tmp85,
	mov	QWORD PTR -16[rbp], rax	# a, tmp85
# main.c:9:     a = a - 1;
	sub	QWORD PTR -16[rbp], 4	# a,
# main.c:11:     *a = 7;
	mov	rax, QWORD PTR -16[rbp]	# tmp86, a
	mov	DWORD PTR [rax], 7	# *a_6,
# main.c:13:     printf("%p\n", (void*)&x);
	lea	rax, -24[rbp]	# tmp87,
	mov	rsi, rax	#, tmp87
	lea	rax, .LC0[rip]	# tmp88,
	mov	rdi, rax	#, tmp88
	mov	eax, 0	#,
	call	printf@PLT	#
# main.c:14:     printf("%d\n", x);
	mov	eax, DWORD PTR -24[rbp]	# x.0_1, x
	mov	esi, eax	#, x.0_1
	lea	rax, .LC1[rip]	# tmp89,
	mov	rdi, rax	#, tmp89
	mov	eax, 0	#,
	call	printf@PLT	#
# main.c:15:     return 0;
	mov	eax, 0	# _10,
# main.c:16: }
	mov	rdx, QWORD PTR -8[rbp]	# tmp92, D.2354
	sub	rdx, QWORD PTR fs:40	# tmp92, MEM[(<address-space-1> long unsigned int *)40B]
	je	.L3	#,
	call	__stack_chk_fail@PLT	#
.L3:
	leave	
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
