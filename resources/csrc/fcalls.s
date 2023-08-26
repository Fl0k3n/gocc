	.file	"fcalls.c"
	.intel_syntax noprefix
# GNU C17 (Ubuntu 11.4.0-1ubuntu1~22.04) version 11.4.0 (x86_64-linux-gnu)
#	compiled by GNU C version 11.4.0, GMP version 6.2.1, MPFR version 4.1.0, MPC version 1.2.1, isl version isl-0.24-GMP

# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed: -masm=intel -mtune=generic -march=x86-64 -O0 -fasynchronous-unwind-tables -fstack-protector-strong -fstack-clash-protection -fcf-protection
	.text
	.globl	q
	.type	q, @function
q:
.LFB0:
	.cfi_startproc
	endbr64	
	push	rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp	#,
	.cfi_def_cfa_register 6
# fcalls.c:24: }
	nop	
	pop	rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE0:
	.size	q, .-q
	.globl	x
	.data
	.align 4
	.type	x, @object
	.size	x, 4
x:
	.long	1
	.text
	.globl	f
	.type	f, @function
f:
.LFB1:
	.cfi_startproc
	endbr64	
	push	rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp	#,
	.cfi_def_cfa_register 6
	sub	rsp, 24	#,
	mov	DWORD PTR -20[rbp], edi	# v, v
# fcalls.c:30:     int x = a.a3 + v;
	mov	rax, QWORD PTR 32[rbp]	# _1, a.a3
# fcalls.c:30:     int x = a.a3 + v;
	mov	edx, eax	# _2, _1
	mov	eax, DWORD PTR -20[rbp]	# v.0_3, v
	add	eax, edx	# _4, _2
# fcalls.c:30:     int x = a.a3 + v;
	mov	DWORD PTR -4[rbp], eax	# x, _4
# fcalls.c:31:     q();
	mov	eax, 0	#,
	call	q	#
# fcalls.c:32:     return 1 + x;
	mov	eax, DWORD PTR -4[rbp]	# tmp88, x
	add	eax, 1	# _9,
# fcalls.c:33: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE1:
	.size	f, .-f
	.globl	g
	.type	g, @function
g:
.LFB2:
	.cfi_startproc
	endbr64	
	push	rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp	#,
	.cfi_def_cfa_register 6
	sub	rsp, 64	#,
# fcalls.c:37:     a.a1 = 1;
	mov	QWORD PTR -64[rbp], 1	# a.a1,
# fcalls.c:38:     a.a2 = 1;
	mov	QWORD PTR -56[rbp], 1	# a.a2,
# fcalls.c:39:     a.a3 = 1;
	mov	QWORD PTR -48[rbp], 1	# a.a3,
# fcalls.c:40:     f(5, a);
	push	QWORD PTR -16[rbp]	# a
	push	QWORD PTR -24[rbp]	# a
	push	QWORD PTR -32[rbp]	# a
	push	QWORD PTR -40[rbp]	# a
	push	QWORD PTR -48[rbp]	# a
	push	QWORD PTR -56[rbp]	# a
	push	QWORD PTR -64[rbp]	# a
	mov	edi, 5	#,
	call	f	#
	add	rsp, 56	#,
# fcalls.c:41: }
	nop	
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE2:
	.size	g, .-g
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
