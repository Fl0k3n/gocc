package codegen

import (
	"fmt"
)

type IntegralRegister struct {
	QwordName string
	DwordName string
	WordName string
	ByteName string
}

type IntegralRegisterFamily int

const (
	RAX IntegralRegisterFamily = iota
	RBX; RCX; RDX; RBP; RSP; RSI; RDI
	R8; R9; R10; R11; R12; R13; R14; R15
)

var IntegralRegisters = map[IntegralRegisterFamily]IntegralRegister {
	RAX: {"rax", "eax", "ax", "al"}, 
	RBX: {"rbx", "ebx", "bx", "bl"},
	RCX: {"rcx", "ecx", "cx", "cl"},
	RDX: {"rdx", "edx", "dx", "dl"},
	RDI: {"rdi", "edi", "di", "dil"},
	RSI: {"rsi", "esi", "si", "sil"},
	RBP: {"rbp", "ebp", "bp", "bpl"},
	RSP: {"rsp", "esp", "sp", "spl"},
	R8:  {"r8", "r8d", "r8w", "r8l"},
	R9:  {"r9", "r9d", "r9w", "r9l"},
	R10: {"r10", "r10d", "r10w", "r10l"},
	R11: {"r11", "r11d", "r11w", "r11l"},
	R12: {"r12", "r12d", "r12w", "r12l"},
	R13: {"r13", "r13d", "r13w", "r13l"},
	R14: {"r14", "r14d", "r14w", "r14l"},
	R15: {"r15", "r15d", "r15w", "r15l"},
}

const INSTRUCTION_POINTER = "rip"

func GetIntegralRegister(fam IntegralRegisterFamily) IntegralRegister {
	return IntegralRegisters[fam]
}

// we use only xmms for simplicity
type FloatingRegister struct {
	Name string
}

type FloatingRegisterFamily int 

const (
	XMM0 FloatingRegisterFamily = iota
	XMM1; XMM2; XMM3; XMM4; XMM5; XMM6; 
	XMM7; XMM8; XMM9; XMM10; XMM11; XMM12;
	XMM13; XMM14; XMM15;
)

func GetFloatingRegister(fam FloatingRegisterFamily) FloatingRegister {
	return FloatingRegister{Name: fmt.Sprintf("xmm%d", int(fam))}
}

type Register interface {}

type RegisterAllocator interface {
	Alloc(fun *AugmentedFunctionIr)
}

// roughly equivalent to the one that gcc uses with -O0
type BasicRegisterAllocator struct {

}

func (a *BasicRegisterAllocator) Alloc(fun *AugmentedFunctionIr) {
	// for _, line := range fun.Code {
	// 	switch l := line.(type) {
	// 		case 
	// 	}
	// }
}

