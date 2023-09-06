package codegen

import (
	"fmt"
)

type RegisterFamily interface {}

type Register interface{
	Size() int
	Name() string
	Equals(Register) bool
}

type IntegralRegisterVariant int

const (
	QWORD IntegralRegisterVariant = iota
	DWORD
	WORD
	BYTE
)

const QWORD_SIZE = 8
const DWORD_SIZE = 4
const WORD_SIZE = 2
const BYTE_SIZE = 1

type IntegralRegisterFamily struct {
	QwordName string
	DwordName string
	WordName string
	ByteName string
	T IntegralRegisterFamilyT
}

func (i IntegralRegisterFamily) Use(varinat IntegralRegisterVariant) IntegralRegister {
	switch varinat {
	case QWORD:
		return IntegralRegister{Family: i, EffectiveName: i.QwordName, EffectiveSize: QWORD_SIZE}
	case DWORD:
		return IntegralRegister{Family: i, EffectiveName: i.DwordName, EffectiveSize: DWORD_SIZE}
	case WORD:
		return IntegralRegister{Family: i, EffectiveName: i.WordName, EffectiveSize: WORD_SIZE}
	case BYTE:
		return IntegralRegister{Family: i, EffectiveName: i.ByteName, EffectiveSize: BYTE_SIZE}
	default:
		panic("Unexpected variant")
	}
}

func (i IntegralRegisterFamily) UseForSize(size int) IntegralRegister {
	var variant IntegralRegisterVariant
	switch size {
	case QWORD_SIZE: variant = QWORD
	case DWORD_SIZE: variant = DWORD
	case WORD_SIZE: variant = WORD
	case BYTE_SIZE: variant = BYTE
	default: panic("Size doesn't fit to any register")
	}
	return i.Use(variant)
}

type IntegralRegister struct {
	Family IntegralRegisterFamily
	EffectiveName string
	EffectiveSize int
}

func (i IntegralRegister) Size() int {
	return i.EffectiveSize
}

func (i IntegralRegister) Name() string {
	return i.EffectiveName
}

func (i IntegralRegister) Equals(other Register) bool {
	if reg, isIntegralRegsiter := other.(IntegralRegister); isIntegralRegsiter {
		return reg.EffectiveSize == i.EffectiveSize && reg.EffectiveName == i.EffectiveName 
	}
	return false
}

type IntegralRegisterFamilyT int

const (
	RAX IntegralRegisterFamilyT = iota
	RBX; RCX; RDX; RBP; RSP; RSI; RDI
	R8; R9; R10; R11; R12; R13; R14; R15
)

var IntegralRegisterFamilies = map[IntegralRegisterFamilyT]IntegralRegisterFamily {
	RAX: {"rax", "eax", "ax", "al", RAX}, 
	RBX: {"rbx", "ebx", "bx", "bl", RBX},
	RCX: {"rcx", "ecx", "cx", "cl", RCX},
	RDX: {"rdx", "edx", "dx", "dl", RDX},
	RDI: {"rdi", "edi", "di", "dil", RDI},
	RSI: {"rsi", "esi", "si", "sil", RSI},
	RBP: {"rbp", "ebp", "bp", "bpl", RBP},
	RSP: {"rsp", "esp", "sp", "spl", RSP},
	R8:  {"r8", "r8d", "r8w", "r8l", R8},
	R9:  {"r9", "r9d", "r9w", "r9l", R9},
	R10: {"r10", "r10d", "r10w", "r10l", R10},
	R11: {"r11", "r11d", "r11w", "r11l", R11},
	R12: {"r12", "r12d", "r12w", "r12l", R12},
	R13: {"r13", "r13d", "r13w", "r13l", R13},
	R14: {"r14", "r14d", "r14w", "r14l", R14},
	R15: {"r15", "r15d", "r15w", "r15l", R15},
}

func GetIntegralRegisterFamily(fam IntegralRegisterFamilyT) IntegralRegisterFamily {
	return IntegralRegisterFamilies[fam]
}

// only xmms for simplicity
type FloatingRegisterFamily struct {
	Name string
	T FloatingRegisterFamilyT
}

func (f FloatingRegisterFamily) use() FloatingRegister {
	return FloatingRegister{Family: f}
}

type FloatingRegisterFamilyT int 

const (
	XMM0 FloatingRegisterFamilyT = iota
	XMM1; XMM2; XMM3; XMM4; XMM5; XMM6; 
	XMM7; XMM8; XMM9; XMM10; XMM11; XMM12;
	XMM13; XMM14; XMM15;
)

const FLOATING_REGISTER_SIZE = 8

type FloatingRegister struct {
	Family FloatingRegisterFamily
}

func (f FloatingRegister) Size() int {
	return FLOATING_REGISTER_SIZE
}

func (f FloatingRegister) Name() string {
	return f.Family.Name
}

func (f FloatingRegister) Equals(other Register) bool {
	if reg, isFloating := other.(FloatingRegister); isFloating {
		return reg.Family.Name == f.Family.Name
	}
	return false
}

func GetFloatingRegisterFamily(fam FloatingRegisterFamilyT) FloatingRegisterFamily {
	return FloatingRegisterFamily{Name: fmt.Sprintf("xmm%d", int(fam)), T: fam}
}

var SYSV_GENERAL_PURPOSE_INTEGRAL_REGISTERS = []IntegralRegisterFamilyT{
	RAX, RBX, RCX, RDX, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15, // without rsp and rbp
}
var SYSV_GENERAL_PURPOSE_FLOATING_REGISTERS = []FloatingRegisterFamilyT{
	XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9,
	XMM10, XMM11, XMM12, XMM13, XMM14, XMM15, // all
}
var SYSV_FUNCTION_ARG_INTEGRAL_REGISTERS = []IntegralRegisterFamilyT{
	RDI, RSI, RDX, RCX, R8, R9,
}
var SYSV_FUNCTION_ARG_FLOATING_REGISTERS = []FloatingRegisterFamilyT{
	XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7,
}
var SYSV_CALLER_SAVE_INTEGRAL_REGISTERS = []IntegralRegisterFamilyT{
	RAX, RDI, RSI, RDX, RCX, R8, R9, R10, R11,
}
var SYSV_CALLER_SAVE_FLOATING_REGISTERS = []FloatingRegisterFamilyT{
	XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9,
	XMM10, XMM11, XMM12, XMM13, XMM14, XMM15,
}
var SYSV_CALLEE_SAVE_INTEGRAL_REGISTERS = []IntegralRegisterFamilyT{
	RBP, RBX, R12, R13, R14, R15, // also rsp but thats implicit
}
var SYSV_CALLEE_SAVE_FLOATING_REGISTERS = []FloatingRegisterFamilyT{

}
const SYS_V_RETURN_INTEGRAL_REGISTER = RAX
const SYS_V_RETURN_FLOATING_REGISTER = XMM0

// div RDX:RAX operand
const DIV_OP_DIVIDENT_HIGHER_BITS_REG = RDX
const DIV_OP_DIVIDENT_LOWER_BITS_REG = RAX

const DIV_OP_RESULT_REG = RAX
const DIV_OP_REMAINDER_REG = RDX
