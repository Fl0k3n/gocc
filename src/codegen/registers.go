package codegen

import (
	"fmt"
	"semantics"
	"utils"
)

type RegisterFamily interface {}

type Register interface{
	Size() int
	Name() string
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
}

func (i IntegralRegisterFamily) use(varinat IntegralRegisterVariant) IntegralRegister {
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

type IntegralRegisterFamilyT int

const (
	RAX IntegralRegisterFamilyT = iota
	RBX; RCX; RDX; RBP; RSP; RSI; RDI
	R8; R9; R10; R11; R12; R13; R14; R15
)

var IntegralRegisterFamilies = map[IntegralRegisterFamilyT]IntegralRegisterFamily {
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

func GetIntegralRegisterFamily(fam IntegralRegisterFamilyT) IntegralRegisterFamily {
	return IntegralRegisterFamilies[fam]
}

// we use only xmms for simplicity
type FloatingRegisterFamily struct {
	Name string
}

func (f FloatingRegisterFamily) use() *FloatingRegister {
	return &FloatingRegister{Family: f}
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

func GetFloatingRegisterFamily(fam FloatingRegisterFamilyT) FloatingRegisterFamily {
	return FloatingRegisterFamily{Name: fmt.Sprintf("xmm%d", int(fam))}
}

type RegisterAllocator interface {
	Alloc(fun *AugmentedFunctionIr)
	GetRegistersThatShouldBePushedOnStack() []Register // push ops should be executed in given order
	GetRegistersToWhichPopFromStack() []Register // pop ops should be executed in given order
	GetFramePointer() IntegralRegister
	GetStackPointer() IntegralRegister
}

// should be similar to the one that gcc uses with -O0
type BasicRegisterAllocator struct {
	typeEngine *semantics.TypeEngine
	generalPurposeIntegralRegisters *utils.Set[IntegralRegisterFamilyT]
	currentlyUsedIntegralRegisters *utils.Set[IntegralRegisterFamilyT]
	generalPurposeFloatingRegisters *utils.Set[FloatingRegisterFamilyT]
	currentlyUsedFloatingRegisters *utils.Set[FloatingRegisterFamilyT]
}

func NewBasicAllocator(typeEngine *semantics.TypeEngine) *BasicRegisterAllocator {
	return &BasicRegisterAllocator{
		typeEngine: typeEngine,
		generalPurposeIntegralRegisters: utils.SetOf[IntegralRegisterFamilyT](
			RAX, RBX, RCX, RSI, RDI, 
		),
		currentlyUsedIntegralRegisters: utils.NewSet[IntegralRegisterFamilyT](),
		generalPurposeFloatingRegisters: utils.SetOf[FloatingRegisterFamilyT](
			XMM0, XMM1, XMM2, XMM3, XMM4,
		),
		currentlyUsedFloatingRegisters: utils.NewSet[FloatingRegisterFamilyT](),
	}
}

func (a *BasicRegisterAllocator) freeAllGeneralPurposeRegisters() {
	a.currentlyUsedFloatingRegisters = utils.NewSet[FloatingRegisterFamilyT]()
	a.currentlyUsedIntegralRegisters = utils.NewSet[IntegralRegisterFamilyT]()
}

func (a *BasicRegisterAllocator) nextFreeIntegralRegister() IntegralRegisterFamily {
	for _, reg := range a.generalPurposeIntegralRegisters.GetAll() {
		if !a.currentlyUsedIntegralRegisters.Has(reg) {
			a.currentlyUsedIntegralRegisters.Add(reg)
			return GetIntegralRegisterFamily(reg)
		}
	}
	panic("out of integral registers") // this should never happen for this naive allocator
}

func (a *BasicRegisterAllocator) nextFreeFloatingRegister() FloatingRegisterFamily {
	for _, reg := range a.generalPurposeFloatingRegisters.GetAll() {
		if !a.currentlyUsedFloatingRegisters.Has(reg) {
			a.currentlyUsedFloatingRegisters.Add(reg)
			return GetFloatingRegisterFamily(reg)
		}
	}
	panic("out of floating registers") // this should never happen for this naive allocator
}

func (a *BasicRegisterAllocator) GetRegistersThatShouldBePushedOnStack() []Register {
	return nil
}

func (a *BasicRegisterAllocator) GetRegistersToWhichPopFromStack() []Register {
	return nil
}

func (a *BasicRegisterAllocator) Alloc(fun *AugmentedFunctionIr) {
	for _, line := range fun.Code {
		a.freeAllGeneralPurposeRegisters()
		switch l := line.(type) {
		case *AugmentedFunctionCallLine:
		case *AugmentedReturnLine:
		default:
			for _, asym := range l.GetSymbols() {
				asym.LoadBeforeRead = true
				asym.StoreAfterWrite = true	
				// TODO
				if a.typeEngine.IsIntegralType(asym.Sym.Ctype) {
					asym.Register = a.nextFreeIntegralRegister().use(DWORD) // TODO
				} else if a.typeEngine.IsFloatingType(asym.Sym.Ctype) {
					asym.Register = a.nextFreeFloatingRegister().use()
				} else {
					fmt.Println("assigning dummy register")
					asym.Register = GetIntegralRegisterFamily(R15).use(QWORD)
				}
			}
		}
	}
}

func (a *BasicRegisterAllocator) GetFramePointer() IntegralRegister {
	return GetIntegralRegisterFamily(RBP).use(QWORD)
}

func (a *BasicRegisterAllocator) GetStackPointer() IntegralRegister {
	return GetIntegralRegisterFamily(RSP).use(QWORD)
}
