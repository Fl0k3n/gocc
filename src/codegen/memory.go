package codegen

import (
	"irs"
	"semantics"
)

const STACK_ALIGNMENT = 16
const SIZEOF_RBP = 8
const SIZEOF_RETURN_ADDR = 8

type ArgumentStorageClass int 

const (
	INTEGER ArgumentStorageClass = iota
	SSE
	MEMORY
	SPLIT // helper class
)

type MemoryManager struct {
	typeEngine *semantics.TypeEngine
	nonGlobalsMemMap map[irs.SymbolType][]MemoryAccessor
}

func NewMemoryManager(typeEngine *semantics.TypeEngine) *MemoryManager {
	return &MemoryManager{
		typeEngine: typeEngine,
	}
}

func (m *MemoryManager) getMemoryAccessor(asym *AugmentedSymbol) MemoryAccessor {
	if asym.Sym.T == irs.GLOBAL {
		if _, isFunction := asym.Sym.Ctype.(semantics.FunctionPtrCtype); isFunction {
			return PLTMemoryAccessor{
				Name: asym.Sym.Name,
			}
		} else {
			return GOTMemoryAccessor{
				Offset: asym.Sym.Index,
			}
		}
	}
	return m.nonGlobalsMemMap[asym.Sym.T][asym.Sym.Index]
}

func (m *MemoryManager) assignMemoryAccessorToSymbolUsages(fun *AugmentedFunctionIr) {
	for _, line := range fun.Code {
		for _, asym := range line.GetSymbols() {
			asym.MemoryAccessor = m.getMemoryAccessor(asym)
		}
	}
}

func (m *MemoryManager) placeOnStack(curSize int, symbolsT irs.SymbolType, snapshot []semantics.Ctype) int {
	mmap := []MemoryAccessor{}
	if symbolsT == irs.ARG {
		// check if is already in memory, if so skip it and set already allocated memory address
	}
	for _, t := range snapshot {
		prevPadding := 0
		if remainder := curSize % t.RequiredAlignment(); remainder != 0 {
			prevPadding = t.RequiredAlignment() - remainder
		}
		mmap = append(mmap, &StackFrameOffsetMemoryAccessor{
			Offset: curSize - prevPadding,
		})
		curSize -= t.Size() - prevPadding
	}
	m.nonGlobalsMemMap[symbolsT] = mmap
	return curSize
}

// curSubtract contains stack size counting from RBP
func (m *MemoryManager) alignStackPointer(curSubtract int) int {
	if remainder := curSubtract % (STACK_ALIGNMENT - SIZEOF_RETURN_ADDR); remainder != 0 {
		curSubtract += (STACK_ALIGNMENT - SIZEOF_RETURN_ADDR) - remainder
	}
	return curSubtract
}

func (m *MemoryManager) canBePassedInRegister(sym *AugmentedSymbol) bool {
	return true
}

func (m *MemoryManager) classifySymbol(asym *AugmentedSymbol) ArgumentStorageClass {
	switch t := asym.Sym.Ctype.(type) {
	case semantics.BuiltinCtype:
		if m.typeEngine.IsIntegralType(t) {
			return INTEGER
		}
		return SSE
	case semantics.FunctionPtrCtype, semantics.PointerCtype, semantics.ArrayCtype: 
		return INTEGER 
	case semantics.StructCtype:
		return SPLIT // TODO
	default:
		panic("unexpected symbol type")
	}
}

// rbpOffset contains difference from rbp to stack pointer, rbp itself must be guaranteed to be 16B aligned
// if this is called just after function prologue (push rbp; mov rbp, rsp) then rbpOffset should be 0
// if something was pushed after rbpOffset it should be negative
func (m *MemoryManager) AllocStackMemoryAndGetStackSubtract(fun *AugmentedFunctionIr, rbpOffset int) int {
	m.nonGlobalsMemMap = map[irs.SymbolType][]MemoryAccessor{}
	// stack contains rbp, locals then temps and then space for args passed in registers
	size := rbpOffset
	size = m.placeOnStack(size, irs.LOCAL, fun.Snapshot.LocalsSnapshot)
	// TODO place only those temps that really need to be placed
	size = m.placeOnStack(size, irs.TEMP, fun.Snapshot.TempsSnapshot)
	size = m.placeOnStack(size, irs.ARG, fun.Snapshot.ArgsSnapshot)
	// we must be aligned to 16 before next function call, but this includes 8B return address so we must be aligned to 8
	size = m.alignStackPointer(size)
	m.assignMemoryAccessorToSymbolUsages(fun)
	return -size
}
