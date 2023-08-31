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
)

type MemoryManager struct {
	nonGlobalsMemMap map[irs.SymbolType][]MemoryAccessor
}

func newMemoryManager() *MemoryManager {
	return &MemoryManager{}
}

func (m *MemoryManager) getMemoryAccessor(asym *AugmentedSymbol) MemoryAccessor {
	if asym.Sym.T == irs.GLOBAL {
		return nil // TODO
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
			Offset: curSize + prevPadding,
		})
		curSize += t.Size() + prevPadding
	}
	m.nonGlobalsMemMap[symbolsT] = mmap
	return curSize
}

// curSubtract contains stack size counting from RBP
func (*MemoryManager) alignStackPointer(curSubtract int) int {
	if remainder := curSubtract % (STACK_ALIGNMENT - SIZEOF_RETURN_ADDR); remainder != 0 {
		curSubtract += (STACK_ALIGNMENT - SIZEOF_RETURN_ADDR) - remainder
	}
	return curSubtract
}

// rbpOffset contains difference from rbp to stack pointer, rbp itself must be guaranteed to be 16B aligned
// if this is called just after function prologue (push rbp; mov rbp, rsp) then rbpOffset should be 0
func (m *MemoryManager) AllocStackMemoryAndGetStackSubtract(fun *AugmentedFunctionIr, rbpOffset int) int {
	m.nonGlobalsMemMap = map[irs.SymbolType][]MemoryAccessor{}
	// stack contains rbp, locals then temps and then space for args passed in registers
	size := rbpOffset
	size = m.placeOnStack(size, irs.LOCAL, fun.Snapshot.LocalsSnapshot)
	// TODO place only those temps that really need to be placed
	size = m.placeOnStack(size, irs.TEMP, fun.Snapshot.TempsSnapshot)
	size = m.placeOnStack(size, irs.ARG, fun.Snapshot.ArgsSnapshot)
	// we must be aligned to 16 before next function call, but this includes 8B return address so we must be aligned to 8
	size -= rbpOffset
	size = m.alignStackPointer(size)
	m.assignMemoryAccessorToSymbolUsages(fun)
	return size
}
