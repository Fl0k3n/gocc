package codegen

import (
	"irs"
	"semantics"
)

const STACK_ALIGNMENT = 16
const REGISTER_ARG_ALIGNMENT = 8
const SIZEOF_RBP = 8
const SIZEOF_RETURN_ADDR = 8
const SIZEOF_STACK_ARG = 8

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
	for _, arg := range fun.Args {
		arg.MemoryAccessor = m.getMemoryAccessor(arg)
	}
	for _, arg := range fun.ArgsPlacedOnCallerStack {
		arg.MemoryAccessor = m.getMemoryAccessor(arg)
	}
	for _, arg := range fun.InRegisterArgsToStoreAfterFunctionEnter {
		arg.MemoryAccessor = m.getMemoryAccessor(arg)
	}
}

func (m *MemoryManager) placeOnStack(curSize int, symbolsT irs.SymbolType, snapshot []*irs.Symbol) int {
	mmap := []MemoryAccessor{}
	if symbolsT == irs.ARG {
		// check if is already in memory, if so skip it and set already allocated memory address
	}
	for _, sym := range snapshot {
		t := sym.Ctype
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

func (m *MemoryManager) canBePassedInRegister(sym *AugmentedSymbol) bool {
	return true
}

func (m *MemoryManager) classifySymbol(sym *irs.Symbol) ArgumentStorageClass {
	switch t := sym.Ctype.(type) {
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

func (m *MemoryManager) setAddressesOfArgsOnStack(fun *AugmentedFunctionIr, rbpOffset int, frameOffset int, curSubtract int) int {
	mmap := make([]MemoryAccessor, len(fun.Snapshot.ArgsSnapshot))
	delta := rbpOffset + frameOffset
	for _, arg := range fun.ArgsPlacedOnCallerStack {
		// TODO structs
		mmap[arg.Sym.Index] = &StackFrameOffsetMemoryAccessor{
			Offset: delta,
		}
		delta += SIZEOF_STACK_ARG
	}
	if remainder := curSubtract % REGISTER_ARG_ALIGNMENT; remainder != 0 {
		curSubtract -= (REGISTER_ARG_ALIGNMENT - remainder)
	}
	for _, arg := range fun.InRegisterArgsToPlaceOnCalleeStack {
		mmap[arg.Index] = &StackFrameOffsetMemoryAccessor{
			Offset: curSubtract,
		}
		curSubtract -= SIZEOF_STACK_ARG
	}
	m.nonGlobalsMemMap[irs.ARG] = mmap
	return curSubtract
}


// rbpOffset contains difference from rbp to stack pointer, rbp itself must be guaranteed to be 16B aligned
// if this is called just after function prologue (push rbp; mov rbp, rsp) then rbpOffset should be 0
// if something was pushed after rbpOffset it should be negative
// frameOffset contains offset from start of pushed return addr to rbpOffset, so after function prologue it should be 16
func (m *MemoryManager) AllocStackMemoryAndGetStackSubtract(fun *AugmentedFunctionIr, rbpOffset int, frameOffset int) int {
	m.nonGlobalsMemMap = map[irs.SymbolType][]MemoryAccessor{}
	// stack layout: | caller frame ... args from 7' pushed right to left | ret addr, rbp 
	// locals, temps, space for args passed in registers, then callee-save-registers (if needed)
	size := rbpOffset
	size = m.placeOnStack(size, irs.LOCAL, fun.Snapshot.LocalsSnapshot)
	size = m.placeOnStack(size, irs.TEMP, fun.Snapshot.TempsSnapshot)
	size = m.setAddressesOfArgsOnStack(fun, rbpOffset, frameOffset, size)
	// we must be aligned to 16 before next function call, but this includes 8B return address so we must be aligned to 8
	m.assignMemoryAccessorToSymbolUsages(fun)
	return -size
}

func (m *MemoryManager) GetStackPointerAlignment(frameOffset int) int {
	if remainder := frameOffset % STACK_ALIGNMENT; remainder != 0 {
		frameOffset += STACK_ALIGNMENT - remainder
	}
	return frameOffset
}