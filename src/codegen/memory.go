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
	globalsMemMap []MemoryAccessor
}

func NewMemoryManager(typeEngine *semantics.TypeEngine) *MemoryManager {
	return &MemoryManager{
		typeEngine: typeEngine,
	}
}

func (m *MemoryManager) getMemoryAccessor(asym *AugmentedSymbol) MemoryAccessor {
	if asym.Sym.T == irs.GLOBAL {
		return m.globalsMemMap[asym.Sym.Index]
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
	for _, sym := range snapshot {
		t := sym.Ctype
		prevPadding := 0
		if remainder := abs(curSize) % t.RequiredAlignment(); remainder != 0 {
			prevPadding = t.RequiredAlignment() - remainder
		}
		curSize -= (t.Size() + prevPadding)
		mmap = append(mmap, StackFrameOffsetMemoryAccessor{
			Offset: curSize,
		})
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

func (m *MemoryManager) isUnsigned(sym *irs.Symbol) bool {
	return m.typeEngine.IsUnsignedType(sym.Ctype)
}

func (m *MemoryManager) setAddressesOfArgsOnStack(fun *AugmentedFunctionIr, rbpOffset int, frameOffset int, curSubtract int) int {
	mmap := make([]MemoryAccessor, len(fun.Snapshot.ArgsSnapshot))
	delta := rbpOffset + frameOffset
	for i := 0; i < len(fun.ArgsPlacedOnCallerStack); i++ {
		// TODO structs
		arg := fun.ArgsPlacedOnCallerStack[i]
		mmap[arg.Sym.Index] = StackFrameOffsetMemoryAccessor{
			Offset: delta,
		}
		delta += SIZEOF_STACK_ARG
	}
	if remainder := abs(curSubtract) % REGISTER_ARG_ALIGNMENT; remainder != 0 {
		curSubtract -= (REGISTER_ARG_ALIGNMENT + remainder)
	}
	for _, arg := range fun.InRegisterArgsToPlaceOnCalleeStack {
		curSubtract -= SIZEOF_STACK_ARG
		mmap[arg.Index] = StackFrameOffsetMemoryAccessor{
			Offset: curSubtract,
		}
	}
	m.nonGlobalsMemMap[irs.ARG] = mmap
	return curSubtract
}

func (m *MemoryManager) setAddressesOfCalleeSaveRegistersOnStack(fun *AugmentedFunctionIr, curSubtract int) int {
	if remainder := curSubtract % REGISTER_ARG_ALIGNMENT; remainder != 0 {
		curSubtract -= (REGISTER_ARG_ALIGNMENT - remainder)
	}
	for _, regWithMem := range fun.IntegralRegistersToPersist {
		curSubtract -= regWithMem.Register.Size()
		regWithMem.MemoryAccessor = StackFrameOffsetMemoryAccessor{
			Offset: curSubtract,
		}
	}
	// TODO check alignment if floating registers of size > 8B are used
	for _, regWithMem := range fun.FloatingRegistersToPersist {
		curSubtract -= regWithMem.Register.Size()
		regWithMem.MemoryAccessor = StackFrameOffsetMemoryAccessor{
			Offset: curSubtract,
		}
	}
	return curSubtract
}

func (m *MemoryManager) RequiresRegisterForCall(funSymbol *AugmentedSymbol) bool {
	if !funSymbol.isGlobal {
		return true
	}
	return !funSymbol.GlobalInfo.IsFunction
}

func (m *MemoryManager) UsesGOTAddressing(sym *AugmentedSymbol) bool {
	return sym.isGlobal && !sym.GlobalInfo.IsFunction && !sym.GlobalInfo.IsStatic
}

// rbpOffset contains difference from rbp to stack pointer, rbp itself must be guaranteed to be 16B aligned
// if this is called just after function prologue (push rbp; mov rbp, rsp) then rbpOffset should be 0
// if something was pushed after rbpOffset it should be negative
// frameOffset contains offset from start of pushed return addr to rbpOffset, so after function prologue it should be 8
func (m *MemoryManager) AllocStackMemoryAndGetStackSubtract(fun *AugmentedFunctionIr, rbpOffset int, frameOffset int) int {
	m.nonGlobalsMemMap = map[irs.SymbolType][]MemoryAccessor{}
	// stack layout: | caller frame ... args from 7' pushed right to left | ret addr, rbp 
	// locals, temps, space for args passed in registers, then callee-save-registers (if needed)
	size := rbpOffset
	size = m.placeOnStack(size, irs.LOCAL, fun.Snapshot.LocalsSnapshot)
	size = m.placeOnStack(size, irs.TEMP, fun.Snapshot.TempsSnapshot)
	size = m.setAddressesOfArgsOnStack(fun, rbpOffset, frameOffset, size)
	size = m.setAddressesOfCalleeSaveRegistersOnStack(fun, size)
	m.assignMemoryAccessorToSymbolUsages(fun)
	return -size
}

func (m *MemoryManager) GetStackPointerAlignment(frameOffset int) int {
	if remainder := abs(frameOffset) % STACK_ALIGNMENT; remainder != 0 {
		frameOffset += STACK_ALIGNMENT - remainder
	}
	return frameOffset
}

func (m *MemoryManager) AssignMemoryToGlobals(globals []*AugmentedGlobalSymbol) {
	m.globalsMemMap = make([]MemoryAccessor, len(globals))
	for _, aglobal := range globals {
		global := aglobal.Global	
		idx := global.Symbol.Index
		if global.IsFunction {
			if global.IsStatic {
				m.globalsMemMap[idx] = LabeledMemoryAccessor{
					Label: global.Symbol.Name,
				}
			} else {
				m.globalsMemMap[idx] = PLTMemoryAccessor{
					Symbol: global.Symbol,
				}
			}
		} else {
			if global.IsStatic {
				m.globalsMemMap[idx] = SectionMemoryAccessor{
					Symbol: global.Symbol,
				}
			} else {
				m.globalsMemMap[idx] = GOTMemoryAccessor{
					Symbol: global.Symbol,
				}
			}
		}
	}
}
