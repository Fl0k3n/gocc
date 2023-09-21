package elf

const GOT_SYMBOL_NAME = "_GLOBAL_OFFSET_TABLE_"
const DYNAMIC_SYMBOL_NAME = "_DYNAMIC"

type Symtab struct {
	symbols []*Symbol
	greatestLocalSymbolIdx uint32
}

func NewSymtab() *Symtab {
	return NewSymtabWith([]*Symbol{})
}

func NewSymtabWith(symbols []*Symbol) *Symtab {
	s := &Symtab{
		symbols: []*Symbol{},
	}
	for _, sym := range symbols {
		s.AddSymbol(sym)
	}
	return s
}

func (s *Symtab) AddSymbol(symbol *Symbol) (idx uint32) {
	idx = uint32(len(s.symbols))
	s.symbols = append(s.symbols, symbol)
	if symbol.Binding() == SB_LOCAL {
		s.greatestLocalSymbolIdx = idx
	}
	return idx
}

func (s *Symtab) ReorderSymbolsToHaveLocalsFirst() (reorderLUT []uint32) {
	res := make([]*Symbol, len(s.symbols))
	reorderLUT = make([]uint32, len(res))
	localsCount := uint32(0)
	for _, sym := range s.symbols {
		if sym.Binding() == SB_LOCAL {
			localsCount++
		}
	}

	localCounter := uint32(0)
	globalCounter := uint32(0)
	for idx, sym := range s.symbols {
		if sym.Binding() == SB_LOCAL {
			res[localCounter] = sym
			reorderLUT[idx] = localCounter
			localCounter++
		} else {
			res[localsCount + globalCounter] = sym
			reorderLUT[idx] = localsCount + globalCounter
			globalCounter++
		}
	}
	s.symbols = res
	s.greatestLocalSymbolIdx = localsCount - 1
	return reorderLUT
}

func (s *Symtab) GetAll() []*Symbol {
	return s.symbols
}

// omits expected null symbol at the start
func (s *Symtab) GetUndefinedSymbols() []*Symbol {
	idxs := s.GetUndefinedSymbolIdxs()
	res := make([]*Symbol, len(idxs))
	for i, idx := range idxs {
		res[i] = s.symbols[idx]
	}
	return res
}

// omits expected null symbol at the start
func (s *Symtab) GetUndefinedSymbolIdxs() []uint32 {
	res := []uint32{}
	if len(s.symbols) == 0 {
		return res
	}
	for idx := 1; idx < len(s.symbols); idx++ {
		sym := s.symbols[idx]
		if sym.Sshndx == SHN_UNDEF {
			res = append(res, uint32(idx))
		}
	}
	return res
}

func (s *Symtab) HasGlobalVariable() bool {
	for _, sym := range s.symbols {
		if sym.Binding() != SB_LOCAL && sym.Type() != ST_FUNC {
			return true
		}
	}
	return false
}

func (s *Symtab) GetSymbolWithIdx(idx uint32) *Symbol {
	return s.symbols[idx]
}

// TODO cache names if this is used often, shouldn't be tho, we got symhashtab for that
func (s *Symtab) GetSymbolIdx(symName string, strtab *Strtab) uint32 {
	for idx, sym := range s.symbols {
		name := strtab.GetStringForIndex(sym.Sname)
		if name == symName {
			return uint32(idx)
		}
	}
	panic("No symbol with name " + symName)
}

func (s *Symtab) IsDefined(symIdx uint32) bool {
	return s.symbols[symIdx].Sshndx != SHN_UNDEF
}

func (s *Symtab) GetSymbol(symName string, strtab *Strtab) *Symbol {
	return s.symbols[s.GetSymbolIdx(symName, strtab)]
}

func (s *Symtab) OverwriteSymbol(idx uint32, sym *Symbol) {
	s.symbols[idx] = sym
	if sym.Binding() == SB_LOCAL && idx > s.greatestLocalSymbolIdx {
		s.greatestLocalSymbolIdx = idx
	}
}

func (s *Symtab) ToBytes() []byte {
	res := make([]byte, len(s.symbols) * SYMBOL_SIZE)
	offset := 0
	for _, sym := range s.symbols {
		for i, b := range sym.ToBytes() {
			res[offset + i] = b
		}
		offset += SYMBOL_SIZE
	}
	return res
}

func (s *Symtab) GetGreatestLocalSymbolId() uint32 {
	return s.greatestLocalSymbolIdx
} 

func (s *Symtab) Size() int {
	return len(s.symbols)
}

func (s *Symtab) BinarySize() int {
	return s.Size() * SYMBOL_SIZE
}
