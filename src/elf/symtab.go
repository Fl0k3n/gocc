package elf

const GOT_SYMBOL_NAME = "_GLOBAL_OFFSET_TABLE_"

type Symtab struct {
	symbols []*Symbol
	greatestLocalSymbolIdx int
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

func (s *Symtab) AddSymbol(symbol *Symbol) (idx int) {
	idx = len(s.symbols)
	s.symbols = append(s.symbols, symbol)
	if symbol.Binding() == SB_LOCAL {
		s.greatestLocalSymbolIdx = idx
	}
	return idx
}

func (s *Symtab) GetAll() []*Symbol {
	return s.symbols
}

// omits expected null symbol at the start
func (s *Symtab) GetUndefinedSymbols() []*Symbol {
	res := []*Symbol{}
	if len(s.symbols) == 0 {
		return res
	}
	for _, sym := range s.symbols[1:] {
		if sym.Sshndx == SHN_UNDEF {
			res = append(res, sym)
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

// TODO cache names if this is used often
func (s *Symtab) GetSymbol(symName string, strtab *Strtab) *Symbol {
	for _, sym := range s.symbols {
		name := strtab.GetStringForIndex(sym.Sname)
		if name == symName {
			return sym
		}
	}
	panic("No symbol with name " + symName)
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
	return uint32(s.greatestLocalSymbolIdx)
} 

func (s *Symtab) Size() int {
	return len(s.symbols)
}

func (s *Symtab) BinarySize() int {
	return s.Size() * SYMBOL_SIZE
}
