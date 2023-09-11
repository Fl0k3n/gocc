package elf

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

func (s *Symtab) GetSymbolWithIdx(idx uint32) *Symbol {
	return s.symbols[idx]
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

func (s *Symtab) getGreatestLocalSymbolId() uint32 {
	return uint32(s.greatestLocalSymbolIdx)
} 

func (s *Symtab) Size() int {
	return len(s.symbols)
}

func (s *Symtab) BinarySize() int {
	return s.Size() * SYMBOL_SIZE
}
