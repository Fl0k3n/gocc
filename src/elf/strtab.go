package elf

const NULL_SYMBOL_STR = ""
const NULL_SYMBOL_STR_ID = 0

type Strtab struct {
	stringTable []byte
	definedStrings map[string]uint32
}

func NewStrtab() *Strtab {
	return &Strtab{
		stringTable: []byte{0},
		definedStrings: map[string]uint32{NULL_SYMBOL_STR: NULL_SYMBOL_STR_ID},
	}
}

func newStrtabFromBytes(bytes []byte, offset uint32, size uint32) *Strtab {
	buff := make([]byte, size)
	copy(buff, bytes[offset:offset+size])
	strtab := Strtab{
		stringTable: buff,
		definedStrings: map[string]uint32{},
	}
	for i := offset; i < offset + size; {
		j := i
		for ;j < offset + size && bytes[j] != 0; j++ {}
		strtab.definedStrings[string(bytes[i:j])] = i - offset
		i = j + 1
	}
	return &strtab
}

func (s *Strtab) PutString(name string) (idx uint32) {
	if id, ok := s.definedStrings[name]; ok {
		return id
	}
	idx = uint32(len(s.stringTable))
	s.definedStrings[name] = idx
	s.stringTable = append(s.stringTable, []byte(name)...)
	s.stringTable = append(s.stringTable, 0)
	return
}

func (s *Strtab) GetStringForIndex(idx uint32) string {
	var i uint32
	for i = idx; s.stringTable[i] != 0; i++ {}
	return string(s.stringTable[idx:i])
}

func (s *Strtab) GetIdx(name string) uint32 {
	return s.definedStrings[name]
}

func (s *Strtab) GetNullCombinedStrings() []byte {
	return s.stringTable
}

func (s *Strtab) GetSize() int {
	return len(s.stringTable)
}
