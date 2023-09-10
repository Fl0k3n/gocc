package elf

const NULL_SYMBOL_STR = ""
const NULL_SYMBOL_STR_ID = 0

type Strtab struct {
	stringTable []string
	stringTableSize uint32
	definedStrings map[string]uint32
}

func newStrtab() *Strtab {
	return &Strtab{
		stringTable: []string{NULL_SYMBOL_STR},
		stringTableSize: 1,
		definedStrings: map[string]uint32{NULL_SYMBOL_STR: NULL_SYMBOL_STR_ID},
	}
}

func (s *Strtab) PutString(name string) (idx uint32) {
	if id, ok := s.definedStrings[name]; ok {
		return id
	}
	idx = s.stringTableSize
	s.definedStrings[name] = idx
	s.stringTableSize += uint32(len(name)) + 1
	s.stringTable = append(s.stringTable, name)
	return
}

func (s *Strtab) GetIdx(name string) uint32 {
	return s.definedStrings[name]
}

func (s *Strtab) GetNullCombinedStrings() []byte {
	const NULL byte = 0
	res := make([]byte, s.stringTableSize)
	idx := 0
	for _, str := range s.stringTable {
		for _, c := range []byte(str) {
			res[idx] = c
			idx++
		}
		res[idx] = NULL
		idx++
	}
	return res
}

func (s *Strtab) GetSize() int {
	return int(s.stringTableSize)
}
