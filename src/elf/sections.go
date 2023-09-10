package elf

const NULL_SECTION = ""
const TEXT = ".text"
const DATA = ".data"
const BSS = ".bss"
const SYMTAB = ".symtab"
const STRTAB = ".strtab"
const SECTION_STRTAB = ".shstrtab"

const RELA_TEXT = ".rela.text"

const NULL_SECTION_IDX = 0

const UNKNOWN_SECTION_ALIGNEMNT = 0

const SHN_ABS uint16 = 0xfff1
const SHN_UNDEF uint16 = 0

type SectionHdrTable struct {
	sectionStrtab *Strtab
	sectionHeaders []SectionHeader
	sectionIndexes map[string]uint16
}

func newSectionHdrTable() *SectionHdrTable {
	return &SectionHdrTable{
		sectionStrtab: newStrtab(),
		sectionIndexes: map[string]uint16{},
	}
}

func (s *SectionHdrTable) AllocSectionHeaders(sectionIdxs map[string]uint16) {
	s.sectionHeaders = make([]SectionHeader, len(sectionIdxs))
	orderedSectionNames := make([]string, len(sectionIdxs))
	for k, v := range sectionIdxs {
		orderedSectionNames[v] = k
	}
	for _, sectionName := range orderedSectionNames {
		s.sectionStrtab.PutString(sectionName)
	}
	s.sectionIndexes = sectionIdxs
}

func (s *SectionHdrTable) GetSectionStrtab() *Strtab {
	return s.sectionStrtab
}

func (s *SectionHdrTable) GetSectionHeaders() []SectionHeader {
	return s.sectionHeaders
}

func (s *SectionHdrTable) GetSectionIdx(sectionName string) uint16 {
	return s.sectionIndexes[sectionName]
}

func (s *SectionHdrTable) HasSection(sectionName string) bool {
	_, ok := s.sectionIndexes[sectionName]
	return ok
}

func (s *SectionHdrTable) CreateNullSection() {
	s.sectionHeaders[NULL_SECTION_IDX] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(NULL_SECTION),
		Stype: uint32(S_NULL),
		Sflags: 0,
		Saddr: 0,
		Soffset: 0,
		Ssize: 0,
		Slink: 0,
		Sinfo: 0,
		Saddralign: 0,
		Sentsize: 0,
	}
}

func (s *SectionHdrTable) CreateTextSectionHeader(fileStartOffset int, size int) {
	s.sectionHeaders[s.GetSectionIdx(TEXT)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(TEXT),
		Stype: uint32(S_PROGBITS),
		Sflags: uint64(S_ALLOC) | uint64(S_EXEC),
		Saddr: 0, // this should be set at linking stage, for other sections too
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: 0,
		Sinfo: 0,
		Saddralign: 1,
		Sentsize: 0,
	}
}

func (s *SectionHdrTable) CreateRelaTextSectionHeader(fileStartOffset int, size int, alignment int) {
	s.sectionHeaders[s.GetSectionIdx(RELA_TEXT)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(RELA_TEXT),
		Stype: uint32(S_RELA),
		Sflags: S_INFO,
		Saddr: 0,
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: uint32(s.GetSectionIdx(SYMTAB)),
		Sinfo: uint32(s.GetSectionIdx(TEXT)),
		Saddralign: uint64(alignment),
		Sentsize: RELA_ENTRY_SIZE,
	}
}

func (s *SectionHdrTable) CreateDataSection(fileStartOffset int, size int, alignment int) {
	s.sectionHeaders[s.GetSectionIdx(DATA)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(DATA),  
		Stype: uint32(S_PROGBITS),
		Sflags: uint64(S_ALLOC) | uint64(S_WRITE),
		Saddr: 0,
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: 0,
		Sinfo: 0,
		Saddralign: uint64(alignment),
		Sentsize: 0,
	}
}

func (s *SectionHdrTable) CreateBssSection(fileStartOffset int, size int, alignment int) {
	s.sectionHeaders[s.GetSectionIdx(BSS)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(BSS),  
		Stype: uint32(S_NOBITS),
		Sflags: uint64(S_ALLOC) | uint64(S_WRITE),
		Saddr: 0,
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: 0,
		Sinfo: 0,
		Saddralign: uint64(alignment),
		Sentsize: 0,
	}
}

func (s *SectionHdrTable) CreateSymtabSection(fileStartOffset int, size int, greatestLocalSymbolId uint32) {
	s.sectionHeaders[s.GetSectionIdx(SYMTAB)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(SYMTAB),  
		Stype: uint32(S_SYMTAB),
		Sflags: 0,
		Saddr: 0,
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: uint32(s.GetSectionIdx(STRTAB)),
		Sinfo: greatestLocalSymbolId + 1,
		Saddralign: 8, // TODO check if this always applies, according to the struct it could be 4 too
		Sentsize: SYMBOL_SIZE,
	}
}

func (s *SectionHdrTable) CreateStrtabSection(fileStartOffset int, size int) {
	s.sectionHeaders[s.GetSectionIdx(STRTAB)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(STRTAB),
		Stype: uint32(S_STRRAB),
		Sflags: 0, // don't load strtab 
		Saddr: 0,
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: 0,
		Sinfo: 0,
		Saddralign: 1,
		Sentsize: 0,
	}
}

func (s *SectionHdrTable) CreateSectionStrtabSection(fileStartOffset int, size int) {
	s.sectionHeaders[s.GetSectionIdx(SECTION_STRTAB)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(SECTION_STRTAB),
		Stype: uint32(S_STRRAB),
		Sflags: 0, // don't load 
		Saddr: 0,
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: 0,
		Sinfo: 0,
		Saddralign: 1,
		Sentsize: 0,
	}
}

func (s *SectionHdrTable) ToBytes() []byte {
	res := make([]byte, len(s.sectionHeaders) * SECTION_HEADER_SIZE)
	idx := 0
	for _, sh := range s.sectionHeaders {
		shBytes := sh.ToBytes()
		for i, b := range shBytes {
			res[idx + i] = b 
		}
		idx += len(shBytes)
	}
	return res
}
