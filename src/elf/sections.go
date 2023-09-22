package elf

const NULL_SECTION = ""
const TEXT = ".text"
const DATA = ".data"
const RO_DATA = ".rodata"
const BSS = ".bss"
const SYMTAB = ".symtab"
const STRTAB = ".strtab"
const SECTION_STRTAB = ".shstrtab"

const INTERP = ".interp"
const HASH_SECTION = ".hash"
const DYNAMIC = ".dynamic"
const DYNSYM = ".dynsym"
const DYNSTR = ".dynstr"
const GOT = ".got"
const PLT = ".plt"
const GOT_PLT = ".got.plt"

const RELA_TEXT = ".rela.text"
const RELA_DYN = ".rela.dyn"
const RELA_PLT = ".rela.plt"

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
		sectionStrtab: NewStrtab(),
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

// keep old sections for new indexes, leave new blank, remove ones without index
func (s *SectionHdrTable) Reindex(sectionIdxs map[string]uint16) (reindexMap map[uint16]uint16) {
	reindexMap = map[uint16]uint16{}
	oldSectionHeaders := s.sectionHeaders
	oldIndexes := s.sectionIndexes
	s.AllocSectionHeaders(sectionIdxs)
	for oldName, oldIdx := range oldIndexes {
		if newIdx, ok := sectionIdxs[oldName]; ok {
			reindexMap[oldIdx] = newIdx
			s.sectionHeaders[newIdx] = oldSectionHeaders[oldIdx]
		}
	}
	return
}

func (s *SectionHdrTable) CreateSectionHeaderWithDefaults(name string) {
	switch name {
	case NULL_SECTION: s.CreateNullSection()
	case TEXT: s.CreateTextSectionHeader(0, 0)
	case DATA: s.CreateDataSection(0, 0, 0)
	case RO_DATA: s.CreateRodataSection(0, 0, 0)
	case BSS: s.CreateBssSection(0, 0, 0)
	case SYMTAB: s.CreateSymtabSection(0, 0, 0)
	case STRTAB: s.CreateStrtabSection(0, 0)
	case SECTION_STRTAB: s.CreateSectionStrtabSection(0, 0)
 	case HASH_SECTION: s.CreateHashSection(0, 0, 0)
	case DYNAMIC: s.CreateDynamicSection(0, 0, 0)
	case DYNSYM: s.CreateDynsymSection(0, 0, 0)
	case DYNSTR: s.CreateDynstrSection(0, 0)
	case GOT: s.CreateGOTSection(0, 0)
	case PLT: s.CreatePLTSection(0, 0, 0)
	case GOT_PLT: s.CreateGOTPLTSection(0, 0)
	case RELA_TEXT: s.CreateRelaTextSectionHeader(0, 0)
	case RELA_DYN: s.CreateRelaDynSectionHeader(0, 0)
	case RELA_PLT: s.CreateRelaPltSectionHeader(0, 0)
	case INTERP: s.CreateInterpSectionHeader(0, 0)
	}
}

func (s *SectionHdrTable) GetSectionStrtab() *Strtab {
	return s.sectionStrtab
}

func (s *SectionHdrTable) GetSectionHeaders() []SectionHeader {
	return s.sectionHeaders
}

func (s *SectionHdrTable) GetSectionIndexes() map[string]uint16 {
	return s.sectionIndexes
}

func (s *SectionHdrTable) GetSectionIdx(sectionName string) uint16 {
	return s.sectionIndexes[sectionName]
}

func (s *SectionHdrTable) HasSection(sectionName string) bool {
	_, ok := s.sectionIndexes[sectionName]
	return ok
}

func (s *SectionHdrTable) GetHeader(sectionName string) *SectionHeader {
	return &s.sectionHeaders[s.GetSectionIdx(sectionName)]
}

func (s *SectionHdrTable) MaybeHeader(sectionName string) (res *SectionHeader, present bool) {
	if !s.HasSection(sectionName) {
		return nil, false
	}
	return s.GetHeader(sectionName), true
}

func (s *SectionHdrTable) GetHeaderByIdx(sectionIdx uint16) *SectionHeader {
	return &s.sectionHeaders[sectionIdx]
}

func (s *SectionHdrTable) NumberOfSections() uint16 {
	return uint16(len(s.sectionHeaders))
}

func (s *SectionHdrTable) AppendSectionHeader(sh SectionHeader, sectionName string) uint16 {
	res := uint16(len(s.sectionHeaders))
	sh.Sname = s.sectionStrtab.PutString(sectionName)	
	s.sectionIndexes[sectionName] = res
	s.sectionHeaders = append(s.sectionHeaders, sh)
	return res
}

func (s *SectionHdrTable) GetSectionName(sectionIdx uint16) string {
	return s.sectionStrtab.GetStringForIndex(s.sectionHeaders[sectionIdx].Sname)
}

func (s *SectionHdrTable) GetSectionWithHighestFileOffset() *SectionHeader {
	// TODO this is not guaranteed by elf spec in general
	return &s.sectionHeaders[len(s.sectionHeaders) - 1]
}

func (s *SectionHdrTable) ChangeSectionSizeAndShiftSuccedingSections(sectionIdx uint16, sizeDelta int) {
	if sizeDelta == 0 {
		return
	}
	section := s.GetHeaderByIdx(sectionIdx)	
	section.Ssize = uint64(int64(section.Ssize) + int64(sizeDelta))
	for i := sectionIdx + 1; i < s.NumberOfSections(); i++ {
		section := s.GetHeaderByIdx(i)
		section.Soffset = uint64(int64(section.Soffset) + int64(sizeDelta))
	}
}

func (s *SectionHdrTable) CreateNullSection() {
	s.sectionHeaders[NULL_SECTION_IDX] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(NULL_SECTION),
		Stype: S_NULL,
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
		Stype: S_PROGBITS,
		Sflags: S_ALLOC | S_WRITE,
		Saddr: 0, // this should be set at linking stage, for other sections too
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: 0,
		Sinfo: 0,
		Saddralign: 1,
		Sentsize: 0,
	}
}

func (s *SectionHdrTable) CreateRelaTextSectionHeader(fileStartOffset int, size int) {
	s.sectionHeaders[s.GetSectionIdx(RELA_TEXT)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(RELA_TEXT),
		Stype: S_RELA,
		Sflags: S_INFO,
		Saddr: 0,
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: uint32(s.GetSectionIdx(SYMTAB)),
		Sinfo: uint32(s.GetSectionIdx(TEXT)),
		Saddralign: 8,
		Sentsize: RELA_ENTRY_SIZE,
	}
}

func (s *SectionHdrTable) CreateDataSection(fileStartOffset int, size int, alignment int) {
	s.sectionHeaders[s.GetSectionIdx(DATA)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(DATA),  
		Stype: S_PROGBITS,
		Sflags: S_ALLOC | S_WRITE,
		Saddr: 0,
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: 0,
		Sinfo: 0,
		Saddralign: uint64(alignment),
		Sentsize: 0,
	}
}

func (s *SectionHdrTable) CreateGOTSection(fileStartOffset int, size int) {
	s.sectionHeaders[s.GetSectionIdx(GOT)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(GOT),  
		Stype: S_PROGBITS,
		Sflags: S_ALLOC | S_WRITE, // TODO it probably shouldn't be writeable for non-dynamically linked execs
		Saddr: 0,
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: 0,
		Sinfo: 0,
		Saddralign: 8,
		Sentsize: 8,
	}
}

func (s *SectionHdrTable) CreateBssSection(fileStartOffset int, size int, alignment int) {
	s.sectionHeaders[s.GetSectionIdx(BSS)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(BSS),  
		Stype: S_NOBITS,
		Sflags: S_ALLOC | S_WRITE,
		Saddr: 0,
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: 0,
		Sinfo: 0,
		Saddralign: uint64(alignment),
		Sentsize: 0,
	}
}

func (s *SectionHdrTable) CreateRodataSection(fileStartOffset int, size int, alignment int) {
	s.sectionHeaders[s.GetSectionIdx(RO_DATA)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(RO_DATA),  
		Stype: S_PROGBITS,
		Sflags: S_ALLOC,
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
		Stype: S_SYMTAB,
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
		Stype: S_STRRAB,
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
		Stype: S_STRRAB,
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

func (s *SectionHdrTable) CreateHashSection(fileStartOffset int, size int, linkedSymtabIdx uint32) {
	s.sectionHeaders[s.GetSectionIdx(HASH_SECTION)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(HASH_SECTION),
		Stype: S_HASH,
		Sflags: S_ALLOC,
		Saddr: 0,
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: linkedSymtabIdx,
		Sinfo: 0,
		Saddralign: 8,
		Sentsize: HASH_ENTRY_SIZE,
	}
}

func (s *SectionHdrTable) CreateDynamicSection(
	fileStartOffset int,
	size int,
	linkedStrtabIdx uint32,
) {
	s.sectionHeaders[s.GetSectionIdx(DYNAMIC)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(DYNAMIC),
		Stype: S_DYNAMIC,
		Sflags: S_ALLOC | S_WRITE,
		Saddr: 0,
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: linkedStrtabIdx,
		Sinfo: 0,
		Saddralign: 8,
		Sentsize: DYNAMIC_ENTRY_SIZE,
	}
}

func (s *SectionHdrTable) CreateDynsymSection(fileStartOffset int, size int, greatestLocalSymbolId uint32) {
	s.sectionHeaders[s.GetSectionIdx(DYNSYM)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(DYNSYM),  
		Stype: S_DYNSYM,
		Sflags: S_ALLOC,
		Saddr: 0,
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: uint32(s.GetSectionIdx(DYNSTR)),
		Sinfo: greatestLocalSymbolId + 1,
		Saddralign: 8,
		Sentsize: SYMBOL_SIZE,
	}
}

func (s *SectionHdrTable) CreateDynstrSection(fileStartOffset int, size int) {
	s.sectionHeaders[s.GetSectionIdx(DYNSTR)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(DYNSTR),
		Stype: S_STRRAB,
		Sflags: S_ALLOC,
		Saddr: 0,
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: 0,
		Sinfo: 0,
		Saddralign: 1,
		Sentsize: 0,
	}
}

func (s *SectionHdrTable) CreatePLTSection(fileStartOffset int, size int, entrySize uint64) {
	s.sectionHeaders[s.GetSectionIdx(PLT)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(PLT),
		Stype: S_PROGBITS,
		Sflags: S_ALLOC | S_EXEC,
		Saddr: 0,
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: 0,
		Sinfo: 0,
		Saddralign: uint64(PLT_ENTRY_SIZE),
		Sentsize: entrySize,
	}
}

func (s *SectionHdrTable) CreateGOTPLTSection(fileStartOffset int, size int) {
	s.sectionHeaders[s.GetSectionIdx(GOT_PLT)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(GOT_PLT),  
		Stype: S_PROGBITS,
		Sflags: S_ALLOC | S_WRITE,
		Saddr: 0,
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: 0,
		Sinfo: 0,
		Saddralign: 8,
		Sentsize: 8,
	}
}

func (s *SectionHdrTable) CreateRelaDynSectionHeader(fileStartOffset int, size int) {
	s.sectionHeaders[s.GetSectionIdx(RELA_DYN)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(RELA_DYN),
		Stype: S_RELA,
		Sflags: S_ALLOC,
		Saddr: 0,
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: uint32(s.GetSectionIdx(DYNSYM)),
		Sinfo: 0,
		Saddralign: 8,
		Sentsize: RELA_ENTRY_SIZE,
	}
}

func (s *SectionHdrTable) CreateRelaPltSectionHeader(fileStartOffset int, size int) {
	s.sectionHeaders[s.GetSectionIdx(RELA_PLT)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(RELA_PLT),
		Stype: S_RELA,
		Sflags: S_ALLOC | S_INFO,
		Saddr: 0,
		Soffset: uint64(fileStartOffset),
		Ssize: uint64(size),
		Slink: uint32(s.GetSectionIdx(DYNSYM)),
		Sinfo: uint32(s.GetSectionIdx(GOT_PLT)),
		Saddralign: 8,
		Sentsize: RELA_ENTRY_SIZE,
	}
}

func (s *SectionHdrTable) CreateInterpSectionHeader(fileStartOffset int, size int) {
	s.sectionHeaders[s.GetSectionIdx(INTERP)] = SectionHeader{
		Sname: s.sectionStrtab.GetIdx(INTERP),
		Stype: S_PROGBITS,
		Sflags: S_ALLOC,
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
