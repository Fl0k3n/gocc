package elf

type SectionType = uint32

const ELF_CLASS_64 = 2
const ELF_DATA_LITTLE_ENDIAN_U2 = 1
const ELF_CURRENT_VERSION = 1
const ELF_SYSTEM_V_ABI = 0

const RELOCATABLE_FILE = 1
const EXECUTABLE_FILE = 2
const SHARED_OBJECT_FILE = 3

const E_X86_64_MACHINE = 0x3E

const GOT_ENTRY_SIZE = 8

const (
	S_NULL SectionType = iota
	S_PROGBITS
	S_SYMTAB
	S_STRRAB 
	S_RELA
	S_HASH 
	S_DYNAMIC 
	S_NOTE 
	S_NOBITS 
	S_REL 
	S_SHLIB
	S_DYNSYM // and some other not needed
)

type SegmentType = uint32

const (
	PT_NULL SegmentType = iota
	PT_LOAD
	PT_DYNAMIC
	PT_INTERP
	PT_NOTE
	PT_SHLIB
	PT_PHDR
)

type SegmentFlag = uint32

const (
	PF_X SegmentFlag = 0x1
	PF_W = 0x2
	PF_R = 0x4
)

type SectionFlag = uint64

const (
	S_WRITE SectionFlag = 0x1
	S_ALLOC = 0x2
	S_EXEC = 0x4
	S_INFO = 0x40
)

type SymbolBinding = uint8 

const (
	SB_LOCAL SymbolBinding = 0
	SB_GLOBAL = 1
	SB_WEAK = 2
)

type SymbolType = uint8 

const (
	ST_NOTYPE SymbolType = 0
	ST_OBJECT = 1
	ST_FUNC = 2
	ST_SECTION = 3
	ST_FILE = 4
)

const RESERVED_SYMBOL_OTHER_FIELD = 0

// names based on official ELF spec (changed due to Go privacy model), types simplified to limit typecasting
// only ELF_64 is supported, and only to the degree neccessary

const ELF_HEADER_SIZE = 64

type Header struct {
	Eident [16]uint8
	Etype uint16
	Emachine uint16
	Eversion uint32
	Eentry uint64
	Ephoff uint64
	Eshoff uint64
	Eflags uint32
	Eehsize uint16
	Ephentsize uint16
	Ephnum uint16
	Eshentsize uint16
	Eshnum uint16
	Eshstrndx uint16
}

func (h *Header) ToBytes() []byte {
	res := make([]byte, ELF_HEADER_SIZE)
	for i := 0; i < 16; i++ {
		res[i] = h.Eident[i]
	}
	encodeUnsignedIntsToLittleEndianU2(res, 16, h.Etype, h.Emachine, h.Eversion, h.Eentry, 
		h.Ephoff, h.Eshoff, h.Eflags, h.Eehsize, h.Ephentsize, h.Ephnum, h.Eshentsize, h.Eshnum, h.Eshstrndx)
	return res
}

func HeaderFromBytes(bytes []byte, offset int) *Header {
	h := Header{}
	for i := 0; i < 16; i++ {
		h.Eident[i] = bytes[offset + i]
	}
	decodeUnsignedIntsFromLittleEndianU2(bytes, offset + 16, &h.Etype, &h.Emachine, &h.Eversion, &h.Eentry, 
		&h.Ephoff, &h.Eshoff, &h.Eflags, &h.Eehsize, &h.Ephentsize, &h.Ephnum, &h.Eshentsize, &h.Eshnum, &h.Eshstrndx)
	return &h
}

const SECTION_HEADER_SIZE = 64

type SectionHeader struct {
	Sname uint32
	Stype uint32
	Sflags uint64
	Saddr uint64       
	Soffset uint64
	Ssize uint64
	Slink uint32
	Sinfo uint32
	Saddralign uint64
	Sentsize uint64
}

func (h *SectionHeader) ToBytes() []byte {
	res := make([]byte, SECTION_HEADER_SIZE)
	encodeUnsignedIntsToLittleEndianU2(res, 0, h.Sname, h.Stype, h.Sflags, h.Saddr, h.Soffset,
		h.Ssize, h.Slink, h.Sinfo, h.Saddralign, h.Sentsize)
	return res
}

func SectionHeaderFromBytes(bytes []byte, offset int) *SectionHeader {
	h := SectionHeader{}
	decodeUnsignedIntsFromLittleEndianU2(bytes, offset, &h.Sname, &h.Stype, &h.Sflags, &h.Saddr, &h.Soffset,
		&h.Ssize, &h.Slink, &h.Sinfo, &h.Saddralign, &h.Sentsize)
	return &h
}

const SYMBOL_SIZE = 24

type Symbol struct {
	Sname uint32
	Sinfo uint8
	Sother uint8 
	Sshndx uint16 
	Svalue uint64 
	Ssize uint64
}

func (s *Symbol) Binding() SymbolBinding {
	return GetSymbolBinding(s.Sinfo)
}

func (s *Symbol) Type() SymbolType {
	return GetSymbolType(s.Sinfo)
}

func (s *Symbol) ToBytes() []byte {
	res := make([]byte, SYMBOL_SIZE)
	encodeUnsignedIntsToLittleEndianU2(res, 0, s.Sname, s.Sinfo, s.Sother, s.Sshndx, s.Svalue, s.Ssize)
	return res
}

func SymbolFromBytes(bytes []byte, offset int) *Symbol {
	s := Symbol{}
	decodeUnsignedIntsFromLittleEndianU2(bytes, offset, &s.Sname, &s.Sinfo, &s.Sother, &s.Sshndx, &s.Svalue, &s.Ssize)
	return &s
}

const RELA_ENTRY_SIZE = 24

type RelocationType uint32

const (
	R_X86_64_PC32 RelocationType = 2
	R_X86_64_PLT32 = 4
	R_X86_64_REX_GOTPCRELX = 42
)

type RelaEntry struct {
	Roffset uint64
	Rinfo uint64
	Raddend int64
}

func (r *RelaEntry) RelocationType() RelocationType {
	return GetRelocationType(r.Rinfo)
}

func (r *RelaEntry) SymbolIdx() uint32 {
	return GetSymbolIdx(r.Rinfo)
}

func (r *RelaEntry) ToBytes() []byte {
	res := make([]byte, RELA_ENTRY_SIZE)
	encodeUnsignedIntsToLittleEndianU2(res, 0, r.Roffset, r.Rinfo)
	offset := 16
	addend := encodeIntToLittleEndianU2(r.Raddend)	
	for i := 0; i < 8; i++ {
		res[offset + i] = addend[i]
	}
	return res
}

func RelaEntryFromBytes(bytes []byte, offset int) *RelaEntry {
	r := RelaEntry{}
	decodeUnsignedIntsFromLittleEndianU2(bytes, offset, &r.Roffset, &r.Rinfo)
	decodeIntFromLittleEndianU2(bytes, offset + 16, &r.Raddend)
	return &r
}

const PROGRAM_HEADER_SIZE = 56

type ProgramHeader struct {
	Ptype uint32
	Pflags uint32
	Poffset uint64
	Pvaddr uint64
	Ppaddr uint64
	Pfilesz uint64
	Pmemsz uint64
	Palign uint64
}

func (p *ProgramHeader) ToBytes() []byte {
	res := make([]byte, PROGRAM_HEADER_SIZE)
	encodeUnsignedIntsToLittleEndianU2(res, 0, p.Ptype, p.Pflags, p.Poffset, p.Pvaddr,
		p.Ppaddr, p.Pfilesz, p.Pmemsz, p.Palign)
	return res
}

func ProgramHeaderFromBytes(bytes []byte, offset int) *ProgramHeader {
	p := ProgramHeader{}
	decodeUnsignedIntsFromLittleEndianU2(bytes, offset, &p.Ptype, &p.Pflags, &p.Poffset,
		&p.Pvaddr, &p.Ppaddr, &p.Pfilesz, &p.Pmemsz, &p.Palign)
	return &p
}

type ElfFile struct {
	Header *Header
	ProgramHdrTable *ProgramHdrTable
	SectionHdrTable *SectionHdrTable
	Code []uint8 // TODO consider not storing entire code and data in memory, use supplier functions configurable by ElfFile creator that return them in chunks
	Data []uint8
	Symtab *Symtab
	Strtab *Strtab
	SectionStrtab *Strtab
	RelaEntries []RelaEntry
	GOT []uint64
}
