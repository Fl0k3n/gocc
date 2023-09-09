package elf

type SectionType uint32

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

type SectionFlag uint64

const (
	S_WRITE SectionFlag = 0x1
	S_ALLOC = 0x2
	S_EXEC = 0x4
)

type SymbolBinding uint8 

const (
	SB_LOCAL SymbolBinding = 0
	SB_GLOBAL = 1
	SB_WEAK = 2
)

type SymbolType uint8 

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

const SYMBOL_SIZE = 24

type Symbol struct {
	Sname uint32
	Sinfo uint8
	Sother uint8 
	Sshndx uint16 
	Svalue uint64 
	Ssize uint64
}

func (s *Symbol) ToBytes() []byte {
	res := make([]byte, SYMBOL_SIZE)
	encodeUnsignedIntsToLittleEndianU2(res, 0, s.Sname, s.Sinfo, s.Sother, s.Sshndx, s.Svalue, s.Ssize)
	return res
}
