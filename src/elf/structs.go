package elf

type SectionType uint32

const (
	NULL SectionType = iota
	PROGBITS
	SYMTAB
	STRRAB 
	RELA
	HASH 
	DYNAMIC 
	NOTE 
	NOBITS 
	REL 
	SHLIB
	DYNSYM // and some other not needed
)

type SectionFlag uint64

const (
	S_WRITE SectionFlag = 0x1
	S_ALLOC = 0x2
	S_EXEC = 0x4
)

// names based on official ELF spec (changed due to Go privacy model), types simplified to limit typecasting
// only ELF_64 is supported, and only to the neccessary degree

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

type Symbol struct {
	Sname uint32
	Sinfo uint8
	Sother uint8 
	Sshndx uint16 
	Svalue uint64 
	Ssize uint64
}
