package elf

import (
	"fmt"
)

type ELFBuilder struct {
	stringTable []string
	stringTableSize uint32
}

func NewElfBuilder(code string, globals string) *ELFBuilder {
	return &ELFBuilder{
		stringTable: []string{},
		stringTableSize: 1,
	}
}

func (e *ELFBuilder) putElfSectionNameInStrtab(name string) (idx uint32) {
	e.stringTableSize += uint32(len(name)) + 1
	idx = e.stringTableSize
	e.stringTable = append(e.stringTable, name)
	return
}

func (e *ELFBuilder) createStrtabSection() {
	header := SectionHeader{
		Sname: e.putElfSectionNameInStrtab(STRTAB),
		//...
	}

	fmt.Println(header)
}

func (e *ELFBuilder) mapGlobalsToSections() {

}

func (e *ELFBuilder) createSymtab() {
	// iterate over globals (including function names)
	// map static to LOCAL, other to GLOBAL
	// value of function symbol (if known) is its offset from start of section (which should be text)
	// size of function symbol is just its size, as a binary, size of other globals is just the size e.g. int is 4 (also for ones in bss)
	// externs should have no type, no size, no value (?)
	// symtab should also contain filename, sections

}

func (e *ELFBuilder) createTextSection() {
	header := SectionHeader{
		Sname: e.putElfSectionNameInStrtab(TEXT),
		Stype: uint32(PROGBITS),
		Sflags: uint64(S_ALLOC) | uint64(S_EXEC), // TODO
		Saddr: 0, // TODO where is data
	}
	fmt.Println(header)
	// paste code, compute size etc
}

func (e *ELFBuilder) createDataSection() {
	header := SectionHeader{
		Sname: e.putElfSectionNameInStrtab(DATA),  
		Stype: uint32(PROGBITS),
		Sflags: uint64(S_ALLOC) | uint64(S_WRITE), // TODO
		Saddr: 0, // TODO where is data
	}
	fmt.Println(header)
	// iterate over initialized globals, compute initializers, handle alignments, and build memory image
}

func (e *ELFBuilder) createBssSection() {
	header := SectionHeader{
		Sname: e.putElfSectionNameInStrtab(BSS),  
		Stype: uint32(NOBITS),
		Sflags: 1, // TODO
		Saddr: 0, // Bss is not stored
	}
	fmt.Println(header)
	// iterate over globals, see which are uninitialized, 
}

func (e *ELFBuilder) createRelocatableELF(resultPath string) {
	// we also need relocation info from compiler
	// for static variables add entries to rela.text for every usage, compile with 0'ed addresses but note where address should be insterted
	// for static functions add function label and call label (use addressing relative to RIP), nothing else needed
	// for global functions add entries to rela.text for every usage, when building shared objects we will also need to store them in dynsym section, these should be addressed via plt
	// for global variables also use rela.text for every usage, similarly as above with .so, they should be addressed via got
}
