package elf

import (
	"asm"
	"fmt"
	"irs"
	"utils"
)

type ELFBuilder struct {
	stringTable []string
	stringTableSize uint32
	code []uint8
	assembledFunctions []*asm.AssembledFunction
	globals []*irs.GlobalSymbol
	symbols []*Symbol
	symbolNameToIdx map[string]int
	sectionIds map[string]int
	definedFunctions *utils.Set[string]
}

func NewBuilder(code []uint8, assembledFunctions []*asm.AssembledFunction, globals []*irs.GlobalSymbol) *ELFBuilder {
	definedFunctions := utils.NewSet[string]()
	for _, fun := range assembledFunctions {
		definedFunctions.Add(fun.FunctionSymbol.Symbol.Name)
	}
	return &ELFBuilder{
		stringTable: []string{},
		stringTableSize: 1, // NULL at the start
		code: code, 
		assembledFunctions: assembledFunctions,
		globals: globals,
		symbols: []*Symbol{},
		symbolNameToIdx: map[string]int{},
		sectionIds: map[string]int{},
		definedFunctions: definedFunctions,
	}
}

func (e *ELFBuilder) classifySymbolToSection(global *irs.GlobalSymbol) string {
	if global.IsExtern || (global.IsFunction && !e.definedFunctions.Has(global.Symbol.Name)) {
		return UNDEFINED
	}
	if global.IsFunction {
		return TEXT	
	}
	if global.Initializers == nil {
		return BSS
	} else {
		return DATA
	}
} 

func (e *ELFBuilder) classifyBinding(global *irs.GlobalSymbol) SymbolBinding {
	if global.IsStatic || (global.IsFunction && e.definedFunctions.Has(global.Symbol.Name)) {
		return SB_LOCAL
	}
	// TODO add suport for weak symbols
	return SB_GLOBAL
}

func (e *ELFBuilder) classifyType(global *irs.GlobalSymbol) SymbolType {
	if global.IsFunction {
		return ST_FUNC
	}
	return ST_OBJECT
}

func (e *ELFBuilder) getSectionsToCreate() []string {
	sectionCounter := 1 // 0 is NULL section
	res := []string{TEXT}
	e.sectionIds[TEXT] = sectionCounter
	hasBss := false
	hasData := false
	for _, global := range e.globals {
		if global.IsFunction {
			continue
		}
		if global.Initializers == nil {
			hasBss = true
		} else {
			hasData = true	
		}
	}
	if hasData {
		sectionCounter++
		e.sectionIds[DATA] = sectionCounter
		res = append(res, DATA)
	}
	if hasBss {
		sectionCounter++
		e.sectionIds[BSS] = sectionCounter
		res = append(res, BSS)
	}
	// TODO check if reloc should be present
	return res
}

func (e *ELFBuilder) putStringInStrtab(name string) (idx uint32) {
	e.stringTableSize += uint32(len(name)) + 1
	idx = e.stringTableSize
	e.stringTable = append(e.stringTable, name)
	return
}

func (e *ELFBuilder) createSymbols(sectionsToCreate []string) {
	NULL_SYMBOL := &Symbol{
		Sname: 0,
		Sinfo: 0,
		Sother: 0, 
		Sshndx: 0, 
		Svalue: 0, 
		Ssize: 0,
	}
	e.symbols = append(e.symbols, NULL_SYMBOL)
	// TODO add file

	for _, sectionName := range sectionsToCreate {
		sectionNum := e.putStringInStrtab(sectionName)
		e.symbols = append(e.symbols, &Symbol{
			Sname: sectionNum,
			Sinfo: encodeSymbolInfo(SB_LOCAL, ST_SECTION),
			Sother: RESERVED_SYMBOL_OTHER_FIELD,
			Sshndx: 0, // TODO set it to the section index
			Svalue: 0,
			Ssize: 0,
		})
	}

	for _, fun := range e.assembledFunctions {
		e.symbols = append(e.symbols, &Symbol{
			Sname: e.putStringInStrtab(fun.FunctionSymbol.Symbol.Name),
			Sinfo: encodeSymbolInfo(SB_LOCAL, ST_FUNC),
			Sother: RESERVED_SYMBOL_OTHER_FIELD,
			Sshndx: uint16(e.sectionIds[TEXT]),
			Svalue: uint64(fun.Offset),
			Ssize: uint64(fun.Size),
		})
	}

	for _, global := range e.globals {
		if global.IsFunction && e.definedFunctions.Has(global.Symbol.Name) {
			continue
		}
		e.symbols = append(e.symbols, &Symbol{
			Sname: e.putStringInStrtab(global.Symbol.Name),
			Sinfo: encodeSymbolInfo(e.classifyBinding(global), e.classifyType(global)),
			Sother: RESERVED_SYMBOL_OTHER_FIELD,
			Sshndx: uint16(e.sectionIds[e.classifySymbolToSection(global)]),
			Svalue: 0,
			Ssize: 0,
		})
	}
}

func (e *ELFBuilder) createStrtabSection() {
	header := SectionHeader{
		Sname: e.putStringInStrtab(STRTAB),
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
		Sname: e.putStringInStrtab(TEXT),
		Stype: uint32(PROGBITS),
		Sflags: uint64(S_ALLOC) | uint64(S_EXEC), // TODO
		Saddr: 0, // TODO where is data
	}
	fmt.Println(header)
	// paste code, compute size etc
}

func (e *ELFBuilder) createDataSection() {
	header := SectionHeader{
		Sname: e.putStringInStrtab(DATA),  
		Stype: uint32(PROGBITS),
		Sflags: uint64(S_ALLOC) | uint64(S_WRITE), // TODO
		Saddr: 0, // TODO where is data
	}
	fmt.Println(header)
	// iterate over initialized globals, compute initializers, handle alignments, and build memory image
}

func (e *ELFBuilder) createBssSection() {
	header := SectionHeader{
		Sname: e.putStringInStrtab(BSS),  
		Stype: uint32(NOBITS),
		Sflags: 1, // TODO
		Saddr: 0, // Bss is not stored
	}
	fmt.Println(header)
	// iterate over globals, see which are uninitialized, 
}

func (e *ELFBuilder) CreateRelocatableELF(resultPath string) {
	// we also need relocation info from compiler
	// for static variables add entries to rela.text for every usage, compile with 0'ed addresses but note where address should be insterted
	// for global functions add entries to rela.text for every usage, when building shared objects we will also need to store them in dynsym section, these should be addressed via plt
	// for global variables also use rela.text for every usage, similarly as above with .so, they should be addressed via got
	sectionsToCreate := e.getSectionsToCreate()
	e.createSymbols(sectionsToCreate)	
}
