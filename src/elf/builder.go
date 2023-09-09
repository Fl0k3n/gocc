package elf

import (
	"asm"
	"irs"
	"utils"
)

type ELFBuilder struct {
	strtab *Strtab
	code []uint8
	assembledFunctions []*asm.AssembledFunction
	globals []*irs.GlobalSymbol
	symbols []*Symbol
	definedFunctions *utils.Set[string]
	sectionVirtualSize map[string]int
	sectionAlignment map[string]int
	sectionHdrTable *SectionHdrTable
	initializedGlobalOffsets map[string]int
}

func NewBuilder(code []uint8, assembledFunctions []*asm.AssembledFunction, globals []*irs.GlobalSymbol) *ELFBuilder {
	definedFunctions := utils.NewSet[string]()
	for _, fun := range assembledFunctions {
		definedFunctions.Add(fun.FunctionSymbol.Symbol.Name)
	}
	return &ELFBuilder{
		strtab: newStrtab(),
		code: code, 
		assembledFunctions: assembledFunctions,
		globals: globals,
		symbols: []*Symbol{},
		definedFunctions: definedFunctions,
		sectionVirtualSize: map[string]int{},
		sectionAlignment: nil,
		sectionHdrTable: newSectionHdrTable(),
		initializedGlobalOffsets: map[string]int{},
	}
}

func (e *ELFBuilder) getSymbolSection(global *irs.GlobalSymbol) string {
	if global.IsExtern || (global.IsFunction && !e.definedFunctions.Has(global.Symbol.Name)) {
		return NULL_SECTION
	}
	if global.IsFunction {
		return TEXT	
	}
	if global.Initializers == nil || len(global.Initializers) == 0 {
		return BSS
	} 
	return DATA
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

func (e *ELFBuilder) checkWhatSectionsAreNeeded() (data bool, bss bool) {
	data = false
	bss = false
	for _, global := range e.globals {
		if global.IsFunction {
			continue
		}
		if global.Initializers == nil || len(global.Initializers) == 0 {
			bss = true
		} else {
			data = true	
		}
	}
	return
}

func (e *ELFBuilder) prepareSections() (sectionsThatNeedSymbol []string) {
	sectionsThatNeedSymbol = append(sectionsThatNeedSymbol, TEXT)
	needsData, needsBss := e.checkWhatSectionsAreNeeded()
	var symtabSectionIdx uint16 = 2
	dataSectionIdx := symtabSectionIdx
	if needsData {
		symtabSectionIdx++
	}
	bssSectionIdx := symtabSectionIdx
	if needsBss {
		symtabSectionIdx ++
	}
	sectionIdxs := map[string]uint16{
		NULL_SECTION: NULL_SECTION_IDX,
		TEXT: 1,
		SYMTAB: symtabSectionIdx,
		STRTAB: symtabSectionIdx + 1,
		SECTION_STRTAB: symtabSectionIdx + 2,
	}
	e.sectionAlignment = map[string]int{
		NULL_SECTION: UNKNOWN_SECTION_ALIGNEMNT,
		TEXT: 1,
		SYMTAB: 1,
		STRTAB: 1,
		SECTION_STRTAB: 1,
	}
	if needsData {
		sectionIdxs[DATA] = dataSectionIdx
		e.sectionAlignment[DATA] = UNKNOWN_SECTION_ALIGNEMNT
		sectionsThatNeedSymbol = append(sectionsThatNeedSymbol, DATA)
	}
	if needsBss {
		sectionIdxs[BSS] = bssSectionIdx
		e.sectionAlignment[BSS] = UNKNOWN_SECTION_ALIGNEMNT
		sectionsThatNeedSymbol = append(sectionsThatNeedSymbol, BSS)
	}
	for sectionName := range sectionIdxs {
		e.sectionVirtualSize[sectionName] = 0
	}
	e.sectionHdrTable.AllocSectionHeaders(sectionIdxs)
	return
}

// assumes that global is not a defined function
func (e *ELFBuilder) getSymbolSize(global *irs.GlobalSymbol, section string) int {
	if section == NULL_SECTION {
		return 0
	}
	return global.Symbol.Ctype.Size()
}

func (e *ELFBuilder) allocSymbolSpaceInSection(global *irs.GlobalSymbol, section string) (offset int) {
	if alignment := e.sectionAlignment[section]; alignment != UNKNOWN_SECTION_ALIGNEMNT {
		e.sectionAlignment[section] = global.Symbol.Ctype.RequiredAlignment()
	}
	offset = e.sectionVirtualSize[section]
	if remainder := offset % global.Symbol.Ctype.RequiredAlignment(); remainder != 0 {
		offset += global.Symbol.Ctype.RequiredAlignment() - remainder
	}

	e.sectionVirtualSize[section] = offset + global.Symbol.Ctype.Size()
	return offset
}

func (e *ELFBuilder) assignSymbolToSection(global *irs.GlobalSymbol, section string) int {
	if section == NULL_SECTION {
		return 0
	}
	if section == DATA {
		offset := e.allocSymbolSpaceInSection(global, section)
		e.initializedGlobalOffsets[global.Symbol.Name] = offset
		return offset
	} else if section == BSS {
		return e.allocSymbolSpaceInSection(global, section)
	} else {
		return 0
	}
}

func (e *ELFBuilder) createSymbols(sectionsRequiringSymbol []string, sourceFileName string) {
	NULL_SYMBOL := &Symbol{
		Sname: NULL_SYMBOL_STR_ID,
		Sinfo: encodeSymbolInfo(SB_LOCAL, ST_NOTYPE),
		Sother: 0, 
		Sshndx: 0, 
		Svalue: 0, 
		Ssize: 0,
	}
	e.symbols = append(e.symbols, NULL_SYMBOL)
	FILE_SYMBOL := &Symbol{
		Sname: e.strtab.PutString(sourceFileName),
		Sinfo: encodeSymbolInfo(SB_LOCAL, ST_FILE),
		Sother: 0, 
		Sshndx: 0, 
		Svalue: 0, 
		Ssize: 0,
	}
	e.symbols = append(e.symbols, FILE_SYMBOL)
	for _, sectionName := range sectionsRequiringSymbol {
		sectionStrId := e.strtab.PutString(sectionName)
		e.symbols = append(e.symbols, &Symbol{
			Sname: sectionStrId,
			Sinfo: encodeSymbolInfo(SB_LOCAL, ST_SECTION),
			Sother: RESERVED_SYMBOL_OTHER_FIELD,
			Sshndx: e.sectionHdrTable.GetSectionIdx(sectionName),
			Svalue: 0,
			Ssize: 0,
		})
	}

	textSectionidx := e.sectionHdrTable.GetSectionIdx(TEXT)
	for _, fun := range e.assembledFunctions {
		e.symbols = append(e.symbols, &Symbol{
			Sname: e.strtab.PutString(fun.FunctionSymbol.Symbol.Name),
			Sinfo: encodeSymbolInfo(SB_LOCAL, ST_FUNC),
			Sother: RESERVED_SYMBOL_OTHER_FIELD,
			Sshndx: textSectionidx,
			Svalue: uint64(fun.Offset),
			Ssize: uint64(fun.Size),
		})
	}

	for _, global := range e.globals {
		if global.IsFunction && e.definedFunctions.Has(global.Symbol.Name) {
			continue
		}
		section := e.getSymbolSection(global)
		e.symbols = append(e.symbols, &Symbol{
			Sname: e.strtab.PutString(global.Symbol.Name),
			Sinfo: encodeSymbolInfo(e.classifyBinding(global), e.classifyType(global)),
			Sother: RESERVED_SYMBOL_OTHER_FIELD,
			Sshndx: e.sectionHdrTable.GetSectionIdx(section),
			Svalue: uint64(e.assignSymbolToSection(global, section)),
			Ssize: uint64(e.getSymbolSize(global, section)),
		})
	}
	e.sectionVirtualSize[SYMTAB] = len(e.symbols) * SYMBOL_SIZE
}

func (e *ELFBuilder) getDataSectionMemoryImage() ([]byte) {
	data := make([]byte, e.sectionVirtualSize[DATA])
	if len(data) == 0 {
		return data
	}
	for _, global := range e.globals {
		if e.getSymbolSection(global) == DATA {
			offset := e.initializedGlobalOffsets[global.Symbol.Name]
			data[offset] = 0x12 // TODO
		}
	}
	return data
}

func (e *ELFBuilder) getGreatestLocalSymbolId() uint32 {
	var res uint32 = 0
	for i, sym := range e.symbols {
		if getSymbolBinding(sym.Sinfo) == SB_LOCAL {
			res = uint32(i)
		}
	}
	return res
}

func (e *ELFBuilder) createSectionHeaders() {
	fileOffset := ELF_HEADER_SIZE
	e.sectionHdrTable.CreateNullSection()
	e.sectionHdrTable.CreateTextSectionHeader(fileOffset, len(e.code))
	fileOffset += len(e.code)
	if e.sectionHdrTable.HasSection(DATA) {
		e.sectionHdrTable.CreateDataSection(fileOffset, e.sectionVirtualSize[DATA], e.sectionAlignment[DATA])
		fileOffset += e.sectionVirtualSize[DATA]
	}
	if e.sectionHdrTable.HasSection(BSS) {
		e.sectionHdrTable.CreateBssSection(fileOffset, e.sectionVirtualSize[BSS], e.sectionAlignment[BSS])
	}
	e.sectionHdrTable.CreateSymtabSection(fileOffset, e.sectionVirtualSize[SYMTAB], e.getGreatestLocalSymbolId())
	fileOffset += e.sectionVirtualSize[SYMTAB]
	e.sectionHdrTable.CreateStrtabSection(fileOffset, e.strtab.GetSize())
	fileOffset += e.strtab.GetSize()
	e.sectionHdrTable.CreateSectionStrtabSection(fileOffset, e.sectionHdrTable.GetSectionStrtab().GetSize())
}

func (e *ELFBuilder) CreateRelocatableELF(sourceFileName string, resultPath string) error {
	// we also need relocation info from compiler
	// for static variables add entries to rela.text for every usage, compile with 0'ed addresses but note where address should be insterted
	// for global functions add entries to rela.text for every usage, when building shared objects we will also need to store them in dynsym section, these should be addressed via plt
	// for global variables also use rela.text for every usage, similarly as above with .so, they should be addressed via got
	sectionsThatNeedSymbol := e.prepareSections()
	e.createSymbols(sectionsThatNeedSymbol, sourceFileName)
	data := e.getDataSectionMemoryImage()
	e.createSectionHeaders()
	return NewSerializer(resultPath).Serialize(
		e.code,
		data,
		e.sectionHdrTable,
		e.symbols,
		e.strtab,
		e.sectionHdrTable.GetSectionStrtab(),
	)
}
