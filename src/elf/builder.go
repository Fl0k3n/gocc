package elf

import (
	"asm"
	"codegen"
	"irs"
	"utils"
)

type ELFBuilder struct {
	strtab *Strtab
	code []uint8
	assembledFunctions []*asm.AssembledFunction
	globals []*irs.GlobalSymbol
	symtab *Symtab
	definedFunctions *utils.Set[string]
	sectionVirtualSize map[string]int
	sectionAlignment map[string]int
	sectionHdrTable *SectionHdrTable
	globalDataOffsets map[string]int
	displacementsToFix []asm.DisplacementToFix
	symbolNameToIdx map[string]int
	initializedGlobals *utils.Set[string]
}

func NewBuilder(
	code []uint8,
	assembledFunctions []*asm.AssembledFunction,
	globals []*irs.GlobalSymbol,
	displacementsToFix []asm.DisplacementToFix,
) *ELFBuilder {
	definedFunctions := utils.NewSet[string]()
	for _, fun := range assembledFunctions {
		definedFunctions.Add(fun.FunctionSymbol.Symbol.Name)
	}
	return &ELFBuilder{
		strtab: newStrtab(),
		code: code, 
		assembledFunctions: assembledFunctions,
		globals: globals,
		symtab: NewSymtab(),
		definedFunctions: definedFunctions,
		sectionVirtualSize: map[string]int{},
		sectionAlignment: nil,
		sectionHdrTable: newSectionHdrTable(),
		globalDataOffsets: map[string]int{},
		displacementsToFix: displacementsToFix,
		symbolNameToIdx: map[string]int{},
		initializedGlobals: utils.NewSet[string](),
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
	if global.IsExtern {
		return ST_NOTYPE
	}
	if global.IsFunction {
		return ST_FUNC
	}
	return ST_OBJECT
}

func (e *ELFBuilder) checkWhatSectionsAreNeeded() (data bool, bss bool, relaText bool) {
	data = false
	bss = false
	relaText = len(e.displacementsToFix) > 0
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

func (e *ELFBuilder) createHeader(sectionsContentSize int) *Header {
	return &Header{
		Eident: [16]uint8{
			0x7F, uint8('E'), uint8('L'), uint8('F'),
			ELF_CLASS_64, ELF_DATA_LITTLE_ENDIAN_U2, ELF_CURRENT_VERSION, ELF_SYSTEM_V_ABI,
			0, 0, 0, 0,
			0, 0, 0, 0,
		},
		Etype: RELOCATABLE_FILE,
		Emachine: E_X86_64_MACHINE,
		Eversion: ELF_CURRENT_VERSION,
		Eentry: 0, // no entry for relocatable file
		Ephoff: 0, // no program headers
		Eshoff: uint64(sectionsContentSize + ELF_HEADER_SIZE),
		Eflags: 0, // unused
		Eehsize: ELF_HEADER_SIZE, 
		Ephentsize: 0, // no program headers 
		Ephnum: 0, // no program headers
		Eshentsize: SECTION_HEADER_SIZE,
		Eshnum: uint16(len(e.sectionHdrTable.GetSectionHeaders())), 
		Eshstrndx: e.sectionHdrTable.GetSectionIdx(SECTION_STRTAB),
	}
}

func (e *ELFBuilder) prepareSections() (sectionsThatNeedSymbol []string) {
	sectionsThatNeedSymbol = append(sectionsThatNeedSymbol, TEXT)
	needsData, needsBss, needsRelaText := e.checkWhatSectionsAreNeeded()
	var symtabSectionIdx uint16 = 2
	relaTextIdx := symtabSectionIdx
	if needsRelaText {
		symtabSectionIdx++
	} 
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
	if needsRelaText {
		sectionIdxs[RELA_TEXT] = relaTextIdx
		e.sectionAlignment[RELA_TEXT] = 8
		sectionsThatNeedSymbol = append(sectionsThatNeedSymbol, RELA_TEXT)
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
	if section == DATA || section == BSS {
		if section == DATA {
			e.initializedGlobals.Add(global.Symbol.Name)
		}
		offset := e.allocSymbolSpaceInSection(global, section)
		e.globalDataOffsets[global.Symbol.Name] = offset
		return offset
	} else {
		return 0
	}
}

func (e *ELFBuilder) addSymbolToSymtab(symbol *Symbol, name string) {
	e.symbolNameToIdx[name] = e.symtab.AddSymbol(symbol)
}

func (e *ELFBuilder) createSymbols(sectionsRequiringSymbol []string, sourceFileName string) {
	NULL_SYMBOL := &Symbol{
		Sname: NULL_SYMBOL_STR_ID,
		Sinfo: encodeSymbolInfo(SB_LOCAL, ST_NOTYPE),
		Sother: 0, 
		Sshndx: SHN_UNDEF, 
		Svalue: 0, 
		Ssize: 0,
	}
	e.addSymbolToSymtab(NULL_SYMBOL, NULL_SYMBOL_STR)
	FILE_SYMBOL := &Symbol{
		Sname: e.strtab.PutString(sourceFileName),
		Sinfo: encodeSymbolInfo(SB_LOCAL, ST_FILE),
		Sother: 0, 
		Sshndx: SHN_ABS, 
		Svalue: 0, 
		Ssize: 0,
	}
	e.addSymbolToSymtab(FILE_SYMBOL, sourceFileName)
	for _, sectionName := range sectionsRequiringSymbol {
		e.addSymbolToSymtab(&Symbol{
			Sname: e.strtab.PutString(sectionName),
			Sinfo: encodeSymbolInfo(SB_LOCAL, ST_SECTION),
			Sother: RESERVED_SYMBOL_OTHER_FIELD,
			Sshndx: e.sectionHdrTable.GetSectionIdx(sectionName),
			Svalue: 0,
			Ssize: 0,
		}, sectionName)
	}

	textSectionidx := e.sectionHdrTable.GetSectionIdx(TEXT)
	for _, fun := range e.assembledFunctions {
		var binding SymbolBinding = SB_GLOBAL
		if fun.FunctionSymbol.IsStatic {
			binding = SB_LOCAL
		}
		e.addSymbolToSymtab(&Symbol{
			Sname: e.strtab.PutString(fun.FunctionSymbol.Symbol.Name),
			Sinfo: encodeSymbolInfo(binding, ST_FUNC),
			Sother: RESERVED_SYMBOL_OTHER_FIELD,
			Sshndx: textSectionidx,
			Svalue: uint64(fun.Offset),
			Ssize: uint64(fun.Size),
		}, fun.FunctionSymbol.Symbol.Name)
	}

	for _, global := range e.globals {
		if global.IsFunction && e.definedFunctions.Has(global.Symbol.Name) {
			continue
		}
		section := e.getSymbolSection(global)
		e.addSymbolToSymtab(&Symbol{
			Sname: e.strtab.PutString(global.Symbol.Name),
			Sinfo: encodeSymbolInfo(e.classifyBinding(global), e.classifyType(global)),
			Sother: RESERVED_SYMBOL_OTHER_FIELD,
			Sshndx: e.sectionHdrTable.GetSectionIdx(section),
			Svalue: uint64(e.assignSymbolToSection(global, section)),
			Ssize: uint64(e.getSymbolSize(global, section)),
		}, global.Symbol.Name)
	}
	e.sectionVirtualSize[SYMTAB] = e.symtab.Size() * SYMBOL_SIZE
}

func (e *ELFBuilder) getDataSectionMemoryImage() ([]byte) {
	data := make([]byte, e.sectionVirtualSize[DATA])
	if len(data) == 0 {
		return data
	}
	for _, global := range e.globals {
		if e.getSymbolSection(global) == DATA {
			offset := e.globalDataOffsets[global.Symbol.Name]
			data[offset] = 0x12 // TODO
		}
	}
	return data
}

func (e *ELFBuilder) getRelocationTypeAndSymbolIdx(accessor codegen.MemoryAccessor) (relocType RelocationType, symbolIdx int, offset int) {
	switch acc := accessor.(type) {
	case codegen.GOTMemoryAccessor:
		return R_X86_64_REX_GOTPCRELX, e.symbolNameToIdx[acc.Symbol.Name], 0
	case codegen.SectionMemoryAccessor:
		if e.initializedGlobals.Has(acc.Symbol.Name) {
			symbolIdx = e.symbolNameToIdx[DATA]
		} else {
			symbolIdx = e.symbolNameToIdx[BSS]
		}
		return R_X86_64_PC32, symbolIdx, e.globalDataOffsets[acc.Symbol.Name]
	case codegen.PLTMemoryAccessor:
		return R_X86_64_PLT32, e.symbolNameToIdx[acc.Symbol.Name], 0
	default: // assembly-wise we could also tolerate labels here
		panic("Unexpected memory accessor to relocate")
	}
}

func (e *ELFBuilder) createRelocationEntries() []RelaEntry {
	res := make([]RelaEntry, len(e.displacementsToFix))
	for i, displacement := range e.displacementsToFix {
		relocType, symbolSymtabIdx, symbolOffset := e.getRelocationTypeAndSymbolIdx(displacement.MemoryAccessor)
		res[i] = RelaEntry{
			Roffset: uint64(displacement.CodeOffset),
			Rinfo: encodeRelocationInfo(uint32(symbolSymtabIdx), relocType),
			Raddend: int64(symbolOffset) - (int64(displacement.SizeToFix) + int64(displacement.InstructionSizeAfterDisplacement)),
		}
	}
	e.sectionVirtualSize[RELA_TEXT] = len(res) * RELA_ENTRY_SIZE
	return res
}

func (e *ELFBuilder) createSectionHeaders() {
	fileOffset := ELF_HEADER_SIZE
	e.sectionHdrTable.CreateNullSection()
	e.sectionHdrTable.CreateTextSectionHeader(fileOffset, len(e.code))
	fileOffset += len(e.code)
	if e.sectionHdrTable.HasSection(RELA_TEXT) {
		e.sectionHdrTable.CreateRelaTextSectionHeader(fileOffset, e.sectionVirtualSize[RELA_TEXT], e.sectionAlignment[RELA_TEXT])
		fileOffset += e.sectionVirtualSize[RELA_TEXT]
	}
	if e.sectionHdrTable.HasSection(DATA) {
		e.sectionHdrTable.CreateDataSection(fileOffset, e.sectionVirtualSize[DATA], e.sectionAlignment[DATA])
		fileOffset += e.sectionVirtualSize[DATA]
	}
	if e.sectionHdrTable.HasSection(BSS) {
		e.sectionHdrTable.CreateBssSection(fileOffset, e.sectionVirtualSize[BSS], e.sectionAlignment[BSS])
	}
	e.sectionHdrTable.CreateSymtabSection(fileOffset, e.sectionVirtualSize[SYMTAB], e.symtab.getGreatestLocalSymbolId())
	fileOffset += e.sectionVirtualSize[SYMTAB]
	e.sectionHdrTable.CreateStrtabSection(fileOffset, e.strtab.GetSize())
	fileOffset += e.strtab.GetSize()
	e.sectionHdrTable.CreateSectionStrtabSection(fileOffset, e.sectionHdrTable.GetSectionStrtab().GetSize())
}

func (e *ELFBuilder) CreateRelocatableELF(sourceFileName string, resultPath string) error {
	sectionsThatNeedSymbol := e.prepareSections()
	e.createSymbols(sectionsThatNeedSymbol, sourceFileName)
	data := e.getDataSectionMemoryImage()
	relaEntries := e.createRelocationEntries()
	e.createSectionHeaders()
	sectionsContentSize := len(e.code) + len(relaEntries) * RELA_ENTRY_SIZE + len(data) +
						   e.symtab.BinarySize() + e.strtab.GetSize() + e.sectionHdrTable.GetSectionStrtab().GetSize() 
	elf := &ElfFile{
		Header: e.createHeader(sectionsContentSize),
		SectionHdrTable: e.sectionHdrTable,
		Code: e.code,
		Data: data,
		Symtab: e.symtab,
		Strtab: e.strtab,
		SectionStrtab: e.sectionHdrTable.GetSectionStrtab(),
		RelaEntries: relaEntries,
	}
	return NewSerializer().Serialize(elf, resultPath)
}

