package linkers

import (
	"asm"
	"codegen"
	"elf"
	"errors"
	"fmt"
	"strings"
	"utils"
)

const DYNAMIC_LINKER_GOT_PLT_ENTRIES = 3
const DYNAMIC_LINKER_PLT_ENTIRES = 1
const UNKNOWN_DYNAMIC_VALUE = 0
const DYNAMIC_SEGMENT_ALIGNMENT = 0x8
const RELA_IDX_SIZE = 4
const RUNTIME_DYNAMIC_LINKER = "/lib64/ld-linux-x86-64.so.2" // TODO .conf or arg
const EXECUTABLE_START_VMA = 0x400000

type AddressesAssignedCallback = func()

type DynamicLinker struct {
	assembler *asm.X86_64Assembler
	helper *LinkageHelper
	targetElf *elf.ElfFile
	addressesAssignedCallbacks []AddressesAssignedCallback
}

func NewDynamicLinker(assembler *asm.X86_64Assembler) *DynamicLinker {
	return &DynamicLinker{
		assembler: assembler,
		helper: newHelper(),
		addressesAssignedCallbacks: make([]func(), 0),	
	}
}

func (l *DynamicLinker) addOnAddressesAssignedCallback(cb AddressesAssignedCallback) {
	l.addressesAssignedCallbacks = append(l.addressesAssignedCallbacks, cb)
}

func (l *DynamicLinker) notifyAddressesAssigned() {
	for _, callback := range l.addressesAssignedCallbacks {
		callback()
	}
	l.addressesAssignedCallbacks = []AddressesAssignedCallback{}
}

func (l *DynamicLinker) getNeededRelaSections(e *elf.ElfFile) (dyn bool, plt bool) {
	dyn = false
	plt = false
	for _, relaTextEntry := range e.RelaTextEntries {
		switch relaTextEntry.RelocationType() {
		case elf.R_X86_64_REX_GOTPCRELX:
			dyn = true
		case elf.R_X86_64_PLT32:
			if !e.Symtab.IsDefined(relaTextEntry.SymbolIdx()) {
				plt = true
			}
		}
	}
	return
}

func (l *DynamicLinker) getNeededGotPltSections(e *elf.ElfFile) (got bool, plt bool) {
	got = false 
	plt = false
	for _, relaTextEntry := range e.RelaTextEntries {
		switch relaTextEntry.RelocationType() {
		case elf.R_X86_64_REX_GOTPCRELX:
			got = true
		case elf.R_X86_64_PLT32:
			if !e.Symtab.IsDefined(relaTextEntry.SymbolIdx()) {
				plt = true
			}
		}
	}
	return
}

func (l *DynamicLinker) getSharedLibPossiblyPresentSectionsInOrder() []string {
	return []string{
		elf.NULL_SECTION,
		elf.HASH_SECTION, elf.DYNSYM, elf.DYNSTR, elf.RELA_DYN, elf.RELA_PLT,
		elf.PLT, elf.TEXT,
		elf.DYNAMIC, elf.GOT, elf.GOT_PLT, elf.DATA, elf.BSS,
		elf.RO_DATA,
		elf.SYMTAB, elf.STRTAB, elf.SECTION_STRTAB,
	}
}

func (l *DynamicLinker) getDynamicExecPossiblyPresentSectionsInOrder() []string {
	return []string{
		elf.NULL_SECTION,
		elf.INTERP,
		elf.HASH_SECTION, elf.DYNSYM, elf.DYNSTR, elf.RELA_DYN, elf.RELA_PLT,
		elf.PLT, elf.TEXT,
		elf.DYNAMIC, elf.GOT, elf.GOT_PLT, elf.DATA, elf.BSS,
		elf.RO_DATA,
		elf.SYMTAB, elf.STRTAB, elf.SECTION_STRTAB,
	}
}

func (l *DynamicLinker) getSectionsInOrder(sectionOrder []string) []string {
	possiblyInitiallyMissingSections := []string {
		elf.DATA, elf.BSS, elf.RO_DATA,
	}
	filteredSections := utils.NewSet[string]()
	needsGot, needsPlt := l.getNeededGotPltSections(l.targetElf)
	needsRelaDyn, needsRelaPlt := l.getNeededRelaSections(l.targetElf)
	if !needsGot {
		filteredSections.Add(elf.GOT)
	}
	if !needsPlt {
		filteredSections.Add(elf.PLT)
		filteredSections.Add(elf.GOT_PLT)
	}
	if !needsRelaDyn {
		filteredSections.Add(elf.RELA_DYN)
	}
	if !needsRelaPlt {
		filteredSections.Add(elf.RELA_PLT)
	}
	for _, sectionName := range possiblyInitiallyMissingSections {
		if !l.targetElf.SectionHdrTable.HasSection(sectionName) {
			filteredSections.Add(sectionName)
		}
	}
	res := []string{}
	for _, sectionName := range sectionOrder {
		if !filteredSections.Has(sectionName) {
			res = append(res, sectionName)
		}
	}
	return res
}

func (l *DynamicLinker) createSectionHeaders(requiredSections []string) {
	sectionIdxMap := map[string]uint16{}
	for i, sectionName := range requiredSections {
		sectionIdxMap[sectionName] = uint16(i)
	}
	missingSections := []string{}
	for _, sectionName := range requiredSections {
		if !l.targetElf.SectionHdrTable.HasSection(sectionName) {
			missingSections = append(missingSections, sectionName)
		}
	}
	reindexMap := l.targetElf.SectionHdrTable.Reindex(sectionIdxMap)	
	for _, sectionName := range missingSections {
		l.targetElf.SectionHdrTable.CreateSectionHeaderWithDefaults(sectionName)
	}
	for _, sym := range l.targetElf.Symtab.GetAll() {
		sym.Sshndx = reindexMap[sym.Sshndx]
	}
}

func (l *DynamicLinker) createDynsymSection() (symIdxToDynsymIdx map[uint32]uint32) {
	dynSymtab := elf.NewSymtab()
	dynStrtab := elf.NewStrtab()
	symIdxToDynsymIdx = map[uint32]uint32{}

	dynSymtab.AddSymbol(l.targetElf.Symtab.GetSymbolWithIdx(0)) // copy null symbol

	for symIdx, sym := range l.targetElf.Symtab.GetAll() {
		if sym.Binding() == elf.SB_GLOBAL {
			strIdx := dynStrtab.PutString(l.targetElf.Strtab.GetStringForIndex(sym.Sname))
			dynsym := elf.Symbol{
				Sname: strIdx,
				Sinfo: sym.Sinfo,
				Sother: sym.Sother,
				Sshndx: sym.Sshndx,
				Svalue: sym.Svalue,
				Ssize: sym.Ssize,
			}
			symIdxToDynsymIdx[uint32(symIdx)] = dynSymtab.AddSymbol(&dynsym)
		}
	}

	l.targetElf.DynSymtab = dynSymtab
	l.targetElf.DynStrtab = dynStrtab
	// nbuckets := uint64((dynSymtab.Size() + 1) / 2)
	nbuckets := uint64(1)
	l.targetElf.SymbolHashTab = elf.BuildSymbolHashTab(nbuckets, dynSymtab, dynStrtab)
	return
}

func (l *DynamicLinker) createRelocationSections() (symIdxToRelaDynIdx map[uint32]int, symIdxToRelaPltIdx map[uint32]int) {
	symbolsToPlaceInPlt := l.helper.GetIdxsOfFunctionSymbolsToBePlacedInPlt(l.targetElf)
	symbolsToPlaceInGot := l.helper.GetIdxsOfSymbolsToBePlacedInGot(l.targetElf)
	// uniqueSymbols := utils.SetOf[uint32](symbolsToPlaceInGot...).With(symbolsToPlaceInPlt...)
	l.targetElf.GOT = make([]uint64, len(symbolsToPlaceInGot))
	l.targetElf.GOT_PLT = make([]uint64, DYNAMIC_LINKER_GOT_PLT_ENTRIES + len(symbolsToPlaceInPlt))
	l.targetElf.PLT = make([]elf.PLTEntry, DYNAMIC_LINKER_PLT_ENTIRES + len(symbolsToPlaceInPlt))
	relaDynEntries := 0
	relaPltEntries := 0
	symIdxToRelaDynIdx = map[uint32]int{}
	symIdxToRelaPltIdx = map[uint32]int{}
	
	for _, relaTextEntry := range l.targetElf.RelaTextEntries {
		symIdx := relaTextEntry.SymbolIdx()
		switch relaTextEntry.RelocationType() {
			case elf.R_X86_64_PLT32:
				if !l.targetElf.Symtab.IsDefined(symIdx) {
					if _, ok := symIdxToRelaPltIdx[symIdx]; !ok {
						symIdxToRelaPltIdx[symIdx] = relaPltEntries
						relaPltEntries++
					}
				}
			case elf.R_X86_64_REX_GOTPCRELX:
				if _, ok := symIdxToRelaDynIdx[symIdx]; !ok {
					symIdxToRelaDynIdx[symIdx] = relaDynEntries
					relaDynEntries++	
				}
		}
	}

	l.targetElf.RelaDynEntries = make([]*elf.RelaEntry, relaDynEntries)
	l.targetElf.RelaPltEntries = make([]*elf.RelaEntry, relaPltEntries)
	return
}

func (l *DynamicLinker) addSectionAddrDynamicEntry(tag uint64, sectionName string) {
	entryIdx := l.targetElf.Dynamic.AddDynamicEntry(tag, 0)
	l.addOnAddressesAssignedCallback(func() {
		entry := l.targetElf.Dynamic.GetEntry(entryIdx)
		entry.DvalOrPtr = l.targetElf.SectionHdrTable.GetHeader(sectionName).Saddr
	})
}

func (l *DynamicLinker) createDynamicSection(depHelper *DependencyHelper, soname *string) {
	l.targetElf.Dynamic = elf.NewDynamicTab()
	dynamicTab := l.targetElf.Dynamic
	var sonameStrIdx uint32
	if soname != nil {
		sonameStrIdx = l.targetElf.DynStrtab.PutString(*soname)
	}

	for _, depName := range depHelper.GetNamesForDynamicNeededEntries() {
		depNameIdx := l.targetElf.DynStrtab.PutString(depName)
		dynamicTab.AddDynamicEntry(elf.DT_NEEDED, uint64(depNameIdx))
	}
	if l.targetElf.SectionHdrTable.HasSection(elf.RELA_PLT) {
		dynamicTab.AddDynamicEntry(elf.DT_PLTRELSZ, uint64(len(l.targetElf.RelaPltEntries) * elf.RELA_ENTRY_SIZE))
		dynamicTab.AddDynamicEntry(elf.DT_PLTREL, elf.DT_RELA)
	}
	if l.targetElf.SectionHdrTable.HasSection(elf.GOT_PLT) {
		l.addSectionAddrDynamicEntry(elf.DT_PLTGOT, elf.GOT_PLT)
	}

	l.addSectionAddrDynamicEntry(elf.DT_HASH, elf.HASH_SECTION)
	l.addSectionAddrDynamicEntry(elf.DT_STRTAB, elf.DYNSTR)
	l.addSectionAddrDynamicEntry(elf.DT_SYMTAB, elf.DYNSYM)
	if l.targetElf.SectionHdrTable.HasSection(elf.RELA_DYN) {
		l.addSectionAddrDynamicEntry(elf.DT_RELA, elf.RELA_DYN)
		dynamicTab.AddDynamicEntry(elf.DT_RELASZ, uint64(len(l.targetElf.RelaDynEntries) * elf.RELA_ENTRY_SIZE))
		dynamicTab.AddDynamicEntry(elf.DT_RELAENT, elf.RELA_ENTRY_SIZE)
	}

	dynamicTab.AddDynamicEntry(elf.DT_STRSZ, uint64(l.targetElf.DynStrtab.GetSize()))
	dynamicTab.AddDynamicEntry(elf.DT_SYMENT, elf.SYMBOL_SIZE)
	if soname != nil  {
		dynamicTab.AddDynamicEntry(elf.DT_SONAME, uint64(sonameStrIdx))
	}

	if l.targetElf.SectionHdrTable.HasSection(elf.RELA_PLT) {
		l.addSectionAddrDynamicEntry(elf.DT_JMPREL, elf.RELA_PLT)
	}

	// must be the last entry
	dynamicTab.AddDynamicEntry(elf.DT_NULL, 0)
}

func (l *DynamicLinker) addNeededSymbols() {
	if l.targetElf.SectionHdrTable.HasSection(elf.GOT) || l.targetElf.SectionHdrTable.HasSection(elf.GOT_PLT) {
		sectionName := elf.GOT_PLT
		if l.targetElf.SectionHdrTable.HasSection(elf.GOT) {
			sectionName = elf.GOT
		}
		idx := l.targetElf.SectionHdrTable.GetSectionIdx(sectionName)
		l.helper.AddGOTReservedSymbol(l.targetElf, idx, UNKNOWN_DYNAMIC_VALUE, elf.SB_LOCAL)
	}
	idx := l.targetElf.SectionHdrTable.GetSectionIdx(elf.DYNAMIC)
	l.helper.AddDynamicReservedSymbol(l.targetElf, idx, UNKNOWN_DYNAMIC_VALUE)
	l.helper.ReorderSymbolsToHaveLocalsFirst(l.targetElf)
}

// requires dynamic section to be already mapped to other loadable segment
func (l *DynamicLinker) CreateDynamicProgramHeader() *elf.ProgramHeader {
	dynamicHdr := l.targetElf.SectionHdrTable.GetHeader(elf.DYNAMIC)
	return &elf.ProgramHeader{
		Ptype: uint32(elf.PT_DYNAMIC),
		Pflags: uint32(elf.PF_R) | uint32(elf.PF_W),
		Poffset: dynamicHdr.Soffset,
		Pvaddr: dynamicHdr.Saddr,
		Ppaddr: dynamicHdr.Saddr,
		Pfilesz: dynamicHdr.Ssize,
		Pmemsz: dynamicHdr.Ssize,
		Palign: DYNAMIC_SEGMENT_ALIGNMENT,
	}
}

func (l *DynamicLinker) createSharedLibDynamicExeCommonProgramHeaders(
	programHdrsCount int,
	firstSectionIdx uint16,
	startVMA uint64,
) []*elf.ProgramHeader {
	dynamicReadableSegment := l.helper.createSegmentContainingElfHeaderAndProgramHeaders(programHdrsCount)
	dynamicReadableSegment.Pvaddr = startVMA
	dynamicReadableSegment.Ppaddr = startVMA
	lastSectionIdx := l.helper.HighestExistingSectionIdx(l.targetElf, elf.DYNSTR, elf.RELA_DYN, elf.RELA_PLT)
	l.helper.joinSectionsToSegment(l.targetElf, dynamicReadableSegment, firstSectionIdx, lastSectionIdx)

	firstSectionIdx = lastSectionIdx + 1
	lastSectionIdx = l.helper.HighestExistingSectionIdx(l.targetElf, elf.PLT, elf.TEXT) 
	fileOffset := dynamicReadableSegment.Poffset + dynamicReadableSegment.Pfilesz
	virtualAddr := dynamicReadableSegment.Pvaddr + dynamicReadableSegment.Pmemsz
	// TODO why ld.so does it this way?
	// fileOffset := uint64(PAGE_SIZE)
	// virtualAddr := uint64(PAGE_SIZE)
	codeSegment := l.helper.createLoadableSegmentFromContiguousSections(
		l.targetElf, firstSectionIdx, lastSectionIdx, fileOffset,  virtualAddr, elf.PF_R | elf.PF_X,
	)

	firstSectionIdx = lastSectionIdx + 1
	lastSectionIdx = l.helper.HighestExistingSectionIdx(l.targetElf, elf.DYNAMIC, elf.GOT, elf.GOT_PLT, elf.DATA, elf.BSS) 
	fileOffset = codeSegment.Poffset + codeSegment.Pfilesz
	virtualAddr = codeSegment.Pvaddr + codeSegment.Pmemsz
	rwDataHeader := l.helper.createLoadableSegmentFromContiguousSections(
		l.targetElf, firstSectionIdx, lastSectionIdx, fileOffset,  virtualAddr, elf.PF_R | elf.PF_W,
	)
	res := []*elf.ProgramHeader{
		dynamicReadableSegment, codeSegment, rwDataHeader,
	}

	fileOffset = rwDataHeader.Poffset + rwDataHeader.Pfilesz
	virtualAddr = rwDataHeader.Pvaddr + rwDataHeader.Pmemsz
	if l.targetElf.SectionHdrTable.HasSection(elf.RO_DATA) {
		rodataHdr := l.helper.createLoadableSegmentFromContiguousSections(
			l.targetElf, lastSectionIdx + 1, lastSectionIdx + 1, fileOffset, virtualAddr, elf.PF_R,
		)	
		fileOffset = rodataHdr.Poffset + rodataHdr.Pfilesz
		lastSectionIdx++
		res = append(res, rodataHdr)
	}

	l.helper.updateFileOffsetsOfSectionsBetween(l.targetElf,
		 lastSectionIdx + 1, l.targetElf.SectionHdrTable.NumberOfSections() - 1, fileOffset)

	res = append(res, l.CreateDynamicProgramHeader())
	return res
}

func (l *DynamicLinker) createSharedLibProgramHeaders() {
	programHdrsCount := 4
	if l.targetElf.SectionHdrTable.HasSection(elf.RO_DATA) {
		programHdrsCount++
	}
	hdrs := l.createSharedLibDynamicExeCommonProgramHeaders(programHdrsCount, 1, 0)
	l.targetElf.ProgramHdrTable = elf.NewProgramHdrTable()
	for _, hdr := range hdrs {
		l.targetElf.ProgramHdrTable.AddProgramHeader(*hdr)
	}
}

func (l *DynamicLinker) createDynamicExeProgramHeaders() {
	programHdrsCount := 6
	if l.targetElf.SectionHdrTable.HasSection(elf.RO_DATA) {
		programHdrsCount++
	}

	hdrs := l.createSharedLibDynamicExeCommonProgramHeaders(programHdrsCount, 1, EXECUTABLE_START_VMA)
	dynamicReadableSegment := hdrs[0]

	phdrHeader := l.helper.createPhdrProgramHeader(uint64(programHdrsCount) * elf.PROGRAM_HEADER_SIZE)
	phdrHeader.Poffset = elf.ELF_HEADER_SIZE
	phdrHeader.Pvaddr = (dynamicReadableSegment.Pvaddr + elf.ELF_HEADER_SIZE)
	phdrHeader.Ppaddr = phdrHeader.Pvaddr

	interpSectionHdr := l.targetElf.SectionHdrTable.GetHeader(elf.INTERP)
	interpHeader := l.helper.createInterpProgramHeader(interpSectionHdr.Ssize)
	interpHeader.Poffset = interpSectionHdr.Soffset
	interpHeader.Pvaddr = interpSectionHdr.Saddr
	interpHeader.Ppaddr = interpHeader.Pvaddr

	l.targetElf.ProgramHdrTable = elf.NewProgramHdrTable().WithProgramHeaders(*phdrHeader, *interpHeader)
	for _, hdr := range hdrs {
		l.targetElf.ProgramHdrTable.AddProgramHeader(*hdr)
	}
}

func (l *DynamicLinker) setTypeOfUndefinedSymbolsAndGetIdxsOfOnesThatCantBeFound(depHelper *DependencyHelper) []uint32 {
	missingSymbolIdx := []uint32{}

	for _, symIdx := range l.targetElf.Symtab.GetUndefinedSymbolIdxs() {
		sym := l.targetElf.Symtab.GetSymbolWithIdx(symIdx)
		symName := l.targetElf.Strtab.GetStringForIndex(sym.Sname)
		if registeredSym, ok := depHelper.LookupSymbol(symName); ok {
			// TODO if its undefined (but registered) in direct dependencies 
			// we should recursivly check its dependencies
			// another option is to forbid shared objects that reference unknown symbols
			// but that doesn't comply with the SYS V ABI, for now we do the latter
			sym.Sinfo = registeredSym.Sinfo
		} else {
			missingSymbolIdx = append(missingSymbolIdx, symIdx)
		}
	}
	return missingSymbolIdx
}

func (l *DynamicLinker) fillGOTSectionAndGetSymbolMapping() map[uint32]int {
	symbolMapping := map[uint32]int{}

	for i, symIdx := range l.helper.GetIdxsOfSymbolsToBePlacedInGot(l.targetElf) {
		sym := l.targetElf.Symtab.GetSymbolWithIdx(symIdx)
		symbolMapping[symIdx] = i
		l.targetElf.GOT[i] = sym.Svalue
	}

	return symbolMapping
}

func (l *DynamicLinker) getPltSymbolMapping() map[uint32]GotPltIdxs {
	symbolMapping := map[uint32]GotPltIdxs{}

	for i, symIdx := range l.helper.GetIdxsOfFunctionSymbolsToBePlacedInPlt(l.targetElf) {
		symbolMapping[symIdx] = GotPltIdxs{
			PltOffset: DYNAMIC_LINKER_PLT_ENTIRES + i,
			GotOffset: DYNAMIC_LINKER_GOT_PLT_ENTRIES + i,
		}
	}

	return symbolMapping
}

func (l *DynamicLinker) fixDisplacement(
	code []uint8,
	targetVMA uint64,
	ripBeforeCode uint64,
	displacementToFix asm.DisplacementToFix,
) {
	rip := ripBeforeCode + uint64(displacementToFix.CodeOffset + displacementToFix.SizeToFix + displacementToFix.InstructionSizeAfterDisplacement) 
	displacement := uint32(targetVMA - rip)
	copy(code[displacementToFix.CodeOffset:], utils.EncodeUnsignedIntToLittleEndianU2(displacement))
}

func (l DynamicLinker) fillWithNoops(code []uint8, fromInclusive int, toExclusive int) {
	for i := fromInclusive; i < toExclusive; i++ {
		code[i] = asm.NOP_OPCODE
	}
}

func (l *DynamicLinker) createDynamicLinkerPltEntry() {
	// push GOT.PLT[1]   ; dynamic linker info
	// jmp [GOT.PLT[2]]  ; address of dynamic linker
	code, displacementsToFix := l.assembler.AssembleStandalone(
		codegen.PushAsmLine{Operand: codegen.EmptyOperands().
			UsingRIP().WithUnknownDisplacement().WithExplicitSize(codegen.QWORD_SIZE),
		},
		codegen.JumpAsmLine{Target: codegen.EmptyOperands().
			UsingRIP().WithUnknownDisplacement().WithExplicitSize(codegen.QWORD_SIZE),
			IsDirectlyRipRelative: false,
		},
	)
	if len(code) > elf.PLT_ENTRY_SIZE {
		panic("PLT code too large")
	}
	pltVMA := l.targetElf.SectionHdrTable.GetHeader(elf.PLT).Saddr
	gotPltVMA := l.targetElf.SectionHdrTable.GetHeader(elf.GOT_PLT).Saddr
	initialRIP := pltVMA

	// push GOT.PLT[1]
	l.fixDisplacement(code, gotPltVMA + elf.GOT_ENTRY_SIZE, initialRIP, displacementsToFix[0])
	// jmp GOT.PLT[2]
	l.fixDisplacement(code, gotPltVMA + 2 * elf.GOT_ENTRY_SIZE, initialRIP, displacementsToFix[1])

	copy(l.targetElf.PLT[0].Code[:], code)
	l.fillWithNoops(l.targetElf.PLT[0].Code[:], len(code), elf.PLT_ENTRY_SIZE)
}

func (l *DynamicLinker) createFunctionPltEntry(pltIdx int, relaPltIdx int, pltGotIdx int) {
	// jmp [GOT.PLT[pltGotIdx]]
	// push relaPltIdx
	// jmp PLT[0]
	code, displacementsToFix := l.assembler.AssembleStandalone(
		codegen.JumpAsmLine{Target: codegen.EmptyOperands().
			UsingRIP().WithUnknownDisplacement().WithExplicitSize(codegen.QWORD_SIZE),
			IsDirectlyRipRelative: false,
		},
		codegen.PushAsmLine{Operand: codegen.EmptyOperands().
			WithImmediate(&codegen.Immediate{
				Val: int64(relaPltIdx),
				Size: RELA_IDX_SIZE},
			).WithExplicitSize(RELA_IDX_SIZE),
		},
		codegen.JumpAsmLine{Target: codegen.EmptyOperands().
			UsingRIP().WithUnknownDisplacement().WithExplicitSize(codegen.QWORD_SIZE),
			IsDirectlyRipRelative: true,
		},
	)
	if len(code) > elf.PLT_ENTRY_SIZE {
		panic("PLT code too large")
	}
	pltVMA := l.targetElf.SectionHdrTable.GetHeader(elf.PLT).Saddr
	gotPltVMA := l.targetElf.SectionHdrTable.GetHeader(elf.GOT_PLT).Saddr
	initialRIP := pltVMA + uint64(pltIdx * elf.PLT_ENTRY_SIZE)

	// jmp [GOT.PLT[pltGotIdx]]
	jmpGotPltDisp := displacementsToFix[0]
	l.fixDisplacement(code, gotPltVMA + uint64(pltGotIdx * elf.GOT_ENTRY_SIZE), initialRIP, jmpGotPltDisp)

	nextInstructionVMA := initialRIP
	nextInstructionVMA += uint64(jmpGotPltDisp.CodeOffset + jmpGotPltDisp.SizeToFix + jmpGotPltDisp.InstructionSizeAfterDisplacement)
	l.targetElf.GOT_PLT[pltGotIdx] = nextInstructionVMA

	// jmp PLT[0]
	l.fixDisplacement(code, pltVMA, initialRIP, displacementsToFix[1])
	copy(l.targetElf.PLT[pltIdx].Code[:], code)
	l.fillWithNoops(l.targetElf.PLT[pltIdx].Code[:], len(code), elf.PLT_ENTRY_SIZE)
}

func (l *DynamicLinker) relocateDynamicExeOrSharedLibrary(
	symtabToDynsymIdxMapping map[uint32]uint32,
	symIdxToGotIdx map[uint32]int,
	symIdxToPltInfo map[uint32]GotPltIdxs,
	symIdxToRelaDynIdx map[uint32]int,
	symIdxToRelaPltIdx map[uint32]int,
) {
	textStartVMA := l.targetElf.SectionHdrTable.GetHeader(elf.TEXT).Saddr
	var pltStartVMA uint64
	var gotStartVMA uint64
	var gotPltStartVMA uint64
	if gotHdr, ok := l.targetElf.SectionHdrTable.MaybeHeader(elf.GOT); ok {
		gotStartVMA = gotHdr.Saddr
	}
	if pltHdr, ok := l.targetElf.SectionHdrTable.MaybeHeader(elf.PLT); ok {
		pltStartVMA = pltHdr.Saddr
		gotPltStartVMA = l.targetElf.SectionHdrTable.GetHeader(elf.GOT_PLT).Saddr
		l.createDynamicLinkerPltEntry()
	}
	alreadyInitializedPlt := utils.NewSet[uint32]()

	for _, rela := range l.targetElf.RelaTextEntries {
		var symbolAddr uint64
		sym := l.targetElf.Symtab.GetSymbolWithIdx(rela.SymbolIdx())

		switch rela.RelocationType() {
		case elf.R_X86_64_PC32:
			symbolAddr = sym.Svalue
		case elf.R_X86_64_PLT32:
			if sym.Sshndx == elf.SHN_UNDEF {
				pltInfo := symIdxToPltInfo[rela.SymbolIdx()]
				if !alreadyInitializedPlt.Has(rela.SymbolIdx()) {
					relaPltIdx := symIdxToRelaPltIdx[rela.SymbolIdx()]
					l.targetElf.RelaPltEntries[relaPltIdx] = &elf.RelaEntry{
						Roffset: gotPltStartVMA + (uint64(pltInfo.GotOffset * elf.GOT_ENTRY_SIZE)),
						Rinfo: elf.EncodeRelocationInfo(symtabToDynsymIdxMapping[rela.SymbolIdx()], elf.R_X86_64_JUMP_SLOT),
						Raddend: 0,
					}
					l.createFunctionPltEntry(pltInfo.PltOffset, relaPltIdx, pltInfo.GotOffset)
					alreadyInitializedPlt.Add(rela.SymbolIdx())
				}
				symbolAddr = pltStartVMA + uint64(pltInfo.PltOffset * elf.PLT_ENTRY_SIZE)
			} else {
				symbolAddr = sym.Svalue
			}
		case elf.R_X86_64_REX_GOTPCRELX:
			gotIdx := symIdxToGotIdx[rela.SymbolIdx()]
			symbolAddr = gotStartVMA + uint64(elf.GOT_ENTRY_SIZE * gotIdx)
			l.targetElf.RelaDynEntries[symIdxToRelaDynIdx[rela.SymbolIdx()]] = &elf.RelaEntry{
				Roffset: gotStartVMA + uint64(gotIdx * elf.GOT_ENTRY_SIZE),
				Rinfo: elf.EncodeRelocationInfo(symtabToDynsymIdxMapping[rela.SymbolIdx()], elf.R_X86_64_GLOB_DAT),
				Raddend: 0,
			}
		default:
			panic("Unsupported relocationType")
		}
		relocation := int64(symbolAddr - (textStartVMA + rela.Roffset)) + rela.Raddend
		for i, b := range elf.EncodeRelocationValue(int32(relocation)) {
			l.targetElf.Code[int(rela.Roffset) + i] = b
		}
	}
	l.targetElf.RelaTextEntries = []*elf.RelaEntry{}
}

func (l *DynamicLinker) fixSharedLibHeader() {
	l.helper.UpdateHeaderForElfWithProgramHeaders(l.targetElf)
	l.targetElf.Header.Etype = elf.SHARED_OBJECT_FILE
	l.targetElf.Header.Eentry = 0
}

func (l *DynamicLinker) fixDynamicExeHeader(entrySymbolName string) error {
	l.helper.UpdateHeaderForElfWithProgramHeaders(l.targetElf)
	l.targetElf.Header.Etype = elf.EXECUTABLE_FILE
	return l.helper.SetEntryPoint(l.targetElf, entrySymbolName)
}

func (l *DynamicLinker) loadTargetElfAndDependencies(
	objFilePath string,
	dependencyDirs []string,
	dependencyShortNames []string,
) (*DependencyHelper, error) {
	e, err := elf.NewDeserializer().Deserialize(objFilePath)
	if err != nil {
		return nil, err
	}
	if e.Header.Etype != elf.RELOCATABLE_FILE {
		return nil, errors.New("Need relocatable file to create shared object or dynamic executable")
	}
	depHelper, err := newDependencyHelper(dependencyDirs, dependencyShortNames)
	if err != nil {
		return nil, err
	}
	l.targetElf = e
	return depHelper, nil 
}

func (l *DynamicLinker) checkIfAllSymbolsCanBeFound(depHelper *DependencyHelper) error {
	missingSymbols := l.setTypeOfUndefinedSymbolsAndGetIdxsOfOnesThatCantBeFound(depHelper)
	if len(missingSymbols) > 0 {
		return errors.New(fmt.Sprintf("Undefined symbols: %s",
			strings.Join(l.helper.GetNamesOfSymbolsWithIdxs(l.targetElf, missingSymbols), ", ")))
	}
	return nil
}

func (l *DynamicLinker) buildSharedObjectOrExecutable(
	objFilePath string,
	resultPath string,
	dependencyDirs []string,
	dependencyShortNames []string,
	sectionsProvider func() []string,
	dynamicSectionBuilder func(*DependencyHelper),
	programHeadersBuilder func(),
	elfHeaderBuilder func(),
) error {
	depHelper, err := l.loadTargetElfAndDependencies(objFilePath, dependencyDirs, dependencyShortNames)
	if err != nil {
		return err
	}
	if err := l.checkIfAllSymbolsCanBeFound(depHelper); err != nil {
		return err
	}
	l.targetElf.Interp = RUNTIME_DYNAMIC_LINKER

	l.createSectionHeaders(sectionsProvider())
	l.addNeededSymbols()
	symIdxToDynsymIdx := l.createDynsymSection()
	symIdxToRelaDynIdx, symIdxToRelaPltIdx := l.createRelocationSections()
	dynamicSectionBuilder(depHelper)

	l.helper.SetSectionSizes(l.targetElf)
	programHeadersBuilder()
	l.helper.SetSymbolValues(l.targetElf, l.targetElf.Symtab)
	l.helper.SetSymbolValues(l.targetElf, l.targetElf.DynSymtab)
	l.notifyAddressesAssigned()

	symIdxToGotIdx := l.fillGOTSectionAndGetSymbolMapping()
	symIdxToPltIdx := l.getPltSymbolMapping()
	l.relocateDynamicExeOrSharedLibrary(symIdxToDynsymIdx, symIdxToGotIdx, symIdxToPltIdx, symIdxToRelaDynIdx, symIdxToRelaPltIdx)
	
	l.helper.FixSectionInterlinks(l.targetElf)
	elfHeaderBuilder()
	return elf.NewSerializer().Serialize(l.targetElf, resultPath, 0744)
}

func (l *DynamicLinker) CreateSharedLibrary(
	objFilePath string,
	resultPath string,
	soname string,
	dependencyDirs []string,
	dependencyShortNames []string,
) error {
	sectionsProvider := func() []string {
		return l.getSectionsInOrder(l.getSharedLibPossiblyPresentSectionsInOrder())
	}
	dynamicSectionBuilder := func(depHelper *DependencyHelper) {
		l.createDynamicSection(depHelper, &soname)
	}
	return l.buildSharedObjectOrExecutable(
		objFilePath,
		resultPath,
		dependencyDirs,
		dependencyShortNames,
		sectionsProvider,
		dynamicSectionBuilder,
		l.createSharedLibProgramHeaders,
		l.fixSharedLibHeader,
	)
}

func (l *DynamicLinker) CreateDynamicallyLinkedExecutable(
	objFilePath string,
	resultPath string,
	entrySymbolName string, 
	dependencyDirs []string,
	dependencyShortNames []string,
) error {
	sectionsProvider := func() []string {
		return l.getSectionsInOrder(l.getDynamicExecPossiblyPresentSectionsInOrder())
	}
	dynamicSectionBuilder := func(depHelper *DependencyHelper) {
		l.createDynamicSection(depHelper, nil)
	}
	programHeadersBuilder := func() {
		l.fixDynamicExeHeader(entrySymbolName)
	}
	return l.buildSharedObjectOrExecutable(
		objFilePath,
		resultPath,
		dependencyDirs,
		dependencyShortNames,
		sectionsProvider,
		dynamicSectionBuilder,
		l.createDynamicExeProgramHeaders,
		programHeadersBuilder,
	)
}
