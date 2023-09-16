package linkers

import (
	"elf"
	"errors"
	"math"
	"strings"
	"utils"
)

const PAGE_SIZE = 4096
const TEXT_VIRT_ADDR = 0x400000 // same as for gnu linker 

type Linker struct {
	textVirtAddr uint64
	textFileOffset uint64
}

func New() *Linker {
	return &Linker{
		textVirtAddr: TEXT_VIRT_ADDR, // must be page aligned to the offset below
		textFileOffset: 0x1000, // we will insert some padding for simplicity, this will guarantee proper text alignment
	}
}

// goal is to minimize padding from lastSegmentFileOffsetEnd to nextSegmentFileOffsetStart
// relativeAlignment must be a multiple of alignment, this should be guaranteed by the elf spec
func (l *Linker) getNextSegmentPhysicalAndVirtualOffsets(
	lastSegmentVirtualAddrEnd uint64,
	lastSegmentFileOffsetEnd uint64,
	requiredAlignment uint64,
	requiredRelativeAlignment uint64,
) (nextSegmentVirtualAddrStart uint64, nextSegmentFileOffsetStart uint64) {
	if requiredRelativeAlignment % requiredAlignment != 0 {
		panic("Relative relative alignment must be a multiple of required alignment")
	}
	nextSegmentFileOffsetStart = lastSegmentFileOffsetEnd
	if remainder := nextSegmentFileOffsetStart % requiredAlignment; remainder != 0 {
		nextSegmentFileOffsetStart += requiredAlignment - remainder
	}
	nextSegmentVirtualAddrStart = lastSegmentVirtualAddrEnd
	if remainder := nextSegmentVirtualAddrStart % requiredAlignment; remainder != 0 {
		nextSegmentVirtualAddrStart += requiredAlignment - remainder
	}
	virtPhyDelta := nextSegmentFileOffsetStart - nextSegmentVirtualAddrStart
	if remainder := virtPhyDelta % requiredRelativeAlignment; remainder != 0 {
		nextSegmentVirtualAddrStart += remainder
	}
	// we can't have multiple segments mapped to the same page if they have different access permissions
	nextSegmentVirtualAddrStart += PAGE_SIZE 
	return
}

func (l *Linker) createTextSegmentAndFixSectionMeta(e *elf.ElfFile) *elf.ProgramHeader {
	textSectionHdr := e.SectionHdrTable.GetHeader(elf.TEXT)
	hdr := elf.ProgramHeader{
		Ptype: uint32(elf.PT_LOAD),
		Pflags: uint32(elf.PF_R) | uint32(elf.PF_X),
		Poffset: l.textFileOffset,
		Pvaddr: l.textVirtAddr,
		Ppaddr: l.textVirtAddr,
		Pfilesz: textSectionHdr.Ssize,
		Pmemsz: textSectionHdr.Ssize,
		Palign: PAGE_SIZE,
	}
	textSectionHdr.Saddr = l.textVirtAddr
	textSectionHdr.Soffset = l.textFileOffset
	return &hdr
}

func (l *Linker) createLoadableSegmentFromContiguousProgbitsOrNobitsSections(
	e *elf.ElfFile,
	startSectionIdx uint16,
	endSectionIdx uint16, // inclusive
	prevSectionEndFileOffset uint64,
	prevSectionEndVirtualAddr uint64,
	pflags uint32,
) *elf.ProgramHeader {
	hdr := elf.ProgramHeader{
		Ptype: uint32(elf.PT_LOAD),
		Pflags: pflags,
		Palign: PAGE_SIZE,
	}
	firstSectionHdr := e.SectionHdrTable.GetHeaderByIdx(startSectionIdx)
	firstVirtAddr, firstFileOffset := l.getNextSegmentPhysicalAndVirtualOffsets(
		prevSectionEndVirtualAddr, prevSectionEndFileOffset, firstSectionHdr.Saddralign, PAGE_SIZE)
	hdr.Poffset = firstFileOffset
	hdr.Pvaddr = firstVirtAddr
	hdr.Ppaddr = firstVirtAddr
	hdr.Pmemsz = firstSectionHdr.Ssize
	if firstSectionHdr.Stype == uint32(elf.S_PROGBITS) {
		hdr.Pfilesz = hdr.Pmemsz
	} else {
		hdr.Pfilesz = 0	
	}
	firstSectionHdr.Saddr = firstVirtAddr
	firstSectionHdr.Soffset = firstFileOffset

	prevEndVirtAddr := firstVirtAddr + hdr.Pmemsz
	prevEndPhyAddr := firstFileOffset + hdr.Pfilesz

	for sectionIdx := startSectionIdx + 1; sectionIdx <= endSectionIdx; sectionIdx++ {
		sectionHdr := e.SectionHdrTable.GetHeaderByIdx(sectionIdx)
		sectionStartVirtAddr := prevEndVirtAddr
		sectionStartPhyAddr := prevEndPhyAddr
		var prevSectionAlignment uint64 = 0
		if sectionHdr.Saddralign != 0 {
			if remainder := sectionStartVirtAddr % sectionHdr.Saddralign; remainder != 0 {
				prevSectionAlignment = sectionHdr.Saddralign - remainder
				sectionStartVirtAddr += prevSectionAlignment
				sectionStartPhyAddr += prevSectionAlignment
			}
		}
		sectionHdr.Saddr = sectionStartVirtAddr
		sectionHdr.Soffset = sectionStartPhyAddr
		hdr.Pmemsz += sectionHdr.Ssize + prevSectionAlignment
		if sectionHdr.Stype == uint32(elf.S_PROGBITS) {
			hdr.Pfilesz += sectionHdr.Ssize + prevSectionAlignment
			prevEndPhyAddr = sectionStartPhyAddr + sectionHdr.Ssize
		} else {
			hdr.Pfilesz += prevSectionAlignment
			prevEndPhyAddr += prevSectionAlignment
		}
		prevEndVirtAddr = sectionStartVirtAddr + sectionHdr.Ssize
	}
	return &hdr
}

func (l *Linker) assertAdjacentOrMissing(e *elf.ElfFile, sectionNames ...string) (minIdx uint16, maxIdx uint16, allMissing bool) {
	sectionIdxs := []uint16{}
	minIdx = math.MaxUint16
	maxIdx = 0
	needsReordering := false
	allMissing = true
	for _, name := range sectionNames {
		if e.SectionHdrTable.HasSection(name) {
			allMissing = false
			idx := e.SectionHdrTable.GetSectionIdx(name)
			sectionIdxs = append(sectionIdxs, e.SectionHdrTable.GetSectionIdx(name))
			if idx > maxIdx {
				maxIdx = idx
			}
			if idx < minIdx {
				minIdx = idx
			}
			if len(sectionIdxs) > 1 && idx != sectionIdxs[len(sectionIdxs) - 2] + 1 {
				needsReordering = true
			}
		}
	}
	if needsReordering {
		panic("TODO sections are not adjacent and need reordering")
	}
	return
}

func (l *Linker) updateFileOffsetsOfSectionsBetween(
	e *elf.ElfFile,
	minSectionIdx uint16,
	maxSectionIdx uint16, // inclusive
	preceedingFileSize uint64,
) (fileOffset uint64) {
	for sectionIdx := minSectionIdx; sectionIdx <= maxSectionIdx; sectionIdx++ {
		sectionHdr := e.SectionHdrTable.GetHeaderByIdx(sectionIdx)
		sectionHdr.Soffset = preceedingFileSize
		if sectionHdr.Stype != elf.S_NOBITS {
			preceedingFileSize += sectionHdr.Ssize
		}
	}
	return preceedingFileSize
}

func (l *Linker) createProgramHeaders(e *elf.ElfFile) {
	programHdrTable := elf.NewProgramHdrTable()
	textHdr := l.createTextSegmentAndFixSectionMeta(e)
	programHdrTable.AddProgramHeader(*textHdr)
	fileOffset := textHdr.Poffset + textHdr.Pfilesz
	textSectionIdx := e.SectionHdrTable.GetSectionIdx(elf.TEXT)
	prevVirtualAddrEnd := textHdr.Pvaddr + textHdr.Pmemsz
	l.updateFileOffsetsOfSectionsBetween(e, textSectionIdx + 1, e.SectionHdrTable.NumberOfSections() - 1, fileOffset) 

	nextSegmentMinSectionIdx, nextSegmentMaxSectionIdx, allMissing := l.assertAdjacentOrMissing(e, elf.GOT, elf.DATA, elf.BSS)
	if !allMissing {
		dataAlikesHdr := l.createLoadableSegmentFromContiguousProgbitsOrNobitsSections(
			e, nextSegmentMinSectionIdx, nextSegmentMaxSectionIdx,
			fileOffset, prevVirtualAddrEnd, uint32(elf.PF_R) | uint32(elf.PF_W),
		) 
		programHdrTable.AddProgramHeader(*dataAlikesHdr)
		fileOffset = dataAlikesHdr.Poffset + dataAlikesHdr.Pfilesz
		prevVirtualAddrEnd = dataAlikesHdr.Pvaddr + dataAlikesHdr.Pmemsz
		l.updateFileOffsetsOfSectionsBetween(e, nextSegmentMaxSectionIdx + 1, e.SectionHdrTable.NumberOfSections() - 1, fileOffset)
	}
	
	if e.SectionHdrTable.HasSection(elf.RO_DATA) {
		rodataSectionIdx := e.SectionHdrTable.GetSectionIdx(elf.RO_DATA)
		rodataHdr := l.createLoadableSegmentFromContiguousProgbitsOrNobitsSections(
			e, rodataSectionIdx, rodataSectionIdx, fileOffset, prevVirtualAddrEnd, uint32(elf.PF_R),
		)
		programHdrTable.AddProgramHeader(*rodataHdr)
		fileOffset = rodataHdr.Poffset + rodataHdr.Pfilesz
		prevVirtualAddrEnd = rodataHdr.Pvaddr + rodataHdr.Pmemsz
		l.updateFileOffsetsOfSectionsBetween(e, rodataSectionIdx + 1, e.SectionHdrTable.NumberOfSections() - 1, fileOffset)
	}

	e.ProgramHdrTable = programHdrTable
}

func (l *Linker) setSymbolValues(e *elf.ElfFile) {
	for _, sym := range e.Symtab.GetAll() {
		switch sym.Type() {
		case elf.ST_NOTYPE, elf.ST_FILE:
			// no need to do anything
		case elf.ST_OBJECT, elf.ST_FUNC, elf.ST_SECTION:
			sectionHdr := e.SectionHdrTable.GetHeaderByIdx(sym.Sshndx)
			sym.Svalue += sectionHdr.Saddr
		default:
			panic("Unexpected symbol type") // TODO return errors instead of panicking
		}
	}
}

func (l *Linker) getIdxsOfSymbolsToBePlacedInGot(e *elf.ElfFile) []uint32 {
	symbolsPlacedInGot := utils.NewSet[uint32]()
	for _, rela := range e.RelaEntries {
		if rela.RelocationType() == elf.R_X86_64_REX_GOTPCRELX {
			symbolsPlacedInGot.Add(rela.SymbolIdx())
		}
	}
	return symbolsPlacedInGot.GetAll()
}

func (l *Linker) fixOffsetsOfSectionsSuccedingGOT(e *elf.ElfFile, relaSize int, gotSize int, gotSectionIdx uint16) {
	// this ruins alignment, so we assume that there was no alignment in file before that call
	offsetDelta := gotSize - relaSize
	for _, sectionHdr := range e.SectionHdrTable.GetSectionHeaders()[gotSectionIdx+1:] {
		sectionHdr.Soffset += uint64(offsetDelta)
	}
}

func (l *Linker) replaceRelaTextSectionWithGOT(e *elf.ElfFile) {
	if !e.SectionHdrTable.HasSection(elf.RELA_TEXT) {
		panic("No rela.text section")
	}
	relaTextIdx := e.SectionHdrTable.GetSectionIdx(elf.RELA_TEXT)
	// if we need GOT we must also have data or bss, this linker assumes 
	// that they are placed like this: .rela.text | .data? | .bss?
	// then .rela.text is replaced with GOT and no symbol section reindexing is needed
	sectionIdxs := e.SectionHdrTable.GetSectionIndexes()
	newSectionIdxs := map[string]uint16{}
	for k, v := range sectionIdxs {
		if v != relaTextIdx {
			newSectionIdxs[k] = v
		} else {
			newSectionIdxs[elf.GOT] = v
		}
	}
	e.SectionHdrTable.Reindex(newSectionIdxs)
	gotSymbols := l.getIdxsOfSymbolsToBePlacedInGot(e)
	gotSize := len(gotSymbols) * elf.GOT_ENTRY_SIZE
	e.SectionHdrTable.CreateGOTSection(0, gotSize)
	l.fixOffsetsOfSectionsSuccedingGOT(e, len(e.RelaEntries) * elf.RELA_ENTRY_SIZE, gotSize, relaTextIdx)
}

func (l *Linker) fillGOTSectionAndGetSymbolMapping(e *elf.ElfFile) map[uint32]int {
	gotSectionHdr := e.SectionHdrTable.GetHeader(elf.GOT)
	gotSectionIdx := e.SectionHdrTable.GetSectionIdx(elf.GOT)
	// TODO do we need that? if so when do we need that
	e.Symtab.AddSymbol(&elf.Symbol{
		Sname: e.Strtab.PutString(elf.GOT_SYMBOL_NAME),
		Sinfo: elf.EncodeSymbolInfo(elf.SB_GLOBAL, elf.ST_OBJECT),
		Sother: elf.RESERVED_SYMBOL_OTHER_FIELD,
		Sshndx: gotSectionIdx,
		Svalue: gotSectionHdr.Saddr,
		Ssize: 0,
	})
	e.SectionHdrTable.ChangeSectionSizeAndShiftSuccedingSections(
		uint16(e.SectionHdrTable.GetSectionIdx(elf.SYMTAB)), elf.SYMBOL_SIZE)

	symbolMapping := map[uint32]int{}
	e.GOT = make([]uint64, gotSectionHdr.Ssize / gotSectionHdr.Sentsize)

	for i, symIdx := range l.getIdxsOfSymbolsToBePlacedInGot(e) {
		sym := e.Symtab.GetSymbolWithIdx(symIdx)
		symbolMapping[symIdx] = i
		e.GOT[i] = sym.Svalue
	}

	return symbolMapping
}

func (l *Linker) removeRelaTextSection(e *elf.ElfFile) {
	if !e.SectionHdrTable.HasSection(elf.RELA_TEXT) {
		return
	}
	sectionIdxs := map[string]uint16{}
	relaTextIdx := e.SectionHdrTable.GetSectionIdx(elf.RELA_TEXT)
	prevSectionHdr := e.SectionHdrTable.GetHeaderByIdx(relaTextIdx - 1)
	for  idx := uint16(0); idx < e.SectionHdrTable.NumberOfSections(); idx++ {
		sectionName := e.SectionHdrTable.GetSectionName(idx)
		if idx < relaTextIdx {
			sectionIdxs[sectionName] = idx
		} else if idx > relaTextIdx {
			sectionIdxs[sectionName] = idx - 1
		}
	}
	e.SectionHdrTable.Reindex(sectionIdxs)

	offset := prevSectionHdr.Soffset
	if prevSectionHdr.Stype != elf.S_NOBITS {
		offset += prevSectionHdr.Ssize
	}
	l.updateFileOffsetsOfSectionsBetween(e, relaTextIdx, e.SectionHdrTable.NumberOfSections() - 1, offset)

	for _, sym := range e.Symtab.GetAll() {
		if sym.Sshndx > relaTextIdx {
			sym.Sshndx--
		}
	}
}

func (l *Linker) requiresGOT(e *elf.ElfFile) bool {
	for _, rela := range e.RelaEntries {
		if rela.RelocationType() == elf.R_X86_64_REX_GOTPCRELX {
			return true
		}
	}
	return false
}

func (l *Linker) fixProgramHeaderForStaticallyLinkedExecutable(e *elf.ElfFile, entrySymbolName string) {
	hdr := e.Header
	hdr.Etype = elf.EXECUTABLE_FILE
	hdr.Eentry = e.Symtab.GetSymbol(entrySymbolName, e.Strtab).Svalue
	hdr.Ephentsize = elf.PROGRAM_HEADER_SIZE
	hdr.Ephnum = uint16(e.ProgramHdrTable.Size())
	hdr.Ephoff = elf.ELF_HEADER_SIZE
	lastSection := e.SectionHdrTable.GetSectionWithHighestFileOffset()
	hdr.Eshoff = lastSection.Soffset + lastSection.Ssize
	hdr.Eshnum = uint16(len(e.SectionHdrTable.GetSectionHeaders()))
	hdr.Eshstrndx = e.SectionHdrTable.GetSectionIdx(elf.SECTION_STRTAB)
}

func (l *Linker) relocateStaticallyLinkedExecutable(e *elf.ElfFile, symIdxToGotIdxMapping map[uint32]int) error {
	// for static linking every symbol must be defined
	if undefinedSymbols := e.Symtab.GetUndefinedSymbols(); len(undefinedSymbols) > 0 {
		symbolNames := make([]string, len(undefinedSymbols))
		for i, sym := range undefinedSymbols {
			symbolNames[i] = e.Strtab.GetStringForIndex(sym.Sname)
		}
		namesJoined := strings.Join(symbolNames, ", ")
		return errors.New("Undefined symbols: " + namesJoined)
	}
	var gotStartVirtualAddr uint64 = 0
	if e.SectionHdrTable.HasSection(elf.GOT) {
		gotStartVirtualAddr = e.SectionHdrTable.GetHeader(elf.GOT).Saddr
	}
	textStartVirtualAddr := e.SectionHdrTable.GetHeader(elf.TEXT).Saddr
	// for static we don't need PLT, we may need GOT tho because assembly is expecting one, for PLT this makes no difference
	for _, rela := range e.RelaEntries {
		var symbolAddr uint64
		switch rela.RelocationType() {
		case elf.R_X86_64_PC32, elf.R_X86_64_PLT32:
			symbolAddr = e.Symtab.GetSymbolWithIdx(rela.SymbolIdx()).Svalue
		case elf.R_X86_64_REX_GOTPCRELX:
			if gotStartVirtualAddr == 0 {
				panic("GOT relocation without GOT section")
			}
			gotIdx := symIdxToGotIdxMapping[rela.SymbolIdx()]
			symbolAddr = gotStartVirtualAddr + uint64(elf.GOT_ENTRY_SIZE * gotIdx)
		default:
			panic("Unsupported relocationType")
		}
		relocation := int64(symbolAddr - (textStartVirtualAddr + rela.Roffset)) + rela.Raddend
		for i, b := range elf.EncodeRelocationValue(int32(relocation)) {
			e.Code[int(rela.Roffset) + i] = b
		}
	}
	e.RelaEntries = []*elf.RelaEntry{}
	return nil
}

func (l *Linker) updateSectionHeadersForChangedSections(e *elf.ElfFile) {
	// tracking these changes is error prone, we should scan every field (expect offset changes)
	// that could've changed and reset them even if they are the same
	// text, data, bss, rela and got is assumed not to be changed (or fixed explicitly elsewhere)
	sectionSizeDeltas := make([]int, e.SectionHdrTable.NumberOfSections())
	for i := 0; i < len(sectionSizeDeltas); i++ {
		sectionSizeDeltas[i] = 0
	}
	symtabHdr := e.SectionHdrTable.GetHeader(elf.SYMTAB)
	sectionSizeDeltas[e.SectionHdrTable.GetSectionIdx(elf.SYMTAB)] = e.Symtab.BinarySize() - int(symtabHdr.Ssize)
	symtabHdr.Sinfo = e.Symtab.GetGreatestLocalSymbolId() + 1
	symtabHdr.Slink = uint32(e.SectionHdrTable.GetSectionIdx(elf.STRTAB))

	strtabHdr := e.SectionHdrTable.GetHeader(elf.STRTAB)
	sectionSizeDeltas[e.SectionHdrTable.GetSectionIdx(elf.STRTAB)] = e.Strtab.GetSize() - int(strtabHdr.Ssize)

	sectionStrtabHdr := e.SectionHdrTable.GetHeader(elf.SECTION_STRTAB)
	sectionSizeDeltas[e.SectionHdrTable.GetSectionIdx(elf.SECTION_STRTAB)] = e.SectionStrtab.GetSize() - int(sectionStrtabHdr.Ssize)

	offsetDeltas := make([]int, len(sectionSizeDeltas))
	sizeDeltaCumSum := 0
	for i := 0; i < len(sectionSizeDeltas); i++ {
		offsetDeltas[i] = sizeDeltaCumSum
		sizeDeltaCumSum += sectionSizeDeltas[i]
		hdr := e.SectionHdrTable.GetHeaderByIdx(uint16(i))
		hdr.Ssize = uint64(int(hdr.Ssize) + sectionSizeDeltas[i])
	}

	for i := 0; i < len(offsetDeltas); i++ {
		hdr := e.SectionHdrTable.GetHeaderByIdx(uint16(i))
		hdr.Soffset = uint64(int(hdr.Soffset) + offsetDeltas[i])
	}
}

func (l *Linker) StaticLinkRelocatablesIntoRelocatable(objFilePaths []string, resultPath string) error {	
	initialElfFile, err := elf.NewDeserializer().Deserialize(objFilePaths[0])
	if err != nil {
		return err
	}
	combiner := newCombiner(initialElfFile)
	for _, filePath := range objFilePaths[1:] {
		e, err := elf.NewDeserializer().Deserialize(filePath)
		if err != nil {
			return err
		}
		if err := combiner.CombineWith(e); err != nil {
			return err
		}
	}
	return elf.NewSerializer().Serialize(combiner.GetCombined(), resultPath, 0644)
}

func (l *Linker) CreateExecutable(objFilePath string, resultPath string, entrySymbolName string) error {
	e, err := elf.NewDeserializer().Deserialize(objFilePath)
	if err != nil {
		return err
	}
	if l.requiresGOT(e) {
		l.replaceRelaTextSectionWithGOT(e)
	} else {
		l.removeRelaTextSection(e)
	}

	l.createProgramHeaders(e)
	l.setSymbolValues(e)

	var symIdxToGotIdxMapping map[uint32]int
	if l.requiresGOT(e) {
		symIdxToGotIdxMapping = l.fillGOTSectionAndGetSymbolMapping(e)
	}
	if err := l.relocateStaticallyLinkedExecutable(e, symIdxToGotIdxMapping); err != nil {
		return err
	}
	l.updateSectionHeadersForChangedSections(e)
	l.fixProgramHeaderForStaticallyLinkedExecutable(e, entrySymbolName)

	return elf.NewSerializer().Serialize(e, resultPath, 0744)
}
