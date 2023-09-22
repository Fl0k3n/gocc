package linkers

import (
	"elf"
	"errors"
	"math"
	"utils"
)

type LinkageHelper struct {

}

func newHelper() *LinkageHelper {
	return &LinkageHelper{}
}

func (l *LinkageHelper) getIdxsOfRelocationSymbolsWithType(e *elf.ElfFile, relocationType elf.RelocationType) []uint32 {
	res := utils.NewSet[uint32]()
	for _, rela := range e.RelaTextEntries {
		if rela.RelocationType() == relocationType {
			res.Add(rela.SymbolIdx())
		}
	}
	return res.GetAll()
}

// goal is to minimize padding from lastSegmentFileOffsetEnd to nextSegmentFileOffsetStart
// relativeAlignment must be a multiple of alignment, this should be guaranteed by the elf spec
func (l *LinkageHelper) GetNextSegmentPhysicalAndVirtualOffsets(
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


// implicitly fixes Saddr and Soffset of section headers, section headers MUST have correct sizes
func (l *LinkageHelper) joinSectionsToSegment(
	e *elf.ElfFile,
	hdr *elf.ProgramHeader,
	startSectionIdx uint16,
	endSectionIdx uint16, // inclusive
) {
	prevEndVirtAddr := hdr.Pvaddr + hdr.Pmemsz
	prevEndPhyAddr := hdr.Poffset + hdr.Pfilesz

	for sectionIdx := startSectionIdx; sectionIdx <= endSectionIdx; sectionIdx++ {
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
		if sectionHdr.Stype != uint32(elf.S_NOBITS) {
			hdr.Pfilesz += sectionHdr.Ssize + prevSectionAlignment
			prevEndPhyAddr = sectionStartPhyAddr + sectionHdr.Ssize
		} else {
			hdr.Pfilesz += prevSectionAlignment
			prevEndPhyAddr += prevSectionAlignment
		}
		prevEndVirtAddr = sectionStartVirtAddr + sectionHdr.Ssize
	}
}

// implicitly fixes Saddr and Soffset of section headers, section headers MUST have correct sizes
func (l *LinkageHelper) createLoadableSegmentFromContiguousSections(
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
	firstVirtAddr, firstFileOffset := l.GetNextSegmentPhysicalAndVirtualOffsets(
		prevSectionEndVirtualAddr, prevSectionEndFileOffset, firstSectionHdr.Saddralign, PAGE_SIZE)
	hdr.Poffset = firstFileOffset
	hdr.Pvaddr = firstVirtAddr
	hdr.Ppaddr = firstVirtAddr
	hdr.Pmemsz = firstSectionHdr.Ssize
	if firstSectionHdr.Stype != uint32(elf.S_NOBITS) {
		hdr.Pfilesz = hdr.Pmemsz
	} else {
		hdr.Pfilesz = 0	
	}
	firstSectionHdr.Saddr = firstVirtAddr
	firstSectionHdr.Soffset = firstFileOffset
	l.joinSectionsToSegment(e, &hdr, startSectionIdx + 1, endSectionIdx)
	return &hdr
}

func (l *LinkageHelper) createSegmentContainingElfHeaderAndProgramHeaders(programHeaderCount int) *elf.ProgramHeader {
	phtSize := programHeaderCount * elf.PROGRAM_HEADER_SIZE
	totalSize := uint64(elf.ELF_HEADER_SIZE + phtSize)
	return &elf.ProgramHeader{
		Ptype: uint32(elf.PT_LOAD),
		Pflags: elf.PF_R,
		Palign: PAGE_SIZE,
		Poffset: 0,
		Pvaddr: 0,
		Ppaddr: 0,
		Pfilesz: totalSize,
		Pmemsz: totalSize,
	}
}

func (l *LinkageHelper) createPhdrProgramHeader(size uint64) *elf.ProgramHeader {
	return &elf.ProgramHeader{
		Ptype: uint32(elf.PT_PHDR),
		Pflags: elf.PF_R,
		Palign: 8,
		Poffset: 0,
		Pvaddr: 0,
		Ppaddr: 0,
		Pfilesz: size,
		Pmemsz: size,
	}
}

func (l *LinkageHelper) createInterpProgramHeader(size uint64) *elf.ProgramHeader {
	return &elf.ProgramHeader{
		Ptype: uint32(elf.PT_INTERP),
		Pflags: elf.PF_R,
		Palign: 1,
		Poffset: 0,
		Pvaddr: 0,
		Ppaddr: 0,
		Pfilesz: size,
		Pmemsz: size,
	}
}

func (l *LinkageHelper) updateFileOffsetsOfSectionsBetween(
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

func (l *LinkageHelper) assertAdjacentOrMissing(e *elf.ElfFile, sectionNames ...string) (minIdx uint16, maxIdx uint16, allMissing bool) {
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

func (l *LinkageHelper) GetIdxsOfSymbolsToBePlacedInGot(e *elf.ElfFile) []uint32 {
	return l.getIdxsOfRelocationSymbolsWithType(e, elf.R_X86_64_REX_GOTPCRELX)
}

func (l *LinkageHelper) GetIdxsOfFunctionSymbolsToBePlacedInPlt(e *elf.ElfFile) []uint32 {
	res := []uint32{}
	for _, symidx := range l.getIdxsOfRelocationSymbolsWithType(e, elf.R_X86_64_PLT32) {
		if !e.Symtab.IsDefined(symidx) {
			res = append(res, symidx)
		}
	}
	return res
}

func (l *LinkageHelper) FixSectionInterlinks(e *elf.ElfFile) {
	if dynamicHdr, ok := e.SectionHdrTable.MaybeHeader(elf.DYNAMIC); ok {
		dynamicHdr.Slink = uint32(e.SectionHdrTable.GetSectionIdx(elf.DYNSTR))
	}
	if hashHdr, ok := e.SectionHdrTable.MaybeHeader(elf.HASH_SECTION); ok {
		hashHdr.Slink = uint32(e.SectionHdrTable.GetSectionIdx(elf.DYNSYM))
	}
	if relaTextHdr, ok := e.SectionHdrTable.MaybeHeader(elf.RELA_TEXT); ok {
		relaTextHdr.Slink = uint32(e.SectionHdrTable.GetSectionIdx(elf.SYMTAB))
		relaTextHdr.Sinfo = uint32(e.SectionHdrTable.GetSectionIdx(elf.TEXT))
	}
	if relaDynHdr, ok := e.SectionHdrTable.MaybeHeader(elf.RELA_DYN); ok {
		relaDynHdr.Slink = uint32(e.SectionHdrTable.GetSectionIdx(elf.DYNSYM))
	}
	if relaPltHdr, ok := e.SectionHdrTable.MaybeHeader(elf.RELA_PLT); ok {
		relaPltHdr.Slink = uint32(e.SectionHdrTable.GetSectionIdx(elf.DYNSYM))
		relaPltHdr.Sinfo = uint32(e.SectionHdrTable.GetSectionIdx(elf.GOT_PLT))
	}
	if symtabHdr, ok := e.SectionHdrTable.MaybeHeader(elf.SYMTAB); ok {
		symtabHdr.Slink = uint32(e.SectionHdrTable.GetSectionIdx(elf.STRTAB))
		symtabHdr.Sinfo = e.Symtab.GetGreatestLocalSymbolId() + 1
	}
	if dynSymHdr, ok := e.SectionHdrTable.MaybeHeader(elf.DYNSYM); ok {
		dynSymHdr.Slink = uint32(e.SectionHdrTable.GetSectionIdx(elf.DYNSTR))
		dynSymHdr.Sinfo = e.DynSymtab.GetGreatestLocalSymbolId() + 1
	}
}

func (l *LinkageHelper) SetSectionSizes(e *elf.ElfFile) {
	sht := e.SectionHdrTable
	setSizeIfExists := func(sectionName string, sizeProvider func() int) {
		if hdr, ok := sht.MaybeHeader(sectionName); ok {
			hdr.Ssize = uint64(sizeProvider())
		}
	}
	sht.GetHeader(elf.NULL_SECTION).Ssize = 0
	setSizeIfExists(elf.HASH_SECTION,   func() int {return e.SymbolHashTab.BinarySize()})
	setSizeIfExists(elf.DYNSYM, 	    func() int {return e.DynSymtab.BinarySize()})
	setSizeIfExists(elf.DYNSTR,		    func() int {return e.DynStrtab.GetSize()})
	setSizeIfExists(elf.RELA_DYN, 	    func() int {return len(e.RelaDynEntries) * elf.RELA_ENTRY_SIZE})
	setSizeIfExists(elf.RELA_PLT,	    func() int {return len(e.RelaPltEntries) * elf.RELA_ENTRY_SIZE})
	setSizeIfExists(elf.PLT, 		    func() int {return len(e.PLT) * elf.PLT_ENTRY_SIZE})
	setSizeIfExists(elf.TEXT,		    func() int {return len(e.Code)})
	setSizeIfExists(elf.INTERP,         func() int {return len([]byte(e.Interp)) + 1})
	setSizeIfExists(elf.DYNAMIC,	    func() int {return e.Dynamic.BinarySize()})
	setSizeIfExists(elf.GOT,		    func() int {return len(e.GOT) * elf.GOT_ENTRY_SIZE})
	setSizeIfExists(elf.GOT_PLT,	    func() int {return len(e.GOT_PLT) * elf.GOT_ENTRY_SIZE})
	setSizeIfExists(elf.DATA,		    func() int {return len(e.Data)})
	setSizeIfExists(elf.RO_DATA, 	    func() int {return len(e.Rodata.Data)})
	setSizeIfExists(elf.SYMTAB, 	    func() int {return e.Symtab.BinarySize()})
	setSizeIfExists(elf.STRTAB, 	    func() int {return e.Strtab.GetSize()})
	setSizeIfExists(elf.SECTION_STRTAB, func() int {return e.SectionStrtab.GetSize()})
}

func (l *LinkageHelper) HighestExistingSectionIdx(e *elf.ElfFile, sections ...string) uint16 {
	maxIdx := uint16(0)
	for _, secName := range sections {
		if e.SectionHdrTable.HasSection(secName) {
			idx := e.SectionHdrTable.GetSectionIdx(secName)
			if idx > maxIdx {
				maxIdx = idx
			}
		}
	}
	return maxIdx
}

func (l *LinkageHelper) SetSymbolValues(e *elf.ElfFile, symtab *elf.Symtab) {
	for _, sym := range symtab.GetAll() {
		if sym.Sshndx == elf.SHN_UNDEF {
			continue
		}
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

func (l *LinkageHelper) AddGOTReservedSymbol(e *elf.ElfFile, gotSectionIdx uint16, symVal uint64, binding elf.SymbolBinding) *elf.Symbol {
	sym := elf.Symbol{
		Sname: e.Strtab.PutString(elf.GOT_SYMBOL_NAME),
		Sinfo: elf.EncodeSymbolInfo(binding, elf.ST_OBJECT),
		Sother: elf.RESERVED_SYMBOL_OTHER_FIELD,
		Sshndx: gotSectionIdx,
		Svalue: symVal,
		Ssize: 0,
	}
	e.Symtab.AddSymbol(&sym)
	return &sym
}

func (l *LinkageHelper) AddDynamicReservedSymbol(e *elf.ElfFile, dynamicSectionIdx uint16, symVal uint64) *elf.Symbol {
	sym := elf.Symbol{
		Sname: e.Strtab.PutString(elf.DYNAMIC_SYMBOL_NAME),
		Sinfo: elf.EncodeSymbolInfo(elf.SB_LOCAL, elf.ST_OBJECT),
		Sother: elf.RESERVED_SYMBOL_OTHER_FIELD,
		Sshndx: dynamicSectionIdx,
		Svalue: symVal,
		Ssize: 0,
	}
	e.Symtab.AddSymbol(&sym)
	return &sym
}

func (l *LinkageHelper) ReorderSymbolsToHaveLocalsFirst(e *elf.ElfFile) (reorderLut []uint32) {
	reorderLut = e.Symtab.ReorderSymbolsToHaveLocalsFirst()
	if e.RelaTextEntries != nil {
		for _, rela := range e.RelaTextEntries {
			rela.Rinfo = elf.EncodeRelocationInfo(reorderLut[rela.SymbolIdx()], rela.RelocationType()) 
		}
	}
	return
}

func (l *LinkageHelper) GetNamesOfSymbolsWithIdxs(e *elf.ElfFile, symIdxs []uint32) []string {
	res := make([]string, len(symIdxs))
	for i, idx := range symIdxs {
		res[i] = e.Strtab.GetStringForIndex(e.Symtab.GetSymbolWithIdx(idx).Sname)
	}
	return res
}

func (l *LinkageHelper) UpdateHeaderForElfWithProgramHeaders(e *elf.ElfFile) {
	hdr := e.Header
	hdr.Ephentsize = elf.PROGRAM_HEADER_SIZE
	hdr.Ephnum = uint16(e.ProgramHdrTable.Size())
	hdr.Ephoff = elf.ELF_HEADER_SIZE
	lastSection := e.SectionHdrTable.GetSectionWithHighestFileOffset()
	hdr.Eshoff = lastSection.Soffset + lastSection.Ssize
	hdr.Eshnum = uint16(len(e.SectionHdrTable.GetSectionHeaders()))
	hdr.Eshstrndx = e.SectionHdrTable.GetSectionIdx(elf.SECTION_STRTAB)
}

func (l *LinkageHelper) SetEntryPoint(e *elf.ElfFile, entrySymbolName string) error {
	entrySym, ok := e.Symtab.TryGetSymbol(entrySymbolName, e.Strtab)
	if !ok {
		return errors.New("Undefined entry symbol: " + entrySymbolName)
	}
	e.Header.Eentry = entrySym.Svalue
	return nil
}
