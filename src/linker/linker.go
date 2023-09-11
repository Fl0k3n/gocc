package linkers

import (
	"elf"
	"fmt"
)

const PAGE_SIZE = 4096

type Linker struct {
	textVirtAddr uint64
	textFileOffset uint64
}

func New() *Linker {
	return &Linker{
		textVirtAddr: 0x400000, // must be page aligned to the offset below
		textFileOffset: 0x1000, // we will insert some padding for simplicity, this will guarantee proper text alignment
	}
}

// goal is to minimize padding from lastSegmentFileOffsetEnd to nextSegmentFileOffsetStart
// relativeAlignment must be a multiple of alignment, this should be guaranteed by elf spec
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

// assumes that either data or bss exists (or both)
func (l *Linker) createDataBssSegmentAndFixSectionMeta(
	e *elf.ElfFile,
	textEndFileOffset uint64,
	textEndVirtualAddr uint64,
) *elf.ProgramHeader {
	hdr := elf.ProgramHeader{
		Ptype: uint32(elf.PT_LOAD),
		Pflags: uint32(elf.PF_R) | uint32(elf.PF_W),
		Palign: PAGE_SIZE,
	}
	if e.SectionHdrTable.HasSection(elf.DATA) {
		dataHdr := e.SectionHdrTable.GetHeader(elf.DATA)
		dataVirtAddr, dataFileOffset := l.getNextSegmentPhysicalAndVirtualOffsets(
			textEndVirtualAddr, textEndFileOffset, dataHdr.Saddralign, PAGE_SIZE)	
		dataHdr.Soffset = dataFileOffset
		dataHdr.Saddr = dataVirtAddr
		hdr.Poffset = dataFileOffset
		hdr.Pvaddr = dataVirtAddr
		hdr.Ppaddr = dataVirtAddr
		hdr.Pfilesz = dataHdr.Ssize
		if e.SectionHdrTable.HasSection(elf.BSS) {
			bssHdr := e.SectionHdrTable.GetHeader(elf.BSS)
			dataEndVirtAddr := dataVirtAddr + dataHdr.Ssize
			bssStartVirtAddr := dataEndVirtAddr
			if remainder := bssStartVirtAddr % bssHdr.Saddralign; remainder != 0 {
				bssStartVirtAddr += bssHdr.Saddralign - remainder
			}
			bssHdr.Soffset = dataFileOffset + dataHdr.Ssize
			bssHdr.Saddr = bssStartVirtAddr
			hdr.Pmemsz = hdr.Pfilesz + (bssStartVirtAddr - dataEndVirtAddr) + bssHdr.Ssize
		} else {
			hdr.Pmemsz = hdr.Pfilesz
		}
	} else {
		bssHdr := e.SectionHdrTable.GetHeader(elf.BSS)
		bssVirtAddr, _ := l.getNextSegmentPhysicalAndVirtualOffsets(
			textEndVirtualAddr, textEndFileOffset, bssHdr.Saddralign, PAGE_SIZE)	
		hdr.Poffset = textEndFileOffset
		hdr.Pvaddr = bssVirtAddr
		hdr.Ppaddr = bssVirtAddr
		hdr.Pfilesz = 0
		bssHdr.Soffset = textEndFileOffset
		bssHdr.Saddr = bssVirtAddr
	}
	// TODO what about .data and .bss symbols
	return &hdr
}

func (l *Linker) createProgramHeaders(e *elf.ElfFile) {
	programHdrTable := elf.NewProgramHdrTable()
	textHdr := l.createTextSegmentAndFixSectionMeta(e)
	programHdrTable.AddProgramHeader(*textHdr)
	if e.SectionHdrTable.HasSection(elf.DATA) || e.SectionHdrTable.HasSection(elf.BSS) {
		dataBssHdr := l.createDataBssSegmentAndFixSectionMeta(e,
			textHdr.Poffset + textHdr.Pfilesz, l.textVirtAddr + textHdr.Pmemsz)
		programHdrTable.AddProgramHeader(*dataBssHdr)
	} 
	e.ProgramHdrTable = programHdrTable
}

// func (l *Linker) setSymbolValues(e *elf.ElfFile) {
// 	for _, sym := range e.Symtab.GetAll() {
// 		switch sym.Type() {
// 		case elf.ST_NOTYPE, elf.ST_FILE, elf.ST_FUNC:
// 			// no need to to anything, functions must already have assigned values, relative to text
// 		case elf.ST_OBJECT:
// 		case elf.ST_SECTION:
// 		default:
// 			panic("Unexpected symbol type") // TODO return errors instead of panicking
// 		}
// 	}
// }

func (l *Linker) createGOT(e *elf.ElfFile) {

}

func (l *Linker) relocateStaticallyLinkedExecutable(e *elf.ElfFile) {
	// for static linking every symbol must be defined
	if undefinedSymbols := e.Symtab.GetUndefinedSymbols(); len(undefinedSymbols) > 0 {
		panic("undefined symbols") // TODO return err
	}
	// for static we don't need PLT, we need GOT tho because assembly is expecting one, for PLT this makes no difference
	for _, rela := range e.RelaEntries {
		switch rela.RelocationType() {
		case elf.R_X86_64_PC32, elf.R_X86_64_PLT32:
		case elf.R_X86_64_REX_GOTPCRELX:
			
		default:
			panic("Unsupported relocationType")
		}
	}
}

func (l *Linker) StaticLinkRelocatablesIntoRelocatable() {

}

func (l *Linker) CreateExecutable(objFilePath string, resultPath string) error {
	elf, err := elf.NewDeserializer().Deserialize(objFilePath)
	if err != nil {
		return err
	}
	fmt.Println(elf)
	return nil
}
