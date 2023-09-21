package linkers

import (
	"elf"
	"errors"
)

type Combiner struct {
	elfBuff *elf.ElfFile
	symbolMap map[string][]uint32
	helper *LinkageHelper
}

func newCombiner(initialElfFile *elf.ElfFile) *Combiner {
	sectionHeaders := map[string]*elf.SectionHeader{}
	for idx, hdr := range initialElfFile.SectionHdrTable.GetSectionHeaders() {
		sectionHeaders[initialElfFile.SectionHdrTable.GetSectionName(uint16(idx))] = &hdr
	}
	symbolMap := map[string][]uint32{}
	for idx, sym := range initialElfFile.Symtab.GetAll() {
		symName := initialElfFile.Strtab.GetStringForIndex(sym.Sname)
		symbolMap[symName] = []uint32{uint32(idx)}
	}
	return &Combiner{
		elfBuff: initialElfFile,
		symbolMap: symbolMap,
		helper: newHelper(),
	}
}

func (c *Combiner) combineTextSection(e *elf.ElfFile) {
	newTextHdr := e.SectionHdrTable.GetHeader(elf.TEXT)
	curTextHdr := c.elfBuff.SectionHdrTable.GetHeader(elf.TEXT)
	c.shiftOffsetsForSymbolsInSection(e, e.SectionHdrTable.GetSectionIdx(elf.TEXT) , int(curTextHdr.Ssize))
	c.elfBuff.Code = append(c.elfBuff.Code, e.Code...)
	if e.RelaTextEntries != nil {
		for _, relaEntry := range e.RelaTextEntries {
			relaEntry.Roffset += curTextHdr.Ssize
		}
	}
	curTextHdr.Ssize += newTextHdr.Ssize
}

func (c *Combiner) combineRelaTextSection(e *elf.ElfFile) {
	newRelaTextHdr := e.SectionHdrTable.GetHeader(elf.RELA_TEXT)
	curRelaTextHdr := c.elfBuff.SectionHdrTable.GetHeader(elf.RELA_TEXT)
	curRelaTextHdr.Ssize += newRelaTextHdr.Ssize
	c.elfBuff.RelaTextEntries = append(c.elfBuff.RelaTextEntries, e.RelaTextEntries...)
}

func (c *Combiner) combineDataSection(e *elf.ElfFile) {
	newDataHdr := e.SectionHdrTable.GetHeader(elf.DATA)
	if c.elfBuff.SectionHdrTable.HasSection(elf.DATA) {
		curDataHdr := c.elfBuff.SectionHdrTable.GetHeader(elf.DATA)
		if remainder := curDataHdr.Ssize % newDataHdr.Saddralign; remainder != 0 {
			paddingSize := newDataHdr.Saddralign - remainder
			curDataHdr.Ssize += paddingSize
			c.elfBuff.Data = append(c.elfBuff.Data, make([]byte, paddingSize)...)
		}
		c.elfBuff.Data = append(c.elfBuff.Data, e.Data...)	
		dataSectionIdx := e.SectionHdrTable.GetSectionIdx(elf.DATA)
		c.shiftOffsetsForSymbolsInSection(e, dataSectionIdx, int(curDataHdr.Ssize))
		dataSymbolIdx := e.Symtab.GetSymbolIdx(elf.DATA, e.Strtab)
		c.shiftSectionRelativeRelaEntries(e, dataSymbolIdx, int(curDataHdr.Ssize))
		curDataHdr.Ssize += newDataHdr.Ssize
	} else {
		c.elfBuff.SectionHdrTable.AppendSectionHeader(*newDataHdr, elf.DATA)
		c.elfBuff.Data = e.Data
	}
}

func (c *Combiner) combineRodataSection(e *elf.ElfFile) {
	newRodataHdr := e.SectionHdrTable.GetHeader(elf.RO_DATA)
	if c.elfBuff.SectionHdrTable.HasSection(elf.RO_DATA) {
		curRodataHdr := c.elfBuff.SectionHdrTable.GetHeader(elf.RO_DATA)
		if remainder := curRodataHdr.Ssize % newRodataHdr.Saddralign; remainder != 0 {
			paddingSize := newRodataHdr.Saddralign - remainder
			curRodataHdr.Ssize += paddingSize
			c.elfBuff.Rodata.Data = append(c.elfBuff.Rodata.Data, make([]byte, paddingSize)...)
		}
		c.elfBuff.Rodata.Data = append(c.elfBuff.Rodata.Data, e.Rodata.Data...)
		rodataSectionIdx := e.SectionHdrTable.GetSectionIdx(elf.RO_DATA)
		// consts probably go there too, for now this shouldn't do anything
		c.shiftOffsetsForSymbolsInSection(e, rodataSectionIdx, int(curRodataHdr.Ssize))
		rodataSymbolIdx := e.Symtab.GetSymbolIdx(elf.RO_DATA, e.Strtab)
		c.shiftSectionRelativeRelaEntries(e, rodataSymbolIdx, int(curRodataHdr.Ssize))
		curRodataHdr.Ssize += newRodataHdr.Ssize
	} else {
		c.elfBuff.SectionHdrTable.AppendSectionHeader(*newRodataHdr, elf.RO_DATA)
		c.elfBuff.Rodata = e.Rodata
	}
}


func (c *Combiner) combineBssSection(e *elf.ElfFile) {
	newBssHdr := e.SectionHdrTable.GetHeader(elf.BSS)
	if c.elfBuff.SectionHdrTable.HasSection(elf.BSS) {
		curBssHdr := c.elfBuff.SectionHdrTable.GetHeader(elf.BSS)
		if remainder := curBssHdr.Ssize % newBssHdr.Saddralign; remainder != 0 {
			curBssHdr.Ssize += newBssHdr.Saddralign - remainder
		}
		c.shiftOffsetsForSymbolsInSection(e, e.SectionHdrTable.GetSectionIdx(elf.BSS), int(curBssHdr.Ssize))
		bssSymbolIdx := e.Symtab.GetSymbolIdx(elf.BSS, e.Strtab) // TODO maybe cache this
		c.shiftSectionRelativeRelaEntries(e, bssSymbolIdx,  int(curBssHdr.Ssize))
		curBssHdr.Ssize += newBssHdr.Ssize
	} else {
		c.elfBuff.SectionHdrTable.AppendSectionHeader(*newBssHdr, elf.BSS)
	}
}

func (c *Combiner) shiftSectionRelativeRelaEntries(target *elf.ElfFile, sectionSymbolIdx uint32, delta int) {
	if target.SectionHdrTable.HasSection(elf.RELA_TEXT) {
		for _, rela := range target.RelaTextEntries {
			if rela.SymbolIdx() == sectionSymbolIdx {
				rela.Raddend += int64(delta)
			}
		}
	}
}

func (c *Combiner) shiftOffsetsForSymbolsInSection(target *elf.ElfFile, sectionIdx uint16, delta int) {
	for _, sym := range target.Symtab.GetAll() {
		if sym.Sshndx == sectionIdx {
			sym.Svalue = uint64(int64(sym.Svalue) + int64(delta))
		}
	}
}

func (c *Combiner) defineMissingSectionHeaders(e *elf.ElfFile) {
	for hdrIdx, hdr := range e.SectionHdrTable.GetSectionHeaders() {
		sectionName := e.SectionHdrTable.GetSectionName(uint16(hdrIdx))
		if !c.elfBuff.SectionHdrTable.HasSection(sectionName) {
			c.elfBuff.SectionHdrTable.AppendSectionHeader(hdr, sectionName)
		}
	}
}

func (c *Combiner) changeSymbolIdxsOfRelaEntries(targetElf *elf.ElfFile, remapLUT []uint32) {
	if targetElf.RelaTextEntries == nil {
		return
	}
	for _, rela := range targetElf.RelaTextEntries {
		rela.Rinfo = elf.EncodeRelocationInfo(remapLUT[rela.SymbolIdx()], rela.RelocationType())
	}
}

func (c *Combiner) changeSymbolSectionIdxs(targetElf *elf.ElfFile, fromIdx uint16, toIdx uint16) {
	for _, sym := range targetElf.Symtab.GetAll() {
		if sym.Sshndx == fromIdx {
			sym.Sshndx = toIdx
		}
	}
}

func (c *Combiner) overwriteSymbolMetaButKeepStrtabIdx(symbolIdx uint32, newSymbol *elf.Symbol) {
	oldSymbol := c.elfBuff.Symtab.GetSymbolWithIdx(symbolIdx)
	newSymbol.Sname = oldSymbol.Sname
	c.elfBuff.Symtab.OverwriteSymbol(symbolIdx, newSymbol)
}

func (c *Combiner) findGlobalSymbol(idxs []uint32) (*elf.Symbol, uint32) {
	for _, otherIdx := range idxs {
		otherSym := c.elfBuff.Symtab.GetSymbolWithIdx(otherIdx) 
		if otherSym.Binding() == elf.SB_GLOBAL {
			return otherSym, otherIdx
		}	
	}
	return nil, 0
}

func (c *Combiner) handleMultipleSameSymbols(
	e *elf.ElfFile,
	symIdx uint32,
	sym *elf.Symbol,
	symName string,
	sameSymbolIdxs []uint32,
) (uint32, error) {
	if sym.Type() == elf.ST_SECTION {
		sectionName := c.elfBuff.SectionHdrTable.GetSectionName(sym.Sshndx)
		return c.elfBuff.Symtab.GetSymbolIdx(sectionName, c.elfBuff.Strtab), nil
	}
	if sym.Binding() == elf.SB_LOCAL || sym.Sshndx == elf.SHN_ABS {
		newSymIdx := c.elfBuff.Symtab.AddSymbol(sym)
		c.symbolMap[symName] = append(sameSymbolIdxs, newSymIdx)
		return newSymIdx, nil
	} else {
		otherGlobalSymbol, otherGlobalSymbolIdx := c.findGlobalSymbol(sameSymbolIdxs)
		if otherGlobalSymbol == nil {
			newSymIdx := c.elfBuff.Symtab.AddSymbol(sym)
			c.symbolMap[symName] = append(sameSymbolIdxs, newSymIdx)
			return newSymIdx, nil
		} else {
			if sym.Sshndx == elf.SHN_UNDEF {
				return otherGlobalSymbolIdx, nil
			} else {
				if otherGlobalSymbol.Sshndx == elf.SHN_UNDEF {
					c.overwriteSymbolMetaButKeepStrtabIdx(otherGlobalSymbolIdx, sym)
					return otherGlobalSymbolIdx, nil
				} else {
					// TODO weak symbols
					return 0, errors.New("Multiple definitions of global symbol with name: " + symName)
				}
			}
		}
	}
}

func (c *Combiner) combineSymtabSection(e *elf.ElfFile) error {
	curSymtabHdr := e.SectionHdrTable.GetHeader(elf.SYMTAB)
	symbolRemapLUT := make([]uint32, e.Symtab.Size())
	for symIdx, sym := range e.Symtab.GetAll() {
		if symIdx == 0 {
			if sym.Type() != elf.ST_NOTYPE {
				return errors.New("expected null symbol as first symbol")
			}
			continue // skip null symbol
		}
		if sym.Sshndx != elf.SHN_ABS && sym.Sshndx != elf.SHN_UNDEF {
			sym.Sshndx = c.elfBuff.SectionHdrTable.GetSectionIdx(e.SectionHdrTable.GetSectionName(sym.Sshndx))
		}
		symName := e.Strtab.GetStringForIndex(sym.Sname)
		sym.Sname = c.elfBuff.Strtab.PutString(symName)
		var newSymbolIdx uint32
		var err error

		if sameSymbolIdxs, isDefined := c.symbolMap[symName]; isDefined {
			newSymbolIdx, err = c.handleMultipleSameSymbols(e, uint32(symIdx), sym, symName, sameSymbolIdxs)
			if err != nil {
				return err
			}
		} else {
			newSymbolIdx = c.elfBuff.Symtab.AddSymbol(sym)
			c.symbolMap[symName] = []uint32{newSymbolIdx}
		}
		symbolRemapLUT[symIdx] = newSymbolIdx
	}
	c.changeSymbolIdxsOfRelaEntries(e, symbolRemapLUT)
	curSymtabHdr.Ssize = uint64(c.elfBuff.Symtab.BinarySize())
	return nil
}

func (c *Combiner) assignIndexesToSectionHeadersInOrder(
	startingIdx uint16,
	idxMap map[string]uint16,
	sectionNames ...string,
) uint16 {
	for _, sname := range sectionNames {
		if c.elfBuff.SectionHdrTable.HasSection(sname) {
			idxMap[sname] = startingIdx
			startingIdx++
		}
	}
	return startingIdx
}

func (c *Combiner) fixFileOffsetsOfSections() uint64 {
	offset := uint64(elf.ELF_HEADER_SIZE)
	for i := uint16(0); i < c.elfBuff.SectionHdrTable.NumberOfSections(); i++ {
		hdr := c.elfBuff.SectionHdrTable.GetHeaderByIdx(i)
		hdr.Soffset = offset
		if hdr.Stype != elf.S_NOBITS {
			offset += hdr.Ssize
		}
	}
	return offset
}

func (c *Combiner) fixSectionReferences() {
	if c.elfBuff.SectionHdrTable.HasSection(elf.SECTION_STRTAB) {
		c.elfBuff.Header.Eshstrndx = c.elfBuff.SectionHdrTable.GetSectionIdx(elf.SECTION_STRTAB)
	}
	if c.elfBuff.SectionHdrTable.HasSection(elf.RELA_TEXT) {
		relaHdr := c.elfBuff.SectionHdrTable.GetHeader(elf.RELA_TEXT)
		relaHdr.Slink = uint32(c.elfBuff.SectionHdrTable.GetSectionIdx(elf.SYMTAB))
		relaHdr.Sinfo = uint32(c.elfBuff.SectionHdrTable.GetSectionIdx(elf.TEXT))
	}
	if c.elfBuff.SectionHdrTable.HasSection(elf.SYMTAB) {
		symtabHdr := c.elfBuff.SectionHdrTable.GetHeader(elf.SYMTAB)
		symtabHdr.Slink = uint32(c.elfBuff.SectionHdrTable.GetSectionIdx(elf.STRTAB))
		symtabHdr.Sinfo = c.elfBuff.Symtab.GetGreatestLocalSymbolId() + 1
	}
}

func (c *Combiner) fixHeaders() {
	if c.elfBuff.SectionHdrTable.HasSection(elf.STRTAB) {
		strtabHdr := c.elfBuff.SectionHdrTable.GetHeader(elf.STRTAB)
		strtabHdr.Ssize = uint64(c.elfBuff.Strtab.GetSize())
	}
	if c.elfBuff.SectionHdrTable.HasSection(elf.SYMTAB) {
		symtabHdr := c.elfBuff.SectionHdrTable.GetHeader(elf.SYMTAB)
		symtabHdr.Ssize = uint64(c.elfBuff.Symtab.BinarySize())
	}
	curSectionIdxs := c.elfBuff.SectionHdrTable.GetSectionIndexes()
	
	newSectionIdxs := map[string]uint16{}
	fixedSectionNames := []string{elf.NULL_SECTION, elf.TEXT, elf.RELA_TEXT, elf.DATA, elf.BSS,
		elf.RO_DATA, elf.SYMTAB, elf.STRTAB, elf.SECTION_STRTAB}
	nextIdx := c.assignIndexesToSectionHeadersInOrder(0, newSectionIdxs, fixedSectionNames...)
	for sectionName := range curSectionIdxs {
		if _, alreadyAssigned := newSectionIdxs[sectionName]; !alreadyAssigned {
			newSectionIdxs[sectionName] = nextIdx
			nextIdx++
		}
	}

	reindexMap := c.elfBuff.SectionHdrTable.Reindex(newSectionIdxs)
	if c.elfBuff.SectionHdrTable.HasSection(elf.SYMTAB) {
		for _, sym := range c.elfBuff.Symtab.GetAll() {
			if sym.Sshndx != elf.SHN_ABS && sym.Sshndx != elf.SHN_UNDEF {
				sym.Sshndx = reindexMap[sym.Sshndx]
			}
		}
	}

	offset := c.fixFileOffsetsOfSections()
	c.elfBuff.Header.Eshoff = offset
	c.elfBuff.Header.Eshnum = uint16(c.elfBuff.SectionHdrTable.NumberOfSections())
	c.elfBuff.Header.Eshstrndx = c.elfBuff.SectionHdrTable.GetSectionIdx(elf.SECTION_STRTAB)
	c.helper.ReorderSymbolsToHaveLocalsFirst(c.elfBuff)
	c.fixSectionReferences()
}

func (c *Combiner) CombineWith(e *elf.ElfFile) error {
	c.combineTextSection(e)
	if e.SectionHdrTable.HasSection(elf.DATA) {
		c.combineDataSection(e)
	}
	if e.SectionHdrTable.HasSection(elf.BSS) {
		c.combineBssSection(e)
	}
	if e.SectionHdrTable.HasSection(elf.RO_DATA) {
		c.combineRodataSection(e)
	}
	hadSymtab := e.SectionHdrTable.HasSection(elf.SYMTAB)
	hadRelaText := e.SectionHdrTable.HasSection(elf.RELA_TEXT)
	c.defineMissingSectionHeaders(e)
	if hadSymtab {
		if err := c.combineSymtabSection(e); err != nil {
			return err
		}
	} else {
		c.elfBuff.Symtab = e.Symtab
	}
	if e.SectionHdrTable.HasSection(elf.RELA_TEXT) {
		if hadRelaText {
			c.combineRelaTextSection(e)
		} else {
			c.elfBuff.RelaTextEntries = e.RelaTextEntries
		}
	}
	return nil
}

func (c *Combiner) GetCombined() *elf.ElfFile {
	c.fixHeaders()
	res := c.elfBuff
	c.elfBuff = nil
	c.symbolMap = nil
	return res
}
