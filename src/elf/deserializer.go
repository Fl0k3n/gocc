package elf

import (
	"errors"
	"os"
	"utils"
)

type Deserializer struct {
	elfFile *ElfFile	
	data []byte
}

func NewDeserializer() *Deserializer {
	return &Deserializer{}
}

func (d *Deserializer) hasValidMagic(hdr *Header) (ok bool) {
	// TODO dont hardcode
	return hdr.Eident[0] == 0x7F && hdr.Eident[1] == uint8('E') && hdr.Eident[2] == uint8('L') && hdr.Eident[3] == uint8('F')
}

func (d *Deserializer) deserializeHeader() error {
	if len(d.data) < ELF_HEADER_SIZE {
		return errors.New("Invalid ELF file")
	}
	hdr := HeaderFromBytes(d.data, 0)
	if !d.hasValidMagic(hdr) {
		return errors.New("File is not an ELF file")
	}
	if hdr.Eshentsize != SECTION_HEADER_SIZE {
		return errors.New("Unsupported section headers")
	}
	if len(d.data) < int(hdr.Eshoff) +  int(hdr.Eshentsize * hdr.Eshnum) {
		return errors.New("Invalid ELF file")
	}
	d.elfFile.Header = hdr
	return nil
}


func (d *Deserializer) deserializeSectionHeaderTable() error {
	sectionHeaders := make([]SectionHeader, d.elfFile.Header.Eshnum)
	sectionHeadersOffset := d.elfFile.Header.Eshoff
	for i := 0; i < int(d.elfFile.Header.Eshnum); i++ {
		sectionHeaders[i] = *SectionHeaderFromBytes(d.data, int(sectionHeadersOffset) + i * int(d.elfFile.Header.Eshentsize))
	}
	if d.elfFile.Header.Eshstrndx >= uint16(len(sectionHeaders)) {
		return errors.New("Invalid section strtab index")
	}
	sectionStrtabHdr := sectionHeaders[d.elfFile.Header.Eshstrndx]
	sectionStrtab := newStrtabFromBytes(d.data, uint32(sectionStrtabHdr.Soffset), uint32(sectionStrtabHdr.Ssize))
	sectionIndexes := map[string]uint16{}
	for i, shdr := range sectionHeaders {
		sectionIndexes[sectionStrtab.GetStringForIndex(shdr.Sname)] = uint16(i)
	}
	d.elfFile.SectionHdrTable = &SectionHdrTable{
		sectionStrtab: sectionStrtab,
		sectionHeaders: sectionHeaders,
		sectionIndexes: sectionIndexes,
	}
	d.elfFile.SectionStrtab = sectionStrtab
	return nil
}

func (d *Deserializer) deserializeCodeAndData() error {
	textHdr := d.elfFile.SectionHdrTable.GetHeader(TEXT)
	d.elfFile.Code = d.data[textHdr.Soffset:textHdr.Soffset+textHdr.Ssize]
	if d.elfFile.SectionHdrTable.HasSection(DATA) {
		dataHdr := d.elfFile.SectionHdrTable.GetHeader(DATA)
		d.elfFile.Data = d.data[dataHdr.Soffset:dataHdr.Soffset+dataHdr.Ssize]
	} else {
		d.elfFile.Data = []byte{}
	}
	return nil
}

func (d *Deserializer) deserializeSymtab() error {
	symtabHdr := d.elfFile.SectionHdrTable.GetHeader(SYMTAB)
	symbols := make([]*Symbol, symtabHdr.Ssize / symtabHdr.Sentsize)
	offset := symtabHdr.Soffset
	for i := 0; i < len(symbols); i++ {
		symbols[i] = SymbolFromBytes(d.data, int(offset))
		offset += SYMBOL_SIZE
	}
	d.elfFile.Symtab = NewSymtabWith(symbols)
	return nil
}

func (d *Deserializer) deserializeStrtab() error {
	strtabHdr := d.elfFile.SectionHdrTable.GetHeader(STRTAB)
	d.elfFile.Strtab = newStrtabFromBytes(d.data, uint32(strtabHdr.Soffset), uint32(strtabHdr.Ssize))
	return nil
}

func (d *Deserializer) deserializeRelaTab() error {
	relaTextHdr := d.elfFile.SectionHdrTable.GetHeader(RELA_TEXT)
	relaEntries := make([]RelaEntry, relaTextHdr.Ssize / relaTextHdr.Sentsize)
	offset := relaTextHdr.Soffset
	for i := 0; i < len(relaEntries); i++ {
		relaEntries[i] = *RelaEntryFromBytes(d.data, int(offset))
		offset += RELA_ENTRY_SIZE
	}
	d.elfFile.RelaEntries = relaEntries
	return nil

}

func (d *Deserializer) Deserialize(inputPath string) (*ElfFile, error) {
	d.elfFile = &ElfFile{}
	data, err := os.ReadFile(inputPath) // TODO don't read entire code + data to memory
	if err != nil {
		return nil, err
	}
	d.data = data

	err = utils.Pipeline().
		Then(d.deserializeHeader).
		Then(d.deserializeSectionHeaderTable).
		Then(d.deserializeCodeAndData).
		Then(d.deserializeSymtab).
		Then(d.deserializeStrtab). 
		Then(d.deserializeRelaTab).
		Error()

	return d.elfFile, err
}
