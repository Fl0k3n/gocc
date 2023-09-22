package elf

import (
	"codegen"
	"errors"
	"os"
	"utils"
)

// TODO refactor this similar to serializer

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
	d.elfFile.Code = make([]uint8, textHdr.Ssize)
	copy(d.elfFile.Code, d.data[textHdr.Soffset:textHdr.Soffset+textHdr.Ssize])
	if d.elfFile.SectionHdrTable.HasSection(DATA) {
		dataHdr := d.elfFile.SectionHdrTable.GetHeader(DATA)
		d.elfFile.Data = make([]uint8, dataHdr.Ssize)
		copy(d.elfFile.Data, d.data[dataHdr.Soffset:dataHdr.Soffset+dataHdr.Ssize])
	} else {
		d.elfFile.Data = []byte{}
	}
	return nil
}

func (d *Deserializer) deserializeRodata() error {
	if d.elfFile.SectionHdrTable.HasSection(RO_DATA) {
		rodataHdr := d.elfFile.SectionHdrTable.GetHeader(RO_DATA)
		buff := make([]uint8, rodataHdr.Ssize)
		copy(buff, d.data[rodataHdr.Soffset:rodataHdr.Soffset+rodataHdr.Ssize])
		d.elfFile.Rodata = codegen.Rodata{
			Data: buff,
			Alignment: uint32(rodataHdr.Saddralign),
		}
	}
	return nil
}

func (d *Deserializer) deserializeSymtab(sectionName string) (*Symtab, error) {
	symtabHdr := d.elfFile.SectionHdrTable.GetHeader(sectionName)
	symbols := make([]*Symbol, symtabHdr.Ssize / SYMBOL_SIZE)
	offset := symtabHdr.Soffset
	for i := 0; i < len(symbols); i++ {
		symbols[i] = SymbolFromBytes(d.data, int(offset))
		offset += SYMBOL_SIZE
	}
	return NewSymtabWith(symbols), nil
}

func (d *Deserializer) deserializeMainSymtab() error {
	if !d.elfFile.SectionHdrTable.HasSection(SYMTAB) {
		return nil
	}
	st, err := d.deserializeSymtab(SYMTAB)
	d.elfFile.Symtab = st
	return err
}

func (d *Deserializer) deserializeDynSymtab() error {
	st, err := d.deserializeSymtab(DYNSYM)
	d.elfFile.DynSymtab = st
	return err
}

func (d *Deserializer) deserializeStrtab() error {
	if !d.elfFile.SectionHdrTable.HasSection(STRTAB) {
		return nil
	}
	strtabHdr := d.elfFile.SectionHdrTable.GetHeader(STRTAB)
	d.elfFile.Strtab = newStrtabFromBytes(d.data, uint32(strtabHdr.Soffset), uint32(strtabHdr.Ssize))
	return nil
}

func (d *Deserializer) deserializeDynstr() error {
	strtabHdr := d.elfFile.SectionHdrTable.GetHeader(DYNSTR)
	d.elfFile.DynStrtab = newStrtabFromBytes(d.data, uint32(strtabHdr.Soffset), uint32(strtabHdr.Ssize))
	return nil
}

func (d *Deserializer) deserializeRelaTab() error {
	if !d.elfFile.SectionHdrTable.HasSection(RELA_TEXT) {
		return nil
	}
	relaTextHdr := d.elfFile.SectionHdrTable.GetHeader(RELA_TEXT)
	relaEntries := make([]*RelaEntry, relaTextHdr.Ssize / relaTextHdr.Sentsize)
	offset := relaTextHdr.Soffset
	for i := 0; i < len(relaEntries); i++ {
		relaEntries[i] = RelaEntryFromBytes(d.data, int(offset))
		offset += RELA_ENTRY_SIZE
	}
	d.elfFile.RelaTextEntries = relaEntries
	return nil
}

func (d *Deserializer) deserializeDynamicTab() error {
	dyn := NewDynamicTab()
	dynamicHdr := d.elfFile.SectionHdrTable.GetHeader(DYNAMIC)

	size := int(dynamicHdr.Ssize / DYNAMIC_ENTRY_SIZE)
	dyn.Alloc(size)

	offset := int(dynamicHdr.Soffset)
	for i := 0; i < size; i++ {
 		dyn.Set(i, *DynamicEntryFromBytes(d.data, offset))
		offset += DYNAMIC_ENTRY_SIZE
	}
	return nil
}

func (d *Deserializer) deserializeHashTab() error {
	hashHdr := d.elfFile.SectionHdrTable.GetHeader(HASH_SECTION)
	var nbuckets uint32
	var nchains uint32
	utils.DecodeUnsignedIntsFromLittleEndianU2(d.data, int(hashHdr.Soffset), &nbuckets, &nchains)
	offset := int(hashHdr.Soffset + 8)
	buckets := make([]uint32, nbuckets)
	chains := make([]uint32, nchains)
	for i := 0; i < int(nbuckets); i++ {
		utils.DecodeUnsignedIntFromLittleEndianU2(d.data, offset, &buckets[i])
		offset += 4
	}
	for i := 0; i < int(nchains); i++ {
		utils.DecodeUnsignedIntFromLittleEndianU2(d.data, offset, &chains[i])
		offset += 4
	}
	d.elfFile.SymbolHashTab = NewSymbolHashTab(buckets, chains)
	return nil
}

func (d *Deserializer) DeserializeDynamicInfo(path string) (*DynamicTab, *Symtab, *SymbolHashTab, *Strtab, error) {
	d.elfFile = &ElfFile{}
	var err error
	data, er := os.ReadFile(path) // TODO don't read entire code + data to memory
	if er != nil {
		err = er
		goto done
	}
	d.data = data
	err = utils.Pipeline().
		Then(d.deserializeHeader).
		Then(d.deserializeSectionHeaderTable).
		Then(d.deserializeDynamicTab).
		Then(d.deserializeDynSymtab).
		Then(d.deserializeDynstr).
		Then(d.deserializeHashTab).
		Error()
done:
	d.data = nil
	if err != nil {
		return nil, nil, nil, nil, err
	}
	return d.elfFile.Dynamic, d.elfFile.DynSymtab, d.elfFile.SymbolHashTab, d.elfFile.DynStrtab, nil
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
		Then(d.deserializeRodata).
		Then(d.deserializeMainSymtab).
		Then(d.deserializeStrtab). 
		Then(d.deserializeRelaTab).
		Error()

	d.data = nil // dealloc
	return d.elfFile, err
}
