package elf

import (
	"bufio"
	"os"
)

const ELF_CLASS_64 = 2
const ELF_DATA_LITTLE_ENDIAN_U2 = 1
const ELF_CURRENT_VERSION = 1
const ELF_SYSTEM_V_ABI = 0

const RELOCATABLE_FILE = 1
const EXECUTABLE_FILE = 2
const SHARED_OBJECT_FILE = 3

const E_X86_64_MACHINE = 0x3E

type ELFSerializer struct {
	outputPath string
	writer *bufio.Writer
}

func NewSerializer(outputPath string) *ELFSerializer {
	return &ELFSerializer{
		outputPath: outputPath,
	}
}

func (e *ELFSerializer) write(bytes []byte) {
	_, err := e.writer.Write(bytes)
	if err != nil {
		panic(err)
	}
}

func (e *ELFSerializer) writeHeader(sectionHeadersOffset int, numberOfSectionHeaders int, sectionStrtabNum uint16) {
	hdr := Header{
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
		Eshoff: uint64(sectionHeadersOffset),
		Eflags: 0, // unused
		Eehsize: ELF_HEADER_SIZE, 
		Ephentsize: 0, // no program headers 
		Ephnum: 0, // no program headers
		Eshentsize: SECTION_HEADER_SIZE,
		Eshnum: uint16(numberOfSectionHeaders), 
		Eshstrndx: sectionStrtabNum,
	}
	e.write(hdr.ToBytes())
}

func (e *ELFSerializer) writeSymtab(symtab []*Symbol) {
	for _, symbol := range symtab {
		e.write(symbol.ToBytes())
	}
}

func (e *ELFSerializer) Serialize(
	code []uint8,
	data []uint8, // must be already aligned as required
	sectionHdrTab *SectionHdrTable,
	symtab []*Symbol,
	strtab *Strtab,
	sectionStrTab *Strtab,
) error {
	file, err := os.OpenFile(e.outputPath, os.O_CREATE | os.O_WRONLY | os.O_TRUNC, 0644)
	if err != nil {
		return err
	}
	defer file.Close()
	e.writer = bufio.NewWriter(file)
	defer e.writer.Flush()

	sections := sectionHdrTab.GetSectionHeaders()
	strtabData := strtab.GetNullCombinedStrings()
	sectionStrtabData := sectionStrTab.GetNullCombinedStrings()
	sectionsContentSize := len(code) + len(data) + len(symtab) * SYMBOL_SIZE + len(strtabData) + len(sectionStrtabData) 

	e.writeHeader(sectionsContentSize + ELF_HEADER_SIZE, len(sections), sectionHdrTab.GetSectionIdx(SECTION_STRTAB))
	e.write(code)
	e.write(data)
	e.writeSymtab(symtab)
	e.write(strtabData)
	e.write(sectionStrtabData)
	e.write(sectionHdrTab.ToBytes())
	return nil
}

