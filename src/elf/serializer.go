package elf

import (
	"bufio"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"utils"
)

type SectionSerializer = func (*ElfFile) []byte

type SectionName = string

type ELFSerializer struct {
	writer *bufio.Writer
	sectionSerializers map[SectionName]SectionSerializer
}

func gotToBytes(GOT []uint64) []byte {
	res := make([]byte, len(GOT) * GOT_ENTRY_SIZE)
	offset := 0	
	for _, gotEntry := range GOT {
		for i, b := range utils.EncodeUnsignedIntToLittleEndianU2(gotEntry) {
			res[offset + i] = b
		}
		offset += GOT_ENTRY_SIZE
	}
	return res
}

func relaToBytes(relaEntries []*RelaEntry) []byte {
	res := make([]byte, len(relaEntries) * RELA_ENTRY_SIZE)
	offset := 0
	for _, entry := range relaEntries {
		for i, b := range entry.ToBytes() {
			res[offset + i] = b
		}
		offset += RELA_ENTRY_SIZE
	}
	return res
}

func pltToBytes(entries []PLTEntry) []byte {
	res := make([]byte, len(entries) * PLT_ENTRY_SIZE)
	offset := 0
	for _, entry := range entries {
		copy(res[offset:], entry.Code[:])
		offset += PLT_ENTRY_SIZE
	}
	return res
}

func NewSerializer() *ELFSerializer {
	return &ELFSerializer{
		sectionSerializers: map[string]func(*ElfFile) []byte{
			TEXT:   		func(e *ElfFile) []byte {return e.Code },
			DATA:   		func(e *ElfFile) []byte {return e.Data },
			BSS:    		func(e *ElfFile) []byte {return []byte{} },
			RO_DATA:   		func(e *ElfFile) []byte {return e.Rodata.Data },
			SYMTAB: 		func(e *ElfFile) []byte {return e.Symtab.ToBytes() },
			STRTAB: 		func(e *ElfFile) []byte {return e.Strtab.GetNullCombinedStrings() },
			SECTION_STRTAB: func(e *ElfFile) []byte {return e.SectionHdrTable.GetSectionStrtab().GetNullCombinedStrings() },
			RELA_TEXT: 		func(e *ElfFile) []byte {return relaToBytes(e.RelaTextEntries)},
			GOT: 			func(e *ElfFile) []byte {return gotToBytes(e.GOT)},
			HASH_SECTION:   func(e *ElfFile) []byte {return e.SymbolHashTab.ToBytes()},
			DYNAMIC: 	    func(e *ElfFile) []byte {return e.Dynamic.ToBytes()},
			DYNSYM:         func(e *ElfFile) []byte {return e.DynSymtab.ToBytes()},
			DYNSTR:		    func(e *ElfFile) []byte {return e.DynStrtab.GetNullCombinedStrings()},
			PLT:		    func(e *ElfFile) []byte {return pltToBytes(e.PLT)},
			GOT_PLT:        func(e *ElfFile) []byte {return gotToBytes(e.GOT_PLT)},
			RELA_DYN:       func(e *ElfFile) []byte {return relaToBytes(e.RelaDynEntries)},
			RELA_PLT:       func(e *ElfFile) []byte {return relaToBytes(e.RelaPltEntries)},
			INTERP:         func(e *ElfFile) []byte {return append([]byte(e.Interp), 0)},
		},
	}
}

func (e *ELFSerializer) write(bytes []byte) {
	_, err := e.writer.Write(bytes)
	if err != nil {
		panic(err)
	}
}

func (e *ELFSerializer) writePadding(size uint64) {
	padding := make([]byte, size)
	e.write(padding)
}

func (e *ELFSerializer) serializeSections(elf *ElfFile, currentOffset uint64) (uint64, error) {
	for shIdx, shdr := range elf.SectionHdrTable.GetSectionHeaders() {
		if shdr.Stype == S_NULL {
			continue
		}
		sectionName := elf.SectionHdrTable.GetSectionName(uint16(shIdx))
		if shdr.Soffset != currentOffset {
			if currentOffset > shdr.Soffset {
				return 0, errors.New(fmt.Sprintf("Invalid section offset for section %s", sectionName))
			}
			e.writePadding(shdr.Soffset - currentOffset)
			currentOffset = shdr.Soffset
		}
		serializer, hasSerializer := e.sectionSerializers[sectionName];
		if !hasSerializer && shdr.Stype != S_NOBITS {
			return 0, errors.New("Missing serializer for section " + sectionName)
		}
		serializedSection := serializer(elf)
		e.write(serializedSection)
		currentOffset += uint64(len(serializedSection))
	}
	return currentOffset, nil
}

func (e *ELFSerializer) Serialize(elf *ElfFile, outputPath string, outputPerm fs.FileMode) error {
	file, err := os.OpenFile(outputPath, os.O_CREATE | os.O_WRONLY | os.O_TRUNC, outputPerm)
	if err != nil {
		return err
	}
	defer file.Close()
	e.writer = bufio.NewWriter(file)
	defer e.writer.Flush()

	e.write(elf.Header.ToBytes())
	var offset uint64 = ELF_HEADER_SIZE
	if elf.Header.Ephoff != 0 {
		if elf.Header.Ephoff != ELF_HEADER_SIZE {
			return errors.New("Expected program header table right after elf header")
		}
		e.write(elf.ProgramHdrTable.ToBytes())
		offset += uint64(PROGRAM_HEADER_SIZE * elf.ProgramHdrTable.Size())
	}
	offset, err = e.serializeSections(elf, offset)
	if err != nil {
		return err
	}
	if offset != elf.Header.Eshoff {
		return errors.New("Section Header table must be right after sections content")
	}
	e.write(elf.SectionHdrTable.ToBytes())

	return nil
}
