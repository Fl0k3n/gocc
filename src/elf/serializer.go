package elf

import (
	"bufio"
	"os"
)

type ELFSerializer struct {
	writer *bufio.Writer
}

func NewSerializer() *ELFSerializer {
	return &ELFSerializer{}
}

func (e *ELFSerializer) write(bytes []byte) {
	_, err := e.writer.Write(bytes)
	if err != nil {
		panic(err)
	}
}

func (e *ELFSerializer) writeRelaText(relaEntries []RelaEntry) {
	for _, entry := range relaEntries {
		e.write(entry.ToBytes())
	}
}

func (e *ELFSerializer) Serialize(elf *ElfFile, outputPath string) error {
	file, err := os.OpenFile(outputPath, os.O_CREATE | os.O_WRONLY | os.O_TRUNC, 0644)
	if err != nil {
		return err
	}
	defer file.Close()
	e.writer = bufio.NewWriter(file)
	defer e.writer.Flush()

	// TODO read section headers to obtain file offsets, write paddings if needed
	e.write(elf.Header.ToBytes())
	e.write(elf.Code)
	e.writeRelaText(elf.RelaEntries)
	e.write(elf.Data)
	e.write(elf.Symtab.ToBytes())
	e.write(elf.Strtab.GetNullCombinedStrings())
	e.write(elf.SectionHdrTable.sectionStrtab.GetNullCombinedStrings())
	e.write(elf.SectionHdrTable.ToBytes())
	return nil
}
