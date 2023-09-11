package elf

type ProgramHdrTable struct {
	programHeaders []ProgramHeader
}

func NewProgramHdrTable() *ProgramHdrTable {
	return &ProgramHdrTable{programHeaders: []ProgramHeader{}}
}

func (p *ProgramHdrTable) AddProgramHeader(hdr ProgramHeader) {
	p.programHeaders = append(p.programHeaders, hdr)
}
