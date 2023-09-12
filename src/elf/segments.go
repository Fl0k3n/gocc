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

func (p *ProgramHdrTable) Size() int {
	return len(p.programHeaders)
}

func (p *ProgramHdrTable) ToBytes() []byte {
	res := make([]byte, len(p.programHeaders) * PROGRAM_HEADER_SIZE)
	offset := 0
	for _, ph := range p.programHeaders {
		for i, b := range ph.ToBytes() {
			res[offset + i] = b
		}
		offset += PROGRAM_HEADER_SIZE
	}
	return res
}
