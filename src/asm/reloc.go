package asm

import "irs"

type CodeRelocationType int

// we use small code mode as defined in sys V ABI so all addressing can use 32bit offsets
// hence relocation types from ELF spec can be significantly simplified

const (
	PROGRAM_COUNTER_FROM_SECTION CodeRelocationType = iota
	FROM_PLT
	FROM_GOT
)

type CodeRelocationInfo struct {
	Offset int
	Addend int
	T CodeRelocationType
	Symbol *irs.Symbol	
}
