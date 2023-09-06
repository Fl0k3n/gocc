package codegen

import (
	"fmt"
	"irs"
)

func asSymbols(asyms []*AugmentedSymbol) []*irs.Symbol {
	res := make([]*irs.Symbol, len(asyms))
	for i, asym := range asyms {
		res[i] = asym.Sym
	}
	return res
}

func createFunctionReturnLabel(funName string) string {
	return fmt.Sprintf("0%s_RET", funName)
}

func createFunctionLabel(funName string) string {
	return funName
}

func getIntegralMemoryDescriptor(size int) (memDescriptor string) {
	switch size {
	case QWORD_SIZE: memDescriptor = "QWORD"
	case DWORD_SIZE: memDescriptor = "DWORD"
	case WORD_SIZE: memDescriptor = "WORD"
	case BYTE_SIZE: memDescriptor = "BYTE"
	default: panic("unknown size")
	}
	return
}
