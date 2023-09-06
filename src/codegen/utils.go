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
