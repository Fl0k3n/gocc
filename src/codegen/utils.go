package codegen

import "irs"

func asSymbols(asyms []*AugmentedSymbol) []*irs.Symbol {
	res := make([]*irs.Symbol, len(asyms))
	for i, asym := range asyms {
		res[i] = asym.Sym
	}
	return res
}
