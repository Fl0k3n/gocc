package grammars

type SymbolT int

const (
	TERMINAL SymbolT = iota
	NONTERMINAL
)

type Symbol struct {
	Val string
	T SymbolT
}

func Nonterminal(val string) Symbol {
	return Symbol{
		Val: val,
		T: NONTERMINAL,
	}
}

func Terminal(val string) Symbol {
	return Symbol{
		Val: val,
		T: TERMINAL,
	}
}

type ProductionId int

type Production struct {
	// corresponds to its index in grammars productions
	ProdId ProductionId
	From string
	To []Symbol
}

type Grammar struct {
	Terminals []string
	Nonterminals []string
	StartNonterminal string
	Productions []*Production
}

func newEmptyGrammar() *Grammar {
	return &Grammar{
		Terminals: make([]string, 0),
		Nonterminals: make([]string, 0),
		StartNonterminal: "",
		Productions: make([]*Production, 0),
	}
}

