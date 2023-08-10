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

type Production struct {
	From Symbol
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

func newProduction(from Symbol, to []Symbol) *Production {
	return &Production{
		From: from,
		To: to,
	}
}
