package main

import (
	"fmt"
	"grammars"
	"parsers"
	"regexp"
	"tokenizers"
	"types"
)

func testGrammarReader() {
	grammarReader, err := grammars.NewReader("../resources/ansi_c_grammar.y")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer grammarReader.Finish()
	grammar, err := grammarReader.Read()
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Println(grammar)
}

func testTokenizer() {
	grammarReader, err := grammars.NewReader("../resources/ansi_c_grammar.y")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer grammarReader.Finish()
	grammar, err := grammarReader.Read()
	tokenizer, err := tokenizers.New("../resources/csrc/simple.c", grammar)
	// tokenizer, err := tokenizers.New("../resources/csrc/functions.c", grammar)
	if err != nil {
		fmt.Println(err)
		return
	}
	defer tokenizer.Finish()
	tokens := make([]tokenizers.Token, 0)
	for {
		tokenizer.Advance()
		token := tokenizer.LastToken()
		if token.T == tokenizers.EOF {
			break
		}
		tokens = append(tokens, token)
	}
	fmt.Println(tokens)
}

func testTableBuilder() {
	grammar := &grammars.Grammar{
		Terminals: []string{"a", "b"},
		Nonterminals: []string{"S'", "S", "X"},
		StartNonterminal: "S'",
		Productions: []*grammars.Production{
			{From: "S'", To: []grammars.Symbol{grammars.Nonterminal("S")}, ProdId: 0},
			{From: "S", To: []grammars.Symbol{grammars.Nonterminal("X"), grammars.Nonterminal("X")}, ProdId: 1},
			{From: "X", To: []grammars.Symbol{grammars.Terminal("a"), grammars.Nonterminal("X")}, ProdId: 2},
			{From: "X", To: []grammars.Symbol{grammars.Terminal("b")}, ProdId: 3},
		},
		StringsToTokenTypes: map[string]string{"a": "a", "b": "b"},
		RegexesToTokenTypes: make(map[*regexp.Regexp]string),
	}
	// grammarReader, err := grammars.NewReader("../resources/ansi_c_grammar.y")
	// if err != nil {
	// 	fmt.Println(err)
	// 	return
	// }
	// defer grammarReader.Finish()
	// grammar, err := grammarReader.Read()
	// if err != nil {
	// 	fmt.Println(err)
	// 	return
	// }

	tb := parsers.NewTableBuilder(grammar)
	tb.BuildConfigurationAutomaton()
	tb.PrintConfigurations()
}

func testTableBuilder2() {
	grammar := &grammars.Grammar{
		Terminals: []string{"=", "+", "int", "id", "(", ")"},
		Nonterminals: []string{"S'", "S", "V", "E", "F"},
		StartNonterminal: "S'",
		Productions: []*grammars.Production{
			{From: "S'", To: []grammars.Symbol{grammars.Nonterminal("S")}, ProdId: 0},
			{From: "S", To: []grammars.Symbol{grammars.Nonterminal("V"), grammars.Terminal("="), grammars.Nonterminal("E")}, ProdId: 1},
			{From: "E", To: []grammars.Symbol{grammars.Nonterminal("F")}, ProdId: 2},
			{From: "E", To: []grammars.Symbol{grammars.Nonterminal("E"), grammars.Terminal("+"), grammars.Nonterminal("F")}, ProdId: 3},
			{From: "F", To: []grammars.Symbol{grammars.Nonterminal("V")}, ProdId: 4},
			{From: "F", To: []grammars.Symbol{grammars.Terminal("int")}, ProdId: 5},
			{From: "F", To: []grammars.Symbol{grammars.Terminal("("), grammars.Nonterminal("E"), grammars.Terminal(")")}, ProdId: 6},
			{From: "V", To: []grammars.Symbol{grammars.Terminal("id")}, ProdId: 7},
		},
		StringsToTokenTypes: map[string]string{"=": "a", "b": "b"},
		RegexesToTokenTypes: make(map[*regexp.Regexp]string),
	}
	// grammarReader, err := grammars.NewReader("../resources/ansi_c_grammar.y")
	// if err != nil {
	// 	fmt.Println(err)
	// 	return
	// }
	// defer grammarReader.Finish()
	// grammar, err := grammarReader.Read()
	// if err != nil {
	// 	fmt.Println(err)
	// 	return
	// }

	tb := parsers.NewTableBuilder(grammar)
	tb.BuildConfigurationAutomaton()
	tb.PrintConfigurations()
}

func testParserSimple() {
	grammar := &grammars.Grammar{
		Terminals: []string{"a", "b"},
		Nonterminals: []string{"S'", "S", "X"},
		StartNonterminal: "S'",
		Productions: []*grammars.Production{
			{From: "S'", To: []grammars.Symbol{grammars.Nonterminal("S")}, ProdId: 0},
			{From: "S", To: []grammars.Symbol{grammars.Nonterminal("X"), grammars.Nonterminal("X")}, ProdId: 1},
			{From: "X", To: []grammars.Symbol{grammars.Terminal("a"), grammars.Nonterminal("X")}, ProdId: 2},
			{From: "X", To: []grammars.Symbol{grammars.Terminal("b")}, ProdId: 3},
		},
		StringsToTokenTypes: map[string]string{"a": "a", "b": "b"},
		RegexesToTokenTypes: make(map[*regexp.Regexp]string),
	}
	tokenizer, err := tokenizers.New("../resources/sample", grammar)
	if err != nil {
		fmt.Println(err)
		return
	}
	defer tokenizer.Finish()
	p := parsers.NewForGrammar(grammar, tokenizer)
	p.BuildParseTree()
}

func testParserSimple2() {
	grammar := &grammars.Grammar{
		Terminals: []string{"=", "+", "int", "id", "(", ")"},
		Nonterminals: []string{"S'", "S", "V", "E", "F"},
		StartNonterminal: "S'",
		Productions: []*grammars.Production{
			{From: "S'", To: []grammars.Symbol{grammars.Nonterminal("S")}, ProdId: 0},
			{From: "S", To: []grammars.Symbol{grammars.Nonterminal("V"), grammars.Terminal("="), grammars.Nonterminal("E")}, ProdId: 1},
			{From: "E", To: []grammars.Symbol{grammars.Nonterminal("F")}, ProdId: 2},
			{From: "E", To: []grammars.Symbol{grammars.Nonterminal("E"), grammars.Terminal("+"), grammars.Nonterminal("F")}, ProdId: 3},
			{From: "F", To: []grammars.Symbol{grammars.Nonterminal("V")}, ProdId: 4},
			{From: "F", To: []grammars.Symbol{grammars.Terminal("int")}, ProdId: 5},
			{From: "F", To: []grammars.Symbol{grammars.Terminal("("), grammars.Nonterminal("E"), grammars.Terminal(")")}, ProdId: 6},
			{From: "V", To: []grammars.Symbol{grammars.Terminal("id")}, ProdId: 7},
		},
		StringsToTokenTypes: map[string]string{"id": "id", "(": "(", ")": ")", "int": "int", "+": "+", "=": "="},
		RegexesToTokenTypes: make(map[*regexp.Regexp]string),
	}
	tokenizer, err := tokenizers.New("../resources/sampleexpr", grammar)
	if err != nil {
		fmt.Println(err)
		return
	}
	defer tokenizer.Finish()
	p := parsers.NewForGrammar(grammar, tokenizer)
	p.BuildParseTree()
}

func testParser() {
	grammarReader, err := grammars.NewReader("../resources/ansi_c_grammar.y")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer grammarReader.Finish()
	grammar, err := grammarReader.Read()
	if err != nil {
		fmt.Println(err)
		return
	}
	tokenizer, err := tokenizers.New("../resources/csrc/functions.c", grammar)
	if err != nil {
		fmt.Println(err)
		return
	}
	defer tokenizer.Finish()
	// p := parsers.NewForGrammar(grammar, tokenizer)
	p := parsers.NewFromFile(grammar, tokenizer, "/home/flok3n/misc/acttab.gob", "/home/flok3n/misc/gototab.gob")
	tu, err := p.BuildParseTree()
	if err != nil {
		fmt.Println("Parser error")
		fmt.Println(err)
	}
	// fmt.Println(tu.GetLineBounds())
	typ := types.NewEngine()
	typ.AugmentASTWithTypeInfo(&tu)
}

func serializeTables() {
	grammarReader, err := grammars.NewReader("../resources/ansi_c_grammar.y")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer grammarReader.Finish()
	grammar, err := grammarReader.Read()
	if err != nil {
		fmt.Println(err)
		return
	}
	tb := parsers.NewTableBuilder(grammar)
	tb.BuildConfigurationAutomaton()
	tb.SerializeTables("/home/flok3n/misc/acttab.gob", "/home/flok3n/misc/gototab.gob")
}


func main() {
	// testParserSimple2()
	testParser()
	// testGrammarReader()
	// testTokenizer()
	// testTableBuilder()
	// testTableBuilder2()
	// serializeTables()
}
