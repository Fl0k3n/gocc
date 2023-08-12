package main

import (
	"fmt"
	"grammars"
	"parsers"
	"tokenizers"
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
	tokenizer, err := tokenizers.New("../resources/csrc/functions.c")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer tokenizer.Finish()
	tokens := make([]string, 0)
	for {
		tokenizer.Advance()
		token := tokenizer.LastToken()
		if token == tokenizers.EOF {
			break
		}
		tokens = append(tokens, token)
	}
	fmt.Println(tokens)
}

func testParser() {
	tokenizer, err := tokenizers.New("../resources/csrc/functions.c")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer tokenizer.Finish()
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
	}
	p := parsers.New(grammar, tokenizer)
	p.BuildParseTree()
}

func main() {
	testParser()
}

