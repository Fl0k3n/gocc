package main

import (
	"fmt"
	"grammars"
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

func main() {
	testTokenizer()
}

