package main

import (
	"fmt"
	"grammars"
)

func main() {
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

