package parsers

import (
	"fmt"
	. "grammars"
	tokens "tokenizers"
	"utils"
)

type Parser struct {
	grammar *Grammar
	tokenizer *tokens.Tokenizer
	actionTable *ActionTable
	gotoTable *GotoTable
	nonTerminalToProds map[string][]*Production
	closures map[string][]*Production
	prodEnumerations []int
	firstSets map[string]*utils.Set[string]
	configurations []*Configuration	
	configurationsLookupMap map[ConfigurationKey]int
}


func New(grammar *Grammar, tokenizer *tokens.Tokenizer) *Parser {
	return &Parser{
		grammar: grammar,
		tokenizer: tokenizer,
	}
}


func (p *Parser) BuildParseTree() {
	p.initDataStructures()
	fmt.Println("x")
}
