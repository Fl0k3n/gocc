package parsers

import (
	"grammars"
	tokens "tokenizers"
)

type Parser struct {
	tokenizer *tokens.Tokenizer
	actionTable *ActionTable
	gotoTable *GotoTable
}


func New(tokenizer *tokens.Tokenizer, actionTable *ActionTable, gotoTable *GotoTable) *Parser {
	return &Parser{
		tokenizer: tokenizer,
		actionTable: actionTable,
		gotoTable: gotoTable,
	}
}

func NewForGrammar(grammar *grammars.Grammar, tokenizer *tokens.Tokenizer) *Parser {
	tb := NewTableBuilder(grammar)
	act, got := tb.BuildConfigurationAutomaton()
	return New(tokenizer, act, got)
}

func (p *Parser) BuildParseTree() {
}
