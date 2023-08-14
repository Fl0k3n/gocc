package parsers

import (
	"fmt"
	"grammars"
	"tokenizers"
	tokens "tokenizers"
	"utils"
)

type Parser struct {
	grammar *grammars.Grammar
	tokenizer *tokens.Tokenizer
	actionTable *ActionTable
	gotoTable *GotoTable
	stateStack *utils.Stack[State]
}


func New(grammar *grammars.Grammar, tokenizer *tokens.Tokenizer,
		 actionTable *ActionTable, gotoTable *GotoTable) *Parser {
	return &Parser{
		grammar: grammar,
		tokenizer: tokenizer,
		actionTable: actionTable,
		gotoTable: gotoTable,
		stateStack: utils.NewStack[State](),
	}
}

func NewForGrammar(grammar *grammars.Grammar, tokenizer *tokens.Tokenizer) *Parser {
	tb := NewTableBuilder(grammar)
	act, got := tb.BuildConfigurationAutomaton()
	// tb.PrintConfigurations()
	return New(grammar, tokenizer, act, got)
}

func (p *Parser) shift(action ShiftAction) {
	p.tokenizer.Advance()
	// p.stateStack.Push(p.tokenizer.LastToken().T)
	// p.currentState = action.nextState
	p.stateStack.Push(action.nextState)
	fmt.Printf("Shift %d\n", action.nextState)
}

func (p *Parser) reduce(action ReduceAction) {
	prod := action.prod
	p.stateStack.PopMany(len(prod.To))
	if nextState, err := p.gotoTable.GetEntry(p.stateStack.Peek(), prod.From); err == nil {
		p.stateStack.Push(nextState)
		fmt.Printf("Reduce %s, goto %d\n", prod.From, nextState)
	} else {
		panic(err)
	}
}

func (p *Parser) BuildParseTree() {
	p.stateStack.Push(0)
	for {
		token := p.tokenizer.Lookahead()
		if token.T == tokenizers.EOF {
			if p.stateStack.Peek() == 0 {
				// TODO fixit
				break
			}
		}

		if action, err := p.actionTable.GetAction(p.stateStack.Peek(), token.T); err == nil  {
			switch action.(type) {
			case ShiftAction:
				p.shift(action.(ShiftAction))
			case ReduceAction:
				p.reduce(action.(ReduceAction))
			}
		} else {
			fmt.Println(err)
			fmt.Printf("syntax error in line %d\n in state %d, no lookahead for token %s\n",
						p.tokenizer.LineIdx, p.stateStack.Peek(), token)
			return
		}
	}
}
