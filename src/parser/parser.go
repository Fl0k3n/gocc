package parsers

import (
	"ast"
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
	astBuilder *ast.Builder
}


func New(grammar *grammars.Grammar, tokenizer *tokens.Tokenizer,
		 actionTable *ActionTable, gotoTable *GotoTable) *Parser {
	return &Parser{
		grammar: grammar,
		tokenizer: tokenizer,
		actionTable: actionTable,
		gotoTable: gotoTable,
		stateStack: utils.NewStack[State](),
		astBuilder: ast.NewBuilder(),
	}
}

func NewForGrammar(grammar *grammars.Grammar, tokenizer *tokens.Tokenizer) *Parser {
	tb := NewTableBuilder(grammar)
	act, got := tb.BuildConfigurationAutomaton()
	return New(grammar, tokenizer, act, got)
}

func NewFromFile(grammar *grammars.Grammar, tokenizer *tokens.Tokenizer, 
				 actionTabFile string, gotoTabFile string) *Parser {
	tb := NewTableBuilder(grammar)
	act, got := tb.DeserializeTables(actionTabFile, gotoTabFile)
	return New(grammar, tokenizer, act, got)
}

func (p *Parser) shift(action ShiftAction) {
	p.tokenizer.Advance()
	p.astBuilder.OnShift(p.tokenizer.LastToken())
	p.stateStack.Push(action.NextState)
	fmt.Printf("Shift %d\n", action.NextState)
}

func (p *Parser) reduce(action ReduceAction) error {
	prod := action.Prod
	p.stateStack.PopMany(len(prod.To))
	if p.stateStack.Peek() != INITIAL_STATE {
		if err := p.astBuilder.OnReduce(prod); err != nil {
			return err
		}
	}
	if nextState, err := p.gotoTable.GetEntry(p.stateStack.Peek(), prod.From); err == nil {
		p.stateStack.Push(nextState)
		fmt.Printf("Reduce %s, goto %d\n", prod.From, nextState)
	} else {
		if p.stateStack.Peek() != INITIAL_STATE {
			return err
		}
	}
	return nil
}

func (p *Parser) BuildParseTree() (ast.Node, error) {
	var err error
	var token tokenizers.Token

	p.stateStack.Push(INITIAL_STATE)
	for {
		token = p.tokenizer.Lookahead()
		if token.T == tokenizers.EOF {
			if p.stateStack.Peek() == INITIAL_STATE {
				break
			}
		}

		if action, err := p.actionTable.GetAction(p.stateStack.Peek(), token.T); err == nil  {
			switch action.(type) {
			case ShiftAction:
				p.shift(action.(ShiftAction))
			case ReduceAction:
				if err = p.reduce(action.(ReduceAction)); err != nil {
					goto parserError
				}
			}
		} else {
			goto parserError
		}
	}
	return p.astBuilder.GetParsedTree()

parserError:
	fmt.Println(err)
	fmt.Printf("syntax error in line %d\n in state %d, no lookahead for token %s\n",
				p.tokenizer.LineIdx, p.stateStack.Peek(), token)
	return nil, err
}
