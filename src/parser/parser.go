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
	verbose bool
}


func New(grammar *grammars.Grammar, tokenizer *tokens.Tokenizer,
		 actionTable *ActionTable, gotoTable *GotoTable, verbose bool) *Parser {
	return &Parser{
		grammar: grammar,
		tokenizer: tokenizer,
		actionTable: actionTable,
		gotoTable: gotoTable,
		stateStack: utils.NewStack[State](),
		astBuilder: ast.NewBuilder(tokenizer),
		verbose: verbose,
	}
}

func NewForGrammar(grammar *grammars.Grammar, tokenizer *tokens.Tokenizer, verbose bool) *Parser {
	tb := NewTableBuilder(grammar)
	act, got := tb.BuildConfigurationAutomaton()
	return New(grammar, tokenizer, act, got, verbose)
}

func NewFromFile(grammar *grammars.Grammar, tokenizer *tokens.Tokenizer, 
				 actionTabFile string, gotoTabFile string, verbose bool) *Parser {
	tb := NewTableBuilder(grammar)
	act, got := tb.DeserializeTables(actionTabFile, gotoTabFile)
	return New(grammar, tokenizer, act, got, verbose)
}

func (p *Parser) shift(action ShiftAction) {
	p.tokenizer.Advance()
	p.astBuilder.OnShift(p.tokenizer.LastToken())
	p.stateStack.Push(action.NextState)
	if p.verbose {
		fmt.Printf("Shift %d\n", action.NextState)
	}
}

func (p *Parser) reduce(action ReduceAction) error {
	prod := action.Prod
	p.stateStack.PopMany(len(prod.To))
	if nextState, err := p.gotoTable.GetEntry(p.stateStack.Peek(), prod.From); err == nil {
		lineInfo := ast.LineInfo{LineNumber: p.tokenizer.LineIdx}
		if err := p.astBuilder.OnReduce(prod, &lineInfo); err != nil {
			if nextState != INITIAL_STATE {
				return err
			}
		}
		p.stateStack.Push(nextState)
		if p.verbose {
			fmt.Printf("Reduce %s, goto %d\n", prod.From, nextState)
		}
	} else {
		if p.stateStack.Peek() != INITIAL_STATE {
			return err
		}
	}
	return nil
}

func (p *Parser) BuildParseTree() (ast.TranslationUnit, error) {
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
			switch act := action.(type) {
			case ShiftAction:
				p.shift(act)
			case ReduceAction:
				if err = p.reduce(act); err != nil {
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
	return ast.TranslationUnit{}, err
}
