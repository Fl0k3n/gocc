package grammars

import (
	"bufio"
	"errors"
	"os"
	"regexp"
	"strings"
	"utils"
)

const TOKEN_PREFIX = "%token"
const START_PREFIX = "%start"
const PRODUCTIONS_PREFIX = "%%"
const PRODUCTION_END_SYMBOL = ";"


var TERMINAL_REGEX = regexp.MustCompile(`'.+?'`)

type Reader struct {
	inputFile *os.File
	scanner *bufio.Scanner
	grammar *Grammar
	currentLine string
	tokens *utils.Set[string]
	terminals *utils.Set[string]
	nonTerminals *utils.Set[string]
}

func NewReader(inputPath string) (*Reader, error){
	f, err := os.Open(inputPath)
	if err != nil {
		return nil, err
	}

	return &Reader{
		inputFile: f,
		scanner: bufio.NewScanner(f),
		grammar: newEmptyGrammar(),
		currentLine: "",
		tokens: utils.NewSet[string](),
		terminals: utils.NewSet[string](),
		nonTerminals: utils.NewSet[string](),
	}, nil
}

func (r *Reader) Finish() {
	r.inputFile.Close()
}

func (r *Reader) readNextNonemptyLine() error {
	for r.scanner.Scan() {
		line := strings.TrimSpace(r.scanner.Text())
		if len(line) > 0 {
			r.currentLine = line
			return nil
		}
	}
	
	return errors.New("Expected next line")
}

func (r* Reader) readTokens() error {
	for {
		if err := r.readNextNonemptyLine(); err != nil {
			return err
		}
		if strings.HasPrefix(r.currentLine, TOKEN_PREFIX) {
			tokens := strings.Split(r.currentLine, " ")
			r.tokens.AddAll(tokens[1:])
		} else {
			break
		}
	}
	return nil
}

func (r* Reader) readStartNonterminal() error {
	if strings.HasPrefix(r.currentLine, START_PREFIX) {
		r.grammar.StartNonterminal = strings.Split(r.currentLine, " ")[1]
		return nil
	}
	return errors.New("Expected start nonterminal")
}

func (r *Reader) advanceToProductions() error {
	for {
		if err := r.readNextNonemptyLine(); err != nil {
			return err
		}
		if strings.HasPrefix(r.currentLine, PRODUCTIONS_PREFIX) {
			break
		}
	}
	return nil
}

func (r *Reader) readAndFlattenProduction() ([]*Production, error) {
	productions := make([]*Production, 0)
	fromNonterminal := Nonterminal("")

	if err := r.readNextNonemptyLine(); err != nil {
		return productions, nil
	}

	fromNonterminal.Val = r.currentLine
	for {
		if err := r.readNextNonemptyLine(); err != nil {
			return nil, errors.New("Expected production terminator")
		}
		if r.currentLine == PRODUCTION_END_SYMBOL {
			break
		}

		tokens := strings.Split(r.currentLine, " ")[1:]
		production := &Production{
			From: fromNonterminal,
			To: make([]Symbol, 0, len(tokens)),
		}
		for idx, token := range tokens {
			if TERMINAL_REGEX.MatchString(token) {
				tokens[idx] = token[1:len(token)-1]
				r.terminals.Add(tokens[idx])
				production.To = append(production.To, Terminal(tokens[idx]))
			} else if r.tokens.Has(token) {
				r.terminals.Add(token)
				production.To = append(production.To, Terminal(token))
			} else {
				r.nonTerminals.Add(token)
				production.To = append(production.To, Nonterminal(token))
			}
		}

		productions = append(productions, production)
	}
	
	return productions, nil
}

func (r *Reader) readProductions() error {
	for {
		prods, err := r.readAndFlattenProduction()
		if err != nil {
			return err
		}
		if len(prods) == 0 {
			return nil
		}
		r.grammar.Productions = append(r.grammar.Productions, prods...)
	}
}

func (r *Reader) fillGrammarSymbols() error {
	r.grammar.Terminals = r.terminals.GetAll()
	r.grammar.Nonterminals = r.nonTerminals.GetAll()
	return nil
}

func (r *Reader) Read() (*Grammar, error) {
	err := utils.Pipeline().
		Then(r.readTokens).
		Then(r.readStartNonterminal).
		Then(r.advanceToProductions).
		Then(r.readProductions).
		Then(r.fillGrammarSymbols).
		Error()

	return r.grammar, err
}
