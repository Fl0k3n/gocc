package grammars

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"regexp"
	"strings"
	"utils"
)

const TOKEN_PREFIX = "%token"
const START_PREFIX = "%start"
const SECTON_DELIMETER = "%%"
const PRODUCTION_END_SYMBOL = ";"
// {L}({L}|{D})*
const ARTIFICIAL_START = "S_0"

var TERMINAL_REGEX = regexp.MustCompile(`'.+?'`)
var REGEX_DEF_USAGE_REGEX = regexp.MustCompile(`\{.+?\}`)

type Reader struct {
	inputFile *os.File
	scanner *bufio.Scanner
	grammar *Grammar
	currentLine string
	tokens *utils.Set[string]
	terminals *utils.Set[string]
	nonTerminals *utils.Set[string]
	regexDefinitions map[string]string
	prodCounter int
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
		regexDefinitions: make(map[string]string),
		prodCounter: 0,
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
		if strings.HasPrefix(r.currentLine, SECTON_DELIMETER) {
			break
		}
	}
	return nil
}

func (r *Reader) readAndFlattenProduction() ([]*Production, error) {
	productions := make([]*Production, 0)

	if err := r.readNextNonemptyLine(); err != nil {
		return productions, nil
	}
	if r.currentLine == SECTON_DELIMETER {
		return productions, nil
	}

	fromNonterminal := r.currentLine
	for {
		if err := r.readNextNonemptyLine(); err != nil {
			return nil, errors.New("Expected production terminator")
		}
		if r.currentLine == PRODUCTION_END_SYMBOL {
			break
		}

		tokens := strings.Split(r.currentLine, " ")[1:]
		production := &Production{
			ProdId: ProductionId(r.prodCounter),
			From: fromNonterminal,
			To: make([]Symbol, 0, len(tokens)),
		}
		r.prodCounter++

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

func (r *Reader) readRegexDefinitions() error {
	for {
		if err := r.readNextNonemptyLine(); err != nil {
			return errors.New("Expected regex definitions terminator")
		}
		if r.currentLine == SECTON_DELIMETER {
			return nil
		}
		fields := strings.Fields(r.currentLine)
		if len(fields) != 2 {
			return errors.New("Invalid regex definition")
		}
		r.regexDefinitions[fields[0]] = fields[1]
	}
}

func (r *Reader) substituteRegexDefinitionUsages(regex string) string {
	res := REGEX_DEF_USAGE_REGEX.ReplaceAllStringFunc(regex, func (defToReplace string) string {
		return r.regexDefinitions[defToReplace[1:len(defToReplace)-1]]
	})
	if !strings.HasPrefix(res, "^") {
		res = "^" + res 
	}
	if !strings.HasSuffix(res, "$") {
		res = res + "$"
	}
	return res
}

func (r *Reader) readTokenDefinitions() error {
	for {
		if err := r.readNextNonemptyLine(); err != nil {
			return errors.New("Expected token definitions terminator")
		}
		if r.currentLine == SECTON_DELIMETER {
			return nil
		}

		fields := strings.Fields(r.currentLine)
		if len(fields) != 2 {
			return errors.New("Invalid token definition")
		}
		def := fields[0]
		token := fields[1]
		if strings.HasPrefix(def, "\"") {
			r.grammar.StringsToTokenTypes[def[1:len(def)-1]] = token
		} else {
			subtituted := r.substituteRegexDefinitionUsages(def)
			if reg, err := regexp.Compile(subtituted); err != nil {
				return errors.New(fmt.Sprintf("Invalid regex definition %s for token %s", subtituted, token))
			} else {
				r.grammar.RegexesToTokenTypes[reg] = token
			}
		}
	}
}

func (r *Reader) fillGrammarSymbols() error {
	r.grammar.Terminals = r.terminals.GetAll()
	r.grammar.Nonterminals = r.nonTerminals.GetAll()
	return nil
}

func (r *Reader) augmentWithArtificialStart() error {
	r.grammar.Nonterminals = append(r.grammar.Nonterminals, ARTIFICIAL_START)
	r.grammar.Productions = append(r.grammar.Productions, &Production{	
		ProdId: ProductionId(len(r.grammar.Productions)),
		From: ARTIFICIAL_START,
		To: []Symbol{{T: NONTERMINAL, Val: r.grammar.StartNonterminal}},
	})
	r.grammar.StartNonterminal = ARTIFICIAL_START
	return nil
}

func (r *Reader) Read() (*Grammar, error) {
	err := utils.Pipeline().
		Then(r.readTokens).
		Then(r.readStartNonterminal).
		Then(r.advanceToProductions).
		Then(r.readProductions).
		Then(r.readRegexDefinitions).
		Then(r.readTokenDefinitions).
		Then(r.fillGrammarSymbols).
		Then(r.augmentWithArtificialStart).
		Error()

	return r.grammar, err
}
