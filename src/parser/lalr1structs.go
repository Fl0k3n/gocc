package parsers

import (
	"errors"
	"fmt"
	. "grammars"
	"sort"
	"strconv"
	"strings"
	"tokenizers"
	"utils"
)

type State int

const INITIAL_STATE = State(0)

type Action interface {
}
type ReduceAction struct {
	Prod *Production
}
type ShiftAction struct {
	NextState State
}
type Actions map[string]Action

type ActionTable struct {
	tab []Actions
}

func newActionTable() *ActionTable {
	return &ActionTable{
		tab: make([]Actions, 0),
	}
}

func (at *ActionTable) RegisterConfiguration() {
	at.tab = append(at.tab, make(Actions))
}

func (at *ActionTable) AddReduceEntry(confId State, terminal string, prod *Production) {
	at.tab[confId][terminal] = ReduceAction{
		Prod: prod,
	}
}

func (at *ActionTable) AddShiftEntry(confId State, terminal string, nextState int) {
	at.tab[confId][terminal] = ShiftAction{
		NextState: State(nextState),
	}
}

func (at *ActionTable) GetAction(state State, terminal string) (Action, error) {
	if act, ok := at.tab[state][terminal]; ok {
		return act, nil
	}
	return ReduceAction{}, errors.New(fmt.Sprintf(
		"no action to take from configuration %d using terminal %s", state, terminal))
}

type Gotos map[string]State

type GotoTable struct {
	tab []Gotos
}

func newGotoTable() *GotoTable {
	return &GotoTable{
		tab: make([]Gotos, 0),
	}
}

func (gt *GotoTable) RegisterConfiguration() {
	gt.tab = append(gt.tab, make(Gotos))
}

func (gt *GotoTable) AddEntry(confId State, nonterminal string, nextState int) {
	gt.tab[confId][nonterminal] = State(nextState)
}

func (gt *GotoTable) GetEntry(state State, nonterminal string) (State, error) {
	if ent, ok := gt.tab[state][nonterminal]; ok {
		return ent, nil
	}
	return -1, errors.New(fmt.Sprintf(
		"no goto entry to take from configuration %d using nonterminal %s", state, nonterminal))
}

type DotProduction struct {
	prod *Production
	dotProdId int
	dotPos int
	lookahead *utils.Set[string]
}

func FromProduction(prod *Production, dotProdCounter int, lookahead *utils.Set[string]) *DotProduction {
	return &DotProduction{
		prod: prod,
		dotProdId: dotProdCounter,
		dotPos: 0,
		lookahead: lookahead,
	}
}

func (dp *DotProduction) CreateShiftedRight() *DotProduction {
	return &DotProduction{
		prod: dp.prod,
		dotProdId: dp.dotProdId + 1,
		dotPos: dp.dotPos + 1,
		lookahead: dp.lookahead,
	}
}

func (dp *DotProduction) IsFullyComputed() bool {
	return dp.dotPos == len(dp.prod.To)
}

func (dp *DotProduction) NextSymbol() Symbol {
	return dp.prod.To[dp.dotPos]
}

func (dp *DotProduction) HasLookaheadSymbol() bool {
	return dp.dotPos + 1 < len(dp.prod.To)
}

func (dp *DotProduction) LookaheadSymbol() Symbol {
	return dp.prod.To[dp.dotPos + 1]
}

type ConfigurationKey string

type Configuration struct {
	productions []*DotProduction
	key ConfigurationKey
}

func EmptyConfiguration() *Configuration {
	return &Configuration{
		productions: make([]*DotProduction, 0),
		key: "",
	}
}

func ConfigurationFrom(dotprods ...*DotProduction) *Configuration {
	conf := EmptyConfiguration()
	for _, dotprod := range dotprods {
		conf.AddDotProd(dotprod)
	}
	return conf
}

func (c *Configuration) Productions() []*DotProduction {
	return c.productions
}

func (c *Configuration) Key() ConfigurationKey {
	return c.key
}

func (c *Configuration) AddDotProd(prod *DotProduction)  {
	c.productions = append(c.productions, prod)
	for i := len(c.productions) - 1; i > 0; i-- {
		prev := c.productions[i-1]
		if prod.dotProdId < prev.dotProdId {
			c.productions[i] = prev
			c.productions[i-1] = prod
		} else {
			break
		}
	}

	var sb strings.Builder 
	for _, prod := range c.productions {
		sb.WriteString("," + strconv.Itoa(prod.dotProdId))
	}
	c.key = ConfigurationKey(sb.String())
}

func (c *Configuration) JoinProdsByNextSymbol() map[string][]*DotProduction {
	res := make(map[string][]*DotProduction)
	for _, prod := range c.productions {
		if !prod.IsFullyComputed() {
			if prods, ok := res[prod.NextSymbol().Val]; ok {
				res[prod.NextSymbol().Val] = append(prods, prod)
			} else {
				res[prod.NextSymbol().Val] = []*DotProduction{prod}
			}
		}
	}
	return res
}

func (c *Configuration) getFullyComputedProds() []*DotProduction {
	res := make([]*DotProduction, 0)
	for _, prod := range c.productions {
		if prod.IsFullyComputed() {
			res = append(res, prod)
		}
	}
	return res
}

func (c *Configuration) Print(confId int) {
	fmt.Printf("Configuration %d {\n", confId)
	for _, dotProd := range c.productions {
		rhs := make([]string, 0, len(dotProd.prod.To))
		las := ""
		for _, sym := range dotProd.prod.To[:dotProd.dotPos] {
			rhs = append(rhs, sym.Val)
		}
		rhs = append(rhs, "@")
		for _, sym := range dotProd.prod.To[dotProd.dotPos:] {
			rhs = append(rhs, sym.Val)
		}
		lookaheads := dotProd.lookahead.GetAll()
		sort.Strings(lookaheads)
		if len(lookaheads) == 0 {
			las = tokenizers.EOF
		} else {
			las = strings.Join(lookaheads, "/")
		}
		fmt.Printf("\t%s -> %s, %s\n", dotProd.prod.From, strings.Join(rhs, " "), las)
	}
	fmt.Println("}")
}
