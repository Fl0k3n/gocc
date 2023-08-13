package parsers

import (
	. "grammars"
	"tokenizers"
	"utils"
)

type TableBuilder struct {
	grammar *Grammar
	actionTable *ActionTable
	gotoTable *GotoTable
	nonTerminalToProds map[string][]*Production
	closures map[string][]*Production
	prodEnumerations []int
	firstSets map[string]*utils.Set[string]
	configurations []*Configuration	
	configurationsLookupMap map[ConfigurationKey]int
}

func NewTableBuilder(g *Grammar) *TableBuilder {
	return &TableBuilder{
		grammar: g,
	}
}

func (tb *TableBuilder) initDataStructures() {
	tb.configurations = make([]*Configuration, 0)
	tb.configurationsLookupMap = make(map[ConfigurationKey]int)
	tb.actionTable = newActionTable()
	tb.gotoTable = newGotoTable()
	tb.initNonterminalToProductionsMap()
	tb.initClosuresMap()
	tb.enumerateDotProductions()
	tb.initFirstSets()
}

func (tb *TableBuilder) initNonterminalToProductionsMap() {
	tb.nonTerminalToProds = make(map[string][]*Production)
	for _, prod := range tb.grammar.Productions {
		otherProds, ok := tb.nonTerminalToProds[prod.From]
		if ok {
			tb.nonTerminalToProds[prod.From] = append(otherProds, prod)
		} else {
			prods := make([]*Production, 1)
			prods[0] = prod
			tb.nonTerminalToProds[prod.From] = prods
		}
	}
}

func (tb *TableBuilder) initClosuresMap() {
	tb.closures = make(map[string][]*Production)
	closures := make(map[string]*utils.Set[*Production])
	toExpandQueue := utils.NewQueue[*Production]()

	for _, nt := range tb.grammar.Nonterminals {
		curClousures := utils.NewSet[*Production]()

		for _, prod := range tb.nonTerminalToProds[nt] {
			curClousures.Add(prod)
			if tb.canExpandProd(prod) {
				toExpandQueue.Push(prod)
			}
		}

		for toExpandQueue.Size() > 0 { 
			prodToExpand := toExpandQueue.Pop()

			if expanded, alreadyExpanded := closures[prodToExpand.To[0].Val]; alreadyExpanded {
				curClousures.AddAll(expanded.GetAll())
			} else {
				for _, prod := range tb.nonTerminalToProds[prodToExpand.To[0].Val] {
					if !curClousures.Has(prod) {
						if tb.canExpandProd(prod) {
							toExpandQueue.Push(prod)
						}
						curClousures.Add(prod)
					}
				}
			}
		}
		closures[nt] = curClousures	
	}

	for k, v := range closures {
		tb.closures[k] = v.GetAll()
	}
}

func (tb *TableBuilder) canExpandProd(prod *Production) bool {
	return len(prod.To) > 0 && prod.To[0].T == NONTERMINAL
}

func (tb *TableBuilder) initFirstSets() {
	// assuming that every nonterminal is not nullable (as its the case for selected ANSI C grammar)
	tb.firstSets = make(map[string]*utils.Set[string])
	nonterminalsQueue := utils.NewQueue[string]()

	for _, nt := range tb.grammar.Nonterminals {
		firstSet := utils.NewSet[string]()
		alreadyEnqueued := utils.SetOf[string](nt)

		handleFirstSymbol := func (prod *Production) {
			if len(prod.To) == 0 {
				return
			}
			nextSymbol := prod.To[0]

			if nextSymbol.T == TERMINAL {
				firstSet.Add(nextSymbol.Val)
			} else if !alreadyEnqueued.Has(nextSymbol.Val) {
				nonterminalsQueue.Push(nextSymbol.Val)
				alreadyEnqueued.Add(nextSymbol.Val)
			}
		}

		for _, prod := range tb.nonTerminalToProds[nt] {
			handleFirstSymbol(prod)
		}

		for nonterminalsQueue.Size() > 0 {
			nextNt := nonterminalsQueue.Pop()
			if theirFirstSet, alreadyComputed := tb.firstSets[nextNt]; alreadyComputed {
				firstSet.AddAll(theirFirstSet.GetAll())
			} else {
				for _, prod := range tb.nonTerminalToProds[nextNt] {
					handleFirstSymbol(prod)
				}
			}
		}
		tb.firstSets[nt] = firstSet
	}
}

func (tb *TableBuilder) enumerateDotProductions() {
	tb.prodEnumerations = make([]int, len(tb.grammar.Productions))
	counter := 0
	for idx, prod := range tb.grammar.Productions {
		if idx != int(prod.ProdId) {
			panic("expected prodId to equal id in grammar")
		}
		tb.prodEnumerations[prod.ProdId] = counter
		counter += 1 + len(prod.To)
	}
}

func (tb *TableBuilder) makeDotProd(prod *Production) *DotProduction {
	return FromProduction(
		prod,
		tb.prodEnumerations[prod.ProdId],
		utils.NewSet[string](),
	)
}

func (tb *TableBuilder) buildLookaheads(parentLookaheads *utils.Set[string], startNt string) map[string]*utils.Set[string] {
	ntToLookaheads := map[string]*utils.Set[string]{}
	closures := tb.closures[startNt]

	for _, closureProd := range closures {
		dotprod := tb.makeDotProd(closureProd)
		if curSym := dotprod.NextSymbol(); curSym.T == NONTERMINAL { 
			if _, ok := ntToLookaheads[curSym.Val]; !ok {
				ntToLookaheads[curSym.Val] = utils.NewSet[string]()
			}
			if dotprod.HasLookaheadSymbol() {
				nextSym := dotprod.LookaheadSymbol()
				if nextSym.T == TERMINAL {
					ntToLookaheads[curSym.Val].Add(nextSym.Val)
				} else {
					ntToLookaheads[curSym.Val].AddAll(tb.firstSets[nextSym.Val].GetAll())
				}
			} else {
				ntToLookaheads[curSym.Val].AddAll(parentLookaheads.GetAll())
			}
		}
	}

	for _, closureProd := range closures {
		if _, ok := ntToLookaheads[closureProd.From]; !ok {
			ntToLookaheads[closureProd.From] = parentLookaheads
		}
	}

	return ntToLookaheads
}

func (tb *TableBuilder) computeClosure(conf *Configuration) *Configuration {
	presentDotProds := make(map[int]*DotProduction)
	prod := conf.productions[0]
	presentDotProds[prod.dotProdId] = prod

	if !prod.IsFullyComputed() && prod.NextSymbol().T == NONTERMINAL {
		nextNt := prod.NextSymbol().Val
		lookaheads := tb.buildLookaheads(prod.lookahead, nextNt)
		for _, closureProd := range tb.closures[nextNt] {
			dotProd := tb.makeDotProd(closureProd)
			lookahead := lookaheads[closureProd.From]

			if alreadyAddedDotProd, ok := presentDotProds[dotProd.dotProdId]; ok {
				alreadyAddedDotProd.lookahead.AddAll(lookahead.GetAll())
			} else {
				dotProd.lookahead = lookahead
				presentDotProds[dotProd.dotProdId] = dotProd
			}
		}
	}

	// TODO consider sorting it based on dotprodId, or better using tree instead of hashmap
	res := EmptyConfiguration()
	for _, dp := range presentDotProds {
		res.AddDotProd(dp)
	}
	return res
}

func (tb *TableBuilder) addConfiguration(conf *Configuration) (confId int) {
	confId = len(tb.configurations)
	tb.configurationsLookupMap[conf.Key()] = confId
	tb.configurations = append(tb.configurations, conf)
	tb.actionTable.RegisterConfiguration()
	tb.gotoTable.RegisterConfiguration()
	return
}

func (tb *TableBuilder) expandConfiguration(conf *Configuration, confId int) {
	for _, dotprod := range conf.productions {
		if dotprod.IsFullyComputed() {
			if dotprod.lookahead.Size() == 0 {
				tb.actionTable.AddReduceEntry(confId, tokenizers.EOF, dotprod.prod)
			} else {
				for _, laTerminal := range dotprod.lookahead.GetAll() {
					tb.actionTable.AddReduceEntry(confId, laTerminal, dotprod.prod)
				}
			}
		} else {
			shiftedProd := dotprod.CreateShiftedRight()
			newConf := tb.computeClosure(ConfigurationFrom(shiftedProd))
			var nextConfId int
			if id, alreadyPresent := tb.configurationsLookupMap[newConf.Key()]; alreadyPresent {
				oldConf := tb.configurations[id]
				for prodIdx := range newConf.productions {
					// same order is guaranteed by key construction algo
					newLookaheads := newConf.productions[prodIdx].lookahead
					oldLookaheads := oldConf.productions[prodIdx].lookahead
					oldConf.productions[prodIdx].lookahead  = newLookaheads.Union(oldLookaheads)
				}
				nextConfId = id
			} else {
				nextConfId = tb.addConfiguration(newConf)
			}
			nextSymbol := dotprod.NextSymbol()

			if nextSymbol.T == TERMINAL {
				tb.actionTable.AddShiftEntry(confId, nextSymbol.Val, nextConfId)
			} else {
				tb.gotoTable.AddEntry(confId, nextSymbol.Val, nextConfId)
			}
		}
	}
}

func (tb *TableBuilder) BuildConfigurationAutomaton() (*ActionTable, *GotoTable) {
	tb.initDataStructures()
	if len(tb.nonTerminalToProds[tb.grammar.StartNonterminal]) != 1 {
		panic("expected single production from artificially added start symbol")
	} 

	startConf := EmptyConfiguration()
	startConf.AddDotProd(tb.makeDotProd(tb.nonTerminalToProds[tb.grammar.StartNonterminal][0]))
	startConf = tb.computeClosure(startConf)
	tb.addConfiguration(startConf)
	expandedConfigurationsCount := 0

	for expandedConfigurationsCount < len(tb.configurations) {
		conf := tb.configurations[expandedConfigurationsCount]
		tb.expandConfiguration(conf, expandedConfigurationsCount)
		expandedConfigurationsCount++
	}

	return tb.actionTable, tb.gotoTable
}

func (tb *TableBuilder) PrintConfigurations() {
	for id, c := range tb.configurations {
		c.Print(id)
	}
}
