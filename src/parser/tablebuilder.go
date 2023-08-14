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
	bruteForceCLM map[ConfigurationKey][]int
	successors []map[string]int
}

func NewTableBuilder(g *Grammar) *TableBuilder {
	return &TableBuilder{
		grammar: g,
	}
}

func (tb *TableBuilder) initDataStructures() {
	tb.configurations = make([]*Configuration, 0)
	tb.configurationsLookupMap = make(map[ConfigurationKey]int)
	tb.bruteForceCLM = make(map[ConfigurationKey][]int)
	tb.successors = make([]map[string]int, 0)
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

func (tb *TableBuilder) computeClosure(conf *Configuration) *Configuration {
	toExpandQueue := utils.NewQueue[*DotProduction]()
	closure := make(map[int]*DotProduction)

	for _, prod := range conf.productions {
		toExpandQueue.Push(prod)
		closure[prod.dotProdId] = prod
	}

	for toExpandQueue.Size() > 0 { 
		prodToExpand := toExpandQueue.Pop()
		if prodToExpand.IsFullyComputed() || prodToExpand.NextSymbol().T == TERMINAL {
			continue
		}

		var lookahead *utils.Set[string]
		if prodToExpand.HasLookaheadSymbol() {
			laSymbol := prodToExpand.LookaheadSymbol()
			if laSymbol.T == TERMINAL {
				lookahead = utils.SetOf(laSymbol.Val)
			} else {
				lookahead = tb.firstSets[laSymbol.Val]
				if lookahead.Size() == 0 {
					panic("Nullable nonterminals not supported")
				}
			}
		} else {
			lookahead = prodToExpand.lookahead
		}

		curSym := prodToExpand.NextSymbol()
		for _, prod := range tb.nonTerminalToProds[curSym.Val] {
			dotprod := tb.makeDotProd(prod)
			if dp, alreadyAdded := closure[dotprod.dotProdId]; alreadyAdded {
				if !lookahead.Difference(dp.lookahead).IsEmpty() {
					dp.lookahead = dp.lookahead.Union(lookahead)
					toExpandQueue.Push(dp)
				}
			} else {
				dotprod.lookahead = lookahead
				toExpandQueue.Push(dotprod)
				closure[dotprod.dotProdId] = dotprod
			}
		}
	}

	res := EmptyConfiguration()
	for _, dotprod := range closure {
		res.AddDotProd(dotprod)
	}

	return res
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

func (tb *TableBuilder) computeClosureOptimized(conf *Configuration) *Configuration {
	presentDotProds := make(map[int]*DotProduction)
	for _, prod := range conf.productions {
		presentDotProds[prod.dotProdId] = prod
	}

	for _, prod := range conf.productions {
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
	}

	// TODO consider sorting it based on dotprodId, or better using tree instead of hashmap
	res := EmptyConfiguration()
	for _, dp := range presentDotProds {
		res.AddDotProd(dp)
	}
	return res
}

func (tb *TableBuilder) mergeAllConfigurations() {
	confLookupMap := make(map[ConfigurationKey]int)
	confIdRemap := make([]int, len(tb.configurations))
	newConfigurations := make([]*Configuration, 0)
	for confId, conf := range tb.configurations {
		if oldConfId, ok := confLookupMap[conf.Key()]; ok {
			tb.mergeConfigurations(newConfigurations[oldConfId], conf, oldConfId)
			confIdRemap[confId] = oldConfId
		} else {
			confIdRemap[confId] = len(newConfigurations)
			newConfigurations = append(newConfigurations, conf)
		}
	}
	newSuccessors := make([]map[string]int, len(newConfigurations))
	for succId := range newSuccessors {
		newSuccessors[succId] = make(map[string]int)
	}

	for confId := range tb.configurations {
		for symbol, oldSuccessorId := range tb.successors[confId] {
			newSuccessors[confIdRemap[confId]][symbol] = confIdRemap[oldSuccessorId]
		}
	}

	tb.configurations = newConfigurations
	tb.successors = newSuccessors
}

func (tb *TableBuilder) addConfiguration(conf *Configuration) (confId int) {
	confId = len(tb.configurations)
	tb.configurationsLookupMap[conf.Key()] = confId
	tb.configurations = append(tb.configurations, conf)
	tb.successors = append(tb.successors, map[string]int{})
	tb.actionTable.RegisterConfiguration()
	tb.gotoTable.RegisterConfiguration()
	tb.bruteForceCLM[conf.Key()] = append(tb.bruteForceCLM[conf.Key()], confId)
	return
}

func (tb *TableBuilder) addReduceEntries(confId int, dotprod *DotProduction) {
	for _, laTerminal := range dotprod.lookahead.GetAll() {
		tb.actionTable.AddReduceEntry(State(confId), laTerminal, dotprod.prod)
	}
}

func (tb *TableBuilder) addShiftGotoEntries(confId int, dotprod *DotProduction, nextConfId int) {
	nextSymbol := dotprod.NextSymbol()
	if nextSymbol.T == TERMINAL {
		tb.actionTable.AddShiftEntry(State(confId), nextSymbol.Val, nextConfId)
	} else {
		tb.gotoTable.AddEntry(State(confId), nextSymbol.Val, nextConfId)
	}
}

func (tb *TableBuilder) mergeConfigurations(oldConf *Configuration, newConf *Configuration, oldConfId int) {
	for prodIdx := range newConf.productions {
		// same order is guaranteed by key construction algo
		newLookaheads := newConf.productions[prodIdx].lookahead
		oldLookaheads := oldConf.productions[prodIdx].lookahead
		prod := oldConf.productions[prodIdx]
		prod.lookahead = newLookaheads.Union(oldLookaheads)
	}
}

func (tb *TableBuilder) expandConfiguration(conf *Configuration, confId int) {
	for _, dotprods := range conf.JoinProdsByNextSymbol() {
		shiftedProds := []*DotProduction{}
		for _, dotprod := range dotprods {
			shiftedProds = append(shiftedProds, dotprod.CreateShiftedRight())
		}
		newConf := tb.computeClosure(ConfigurationFrom(shiftedProds...))

		// there is a problem with adhoc merging because we don't fix lookaheads of successors
		// var nextConfId int
		// if id, alreadyPresent := tb.configurationsLookupMap[newConf.Key()]; alreadyPresent {
		// 	oldConf := tb.configurations[id]
		// 	tb.mergeConfigurations(oldConf, newConf, id)
		// 	nextConfId = id
		// } else {
		// 	nextConfId = tb.addConfiguration(newConf)
		// }
		var nextConfId int
		foundMatch := false
		if ids, alreadyPresent := tb.bruteForceCLM[newConf.Key()]; alreadyPresent {
			for _, id := range ids {
				cnf := tb.configurations[id]
				isSame := true
				for prodIdx := range newConf.productions {
					if !cnf.productions[prodIdx].lookahead.Equals(newConf.productions[prodIdx].lookahead) {
						isSame = false
						break
					}
				}
				if isSame {
					foundMatch = true
					nextConfId = id
					break
				}
			}
		} else {
			tb.bruteForceCLM[newConf.Key()] = make([]int, 0)
		}
		
		if !foundMatch {
			nextConfId = tb.addConfiguration(newConf)
		}
		tb.successors[confId][dotprods[0].NextSymbol().Val] = nextConfId
	}
}

func (tb *TableBuilder) fillTables() {
	for confId, conf := range tb.configurations {
		for _, fullyComputedProd := range conf.getFullyComputedProds() {
			tb.addReduceEntries(confId, fullyComputedProd)	
		}
		for _, prod := range conf.JoinProdsByNextSymbol() {
			tb.addShiftGotoEntries(confId, prod[0], tb.successors[confId][prod[0].NextSymbol().Val])
		}
	}
}

func (tb *TableBuilder) BuildConfigurationAutomaton() (*ActionTable, *GotoTable) {
	tb.initDataStructures()
	if len(tb.nonTerminalToProds[tb.grammar.StartNonterminal]) != 1 {
		panic("expected single production from artificially added start symbol")
	} 

	startConf := EmptyConfiguration()
	initialProd := tb.makeDotProd(tb.nonTerminalToProds[tb.grammar.StartNonterminal][0])
	initialProd.lookahead = utils.SetOf[string](tokenizers.EOF)
	startConf.AddDotProd(initialProd)
	startConf = tb.computeClosure(startConf)
	tb.addConfiguration(startConf)
	expandedConfigurationsCount := 0

	for expandedConfigurationsCount < len(tb.configurations) {
		conf := tb.configurations[expandedConfigurationsCount]
		tb.expandConfiguration(conf, expandedConfigurationsCount)
		expandedConfigurationsCount++
	}
	tb.mergeAllConfigurations()
	tb.fillTables()
	return tb.actionTable, tb.gotoTable
}

func (tb *TableBuilder) PrintConfigurations() {
	for id, c := range tb.configurations {
		c.Print(id)
	}
}
