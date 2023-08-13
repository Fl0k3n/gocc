package parsers

import (
	. "grammars"
	"tokenizers"
	"utils"
)

func (p *Parser) initDataStructures() {
	p.configurations = make([]*Configuration, 0)
	p.configurationsLookupMap = make(map[ConfigurationKey]int)
	p.actionTable = newActionTable()
	p.gotoTable = newGotoTable()
	p.initNonterminalToProductionsMap()
	p.initClosuresMap()
	p.enumerateDotProductions()
	p.initFirstSets()
	p.buildConfigurationAutomaton()
}

func (p *Parser) initNonterminalToProductionsMap() {
	p.nonTerminalToProds = make(map[string][]*Production)
	for _, prod := range p.grammar.Productions {
		otherProds, ok := p.nonTerminalToProds[prod.From]
		if ok {
			p.nonTerminalToProds[prod.From] = append(otherProds, prod)
		} else {
			prods := make([]*Production, 1)
			prods[0] = prod
			p.nonTerminalToProds[prod.From] = prods
		}
	}
}

func (p *Parser) initClosuresMap() {
	p.closures = make(map[string][]*Production)
	closures := make(map[string]*utils.Set[*Production])
	toExpandQueue := utils.NewQueue[*Production]()

	for _, nt := range p.grammar.Nonterminals {
		curClousures := utils.NewSet[*Production]()

		for _, prod := range p.nonTerminalToProds[nt] {
			curClousures.Add(prod)
			if p.canExpandProd(prod) {
				toExpandQueue.Push(prod)
			}
		}

		for toExpandQueue.Size() > 0 { 
			prodToExpand := toExpandQueue.Pop()

			if expanded, alreadyExpanded := closures[prodToExpand.To[0].Val]; alreadyExpanded {
				curClousures.AddAll(expanded.GetAll())
			} else {
				for _, prod := range p.nonTerminalToProds[prodToExpand.To[0].Val] {
					if !curClousures.Has(prod) {
						if p.canExpandProd(prod) {
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
		p.closures[k] = v.GetAll()
	}
}

func (p *Parser) canExpandProd(prod *Production) bool {
	return len(prod.To) > 0 && prod.To[0].T == NONTERMINAL
}

func (p* Parser) initFirstSets() {
	// assuming that every nonterminal is not nullable (as its the case for selected ANSI C grammar)
	p.firstSets = make(map[string]*utils.Set[string])
	nonterminalsQueue := utils.NewQueue[string]()

	for _, nt := range p.grammar.Nonterminals {
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

		for _, prod := range p.nonTerminalToProds[nt] {
			handleFirstSymbol(prod)
		}

		for nonterminalsQueue.Size() > 0 {
			nextNt := nonterminalsQueue.Pop()
			if theirFirstSet, alreadyComputed := p.firstSets[nextNt]; alreadyComputed {
				firstSet.AddAll(theirFirstSet.GetAll())
			} else {
				for _, prod := range p.nonTerminalToProds[nextNt] {
					handleFirstSymbol(prod)
				}
			}
		}
		p.firstSets[nt] = firstSet
	}
}

func (p *Parser) enumerateDotProductions() {
	p.prodEnumerations = make([]int, len(p.grammar.Productions))
	counter := 0
	for idx, prod := range p.grammar.Productions {
		if idx != int(prod.ProdId) {
			panic("expected prodId to equal id in grammar")
		}
		p.prodEnumerations[prod.ProdId] = counter
		counter += 1 + len(prod.To)
	}
}

func (p *Parser) makeDotProd(prod *Production) *DotProduction {
	return FromProduction(
		prod,
		p.prodEnumerations[prod.ProdId],
		utils.NewSet[string](),
	)
}

func (p *Parser) buildLookaheads(parentLookaheads *utils.Set[string], startNt string) map[string]*utils.Set[string] {
	ntToLookaheads := map[string]*utils.Set[string]{}
	closures := p.closures[startNt]

	for _, closureProd := range closures {
		dotprod := p.makeDotProd(closureProd)
		if curSym := dotprod.NextSymbol(); curSym.T == NONTERMINAL { 
			if _, ok := ntToLookaheads[curSym.Val]; !ok {
				ntToLookaheads[curSym.Val] = utils.NewSet[string]()
			}
			if dotprod.HasLookaheadSymbol() {
				nextSym := dotprod.LookaheadSymbol()
				if nextSym.T == TERMINAL {
					ntToLookaheads[curSym.Val].Add(nextSym.Val)
				} else {
					ntToLookaheads[curSym.Val].AddAll(p.firstSets[nextSym.Val].GetAll())
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

func (p *Parser) computeClosure(conf *Configuration) *Configuration {
	presentDotProds := make(map[int]*DotProduction)
	prod := conf.productions[0]
	presentDotProds[prod.dotProdId] = prod

	if !prod.IsFullyComputed() && prod.NextSymbol().T == NONTERMINAL {
		nextNt := prod.NextSymbol().Val
		lookaheads := p.buildLookaheads(prod.lookahead, nextNt)
		for _, closureProd := range p.closures[nextNt] {
			dotProd := p.makeDotProd(closureProd)
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


func (p *Parser) addConfiguration(conf *Configuration) (confId int) {
	confId = len(p.configurations)
	p.configurationsLookupMap[conf.Key()] = confId
	p.configurations = append(p.configurations, conf)
	p.actionTable.RegisterConfiguration()
	p.gotoTable.RegisterConfiguration()
	return
}

func (p *Parser) expandConfiguration(conf *Configuration, confId int) {
	for _, dotprod := range conf.productions {
		if dotprod.IsFullyComputed() {
			if dotprod.lookahead.Size() == 0 {
				p.actionTable.AddReduceEntry(confId, tokenizers.EOF, dotprod.prod)
			} else {
				for _, laTerminal := range dotprod.lookahead.GetAll() {
					p.actionTable.AddReduceEntry(confId, laTerminal, dotprod.prod)
				}
			}
		} else {
			shiftedProd := dotprod.CreateShiftedRight()
			newConf := p.computeClosure(ConfigurationFrom(shiftedProd))
			var nextConfId int
			if id, alreadyPresent := p.configurationsLookupMap[newConf.Key()]; alreadyPresent {
				oldConf := p.configurations[id]
				for prodIdx := range newConf.productions {
					// same order is guaranteed by key construction algo
					newLookaheads := newConf.productions[prodIdx].lookahead
					oldLookaheads := oldConf.productions[prodIdx].lookahead
					oldConf.productions[prodIdx].lookahead  = newLookaheads.Union(oldLookaheads)
				}
				nextConfId = id
			} else {
				nextConfId = p.addConfiguration(newConf)
			}
			nextSymbol := dotprod.NextSymbol()

			if nextSymbol.T == TERMINAL {
				p.actionTable.AddShiftEntry(confId, nextSymbol.Val, nextConfId)
			} else {
				p.gotoTable.AddEntry(confId, nextSymbol.Val, nextConfId)
			}
		}
	}
}

func (p *Parser) buildConfigurationAutomaton() {
	if len(p.nonTerminalToProds[p.grammar.StartNonterminal]) != 1 {
		panic("expected single production from artificially added start symbol")
	} 

	startConf := EmptyConfiguration()
	startConf.AddDotProd(p.makeDotProd(p.nonTerminalToProds[p.grammar.StartNonterminal][0]))
	startConf = p.computeClosure(startConf)
	p.addConfiguration(startConf)
	expandedConfigurationsCount := 0

	for expandedConfigurationsCount < len(p.configurations) {
		conf := p.configurations[expandedConfigurationsCount]
		p.expandConfiguration(conf, expandedConfigurationsCount)
		expandedConfigurationsCount++
	}
}
