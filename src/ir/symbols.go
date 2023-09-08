package irs

import (
	"ast"
	"semantics"
	"symtabs"
)

type SymbolType int

const (
	ARG SymbolType = iota
	LOCAL
	GLOBAL
	TEMP
)

type SymbolName string

type Symbol struct {
	Name string
	T SymbolType
	Index int
	Ctype semantics.Ctype
}

type GlobalInitializer struct {
	Offset int
	Expression ast.Expression // TODO turn this into binary value
}

type GlobalSymbol struct {
	Symbol *Symbol
	IsExtern bool
	IsStatic bool
	IsFunction bool // function, not function pointer
	Initializers []*GlobalInitializer
}

type CountingSymtab struct {
	symtabs.Symtab[*Symbol]
	counters map[SymbolType]int
	scopeCounters map[SymbolType]int
}

func newZeroedSymbolCounters() map[SymbolType]int {
	return map[SymbolType]int{
		ARG: 0,
		LOCAL: 0,
		GLOBAL: 0,
		TEMP: 0,
	}
}

func NewCountingSymtab() *CountingSymtab {
	return &CountingSymtab{
		Symtab: *symtabs.NewSymtab[*Symbol](),
		counters: newZeroedSymbolCounters(),
	}
}

func (cs *CountingSymtab) DefineNewOfType(symname string, symbolT SymbolType, t semantics.Ctype) *Symbol {
	sym := Symbol{
		Name: symname,
		T: symbolT,
		Ctype: t,
	}
	return cs.Define(symname, &sym)
}

func (cs *CountingSymtab) Define(symname string, info *Symbol) *Symbol {
	info.Index = cs.counters[info.T]
	cs.counters[info.T]++
	cs.scopeCounters[info.T]++
	cs.Symtab.Define(symname, info)
	return info
}

func (cs *CountingSymtab) LeaveScope(subtractScopeCounters bool) {
	cs.Symtab.LeaveScope()
	if subtractScopeCounters {
		for k, v := range cs.scopeCounters {
			cs.counters[k] -= v
			cs.scopeCounters[k] = 0
		}
	}
}

func (cs *CountingSymtab) EnterScope(resetScopeCounters bool) {
	cs.Symtab.EnterScope()
	if resetScopeCounters {
		cs.scopeCounters = newZeroedSymbolCounters()
	}
}

func (cs *CountingSymtab) SymbolsOfType(t SymbolType) int {
	return cs.counters[t]
}
