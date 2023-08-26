package irs

import (
	"symtabs"
	"types"
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
	Ctype types.Ctype
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

func (cs *CountingSymtab) DefineNewOfType(symname string, symbolT SymbolType, t types.Ctype) *Symbol {
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
	cs.Symtab.Define(symname, info)
	return info
}

func (cs *CountingSymtab) LeaveScope() {
	cs.Symtab.LeaveScope()
	for k, v := range cs.scopeCounters {
		cs.counters[k] -= v
	}
}

func (cs *CountingSymtab) EnterScope() {
	cs.Symtab.EnterScope()
	cs.scopeCounters = newZeroedSymbolCounters()
}

func (cs *CountingSymtab) SymbolsOfType(t SymbolType) int {
	return cs.counters[t]
}
