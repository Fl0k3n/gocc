package irs

import (
	"ast"
	"fmt"
	"semantics"
)


type ScopeManager struct {
	symtab *CountingSymtab
	typeEngine *semantics.TypeEngine
}

func newScopeManager(typeEngine *semantics.TypeEngine) *ScopeManager {
	return &ScopeManager{
		symtab: NewCountingSymtab(),
		typeEngine: typeEngine,
	}
}

func (sm *ScopeManager) TypeOf(symname string) (t semantics.Ctype, ok bool) {
	if sym, ok := sm.symtab.Lookup(symname); ok {
		return sym.Ctype, true
	}
	fmt.Println("Warning: undefined symbol " + symname)
	return nil, false
}

func (s *ScopeManager) getSymbol(name string) *Symbol {
	res, ok := s.symtab.Lookup(name)
	if !ok {
		panic("Undefined symbol " + name)
	}
	return res
}

func (s *ScopeManager) EnterFunction(f *ast.FunctionDefinition) (name string) {
	fptr := s.typeEngine.GetFunctionDeclaration(f)
	s.symtab.DefineNewOfType(fptr.Name(), GLOBAL, fptr)
	s.symtab.EnterScope()
	for paramNum := range fptr.ParamTypes {
		s.symtab.DefineNewOfType(fptr.ParamNames[paramNum], ARG, fptr.ParamTypes[paramNum])
	}
	return fptr.Name()
}

func (s *ScopeManager) LeaveFunction() {
	s.symtab.LeaveScope()
}

func (s *ScopeManager) EnterGlobalScope() {
	s.symtab.EnterScope()
}

func (s *ScopeManager) EnterCompoundStatement() {
	s.symtab.EnterScope()
}

func (s *ScopeManager) LeaveCompoundStatement() {
	s.symtab.LeaveScope()
}

func (s *ScopeManager) newTemp(t semantics.Ctype) *Symbol {
	tmpCounter := s.symtab.SymbolsOfType(TEMP)
	name := fmt.Sprintf("%dt", tmpCounter)
	return s.symtab.DefineNewOfType(name, TEMP, t)
}

func (s *ScopeManager) newLocalVariable(name string, t semantics.Ctype) *Symbol {
	return s.symtab.DefineNewOfType(name, LOCAL, t)
}
