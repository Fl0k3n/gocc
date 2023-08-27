package irs

import (
	"ast"
	"fmt"
	"types"
)


type ScopeManager struct {
	symtab *CountingSymtab
	functab map[string][]*types.FunctionDefinition
}

func newScopeManager() *ScopeManager {
	return &ScopeManager{
		symtab: NewCountingSymtab(),
		functab: map[string][]*types.FunctionDefinition{},
	}
}

func (s *ScopeManager) getSymbol(name string) *Symbol {
	res, ok := s.symtab.Lookup(name)
	if !ok {
		panic("Undefined symbol " + name)
	}
	return res
}

func (s *ScopeManager) EnterFunction(f *ast.FunctionDefinition) {
	s.symtab.EnterScope()
	s.typesystem.EnterFunction(f) // TODO define symbols, types and names, also handle body (top level compound statement)
}

func (s *ScopeManager) LeaveFunction() {
	s.symtab.LeaveScope()
	s.typesystem.LeaveFunction()
}

func (s *ScopeManager) EnterGlobalScope() {
	s.symtab.EnterScope()
	s.typesystem.EnterScope()
}

func (s *ScopeManager) EnterCompoundStatement(cs *ast.CompoundStatement) {
	s.symtab.EnterScope()
	s.typesystem.EnterCompoundStatement(cs)
}

func (s *ScopeManager) LeaveCompoundStatement(cs *ast.CompoundStatement) {
	s.symtab.LeaveScope()
}

func (s *ScopeManager) newTemp(t types.Ctype) *Symbol {
	tmpCounter := s.symtab.SymbolsOfType(TEMP)
	name := fmt.Sprintf("%dt", tmpCounter)
	return s.symtab.DefineNewOfType(name, TEMP, t)
}

func (s *ScopeManager) IsUsingFunctionPointerCall(expr ast.FunctionCallPostfixExpression) bool {
	return s.typesystem.IsUsingFunctionPointer(expr)
}