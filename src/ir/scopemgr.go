package irs

import (
	"ast"
	"fmt"
	"semantics"
	"utils"
)

type StatementContext struct {
	BreakLabel string
	ContinueLabel string
}

type NonGlobalsSnapshot struct {
	ArgsSnapshot []semantics.Ctype
	LocalsSnapshot []semantics.Ctype
	// TODO if we reuse temps each snapshot should have a mark till which ir line given type is valid
	TempsSnapshot []semantics.Ctype
}

func newSnapshot() *NonGlobalsSnapshot {
	return &NonGlobalsSnapshot{
		ArgsSnapshot: []semantics.Ctype{},
		LocalsSnapshot: []semantics.Ctype{},
		TempsSnapshot: []semantics.Ctype{},
	}
}

type ScopeManager struct {
	symtab *CountingSymtab
	typeEngine *semantics.TypeEngine
	statementContexts *utils.Stack[StatementContext]
	curFuncSnapshot *NonGlobalsSnapshot
}

func newScopeManager(typeEngine *semantics.TypeEngine) *ScopeManager {
	return &ScopeManager{
		symtab: NewCountingSymtab(),
		typeEngine: typeEngine,
		statementContexts: utils.NewStack[StatementContext](),
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

func (s *ScopeManager) EnterFunction(f *ast.FunctionDefinition) *semantics.FunctionPtrCtype {
	s.curFuncSnapshot = newSnapshot()
	fptr := s.typeEngine.GetFunctionDeclaration(f)
	s.symtab.DefineNewOfType(fptr.Name(), GLOBAL, fptr)
	s.symtab.EnterScope(true)
	for paramNum, paramType := range fptr.ParamTypes {
		s.symtab.DefineNewOfType(fptr.ParamNames[paramNum], ARG, paramType)
		s.curFuncSnapshot.ArgsSnapshot = append(s.curFuncSnapshot.ArgsSnapshot, paramType)
	}
	return &fptr
}

func (s *ScopeManager) LeaveFunction() {
	s.symtab.LeaveScope(true)
}

func (s *ScopeManager) EnterGlobalScope() {
	s.symtab.EnterScope(true)
}

func (s *ScopeManager) EnterCompoundStatement() {
	s.symtab.EnterScope(false)
}

func (s *ScopeManager) LeaveCompoundStatement() {
	s.symtab.LeaveScope(false)
}

func (s *ScopeManager) EnterStatementContext(ctx StatementContext) {
	s.statementContexts.Push(ctx)
}

func (s *ScopeManager) LeaveStatementContext() {
	s.statementContexts.Pop()
}

func (s *ScopeManager) GetStatementContext() StatementContext {
	return s.statementContexts.Peek()
}

func (s *ScopeManager) newTemp(t semantics.Ctype) *Symbol {
	s.curFuncSnapshot.TempsSnapshot = append(s.curFuncSnapshot.TempsSnapshot, t)
	tmpCounter := s.symtab.SymbolsOfType(TEMP)
	name := fmt.Sprintf("%dt", tmpCounter)
	return s.symtab.DefineNewOfType(name, TEMP, t)
}

func (s *ScopeManager) newLocalVariable(name string, t semantics.Ctype) *Symbol {
	s.curFuncSnapshot.LocalsSnapshot = append(s.curFuncSnapshot.LocalsSnapshot, t)
	return s.symtab.DefineNewOfType(name, LOCAL, t)
}

func (s *ScopeManager) newGlobalVariable(name string, t semantics.Ctype) *Symbol {
	return s.symtab.DefineNewOfType(name, GLOBAL, t)
}

func (s *ScopeManager) GetNonGlobalsSnapshot() *NonGlobalsSnapshot {
	return s.curFuncSnapshot
}
