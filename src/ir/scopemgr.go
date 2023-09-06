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
	ArgsSnapshot []*Symbol
	LocalsSnapshot []*Symbol
	// TODO if we reuse temps each snapshot should have a mark till which ir line given type is valid
	TempsSnapshot []*Symbol
}

func newSnapshot() *NonGlobalsSnapshot {
	return &NonGlobalsSnapshot{
		ArgsSnapshot: []*Symbol{},
		LocalsSnapshot: []*Symbol{},
		TempsSnapshot: []*Symbol{},
	}
}

type ScopeManager struct {
	symtab *CountingSymtab
	typeEngine *semantics.TypeEngine
	statementContexts *utils.Stack[StatementContext]
	curFuncSnapshot *NonGlobalsSnapshot
	globalSymbols []*GlobalSymbol
}

func newScopeManager(typeEngine *semantics.TypeEngine) *ScopeManager {
	return &ScopeManager{
		symtab: NewCountingSymtab(),
		typeEngine: typeEngine,
		statementContexts: utils.NewStack[StatementContext](),
		globalSymbols: []*GlobalSymbol{},
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

func (s *ScopeManager) EnterFunction(f *ast.FunctionDefinition, static bool) *semantics.FunctionPtrCtype {
	s.curFuncSnapshot = newSnapshot()
	fptr := s.typeEngine.GetFunctionDeclaration(f)
	s.newGlobalVariable(fptr.Name(), fptr, true, static, false, nil)
	s.symtab.EnterScope(true)
	for paramNum, paramType := range fptr.ParamTypes {
		sym := s.symtab.DefineNewOfType(fptr.ParamNames[paramNum], ARG, paramType)
		s.curFuncSnapshot.ArgsSnapshot = append(s.curFuncSnapshot.ArgsSnapshot, sym)
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
	tmpCounter := s.symtab.SymbolsOfType(TEMP)
	name := fmt.Sprintf("%dt", tmpCounter)
	sym := s.symtab.DefineNewOfType(name, TEMP, t)
	s.curFuncSnapshot.TempsSnapshot = append(s.curFuncSnapshot.TempsSnapshot, sym)
	return sym
}

func (s *ScopeManager) newLocalVariable(name string, t semantics.Ctype) *Symbol {
	sym := s.symtab.DefineNewOfType(name, LOCAL, t)
	s.curFuncSnapshot.LocalsSnapshot = append(s.curFuncSnapshot.LocalsSnapshot, sym)
	return sym
}

func (s *ScopeManager) newGlobalVariable(name string, t semantics.Ctype, function bool, static bool, extern bool, initializers []*GlobalInitializer) *Symbol {
	sym := s.symtab.DefineNewOfType(name, GLOBAL, t)
	s.globalSymbols = append(s.globalSymbols, &GlobalSymbol{
		Symbol: sym,
		IsExtern: extern,
		IsStatic: static,
		IsFunction: function,
		Initializers: initializers,
	})
	return sym
}

func (s *ScopeManager) GetNonGlobalsSnapshot() *NonGlobalsSnapshot {
	return s.curFuncSnapshot
}

func (s *ScopeManager) GetGlobalSymbols() []*GlobalSymbol {
	return s.globalSymbols
}
