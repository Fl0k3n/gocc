package semantics

import (
	"ast"
	"errors"
	"fmt"
	"symtabs"
	"utils"
)


type SemanticAnalyzer struct {
	errorTracker *ErrorTracker
	funcGotoLabels *utils.Set[string]
	funcGotosToCheck *utils.Set[string]
	symtab *symtabs.Symtab[Symbol]
	typeEngine *TypeEngine
}

func NewAnalyzer(et *ErrorTracker) *SemanticAnalyzer {
	s := &SemanticAnalyzer{
		errorTracker: et,
		symtab: symtabs.NewSymtab[Symbol](),
	}
	typeEngine := NewEngine(s, et)
	s.typeEngine = typeEngine
	s.enterGlobalScope()
	return s
}

func (s *SemanticAnalyzer) TypeOf(symname string) (t Ctype, ok bool)  {
	if sym, ok := s.symtab.Lookup(symname); ok {
		return sym.Type, true
	}
	return nil, false
}

func (s *SemanticAnalyzer) getDeclarationSymbolsAndTypeCheckInitializers(dec *ast.Declaration) ([]Symbol, error) {
	if dec.InitDeclaratorList == nil {
		return nil, errors.New("Missing indentifier")
	}
	if len(dec.DeclarationSpecifiers.StorageClassSpecifiers) != 0 {
		s.errorTracker.registerSemanticError("Storage class specifiers illegal here", dec.LineInfo)	
	}

	partialType, err := s.typeEngine.getPartialTypeFromSpecifiers(dec.DeclarationSpecifiers.TypeSpecifiers)
	if err != nil {
		return nil, err // or fallback to void* for further type checking?
	}	

	symbols := make([]Symbol, 0)
	for _, initDecl := range dec.InitDeclaratorList.InitDeclarators {
		t, name := s.typeEngine.extractTypeAndName(initDecl.Declarator, partialType)
		if initDecl.Initializer != nil {
			if err := s.typeEngine.typeCheckInitializer(t, initDecl.Initializer); err != nil {
				s.errorTracker.registerTypeError("Invalid initializer type", initDecl.LineInfo)
			}
		}
		if b, isBuiltin := t.(BuiltinCtype); isBuiltin {
			if b.Builtin == VOID {
				s.errorTracker.registerTypeError("Can't declare void variables", initDecl.LineInfo)
				t = VOID_POINTER
			}
		}
		symbols = append(symbols, Symbol{Name: name, Type: t, LineInfo: initDecl.LineInfo})
	}
	return symbols, nil
}

func (s *SemanticAnalyzer) enterGlobalScope() {
	s.symtab.EnterScope()
}

func (s *SemanticAnalyzer) addSymbolsFromDeclarationListToCurrentScope(dl *ast.DeclarationList) {
	for _, dec := range dl.Declarations {
		declaredSymbols, err := s.getDeclarationSymbolsAndTypeCheckInitializers(dec)
		if err != nil {
			s.errorTracker.registerSemanticError(err.Error(), dl.LineInfo)
		} else {
			for _, sym := range declaredSymbols {
				s.defineSymbol(sym)
			}
		}
	}
}

func (s *SemanticAnalyzer) checkSwitchSemantics(switchStmnt ast.SwitchSelectionStatement) (terminateFurtherChecks bool) {
	foundDefault := false
	foundCase := false
	isEmpty := true
	terminateFurtherChecks = false
	if body, isCompound := switchStmnt.SwitchBody.(ast.CompoundStatement); isCompound {
		if body.DeclarationList != nil {
			s.errorTracker.registerSemanticError("Declarations are illegal inside switch statement", switchStmnt.LineInfo)
		}
		if body.StatementList != nil {
			isEmpty = false
		}
		for _, stmnt := range body.StatementList.Statements {
			if _, isCase := stmnt.(ast.CaseLabeledStatement); isCase {
				if foundDefault {
					s.errorTracker.registerSemanticError("Default statement must be the last switch control statement", switchStmnt.LineInfo)
				}
				foundCase = true
			} else if _, isDefault := stmnt.(ast.DefaultLabeledStatement); isDefault {
				if foundDefault {
					s.errorTracker.registerSemanticError("At most one default statement is allowed", switchStmnt.LineInfo)
				}
				foundDefault = true
			} else if !foundCase && !foundDefault {
				s.errorTracker.registerSemanticError("Case or default must be the first switch statement", switchStmnt.LineInfo)
				terminateFurtherChecks = true
			}
		}
	} else {
		s.errorTracker.registerSemanticError("Switch body must be a compound statement", switchStmnt.LineInfo)
		terminateFurtherChecks = true
	}
	if isEmpty {
		s.errorTracker.registerSemanticError("Illegal empty switch statement", switchStmnt.LineInfo)
		terminateFurtherChecks = true
	}
	return
}

func (s *SemanticAnalyzer) handleStatement(statement ast.Statement, ctx StatementContext) {
	switch stmnt := statement.(type) {
	case ast.CompoundStatement:
		s.symtab.EnterScope()
		defer s.symtab.LeaveScope()
		s.handleCompoundStatement(&stmnt, ctx)
	case ast.CaseLabeledStatement:
		if !ctx.ExpectsCase {
			s.errorTracker.registerSemanticError("unexpected case statement not inside switch", stmnt.LineInfo)
		} else if err := s.typeEngine.checkCaseExpression(stmnt.Expression, ctx.CaseExpressionType); err != nil {
			s.errorTracker.registerTypeError(err.Error(), stmnt.LineInfo)
		}
		s.handleStatement(stmnt.Statement, ctx.WithDisallowedCase().And().WithAllowedBreak())
	case ast.DefaultLabeledStatement:
		if !ctx.ExpectsCase {
			s.errorTracker.registerSemanticError("unexpected default statement not inside switch", stmnt.LineInfo)
		} else {
			s.handleStatement(stmnt.Statement, ctx.WithDisallowedCase().And().WithAllowedBreak())
		}
	case ast.IdentifierLabeledStatement:
		if s.funcGotoLabels.Has(stmnt.Identifier) {
			s.errorTracker.registerSemanticError("Illegal multiple usage of same label", stmnt.LineInfo)
		} else {
			s.funcGotoLabels.Add(stmnt.Identifier)
		}
		s.handleStatement(stmnt.Statement, ctx)
	case ast.ExpressionStatement:
		if stmnt.Expression != nil {
			if _, err := s.typeEngine.getTypeOfExpression(stmnt.Expression); err != nil {
				s.errorTracker.registerTypeError(err.Error(), stmnt.LineInfo)
			}
		}
	case ast.IfSelectionStatement:
		if err := s.typeEngine.checkCondition(stmnt.Condition); err != nil {
			s.errorTracker.registerTypeError(err.Error(), stmnt.LineInfo)
		}
		s.handleStatement(stmnt.IfStatement, ctx)
		if stmnt.ElseStatement != nil {
			s.handleStatement(stmnt.ElseStatement, ctx)
		}
	case ast.SwitchSelectionStatement:
		if switchExprT, err := s.typeEngine.getTypeOfExpression(stmnt.SwitchExpression); err != nil {
			s.errorTracker.registerTypeError(err.Error(), stmnt.LineInfo)
		} else if err = s.typeEngine.checkSwitchExpressionType(switchExprT); err != nil {
			s.errorTracker.registerTypeError(err.Error(), stmnt.LineInfo)
		} else {
			terminateFurtherChecking := s.checkSwitchSemantics(stmnt) 
			if !terminateFurtherChecking {
				compStmnt := stmnt.SwitchBody.(ast.CompoundStatement)
				for _, nestedStmnt := range compStmnt.StatementList.Statements {
					s.handleStatement(nestedStmnt, ctx.WithExpectedCase(switchExprT).And().WithAllowedBreak())
				}
			}
		}
	case ast.WhileIterationStatement:
		if err := s.typeEngine.checkCondition(stmnt.Condition); err != nil {
			s.errorTracker.registerTypeError(err.Error(), stmnt.LineInfo)
		}
		s.handleStatement(stmnt.Body, ctx.WithAllowedBreak())
	case ast.DoWhileIterationStatement:
		if err := s.typeEngine.checkCondition(stmnt.Condition); err != nil {
			s.errorTracker.registerTypeError(err.Error(), stmnt.LineInfo)
		}
		s.handleStatement(stmnt.Body, ctx.WithAllowedBreak())
	case ast.ForIterationStatement:
	    s.handleStatement(stmnt.Initializer, ctx)
		if stmnt.Condition.Expression != nil {
			if err := s.typeEngine.checkCondition(stmnt.Condition.Expression); err != nil {
				s.errorTracker.registerTypeError(err.Error(), stmnt.LineInfo)
			}
		}
		if stmnt.Updater != nil {
			if _, err := s.typeEngine.getTypeOfExpression(stmnt.Updater); err != nil {
				s.errorTracker.registerTypeError(err.Error(), stmnt.LineInfo)
			}
		}
		s.handleStatement(stmnt.Body, ctx.WithAllowedBreak())
	case ast.LoopControlJumpStatement:
		if !ctx.CanUseBreak {
			s.errorTracker.registerSemanticError("Illegal break not inside in loop or switch", stmnt.LineInfo)
		}
	case ast.GotoJumpStatement:
		s.funcGotosToCheck.Add(stmnt.Label)
	case ast.ReturnJumpStatement:
		if err := s.typeEngine.checkReturnExpression(stmnt.Expression, ctx.RequiredReturnType); err != nil {
			s.errorTracker.registerTypeError(err.Error(), stmnt.LineInfo)
		}
	default:
		panic("Unexpected statement")
	}
}

func (s *SemanticAnalyzer) handleCompoundStatement(cs *ast.CompoundStatement, ctx StatementContext) {
	if cs.DeclarationList != nil {
		s.addSymbolsFromDeclarationListToCurrentScope(cs.DeclarationList)
	}
	if cs.StatementList != nil {
		for _, stmnt := range cs.StatementList.Statements {
			s.handleStatement(stmnt, ctx)
		}
	}
}

func (s *SemanticAnalyzer) handleFunctionDefinition(fun *ast.FunctionDefinition) {
	s.funcGotoLabels = utils.NewSet[string]()
	s.funcGotosToCheck = utils.NewSet[string]()
	
	fdef := s.typeEngine.GetFunctionDeclaration(fun)
	s.defineSymbol(Symbol{Name: fdef.name, Type: fdef, LineInfo: fun.LineInfo})
	if len(fdef.ParamNames) != len(fdef.ParamTypes) {
		s.errorTracker.registerSemanticError("Function definition must contain all parameter names", fun.LineInfo)
		return
	}
	s.symtab.EnterScope()
	defer s.symtab.LeaveScope()

	for paramNum := range fdef.ParamTypes {
		s.defineSymbol(Symbol{Name: fdef.ParamNames[paramNum], Type: fdef.ParamTypes[paramNum]})
	}
	s.handleCompoundStatement(fun.Body, StatementContext{
		CanUseBreak: false,
		ExpectsCase: false,
		RequiredReturnType: fdef.ReturnType,
	})

	for _, label := range s.funcGotosToCheck.GetAll() {
		if !s.funcGotoLabels.Has(label) {
			s.errorTracker.registerSemanticError("Undefined label", fun.LineInfo) // TODO more specific line info
		}
	}
}

func (s *SemanticAnalyzer) handleTopLevelDeclaration(dec *ast.Declaration) {
	definedSymbols := s.typeEngine.GetSymbolsForTopLevelDeclarationAndDefineNewTypes(dec)
	for _, sym := range definedSymbols {
		s.defineSymbol(sym)
	}
}

func (s *SemanticAnalyzer) defineSymbol(sym Symbol) {
	if prevDef, ok := s.symtab.HasInCurrentScope(sym.Name); ok {
		s.errorTracker.registerSemanticError(fmt.Sprintf("Redefinition of symbol %s previously defined in %d",
							sym.Name, prevDef.LineInfo.LineNumber), sym.LineInfo)
	} else {
		s.symtab.Define(sym.Name, sym)
	}
}

func (s *SemanticAnalyzer) Analyze(root *ast.TranslationUnit) {
	for _, dec := range root.ExternalDeclarations {
		switch declaration := dec.(type) {
		case ast.FunctionDefinition:
			s.handleFunctionDefinition(&declaration)
		case ast.Declaration:
			s.handleTopLevelDeclaration(&declaration)
		}
	}
}
