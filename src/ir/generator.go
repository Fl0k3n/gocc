package irs

import (
	"ast"
	"grammars"
	"semantics"
)

type IRGenerator struct {
	scopeMgr *ScopeManager
	typeEngine *semantics.TypeEngine
	writer *Writer
	labels *LabelProvider
	curFunctionName string
	globalSymbols []*GlobalSymbol
}

func NewGenerator() *IRGenerator {
	scopeMgr := newScopeManager(nil)
	typeEngine := semantics.NewEngine(scopeMgr, semantics.NewErrorTracker())
	scopeMgr.typeEngine = typeEngine // TODO untangle dependencies
	return &IRGenerator{
		scopeMgr: scopeMgr,
		writer: NewWriter(),
		typeEngine: typeEngine,
		labels: newLabelProvider(),
		globalSymbols: []*GlobalSymbol{},
	}
}

func (g *IRGenerator) flattenArrayAccessor(array semantics.ArrayCtype, dimensionSymbols []*Symbol) *Symbol {
	partialSumSymbol := dimensionSymbols[0]
	curMult := 1
	for i := 1; i < len(dimensionSymbols); i++ {
		curMult *= array.DimensionSizes[len(array.DimensionSizes) - i]
		nextIdx := dimensionSymbols[i]
		multipliedNextIdx := g.multiplyByIntConst(nextIdx, curMult)
		mergedSymbol := g.scopeMgr.newTemp(partialSumSymbol.Ctype)
		g.writer.WriteBinaryAddition(mergedSymbol, partialSumSymbol, multipliedNextIdx)
		// from -> to
		// if g.typeEngine.ShouldBeUpCasted(leftSymbol, rightSymbol) {
			// leftSymbol = g.cast(leftSymbol, rightSymbol.Ctype)
		// } else if g.typeEngine.ShouldBeUpCasted(rightSymbol, leftSymbol) {
			// rightSymbol = g.cast(rightSymbol, leftSymbol.Ctype)
		// }
		partialSumSymbol = mergedSymbol
	}
	return partialSumSymbol
}

func (g *IRGenerator) makeIntConst(c int) *Symbol {
	res := g.scopeMgr.newTemp(semantics.BuiltinFrom("int")) // TODO ?
	g.writer.WriteIntAssignment(res, c)
	return res
}

func (g *IRGenerator) multiplyByIntConst(sym *Symbol, c int) *Symbol {
	multiplierSymbol := g.makeIntConst(c)
	res := g.scopeMgr.newTemp(sym.Ctype)
	g.writer.WriteBinaryMultiplication(res, sym, multiplierSymbol)
	return res
}

func (g *IRGenerator) moveByOffset(base *Symbol, offset *Symbol) *Symbol {
	res := g.scopeMgr.newTemp(base.Ctype)
	g.writer.WriteBinaryAddition(res, base, offset)
	return res
}

func (g *IRGenerator) moveByIntOffset(base *Symbol, offset int) *Symbol {
	offsetSym := g.makeIntConst(offset)
	return g.moveByOffset(base, offsetSym)
}

func (g *IRGenerator) getAddress(sym *Symbol) *Symbol {
	res := g.scopeMgr.newTemp(semantics.PointerCtype{Target: sym.Ctype})
	g.writer.WriteAddressUnaryOperation(res, sym)
	return res
}

func (g *IRGenerator) dereference(sym *Symbol) *Symbol {
	resT := sym.Ctype.(semantics.PointerCtype)
	res := g.scopeMgr.newTemp(resT.Target)
	g.writer.WriteDereferenceUnaryOperation(res, sym)
	return res
}

func (g *IRGenerator) incrementOrDecrement(sym *Symbol, operator ast.IncDecOperator) *Symbol {
	res := g.scopeMgr.newTemp(sym.Ctype)
	valSymbol := g.makeIntConst(1)
	if operator == ast.INC {
		g.writer.WriteBinaryAddition(res, sym, valSymbol)
	} else {
		g.writer.WriteBinarySubtraction(res, sym, valSymbol)
	}
	g.writer.WriteBiSymbolAssignment(sym, res)
	return sym
}

func (g *IRGenerator) checkIsEqual(lhs *Symbol, rhs *Symbol) *Symbol {
	res := g.scopeMgr.newTemp(semantics.BuiltinFrom("int")) // TODO
	g.writer.WriteEqComparion(res, lhs, rhs)
	return res
}

func (g *IRGenerator) checkIsNotEqual(lhs *Symbol, rhs *Symbol) *Symbol {
	res := g.scopeMgr.newTemp(semantics.BuiltinFrom("int")) // TODO
	g.writer.WriteNeqComparion(res, lhs, rhs)
	return res
}

func (g *IRGenerator) negate(sym *Symbol) *Symbol {
	res := g.scopeMgr.newTemp(sym.Ctype)
	zero := g.makeIntConst(0)
	zero.Ctype = sym.Ctype
	g.writer.WriteBinarySubtraction(res, zero, sym)
	return res
}

func (g *IRGenerator) makeUnaryCast(sym *Symbol, operator string) *Symbol {
	switch operator {
	case "&":
		return g.getAddress(sym)
	case "*":
		return g.dereference(sym)
	case "+":
		return sym
	case "-":
		return g.negate(sym)
	case "~":
		res := g.scopeMgr.newTemp(sym.Ctype)
		g.writer.WriteBitwiseNegationUnaryOperation(res, sym)
		return res
	case "!":
		res := g.scopeMgr.newTemp(semantics.BuiltinFrom("int")) // TODO integral of size equal to sym, or byte
		g.writer.WriteLogicalNegationUnaryOperation(res, sym)
		return res
	}
	panic("unexpected unary cast operator " + operator)
}

func (g *IRGenerator) typeCast(sym *Symbol, resultType semantics.Ctype) *Symbol {
	// TODO
	return sym
}

func (g *IRGenerator) simplifyAssignment(lhs *Symbol, rhs *Symbol, operator string) *Symbol {
	binaryOp := grammars.ConvertAssignmentOpToBinaryOp(operator)
	simplified := g.scopeMgr.newTemp(lhs.Ctype) // TODO cast???
	g.writer.WriteBinaryOperation(simplified, lhs, binaryOp, rhs)
	return simplified
}

func (g *IRGenerator) generateExpressionAndGetResultSymbol(expression ast.Expression) *Symbol {
	exprT := g.typeEngine.GetTypeOfExpression(expression)
	switch expr := expression.(type) {
	case ast.IdentifierExpression:
		sym := g.scopeMgr.getSymbol(expr.Identifier)
		resSymbol := g.scopeMgr.newTemp(sym.Ctype)
		g.writer.WriteBiSymbolAssignment(resSymbol, sym)
		return resSymbol
	case ast.ConstantValExpression:
		resSymbol := g.scopeMgr.newTemp(exprT)
		g.writer.WriteConstantAssignment(resSymbol, expr.Constant)
		return resSymbol
	case ast.StringLiteralExpression:
		resSymbol := g.scopeMgr.newTemp(exprT)
		g.writer.WriteConstantAssignment(resSymbol, expr.StringLiteral)
		return resSymbol
	case ast.ArrayAccessPostfixExpression:
		curExpr := expr
		dimensionSymbols := []*Symbol{}
		for {
			dimensionSymbols = append(dimensionSymbols, g.generateExpressionAndGetResultSymbol(curExpr.ArrayExpression))
			if nestedExpr, isArrayAccess := curExpr.PostfixExpression.(ast.ArrayAccessPostfixExpression); isArrayAccess {
				curExpr = nestedExpr
			} else {
				break
			}
		}
		arrOrPtrT := g.typeEngine.GetTypeOfExpression(curExpr.PostfixExpression)
		var offset *Symbol
		if len(dimensionSymbols) > 1 {
			offset = g.flattenArrayAccessor(arrOrPtrT.(semantics.ArrayCtype), dimensionSymbols)
		} else {
			offset = dimensionSymbols[0]
		}
		if nestedTypeSize := g.typeEngine.GetNestedTypeSize(arrOrPtrT); nestedTypeSize > 1 {
			offset = g.multiplyByIntConst(offset, nestedTypeSize)
		}
		
		arrBaseSymbol := g.generateExpressionAndGetResultSymbol(curExpr.PostfixExpression)
		// TODO type casting
		return g.moveByOffset(arrBaseSymbol, offset)	
	case ast.FunctionCallPostfixExpression:
		funcSymbol := g.generateExpressionAndGetResultSymbol(expr.FunctionAccessor)
		funcType := funcSymbol.Ctype.(semantics.FunctionPtrCtype)
		var returnSymbol *Symbol = nil
		if !g.typeEngine.ReturnsVoid(funcType) {
			returnSymbol = g.scopeMgr.newTemp(funcType.ReturnType)
		}
		args := []*Symbol{}
		if expr.Args != nil {
			for _, argExpr := range expr.Args.Expressions {
				args = append(args, g.generateExpressionAndGetResultSymbol(argExpr))
			}
		}
		g.writer.WriteFunctionCall(funcSymbol, returnSymbol, args)
		return returnSymbol // this can be nil but typesystem guarantess that if it is it will never be used
	case ast.StructAccessPostfixExpression:
		var structSymbol *Symbol
		if expr.AccessMethod == ast.POINTER_ACCESS {
			structPtrSymbol := g.generateExpressionAndGetResultSymbol(expr.StructAccessor)
			structSymbol = g.dereference(structPtrSymbol)
		} else {
			structSymbol = g.generateExpressionAndGetResultSymbol(expr.StructAccessor)
		}
		structType := structSymbol.Ctype.(semantics.StructCtype)
		fieldType, fieldOffset := structType.Field(expr.FieldIdentifier)
		res := g.moveByIntOffset(structSymbol, fieldOffset)
		res.Ctype = fieldType
		return res
	case ast.IncDecPostfixExpression:
		symbolToIncDec := g.generateExpressionAndGetResultSymbol(expr.PostfixExpression)
		return g.incrementOrDecrement(symbolToIncDec, expr.Operator)
	case ast.IncDecUnaryExpression:
		symbolToIncDec := g.generateExpressionAndGetResultSymbol(expr.UnaryExpression)
		return g.incrementOrDecrement(symbolToIncDec, expr.Operator)
	case ast.CastUnaryExpression:
		symToCast := g.generateExpressionAndGetResultSymbol(expr.CastExpression)
		return g.makeUnaryCast(symToCast, expr.Operator)
	case ast.SizeofUnaryExpression:
		var nestedExprT semantics.Ctype
		if expr.NestedUnaryExpression != nil {
			nestedExprT = g.typeEngine.GetTypeOfExpression(expr.NestedUnaryExpression)
		} else {
			nestedExprT = g.typeEngine.ConvertToCtype(expr.TypeName)
		}
		res := g.makeIntConst(nestedExprT.Size())
		res.Ctype = exprT
		return res
	case ast.TypeCastCastExpression:
		initialSym := g.generateExpressionAndGetResultSymbol(expr.Expression)
		return g.typeCast(initialSym, exprT)
	case ast.BinaryArithmeticExpression:
		leftOperand := g.generateExpressionAndGetResultSymbol(expr.LhsExpression)
		rightOperand := g.generateExpressionAndGetResultSymbol(expr.RhsExpression)
		// TODO type cast
		res := g.scopeMgr.newTemp(exprT)
		g.writer.WriteBinaryOperation(res, leftOperand, expr.Operator, rightOperand)
		return res
	case ast.ConditionalExpression:
		// TODO type cast
		res := g.scopeMgr.newTemp(exprT)
		cond := g.generateExpressionAndGetResultSymbol(expr.Condition)
		endifLabel := g.labels.Next(TERNARY_ENDIF)
		elseLabel := g.labels.Next(TERNARY_ELSE)
		g.writer.WriteIfgotoLine(cond, elseLabel)
		ifRes := g.generateExpressionAndGetResultSymbol(expr.IfTrueExpression)
		g.writer.WriteBiSymbolAssignment(res, ifRes)
		g.writer.WriteGotoLine(endifLabel)
		g.writer.WriteLabel(elseLabel)
		elseRes := g.generateExpressionAndGetResultSymbol(expr.ElseExpression)
		g.writer.WriteBiSymbolAssignment(res, elseRes)
		g.writer.WriteLabel(endifLabel)
		return res
	case ast.AssignmentExpression:
		lhs := g.generateExpressionAndGetResultSymbol(expr.LhsExpression)
		rhs := g.generateExpressionAndGetResultSymbol(expr.RhsExpression)
		if expr.Operator != "=" {
			rhs = g.simplifyAssignment(lhs, rhs, expr.Operator)
		}
		// TODO cast rhs?
		g.writer.WriteBiSymbolAssignment(lhs, rhs)
		return lhs
	}
	panic("Unexpected expression")
}

func (g *IRGenerator) generateExpressionAndAssignResultTo(sym *Symbol, offset int, expr ast.Expression) {
	resSym := g.generateExpressionAndGetResultSymbol(expr)
	if offset != 0 {
		shifted := g.moveByIntOffset(sym, offset)
		g.writer.WriteBiSymbolAssignment(shifted, resSym)
	} else {
		g.writer.WriteBiSymbolAssignment(sym, resSym)
	}
}

func (g *IRGenerator) initializeSymbols(dec *ast.Declaration) {
	for _, symDef := range g.typeEngine.GetDeclaredSymbols(dec) {
		variable := g.scopeMgr.newLocalVariable(symDef.Name, symDef.T)
		if symDef.Initializer != nil {
			if structT, isStruct := symDef.T.(semantics.StructCtype); isStruct {
				if symDef.Initializer.Expression != nil {
					g.generateExpressionAndAssignResultTo(variable, 0, symDef.Initializer.Expression)
				} else {
					fields, expressions := g.typeEngine.GetStructFieldInitializers(structT, symDef.Initializer)
					for i := range fields {
						_, offset := structT.Field(fields[i])
						g.generateExpressionAndAssignResultTo(variable, offset, expressions[i])
					}
				}
			} else {
				g.generateExpressionAndAssignResultTo(variable, 0, symDef.Initializer.Expression)
			}
		}
	}
}

func (g *IRGenerator) generateSwitchBranching(stmnt ast.SwitchSelectionStatement, switchEndLabel string) (caseLabels []string, defaultLabel string) {
	hasDefault := false
	switchExpr := g.generateExpressionAndGetResultSymbol(stmnt.SwitchExpression)
	for _, nestedStmtn := range stmnt.SwitchBody.(ast.CompoundStatement).StatementList.Statements {
		if caseStmnt, isCase := nestedStmtn.(ast.CaseLabeledStatement); isCase {
			caseExpr := g.generateExpressionAndGetResultSymbol(caseStmnt.Expression)
			label := g.labels.Next(CASE)
			caseLabels = append(caseLabels, label)
			cmp := g.checkIsNotEqual(switchExpr, caseExpr)
			g.writer.WriteIfgotoLine(cmp, label)
		} else if _, isDefault := nestedStmtn.(ast.DefaultLabeledStatement); isDefault {
			defaultLabel = g.labels.Next(DEFAULT)
			hasDefault = true
			g.writer.WriteGotoLine(defaultLabel)
		}
	}
	if !hasDefault {
		g.writer.WriteGotoLine(switchEndLabel)
	}
	return
}

func (g *IRGenerator) generateStatement(statement ast.Statement) {
	switch stmnt := statement.(type) {
	case ast.CompoundStatement:
		g.scopeMgr.EnterCompoundStatement()
		defer g.scopeMgr.LeaveCompoundStatement()
		g.generateCompoundStatement(&stmnt)
	case ast.IdentifierLabeledStatement:
		g.writer.WriteLabel(g.labels.WithFunctionName(stmnt.Identifier))
		g.generateStatement(stmnt.Statement)
	case ast.ExpressionStatement:
		if stmnt.Expression != nil {
			g.generateExpressionAndGetResultSymbol(stmnt.Expression)
		}
	case ast.IfSelectionStatement:
		cond := g.generateExpressionAndGetResultSymbol(stmnt.Condition)
		endifLabel := g.labels.Next(END_IF)
		hasElse := stmnt.ElseStatement != nil
		var elseLabel string
		if hasElse {
			elseLabel = g.labels.Next(ELSE)
			g.writer.WriteIfgotoLine(cond, elseLabel)
		} else {
			g.writer.WriteIfgotoLine(cond, endifLabel)
		}
		g.generateStatement(stmnt.IfStatement)
		if hasElse {
			g.writer.WriteGotoLine(endifLabel)
			g.writer.WriteLabel(elseLabel)
			g.generateStatement(stmnt.ElseStatement)
		}
		g.writer.WriteLabel(endifLabel)
	case ast.SwitchSelectionStatement:
		switchEndLabel := g.labels.Next(END_SWITCH)
		caseLabels, defaultLabel := g.generateSwitchBranching(stmnt, switchEndLabel)
		caseCounter := 0
		g.scopeMgr.EnterStatementContext(StatementContext{BreakLabel: switchEndLabel})
		for _, nestedStmtn := range stmnt.SwitchBody.(ast.CompoundStatement).StatementList.Statements {
			if caseStmnt, isCase := nestedStmtn.(ast.CaseLabeledStatement); isCase {
				g.writer.WriteLabel(caseLabels[caseCounter])
				g.generateStatement(caseStmnt.Statement)
				caseCounter++
			} else if defaultStmnt, isDefault := nestedStmtn.(ast.DefaultLabeledStatement); isDefault {
				g.writer.WriteLabel(defaultLabel)
				g.generateStatement(defaultStmnt.Statement)
			} else {
				g.generateStatement(nestedStmtn)
			}
		}
		g.scopeMgr.LeaveStatementContext()
		g.writer.WriteLabel(switchEndLabel)
	case ast.WhileIterationStatement:
		whileLabel := g.labels.Next(BEGIN_WHILE)
		endWhileLabel := g.labels.Next(END_WHILE)
		g.scopeMgr.EnterStatementContext(StatementContext{BreakLabel: endWhileLabel, ContinueLabel: whileLabel})
		g.writer.WriteLabel(whileLabel)
		cond := g.generateExpressionAndGetResultSymbol(stmnt.Condition)
		g.writer.WriteIfgotoLine(cond, endWhileLabel)
		g.generateStatement(stmnt.Body)
		g.writer.WriteGotoLine(whileLabel)
		g.writer.WriteLabel(endWhileLabel)
		g.scopeMgr.LeaveStatementContext()
	case ast.DoWhileIterationStatement:
		whileLabel := g.labels.Next(BEGIN_WHILE)
		endWhileLabel := g.labels.Next(END_WHILE)
		g.scopeMgr.EnterStatementContext(StatementContext{BreakLabel: endWhileLabel, ContinueLabel: whileLabel})
		g.writer.WriteLabel(whileLabel)
		g.generateStatement(stmnt.Body) 
		cond := g.generateExpressionAndGetResultSymbol(stmnt.Condition)
		g.writer.WriteIfgotoLine(cond, endWhileLabel)
		g.writer.WriteGotoLine(whileLabel)
		g.writer.WriteLabel(endWhileLabel)
		g.scopeMgr.LeaveStatementContext()
	case ast.ForIterationStatement:
		forLabel := g.labels.Next(BEGIN_FOR)
		endForLabel := g.labels.Next(END_FOR)
		g.generateStatement(stmnt.Initializer)
		g.writer.WriteLabel(forLabel)
		if stmnt.Condition.Expression != nil {
			cond := g.generateExpressionAndGetResultSymbol(stmnt.Condition.Expression)
			g.writer.WriteIfgotoLine(cond, endForLabel)
		}
		g.scopeMgr.EnterStatementContext(StatementContext{BreakLabel: endForLabel, ContinueLabel: forLabel})
		g.generateStatement(stmnt.Body)
		if stmnt.Updater != nil {
			g.generateExpressionAndGetResultSymbol(stmnt.Updater)
		}
		g.writer.WriteGotoLine(forLabel)
		g.writer.WriteLabel(endForLabel)
		g.scopeMgr.LeaveStatementContext()
	case ast.LoopControlJumpStatement:
		ctx := g.scopeMgr.GetStatementContext()
		if stmnt.ControlOption == ast.BREAK {
			g.writer.WriteGotoLine(ctx.BreakLabel)
		} else {
			g.writer.WriteGotoLine(ctx.ContinueLabel)
		}
	case ast.GotoJumpStatement:
		g.writer.WriteGotoLine(stmnt.Label)
	case ast.ReturnJumpStatement:
		if stmnt.Expression != nil {
			retSym := g.generateExpressionAndGetResultSymbol(stmnt.Expression)
			g.writer.WriteReturnLine(retSym)
		} else {
			g.writer.WriteReturnLine(nil)
		}
	default:
		panic("Unexpected statement")
	}
}

func (g *IRGenerator) generateCompoundStatement(cs *ast.CompoundStatement) {
	if cs.DeclarationList != nil {
		for _, dec := range cs.DeclarationList.Declarations {
			g.initializeSymbols(dec)
		}
	}
	if cs.StatementList != nil {
		for _, statement := range cs.StatementList.Statements {
			g.generateStatement(statement)
		}
	}
}

func (g *IRGenerator) generateFunction(f *ast.FunctionDefinition) {
	fun := g.scopeMgr.EnterFunction(f)
	defer g.scopeMgr.LeaveFunction()
	g.curFunctionName = fun.Name()
	g.labels.EnterFunction(fun.Name())
	
	if f.DeclarationList != nil {
		panic("todo")
	// 	for _, dec := range f.DeclarationList.Declarations {
	// 		g.initializeSymbols(dec)
	// 	}
	}
	
	g.generateCompoundStatement(f.Body)
	if g.typeEngine.ReturnsVoid(*fun) {
		g.writer.WriteReturnLine(nil)
	}
}

func (g *IRGenerator) handleTopLevelDeclaration(dec *ast.Declaration) {
	g.typeEngine.GetSymbolsForTopLevelDeclarationAndDefineNewTypes(dec)
	for _, globalDef := range g.typeEngine.GetDeclaredGlobals(dec) {
		sym := g.scopeMgr.newGlobalVariable(globalDef.Name, globalDef.T)
		initializers := []*GlobalInitializer{}
		if globalDef.Initializer != nil {
			if structT, isStruct := globalDef.T.(semantics.StructCtype); isStruct {
				if globalDef.Initializer.Expression != nil {
					initializers = append(initializers, &GlobalInitializer{Offset: 0, Expression: globalDef.Initializer.Expression})
				} else {
					fields, expressions := g.typeEngine.GetStructFieldInitializers(structT, globalDef.Initializer)
					for i := range fields {
						_, offset := structT.Field(fields[i])
						initializers = append(initializers, &GlobalInitializer{Offset: offset, Expression: expressions[i]})
					}
				}
			} else {
				initializers = append(initializers, &GlobalInitializer{Offset: 0, Expression: globalDef.Initializer.Expression})
			}
		}
		globalSym := GlobalSymbol{
			Symbol: sym,
			isExtern: globalDef.Extern,
			isStatic: globalDef.Static,
			Initializers: initializers,
		}
		g.globalSymbols = append(g.globalSymbols, &globalSym)
	}
}

func (g *IRGenerator) Generate(root *ast.TranslationUnit) {
	g.scopeMgr.EnterGlobalScope()
	for _, ed := range root.ExternalDeclarations {
		switch ced := ed.(type) {
		case ast.FunctionDefinition:
			g.generateFunction(&ced)
		case ast.Declaration:
			g.handleTopLevelDeclaration(&ced)
		}
	}
	g.writer.PrintAll()
}
