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
	curFuncReturnType semantics.Ctype
}

func NewGenerator(writer *Writer) *IRGenerator {
	scopeMgr := newScopeManager(nil)
	typeEngine := semantics.NewEngine(scopeMgr, semantics.NewErrorTracker())
	scopeMgr.typeEngine = typeEngine // TODO untangle dependencies
	return &IRGenerator{
		scopeMgr: scopeMgr,
		writer: writer,
		typeEngine: typeEngine,
		labels: NewLabelProvider(),
	}
}

func (g *IRGenerator) flattenArrayAccessor(array semantics.ArrayCtype, dimensionSymbols []*Symbol) *Symbol {
	partialSumSymbol := dimensionSymbols[0]
	var curMult int64 = 1
	for i := 1; i < len(dimensionSymbols); i++ {
		curMult *= int64(array.DimensionSizes[len(array.DimensionSizes) - i])
		nextIdx := dimensionSymbols[i]
		multipliedNextIdx := g.multiplyByIntConst(nextIdx, curMult)
		mergedSymbol := g.scopeMgr.newTemp(partialSumSymbol.Ctype)
		castInfo := g.typeEngine.GetRequiredTypeCastForBinaryArithmeticOperation(partialSumSymbol.Ctype, multipliedNextIdx.Ctype)
		if castInfo.LeftRequiresCast {
			partialSumSymbol = g.typeCast(partialSumSymbol, castInfo.LeftTargetType)
		}
		if castInfo.RightRequiresCast {
			// this shouldn't be needed
			multipliedNextIdx = g.typeCast(multipliedNextIdx, castInfo.RightTargetType)
		}
		g.writer.WriteBinaryAddition(mergedSymbol, partialSumSymbol, multipliedNextIdx)
		partialSumSymbol = mergedSymbol
	}
	return partialSumSymbol
}

func (g *IRGenerator) makeIntConst(c int64, T semantics.Ctype) *Symbol {
	constant := semantics.IntegralConstant{
		Val: c,
		T: T,
	}
	res := g.scopeMgr.newTemp(constant.T)
	g.writer.WriteIntAssignment(res, constant)
	return res
}

func (g *IRGenerator) multiplyByIntConst(sym *Symbol, c int64) *Symbol {
	multiplierSymbol := g.makeIntConst(c, sym.Ctype)
	res := g.scopeMgr.newTemp(sym.Ctype)
	g.writer.WriteBinaryMultiplication(res, sym, multiplierSymbol)
	return res
}

func (g *IRGenerator) moveByOffset(base *Symbol, offset *Symbol, resType semantics.Ctype) *Symbol {
	res := g.scopeMgr.newTemp(resType)
	g.writer.WriteBinaryAddition(res, base, offset)
	return res
}

func (g *IRGenerator) moveByIntOffset(base *Symbol, offset int64, resType semantics.Ctype) *Symbol {
	offsetSym := g.makeIntConst(offset, g.typeEngine.GetIntConstantTypeHavingSize(base.Ctype.Size()))
	return g.moveByOffset(base, offsetSym, resType)
}

func (g *IRGenerator) reference(sym *Symbol) *Symbol {
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

func (g *IRGenerator) incrementOrDecrement(lval *LValue, sym *Symbol, operator ast.IncDecOperator) *Symbol {
	res := g.scopeMgr.newTemp(sym.Ctype)
	var valSymbol *Symbol
	if ptr, isPtr := sym.Ctype.(semantics.PointerCtype); isPtr {
		valSymbol = g.makeIntConst(int64(ptr.Target.Size()), sym.Ctype)
	} else {
		valSymbol = g.makeIntConst(1, sym.Ctype)
	}
	if operator == ast.INC {
		g.writer.WriteBinaryAddition(res, sym, valSymbol)
	} else {
		g.writer.WriteBinarySubtraction(res, sym, valSymbol)
	}
	g.writer.WriteBiSymbolAssignment(lval, res)
	return sym
}

func (g *IRGenerator) checkIsEqual(lhs *Symbol, rhs *Symbol) *Symbol {
	res := g.scopeMgr.newTemp(semantics.COMPARISON_TYPE)
	g.writer.WriteEqComparion(res, lhs, rhs)
	return res
}

func (g *IRGenerator) checkIsNotEqual(lhs *Symbol, rhs *Symbol) *Symbol {
	res := g.scopeMgr.newTemp(semantics.COMPARISON_TYPE)
	g.writer.WriteNeqComparion(res, lhs, rhs)
	return res
}

func (g *IRGenerator) negate(sym *Symbol) *Symbol {
	res := g.scopeMgr.newTemp(sym.Ctype)
	zero := g.makeIntConst(0, sym.Ctype)
	zero.Ctype = sym.Ctype
	g.writer.WriteBinarySubtraction(res, zero, sym)
	return res
}

func (g *IRGenerator) makeUnaryCast(sym *Symbol, operator string) *Symbol {
	switch operator {
	case "&":
		return g.reference(sym)
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
		res := g.scopeMgr.newTemp(semantics.COMPARISON_TYPE)
		g.writer.WriteLogicalNegationUnaryOperation(res, sym)
		return res
	}
	panic("unexpected unary cast operator " + operator)
}

func (g *IRGenerator) typeCast(sym *Symbol, resultType semantics.Ctype) *Symbol {
	if g.typeEngine.InterpretAsAddressInArithmetic(sym.Ctype) && g.typeEngine.IsPointer(resultType) {
		return g.reference(sym)
	} else {
		res := g.scopeMgr.newTemp(resultType)
		g.writer.WriteTypeCastLine(res, sym)
		return res
	}
}

func (g *IRGenerator) simplifyAssignment(lhs *Symbol, rhs *Symbol, operator string) *Symbol {
	binaryOp := grammars.ConvertAssignmentOpToBinaryOp(operator)
	castInfo := g.typeEngine.GetRequiredTypeCastForBinaryArithmeticOperation(lhs.Ctype, rhs.Ctype)
	if castInfo.LeftRequiresCast {
		lhs = g.typeCast(lhs, castInfo.LeftTargetType)
	}
	if castInfo.RightRequiresCast {
		rhs = g.typeCast(rhs, castInfo.RightTargetType)
	}
	simplified := g.scopeMgr.newTemp(rhs.Ctype)
	lhs, rhs = g.checkForPointerArithmeticOperation(lhs, rhs)
	g.writer.WriteBinaryOperation(simplified, lhs, binaryOp, rhs)
	return simplified
}

func (g *IRGenerator) checkForPointerArithmeticOperation(leftOp *Symbol, rightOp *Symbol) (left *Symbol, right *Symbol) {
	left = leftOp
	right = rightOp
	if leftPtr, leftIsPtr := leftOp.Ctype.(semantics.PointerCtype); leftIsPtr {
		if g.typeEngine.IsIntegralType(rightOp.Ctype) {
			right = g.multiplyByIntConst(rightOp, int64(leftPtr.Target.Size()))
		}
	} else if rightPtr, rightIsPtr := rightOp.Ctype.(semantics.PointerCtype); rightIsPtr {
		if g.typeEngine.IsIntegralType(leftOp.Ctype) {
			left = g.multiplyByIntConst(leftOp, int64(rightPtr.Target.Size()))
		}
	}
	return
}

func (g *IRGenerator) getArraySymbolWithDisplacement(
	expr ast.ArrayAccessPostfixExpression,
) (arr *Symbol, offset *Symbol, nestedType semantics.Ctype) {
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
	if len(dimensionSymbols) > 1 {
		offset = g.flattenArrayAccessor(arrOrPtrT.(semantics.ArrayCtype), dimensionSymbols)
	} else {
		offset = dimensionSymbols[0]
	}
	if nestedType = g.typeEngine.GetNestedType(arrOrPtrT); nestedType.Size() > 1 {
		offset = g.multiplyByIntConst(offset, int64(nestedType.Size()))
	}
	if !g.typeEngine.AreTypesEqual(g.typeEngine.GetTypeOfArrayDisplacement(), offset.Ctype) {
		offset = g.typeCast(offset, g.typeEngine.GetTypeOfArrayDisplacement())
	}
	lval := g.generateLValueForAssignment(curExpr.PostfixExpression)
	if g.typeEngine.IsPointer(lval.Sym.Ctype) {
		arr = lval.Sym	
	} else {
		if lval.IsDereferenced {
			arr = lval.Sym
		} else {
			arr = g.reference(lval.Sym)
		}
	}
	return
}

func (g *IRGenerator) getStructSymbolWithDisplacement(expr ast.StructAccessPostfixExpression) (sym *Symbol, offset int, fieldType semantics.Ctype) {
	if expr.AccessMethod == ast.POINTER_ACCESS {
		lval := g.generateLValueForAssignment(expr.StructAccessor)
		if lval.IsDereferenced {
			sym = g.dereference(lval.Sym)
		} else {
			sym = lval.Sym
		}
	} else {
		lval := g.generateLValueForAssignment(expr.StructAccessor)
		if lval.IsDereferenced {
			sym = lval.Sym
		} else {
			sym = g.reference(lval.Sym)
		}
	}
	structType := sym.Ctype.(semantics.PointerCtype).Target.(semantics.StructCtype)
	fieldType, offset = structType.Field(expr.FieldIdentifier)
	return 
}

func (g *IRGenerator) generateExpressionAndGetResultSymbol(expression ast.Expression) *Symbol {
	exprT := g.typeEngine.GetTypeOfExpression(expression)
	switch expr := expression.(type) {
	case ast.IdentifierExpression:
		sym := g.scopeMgr.getSymbol(expr.Identifier)
		return sym
	case ast.ConstantValExpression:
		resSymbol := g.scopeMgr.newTemp(exprT)
		g.writer.WriteConstantAssignment(resSymbol, g.typeEngine.GetConstantInfo(expr))
		return resSymbol
	case ast.StringLiteralExpression:
		resSymbol := g.scopeMgr.newTemp(exprT)
		g.writer.WriteStringAssignment(resSymbol, expr.StringLiteral)
		return resSymbol
	case ast.ArrayAccessPostfixExpression:
		arrBaseSymbol, displacement, nestedType := g.getArraySymbolWithDisplacement(expr)
		shiftedArr := g.moveByOffset(arrBaseSymbol, displacement, g.typeEngine.WrapInPointer(nestedType))
		return g.dereference(shiftedArr)
	case ast.FunctionCallPostfixExpression:
		funcSymbol := g.generateExpressionAndGetResultSymbol(expr.FunctionAccessor)
		funcType := funcSymbol.Ctype.(semantics.FunctionPtrCtype)
		var returnSymbol *Symbol = nil
		if !g.typeEngine.ReturnsVoid(funcType) {
			returnSymbol = g.scopeMgr.newTemp(funcType.ReturnType)
		}
		args := []*Symbol{}
		if expr.Args != nil {
			for paramNum, argExpr := range expr.Args.Expressions {
				arg := g.generateExpressionAndGetResultSymbol(argExpr)
				paramType := funcSymbol.Ctype.(semantics.FunctionPtrCtype).ParamTypes[paramNum]
				if castTo, argMustBeCasted := g.typeEngine.GetAssignmentCastInfo(paramType, arg.Ctype); argMustBeCasted {
					arg = g.typeCast(arg, castTo)
				}
				args = append(args, arg)
			}
		}
		g.writer.WriteFunctionCall(funcSymbol, returnSymbol, args)
		return returnSymbol // this can be nil but typesystem guarantess that if it is it will never be used
	case ast.StructAccessPostfixExpression:
		structSymbol, fieldOffset, fieldType := g.getStructSymbolWithDisplacement(expr)
		fieldPtr := g.moveByIntOffset(structSymbol, int64(fieldOffset), g.typeEngine.WrapInPointer(fieldType))
		return g.dereference(fieldPtr)
	case ast.IncDecPostfixExpression:
		rval := g.generateExpressionAndGetResultSymbol(expr.PostfixExpression)
		lval := g.generateLValueForAssignment(expr.PostfixExpression)
		return g.incrementOrDecrement(lval, rval, expr.Operator)
	case ast.IncDecUnaryExpression:
		rval := g.generateExpressionAndGetResultSymbol(expr.UnaryExpression)
		lval := g.generateLValueForAssignment(expr.UnaryExpression)
		return g.incrementOrDecrement(lval, rval, expr.Operator)
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
		res := g.makeIntConst(int64(nestedExprT.Size()), exprT)
		res.Ctype = exprT
		return res
	case ast.TypeCastCastExpression:
		initialSym := g.generateExpressionAndGetResultSymbol(expr.Expression)
		return g.typeCast(initialSym, exprT)
	case ast.BinaryArithmeticExpression:
		leftOperand := g.generateExpressionAndGetResultSymbol(expr.LhsExpression)
		rightOperand := g.generateExpressionAndGetResultSymbol(expr.RhsExpression)
		leftOperand, rightOperand = g.checkForPointerArithmeticOperation(leftOperand, rightOperand)
		castInfo := g.typeEngine.GetRequiredTypeCastForBinaryArithmeticOperation(leftOperand.Ctype, rightOperand.Ctype)
		if castInfo.LeftRequiresCast {
			leftOperand = g.typeCast(leftOperand, castInfo.LeftTargetType)
		}
		if castInfo.RightRequiresCast {
			rightOperand = g.typeCast(rightOperand, castInfo.RightTargetType)
		}
		res := g.scopeMgr.newTemp(exprT)
		g.writer.WriteBinaryOperation(res, leftOperand, expr.Operator, rightOperand)
		return res
	case ast.ConditionalExpression:
		res := g.scopeMgr.newTemp(exprT)
		cond := g.generateExpressionAndGetResultSymbol(expr.Condition)
		endifLabel := g.labels.Next(TERNARY_ENDIF)
		elseLabel := g.labels.Next(TERNARY_ELSE)
		g.writer.WriteIfgotoLine(cond, elseLabel)
		ifRes := g.generateExpressionAndGetResultSymbol(expr.IfTrueExpression)
		g.writer.WriteBiSymbolAssignment(&LValue{IsDereferenced: false, Sym: res}, ifRes)
		g.writer.WriteGotoLine(endifLabel)
		g.writer.WriteLabel(elseLabel)
		elseRes := g.generateExpressionAndGetResultSymbol(expr.ElseExpression)
		g.writer.WriteBiSymbolAssignment(&LValue{IsDereferenced: false, Sym: res}, elseRes)
		g.writer.WriteLabel(endifLabel)
		return res
	case ast.AssignmentExpression:
		lValue := g.generateLValueForAssignment(expr.LhsExpression)
		rhs := g.generateExpressionAndGetResultSymbol(expr.RhsExpression)
		if expr.Operator != "=" {
			rhs = g.simplifyAssignment(g.generateExpressionAndGetResultSymbol(expr.LhsExpression), rhs, expr.Operator)
		}
		if castType, rhsRequiresCast := g.typeEngine.GetAssignmentCastInfo(exprT, rhs.Ctype); rhsRequiresCast {
			rhs = g.typeCast(rhs, castType)
		}
		g.writer.WriteBiSymbolAssignment(lValue, rhs)
		return rhs
	}
	panic("Unexpected expression")
}

func (g *IRGenerator) generateLValueForAssignment(lhsExpression ast.Expression) *LValue {
	switch expr := lhsExpression.(type) {
	case ast.IdentifierExpression:
		return &LValue{IsDereferenced: false, Sym: g.scopeMgr.getSymbol(expr.Identifier)}
	case ast.CastUnaryExpression:
		return &LValue{IsDereferenced: true, Sym: g.generateExpressionAndGetResultSymbol(expr.CastExpression)}
	case ast.ArrayAccessPostfixExpression:
		arrOrPtrSym, displacement, nestedType := g.getArraySymbolWithDisplacement(expr)
		shifted := g.moveByOffset(arrOrPtrSym, displacement, g.typeEngine.WrapInPointer(nestedType))
		return &LValue{IsDereferenced: true, Sym: shifted}
	case ast.StructAccessPostfixExpression:
		structSymbol, fieldOffset, fieldType := g.getStructSymbolWithDisplacement(expr)
		fieldPtr := g.moveByIntOffset(structSymbol, int64(fieldOffset), g.typeEngine.WrapInPointer(fieldType))
		return &LValue{IsDereferenced: true, Sym: fieldPtr}
	case ast.FunctionCallPostfixExpression:
		// TODO depending on context we may have to enforce return value to be a pointer
		returnSymbol := g.generateExpressionAndGetResultSymbol(expr)
		return &LValue{IsDereferenced: false, Sym: returnSymbol}
	default:
		panic("Unexpected Lvalue expression")
	}
}

func (g *IRGenerator) generateExpressionAndAssignResultTo(sym *Symbol, offset int64, expr ast.Expression, lhsType semantics.Ctype) {
	resSym := g.generateExpressionAndGetResultSymbol(expr)
	if castType, rhsRequiresCast := g.typeEngine.GetAssignmentCastInfo(lhsType, resSym.Ctype); rhsRequiresCast {
		resSym = g.typeCast(resSym, castType)
	}
	if offset != 0 {
		shifted := g.moveByIntOffset(sym, int64(offset), g.typeEngine.WrapInPointer(lhsType))
		g.writer.WriteBiSymbolAssignment(&LValue{IsDereferenced: true, Sym: shifted}, resSym)
	} else {
		dereference := g.typeEngine.InterpretAsAddressInArithmetic(sym.Ctype)
		g.writer.WriteBiSymbolAssignment(&LValue{IsDereferenced: dereference, Sym: sym}, resSym)
	}
}

func (g *IRGenerator) initializeSymbols(dec *ast.Declaration) {
	for _, symDef := range g.typeEngine.GetDeclaredSymbols(dec) {
		variable := g.scopeMgr.newLocalVariable(symDef.Name, symDef.T)
		if symDef.Initializer != nil {
			if structT, isStruct := symDef.T.(semantics.StructCtype); isStruct {
				variable = g.reference(variable)
				if symDef.Initializer.Expression != nil {
					g.generateExpressionAndAssignResultTo(variable, 0, symDef.Initializer.Expression, structT)
				} else {
					fields, expressions := g.typeEngine.GetStructFieldInitializers(structT, symDef.Initializer)
					for i := range fields {
						t, offset := structT.Field(fields[i])
						g.generateExpressionAndAssignResultTo(variable, int64(offset), expressions[i], t)
					}
				}
			} else {
				g.generateExpressionAndAssignResultTo(variable, 0, symDef.Initializer.Expression, symDef.T)
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
			if castTo, retMustBeCasted := g.typeEngine.GetAssignmentCastInfo(g.curFuncReturnType, retSym.Ctype); retMustBeCasted {
				retSym = g.typeCast(retSym, castTo)
			}
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
	funPtr, funSymbol := g.scopeMgr.EnterFunction(f, g.typeEngine.IsStatic(f))
	defer g.scopeMgr.LeaveFunction()
	g.writer.EnterFunction(funSymbol)
	g.curFunctionName = funSymbol.Symbol.Name
	g.curFuncReturnType = funPtr.ReturnType
	g.labels.EnterFunction(g.curFunctionName)
	
	if f.DeclarationList != nil {
		panic("todo")
	// 	for _, dec := range f.DeclarationList.Declarations {
	// 		g.initializeSymbols(dec)
	// 	}
	}
	
	g.generateCompoundStatement(f.Body)
	if g.typeEngine.ReturnsVoid(*funPtr) {
		g.writer.WriteReturnLine(nil)
	}
	g.writer.SaveSnapshot(g.scopeMgr.GetNonGlobalsSnapshot())
}

func (g *IRGenerator) handleTopLevelDeclaration(dec *ast.Declaration) {
	g.typeEngine.GetSymbolsForTopLevelDeclarationAndDefineNewTypes(dec)
	for _, globalDef := range g.typeEngine.GetDeclaredGlobals(dec) {
		initializers := []*GlobalInitializer{}
		if globalDef.Initializer != nil {
			if structT, isStruct := globalDef.T.(semantics.StructCtype); isStruct {
				if globalDef.Initializer.Expression != nil {
					initializers = append(initializers, &GlobalInitializer{
						Offset: 0,
						Constant: g.typeEngine.EvalConstantExpression(globalDef.Initializer.Expression),
					})
				} else {
					fields, expressions := g.typeEngine.GetStructFieldInitializers(structT, globalDef.Initializer)
					for i := range fields {
						_, offset := structT.Field(fields[i])
						initializers = append(initializers, &GlobalInitializer{
							Offset: offset,
							Constant: g.typeEngine.EvalConstantExpression(expressions[i]),
						})
					}
				}
			} else {
				initializers = append(initializers, &GlobalInitializer{
					Offset: 0,
					Constant: g.typeEngine.EvalConstantExpression(globalDef.Initializer.Expression),
				})
			}
		}
		g.scopeMgr.newGlobalVariable(globalDef.Name, globalDef.T,
					 globalDef.Function, globalDef.Static, globalDef.Extern, initializers)
	}
}

func (g *IRGenerator) Generate(root *ast.TranslationUnit) *IntermediateRepresentation {
	g.scopeMgr.EnterGlobalScope()
	for _, ed := range root.ExternalDeclarations {
		switch ced := ed.(type) {
		case ast.FunctionDefinition:
			g.generateFunction(&ced)
		case ast.Declaration:
			g.handleTopLevelDeclaration(&ced)
		}
	}
	return &IntermediateRepresentation{
		FunctionIr: g.writer.GetFunctions(),
		Globals: g.scopeMgr.GetGlobalSymbols(),
		BootstrappedTypeEngine: g.typeEngine,
	}
}

func (g *IRGenerator) PrintIR() {
	g.writer.PrintAll()
}
