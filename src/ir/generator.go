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
		g.writer.WirteBinaryAddition(mergedSymbol, partialSumSymbol, multipliedNextIdx)
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
	g.writer.WirteBinaryMultiplication(res, sym, multiplierSymbol)
	return res
}

func (g *IRGenerator) moveByOffset(base *Symbol, offset *Symbol) *Symbol {
	res := g.scopeMgr.newTemp(base.Ctype)
	g.writer.WirteBinaryAddition(res, base, offset)
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
		g.writer.WirteBinaryAddition(res, sym, valSymbol)
	} else {
		g.writer.WirteBinarySubtraction(res, sym, valSymbol)
	}
	return res
}

func (g *IRGenerator) negate(sym *Symbol) *Symbol {
	res := g.scopeMgr.newTemp(sym.Ctype)
	zero := g.makeIntConst(0)
	zero.Ctype = sym.Ctype
	g.writer.WirteBinarySubtraction(res, zero, sym)
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
	g.writer.WirteBinaryOperation(simplified, lhs, binaryOp, rhs)
	return simplified
}

func (g *IRGenerator) generateExpressionAndGetResultSymbol(expression ast.Expression) *Symbol {
	exprT := g.typeEngine.GetTypeOfExpression(expression)
	switch expr := expression.(type) {
	case ast.IdentifierExpression:
		return g.scopeMgr.getSymbol(expr.Identifier)
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
		g.writer.WirteBinaryOperation(res, leftOperand, expr.Operator, rightOperand)
		return res
	case ast.ConditionalExpression:
		// TODO
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
					panic("copy entire struct") // TODO
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

func (g *IRGenerator) generateStatement(statement ast.Statement) {
	switch stmnt := statement.(type) {
	case ast.CompoundStatement:
		g.scopeMgr.EnterCompoundStatement()
		defer g.scopeMgr.LeaveCompoundStatement()
		g.generateCompoundStatement(&stmnt)
	case ast.CaseLabeledStatement:
	case ast.DefaultLabeledStatement:
	case ast.IdentifierLabeledStatement:
	case ast.ExpressionStatement:
		g.generateExpressionAndGetResultSymbol(stmnt.Expression)
	case ast.IfSelectionStatement:
		cond := g.generateExpressionAndGetResultSymbol(stmnt.Condition)
		hasElse := stmnt.ElseStatement != nil
		endifLabel := g.labels.Next(END_IF)
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
	case ast.WhileIterationStatement:
	case ast.DoWhileIterationStatement:
	case ast.ForIterationStatement:
	case ast.LoopControlJumpStatement:
	case ast.GotoJumpStatement:
	case ast.ReturnJumpStatement:
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

func (g *IRGenerator) GenerateFunction(f *ast.FunctionDefinition) {
	funName := g.scopeMgr.EnterFunction(f)
	defer g.scopeMgr.LeaveFunction()
	g.curFunctionName = funName
	g.labels.EnterFunction(funName)
	
	if f.DeclarationList != nil {
		panic("todo")
	// 	for _, dec := range f.DeclarationList.Declarations {
	// 		g.initializeSymbols(dec)
	// 	}
	}
	
	g.generateCompoundStatement(f.Body)
}

func (g *IRGenerator) Generate(root *ast.TranslationUnit) {
	g.scopeMgr.EnterGlobalScope()
	for _, ed := range root.ExternalDeclarations {
		switch ced := ed.(type) {
		case ast.FunctionDefinition:
			g.GenerateFunction(&ced)
		}
	}
	g.writer.PrintAll()
}
