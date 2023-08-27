package irs

import (
	"ast"
	"grammars"
	"types"
	"utils"
)

type IRGenerator struct {
	scopeMgr *ScopeManager
	typesystem *types.Engine
	writer *Writer
}

func NewGenerator() *IRGenerator {
	return &IRGenerator{
		scopeMgr: newScopeManager(),
		writer: NewWriter(),
	}
}

func (g *IRGenerator) flattenArrayAccessor(array types.ArrayCtype, dimensionSymbols *utils.Stack[*Symbol]) {
	for dimIdx := 0; dimensionSymbols.Size() > 1; dimIdx++ {
		rightSymbol := dimensionSymbols.Pop()
		leftSymbol := dimensionSymbols.Pop()
		// from -> to
		if g.typesystem.ShouldBeUpCasted(leftSymbol, rightSymbol) {
			// leftSymbol = g.cast(leftSymbol, rightSymbol.Ctype)
		} else if g.typesystem.ShouldBeUpCasted(rightSymbol, leftSymbol) {
			// rightSymbol = g.cast(rightSymbol, leftSymbol.Ctype)
		}

		multipliedLeftSymbol := g.multiplyByIntConst(leftSymbol, array.DimensionSizes[dimIdx])
		mergedSymbol := g.scopeMgr.newTemp(leftSymbol.Ctype)
		g.writer.WirteBinaryAddition(mergedSymbol, multipliedLeftSymbol, rightSymbol)
		dimensionSymbols.Push(mergedSymbol)
	}
}

func (g *IRGenerator) getNestedTypeSize(arrOrPtrT types.Ctype) int {
	if arr, isArr := arrOrPtrT.(types.ArrayCtype); isArr {
		return arr.NestedType.Size()
	} else {
		return arrOrPtrT.(types.PointerCtype).Target.Size()
	}
}

func (g *IRGenerator) makeIntConst(c int) *Symbol {
	res := g.scopeMgr.newTemp(types.BuiltinFrom("int")) // TODO ?
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
	res := g.scopeMgr.newTemp(types.PointerCtype{Target: sym.Ctype})
	g.writer.WriteAddressUnaryOperation(res, sym)
	return res
}

func (g *IRGenerator) dereference(sym *Symbol) *Symbol {
	resT := sym.Ctype.(types.PointerCtype)
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
		res := g.scopeMgr.newTemp(types.BuiltinFrom("int")) // TODO integral of size equal to sym, or byte
		g.writer.WriteLogicalNegationUnaryOperation(res, sym)
		return res
	}
	panic("unexpected unary cast operator " + operator)
}

func (g *IRGenerator) typeCast(sym *Symbol, resultType types.Ctype) *Symbol {
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
	exprT := g.typesystem.GetTypeOfExpression(expression)
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
		dimensionSymbols := utils.NewStack[*Symbol]()
		for {
			dimensionSymbols.Push(g.generateExpressionAndGetResultSymbol(curExpr.ArrayExpression))
			if nestedExpr, isArrayAccess := curExpr.PostfixExpression.(ast.ArrayAccessPostfixExpression); isArrayAccess {
				curExpr = nestedExpr
			} else {
				break
			}
		}
		arrOrPtrT := g.typesystem.GetTypeOfExpression(curExpr.PostfixExpression)
		if dimensionSymbols.Size() > 1 {
			g.flattenArrayAccessor(arrOrPtrT.(types.ArrayCtype), dimensionSymbols)
		}
		if nestedTypeSize := g.getNestedTypeSize(arrOrPtrT); nestedTypeSize > 1 {
			offset := dimensionSymbols.Pop()
			multipliedOffset := g.multiplyByIntConst(offset, nestedTypeSize)
			dimensionSymbols.Push(multipliedOffset)
		}
		
		arrBaseSymbol := g.generateExpressionAndGetResultSymbol(curExpr.PostfixExpression)
		// TODO type casting
		return g.moveByOffset(arrBaseSymbol, dimensionSymbols.Pop())	
	case ast.FunctionCallPostfixExpression:
		funcSymbol := g.generateExpressionAndGetResultSymbol(expr.FunctionAccessor)
		funcType := funcSymbol.Ctype.(types.FunctionPtrCtype)
		var returnSymbol *Symbol = nil
		if !g.typesystem.ReturnsVoid(funcType) {
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
		structType := structSymbol.Ctype.(types.StructCtype)
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
		var nestedExprT types.Ctype
		if expr.NestedUnaryExpression != nil {
			nestedExprT = g.typesystem.GetTypeOfExpression(expr.NestedUnaryExpression)
		} else {
			nestedExprT = g.typesystem.ConvertToCtype(expr.TypeName)
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

}

func (g *IRGenerator) extractInitializers(dec *ast.Declaration) ([]Symbol, []ast.Expression) {
	// todo remeber about structs
	symbolNames, initializers := g.typesystem.getInitializedSymbolNamesWithInitializers(dec)
	symbols := []Symbol{}
	for _, name := range symbolNames {
		symbols = append(symbols, g.scopeMgr.getSymbol(name))
	}
	return symbols, initializers
}

func (g *IRGenerator) initializeSymbols(dec *ast.Declaration) {
	symbols, initializers := g.extractInitializers(dec)
	for idx := range symbols {
		g.generateExpressionAndAssignResultTo(&symbols[idx], 9999999999, initializers[idx])
	}
}

func (g *IRGenerator) generateStatement(statement *ast.Statement) {

}

func (g *IRGenerator) generateCompoundStatement(cs *ast.CompoundStatement) {
	if cs.DeclarationList != nil {
		for _, dec := range cs.DeclarationList.Declarations {
			g.initializeSymbols(dec)
		}
	}
	if cs.StatementList != nil {
		for _, statement := range cs.StatementList.Statements {
			g.generateStatement(&statement)
		}
	}
}

func (g *IRGenerator) GenerateFunction(f *ast.FunctionDefinition) {
	g.scopeMgr.EnterFunction(f)
	defer g.scopeMgr.LeaveFunction()
	
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
}
