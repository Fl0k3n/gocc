package irs

import (
	"ast"
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

func (g *IRGenerator) getAddressOfStruct(structSymbol *Symbol) *Symbol {
	res := g.scopeMgr.newTemp(types.PointerCtype{Target: structSymbol.Ctype})
	g.writer.WriteAddressUnaryOperation(res, structSymbol)
	return res
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
	case ast.StructAccessPostfixExpression:
		if expr.AccessMethod == ast.POINTER_ACCESS {
			
		} else {
			structSymbol := g.generateExpressionAndGetResultSymbol(expr.StructAccessor)
			structType := g.typesystem.GetTypeOfExpression(expr.StructAccessor).(types.StructCtype)
			fieldType, fieldOffset := structType.Field(expr.FieldIdentifier)
			res := g.moveByIntOffset(structSymbol, fieldOffset)
			res.Ctype = fieldType
			return res
		}
	}
}

func (g *IRGenerator) generateExpressionAndAssignResultTo(sym *Symbol, expr ast.Expression) {

}

func (g *IRGenerator) extractInitializers(dec *ast.Declaration) ([]Symbol, []ast.Expression) {
	// with linearized structs, meaning that if struct with name s has fields x and y we should have 2 symbols: s.x and s.y
	// we don't deal with structs as a whole anymore
	
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
		g.generateExpressionAndAssignResultTo(&symbols[idx], initializers[idx])
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
