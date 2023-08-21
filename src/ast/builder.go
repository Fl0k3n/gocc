package ast

import (
	"errors"
	"grammars"
	"strings"
	tok "tokenizers"
	"utils"
)

type NodeBuilder func(*grammars.Production) (Node, error)

type Builder struct {
	reductionStack *utils.Stack[Node]
	tokenStack *utils.Stack[tok.Token]
	expressionBuilders map[string]NodeBuilder
	miscBuilders map[string]NodeBuilder 
	lineNumber int
}

func NewBuilder() *Builder {
	ab := &Builder{
		tokenStack: utils.NewStack[tok.Token](),
		reductionStack: utils.NewStack[Node](),
	}
	ab.expressionBuilders = map[string]NodeBuilder{
		"postfix": ab.buildPostfixExpression,
		"unary": ab.buildUnaryExpression,
		"cast": ab.buildCastExpression,
		"multiplicative": ab.buildArithmeticBinaryExpression,
		"additive": ab.buildArithmeticBinaryExpression,
		"shift": ab.buildArithmeticBinaryExpression,
		"relational": ab.buildArithmeticBinaryExpression,
		"equality": ab.buildArithmeticBinaryExpression,
		"and": ab.buildArithmeticBinaryExpression,
		"exclusive_or": ab.buildArithmeticBinaryExpression,
		"inclusive_or": ab.buildArithmeticBinaryExpression,
		"logical_and": ab.buildArithmeticBinaryExpression,
		"logical_or": ab.buildArithmeticBinaryExpression,
		"conditional": ab.buildConditionalExpression,
		"assignment": ab.buildAssigmentExpression,
		"constant": ab.buildConstantExpression,
	}
	ab.miscBuilders = map[string]NodeBuilder{
		"expression": ab.buildExpression,
		"assignment_operator": ab.buildAssigmentOperator,
		"unary_operator": ab.buildUnaryOperator,
		"type_name": ab.buildTypeName,
		"argument_expression_list": ab.buildArgumentExpressionList,
		"translation_unit": ab.buildTranslationUnit,
		"external_declaration": ab.buildExternalDeclaration,
		"function_definition": ab.buildFunctionDefinition,
		"type_specifier": ab.buildTypeSpecifier,
		"enum_specifier": ab.buildEnumSpecifier,
		"enumerator_list": ab.buildEnumeratorList,
		"enumerator": ab.buildEnumerator,
		"struct_or_union_specifier": ab.buildStructSpecifier,
		"struct_or_union": ab.buildStructOrUnion,
		"struct_declaration_list": ab.buildStructDeclarationList,
		"struct_declaration": ab.buildStructDeclaration,
		"specifier_qualifier_list": ab.buildSpecifierQualifierList,
		"struct_declarator_list": ab.buildStructDeclaratorList,
		"struct_declarator": ab.buildStructDeclarator,
		"declarator": ab.buildDeclarator,
		"pointer": ab.buildPointer,
		"type_qualifier_list": ab.buildTypeQualifierList,
		"type_qualifier": ab.buildTypeQualifier,
		"direct_declarator": ab.buildDirectDeclarator,
		"parameter_type_list": ab.buildParameterTypeList,
		"parameter_list": ab.buildParameterList,
		"parameter_declaration": ab.buildParameterDeclaration,
		"declaration_specifiers": ab.buildDeclarationSpecifiers,
		"storage_class_specifier": ab.buildStorageClassSpecifier,
		"abstract_declarator": ab.buildAbstractDeclarator,
		"direct_abstract_declarator": ab.buildDirectAbstractDeclarator,
		"initializer_list": ab.buildInitializerList,
		"initializer": ab.buildInitializer,
		"declaration": ab.buildDeclaration,
		"declaration_list": ab.buildDeclarationList,
		"init_declarator_list": ab.buildInitDeclaratorList,
		"init_declarator": ab.buildInitDeclarator,
	}
	return ab
}

func (ab *Builder) lineInfoFor(node Node) LineInfo {
	return LineInfo{
		LineNumber: ab.lineNumber,
		Owner: node,
	}
}

func (ab *Builder) buildPrimaryExpression(prod *grammars.Production) (node Node, err error) {
	if len(prod.To) == 1 {
		sym := prod.To[0]
		token := ab.tokenStack.Pop()
		switch sym.Val { 
		case "IDENTIFIER":
			ie := IdentifierExpression{Identifier: token.V}
			ie.LineInfo = ab.lineInfoFor(ie)
			node = ie
		case "CONSTANT":
			ce := ConstantValExpression{Constant: token.V}
			ce.LineInfo = ab.lineInfoFor(ce)
			node = ce
		case "STRING_LITERAL":
			sli := StringLiteralExpression{StringLiteral: token.V}
			sli.LineInfo = ab.lineInfoFor(sli)
			node = sli
		default:
			panic("Invalid primary expression")
		} 
	} else {
		// ( expression )
		ab.tokenStack.PopMany(2)
		node = ab.reductionStack.Pop()
	}
	return
}

func (ab *Builder) buildPostfixExpression(prod *grammars.Production) (node Node, err error) {
	switch prod.To[1].Val {
	case "[":
		ab.tokenStack.PopMany(2)
		arrExpr := ab.reductionStack.Pop().(Expression)
		postfixExpr := ab.reductionStack.Pop().(Expression)
		aape := ArrayAccessPostfixExpression{
			PostfixExpression: &postfixExpr,
			ArrayExpression: &arrExpr,
		}
		aape.LineInfo = ab.lineInfoFor(aape)
		node = aape
	case "(":
		ab.tokenStack.PopMany(2)
		if prod.To[2].Val == "argument_expression_list" {
			ael := ab.reductionStack.Pop().(ArgumentExpressionList)
			funcExpr := ab.reductionStack.Pop().(Expression)
			fcpe := FunctionCallPostfixExpression{
				FunctionAccessor: &funcExpr,
				Args: &ael,
			}
			fcpe.LineInfo = ab.lineInfoFor(fcpe)
			node = fcpe
		} else {
			funcExpr := ab.reductionStack.Pop().(Expression)
			fcpe := FunctionCallPostfixExpression{
				FunctionAccessor: &funcExpr,
				Args: nil,
			}
			fcpe.LineInfo = ab.lineInfoFor(fcpe)
			node = fcpe
		}
	case "PTR_OP", ".":
		ab.tokenStack.Pop()
		expr := ab.reductionStack.Pop().(Expression)
		var accessMethod AccessMethod
		if prod.To[1].Val == "PTR_OP" {
			accessMethod = POINTER_ACCESS
		} else {
			accessMethod = DOT_ACCESS
		}
		sape := StructAccessPostfixExpression{
			StructAccessor: &expr,
			AccessMethod: accessMethod,
			FieldIdentifier: ab.tokenStack.Pop().V,
		}
		sape.LineInfo = ab.lineInfoFor(sape)
		node = sape
	case "INC_OP", "DEC_OP":
		ab.tokenStack.Pop()
		expr := ab.reductionStack.Pop().(Expression)
		var op IncDecOperator
		if prod.To[1].Val == "INC_OP" {
			op = INC
		} else {
			op = DEC
		}
		idpe := IncDecPostfixExpression{
			PostfixExpression: &expr,
			Operator: op,
		}
		idpe.LineInfo = ab.lineInfoFor(idpe)
		node = idpe
	default:
		panic("Invalid postfix expression")
	}
	return
}

func (ab *Builder) buildUnaryExpression(prod *grammars.Production) (node Node, err error) {
	switch prod.To[0].Val {
	case "INC_OP", "DEC_OP":
		ab.tokenStack.Pop()
		expr := ab.reductionStack.Pop().(Expression)
		var op IncDecOperator
		if prod.To[1].Val == "INC_OP" {
			op = INC
		} else {
			op = DEC
		}
		idue := IncDecUnaryExpression{
			UnaryExpression: &expr,
			Operator: op,
		}
		idue.LineInfo = ab.lineInfoFor(idue)
		node = idue
	case "SIZEOF":
		if prod.To[1].Val == "(" {
			ab.reductionStack.PopMany(3)
			typeName := ab.reductionStack.Pop().(TypeName)
			sue := SizeofUnaryExpression{
				TypeName: &typeName,
				NestedUnaryExpression: nil,
			}
			sue.LineInfo = ab.lineInfoFor(sue)
			node = sue
		} else {
			ab.tokenStack.Pop()
			expr := ab.reductionStack.Pop().(Expression)
			sue := SizeofUnaryExpression{
				TypeName: nil,
				NestedUnaryExpression: &expr,
			}
			sue.LineInfo = ab.lineInfoFor(sue)
			node = sue
		}
	case "unary_operator":
		castExpr := ab.reductionStack.Pop().(Expression)
		unaryOp := ab.reductionStack.Pop().(StringPrimitive)
		cue := CastUnaryExpression{
			CastExpression: &castExpr,
			Operator: unaryOp.Val,
		}
		cue.LineInfo = ab.lineInfoFor(cue)
		node = cue
	default: 
		panic("Invalid unary expression")
	}
	return
}

func (ab *Builder) buildCastExpression(prod *grammars.Production) (node Node, err error) {
	ab.tokenStack.PopMany(2)
	castExpr := ab.reductionStack.Pop().(Expression)
	typeName := ab.reductionStack.Pop().(TypeName)
	n := TypeCastCastExpression{
		Typename: &typeName,
		Expression: &castExpr,
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildBinaryExpress() (lhs Node, rhs Node, op string) {
	op = ab.tokenStack.Pop().V
	rhs = ab.reductionStack.Pop()
	lhs = ab.reductionStack.Pop()
	return
}

func (ab *Builder) buildArithmeticBinaryExpression(prod *grammars.Production) (node Node, err error) {
	op := ab.tokenStack.Pop().V
	rhs := ab.reductionStack.Pop().(Expression)
	lhs := ab.reductionStack.Pop().(Expression)
	
	n := BinaryArithmeticExpression{
		LhsExpression: &lhs,
		Operator: op,
		RhsExpression: &rhs,
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildConditionalExpression(prod *grammars.Production) (node Node, err error) {
	ab.tokenStack.PopMany(2)
	elseExpr := ab.reductionStack.Pop().(Expression)
	ifExpr := ab.reductionStack.Pop().(Expression)
	conditionExpr := ab.reductionStack.Pop().(Expression)
	n := ConditionalExpression{
		Condition: &conditionExpr,
		IfTrueExpression: &ifExpr,
		ElseExpression: &elseExpr,
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildAssigmentExpression(prod *grammars.Production) (node Node, err error) {
	rhs := ab.reductionStack.Pop().(Expression)
	assigmentOp := ab.reductionStack.Pop().(StringPrimitive)
	lhs := ab.reductionStack.Pop().(Expression)
	n := AssigmentExpression{
		LhsExpression: &lhs,
		Operator: assigmentOp.Val,
		RhsExpression: &rhs,
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildExpression(prod *grammars.Production) (node Node, err error) {
	if len(prod.To) == 1 {
		node = ab.reductionStack.Pop()
		return
	} else {
		panic(", expressions are unsupported")
	}
}

func (ab *Builder) buildConstantExpression(prod *grammars.Production) (node Node, err error) {
	expr := ab.reductionStack.Pop().(Expression)
	n := ConstantExpression{
		Expression: &expr,
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildAssigmentOperator(prod *grammars.Production) (node Node, err error) {
	n := StringPrimitive{Val: ab.tokenStack.Pop().V}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildUnaryOperator(prod *grammars.Production) (node Node, err error) {
	n := StringPrimitive{Val: ab.tokenStack.Pop().V}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildTypeName(prod *grammars.Production) (node Node, err error) {
	var ad *AbstractDeclarator = nil
	if len(prod.To) == 2 {
		a := ab.reductionStack.Pop().(AbstractDeclarator)
		ad = &a
	}
	sql := ab.reductionStack.Pop().(SpecifierQulifierList)
	n := TypeName{
		AbstractDeclarator: ad,	
		SpecifierQulifierList: &sql,
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildArgumentExpressionList(prod *grammars.Production) (node Node, err error) {
	if len(prod.To) == 1 {
		expr := ab.reductionStack.Pop().(Expression)
		n := ArgumentExpressionList{
			Expressions: []Expression{expr},
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	} else {
		ab.tokenStack.Pop()
		expr := ab.reductionStack.Pop().(Expression)
		ael := ab.reductionStack.Pop().(ArgumentExpressionList)
		ael.Expressions = append(ael.Expressions, expr)
		node = ael
	}
	return
}

func (ab *Builder) buildStatement(prod *grammars.Production) (node Node, err error) {
	node = ab.reductionStack.Pop()
	return
}

func (ab *Builder) buildLabeledStatement(prod *grammars.Production) (node Node, err error) {
	ab.tokenStack.Pop()
	stmnt := ab.reductionStack.Pop().(Statement)
	switch prod.To[0].Val {
	case "IDENTIFIER":
		ident := ab.tokenStack.Pop().V
		n := IdentifierLabeledStatement{
			Identifier: ident,
			Statement: &stmnt,
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	case "CASE":
		ab.tokenStack.Pop()
		expr := ab.reductionStack.Pop().(Expression)
		n := CaseLabeledStatement{
			Statement: &stmnt,
			Expression: &expr,
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	case "DEFAULT":
		ab.tokenStack.Pop()
		n := DefaultLabeledStatement{
			Statement: &stmnt,
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	}
	return
}

func (ab *Builder) buildCompoundStatement(prod *grammars.Production) (node Node, err error) {
	ab.tokenStack.PopMany(prod.SymbolsOfType(grammars.TERMINAL))
	var decList *DeclarationList = nil
	var stmntList *StatementList = nil
	if prod.To[1].Val == "declaration_list" {
		if prod.To[2].Val == "statement_list" {
			stl := ab.reductionStack.Pop().(StatementList)
			stmntList = &stl
		}
		dl := ab.reductionStack.Pop().(DeclarationList)
		decList = &dl
	} else if prod.To[1].Val == "statement_list" {
		stl := ab.reductionStack.Pop().(StatementList)
		stmntList = &stl
	}
	n := CompoundStatement{
		DeclarationList: decList,
		StatementList: stmntList,
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildExpressionStatement(prod *grammars.Production) (node Node, err error) {
	ab.tokenStack.Pop()
	if len(prod.To) == 1 {
		// a bit of a hack but for any program it will never be called with empty stack
		node = ab.reductionStack.Pop()
	} else {
		expr := ab.reductionStack.Pop().(Expression)
		n := ExpressionStatement{
			Expression: &expr,
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	}
	return
}

func (ab *Builder) buildSelectionStatement(prod *grammars.Production) (node Node, err error) {
	ab.tokenStack.PopMany(prod.SymbolsOfType(grammars.TERMINAL))
	if prod.To[0].Val == "IF" {
		var elseStmnt *Statement = nil
		if len(prod.To) == 7 {
			stmnt := ab.reductionStack.Pop().(Statement)
			elseStmnt = &stmnt
		}
		ifStmnt := ab.reductionStack.Pop().(Statement)
		cond := ab.reductionStack.Pop().(Expression)
		n := IfSelectionStatement{
			Condition: &cond,
			IfStatement: &ifStmnt,
			ElseStatement: elseStmnt,
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	} else {
		body := ab.reductionStack.Pop().(Statement)
		cond := ab.reductionStack.Pop().(Expression)
		n := SwitchSelectionStatement{
			SwitchExpression: &cond,
			SwitchBody: &body,
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	}
	return
}

func (ab *Builder) buildIterationStatement(prod *grammars.Production) (node Node, err error) {
	ab.tokenStack.PopMany(prod.SymbolsOfType(grammars.TERMINAL))
	switch prod.To[0].Val {
	case "WHILE":
		stmnt := ab.reductionStack.Pop().(Statement)
		cond := ab.reductionStack.Pop().(Expression)
		n := WhileIterationStatement{
			Condition: &cond,
			Body: &stmnt,
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	case "DO":
		cond := ab.reductionStack.Pop().(Expression)
		stmnt := ab.reductionStack.Pop().(Statement)
		n := DoWhileIterationStatement{
			Condition: &cond,
			Body: &stmnt,
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	case "FOR":
		stmnt := ab.reductionStack.Pop().(Statement)
		var updater Expression = nil
		if len(prod.To) == 7 {
			expr := ab.reductionStack.Pop().(Expression)
			updater = &expr
		}
		cond := ab.reductionStack.Pop().(ExpressionStatement)
		initializer := ab.reductionStack.Pop().(ExpressionStatement)
		n := ForIterationStatement{
			Initializer: initializer,
			Condition: cond,
			Updater: updater,
			Body: &stmnt,
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	}
	return
}

func (ab *Builder) buildJumpStatement(prod *grammars.Production) (node Node, err error) {
	firstSym := prod.To[0]
	if len(prod.To) == 2 {
		ab.tokenStack.PopMany(2)
		switch firstSym.Val {
		case "CONTINUE":
			n := LoopControlJumpStatement{
				ControlOption: CONTINUE,
			}
			n.LineInfo = ab.lineInfoFor(n)
			node = n
		case "BREAK":
			n := LoopControlJumpStatement{
				ControlOption: BREAK,
			}
			n.LineInfo = ab.lineInfoFor(n)
			node = n
		case "RETURN":
			n := ReturnJumpStatement{
				Expression: nil,
			}
			n.LineInfo = ab.lineInfoFor(n)
			node = n
		}
	} else if firstSym.Val == "GOTO" {
		ab.tokenStack.Pop()
		label := ab.tokenStack.Pop().V
		n  := GotoJumpStatement{
			Label: label,
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
		ab.tokenStack.Pop()
	} else {
		ab.tokenStack.PopMany(2)
		expr := ab.reductionStack.Pop().(Expression)
		n := ReturnJumpStatement{
			Expression: &expr,
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	}
	return
}

func (ab *Builder) buildStatementList(prod *grammars.Production) (node Node, err error) {
	if len(prod.To) == 1 {
		stmnt := ab.reductionStack.Pop().(Statement)
		n := StatementList{
			Statements: []Statement{stmnt},
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	} else {
		stmnt := ab.reductionStack.Pop().(Statement)
		stmntList := ab.reductionStack.Pop().(StatementList)
		stmntList.Statements = append(stmntList.Statements, stmnt)
		node = stmntList
	}
	return
}

func (ab *Builder) buildFunctionDefinition(prod *grammars.Production) (node Node, err error) {
	cs := ab.reductionStack.Pop().(CompoundStatement)
	var decSpecs *DeclarationSpecifiers = nil
	var dec *Declarator = nil
	var decList *DeclarationList = nil 
	if prod.To[len(prod.To) - 2].Val == "declarator" {
		dc := ab.reductionStack.Pop().(Declarator)
		dec = &dc
	} else {
		dl := ab.reductionStack.Pop().(DeclarationList)
		decList = &dl
		dc := ab.reductionStack.Pop().(Declarator)
		dec = &dc
	}
	if prod.To[0].Val == "declaration_specifiers" {
		ds := ab.reductionStack.Pop().(DeclarationSpecifiers)
		decSpecs = &ds
	}
	n := FunctionDefinition{
		DeclarationSpecifiers: decSpecs,
		Declarator: dec,
		DeclarationList: decList,
		Body: &cs,
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildExternalDeclaration(prod *grammars.Production) (node Node, err error) {
	node = ab.reductionStack.Pop()
	return
}

func (ab *Builder) buildTranslationUnit(prod *grammars.Production) (node Node, err error) {
	if len(prod.To) == 1 {
		dec := ab.reductionStack.Pop().(ExternalDeclaration)
		n := TranslationUnit{
			ExternalDeclarations: []ExternalDeclaration{dec},
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	} else {
		dec := ab.reductionStack.Pop().(ExternalDeclaration)
		tu := ab.reductionStack.Pop().(TranslationUnit)
		tu.ExternalDeclarations = append(tu.ExternalDeclarations, dec)
		tu.LineInfo = ab.lineInfoFor(tu)
		node = tu
	}
	return
}

func (ab *Builder) buildTypeSpecifier(prod *grammars.Production) (node Node, err error) {
	if prod.To[0].T == grammars.TERMINAL {
		typeName := ab.tokenStack.Pop().V
		n := DirectTypeSpecifier{
			TypeName: typeName,
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	} else {
		node = ab.reductionStack.Pop()
	} 
	return
}

func (ab *Builder) buildEnumSpecifier(prod *grammars.Production) (node Node, err error) {
	var ident *string = nil
	var enumList *EnumeratorList = nil
	if len(prod.To) > 2 {
		ab.tokenStack.PopMany(2)
		el := ab.reductionStack.Pop().(EnumeratorList)
		enumList = &el
	} 
	if prod.To[1].Val == "IDENTIFIER" {
		iden := ab.tokenStack.Pop().V
		ident = &iden
	}
	ab.tokenStack.Pop()
	n := EnumTypeSpecifier{
		Identifier: ident,
		EnumeratorList: enumList,
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildStructSpecifier(prod *grammars.Production) (node Node, err error) {
	var ident *string = nil
	var structDecList *StructDeclarationList = nil
	if len(prod.To) > 2 {
		ab.tokenStack.PopMany(2)
		sl := ab.reductionStack.Pop().(StructDeclarationList)
		structDecList = &sl
	} 
	if prod.To[1].Val == "IDENTIFIER" {
		iden := ab.tokenStack.Pop().V
		ident = &iden
	}
	n := StructTypeSpecifier{
		Identifier: ident,
		StructDeclarationList: structDecList,
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	ab.reductionStack.Pop()
	return
}

func (ab *Builder) buildEnumeratorList(prod *grammars.Production) (node Node, err error) {
	if len(prod.To) == 1 {
		enum := ab.reductionStack.Pop().(Enumerator)
		n := EnumeratorList{
			Enumerators: []*Enumerator{&enum},
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	} else {
		enum := ab.reductionStack.Pop().(Enumerator)
		el := ab.reductionStack.Pop().(EnumeratorList)
		el.Enumerators = append(el.Enumerators, &enum)
		el.LineInfo = ab.lineInfoFor(el)
		node = el
	}
	return
}

func (ab *Builder) buildStructDeclarationList(prod *grammars.Production) (node Node, err error) {
	if len(prod.To) == 1 {
		sd := ab.reductionStack.Pop().(StructDeclaration)
		n := StructDeclarationList{
			StructDeclarations: []*StructDeclaration{&sd},
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	} else {
		sd := ab.reductionStack.Pop().(StructDeclaration)
		sdl := ab.reductionStack.Pop().(StructDeclarationList)
		sdl.StructDeclarations = append(sdl.StructDeclarations, &sd)
		sdl.LineInfo = ab.lineInfoFor(sdl)
		node = sdl
	}
	return
}

func (ab *Builder) buildStructDeclaration(prod *grammars.Production) (node Node, err error) {
	ab.tokenStack.Pop()
	sdl := ab.reductionStack.Pop().(StructDeclaratorList)
	sql := ab.reductionStack.Pop().(SpecifierQulifierList)
	n := StructDeclaration{
		SpecifierQulifierList: &sql,
		StructDeclaratorList: &sdl,
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildSpecifierQualifierList(prod *grammars.Production) (node Node, err error) {
	var n SpecifierQulifierList
	if len(prod.To) == 2 {
		n = ab.reductionStack.Pop().(SpecifierQulifierList)
	} else {
		n = SpecifierQulifierList{
			TypeSpecifiers: make([]TypeSpecifier, 0),
			TypeQualifiers: make([]*TypeQualifier, 0),
		}
	}
	if prod.To[0].Val == "type_specifier" {
		ts := ab.reductionStack.Pop().(TypeSpecifier)
		n.TypeSpecifiers = append(n.TypeSpecifiers, ts)
	} else {
		tq := ab.reductionStack.Pop().(TypeQualifier)
		n.TypeQualifiers = append(n.TypeQualifiers, &tq)
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildStructDeclaratorList(prod *grammars.Production) (node Node, err error) {
	if len(prod.To) == 1 {
		sd := ab.reductionStack.Pop().(StructDeclarator)
		n := StructDeclaratorList{
			StructDeclarators: []*StructDeclarator{&sd},
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	} else {
		ab.tokenStack.Pop()
		sd := ab.reductionStack.Pop().(StructDeclarator)
		sdl := ab.reductionStack.Pop().(StructDeclaratorList)
		sdl.StructDeclarators = append(sdl.StructDeclarators, &sd)
		sdl.LineInfo = ab.lineInfoFor(sdl)
		node = sdl
	}
	return
}

func (ab *Builder) buildStructDeclarator(prod *grammars.Production) (node Node, err error) {
	var expr Expression = nil
	if prod.To[len(prod.To) - 1].Val == "constant_expression" {
		e := ab.reductionStack.Pop().(Expression)
		expr = &e
		ab.tokenStack.Pop()
	}
	decl := ab.reductionStack.Pop().(Declarator)
	n := StructDeclarator{
		Declarator: &decl,
		Expression: expr,
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildStructOrUnion(prod *grammars.Production) (node Node, err error) {
	node = StringPrimitive{Val: ab.tokenStack.Pop().V}
	return
}

func (ab *Builder) buildDeclarator(prod *grammars.Production) (node Node, err error) {
	decl := ab.reductionStack.Pop().(DirectDeclarator)
	var n Declarator
	if len(prod.To) == 1 {
		n = Declarator{
			Pointer: nil,
			DirectDeclarator: decl,
		}
	} else {
		ptr := ab.reductionStack.Pop().(Pointer)
		n = Declarator{
			Pointer: &ptr,
			DirectDeclarator: decl,
		}
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildPointer(prod *grammars.Production) (node Node, err error) {
	ab.tokenStack.Pop()
	var tql *TypeQualifierList
	var ptr *Pointer
	if prod.To[len(prod.To) - 1].Val == "pointer" {
		p := ab.reductionStack.Pop().(Pointer)
		ptr = &p
	}
	if len(prod.To) > 1 && prod.To[1].Val == "type_qualifier_list" {
		t := ab.reductionStack.Pop().(TypeQualifierList)
		tql = &t
	}
	n := Pointer{
		TypeQualifierList: tql,
		NestedPointer: ptr,
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildTypeQualifierList(prod *grammars.Production) (node Node, err error) {
	if len(prod.To) == 1 {
		tq := ab.reductionStack.Pop().(TypeQualifier)
		n := TypeQualifierList{
			TypeQualifiers: []*TypeQualifier{&tq},
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	} else {
		tq := ab.reductionStack.Pop().(TypeQualifier)
		tql := ab.reductionStack.Pop().(TypeQualifierList)
		tql.TypeQualifiers = append(tql.TypeQualifiers, &tq)
		tql.LineInfo = ab.lineInfoFor(tql)
		node = tql
	}
	return
}

func (ab *Builder) buildTypeQualifier(prod *grammars.Production) (node Node, err error) {
	node = TypeQualifier{Val: ab.tokenStack.Pop().V}
	return
}

func (ab *Builder) buildDirectDeclarator(prod *grammars.Production) (node Node, err error) {
	if len(prod.To) == 1 {
		ident := ab.tokenStack.Pop().V
		n := DirectIdentifierDeclarator{
			Identifier: ident,
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	} else if prod.To[1].Val == "[" {
		ab.tokenStack.PopMany(2)
		var expr Expression = nil
		if prod.To[2].Val == "constant_expression" {
			e := ab.reductionStack.Pop().(Expression)
			expr = &e
		}
		decl := ab.reductionStack.Pop().(DirectDeclarator)
		n := DirectArrayDeclarator{
			Declarator: decl,
			ArrayExpression: expr,
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	} else if prod.To[1].Val == "declarator" {
		ab.tokenStack.PopMany(2)
		node = ab.reductionStack.Pop()
	} else {
		ab.tokenStack.PopMany(2)
		var ptl *ParameterTypeList = nil
		var il *IdentifierList = nil
		if prod.To[2].T == grammars.NONTERMINAL {
			if prod.To[2].Val == "parameter_type_list" {
				p := ab.reductionStack.Pop().(ParameterTypeList)
				ptl = &p
			} else {
				i := ab.reductionStack.Pop().(IdentifierList)
				il = &i
			}
		}
		decl := ab.reductionStack.Pop().(DirectDeclarator)
		n := DirectFunctionDeclarator{
			Declarator: decl,
			ParameterTypeList: ptl,
			IndentifierList: il,
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	}
	return
}

func (ab *Builder) buildParameterTypeList(prod *grammars.Production) (node Node, err error) {
	if len(prod.To) == 1 {
		pl := ab.reductionStack.Pop().(ParameterList)
		n := ParameterTypeList{
			ParameterList: &pl,
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	} else {
		panic("ellipsis not supported")
	}
	return
}

func (ab *Builder) buildParameterList(prod *grammars.Production) (node Node, err error) {
	if len(prod.To) == 1 {
		pd := ab.reductionStack.Pop().(ParameterDeclaration)
		n := ParameterList{
			ParameterDeclarations: []*ParameterDeclaration{&pd},
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	} else {
		ab.tokenStack.Pop()
		pd := ab.reductionStack.Pop().(ParameterDeclaration)
		pl := ab.reductionStack.Pop().(ParameterList)
		pl.ParameterDeclarations = append(pl.ParameterDeclarations, &pd)
		pl.LineInfo = ab.lineInfoFor(pl)
		node = pl
	}
	return
}

func (ab *Builder) buildParameterDeclaration(prod *grammars.Production) (node Node, err error) {
	var dec *Declarator = nil
	var adec *AbstractDeclarator = nil
	if len(prod.To) > 1 {
		if prod.To[1].Val == "declarator" {
			d := ab.reductionStack.Pop().(Declarator)
			dec = &d
		} else {
			ad := ab.reductionStack.Pop().(AbstractDeclarator)
			adec = &ad
		}
	}
	dspec := ab.reductionStack.Pop().(DeclarationSpecifiers)
	n := ParameterDeclaration{
		DeclarationSpecifiers: &dspec,
		Declarator: dec,
		AbstractDeclarator: adec,
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildDeclarationSpecifiers(prod *grammars.Production) (node Node, err error) {
	var n DeclarationSpecifiers
	if len(prod.To) == 2 {
		n = ab.reductionStack.Pop().(DeclarationSpecifiers)
	} else {
		n = DeclarationSpecifiers{
			TypeSpecifiers: make([]TypeSpecifier, 0),
			TypeQualifiers: make([]*TypeQualifier, 0),
			StorageClassSpecifiers: make([]*StorageClassSpecifier, 0),
		}
	}
	switch prod.To[0].Val {
	case "type_specifier":
		ts := ab.reductionStack.Pop().(TypeSpecifier)
		n.TypeSpecifiers = append(n.TypeSpecifiers, ts)
	case "type_qualifier":
		tq := ab.reductionStack.Pop().(TypeQualifier)
		n.TypeQualifiers = append(n.TypeQualifiers, &tq)
	default:
		scs := ab.reductionStack.Pop().(StorageClassSpecifier)
		n.StorageClassSpecifiers = append(n.StorageClassSpecifiers, &scs)
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildStorageClassSpecifier(prod *grammars.Production) (node Node, err error) {
	n := StorageClassSpecifier{Val: ab.tokenStack.Pop().V}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildAbstractDeclarator(prod *grammars.Production) (node Node, err error) {
	var ptr *Pointer = nil
	var dad DirectAbstractDeclarator = nil
	if len(prod.To) == 2 || prod.To[0].Val != "pointer" {
		d := ab.reductionStack.Pop().(DirectAbstractDeclarator)
		dad = d
	}
	if prod.To[0].Val == "pointer" {
		p := ab.reductionStack.Pop().(Pointer)
		ptr = &p
	}
	n := AbstractDeclarator{
		Pointer: ptr,
		DirectAbstractDeclarator: dad,
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildDirectAbstractDeclarator(prod *grammars.Production) (node Node, err error) {
	ab.tokenStack.PopMany(prod.SymbolsOfType(grammars.TERMINAL))

	if prod.To[1].Val == "abstract_declarator" {
		node = ab.reductionStack.Pop()
	} else if prod.To[len(prod.To) - 1].Val == "]" {
		var expr Expression = nil
		var dad DirectAbstractDeclarator = nil
		if prod.To[len(prod.To) - 2].Val == "constant_expression" { 
			e := ab.reductionStack.Pop().(Expression)
			expr = e
		}
		if prod.To[0].Val == "direct_abstract_declarator" {
			d := ab.reductionStack.Pop().(DirectAbstractDeclarator)
			dad = d
		}
		n := DirectAbstractArrayDeclarator{
			Expression: expr,
			DirectAbstractDeclarator: dad,
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	} else {
		var ptl *ParameterTypeList = nil
		var dad DirectAbstractDeclarator = nil
		if prod.To[len(prod.To) - 2].Val == "parameter_type_list" { 
			p := ab.reductionStack.Pop().(ParameterTypeList)
			ptl = &p
		}
		if prod.To[0].Val == "direct_abstract_declarator" {
			d := ab.reductionStack.Pop().(DirectAbstractDeclarator)
			dad = d
		}
		n := DirectAbstractFunctionDeclarator{
			ParameterTypeList: ptl,
			DirectAbstractDeclarator: dad,
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	}
	return
}

func (ab *Builder) buildInitializerList(prod *grammars.Production) (node Node, err error) {
	if len(prod.To) == 1 {
		il := ab.reductionStack.Pop().(Initializer)
		n := InitializerList{
			Initializers: []*Initializer{&il},
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	} else {
		ab.tokenStack.Pop()
		il := ab.reductionStack.Pop().(Initializer)
		ill := ab.reductionStack.Pop().(InitializerList)
		ill.Initializers = append(ill.Initializers, &il)
		ill.LineInfo = ab.lineInfoFor(ill)
		node = ill
	}
	return
}

func (ab *Builder) buildInitializer(prod *grammars.Production) (node Node, err error) {
	var n Initializer
	if prod.To[0].Val == "assignment_expression" {
		expr := ab.reductionStack.Pop().(Expression)
		n = Initializer{
			Expression: expr,
			InitializerList: nil,
		}
	} else {
		ab.tokenStack.PopMany(prod.SymbolsOfType(grammars.TERMINAL))
		il := ab.reductionStack.Pop().(InitializerList)
		n = Initializer{
			Expression: nil,
			InitializerList: &il,
		}
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildDeclaration(prod *grammars.Production) (node Node, err error) {
	var idl *InitDeclaratorList = nil 
	ab.tokenStack.Pop()
	if prod.To[1].Val == "init_declarator_list" {
		i := ab.reductionStack.Pop().(InitDeclaratorList)
		idl = &i
	}
	ds := ab.reductionStack.Pop().(DeclarationSpecifiers)
	n := Declaration{
		DeclarationSpecifiers: &ds,
		InitDeclaratorList: idl,
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildDeclarationList(prod *grammars.Production) (node Node, err error) {
	if len(prod.To) == 1 {
		dec := ab.reductionStack.Pop().(Declaration)
		n := DeclarationList{
			Declarations: []*Declaration{&dec},
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	} else {
		dec := ab.reductionStack.Pop().(Declaration)
		dl := ab.reductionStack.Pop().(DeclarationList)
		dl.Declarations = append(dl.Declarations, &dec)
		dl.LineInfo = ab.lineInfoFor(dl)
		node = dl
	}
	return
}

func (ab *Builder) buildInitDeclaratorList(prod *grammars.Production) (node Node, err error) {
	if len(prod.To) == 1 {
		id := ab.reductionStack.Pop().(InitDeclarator)
		n := InitDeclaratorList{
			InitDeclarators: []*InitDeclarator{&id},
		}
		n.LineInfo = ab.lineInfoFor(n)
		node = n
	} else {
		ab.tokenStack.Pop()
		id := ab.reductionStack.Pop().(InitDeclarator)
		idl := ab.reductionStack.Pop().(InitDeclaratorList)
		idl.InitDeclarators = append(idl.InitDeclarators, &id)
		idl.LineInfo = ab.lineInfoFor(idl)
		node = idl
	}
	return
}

func (ab *Builder) buildInitDeclarator(prod *grammars.Production) (node Node, err error) {
	var ini *Initializer
	if len(prod.To)	> 1 {
		ab.tokenStack.Pop()
		i := ab.reductionStack.Pop().(Initializer)
		ini = &i
	}
	dec := ab.reductionStack.Pop().(Declarator)
	n := InitDeclarator{
		Declarator: &dec,
		Initializer: ini,
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) buildEnumerator(prod *grammars.Production) (node Node, err error) {
	var expr Expression = nil
	if len(prod.To) > 1 {
		e := ab.reductionStack.Pop().(Expression)
		expr = &e
		ab.tokenStack.Pop()
	}
	ident := ab.tokenStack.Pop().V
	n := Enumerator{
		Identifier: ident,
		Expression: expr,
	}
	n.LineInfo = ab.lineInfoFor(n)
	node = n
	return
}

func (ab *Builder) reduceStatement(prod *grammars.Production) (node Node, err error) {
	switch prod.From {
	case "statement":
		node, err = ab.buildStatement(prod)
	case "labeled_statement":
		node, err = ab.buildLabeledStatement(prod)
	case "compound_statement":
		node, err = ab.buildCompoundStatement(prod)
	case "expression_statement":
		node, err = ab.buildExpressionStatement(prod)
	case "selection_statement":
		node, err = ab.buildSelectionStatement(prod)
	case "iteration_statement":
		node, err = ab.buildIterationStatement(prod)
	case "jump_statement":
		node, err = ab.buildJumpStatement(prod)
	case "statement_list":
		node, err = ab.buildStatementList(prod)
	}
	return
}

func (ab *Builder) OnReduce(prod *grammars.Production, lineInfo *LineInfo) error {
	var err error = nil
	var node Node
	ab.lineNumber = lineInfo.LineNumber
	if prod.From == "primary_expression" {
		node, err = ab.buildPrimaryExpression(prod)
	} else if strings.HasSuffix(prod.From, "_expression") {
		if len(prod.To)	== 1  {
			return nil
		}
		node, err = ab.expressionBuilders[prod.From[:len(prod.From) - 11]](prod)
	} else if strings.Contains(prod.From, "statement") {
		node, err = ab.reduceStatement(prod)
	} else {
		if builder, ok := ab.miscBuilders[prod.From]; ok {
			node, err = builder(prod)
		} else {
			panic("Unsupported production from " + prod.From)
		}
	}

	if err != nil {
		return err
	}
	if node == nil {
		panic("Reduction failed, got nil for production from " + prod.From)
	}

	ab.reductionStack.Push(node)
	return nil
}

func (ab *Builder) OnShift(tok tok.Token) {
	ab.tokenStack.Push(tok)
}

func (ab *Builder) GetParsedTree() (TranslationUnit, error) {
	if ab.reductionStack.Size() == 0 {
		var tr TranslationUnit
		return tr, errors.New("Empty AST")
	}
	return ab.reductionStack.Peek().(TranslationUnit), nil
}

