package parsers

import (
	"errors"
	"grammars"
	"strings"
	tok "tokenizers"
	"utils"
)

type ASTBuilder struct {
	reductionStack *utils.Stack[ASTNode]
	tokenStack *utils.Stack[tok.Token]
	expressionBuilders map[string]func(*grammars.Production) (ASTNode, error)
}

func newASTBuilder() *ASTBuilder {
	ab := &ASTBuilder{
		tokenStack: utils.NewStack[tok.Token](),
		reductionStack: utils.NewStack[ASTNode](),
	}
	ab.expressionBuilders = map[string]func(*grammars.Production) (ASTNode, error){
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
	return ab
}

func (ab *ASTBuilder) buildPrimaryExpression(prod *grammars.Production) (node ASTNode, err error) {
	if len(prod.To) == 1 {
		sym := prod.To[0]
		token := ab.tokenStack.Pop()
		switch sym.Val { 
		case "IDENTIFIER":
			node = &IdentifierExpression{Identifier: token.V}
		case "CONSTANT":
			node = &ConstantValExpression{Constant: token.V}
		case "STRING_LITERAL":
			node = &StringLiteralExpression{StringLiteral: token.V}
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

func (ab *ASTBuilder) buildPostfixExpression(prod *grammars.Production) (node ASTNode, err error) {
	switch prod.To[1].Val {
	case "[":
		ab.tokenStack.PopMany(2)
		arrExpr := ab.reductionStack.Pop().(Expression)
		postfixExpr := ab.reductionStack.Pop().(Expression)
		node = ArrayAccessPostfixExpression{
			PostfixExpression: &postfixExpr,
			ArrayExpression: &arrExpr,
		}
	case "(":
		ab.tokenStack.PopMany(2)
		if prod.To[2].Val == "argument_expression_list" {
			ael := ab.reductionStack.Pop().(ArgumentExpressionList)
			funcExpr := ab.reductionStack.Pop().(Expression)
			node = FunctionCallPostfixExpression{
				FunctionAccessor: &funcExpr,
				Args: &ael,
			}
		} else {
			funcExpr := ab.reductionStack.Pop().(Expression)
			node = FunctionCallPostfixExpression{
				FunctionAccessor: &funcExpr,
				Args: nil,
			}
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
		node = StructAccessPostfixExpression{
			StructAccessor: &expr,
			AccessMethod: accessMethod,
			FieldIdentifier: ab.tokenStack.Pop().V,
		}
	case "INC_OP", "DEC_OP":
		ab.tokenStack.Pop()
		expr := ab.reductionStack.Pop().(Expression)
		var op IncDecOperator
		if prod.To[1].Val == "INC_OP" {
			op = INC
		} else {
			op = DEC
		}
		node = IncDecPostfixExpression{
			PostfixExpression: &expr,
			Operator: op,
		}
	default:
		panic("Invalid postfix expression")
	}
	return
}

func (ab *ASTBuilder) buildUnaryExpression(prod *grammars.Production) (node ASTNode, err error) {
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
		node = IncDecUnaryExpression{
			UnaryExpression: &expr,
			Operator: op,
		}
	case "SIZEOF":
		if prod.To[1].Val == "(" {
			ab.reductionStack.PopMany(3)
			typeName := ab.reductionStack.Pop().(TypeName)
			node = SizeofUnaryExpression{
				TypeName: &typeName,
				NestedUnaryExpression: nil,
			}
		} else {
			ab.tokenStack.Pop()
			expr := ab.reductionStack.Pop().(Expression)
			node = SizeofUnaryExpression{
				TypeName: nil,
				NestedUnaryExpression: &expr,
			}
		}
	case "unary_operator":
		castExpr := ab.reductionStack.Pop().(Expression)
		unaryOp := ab.reductionStack.Pop().(string)
		node = CastUnaryExpression{
			CastExpression: &castExpr,
			Operator: unaryOp,
		}
	default: 
		panic("Invalid unary expression")
	}
	return
}

func (ab *ASTBuilder) buildCastExpression(prod *grammars.Production) (node ASTNode, err error) {
	ab.tokenStack.PopMany(2)
	castExpr := ab.reductionStack.Pop().(Expression)
	typeName := ab.reductionStack.Pop().(TypeName)
	node = TypeCastCastExpression{
		Typename: &typeName,
		Expression: &castExpr,
	}
	return
}

func (ab *ASTBuilder) buildBinaryExpress() (lhs ASTNode, rhs ASTNode, op string) {
	op = ab.tokenStack.Pop().V
	rhs = ab.reductionStack.Pop()
	lhs = ab.reductionStack.Pop()
	return
}

func (ab *ASTBuilder) buildArithmeticBinaryExpression(prod *grammars.Production) (node ASTNode, err error) {
	op := ab.tokenStack.Pop().V
	rhs := ab.reductionStack.Pop().(Expression)
	lhs := ab.reductionStack.Pop().(Expression)
	node = BinaryArithmeticExpression{
		LhsExpression: &lhs,
		Operator: op,
		RhsExpression: &rhs,
	}
	return
}

func (ab *ASTBuilder) buildConditionalExpression(prod *grammars.Production) (node ASTNode, err error) {
	ab.tokenStack.PopMany(2)
	elseExpr := ab.reductionStack.Pop().(Expression)
	ifExpr := ab.reductionStack.Pop().(Expression)
	conditionExpr := ab.reductionStack.Pop().(Expression)
	node = &ConditionalExpression{
		Condition: &conditionExpr,
		IfTrueExpression: &ifExpr,
		ElseExpression: &elseExpr,
	}
	return
}

func (ab *ASTBuilder) buildAssigmentExpression(prod *grammars.Production) (node ASTNode, err error) {
	rhs := ab.reductionStack.Pop().(Expression)
	assigmentOp := ab.reductionStack.Pop().(string)
	lhs := ab.reductionStack.Pop().(Expression)
	node = AssigmentExpression{
		LhsExpression: &lhs,
		Operator: assigmentOp,
		RhsExpression: &rhs,
	}
	return
}

func (ab *ASTBuilder) buildExpression(prod *grammars.Production) (node ASTNode, err error) {
	if len(prod.To) == 1 {
		node = ab.reductionStack.Pop()
		return
	} else {
		panic(", expressions are unsupported")
	}
}

func (ab *ASTBuilder) buildConstantExpression(prod *grammars.Production) (node ASTNode, err error) {
	expr := ab.reductionStack.Pop().(Expression)
	node = ConstantExpression{
		Expression: &expr,
	}
	return
}

func (ab *ASTBuilder) buildAssigmentOperator(prod *grammars.Production) (node ASTNode, err error) {
	node = ab.tokenStack.Pop().V
	return
}

func (ab *ASTBuilder) buildUnaryOperator(prod *grammars.Production) (node ASTNode, err error) {
	node = ab.tokenStack.Pop().V
	return
}

func (ab *ASTBuilder) buildTypeName(prod *grammars.Production) (node ASTNode, err error) {
	node = ab.tokenStack.Pop().V
	return
}

func (ab *ASTBuilder) buildArgumentExpressionList(prod *grammars.Production) (node ASTNode, err error) {
	if len(prod.To) == 1 {
		expr := ab.reductionStack.Pop().(Expression)
		node = ArgumentExpressionList{
			Expressions: []*Expression{&expr},
		}
	} else {
		ab.tokenStack.Pop()
		expr := ab.reductionStack.Pop().(Expression)
		ael := ab.reductionStack.Pop().(ArgumentExpressionList)
		ael.Expressions = append(ael.Expressions, &expr)
		node = ael
	}
	return
}

func (ab *ASTBuilder) buildStatement(prod *grammars.Production) (node ASTNode, err error) {
	node = ab.reductionStack.Pop()
	return
}

func (ab *ASTBuilder) buildLabeledStatement(prod *grammars.Production) (node ASTNode, err error) {
	ab.tokenStack.Pop()
	stmnt := ab.reductionStack.Pop().(Statement)
	switch prod.To[0].Val {
	case "IDENTIFIER":
		ident := ab.tokenStack.Pop().V
		node = IdentifierLabeledStatement{
			Identifier: ident,
			Statement: &stmnt,
		}
	case "CASE":
		ab.tokenStack.Pop()
		expr := ab.reductionStack.Pop().(Expression)
		node = CaseLabeledStatement{
			Statement: &stmnt,
			Expression: &expr,
		}
	case "DEFAULT":
		ab.tokenStack.Pop()
		node = DefaultLabeledStatement{
			Statement: &stmnt,
		}
	}
	return
}

func (ab *ASTBuilder) buildCompoundStatement(prod *grammars.Production) (node ASTNode, err error) {
	ab.reductionStack.PopMany(prod.SymbolsOfType(grammars.TERMINAL))
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
	node = CompoundStatement{
		DeclarationList: decList,
		StatementList: stmntList,
	}
	return
}

func (ab *ASTBuilder) buildExpressionStatement(prod *grammars.Production) (node ASTNode, err error) {
	ab.tokenStack.Pop()
	if len(prod.To) == 1 {
		// a bit of a hack but for any program it will never be called with empty stack
		node = ab.reductionStack.Pop()
	} else {
		expr := ab.reductionStack.Pop().(Expression)
		node = ExpressionStatement{
			Expression: &expr,
		}
	}
	return
}

func (ab *ASTBuilder) buildSelectionStatement(prod *grammars.Production) (node ASTNode, err error) {
	ab.tokenStack.PopMany(prod.SymbolsOfType(grammars.TERMINAL))
	if prod.To[0].Val == "IF" {
		var elseStmnt *Statement = nil
		if len(prod.To) == 7 {
			stmnt := ab.reductionStack.Pop().(Statement)
			elseStmnt = &stmnt
		}
		ifStmnt := ab.reductionStack.Pop().(Statement)
		cond := ab.reductionStack.Pop().(Expression)
		node = IfSelectionStatement{
			Condition: &cond,
			IfStatement: &ifStmnt,
			ElseStatement: elseStmnt,
		}
	} else {
		body := ab.reductionStack.Pop().(Statement)
		cond := ab.reductionStack.Pop().(Expression)
		node = SwitchSelectionStatement{
			SwitchExpression: &cond,
			SwitchBody: &body,
		}
	}
	return
}

func (ab *ASTBuilder) buildIterationStatement(prod *grammars.Production) (node ASTNode, err error) {
	ab.tokenStack.PopMany(prod.SymbolsOfType(grammars.TERMINAL))
	switch prod.To[0].Val {
	case "WHILE":
		stmnt := ab.reductionStack.Pop().(Statement)
		cond := ab.reductionStack.Pop().(Expression)
		node = WhileIterationStatement{
			Condition: &cond,
			Body: &stmnt,
		}
	case "DO":
		cond := ab.reductionStack.Pop().(Expression)
		stmnt := ab.reductionStack.Pop().(Statement)
		node = DoWhileIterationStatement{
			Condition: &cond,
			Body: &stmnt,
		}
	case "FOR":
		stmnt := ab.reductionStack.Pop().(Statement)
		var updater *Expression = nil
		if len(prod.To) == 7 {
			expr := ab.reductionStack.Pop().(Expression)
			updater = &expr
		}
		cond := ab.reductionStack.Pop().(ExpressionStatement)
		initializer := ab.reductionStack.Pop().(ExpressionStatement)
		node = ForIterationStatement{
			Initializer: &initializer,
			Condition: &cond,
			Updater: updater,
			Body: &stmnt,
		}
	}
	return
}

func (ab *ASTBuilder) buildJumpStatement(prod *grammars.Production) (node ASTNode, err error) {
	firstSym := prod.To[0]
	if len(prod.To) == 1 {
		ab.tokenStack.PopMany(2)
		switch firstSym.Val {
		case "CONTINUE":
			node = LoopControlJumpStatement{
				ControlOption: CONTINUE,
			}
		case "BREAK":
			node = LoopControlJumpStatement{
				ControlOption: BREAK,
			}
		case "RETURN":
			node = ReturnJumpStatement{
				Expression: nil,
			}
		}
	} else if firstSym.Val == "GOTO" {
		ab.tokenStack.Pop()
		label := ab.tokenStack.Pop().V
		node = GotoJumpStatement{
			Label: label,
		}
		ab.tokenStack.Pop()
	} else {
		ab.tokenStack.PopMany(2)
		expr := ab.reductionStack.Pop().(Expression)
		node = ReturnJumpStatement{
			Expression: &expr,
		}
	}
	return
}

func (ab *ASTBuilder) buildStatementList(prod *grammars.Production) (node ASTNode, err error) {
	if len(prod.To) == 1 {
		stmnt := ab.reductionStack.Pop().(Statement)
		node = StatementList{
			Statements: []*Statement{&stmnt},
		}
	} else {
		stmnt := ab.reductionStack.Pop().(Statement)
		stmntList := ab.reductionStack.Pop().(StatementList)
		stmntList.Statements = append(stmntList.Statements, &stmnt)
		node = stmntList
	}
	return
}

func (ab *ASTBuilder) buildFunctionDefinition(prod *grammars.Production) (node ASTNode, err error) {
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
	node = FunctionDefinition{
		DeclarationSpecifiers: decSpecs,
		Declarator: dec,
		DeclarationList: decList,
		Body: &cs,
	}
	return
}

func (ab *ASTBuilder) buildExternalDeclaration(prod *grammars.Production) (node ASTNode, err error) {
	node = ab.reductionStack.Pop()
	return
}

func (ab *ASTBuilder) buildTranslationUnit(prod *grammars.Production) (node ASTNode, err error) {
	if len(prod.To) == 1 {
		dec := ab.reductionStack.Pop().(ExternalDeclaration)
		node = TranslationUnit{
			ExternalDeclarations: []*ExternalDeclaration{&dec},
		}
	} else {
		dec := ab.reductionStack.Pop().(ExternalDeclaration)
		tu := ab.reductionStack.Pop().(TranslationUnit)
		tu.ExternalDeclarations = append(tu.ExternalDeclarations, &dec)
		node = tu
	}
	return
}

func (ab *ASTBuilder) reduceStatement(prod *grammars.Production) (node ASTNode, err error) {
	switch prod.From {
	case "statement":
		node, err = ab.buildStatement(prod)
	case "labeled_statement":
		node, err = ab.buildLabeledStatement(prod)
	case "compund_statement":
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

func (ab *ASTBuilder) OnReduce(prod *grammars.Production) error {
	var err error = nil
	var node ASTNode
	supported := true
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
		switch prod.From {
		case "expression":
			node, err = ab.buildExpression(prod)
		case "assignment_operator":
			node, err = ab.buildAssigmentOperator(prod)
		case "unary_operator":
			node, err = ab.buildUnaryOperator(prod)
		case "type_name":
			node, err = ab.buildTypeName(prod)
		case "argument_expression_list":
			node, err = ab.buildArgumentExpressionList(prod)
		case "translation_unit":
			node, err = ab.buildTranslationUnit(prod)
		case "external_declaration":
			node, err = ab.buildExternalDeclaration(prod)
		case "function_definition":
			node, err = ab.buildFunctionDefinition(prod)
		default:
			supported = false
		}
	}

	if err != nil {
		return err
	}
	if supported { 
		ab.reductionStack.Push(node)
	}
	return nil
}

func (ab *ASTBuilder) OnShift(tok tok.Token) {
	ab.tokenStack.Push(tok)
}

func (ab *ASTBuilder) GetParsedTree() (ASTNode, error) {
	if ab.reductionStack.Size() == 0 {
		return nil, errors.New("Empty AST")
	}
	return ab.reductionStack.Peek(), nil
}

