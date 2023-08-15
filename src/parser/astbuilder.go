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

