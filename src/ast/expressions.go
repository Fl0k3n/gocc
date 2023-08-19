package ast

type IdentifierExpression struct {
	Identifier string
	LineInfo
}

type ConstantValExpression struct {
	Constant string
	LineInfo
}

type StringLiteralExpression struct {
	StringLiteral string
	LineInfo
}

type ArrayAccessPostfixExpression struct {
	PostfixExpression Expression
	ArrayExpression Expression
	LineInfo
}

type FunctionCallPostfixExpression struct {
	FunctionAccessor Expression
	Args *ArgumentExpressionList
	LineInfo
}

type AccessMethod int
const (
	POINTER_ACCESS AccessMethod = iota
	DOT_ACCESS
)

type StructAccessPostfixExpression struct {
	StructAccessor Expression
	FieldIdentifier string
	AccessMethod AccessMethod
	LineInfo
}

type IncDecOperator int
const (
	INC IncDecOperator = iota
	DEC
)

type IncDecPostfixExpression struct {
	PostfixExpression Expression
	Operator IncDecOperator
	LineInfo
}

type ArgumentExpressionList struct {
	Expressions []Expression
	LineInfo
}

type IncDecUnaryExpression struct {
	UnaryExpression Expression
	Operator IncDecOperator
	LineInfo
}

type CastUnaryExpression struct {
	CastExpression Expression
	Operator string
	LineInfo
}

type SizeofUnaryExpression struct {
	TypeName *TypeName
	NestedUnaryExpression Expression
	LineInfo
}

type TypeCastCastExpression struct {
	Typename *TypeName
	Expression Expression
	LineInfo
}

type BinaryArithmeticExpression struct {
	LhsExpression Expression
	Operator string
	RhsExpression Expression
	LineInfo
}

type ConditionalExpression struct {
	Condition Expression
	IfTrueExpression Expression
	ElseExpression Expression
	LineInfo
}

type AssigmentExpression struct {
	LhsExpression Expression
	Operator string
	RhsExpression Expression
	LineInfo
}

type CompositeExpression struct {
	Expressions []Expression
	LineInfo
}

type ConstantExpression struct {
	Expression Expression
	LineInfo
}

type Expression interface {}
