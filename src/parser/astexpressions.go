package parsers

type IdentifierExpression struct {
	Identifier string
}

type ConstantValExpression struct {
	Constant string
}

type StringLiteralExpression struct {
	StringLiteral string
}

type NestedExpression struct {
	Expression *Expression
}

type ArrayAccessPostfixExpression struct {
	PostfixExpression *Expression
	ArrayExpression *Expression
}

type FunctionCallPostfixExpression struct {
	FunctionAccessor *Expression
	Args *ArgumentExpressionList
}

type AccessMethod int
const (
	POINTER_ACCESS AccessMethod = iota
	DOT_ACCESS
)

type StructAccessPostfixExpression struct {
	StructAccessor *Expression
	FieldIdentifier string
	AccessMethod AccessMethod
}

type IncDecOperator int
const (
	INC IncDecOperator = iota
	DEC
)

type IncDecPostfixExpression struct {
	PostfixExpression *Expression
	Operator IncDecOperator
}

type ArgumentExpressionList struct {
	Expressions []*Expression
}

type IncDecUnaryExpression struct {
	UnaryExpression *Expression
	Operator IncDecOperator
}

type CastUnaryExpression struct {
	CastExpression *Expression
	Operator string
}

type SizeofUnaryExpression struct {
	TypeName *TypeName
	NestedUnaryExpression *Expression
}

type TypeCastCastExpression struct {
	Typename *TypeName
	Expression *Expression
}

type BinaryArithmeticExpression struct {
	LhsExpression *Expression
	Operator string
	RhsExpression *Expression
}

type ConditionalExpression struct {
	Condition *Expression
	IfTrueExpression *Expression
	ElseExpression *Expression
}

type AssigmentExpression struct {
	LhsExpression *Expression
	Operator string
	RhsExpression *Expression
}

type CompositeExpression struct {
	Expressions []*Expression
}

type ConstantExpression struct {
	Expression *Expression
}

type Expression interface {}
