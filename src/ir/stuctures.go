package irs

type IRLine interface{}

type ConstantAssignmentLine struct {
	LhsSymbol *Symbol
	Constant string // TODO
}

type StringConstantAssignmentLine struct {
	LhsSymbol *Symbol
	Constant string
}

type BinaryOperationLine struct {
	LhsSymbol *Symbol
	LeftOperand *Symbol
	Operator string
	RightOperand *Symbol	
}

type UnaryOperationLine struct {
	LhsSymbol *Symbol
	Operator string
	Operand *Symbol
}
