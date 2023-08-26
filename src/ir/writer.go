package irs

import "fmt"


type Writer struct {

}

func NewWriter() *Writer {
	return &Writer{}
}

func (w *Writer) WriteLine(line IRLine) {

}

func (w *Writer) WriteConstantAssignment(lhsSymbol *Symbol, constant string) {
	w.WriteLine(ConstantAssignmentLine{
		LhsSymbol: lhsSymbol,
		Constant: constant,
	})
}

func (w *Writer) WriteIntAssignment(lhsSymbol *Symbol, val int) {
	w.WriteConstantAssignment(lhsSymbol, fmt.Sprintf("%d", val))
}

func (w *Writer) WirteBinaryOperation(lhsSymbol *Symbol, leftOperand *Symbol, operator string, rightOperand *Symbol) {
	w.WriteLine(BinaryOperationLine{
		LhsSymbol: lhsSymbol,
		LeftOperand: leftOperand,
		Operator: operator,
		RightOperand: rightOperand,
	})
}

func (w *Writer) WirteBinaryAddition(lhsSymbol *Symbol, leftOperand *Symbol, rightOperand *Symbol) {
	w.WirteBinaryOperation(lhsSymbol, leftOperand, "+", rightOperand)
}

func (w *Writer) WirteBinaryMultiplication(lhsSymbol *Symbol, leftOperand *Symbol, rightOperand *Symbol) {
	w.WirteBinaryOperation(lhsSymbol, leftOperand, "*", rightOperand)
}

func (w *Writer) WriteUnaryOperation(lhsSymbol *Symbol, operator string, operand *Symbol) {
	w.WriteLine(UnaryOperationLine{
		LhsSymbol: lhsSymbol,
		Operator: operator,
		Operand: operand,
	})
}

func (w *Writer) WriteAddressUnaryOperation(lhsSymbol *Symbol, operand *Symbol) {
	w.WriteUnaryOperation(lhsSymbol, "&", operand)
}
