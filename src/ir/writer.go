package irs

import "fmt"


type Writer struct {
	lines []IRLine
}

func NewWriter() *Writer {
	return &Writer{
		lines: []IRLine{},
	}
}

func (w *Writer) WriteLine(line IRLine) {
	w.lines = append(w.lines, line)
}

func (w *Writer) PrintAll() {
	for _, line := range w.lines {
		fmt.Println(line.String())
	}
}

func (w *Writer) WriteConstantAssignment(lhsSymbol *Symbol, constant string) {
	w.WriteLine(&ConstantAssignmentLine{
		LhsSymbol: lhsSymbol,
		Constant: constant,
	})
}

func (w *Writer) WriteIntAssignment(lhsSymbol *Symbol, val int) {
	w.WriteConstantAssignment(lhsSymbol, fmt.Sprintf("%d", val))
}

func (w *Writer) WirteBinaryOperation(lhsSymbol *Symbol, leftOperand *Symbol, operator string, rightOperand *Symbol) {
	w.WriteLine(&BinaryOperationLine{
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

func (w *Writer) WirteBinarySubtraction(lhsSymbol *Symbol, leftOperand *Symbol, rightOperand *Symbol) {
	w.WirteBinaryOperation(lhsSymbol, leftOperand, "-", rightOperand)
}

func (w *Writer) WriteUnaryOperation(lhsSymbol *Symbol, operator string, operand *Symbol) {
	w.WriteLine(&UnaryOperationLine{
		LhsSymbol: lhsSymbol,
		Operator: operator,
		Operand: operand,
	})
}

func (w *Writer) WriteAddressUnaryOperation(lhsSymbol *Symbol, operand *Symbol) {
	w.WriteUnaryOperation(lhsSymbol, "&", operand)
}

func (w *Writer) WriteDereferenceUnaryOperation(lhsSymbol *Symbol, operand *Symbol) {
	w.WriteUnaryOperation(lhsSymbol, "*", operand)
}

func (w *Writer) WriteBitwiseNegationUnaryOperation(lhsSymbol *Symbol, operand *Symbol) {
	w.WriteUnaryOperation(lhsSymbol, "~", operand)
}

func (w *Writer) WriteLogicalNegationUnaryOperation(lhsSymbol *Symbol, operand *Symbol) {
	w.WriteUnaryOperation(lhsSymbol, "!", operand)
}

func (w *Writer) WriteFunctionCall(functionSymbol *Symbol, returnSymbol *Symbol, args []*Symbol) {
	w.WriteLine(&FunctionCallLine{
		FunctionSymbol: functionSymbol,
		ReturnSymbol: returnSymbol,
		Args: args,
	})
}

func (w *Writer) WriteBiSymbolAssignment(lhs *Symbol, rhs *Symbol) {
	w.WriteLine(&BiSymbolAssignmentLine{
		LhsSymbol: lhs,
		RhsSymbol: rhs,
	})
}

func (w *Writer) WriteLabel(label string) {
	w.WriteLine(&LabelLine{
		Label: label,
	})
}

func (w *Writer) WriteIfgotoLine(condition *Symbol, target string) {
	w.WriteLine(&IfGotoLine{
		TargetLabel: target,
		ConditionSymbol: condition,
	})
}

func (w *Writer) WriteGotoLine(label string) {
	w.WriteLine(&GotoLine{
		TargetLabel: label,
	})
}
