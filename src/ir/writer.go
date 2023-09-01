package irs

import (
	"fmt"
	"semantics"
)


type Writer struct {
	curFunc *FunctionIR
	functions []*FunctionIR
}

func NewWriter() *Writer {
	return &Writer{
		functions: []*FunctionIR{},
		curFunc: nil,
	}
}

func (w *Writer) GetFunctions() []*FunctionIR {
	return w.functions
}

func (w *Writer) WriteLine(line IRLine) {
	w.curFunc.Code = append(w.curFunc.Code, line)
}

func (w *Writer) PrintAll() {
	for _, fun := range w.functions {
		fmt.Printf("function %s:\n", fun.Name)
		for _, line := range fun.Code {
			if _, isLabel := line.(*LabelLine); isLabel {
				fmt.Println(line.String())
			} else {
				fmt.Printf("\t%s\n", line.String())
			}
		}
	}
}

func (w *Writer) WriteConstantAssignment(lhsSymbol *Symbol, constant semantics.ProgramConstant) {
	w.WriteLine(&ConstantAssignmentLine{
		LhsSymbol: lhsSymbol,
		Constant: constant,
	})
}

func (w *Writer) WriteStringAssignment(lhsSymbol *Symbol, v string) {
	w.WriteLine(&StringAssignmentLine{
		LhsSymbol: lhsSymbol,
		Val: v,
	})
}

func (w *Writer) WriteIntAssignment(lhsSymbol *Symbol, constant semantics.IntegralConstant) {
	w.WriteConstantAssignment(lhsSymbol, constant)
}

func (w *Writer) WriteBinaryOperation(lhsSymbol *Symbol, leftOperand *Symbol, operator string, rightOperand *Symbol) {
	w.WriteLine(&BinaryOperationLine{
		LhsSymbol: lhsSymbol,
		LeftOperand: leftOperand,
		Operator: operator,
		RightOperand: rightOperand,
	})
}

func (w *Writer) WriteBinaryAddition(lhsSymbol *Symbol, leftOperand *Symbol, rightOperand *Symbol) {
	w.WriteBinaryOperation(lhsSymbol, leftOperand, "+", rightOperand)
}

func (w *Writer) WriteBinaryMultiplication(lhsSymbol *Symbol, leftOperand *Symbol, rightOperand *Symbol) {
	w.WriteBinaryOperation(lhsSymbol, leftOperand, "*", rightOperand)
}

func (w *Writer) WriteBinarySubtraction(lhsSymbol *Symbol, leftOperand *Symbol, rightOperand *Symbol) {
	w.WriteBinaryOperation(lhsSymbol, leftOperand, "-", rightOperand)
}

func (w *Writer) WriteEqComparion(lhsSymbol *Symbol, leftOperand *Symbol, rightOperand *Symbol) {
	w.WriteBinaryOperation(lhsSymbol, leftOperand, "==", rightOperand)
}

func (w *Writer) WriteNeqComparion(lhsSymbol *Symbol, leftOperand *Symbol, rightOperand *Symbol) {
	w.WriteBinaryOperation(lhsSymbol, leftOperand, "!=", rightOperand)
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

func (w *Writer) WriteBiSymbolAssignment(lval *LValue, rhs *Symbol) {
	w.WriteLine(&BiSymbolAssignmentLine{
		LValue: lval,
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

func (w *Writer) WriteReturnLine(returnSymbol *Symbol) {
	w.WriteLine(&ReturnLine{
		ReturnSymbol: returnSymbol,
	})
}

func (w *Writer) WriteTypeCastLine(lhs *Symbol, rhs *Symbol) {
	w.WriteLine(&TypeCastLine{
		FromSymbol: rhs,
		ToSymbol: lhs,
	})
}

func (w *Writer) EnterFunction(name string) {
	w.curFunc = &FunctionIR{
		Code: []IRLine{},
		Name: name,
	}
	w.functions = append(w.functions, w.curFunc)
}

func (w *Writer) SaveSnapshot(snapshot *NonGlobalsSnapshot) {
	w.curFunc.Snapshot = snapshot
}
