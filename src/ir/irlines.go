package irs

import (
	"fmt"
	"semantics"
	"strings"
)

type IRLine interface {
	String() string
}

type FunctionIR struct {
	Code []IRLine
	FunctionSymbol *GlobalSymbol
	Snapshot *NonGlobalsSnapshot
}

type LValue struct {
	IsDereferenced bool
	Sym *Symbol
}

type ConstantAssignmentLine struct {
	LhsSymbol *Symbol
	Constant semantics.ProgramConstant
}

func (c *ConstantAssignmentLine) String() string {
	return fmt.Sprintf("%s = %s", c.LhsSymbol.Name, c.Constant.String())
}

type StringAssignmentLine struct {
	LhsSymbol *Symbol
	Val string
}

func (c *StringAssignmentLine) String() string {
	return fmt.Sprintf("%s = \"%s\"", c.LhsSymbol.Name, c.Val)
}

type BiSymbolAssignmentLine struct {
	LValue *LValue
	RhsSymbol *Symbol
}

func (c *BiSymbolAssignmentLine) String() string {
	prefix := ""
	if c.LValue.IsDereferenced {
		prefix = "*"
	}
	return fmt.Sprintf("%s%s = %s", prefix, c.LValue.Sym.Name, c.RhsSymbol.Name)
}

type BinaryOperationLine struct {
	LhsSymbol *Symbol
	LeftOperand *Symbol
	Operator string
	RightOperand *Symbol	
}

func (c *BinaryOperationLine) String() string {
	return fmt.Sprintf("%s = %s %s %s", c.LhsSymbol.Name, c.LeftOperand.Name, c.Operator, c.RightOperand.Name)
}

type UnaryOperationLine struct {
	LhsSymbol *Symbol
	Operator string
	Operand *Symbol
}

func (c *UnaryOperationLine) String() string {
	return fmt.Sprintf("%s = %s%s", c.LhsSymbol.Name, c.Operator, c.Operand.Name)
}

type FunctionCallLine struct {
	ReturnSymbol *Symbol
	FunctionSymbol *Symbol
	Args []*Symbol
}

func (c *FunctionCallLine) String() string {
	args := make([]string, 0, len(c.Args))
	for _, arg := range c.Args {
		args = append(args, arg.Name)
	}
	returnString := ""
	if c.ReturnSymbol != nil {
		returnString = " return to " + c.ReturnSymbol.Name
	}
	return fmt.Sprintf("Call %s with (%s)%s", c.FunctionSymbol.Name, strings.Join(args, ", "), returnString)
}

type LabelLine struct {
	Label string
}

func (c *LabelLine) String() string {
	return "." + c.Label + ":"
}

type IfGotoLine struct {
	TargetLabel string
	ConditionSymbol *Symbol
}

func (c *IfGotoLine) String() string {
	return fmt.Sprintf("IFZ %s goto %s", c.ConditionSymbol.Name, c.TargetLabel)
}

type GotoLine struct {
	TargetLabel string
}

func (c *GotoLine) String() string {
	return fmt.Sprintf("goto %s", c.TargetLabel)
}

type ReturnLine struct {
	ReturnSymbol *Symbol
}

func (c *ReturnLine) String() string {
	if c.ReturnSymbol != nil {
		return fmt.Sprintf("return %s", c.ReturnSymbol.Name)
	}
	return "return"
}

type TypeCastLine struct {
	FromSymbol *Symbol
	ToSymbol *Symbol	
}

func (c *TypeCastLine) String() string {
	return fmt.Sprintf("%s = cast(%s) %s", c.ToSymbol.Name, c.ToSymbol.Ctype.HumanReadableName(), c.FromSymbol.Name)
}

type IntermediateRepresentation struct {
	FunctionIr []*FunctionIR
	Globals []*GlobalSymbol
	BootstrappedTypeEngine *semantics.TypeEngine
}
