package irs

import (
	"fmt"
	"strings"
)

type IRLine interface {
	String() string
}

type ConstantAssignmentLine struct {
	LhsSymbol *Symbol
	Constant string // TODO
}

func (c *ConstantAssignmentLine) String() string {
	return fmt.Sprintf("%s = %s", c.LhsSymbol.Name, c.Constant)
}

type StringConstantAssignmentLine struct {
	LhsSymbol *Symbol
	Constant string
}

func (c *StringConstantAssignmentLine) String() string {
	return fmt.Sprintf("%s = \"%s\"", c.LhsSymbol.Name, c.Constant)
}

type BiSymbolAssignmentLine struct {
	LhsSymbol *Symbol
	RhsSymbol *Symbol
}

func (c *BiSymbolAssignmentLine) String() string {
	return fmt.Sprintf("%s = %s", c.LhsSymbol.Name, c.RhsSymbol.Name)
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
	return "." + c.Label
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
