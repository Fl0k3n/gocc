package codegen

import "fmt"

type AsmLine interface {
	String() string
}

type PlaceholderAsmLine struct {
	line string
}

func (p PlaceholderAsmLine) String() string {
	return p.line
}

type RegisterOrMemoryOperand struct {
	Register Register
	Memory MemoryAccessor
}

func justRegister(reg Register) *RegisterOrMemoryOperand {
	return &RegisterOrMemoryOperand{
		Register: reg,
		Memory: nil,
	}
}

func justMemory(mem MemoryAccessor) *RegisterOrMemoryOperand {
	return &RegisterOrMemoryOperand{
		Register: nil,
		Memory: mem,
	}
}

type SIB struct {

}

type Displacement struct {

}

type Immediate struct {

}

type Operands struct {
	FirstOperand *RegisterOrMemoryOperand
	SecondOperand *RegisterOrMemoryOperand
	UsesExplicitSib bool
	SIB *SIB
	Uses8bDisplacement bool
	Uses32bDisplacement bool
	UsesRipDisplacement bool
	Displacement *Displacement
}

func emptyOperands() *Operands {
	return &Operands{
		FirstOperand: nil,
		SecondOperand: nil,
		UsesExplicitSib: false,	
		SIB: nil,
		Uses8bDisplacement: false,
		Uses32bDisplacement: false,
		UsesRipDisplacement: false,
		Displacement: nil,
	}
}

func (m *Operands) WithFirstOperand(op *RegisterOrMemoryOperand) *Operands {
	m.FirstOperand = op
	return m
}

func (m *Operands) WithSecondOperand(op *RegisterOrMemoryOperand) *Operands {
	m.SecondOperand = op
	return m
}

func (m *Operands) GetSizeFromRegister() int {
	if m.FirstOperand.Register != nil {
		return m.FirstOperand.Register.Size()
	}
	if m.SecondOperand != nil && m.SecondOperand.Register != nil {
		return m.SecondOperand.Register.Size()
	}
	panic("Requested register size with unset registers")
}

func (m *Operands) WithSib(SIB *SIB) *Operands {
	m.SIB = SIB
	return m
}

func (m *Operands) IsFirstOperandMemory() bool {
	return m.FirstOperand.Memory != nil
}

func (m *Operands) IsSecondOperandMemory() bool {
	return m.SecondOperand.Memory != nil
}

func (m *Operands) IsFirstOperandRegister() bool {
	return m.FirstOperand.Register != nil
}

func (m *Operands) IsSecondOperandRegister() bool {
	return m.SecondOperand.Register != nil
}

type PushAsmLine struct {
	Register IntegralRegister
}

type MovAsmLine struct {
	UsesModRM bool 
	Operands *Operands
	OperandSize int
	UsesImmediate bool
	Imm Immediate
}

func (l MovAsmLine) String() string {
	return fmt.Sprintf("mov TODO")
}
