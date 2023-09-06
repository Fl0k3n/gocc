package codegen

import (
	"fmt"
)

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
	Val int64
	Size int
}

func (imm Immediate) EncodeToLittleEndianU2() []uint8 {
	res := make([]uint8, imm.Size)
	var mask int64 = 0xff
	shift := 0
	for i := 0; i < imm.Size; i++ {
		res[i] = uint8((imm.Val & mask) >> shift)
		shift += 8
		mask <<= 8
	}
	return res
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
	DataTransferSize int
}

func (o *Operands) ToAssembly() string {
	// TODO
	res := ""
	if o.IsFirstOperandRegister() {
		reg := o.FirstOperand.Register
		res += reg.Name()
	} else {
		mem := o.FirstOperand.Memory.(RegisterMemoryAccessor)
		res += getIntegralMemoryDescriptor(o.DataTransferSize) + " PTR [" + mem.Register.Name() + "]"
	}
	if o.SecondOperand != nil {
		res += ", "
		if o.IsSecondOperandRegister() {
			res += o.SecondOperand.Register.Name()
		} else {
			mem := o.SecondOperand.Memory.(RegisterMemoryAccessor)
			res += getIntegralMemoryDescriptor(o.DataTransferSize) + " PTR [" + mem.Register.Name() + "]"
		}
	}
	return res
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
		DataTransferSize: -1,
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

func (m *Operands) WithSizeFromRegister() *Operands {
	if m.FirstOperand.Register != nil {
		m.DataTransferSize = m.FirstOperand.Register.Size()
	} else if m.SecondOperand != nil && m.SecondOperand.Register != nil {
		m.DataTransferSize = m.SecondOperand.Register.Size()
	} else {
		panic("no register")
	}
	return m
}

func (m *Operands) WithExplicitSize(size int) *Operands {
	m.DataTransferSize = size
	return m
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
	Operands *Operands
	UsesImmediate bool
	Imm *Immediate
}

func (l MovAsmLine) String() string {
	ops := l.Operands.ToAssembly()
	if l.UsesImmediate {
		ops += fmt.Sprintf(", %d", l.Imm.Val)
	}
	return fmt.Sprintf("mov " + ops)
}
