package codegen

import (
	"fmt"
	"math"
)

const UNKNOWN_DISPLACEMNT_SIZE = 32

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

func unknownDisplacement() *Displacement {
	return &Displacement{
		Val: 0,
		Size: DWORD_SIZE,
	} 
}

func getDisplacementSize(val int) int {
	if val < math.MaxInt8 && val > math.MinInt8 {
		return BYTE_SIZE
	}
	return DWORD_SIZE
}

func setSimplifiedMemoryAccessor(ops *Operands, mem MemoryAccessor, firstOp bool) {
	var simplifiedAccessor MemoryAccessor

	switch accessor := mem.(type) {
	case RegisterMemoryAccessor:
		simplifiedAccessor = accessor
	case StackFrameOffsetMemoryAccessor:
		simplifiedAccessor = RegisterMemoryAccessor{
			Register: GetIntegralRegisterFamily(RBP).UseForSize(QWORD_SIZE),
		}
		displacementSize := getDisplacementSize(accessor.Offset)
		if displacementSize == BYTE_SIZE {
			ops.Uses8bDisplacement = true
		} else {
			ops.Uses32bDisplacement = true
		}
		ops.Displacement = &Displacement{
			Val: accessor.Offset,
			Size: displacementSize,
		}
	case LabeledMemoryAccessor, SectionMemoryAccessor, GOTMemoryAccessor, PLTMemoryAccessor:
		simplifiedAccessor = nil
		ops.UsesRipDisplacement = true
		ops.SetUnknownDisplacement()
	default:
		panic("Unexpected memory accessor")
	}

	if firstOp {
		ops.FirstOperand = justMemory(simplifiedAccessor)
	} else {
		ops.SecondOperand = justMemory(simplifiedAccessor) 
	}
	ops.OriginalMemoryAccessor = mem
}

type SIB struct {

}

type Displacement struct {
	Val int
	Size int
}

func (dis Displacement) EncodeToLittleEndianU2() []uint8 {
	res := make([]uint8, dis.Size)
	var mask int = 0xff
	shift := 0
	for i := 0; i < dis.Size; i++ {
		res[i] = uint8((dis.Val & mask) >> shift)
		shift += 8
		mask <<= 8
	}
	return res
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
	OriginalMemoryAccessor MemoryAccessor
}

func (o *Operands) ToAssembly() string {
	res := ""
	if o.IsFirstOperandRegister() {
		reg := o.FirstOperand.Register
		res += reg.Name()
	} else {
		res += stringifyMemoryAccessor(o.FirstOperand.Memory, o.UsesRipDisplacement,
				 o.Uses32bDisplacement || o.Uses8bDisplacement, o.Displacement, o.DataTransferSize)
	}
	if o.SecondOperand != nil {
		res += ", "
		if o.IsSecondOperandRegister() {
			res += o.SecondOperand.Register.Name()
		} else {
			res += stringifyMemoryAccessor(o.SecondOperand.Memory, o.UsesRipDisplacement,
				o.Uses32bDisplacement || o.Uses8bDisplacement, o.Displacement, o.DataTransferSize)
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
		OriginalMemoryAccessor: nil,
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

func (m *Operands) SetUnknownDisplacement() {
	m.Displacement = unknownDisplacement()
	m.Uses32bDisplacement = true
}

func (m *Operands) WithPossiblyComplexMemoryFirstOperand(mem MemoryAccessor) *Operands {
	setSimplifiedMemoryAccessor(m, mem, true)
	return m 	
}

func (m *Operands) WithPossiblyComplexMemorySecondOperand(mem MemoryAccessor) *Operands {
	setSimplifiedMemoryAccessor(m, mem, false)
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
	return m.FirstOperand.Memory != nil || (m.FirstOperand.Register == nil && m.UsesRipDisplacement)
}

func (m *Operands) IsSecondOperandMemory() bool {
	return m.SecondOperand.Memory != nil ||
		  (m.FirstOperand.Memory == nil && m.SecondOperand.Register == nil && m.UsesRipDisplacement)
}

func (m *Operands) IsFirstOperandRegister() bool {
	return m.FirstOperand.Register != nil
}

func (m *Operands) IsSecondOperandRegister() bool {
	return m.SecondOperand.Register != nil
}


type LabelAsmLine struct {
	Label string
}

func (l LabelAsmLine) String() string {
	return fmt.Sprintf(".%s:", l.Label)
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

type JumpAsmLine struct {
	Target *Operands
}

func (l JumpAsmLine) String() string {
	return fmt.Sprintf("jmp %s", l.Target.ToAssembly())
}

type JumpCondition int 

const (
	EQUAL JumpCondition = iota
	LESS_THAN
	GREATER_THAN
	LESS_OR_EQUAL
	GREATER_OR_EQUAL
)

var JUMP_CONDITION_STRING_ENCODING = map[JumpCondition]string {
	EQUAL: "E",
	LESS_THAN: "L",
	GREATER_THAN: "G",
	LESS_OR_EQUAL: "LE",
	GREATER_OR_EQUAL: "GE",
}

type ConditionalJumpAsmLine struct {
	Target *Operands
	Condition JumpCondition
	Negated bool
}

func (l ConditionalJumpAsmLine) String() string {
	negationPfx := ""
	if l.Negated {
		negationPfx = "N"
	}
	return fmt.Sprintf("j%s%s %s", negationPfx, JUMP_CONDITION_STRING_ENCODING[l.Condition], l.Target.ToAssembly())
}

var RELATIONAL_OPERATOR_TO_CONDITION = map[string]JumpCondition {
	"==": EQUAL,
	"<": LESS_THAN,
	">": GREATER_THAN,
	"<=": LESS_OR_EQUAL,
	">=": GREATER_OR_EQUAL,
}

type SetccAsmLine struct {
	Operands *Operands
	Condition JumpCondition // TODO refactor this
	Negated bool
}

func (l SetccAsmLine) String() string {
	negationPfx := ""
	if l.Negated {
		negationPfx = "N"
	}
	return fmt.Sprintf("set%s%s %s", negationPfx, JUMP_CONDITION_STRING_ENCODING[l.Condition], l.Operands.ToAssembly())
}

type CompareAsmLine struct {
	Operands *Operands
	UsesImmediate bool
	Imm *Immediate
}

func (l CompareAsmLine) String() string {
	ops := l.Operands.ToAssembly()
	if l.UsesImmediate {
		ops += fmt.Sprintf(", %d", l.Imm.Val)
	}
	return fmt.Sprintf("cmp " + ops)
}

type PushAsmLine struct {
	Operand *Operands
	UsesImmediate bool
	Imm *Immediate
}

func (l PushAsmLine) String() string {
	if l.UsesImmediate {
		return fmt.Sprintf("push %s %d", getIntegralMemoryDescriptor(l.Imm.Size), l.Imm.Val)
	} else {
		return fmt.Sprintf("push %s", l.Operand.ToAssembly())
	}
}

type PopAsmLine struct {
	Operand *Operands
}

func (l PopAsmLine) String() string {
	return fmt.Sprintf("pop %s", l.Operand.ToAssembly())
}

type CallAsmLine struct {
	Target *Operands
}

func (l CallAsmLine) String() string {
	return fmt.Sprintf("call %s", l.Target.ToAssembly())
}

type ReturnAsmLine struct {}

func (l ReturnAsmLine) String() string {
	return "ret"
}

type AddAsmLine struct {
	Operands *Operands
	UsesImmediate bool
	Imm *Immediate
}

func (l AddAsmLine) String() string {
	ops := l.Operands.ToAssembly()
	if l.UsesImmediate {
		ops += fmt.Sprintf(", %d", l.Imm.Val)
	}
	return fmt.Sprintf("add " + ops)
}

type SubAsmLine struct {
	Operands *Operands
	UsesImmediate bool
	Imm *Immediate
}

func (l SubAsmLine) String() string {
	ops := l.Operands.ToAssembly()
	if l.UsesImmediate {
		ops += fmt.Sprintf(", %d", l.Imm.Val)
	}
	return fmt.Sprintf("sub " + ops)
}

type SignedMulAsmLine struct {
	Operands *Operands
	// todo third immediate operand
}

func (l SignedMulAsmLine) String() string {
	return fmt.Sprintf("imul %s",l.Operands.ToAssembly())
}

type SignedDivAsmLine struct {
	Divider *Operands
}

func (l SignedDivAsmLine) String() string {
	return fmt.Sprintf("idiv %s",l.Divider.ToAssembly())
}
