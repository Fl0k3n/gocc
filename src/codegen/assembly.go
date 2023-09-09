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

func (imm Immediate) String() string {
	return fmt.Sprintf("%d", imm.Val)
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
	UsesImmediate bool
	Imm *Immediate
	DataTransferSize int
	OriginalMemoryAccessor MemoryAccessor
}

func (o *Operands) ToAssembly() string {
	res := ""
	if o.IsFirstOperandRegister() {
		res = o.FirstOperand.Register.Name()
	} else if o.IsFirstOperandMemory() {
		res = stringifyMemoryAccessor(o.FirstOperand.Memory, o.UsesRipDisplacement,
				 o.Uses32bDisplacement || o.Uses8bDisplacement, o.Displacement, o.DataTransferSize)
	} else if o.UsesImmediate {
		res = o.Imm.String()
	}
	if o.SecondOperand != nil {
		res += ", "
		if o.IsSecondOperandRegister() {
			res += o.SecondOperand.Register.Name()
		} else if o.IsSecondOperandMemory() {
			res += stringifyMemoryAccessor(o.SecondOperand.Memory, o.UsesRipDisplacement,
				o.Uses32bDisplacement || o.Uses8bDisplacement, o.Displacement, o.DataTransferSize)
		} else {
			res += o.Imm.String()
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
		UsesImmediate: false,
		Imm: nil,
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

func (m *Operands) WithImmediate(imm *Immediate) *Operands {
	m.Imm = imm
	m.UsesImmediate = true
	return m
}

func (m *Operands) WithAutoSizedImmediate(imm *Immediate) *Operands {
	if m.DataTransferSize == QWORD_SIZE {
		imm.Size = DWORD_SIZE
	} else {
		imm.Size = m.DataTransferSize
	}
	return m.WithImmediate(imm)	
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
}

func (l MovAsmLine) String() string {
	return fmt.Sprintf("mov " + l.Operands.ToAssembly())
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
}

func (l CompareAsmLine) String() string {
	return fmt.Sprintf("cmp " + l.Operands.ToAssembly())
}

type PushAsmLine struct {
	Operand *Operands
}

func (l PushAsmLine) String() string {
	if l.Operand.UsesImmediate {
		return fmt.Sprintf("push %s %d", getIntegralMemoryDescriptor(l.Operand.Imm.Size), l.Operand.Imm.Val)
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
}

func (l AddAsmLine) String() string {
	return fmt.Sprintf("add " + l.Operands.ToAssembly())
}

type SubAsmLine struct {
	Operands *Operands
}

func (l SubAsmLine) String() string {
	return fmt.Sprintf("sub " + l.Operands.ToAssembly())
}

type SignedMulAsmLine struct {
	Operands *Operands
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

type MovWithZeroExtend struct {
	Operands *Operands
	RightOperandSize int
}

func (l MovWithZeroExtend) String() string {
	return fmt.Sprintf("movzx %s", l.Operands.ToAssembly())
}

type MovWithSignExtend struct {
	Operands *Operands
	RightOperandSize int
}

func (l MovWithSignExtend) String() string {
	mnem := "movsx"
	if l.Operands.FirstOperand.Register.Size() == QWORD_SIZE && l.RightOperandSize == DWORD_SIZE {
		mnem = "movsxd"
	}
	return fmt.Sprintf("%s %s", mnem, l.Operands.ToAssembly())
}

type NegateAsmLine struct {
	Operands *Operands
}

func (l NegateAsmLine) String() string {
	return fmt.Sprintf("neg %s", l.Operands.ToAssembly())
}

type LeaAsmLine struct {
	Operands *Operands
}

func (l LeaAsmLine) String() string {
	return fmt.Sprintf("lea %s", l.Operands.ToAssembly())
}
