package codegen

import (
	"fmt"
	"irs"
)

// Intel ASM syntax

type FunctionCode struct {
	Symbol *irs.GlobalSymbol
	Code []AsmLine
}

type X86_64Writer struct {
	curFunc *FunctionCode
	functions []*FunctionCode
}

func NewWriter() *X86_64Writer {
	return &X86_64Writer{
		functions: []*FunctionCode{},
	}
}

func (w *X86_64Writer) EnterFunction(funSymbol *irs.GlobalSymbol) {
	w.curFunc = &FunctionCode{
		Symbol: funSymbol,
		Code: []AsmLine{},
	}
	w.functions = append(w.functions, w.curFunc)
}

func (w *X86_64Writer) GetAssembly() []*FunctionCode {
	return w.functions
}

func (w *X86_64Writer) writePlaceholder(line string) {
	w.curFunc.Code = append(w.curFunc.Code, PlaceholderAsmLine{line})
}

func (w *X86_64Writer) writeLine(asmLine AsmLine) {
	w.curFunc.Code = append(w.curFunc.Code, asmLine)
}

func (w *X86_64Writer) PushIntegralReg(reg IntegralRegister) {
	if reg.EffectiveSize == DWORD_SIZE {
		panic("can't push dword")
	}
	w.writeLine(PushAsmLine{
		Operand: emptyOperands().WithFirstOperand(justRegister(reg)).WithSizeFromRegister(),
	})
}

func (w *X86_64Writer) PushFloatingReg(reg FloatingRegister) {
	w.writePlaceholder(fmt.Sprintf("PUSH FLOAT TODO %s", reg.Name()))
}

func (w *X86_64Writer) PopIntegralReg(reg IntegralRegister) {
	if reg.EffectiveSize == DWORD_SIZE {
		panic("can't pop dword")
	}
	w.writeLine(PopAsmLine{
		Operand: emptyOperands().WithFirstOperand(justRegister(reg)).WithSizeFromRegister(),
	})
}

func (w *X86_64Writer) PopFloatingReg(reg FloatingRegister) {
	w.writePlaceholder(fmt.Sprintf("POP FLOAT TODO %s", reg.Name()))
}

func (w *X86_64Writer) MovIntegralRegisterToIntegralRegister(dest IntegralRegister, src IntegralRegister) {
	w.writeLine(MovAsmLine{
		Operands: emptyOperands().WithFirstOperand(justRegister(dest)).
					WithSecondOperand(justRegister(src)).WithSizeFromRegister(),
	})
}

func (w *X86_64Writer) MovIntegralConstantToIntegralRegister(dest IntegralRegister, val int) {
	w.writeLine(MovAsmLine{
		Operands: emptyOperands().WithFirstOperand(justRegister(dest)).WithSizeFromRegister().
			WithAutoSizedImmediate(&Immediate{Val: int64(val)}),
	})
}

func (w *X86_64Writer) MovMemoryToIntegralRegister(dest IntegralRegister, mem MemoryAccessor) {
	w.writeLine(MovAsmLine{
		Operands: emptyOperands().WithFirstOperand(justRegister(dest)).
			WithPossiblyComplexMemorySecondOperand(mem).WithSizeFromRegister(),
	})
}

func (w *X86_64Writer) MovIntegralRegisterToMemory(dest MemoryAccessor, src IntegralRegister) {
	w.writeLine(MovAsmLine{
		Operands: emptyOperands().WithPossiblyComplexMemoryFirstOperand(dest).
			WithSecondOperand(justRegister(src)).WithSizeFromRegister(),
	})
}

func (w *X86_64Writer) MovIntegralConstantToMemory(dest MemoryAccessor, size int, val int) {
	w.writeLine(MovAsmLine{
		Operands: emptyOperands().WithPossiblyComplexMemoryFirstOperand(dest).WithExplicitSize(size).
			WithAutoSizedImmediate(&Immediate{Val: int64(val)}),
	})
}

func (w *X86_64Writer) AddIntegralRegisters(left IntegralRegister, right IntegralRegister) {
	w.writeLine(AddAsmLine{
		Operands: emptyOperands().WithFirstOperand(justRegister(left)).
			WithSecondOperand(justRegister(right)).WithSizeFromRegister(),
	})
}

func (w *X86_64Writer) SubIntegralRegisters(left IntegralRegister, right IntegralRegister) {
	w.writeLine(SubAsmLine{
		Operands: emptyOperands().WithFirstOperand(justRegister(left)).
			WithSecondOperand(justRegister(right)).WithSizeFromRegister(),
	})
}

func (w *X86_64Writer) SignedMultiplyIntegralRegisters(left IntegralRegister, right IntegralRegister) {
	w.writeLine(SignedMulAsmLine{
		Operands: emptyOperands().WithFirstOperand(justRegister(left)). 
			WithSecondOperand(justRegister(right)).WithSizeFromRegister(),
	})	
}

func (w *X86_64Writer) SignedDivideRaxRdxByIntegralRegister(divider IntegralRegister) {
	w.writeLine(SignedDivAsmLine{
		Divider: emptyOperands().WithFirstOperand(justRegister(divider)).WithSizeFromRegister(),
	})
}

func (w *X86_64Writer) ClearIntegralRegister(reg IntegralRegister) {
	// TODO use xor reg, reg
	w.writeLine(MovAsmLine{
		Operands: emptyOperands().WithFirstOperand(justRegister(reg)).
			WithImmediate(&Immediate{Val: 0, Size: reg.Size()}).WithSizeFromRegister(),
	})
}

func (w *X86_64Writer) SubtractConstantInteger(src IntegralRegister, val int) {
	w.writeLine(SubAsmLine{
		Operands: emptyOperands().WithFirstOperand(justRegister(src)).WithSizeFromRegister(). 
			WithAutoSizedImmediate(&Immediate{Val: int64(val)}),
	})}

func (w *X86_64Writer) AddConstantInteger(src IntegralRegister, val int) {
	w.writeLine(AddAsmLine{
		Operands: emptyOperands().WithFirstOperand(justRegister(src)).WithSizeFromRegister(). 
			WithAutoSizedImmediate(&Immediate{Val: int64(val)}),
	})
}

func (w *X86_64Writer) PutLabel(label string) {
	w.writeLine(LabelAsmLine{
		Label: label,
	})
}

func (w *X86_64Writer) JumpToLabel(label string) {
	w.writeLine(JumpAsmLine{
		Target: emptyOperands().
			WithPossiblyComplexMemoryFirstOperand(LabeledMemoryAccessor{Label: label}).
			WithExplicitSize(QWORD_SIZE),
	})
}

func (w *X86_64Writer) CompareToZero(reg Register) {
	w.writeLine(CompareAsmLine{
		Operands: emptyOperands().WithFirstOperand(justRegister(reg)).WithSizeFromRegister().
			WithAutoSizedImmediate(&Immediate{Val: int64(0)}),
	})
}

func (w *X86_64Writer) CompareIntegralRegisters(left IntegralRegister, right IntegralRegister) {
	w.writeLine(CompareAsmLine{
		Operands: emptyOperands().WithFirstOperand(justRegister(left)).
			WithSecondOperand(justRegister(right)).WithSizeFromRegister(),
	})
}

func (w *X86_64Writer) SetComparisonResult(reg IntegralRegister, condition JumpCondition, negated bool) {
	w.writeLine(SetccAsmLine{
		Operands: emptyOperands().WithFirstOperand(justRegister(reg.Family.UseForSize(BYTE_SIZE))). 
			WithExplicitSize(BYTE_SIZE),
		Condition: condition,
		Negated: negated,
	})
	if reg.Size() != BYTE_SIZE {
		w.ZeroExtend(reg, reg.Family.UseForSize(BYTE_SIZE))
	}
}

func (w *X86_64Writer) JumpIfZero(label string) {
	w.writeLine(ConditionalJumpAsmLine{
		Target: emptyOperands().
			WithPossiblyComplexMemoryFirstOperand(LabeledMemoryAccessor{Label: label}).WithExplicitSize(QWORD_SIZE),
		Condition: EQUAL,
		Negated: false,
	})
}

func (w *X86_64Writer) JumpIfNotZero(label string) {
	w.writeLine(ConditionalJumpAsmLine{
		Target: emptyOperands().
			WithPossiblyComplexMemoryFirstOperand(LabeledMemoryAccessor{Label: label}).WithExplicitSize(QWORD_SIZE),
		Condition: EQUAL,
		Negated: true,
	})
}

func (w *X86_64Writer) ZeroExtend(dest IntegralRegister, src IntegralRegister) {
	if dest.Size() == QWORD_SIZE {
		dest = dest.Family.UseForSize(DWORD_SIZE)
	}
	w.writeLine(MovWithZeroExtend{
		Operands: emptyOperands().WithFirstOperand(justRegister(dest)).
			WithSecondOperand(justRegister(src)).WithExplicitSize(dest.Size()),
		RightOperandSize: src.Size(),
	})
}

func (w *X86_64Writer) SignExtend(dest IntegralRegister, src IntegralRegister) {
	w.writeLine(MovWithSignExtend{
		Operands: emptyOperands().WithFirstOperand(justRegister(dest)).
			WithSecondOperand(justRegister(src)).WithExplicitSize(dest.Size()),
		RightOperandSize: src.Size(),
	})
}

func (w *X86_64Writer) NegateIntegralRegister(reg IntegralRegister) {
	w.writeLine(NegateAsmLine{
		Operands: emptyOperands().WithFirstOperand(justRegister(reg)).WithSizeFromRegister(),
	})
}

func (w *X86_64Writer) Call(mem MemoryAccessor) {
	w.writeLine(CallAsmLine{
		Target: emptyOperands().WithPossiblyComplexMemoryFirstOperand(mem).WithExplicitSize(QWORD_SIZE),
	})
}

func (w *X86_64Writer) Return() {
	w.writeLine(ReturnAsmLine{})
}

func (w *X86_64Writer) Reference(destReg IntegralRegister, mem MemoryAccessor) {
	w.writeLine(LeaAsmLine{
		Operands: emptyOperands().WithFirstOperand(justRegister(destReg)).
			WithPossiblyComplexMemorySecondOperand(mem).WithSizeFromRegister(),
	})
}

