package codegen

import "fmt"

// Intel ASM syntax

type FunctionCode struct {
	Name string
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

func (w *X86_64Writer) EnterFunction(Name string) {
	w.curFunc = &FunctionCode{
		Name: Name,
		Code: []AsmLine{},
	}
	w.functions = append(w.functions, w.curFunc)
}

func (w *X86_64Writer) PrintAll() {
	for _, fun := range w.functions {
		fmt.Println("")
		fmt.Println("function " + fun.Name + ":")
		fmt.Println("")
		for _, line := range fun.Code {
			fmt.Println(line)
		}
	}
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
	w.writePlaceholder(fmt.Sprintf("push %s", reg.EffectiveName))
}

func (w *X86_64Writer) PushFloatingReg(reg FloatingRegister) {
	w.writePlaceholder(fmt.Sprintf("PUSH FLOAT TODO %s", reg.Name()))
}

func (w *X86_64Writer) PopIntegralReg(reg IntegralRegister) {
	w.writePlaceholder(fmt.Sprintf("pop %s", reg.EffectiveName))
}

func (w *X86_64Writer) PopFloatingReg(reg FloatingRegister) {
	w.writePlaceholder(fmt.Sprintf("POP FLOAT TODO %s", reg.Name()))
}

func (w *X86_64Writer) MovIntegralRegisterToIntegralRegister(dest IntegralRegister, src IntegralRegister) {
	w.writeLine(MovAsmLine{
		Operands: emptyOperands().WithFirstOperand(justRegister(dest)).
					WithSecondOperand(justRegister(src)).WithSizeFromRegister(),
		UsesImmediate: false,
	})
}

func (w *X86_64Writer) MovIntegralConstantToIntegralRegister(dest IntegralRegister, val int) {
	l := MovAsmLine{
		Operands: emptyOperands().WithFirstOperand(justRegister(dest)).WithSizeFromRegister(),
		UsesImmediate: true,
		Imm: &Immediate{
			Val: int64(val),
		},
	}
	immSize := l.Operands.DataTransferSize
	if immSize == QWORD_SIZE {
		immSize = 4
	}
	l.Imm.Size = immSize
	w.writeLine(l)
}

func (w *X86_64Writer) MovMemoryToIntegralRegister(dest IntegralRegister, mem MemoryAccessor) {
	w.writeLine(MovAsmLine{
		Operands: emptyOperands().WithFirstOperand(justRegister(dest)).
			WithSecondOperand(justMemory(mem)).WithSizeFromRegister(),
		UsesImmediate: false,
	})
}

func (w *X86_64Writer) MovIntegralRegisterToMemory(dest MemoryAccessor, src IntegralRegister) {
	w.writeLine(MovAsmLine{
		Operands: emptyOperands().WithFirstOperand(justMemory(dest)).
			WithSecondOperand(justRegister(src)).WithSizeFromRegister(),
		UsesImmediate: false,
	})
}

func (w *X86_64Writer) MovIntegralConstantToMemory(dest MemoryAccessor, size int, val int) {
	immSize := size 
	if size == QWORD_SIZE {
		immSize = 4
	}
	w.writeLine(MovAsmLine{
		Operands: emptyOperands().WithFirstOperand(justMemory(dest)).WithExplicitSize(size),
		UsesImmediate: true,
		Imm: &Immediate{Val: int64(val), Size: immSize},
	})
}

func (w *X86_64Writer) SubtractConstantInteger(src IntegralRegister, val int) {
	w.writePlaceholder(fmt.Sprintf("sub %s, %d", src.EffectiveName, val))
}

func (w *X86_64Writer) AddConstantInteger(src IntegralRegister, val int) {
	w.writePlaceholder(fmt.Sprintf("add %s, %d", src.EffectiveName, val))
}

func (w *X86_64Writer) PutLabel(label string) {
	w.writePlaceholder(label + ":")
}

func (w *X86_64Writer) JumpToLabel(label string) {
	w.writePlaceholder("jmp " + label)
}

func (w *X86_64Writer) CompareToZero(reg Register) {
	w.writePlaceholder(fmt.Sprintf("cmp %s, 0", reg.Name()))
}

func (w *X86_64Writer) JumpIfZero(label string) {
	w.writePlaceholder("jz " + label)
}

func (w *X86_64Writer) PushIntegralRegister(src IntegralRegister) {
	w.writePlaceholder(fmt.Sprintf("push %s", src.Name()))
}

func (w *X86_64Writer) Call(mem MemoryAccessor) {
	w.writePlaceholder(fmt.Sprintf("call %s", mem.String()))
}

func (w *X86_64Writer) Return() {
	w.writePlaceholder("ret")
}
