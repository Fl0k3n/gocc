package codegen

import "fmt"

// Intel ASM syntax

type FunctionCode struct {
	Name string
	Code []string
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
		Code: []string{},
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

func (w *X86_64Writer) writeLine(line string) {
	w.curFunc.Code = append(w.curFunc.Code, line)
}

func (w *X86_64Writer) PushIntegralReg(reg IntegralRegister) {
	w.writeLine(fmt.Sprintf("push %s", reg.EffectiveName))
}

func (w *X86_64Writer) PushFloatingReg(reg FloatingRegister) {
	w.writeLine(fmt.Sprintf("PUSH FLOAT TODO %s", reg.Name()))
}

func (w *X86_64Writer) MovIntegralRegisterToIntegralRegister(dest IntegralRegister, src IntegralRegister) {
	w.writeLine(fmt.Sprintf("mov %s, %s", dest.EffectiveName, src.EffectiveName))
}

func (w *X86_64Writer) MovIntegralConstantToIntegralRegister(dest IntegralRegister, val int) {
	w.writeLine(fmt.Sprintf("mov %s, #%d", dest.EffectiveName, val))
}

func (w *X86_64Writer) getIntegralMemoryDescriptor(size int) (memDescriptor string) {
	switch size {
	case QWORD_SIZE: memDescriptor = "QWORD"
	case DWORD_SIZE: memDescriptor = "DWORD"
	case WORD_SIZE: memDescriptor = "WORD"
	case BYTE_SIZE: memDescriptor = "BYTE"
	default: panic("unknown size")
	}
	return
}

func (w *X86_64Writer) MovMemoryToIntegralRegister(dest IntegralRegister, mem MemoryAccessor) {
	memDescriptor := w.getIntegralMemoryDescriptor(dest.EffectiveSize)
	w.writeLine(fmt.Sprintf("mov %s, %s PTR %s", dest.EffectiveName, memDescriptor, mem.String()))
}

func (w *X86_64Writer) MovIntegralRegisterToMemory(dest MemoryAccessor, src IntegralRegister) {
	memDescriptor := w.getIntegralMemoryDescriptor(src.EffectiveSize)
	w.writeLine(fmt.Sprintf("mov %s PTR %s, %s", memDescriptor, dest.String(), src.EffectiveName))
}

func (w *X86_64Writer) MovIntegralConstantToMemory(dest MemoryAccessor, val int) {
	w.writeLine(fmt.Sprintf("mov %s, #%d", dest.String(), val))
}

func (w *X86_64Writer) SubtractConstantInteger(src IntegralRegister, val int) {
	w.writeLine(fmt.Sprintf("sub %s, %d", src.EffectiveName, val))
}

func (w *X86_64Writer) AddConstantInteger(src IntegralRegister, val int) {
	w.writeLine(fmt.Sprintf("add %s, %d", src.EffectiveName, val))
}

func (w *X86_64Writer) PutLabel(label string) {
	w.writeLine(label + ":")
}

func (w *X86_64Writer) JumpToLabel(label string) {
	w.writeLine("jmp " + label)
}

func (w *X86_64Writer) CompareToZero(reg Register) {
	w.writeLine(fmt.Sprintf("cmp %s, 0", reg.Name()))
}

func (w *X86_64Writer) JumpIfZero(label string) {
	w.writeLine("jz " + label)
}

func (w *X86_64Writer) PushIntegralRegister(src IntegralRegister) {
	w.writeLine(fmt.Sprintf("push %s", src.Name()))
}

func (w *X86_64Writer) Call(mem MemoryAccessor) {
	w.writeLine(fmt.Sprintf("call %s", mem.String()))
}
