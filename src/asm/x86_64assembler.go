package asm

import (
	"codegen"
	"fmt"
)

type X86_64Assembler struct {
	assembledCode []uint8
}

func NewAssembler() *X86_64Assembler {
	return &X86_64Assembler{
		assembledCode: []uint8{},
	}
}

func (a *X86_64Assembler) write(bytes ...uint8) {
	a.assembledCode = append(a.assembledCode, bytes...)
}

func (a *X86_64Assembler) assembleMov(m codegen.MovAsmLine) {
	if !m.UsesModRM {
		panic("only modrm supported for now")
	}

	var opcode uint8
	immediate := []uint8{}

	if m.UsesImmediate {
		
	} else {
		if m.Operands.IsFirstOperandMemory() || (m.Operands.IsFirstOperandRegister() && m.Operands.IsSecondOperandRegister()) {
			if m.OperandSize == codegen.BYTE_SIZE {
				opcode = 0x88
			} else {
				opcode = 0x89
			}
		} else {
			if m.OperandSize == codegen.BYTE_SIZE {
				opcode = 0x8A
			} else {
				opcode = 0x8B
			}
		}
	}
	a.write(a.assembleInstruction([]uint8{opcode}, m.Operands, NOT_OPCODE, codegen.DWORD_SIZE, m.OperandSize)...)
	a.write(immediate...)
}

func (a *X86_64Assembler) Assemble(code codegen.AsmLine) {
	switch c := code.(type) {
	case codegen.MovAsmLine:
		a.assembleMov(c)
	case codegen.PlaceholderAsmLine:
		fmt.Println("Skipping placeholder")
	default:
		panic("Unsupported")
	}
}

func (a *X86_64Assembler) PrintBytes() {
	const LINE_WIDTH = 30
	for i, b := range a.assembledCode {
		if b < 10 {
			fmt.Printf("0%x ", b)
		} else {
			fmt.Printf("%x ", b)
		}
		if (i + 1) % LINE_WIDTH == 0 {
			fmt.Println()
		}
	}
}
