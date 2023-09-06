package asm

import (
	"codegen"
	"fmt"
)

type X86_64Assembler struct {
	assembledCode []uint8
	individualCodeAsms [][]uint8
	code []codegen.AsmLine
}

func NewAssembler() *X86_64Assembler {
	return &X86_64Assembler{
		assembledCode: []uint8{},
		code: []codegen.AsmLine{},
		individualCodeAsms: [][]uint8{}, // TODO
	}
}

func (a *X86_64Assembler) write(bytes ...uint8) {
	a.assembledCode = append(a.assembledCode, bytes...)
}

func (a *X86_64Assembler) assembleMov(m codegen.MovAsmLine) {
	var opcode uint8
	if m.UsesImmediate {
		if m.Operands.IsFirstOperandRegister() {
			if m.Operands.DataTransferSize == codegen.QWORD_SIZE {
				a.write(a.assembleMIInstruction(0xC7, 0, m.Operands, m.Imm, codegen.DWORD_SIZE)...)
			} else if m.Operands.DataTransferSize == codegen.BYTE_SIZE {
				a.write(a.assembleOIInstruction(0xB0, m.Operands, m.Imm, codegen.DWORD_SIZE,)...)
			} else {
				a.write(a.assembleOIInstruction(0xB8, m.Operands, m.Imm, codegen.DWORD_SIZE,)...)
			}
		} else {
			if m.Imm.Size == codegen.BYTE_SIZE {
				opcode = 0xC6
			} else {
				opcode = 0xC7
			}
			a.write(a.assembleMIInstruction(opcode, 0, m.Operands, m.Imm, codegen.DWORD_SIZE)...)
		}
	} else {
		if m.Operands.IsFirstOperandMemory() || (m.Operands.IsFirstOperandRegister() && m.Operands.IsSecondOperandRegister()) {
			if m.Operands.DataTransferSize == codegen.BYTE_SIZE {
				opcode = 0x88
			} else {
				opcode = 0x89
			}
		} else {
			if m.Operands.DataTransferSize == codegen.BYTE_SIZE {
				opcode = 0x8A
			} else {
				opcode = 0x8B
			}
		}
		a.write(a.assembleMRInstruction([]uint8{opcode}, m.Operands, NOT_OPCODE, codegen.DWORD_SIZE)...)
	}
}

func (a *X86_64Assembler) Assemble(code codegen.AsmLine) {
	sizeBefore := len(a.assembledCode)
	a.code = append(a.code, code) // TODO
	switch c := code.(type) {
	case codegen.MovAsmLine:
		a.assembleMov(c)
	case codegen.PlaceholderAsmLine:
		fmt.Println("Skipping placeholder")
	default:
		panic("Unsupported")
	}
	a.individualCodeAsms = append(a.individualCodeAsms, a.assembledCode[sizeBefore:])
}

// TODO this is just for testing
func (a *X86_64Assembler) AssembleMultiple(codeLines []codegen.AsmLine) {
	for _, l := range codeLines {
		a.Assemble(l)
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

func (a *X86_64Assembler) PrintAssemblyAlongAssembledBytes() {
	for i := range a.code {
		assembly := a.code[i]
		assembled := a.individualCodeAsms[i]
		fmt.Printf("%s  |  %s\n", assembly.String(), StringifyBytes(assembled))
	}
}