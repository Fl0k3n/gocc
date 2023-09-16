package asm

import (
	"codegen"
	"fmt"
	"strings"
)

type X86_64Assembler struct {
	assembledCode []uint8
	individualCodeAsms [][]uint8
	code []codegen.AsmLine
	relocator *Relocator
}

func NewAssembler(relocator *Relocator) *X86_64Assembler {
	return &X86_64Assembler{
		assembledCode: []uint8{},
		code: []codegen.AsmLine{},
		individualCodeAsms: [][]uint8{}, // TODO
		relocator: relocator,
	}
}

func (a *X86_64Assembler) write(bytes ...uint8) {
	a.assembledCode = append(a.assembledCode, bytes...)
}

func (a *X86_64Assembler) assembleMov(m codegen.MovAsmLine) {
	var opcode uint8
	if m.Operands.UsesImmediate {
		if m.Operands.IsFirstOperandRegister() {
			if m.Operands.DataTransferSize == codegen.QWORD_SIZE {
				a.write(a.assembleMIInstruction(0xC7, 0, m.Operands, codegen.DWORD_SIZE)...)
			} else if m.Operands.DataTransferSize == codegen.BYTE_SIZE {
				a.write(a.assembleOIInstruction(0xB0, m.Operands, codegen.DWORD_SIZE)...)
			} else {
				a.write(a.assembleOIInstruction(0xB8, m.Operands, codegen.DWORD_SIZE)...)
			}
		} else {
			if m.Operands.Imm.Size == codegen.BYTE_SIZE {
				opcode = 0xC6
			} else {
				opcode = 0xC7
			}
			a.write(a.assembleMIInstruction(opcode, 0, m.Operands, codegen.DWORD_SIZE)...)
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
		a.write(a.assembleMRInstruction([]uint8{opcode}, m.Operands, NOT_OPCODE, codegen.DWORD_SIZE, false)...)
	}
}

func (a *X86_64Assembler) assembleMovss(m codegen.MovFloatingAsmLine) {
	opcode := []uint8{0x0F, 0}
	if m.Operands.IsFirstOperandMemory() {
		opcode[1] = 0x11
	} else {
		opcode[1] = 0x10
	}	
	a.write(a.assembleMRInstruction(opcode, m.Operands, NOT_OPCODE, m.Operands.DataTransferSize, true)...)
}

func (a *X86_64Assembler) assembleUnconditionalJump(code codegen.JumpAsmLine) {
	if code.Target.UsesRipDisplacement {
		var opcode uint8
		disp := code.Target.Displacement
		if disp.Size == codegen.BYTE_SIZE {
			opcode = 0xEB
		} else {
			opcode = 0xE9
		}
		a.write(opcode)
		a.relocator.RecordDisplacementToFix(code.Target.OriginalMemoryAccessor, len(a.assembledCode), disp.Size, 0)
		a.write(disp.EncodeToLittleEndianU2()...)
	} else {
		a.write(a.assembleMRInstruction([]uint8{0xFF}, code.Target, 4, codegen.QWORD_SIZE, false)...)
	}
}

func (a *X86_64Assembler) assembleConditionalJump(code codegen.ConditionalJumpAsmLine) {
	if !code.Target.UsesRipDisplacement {
		panic("expected rip jcc")
	}
	disp := code.Target.Displacement
	var opcode []uint8
	if disp.Size == codegen.BYTE_SIZE {
		if op, ok := nonNegatedJcc8bLUT[code.Condition]; !ok {
			panic("jcc opcode not known")
		} else {
			opcode = []uint8{op}
		}
	} else {
		if op, ok := lookup[codegen.JumpCondition](nonNegatedJcc32bLUT, code.Condition); !ok {
			panic("jcc opcode not known")
		} else {
			opcode = op
		}
	}
	if code.Negated {
		opcode[len(opcode) - 1] += 1
	}
	a.write(opcode...)
	a.relocator.RecordDisplacementToFix(code.Target.OriginalMemoryAccessor, len(a.assembledCode), disp.Size, 0)
	a.write(disp.EncodeToLittleEndianU2()...)
}

func (a *X86_64Assembler) assembleSetcc(code codegen.SetccAsmLine) {
	opcode, ok := lookup[codegen.JumpCondition](nonNegatedSetccLUT, code.Condition)
	if !ok {
		panic("setcc opcode not known")
	}
	if code.Negated {
		opcode[len(opcode) - 1]++
	}
	a.write(a.assembleMRInstruction(opcode, code.Operands, NOT_OPCODE, codegen.BYTE_SIZE, false)...)
}

func (a *X86_64Assembler) assembleCmp(c codegen.CompareAsmLine) {
	var opcode uint8
	if c.Operands.UsesImmediate {
		if c.Operands.IsFirstOperandRegister() &&
		   c.Operands.FirstOperand.Register.(codegen.IntegralRegister).Family.T == codegen.RAX {
			a.write(a.assembleRaxImmInstruction(0x3D, 0x3D, c.Operands)...)
		} else {
			if c.Operands.DataTransferSize == codegen.BYTE_SIZE {
				a.write(a.assembleMIInstruction(0x80, 7, c.Operands, codegen.DWORD_SIZE)...)
			} else {
				a.write(a.assembleMIInstruction(0x81, 7, c.Operands, codegen.DWORD_SIZE)...)
			}
		}
	} else {
		if c.Operands.IsFirstOperandMemory() || (c.Operands.IsFirstOperandRegister() && c.Operands.IsSecondOperandRegister()) {
			if c.Operands.DataTransferSize == codegen.BYTE_SIZE {
				opcode = 0x38
			} else {
				opcode = 0x39
			}
		} else {
			if c.Operands.DataTransferSize == codegen.BYTE_SIZE {
				opcode = 0x3A
			} else {
				opcode = 0x3B
			}
		}
		a.write(a.assembleMRInstruction([]uint8{opcode}, c.Operands, NOT_OPCODE, codegen.DWORD_SIZE, false)...)
	}
}

func (a *X86_64Assembler) assemblePush(p codegen.PushAsmLine) {
	var opcode uint8
	if p.Operand.UsesImmediate {
		if p.Operand.Imm.Size == codegen.BYTE_SIZE {
			opcode = 0x6A
		} else {
			opcode = 0x68
		}
		a.write(opcode)
		a.write(p.Operand.Imm.EncodeToLittleEndianU2()...)
	} else if p.Operand.IsFirstOperandRegister() {
		reg := p.Operand.FirstOperand.Register
		opcode := 0x50 + getTruncatedRegisterNum(reg)
		if p.Operand.DataTransferSize == codegen.WORD_SIZE {
			a.write(OP_OVERLOAD)
		}
		rex := emptyREX()
		rex.updateForRegExtensionIfNeeded(reg)
		if rex.IsNeeded {
			a.write(rex.encode())
		}
		a.write(opcode)
	} else {
		a.write(a.assembleMRInstruction([]uint8{0xFF}, p.Operand, 6, codegen.QWORD_SIZE, false)...)
	}
}

func (a *X86_64Assembler) assemblePop(p codegen.PopAsmLine) {
	if p.Operand.IsFirstOperandRegister() {
		reg := p.Operand.FirstOperand.Register
		opcode := 0x58 + getTruncatedRegisterNum(reg)
		if p.Operand.DataTransferSize == codegen.WORD_SIZE {
			a.write(OP_OVERLOAD)
		}
		rex := emptyREX()
		rex.updateForRegExtensionIfNeeded(reg)
		if rex.IsNeeded {
			a.write(rex.encode())
		}
		a.write(opcode)
	} else {
		a.write(a.assembleMRInstruction([]uint8{0x0F}, p.Operand, 0, codegen.QWORD_SIZE, false)...)
	}
}

func (a *X86_64Assembler) assembleCall(c codegen.CallAsmLine) {
	if c.Target.UsesRipDisplacement {
		a.write(0xE8)
		a.relocator.RecordDisplacementToFix(c.Target.OriginalMemoryAccessor, len(a.assembledCode), c.Target.Displacement.Size, 0)
		a.write(c.Target.Displacement.EncodeToLittleEndianU2()...)
	} else {
		a.write(a.assembleMRInstruction([]uint8{0xFF}, c.Target, 2, codegen.QWORD_SIZE, false)...)
	}
}

func (a *X86_64Assembler) assembleRet(c codegen.ReturnAsmLine) {
	a.write(0xC3)
}

func (a *X86_64Assembler) assembleAdd(c codegen.AddAsmLine) {
	var opcode uint8
	if c.Operands.UsesImmediate {
		if c.Operands.IsFirstOperandRegister() &&
		   c.Operands.FirstOperand.Register.(codegen.IntegralRegister).Family.T == codegen.RAX {
			a.write(a.assembleRaxImmInstruction(0x04, 0x05, c.Operands)...)
		} else {
			if c.Operands.DataTransferSize == codegen.BYTE_SIZE {
				a.write(a.assembleMIInstruction(0x80, 0, c.Operands, codegen.DWORD_SIZE)...)
			} else {
				a.write(a.assembleMIInstruction(0x81, 0, c.Operands, codegen.DWORD_SIZE)...)
			}
		}
	} else {
		if c.Operands.IsFirstOperandMemory() || (c.Operands.IsFirstOperandRegister() && c.Operands.IsSecondOperandRegister()) {
			if c.Operands.DataTransferSize == codegen.BYTE_SIZE {
				opcode = 0x00
			} else {
				opcode = 0x01
			}
		} else {
			if c.Operands.DataTransferSize == codegen.BYTE_SIZE {
				opcode = 0x02
			} else {
				opcode = 0x03
			}
		}
		a.write(a.assembleMRInstruction([]uint8{opcode}, c.Operands, NOT_OPCODE, codegen.DWORD_SIZE, false)...)
	}	
}

func (a *X86_64Assembler) assembleAdds(c codegen.AddFloatingAsmLine) {
	opcode := []uint8{0x0F, 0x58}
	a.write(a.assembleMRInstruction(opcode, c.Operands, NOT_OPCODE, c.Operands.DataTransferSize, true)...)
}

func (a *X86_64Assembler) assembleSub(c codegen.SubAsmLine) {
	var opcode uint8
	if c.Operands.UsesImmediate {
		if c.Operands.IsFirstOperandRegister() &&
		   c.Operands.FirstOperand.Register.(codegen.IntegralRegister).Family.T == codegen.RAX {
			a.write(a.assembleRaxImmInstruction(0x2C, 0x2D, c.Operands)...)
		} else {
			if c.Operands.DataTransferSize == codegen.BYTE_SIZE {
				a.write(a.assembleMIInstruction(0x80, 5, c.Operands, codegen.DWORD_SIZE)...)
			} else {
				a.write(a.assembleMIInstruction(0x81, 5, c.Operands, codegen.DWORD_SIZE)...)
			}
		}
	} else {
		if c.Operands.IsFirstOperandMemory() || (c.Operands.IsFirstOperandRegister() && c.Operands.IsSecondOperandRegister()) {
			if c.Operands.DataTransferSize == codegen.BYTE_SIZE {
				opcode = 0x28
			} else {
				opcode = 0x29
			}
		} else {
			if c.Operands.DataTransferSize == codegen.BYTE_SIZE {
				opcode = 0x2A
			} else {
				opcode = 0x2B
			}
		}
		a.write(a.assembleMRInstruction([]uint8{opcode}, c.Operands, NOT_OPCODE, codegen.DWORD_SIZE, false)...)
	}	
}

func (a *X86_64Assembler) assembleImul(m codegen.SignedMulAsmLine) {
	// TODO handle immediate
	a.write(a.assembleMRInstruction([]uint8{0x0F, 0xAF}, m.Operands, NOT_OPCODE, codegen.DWORD_SIZE, true)...)
}

func (a *X86_64Assembler) assembleIdiv(d codegen.SignedDivAsmLine) {
	// TODO support for bytes
	var opcode uint8
	if d.Divider.DataTransferSize == codegen.BYTE_SIZE {
		opcode = 0xF6
	} else {
		opcode = 0xF7
	}
	a.write(a.assembleMRInstruction([]uint8{opcode}, d.Divider, 7, codegen.DWORD_SIZE, false)...)
}

func (a *X86_64Assembler) assembleMovzx(m codegen.MovWithZeroExtend) {
	var opcode uint8 = 0xB7
	if m.RightOperandSize == codegen.BYTE_SIZE {
		opcode = 0xB6
	}
	a.write(a.assembleMRInstruction([]uint8{0x0F, opcode}, m.Operands, NOT_OPCODE, codegen.DWORD_SIZE, true)...)
}

func (a *X86_64Assembler) assembleMovsx(m codegen.MovWithSignExtend) {
	var opcode []uint8
	if m.RightOperandSize == codegen.BYTE_SIZE {
		opcode = []uint8{0x0F, 0xBE}
	} else if m.RightOperandSize == codegen.DWORD_SIZE && m.Operands.FirstOperand.Register.Size() == codegen.QWORD_SIZE {
		opcode = []uint8{0x63}
	} else {
		opcode = []uint8{0x0F, 0xBF}
	}
	a.write(a.assembleMRInstruction(opcode, m.Operands, NOT_OPCODE, codegen.DWORD_SIZE, true)...)
}

func (a *X86_64Assembler) assembleNeg(n codegen.NegateAsmLine) {
	var opcode uint8
	if n.Operands.DataTransferSize == codegen.BYTE_SIZE {
		opcode = 0xF6
	} else {
		opcode = 0xF7
	}
	a.write(a.assembleMRInstruction([]uint8{opcode}, n.Operands, 3, codegen.DWORD_SIZE, false)...)
}

func (a *X86_64Assembler) assembleLea(l codegen.LeaAsmLine) {
	a.write(a.assembleMRInstruction([]uint8{0x8D}, l.Operands, NOT_OPCODE, codegen.DWORD_SIZE, true)...)
}

func (a *X86_64Assembler) assembleLine(code codegen.AsmLine) {
	if _, isPlaceholder := code.(codegen.PlaceholderAsmLine); isPlaceholder {
		return
	}
	sizeBefore := len(a.assembledCode)
	a.code = append(a.code, code) // TODO
	switch c := code.(type) {
	case codegen.MovAsmLine:
		a.assembleMov(c)
	case codegen.MovFloatingAsmLine:
		a.assembleMovss(c)
	case codegen.LabelAsmLine:
		a.relocator.PutLabel(c.Label, len(a.assembledCode))
	case codegen.JumpAsmLine:
		a.assembleUnconditionalJump(c)
	case codegen.ConditionalJumpAsmLine:
		a.assembleConditionalJump(c)
	case codegen.SetccAsmLine:
		a.assembleSetcc(c)
	case codegen.CompareAsmLine:
		a.assembleCmp(c)
	case codegen.PushAsmLine:
		a.assemblePush(c)
	case codegen.PopAsmLine:
		a.assemblePop(c)
	case codegen.CallAsmLine:
		a.assembleCall(c)
	case codegen.ReturnAsmLine:
		a.assembleRet(c)		
	case codegen.AddAsmLine:
		a.assembleAdd(c)
	case codegen.AddFloatingAsmLine:
		a.assembleAdds(c)
	case codegen.SubAsmLine:
		a.assembleSub(c)
	case codegen.SignedMulAsmLine:
		a.assembleImul(c)
	case codegen.SignedDivAsmLine:
		a.assembleIdiv(c)
	case codegen.MovWithZeroExtend:
		a.assembleMovzx(c)
	case codegen.MovWithSignExtend:
		a.assembleMovsx(c)
	case codegen.NegateAsmLine:
		a.assembleNeg(c)
	case codegen.LeaAsmLine:
		a.assembleLea(c)
	default:  
		panic("Unsupported")
	}
	a.individualCodeAsms = append(a.individualCodeAsms, a.assembledCode[sizeBefore:])
}

// TODO this is just for testing
func (a *X86_64Assembler) AssembleMultiple(codeLines []codegen.AsmLine) {
	for _, l := range codeLines {
		a.assembleLine(l)
	}
}

func (a *X86_64Assembler) Assemble(functions []*codegen.FunctionCode) ([]*AssembledFunction, []uint8) {
	assembledFuncs := []*AssembledFunction{}
	for _, f := range functions {
		offset := len(a.assembledCode)
		for _, line := range f.Code {
			a.assembleLine(line)
		}
		assembledFuncs = append(assembledFuncs, &AssembledFunction{
			FunctionSymbol: f.Symbol,
			Offset: offset,
			Size: len(a.assembledCode) - offset,
		})
	}
	return assembledFuncs, a.assembledCode
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
		str := assembly.String()
		padding := 60 - len(str)
		if padding < 0 {
			padding = 0
		}
		fmt.Printf("%s%s | %s\n", str, strings.Repeat(" ", padding), StringifyBytes(assembled))
	}
}

func (a *X86_64Assembler) GetAssembledBytes() []uint8 {
	return a.assembledCode	
}
