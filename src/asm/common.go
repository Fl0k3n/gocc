package asm

import "codegen"

const NOT_OPCODE uint8 = 0


func (a *X86_64Assembler) getSizeOverridePrefixes(ops *codegen.Operands, rex *REX, defaultOperationSize int) []uint8 {
	const ADDR_OVERLOAD uint8 = 0x67
	const OP_OVERLOAD uint8 = 0x66
	res := []uint8{}

	if defaultOperationSize == codegen.DWORD_SIZE && ops.DataTransferSize == codegen.WORD_SIZE {
		res = append(res, OP_OVERLOAD)
	}
	if ops.DataTransferSize == codegen.QWORD_SIZE && defaultOperationSize != codegen.QWORD_SIZE {
		rex.setSizeOverrideFlag()
	}
	if ops.IsFirstOperandMemory() || (ops.SecondOperand != nil && ops.IsSecondOperandMemory()) {
		var memop codegen.MemoryAccessor
		if ops.IsFirstOperandMemory() {
			memop = ops.FirstOperand.Memory
		} else {
			memop = ops.SecondOperand.Memory
		}
		if memop.(codegen.RegisterMemoryAccessor).Register.Size() != codegen.QWORD_SIZE {
			res = append(res, ADDR_OVERLOAD)
		}
	}
	return res
}

// assumes instruction that doesn't have explicit SIB
func (a *X86_64Assembler) needsImplicitSIB(ops *codegen.Operands) bool {
	var memoryOperand codegen.MemoryAccessor
	if ops.IsFirstOperandMemory() {
		memoryOperand = ops.FirstOperand.Memory
	} else {
		memoryOperand = ops.SecondOperand.Memory
	}
	return getTruncatedRegisterNum(memoryOperand.(codegen.RegisterMemoryAccessor).Register) == 4 
}

func (a *X86_64Assembler) getSIB(ops *codegen.Operands, sibOperand codegen.MemoryAccessor, rex *REX) SIB {
	sib := SIB{isNeeded: true}

	if ops.UsesExplicitSib {
		// TODO 
	} else {
		// implicit sib, use just the base
		reg := sibOperand.(codegen.RegisterMemoryAccessor).Register
		sib.index = 0b100
		sib.scale = 0
		rex.updateForSibBaseExtensionIfNeeded(reg)
		sib.base = getTruncatedRegisterNum(reg)
	}
	return sib
}

func (a *X86_64Assembler) assembleOIInstruction(opcode uint8, ops *codegen.Operands, im *codegen.Immediate, defaultOperationSize int) []uint8 {
	rex := emptyREX()
	reg := ops.FirstOperand.Register
	rex.updateForRmExtensionIfNeeded(reg)
	opcode += getTruncatedRegisterNum(reg)
	res := a.getSizeOverridePrefixes(ops, &rex, defaultOperationSize)
	if rex.IsNeeded {
		res = append(res, rex.encode())
	}
	res = append(res, opcode)
	res = append(res, im.EncodeToLittleEndianU2()...)
	return res
}

func (a *X86_64Assembler) assembleMIInstruction(opcode uint8, modRmOpcode uint8, ops *codegen.Operands, im *codegen.Immediate, defaultOperationSize int) []uint8 {
	mrAsm := a.assembleMRInstruction([]uint8{opcode}, ops, modRmOpcode, defaultOperationSize)
	mrAsm = append(mrAsm, im.EncodeToLittleEndianU2()...)
	return mrAsm
}

// assumes MR encoding so if both operands are registers first is obtained using rm bits second using reg bits
func (a *X86_64Assembler) assembleMRInstruction(opcode []uint8, ops *codegen.Operands, modRmOpcode uint8, defaultOperationSize int) []uint8 {
	modrm := ModRM{}
	rex := emptyREX()
	sib := emptySIB()

	if ops.SecondOperand == nil {
		modrm.reg = modRmOpcode
	} 	
	// TODO RIP displacement
	if ops.IsFirstOperandMemory() || (ops.SecondOperand != nil && ops.IsSecondOperandMemory()) {
		if ops.Uses8bDisplacement {
			modrm.mod = 0b01
		} else if ops.Uses32bDisplacement {
			modrm.mod = 0b10
		} else {
			modrm.mod = 0b00
		}
		var memoryOperand codegen.MemoryAccessor
		if ops.IsFirstOperandMemory() {
			if ops.SecondOperand != nil {
				modrm.reg = getTruncatedRegisterNum(ops.SecondOperand.Register)
				rex.updateForRegExtensionIfNeeded(ops.SecondOperand.Register)
			}
			memoryOperand = ops.FirstOperand.Memory
		} else {
			modrm.reg = getTruncatedRegisterNum(ops.FirstOperand.Register)
			rex.updateForRegExtensionIfNeeded(ops.FirstOperand.Register)
			memoryOperand = ops.SecondOperand.Memory
		}
		if ops.UsesExplicitSib {
			modrm.rm = 0b100
			// TODO handle SIB
		} else if a.needsImplicitSIB(ops) {
			modrm.rm = 0b100
			sib = a.getSIB(ops, memoryOperand, &rex)
		} else {
			memReg := memoryOperand.(codegen.RegisterMemoryAccessor).Register
			modrm.rm = getTruncatedRegisterNum(memReg)
			rex.updateForRmExtensionIfNeeded(memReg)
		}
	} else {
		modrm.mod = 0b11
		if ops.SecondOperand != nil {
			modrm.reg = getTruncatedRegisterNum(ops.SecondOperand.Register)
			rex.updateForRegExtensionIfNeeded(ops.SecondOperand.Register)
		}
		modrm.rm = getTruncatedRegisterNum(ops.FirstOperand.Register)
		rex.updateForRmExtensionIfNeeded(ops.FirstOperand.Register)
	}
	
	res := a.getSizeOverridePrefixes(ops, &rex, defaultOperationSize)
	if rex.IsNeeded {
		res = append(res, rex.encode())
	}
	res = append(res, opcode...)
	res = append(res, modrm.encode())
	if sib.isNeeded {
		res = append(res, sib.encode())
	}
	return res
}

func (a *X86_64Assembler) assembleImmediateForSize(imm *codegen.Immediate, size int) []uint8 {
	return []uint8{}
}
