package asm

import (
	"codegen"
	"irs"
)

const NOT_OPCODE uint8 = 0
const ADDR_OVERLOAD uint8 = 0x67
const OP_OVERLOAD uint8 = 0x66
const FLOAT_OVERLOAD uint8 = 0xF3
const DOUBLE_OVERLOAD uint8 = 0xF2

const OPCODE_SECOND_BYTE_PFX = 0x0F

type AssembledFunction struct {
	FunctionSymbol *irs.GlobalSymbol
	Size int
	Offset int
}

func (a *X86_64Assembler) getSizeOverridePrefixes(ops *codegen.Operands, rex *REX, defaultOperationSize int) []uint8 {
	res := []uint8{}
	if defaultOperationSize == codegen.DWORD_SIZE && ops.DataTransferSize == codegen.WORD_SIZE {
		res = append(res, OP_OVERLOAD)
	}
	if ops.DataTransferSize == codegen.QWORD_SIZE && defaultOperationSize != codegen.QWORD_SIZE {
		rex.setSizeOverrideFlag()
	}
	if !ops.UsesRipDisplacement && (ops.IsFirstOperandMemory() || (ops.SecondOperand != nil && ops.IsSecondOperandMemory())) {
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
	if ops.IsSSE {
		if ops.DataTransferSize == codegen.DWORD_SIZE {
			res = append(res, FLOAT_OVERLOAD)
	 	} else {
			res = append(res, DOUBLE_OVERLOAD)
		}
	}
	return res
}

// assumes instruction doesn't have explicit SIB
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

// assumes that default operation size is 32b
func (a *X86_64Assembler) assembleRaxImmInstruction(byteOpcode uint8, notByteOpcode uint8, ops *codegen.Operands) []uint8 {
	res := []uint8{}
	switch ops.DataTransferSize {
	case codegen.BYTE_SIZE:
		res = append(res, byteOpcode)
	case codegen.WORD_SIZE:
		res = append(res, OP_OVERLOAD, notByteOpcode)
	case codegen.DWORD_SIZE:
		res = append(res, notByteOpcode)
	case codegen.QWORD_SIZE:
		rex := emptyREX()
		rex.W = 1
		res = append(res, rex.encode(), notByteOpcode)
	}
	res = append(res, ops.Imm.EncodeToLittleEndianU2()...)
	return res
}

func (a *X86_64Assembler) assembleOIInstruction(opcode uint8, ops *codegen.Operands, defaultOperationSize int) []uint8 {
	rex := emptyREX()
	reg := ops.FirstOperand.Register
	rex.updateForRmExtensionIfNeeded(reg)
	opcode += getTruncatedRegisterNum(reg)
	res := a.getSizeOverridePrefixes(ops, &rex, defaultOperationSize)
	if rex.IsNeeded {
		res = append(res, rex.encode())
	}
	res = append(res, opcode)
	res = append(res, ops.Imm.EncodeToLittleEndianU2()...)
	return res
}

func (a *X86_64Assembler) assembleMIInstruction(opcode uint8, modRmOpcode uint8, ops *codegen.Operands, defaultOperationSize int) []uint8 {
	mrAsm := a.assembleMRInstruction([]uint8{opcode}, ops, modRmOpcode, defaultOperationSize, false)
	mrAsm = append(mrAsm, ops.Imm.EncodeToLittleEndianU2()...)
	return mrAsm
}

func (a *X86_64Assembler) assembleModrmRexAndSib(
	ops *codegen.Operands,
	modRmOpcode uint8,
	defaultOperationSize int,
	useRMencoding bool,
) (ModRM, REX, SIB) {
	modrm := ModRM{}
	rex := emptyREX()
	sib := emptySIB()

	if ops.SecondOperand == nil {
		modrm.reg = modRmOpcode
	} 	
	if ops.IsFirstOperandMemory() || (ops.SecondOperand != nil && ops.IsSecondOperandMemory()) {
		if ops.UsesRipDisplacement {
			modrm.mod = 0b00
			modrm.rm = 0b101
			var reg codegen.Register = nil
			if ops.FirstOperand != nil && ops.IsFirstOperandRegister() {
				reg = ops.FirstOperand.Register
			} else if ops.SecondOperand != nil && ops.IsSecondOperandRegister() {
				reg = ops.SecondOperand.Register
			}
			if reg != nil {
				modrm.reg = getTruncatedRegisterNum(reg)
				rex.updateForRegExtensionIfNeeded(reg)
			}
		} else {
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
		}
	} else {
		modrm.mod = 0b11
		if useRMencoding {
			if ops.SecondOperand != nil {
				modrm.rm = getTruncatedRegisterNum(ops.SecondOperand.Register)
				rex.updateForRmExtensionIfNeeded(ops.SecondOperand.Register)
			}
			modrm.reg = getTruncatedRegisterNum(ops.FirstOperand.Register)
			rex.updateForRegExtensionIfNeeded(ops.FirstOperand.Register)
		} else {
			if ops.SecondOperand != nil {
				modrm.reg = getTruncatedRegisterNum(ops.SecondOperand.Register)
				rex.updateForRegExtensionIfNeeded(ops.SecondOperand.Register)
			}
			modrm.rm = getTruncatedRegisterNum(ops.FirstOperand.Register)
			rex.updateForRmExtensionIfNeeded(ops.FirstOperand.Register)
		}
	}
	return modrm, rex, sib
}

func (a *X86_64Assembler) assembleMRInstruction(opcode []uint8, ops *codegen.Operands, modRmOpcode uint8, defaultOperationSize int, useRMencoding bool) []uint8 {
	modrm, rex, sib := a.assembleModrmRexAndSib(ops, modRmOpcode, defaultOperationSize, useRMencoding)
	res := a.getSizeOverridePrefixes(ops, &rex, defaultOperationSize)
	if rex.IsNeeded {
		res = append(res, rex.encode())
	}
	res = append(res, opcode...)
	res = append(res, modrm.encode())
	if sib.isNeeded {
		res = append(res, sib.encode())
	}
	if ops.UsesRipDisplacement {
		remainingInstructionSize := 0
		if ops.UsesImmediate {
			remainingInstructionSize = ops.Imm.Size
		}
		a.relocator.RecordDisplacementToFix(ops.OriginalMemoryAccessor,
			len(a.assembledCode) + len(res), ops.Displacement.Size, remainingInstructionSize)
	}
	if ops.Uses32bDisplacement || ops.Uses8bDisplacement {
		res = append(res, ops.Displacement.EncodeToLittleEndianU2()...)
	}
	return res
}
