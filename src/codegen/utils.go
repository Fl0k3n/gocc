package codegen

import (
	"fmt"
	"irs"
	"semantics"
	"utils"
)

func asSymbols(asyms []*AugmentedSymbol) []*irs.Symbol {
	res := make([]*irs.Symbol, len(asyms))
	for i, asym := range asyms {
		res[i] = asym.Sym
	}
	return res
}

func createFunctionReturnLabel(funName string) string {
	return fmt.Sprintf("0%s_RET", funName)
}

func createFunctionLabel(funName string) string {
	return funName
}

func getIntegralMemoryDescriptor(size int) (memDescriptor string) {
	switch size {
	case QWORD_SIZE: memDescriptor = "QWORD"
	case DWORD_SIZE: memDescriptor = "DWORD"
	case WORD_SIZE: memDescriptor = "WORD"
	case BYTE_SIZE: memDescriptor = "BYTE"
	default: panic("unknown size")
	}
	return
}

func stringifyDisplacement(displacement *Displacement) (sign string, val int) {
	sign = "+"
	val = displacement.Val
	if val < 0 {
		sign = "-"
		val = -1 * val
	}
	return
}

func stringifyMemoryAccessor(
	mem MemoryAccessor,
	usesRip bool,
	usesDisplacement bool,
	displacement *Displacement,
	opSize int,
	originalMemoryAccessor MemoryAccessor,
) string {
	var res string
	descriptor := getIntegralMemoryDescriptor(opSize)
	if usesRip {
		sign, displacement := stringifyDisplacement(displacement)
		ogMemAccessorInfo := ""
		switch ma := originalMemoryAccessor.(type) {
		case GOTMemoryAccessor:
			ogMemAccessorInfo = fmt.Sprintf(" (%s@GOT)", ma.Symbol.Name)
		case PLTMemoryAccessor:
			ogMemAccessorInfo = fmt.Sprintf(" (%s@PLT)", ma.Symbol.Name)
		case LabeledMemoryAccessor:
			ogMemAccessorInfo = fmt.Sprintf(" (%s)", ma.Label)
		case SectionMemoryAccessor:
			ogMemAccessorInfo = fmt.Sprintf(" (%s@Section)", ma.Symbol.Name)
		}
		res = fmt.Sprintf("%s PTR [rip %s %d]%s", descriptor, sign, displacement, ogMemAccessorInfo)
	} else {
		mem := mem.(RegisterMemoryAccessor)
		if usesDisplacement {
			sign, displacement := stringifyDisplacement(displacement)
			res = fmt.Sprintf("%s PTR [%s %s %d]", descriptor, mem.Register.Name(), sign, displacement)
		} else {
			res = descriptor + " PTR [" + mem.Register.Name() + "]"
		}
	}
	return res
}

func abs(v int) int {
	if v < 0 {
		return -v
	}
	return v
}

func encodeNumericProgramConstant(pc semantics.ProgramConstant, typeEngine *semantics.TypeEngine) (encoded []byte) {
	switch c := pc.(type) {
	case semantics.IntegralConstant:
		if typeEngine.IsUnsigned(c) {
			switch c.T.Size() {
			case 1:
				encoded = utils.EncodeUnsignedIntToLittleEndianU2(uint8(c.Val))
			case 2:
				encoded = utils.EncodeUnsignedIntToLittleEndianU2(uint16(c.Val))
			case 4:
				encoded = utils.EncodeUnsignedIntToLittleEndianU2(uint32(c.Val))
			case 8:
				encoded = utils.EncodeUnsignedIntToLittleEndianU2(uint64(c.Val))
			default:
				panic("Unexpected unsigned constant")
			}
		} else {
			switch c.T.Size() {
			case 1:
				encoded = utils.EncodeIntToLittleEndianU2(int8(c.Val))
			case 2:
				encoded = utils.EncodeIntToLittleEndianU2(int16(c.Val))
			case 4:
				encoded = utils.EncodeIntToLittleEndianU2(int32(c.Val))
			case 8:
				encoded = utils.EncodeIntToLittleEndianU2(int64(c.Val))
			default:
				panic("Unexpected signed constant")
			}

		}
	case semantics.FloatingConstant:
		if c.T.Size() == QWORD_SIZE {
			encoded = utils.EncodeDoubleToLittleEndian(c.Val)
		} else {
			encoded = utils.EncodeFloatToLittleEndian(float32(c.Val))
		}
	case semantics.StringConstant:
		panic("Expected numeric constant")
	}
	return
}

func getFloatingTypeSign(size int) string {
	if size == QWORD_SIZE {
		return "d"
	}
	return "s"
}

func getFlotingOpname(baseName string, operands *Operands) string {
	return getFloatingTypeSign(operands.DataTransferSize)
}
