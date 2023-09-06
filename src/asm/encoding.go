package asm

import "codegen"

type REX struct {
	IsNeeded bool
	W uint8 // size overload
	R uint8 // reg part of modrm uses registers from r8 to r15 
	X uint8 // index of sib uses registers from r8 to r15
	B uint8 // rm part of modrm or base of sib uses registers from r8 to r15
}

func emptyREX() REX {
	return REX{
		IsNeeded: false,
		W: 0,
		R: 0,
		X: 0,
		B: 0,
	}
}

func (r *REX) encode() uint8 {
	return 0b01000000 | (r.W << 3) | (r.R << 2) | (r.X << 1) | r.B
}

func (r *REX) updateForRegExtensionIfNeeded(register codegen.Register) {
	if isExtendedRegister(register) {
		r.IsNeeded = true
		r.R = 1
	}
}

func (r *REX) updateForRmExtensionIfNeeded(register codegen.Register) {
	if isExtendedRegister(register) {
		r.IsNeeded = true
		r.B = 1
	}
}

func (r *REX) updateForSibBaseExtensionIfNeeded(register codegen.Register) {
	if isExtendedRegister(register) {
		r.IsNeeded = true
		r.B = 1
	}
}

func (r *REX) updateForSibIndexExtensionIfNeeded(register codegen.Register) {
	if isExtendedRegister(register) {
		r.IsNeeded = true
		r.X = 1
	}
}

func (r *REX) setSizeOverrideFlag() {
	r.IsNeeded = true
	r.W = 1
}

type ModRM struct {
	mod uint8
	reg uint8
	rm uint8	
}

func (m *ModRM) encode() uint8 {
	return m.rm | (m.reg << 3) | (m.mod << 6)
} 

type SIB struct {
	isNeeded bool
	scale uint8
	index uint8
	base uint8
}

func emptySIB() SIB {
	return SIB{
		isNeeded: false,
	}
}

func (s *SIB) encode() uint8 {
	return s.base | (s.index << 3) | (s.scale << 6) 
} 

// id of r8 maps to rax etc
func getTruncatedRegisterNum(reg codegen.Register) uint8 {
	if intReg, isInt := reg.(codegen.IntegralRegister); isInt {
		switch intReg.Family.T {
		case codegen.RAX, codegen.R8:
			return 0
		case codegen.RCX, codegen.R9:
			return 1
		case codegen.RDX, codegen.R10:
			return 2
		case codegen.RBX, codegen.R11:
			return 3
		case codegen.RSP, codegen.R12:
			return 4
		case codegen.RBP, codegen.R13:
			return 5
		case codegen.RSI, codegen.R14:
			return 6
		case codegen.RDI, codegen.R15:
			return 7
		default:
			panic("Unexpected integral register")
		} 
	} else {
		return uint8(reg.(codegen.FloatingRegister).Family.T) % 8
	}
}

func isExtendedRegister(reg codegen.Register) bool {
	if intReg, isInt := reg.(codegen.IntegralRegister); isInt {
		fam := intReg.Family.T
		switch fam {
		case codegen.R8, codegen.R9, codegen.R10, codegen.R11, codegen.R12, codegen.R13, codegen.R14, codegen.R15:
			return true
		}
		return false
	} else {
		return int(reg.(codegen.FloatingRegister).Family.T) >= 7
	}
}

// left to right ([0, 3] maps to 00001001)
func bitmask(bits ...uint8) uint8 {
	var res uint8 = 0
	for _, b := range bits {
		res += (1 >> b)
	}
	return res
}

func combineWithMasked(accumulator uint8, val uint8, mask uint8) uint8 {
	accumulator |= (val & mask)
	return accumulator
}
