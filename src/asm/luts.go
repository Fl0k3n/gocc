package asm

import "codegen"

// TODO fill this

var nonNegatedJcc32bLUT = map[codegen.JumpCondition][]uint8 {
	codegen.EQUAL: {0x0F, 0x84},
	codegen.LESS_THAN: {0x0F, 0x8C},
	codegen.LESS_OR_EQUAL: {0x0F, 0x8E},
	codegen.GREATER_THAN: {0x0F, 0x8F},
	codegen.GREATER_OR_EQUAL: {0x0F, 0x8D},
}
var nonNegatedJcc8bLUT = map[codegen.JumpCondition]uint8 {
	codegen.EQUAL: 0x74,
}
var nonNegatedSetccLUT = map[codegen.JumpCondition][]uint8 {
	codegen.EQUAL: {0x0F, 0x94},
	codegen.LESS_THAN: {0x0F, 0x9C},
	codegen.LESS_OR_EQUAL: {0x0F, 0x9E},
	codegen.GREATER_THAN: {0x0F, 0x9F},
	codegen.GREATER_OR_EQUAL: {0x0F, 0x9D},
}
