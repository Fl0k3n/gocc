package codegen

import (
	"fmt"
	"irs"
)

type MemoryAccessor interface {
	String() string
}

type StackFrameOffsetMemoryAccessor struct {
	Offset int
}

func (s StackFrameOffsetMemoryAccessor) String() string {
	return fmt.Sprintf("%d[RBP]", s.Offset)
}

type RegisterMemoryAccessor struct {
	Register IntegralRegister
}

func (r RegisterMemoryAccessor) String() string {
	return fmt.Sprintf("%s", r.Register.EffectiveName)
}

type LabeledMemoryAccessor struct {
	Label string
}

func (r LabeledMemoryAccessor) String() string {
	return r.Label
}

type GOTMemoryAccessor struct {
	Symbol *irs.Symbol
}

func (g GOTMemoryAccessor) String() string {
	return fmt.Sprintf("%s@GOT", g.Symbol.Name)
}

type PLTMemoryAccessor struct {
	Symbol *irs.Symbol
}

func (p PLTMemoryAccessor) String() string {
	return fmt.Sprintf("%s@PLT", p.Symbol.Name)
}

type SectionMemoryAccessor struct {
	Symbol *irs.Symbol
}

func (p SectionMemoryAccessor) String() string {
	return fmt.Sprintf("%s@KnownSection", p.Symbol.Name)
}

type RoDataMemoryAccessor struct {
	Offset uint32
}

func (r RoDataMemoryAccessor) String() string {
	return fmt.Sprintf("%d[.rodata]", r.Offset)
}

