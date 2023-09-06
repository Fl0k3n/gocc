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
	return fmt.Sprintf("GOT@%s", g.Symbol.Name)
}

type PLTMemoryAccessor struct {
	Symbol *irs.Symbol
}

func (p PLTMemoryAccessor) String() string {
	return fmt.Sprintf("PLT@%s", p.Symbol.Name)
}

type SectionMemoryAccessor struct {
	Symbol *irs.Symbol
}

func (p SectionMemoryAccessor) String() string {
	return fmt.Sprintf("KnownSection@%s", p.Symbol.Name)
}