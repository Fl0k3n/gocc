package asm

import (
	"codegen"
	"fmt"
	"irs"
	"strings"
)

type CodeRelocationType int

// we use small code mode as defined in sys V ABI so all addressing can use 32bit offsets
// hence relocation types from ELF spec can be significantly simplified

const (
	PROGRAM_COUNTER_FROM_SECTION CodeRelocationType = iota
	FROM_PLT
	FROM_GOT
)

type DisplacementToFix struct {
	MemoryAccessor codegen.MemoryAccessor
	CodeOffset int
	SizeToFix int
	InstructionSizeAfterDisplacement int
}

type CodeRelocationInfo struct {
	Offset int
	Addend int
	T CodeRelocationType
	Symbol *irs.Symbol	
}

type Relocator struct {
	displacementsToFix []DisplacementToFix
	labels map[string]int
}

func NewRelocator() *Relocator {
	return &Relocator{
		displacementsToFix: []DisplacementToFix{},
		labels: map[string]int{},
	}
}

func (r *Relocator) RecordDisplacementToFix(accessor codegen.MemoryAccessor, offset int, sizeToFix int, instructionSizeAfterDisplacement int) {
	r.displacementsToFix = append(r.displacementsToFix, DisplacementToFix{
		MemoryAccessor: accessor,
		CodeOffset: offset,
		SizeToFix: sizeToFix,
		InstructionSizeAfterDisplacement: instructionSizeAfterDisplacement,
	})
}

func (r *Relocator) GetDisplacementsToFix() []DisplacementToFix {
	return r.displacementsToFix
}

func (r *Relocator) Reset() {
	r.labels = map[string]int{}
	r.displacementsToFix = []DisplacementToFix{}
}

func (r *Relocator) PutLabel(label string, offset int) {
	r.labels[label] = offset
}

func (r *Relocator) fixLabelRipDisplacement(label string, disp *DisplacementToFix, assembly []uint8) {
	rip := disp.CodeOffset + disp.SizeToFix + disp.InstructionSizeAfterDisplacement
	labelPos := r.labels[label]	
	realDisplacement := codegen.Displacement{
		Val: labelPos - rip,
		Size: disp.SizeToFix,
	}.EncodeToLittleEndianU2()

	for i := 0; i < len(realDisplacement); i++ {
		assembly[disp.CodeOffset + i] = realDisplacement[i]
	}
}

func (r *Relocator) PrepareForRelocation(assembly []uint8) (remainingDisplacements []DisplacementToFix){
	for _, displacement := range r.displacementsToFix {
		switch mem := displacement.MemoryAccessor.(type) {
		case codegen.LabeledMemoryAccessor:
			r.fixLabelRipDisplacement(mem.Label, &displacement, assembly)
		default:
			remainingDisplacements = append(remainingDisplacements, displacement)
		}
	}
	r.displacementsToFix = remainingDisplacements
	return
}

func (r *Relocator) PrintDisplacementsToFix(assembly []uint8) {
	for _, df := range r.displacementsToFix {
		bytes := assembly[df.CodeOffset:df.CodeOffset+df.SizeToFix]	
		str := StringifyBytes(bytes)
		padding := 20 - len(str)
		fmt.Printf("%s%s | %s\n", str, strings.Repeat(" ", padding), df.MemoryAccessor.String())
	}
}
