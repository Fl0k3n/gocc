package semantics

import (
	"errors"
	"fmt"
	"strings"
)


type Ctype interface {
	Name() string
	Size() int
	RequiredAlignment() int
	HumanReadableName() string
}

const ANONYMOUS = ""
const UNSPECIFIED_ARR_SIZE = 0
var UNKNOWN_OR_PARTIAL = Ctype(nil)
var VOID_POINTER = PointerCtype{name: ANONYMOUS, Target: BuiltinFrom("void")}

type TypeDefinition struct {
	Ctype Ctype
	DeclarationLine int
}

type PointerCtype struct {
	Target Ctype
	name string
}

func (p *PointerCtype) WithTargetOnLowestLevel(newTarget Ctype) PointerCtype {
	if nestedPtr, isNested := p.Target.(PointerCtype); isNested {
		p.Target = nestedPtr.WithTargetOnLowestLevel(newTarget)
	} else {
		p.Target = newTarget
	}
	return *p
}

func (p *PointerCtype) GetLowestLevelTarget() Ctype {
	if nestedPtr, isNested := p.Target.(PointerCtype); isNested {
		return nestedPtr.Target
	}
	return p.Target
}

func (p *PointerCtype) AsFunctionPointerAtLowestLevel(funcPtrTarget *FunctionPtrCtype) Ctype {
	p.name = funcPtrTarget.name
	if nestedPtr, isNested := p.Target.(PointerCtype); isNested {
		p.Target = nestedPtr.AsFunctionPointerAtLowestLevel(funcPtrTarget)
		return *p
	}
	return *funcPtrTarget
}

func (pc PointerCtype) Name() string {
	return pc.name
}

func (pc PointerCtype) Size() int {
	return POINTER_SIZE 
}

func (pc PointerCtype) RequiredAlignment() int {
	return POINTER_ALIGNMENT
}

func (pc PointerCtype) HumanReadableName() string {
	return fmt.Sprintf("%s*", pc.Target.HumanReadableName())
}

type BuiltinCtype struct {
	Builtin Builtin
}

func BuiltinFrom(name string) BuiltinCtype {
	return BuiltinCtype{
		Builtin: Builtin(name),
	}
}

func (bc BuiltinCtype) Name() string {
	return string(bc.Builtin)
}

func (bc BuiltinCtype) Size() int {
	return sizeof[bc.Builtin]
}

func (bc BuiltinCtype) RequiredAlignment() int {
	return alignment[bc.Builtin]
}

func (bc BuiltinCtype) HumanReadableName() string {
	return bc.Name()
}

type StructCtype struct {
	name string
	size int
	NestedFieldTypes []Ctype
	NestedFieldNames []string
	AlignedFieldOffsets []int
}

func NewStruct(name string, fieldTypes []Ctype, fieldNames []string) StructCtype {
	size := 0
	offsets := make([]int, len(fieldTypes))
	for i, f := range fieldTypes {
		prevVarPadding := 0
		if remainder := size % f.RequiredAlignment(); remainder != 0 {
			prevVarPadding = f.RequiredAlignment() - remainder
		}
		offsets[i] = size + prevVarPadding
		size += f.Size() + prevVarPadding
	}

	if len(fieldTypes) > 0 {
		// padding for proper array access
		if remainder := size % fieldTypes[0].RequiredAlignment(); remainder != 0 {
			size += fieldTypes[0].RequiredAlignment() - remainder
		}
	}

	return StructCtype{
		name: name,
		size: size,
		NestedFieldTypes: fieldTypes,
		NestedFieldNames: fieldNames,
		AlignedFieldOffsets: offsets,
	}
} 

func (cc StructCtype) Name() string {
	return cc.name
}

func (cc StructCtype) Size() int {
	return cc.size
}

func (cc StructCtype) RequiredAlignment() int {
	return cc.NestedFieldTypes[0].RequiredAlignment()
}

func (cc StructCtype) HumanReadableName() string {
	return fmt.Sprintf("struct %s", cc.name)
}

func (cc *StructCtype) MaybeField(name string) (t Ctype, offset int, err error) {
	for idx, fname := range cc.NestedFieldNames {
		if fname == name {
			return cc.NestedFieldTypes[idx], cc.AlignedFieldOffsets[idx], nil
		}
	}
	return nil, 0, errors.New("No field named " + name)
}

func (cc *StructCtype) Field(name string) (t Ctype, offset int) {
	for idx, fname := range cc.NestedFieldNames {
		if fname == name {
			return cc.NestedFieldTypes[idx], cc.AlignedFieldOffsets[idx]
		}
	}
	panic("Struct Ctype " + cc.name + " doesn't contain field named " + name)
}

type ArrayCtype struct {
	name string
	DimensionSizes []int // left to right
	NestedType Ctype
	size int
}

func (ac ArrayCtype) Name() string {
	return ac.name
}

func (ac ArrayCtype) Size() int {
	if len(ac.DimensionSizes) > 0 && ac.DimensionSizes[0] == UNSPECIFIED_ARR_SIZE {
		return POINTER_SIZE
	}
	return ac.size
}

func (ac ArrayCtype) RequiredAlignment() int {
	return POINTER_ALIGNMENT
}

func (ac ArrayCtype) HumanReadableName() string {
	dimsStr := ""
	for _, dim := range ac.DimensionSizes {
		if dim == UNSPECIFIED_ARR_SIZE {
			dimsStr += "[]"
		} else {
			dimsStr += fmt.Sprintf("[%d]", dim)
		}
	}
	return fmt.Sprintf("%s%s", ac.NestedType.HumanReadableName(), dimsStr)
}

func NewArray(name string, dimensions []int, nestedType Ctype) ArrayCtype {
	size := -1
	if nestedType != UNKNOWN_OR_PARTIAL {
		size = nestedType.Size()
		for _, dim := range dimensions {
			// TODO check overflow/bounds
			size *= dim
		}
	}
	return ArrayCtype{
		name: name,
		DimensionSizes: dimensions,
		NestedType: nestedType,
		size: size,
	}
}

func (ac ArrayCtype) WithRecomputedSize() ArrayCtype {
	return NewArray(ac.name, ac.DimensionSizes, ac.NestedType)
}

type FunctionPtrCtype struct {
	name string
	ReturnType Ctype
	ParamTypes []Ctype
	ParamNames []string
}

func (fp FunctionPtrCtype) Name() string {
	return fp.name
}

func (fp FunctionPtrCtype) Size() int {
	return POINTER_SIZE 
}

func (fp FunctionPtrCtype) RequiredAlignment() int {
	return POINTER_ALIGNMENT
}

func (fp FunctionPtrCtype) HumanReadableName() string {
	argsRepr := make([]string, 0, len(fp.ParamTypes))
	for _, param := range fp.ParamTypes {
		argsRepr = append(argsRepr, param.HumanReadableName())
	}
	return fmt.Sprintf("%s (*)%s", fp.ReturnType.HumanReadableName(), strings.Join(argsRepr, ","))
}
