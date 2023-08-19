package types


type Ctype interface {
	Name() string
	Size() int
	RequiredAlignment() int
}

const ANONYMOUS = ""
var UNKNOWN_OR_PARTIAL = Ctype(nil)

type TypeDefinition struct {
	Ctype Ctype
	DeclarationLine int
}

type PointerCtype struct {
	Target Ctype
	name string
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

func (cc *StructCtype) Field(name string) (t Ctype, offset int) {
	for idx, fname := range cc.NestedFieldNames {
		if fname == name {
			return cc.NestedFieldTypes[idx], cc.AlignedFieldOffsets[idx]
		}
	}
	panic("Ctype " + cc.name + " doesn't contain field named " + name)
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
	return ac.size
}

func (ac ArrayCtype) RequiredAlignment() int {
	return POINTER_ALIGNMENT
}

func NewArray(name string, dimensions []int, nestedType Ctype) ArrayCtype {
	size := nestedType.Size()
	for _, dim := range dimensions {
		// TODO check overflow/bounds
		size *= dim
	}
	return ArrayCtype{
		name: name,
		DimensionSizes: dimensions,
		NestedType: nestedType,
		size: size,
	}
}

type FunctionPtrCtype struct {
	name string
	ReturnType Ctype
	ParamTypes []Ctype
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
