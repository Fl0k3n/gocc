package codegen

import (
	"irs"
	"semantics"
	"utils"
)

type offset = uint32

const SIZEOF_FLOAT = DWORD_SIZE
const SIZEOF_DOUBLE = QWORD_SIZE

const STRING_ALIGNMENT = 1
const FLOAT_ALIGNMENT = 4
const DOUBLE_ALIGNMENT = 8

type Rodata struct {
	Data []uint8
	Alignment uint32
}

type ReadonlyDataManager struct {
	rodataSize uint32
	totalSize uint32
	strings map[string]offset
	floats map[float32]offset
	doubles map[float64]offset
}

func newReadonlyDataManager() *ReadonlyDataManager {
	return &ReadonlyDataManager{
		rodataSize: 0,
		totalSize: 0,
		strings: map[string]offset{},
		floats: map[float32]offset{},
		doubles: map[float64]offset{},
	}
}

func (r *ReadonlyDataManager) registerFloatingConstant(fc *semantics.FloatingConstant) {
	if fc.T.Size() == SIZEOF_FLOAT {
		r.floats[float32(fc.Val)] = 0
	} else if fc.T.Size() == SIZEOF_DOUBLE {
		r.doubles[fc.Val] = 0
	} else {
		panic("Unexpected floating constant size")
	}
}

func (r *ReadonlyDataManager) registerStringConstant(s string) {
	r.strings[s] = 0
}

func (r *ReadonlyDataManager) assignOffsets() {
	size := offset(0)
	if len(r.strings) > 0 {
		stringsWithOffsets := map[string]offset{}
		for str := range r.strings {
			stringsWithOffsets[str] = size
			size += uint32(len(str) + 1)
		}
		r.strings = stringsWithOffsets
	}
	if len(r.floats) > 0 {
		if remainder := size % FLOAT_ALIGNMENT; remainder != 0 {
			size += FLOAT_ALIGNMENT - remainder
		}
		floatsWithOffsets := map[float32]offset{}
		for f := range r.floats {
			floatsWithOffsets[f] = size
			size += SIZEOF_FLOAT
		}
		r.floats = floatsWithOffsets
	}
	if len(r.doubles) > 0 {
		if remainder := size % DOUBLE_ALIGNMENT; remainder != 0 {
			size += DOUBLE_ALIGNMENT - remainder
		}
		doublesWithOffsets := map[float64]offset{}
		for d := range r.doubles {
			doublesWithOffsets[d] = size
			size += SIZEOF_DOUBLE
		}
		r.doubles = doublesWithOffsets
	}
	r.totalSize = size
}

func (r *ReadonlyDataManager) GetReadonlyDataAlignment() uint32 {
	if len(r.strings) > 0 {
		return STRING_ALIGNMENT
	}
	if len(r.floats) > 0 {
		return FLOAT_ALIGNMENT
	}
	return DOUBLE_ALIGNMENT
}

func (r *ReadonlyDataManager) HasReadonlyData() bool {
	return r.totalSize > 0
}

func (r *ReadonlyDataManager) RegisterReadonlyData(ir *irs.IntermediateRepresentation) {
	for _, fun := range ir.FunctionIr {
		for _, line := range fun.Code {
			switch irLine := line.(type) {
			case *irs.ConstantAssignmentLine:
				switch con := irLine.Constant.(type) {
				case semantics.FloatingConstant:
					r.registerFloatingConstant(&con)
				case semantics.StringConstant:
					r.registerStringConstant(con.Val)
				}
			case *irs.StringAssignmentLine:
				r.registerStringConstant(irLine.Val)
			}
		}
	}
	r.assignOffsets()
}

func (r *ReadonlyDataManager) GetOffset(pc semantics.ProgramConstant) offset {
	switch con := pc.(type) {
	case semantics.FloatingConstant:
		if con.T.Size() == QWORD_SIZE {
			return r.doubles[con.Val]
		}
		return r.floats[float32(con.Val)]
	case semantics.StringConstant:
		return r.GetStringOffset(con.Val)
	default:
		panic("Unexpected constant type")
	}
}

func (r *ReadonlyDataManager) GetStringOffset(str string) offset {
	return r.strings[str]
}

func (r *ReadonlyDataManager) AssignMemoryAccessorsToProgramConstantUsages(fun *AugmentedFunctionIr) {
	for _, line := range fun.Code {
		switch c := line.(type) {
		case *AugmentedConstantAssignmentLine:
			if _, isIntegral := c.AugmentedConstant.Constant.(semantics.IntegralConstant); !isIntegral {
				c.AugmentedConstant.ConstantMemoryAccessor = RoDataMemoryAccessor{
					Offset: r.GetOffset(c.AugmentedConstant.Constant),
				}
			}
		case *AugmentedStringAssignmentLine:
			c.StringMemoryAccessor = RoDataMemoryAccessor{
				Offset: r.GetStringOffset(c.Val),
			}
		}
	}
}

func (r *ReadonlyDataManager) GetSerialized() Rodata {
	res := make([]uint8, r.totalSize)
	for s, off := range r.strings {
		strBytes := []byte(s)
		for i, char := range strBytes {
			res[off + uint32(i)] = char
		}	
		res[off + uint32(len(strBytes))] = 0 // NULL
	}
	for f, off := range r.floats { 
		for i, b := range utils.EncodeFloatToLittleEndian(f) {
			res[off + uint32(i)] = b
		}
	}
	for d, off := range r.doubles {
		for i, b := range utils.EncodeDoubleToLittleEndian(d) {
			res[off + uint32(i)] = b
		}
	}
	return Rodata{
		Data: res,
		Alignment: r.GetReadonlyDataAlignment(),
	}
}
