package codegen

import "irs"


type MemoryManager struct {

}

func (m *MemoryManager) GetRequiredStackSubtract(fun *irs.FunctionIR) int64 {
	return 0
}

// func (m *MemoryManager) 