package codegen

import (
	"asms"
	"fmt"
	"irs"
	"semantics"
)


type Generator struct {
	functions []*irs.FunctionIR
	globals []*irs.GlobalSymbol
	asm *asms.Writer
	registerAllocator RegisterAllocator
	typeEngine *semantics.TypeEngine
}

func NewGenerator(functions []*irs.FunctionIR, globals []*irs.GlobalSymbol, asmWriter *asms.Writer,
				 registerAllocator RegisterAllocator, typeEngine *semantics.TypeEngine) *Generator {
	return &Generator{
		functions: functions,
		globals: globals,
		asm: asmWriter,
		registerAllocator: registerAllocator,
		typeEngine: typeEngine,
	}
}

func (g *Generator) getStorageClass(t semantics.Ctype) StorageClass {
	switch {
	case g.typeEngine.IsIntegralType(t) || g.typeEngine.IsPointer(t): return INTEGER
	case g.typeEngine.IsFloatingType(t): return FLOATING
	default: return MEMORY
	}
}

func (g *Generator) augmentSymbol(sym *irs.Symbol) *AugmentedSymbol {
	if sym == nil {
		return nil
	}
	return &AugmentedSymbol{
		Sym: sym,
		Identity: fmt.Sprintf("%d_%d", int(sym.T), sym.Index),
		Size: sym.Ctype.Size(),
		StorageClass: g.getStorageClass(sym.Ctype),
	}
}

func (g *Generator) generateFunction(fun *irs.FunctionIR) {
	// g.registerAllocator.Alloc(fun)
	// for _, line := range fun.Code {
	// 	g.registerAllocator.Advance()
	// 	switch ir := line.(type) {
	// 	case *irs.BinaryOperationLine:
	// 		leftReg := g.registerAllocator.GetLeftOperandRegister()
			
	// 	}
	// }
}

func (g *Generator) handleGlobals() {

}

func (g *Generator) Generate() {
	g.handleGlobals()
	for _, fun := range g.functions {
		g.generateFunction(fun)
	}
}

