package codegen

import (
	"fmt"
	"irs"
	"semantics"
)


type Generator struct {
	functions []*irs.FunctionIR
	globals []*irs.GlobalSymbol
	asm *X86_64Writer
	registerAllocator RegisterAllocator
	typeEngine *semantics.TypeEngine
	memoryManager *MemoryManager
}

func NewGenerator(functions []*irs.FunctionIR, globals []*irs.GlobalSymbol,
				 registerAllocator RegisterAllocator, typeEngine *semantics.TypeEngine) *Generator {
	return &Generator{
		functions: functions,
		globals: globals,
		asm: NewWriter(),
		registerAllocator: registerAllocator,
		typeEngine: typeEngine,
		memoryManager: newMemoryManager(),
	}
}

// func (g *Generator) getStorageClass(t semantics.Ctype) StorageMode {
// 	switch {
// 	case g.typeEngine.IsIntegralType(t) || g.typeEngine.IsPointer(t): return INTEGRAL
// 	case g.typeEngine.IsFloatingType(t): return FLOATING
// 	default: return MEMORY
// 	}
// }

func (g *Generator) augmentSymbol(sym *irs.Symbol) *AugmentedSymbol {
	if sym == nil {
		return nil
	}
	return &AugmentedSymbol{
		Sym: sym,
		Identity: fmt.Sprintf("%d_%d", int(sym.T), sym.Index),
	}
}

func (g *Generator) generateFunctionPrologue(fun *AugmentedFunctionIr) {
	framePtr := g.registerAllocator.GetFramePointer()
	stackPtr := g.registerAllocator.GetStackPointer()
	g.asm.PushIntegralReg(framePtr)
	g.asm.MovIntegralRegisterToIntegralRegister(framePtr, stackPtr)
}

func (g *Generator) generateFunctionEpilogue(fun *AugmentedFunctionIr) {

}

func (g *Generator) load(asym *AugmentedSymbol) {
	if integralReg, isIntegral := asym.Register.(IntegralRegister); isIntegral {
		g.asm.MovMemoryToIntegralRegister(integralReg, asym.MemoryAccessor)
	}
}

func (g *Generator) loadAddress(asym *AugmentedSymbol) {

}

func (g *Generator) store(destMem MemoryAccessor, srcReg Register) {
	if integralReg, isIntegral := srcReg.(IntegralRegister); isIntegral {
		g.asm.MovIntegralRegisterToMemory(destMem, integralReg)
	}
}

func (g *Generator) storeInLValue(dest *AugmentedLValue, srcReg Register) {
	if dest.IsDereferenced {
		g.loadAddress(dest.Sym)
		g.store(RegisterMemoryAccessor{Register: dest.Sym.Register.(IntegralRegister)}, srcReg)
	} else {
		g.store(dest.Sym.MemoryAccessor, srcReg)
	}
}

func (g *Generator) performBinaryOperationOnRegisters(leftReg Register, operator string, rightReg Register) (resultReg Register) {
	switch operator {
	case "+":
		g.asm.writeLine(fmt.Sprintf("add %s, %s", leftReg.Name(), rightReg.Name()))
	}
	return leftReg
}

func (g *Generator) generateFunction(fun *irs.FunctionIR) {	
	augmentedFun := g.prepareAugmentedIr(fun)
	g.generateFunctionPrologue(augmentedFun)
	stackSubtact := g.memoryManager.AllocStackMemoryAndGetStackSubtract(augmentedFun, 0)
	if stackSubtact > 0 {
		g.asm.SubtractConstantInteger(g.registerAllocator.GetStackPointer(), stackSubtact)
	}

	g.registerAllocator.Alloc(augmentedFun)
	for _, line := range augmentedFun.Code {
		switch ir := line.(type) {
		case AugmentedConstantAssignmentLine:
			c := ir.Constant.(semantics.IntegralConstant)
			if g.typeEngine.IsIntegralType(c.T) {
				g.asm.MovIntegralConstantToMemory(ir.LhsSymbol.MemoryAccessor, int(c.Val))
			} else {
				// TODO
			}
		case AugmentedStringAssignmentLine:
		case AugmentedBiSymbolAssignmentLine:
			if ir.RhsSymbol.LoadBeforeRead {
				g.load(ir.RhsSymbol)
			}
			g.storeInLValue(ir.LValue, ir.RhsSymbol.Register)
		case AugmentedBinaryOperationLine:
			if ir.LeftOperand.LoadBeforeRead {
				g.load(ir.LeftOperand)
			}
			if ir.RightOperand.LoadBeforeRead {
				g.load(ir.RightOperand)
			}
			resReg := g.performBinaryOperationOnRegisters(ir.LeftOperand.Register, ir.Operator, ir.RightOperand.Register)
			g.store(ir.LhsSymbol.MemoryAccessor, resReg)
		case AugmentedGotoLine:
			g.asm.JumpToLabel(ir.TargetLabel)
		case AugmentedIfGotoLine:
			if ir.ConditionSymbol.LoadBeforeRead {
				g.load(ir.ConditionSymbol)
			}
			// TODO
			g.asm.JumpIfZero(ir.TargetLabel)
		}
	}
}

func (g *Generator) handleGlobals() {

}

func (g *Generator) Generate() {
	g.handleGlobals()
	for _, fun := range g.functions {
		g.generateFunction(fun)
	}
	g.asm.PrintAll()
}

