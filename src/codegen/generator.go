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
				 registerAllocator RegisterAllocator, memoryManager *MemoryManager,
				 assemblyWriter *X86_64Writer, typeEngine *semantics.TypeEngine) *Generator {
	return &Generator{
		functions: functions,
		globals: globals,
		asm: assemblyWriter,
		registerAllocator: registerAllocator,
		typeEngine: typeEngine,
		memoryManager: memoryManager,
	}
}

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

func (g *Generator) subtractStackPointer(by int) {
	if by != 0 {
		g.asm.SubtractConstantInteger(g.registerAllocator.GetStackPointer(), by)
	}
}

func (g *Generator) increaseStackPointer(by int) {
	if by != 0 {
		g.asm.AddConstantInteger(g.registerAllocator.GetStackPointer(), by)
	}
}

func (g *Generator) copyRegister(dest Register, src Register) {

}

func (g *Generator) call(asym *AugmentedSymbol) {
	reg := asym.Register.(IntegralRegister)
	g.asm.MovMemoryToIntegralRegister(reg, asym.MemoryAccessor)
	g.asm.Call(RegisterMemoryAccessor{Register: reg})
}

func (g *Generator) saveConstantInRegister(dest Register, con semantics.ProgramConstant) {
	c := con.(semantics.IntegralConstant)
	if g.typeEngine.IsIntegralType(c.T) {
		// TODO check if its integral (should always be?)
		g.asm.MovIntegralConstantToIntegralRegister(dest.(IntegralRegister), int(c.Val))
		// g.asm.MovIntegralConstantToMemory(.LhsSymbol.MemoryAccessor, int(c.Val))
	} else {
		// TODO
	}
}

func (g *Generator) performBinaryOperationOnRegisters(leftReg Register, operator string, rightReg Register) (resultReg Register) {
	// TODO this is placeholder
	switch operator {
	case "+":
		g.asm.writeLine(fmt.Sprintf("add %s, %s", leftReg.Name(), rightReg.Name()))
	case "-":
		g.asm.writeLine(fmt.Sprintf("sub %s, %s", leftReg.Name(), rightReg.Name()))
	case "*":
		g.asm.writeLine(fmt.Sprintf("imul %s, %s", leftReg.Name(), rightReg.Name()))
	case "/", "%":
		g.asm.writeLine(fmt.Sprintf("idiv %s", rightReg.Name()))
	}
	return leftReg
}

func (g *Generator) pushCalleeSaveRegisters(fun *AugmentedFunctionIr) (stackDelta int) {
	stackDelta = 0
	for _, reg := range fun.IntegralRegistersToPersist {
		g.asm.PushIntegralReg(reg)
		stackDelta += reg.Size()
	}
	for _, reg := range fun.FloatingRegistersToPersist {
		g.asm.PushFloatingReg(reg)
		stackDelta += reg.Size()
	}
	return
}

// this should be needed only for the semi-useless basic allocator
func (g *Generator) storeArgsPassedInRegsOnStack(fun *AugmentedFunctionIr) {
	for _, arg := range fun.InRegisterArgsToStoreAfterFunctionEnter {
		g.store(arg.MemoryAccessor, arg.Register)
	}
}

func (g *Generator) storeArgsOnStackAndGetSubtract(l *AugmentedFunctionCallLine) int {
	subtract := 0
	stackDiff := 0
	for _, arg := range l.ViaStackArgs {
		if arg.LoadBeforeRead {
			g.load(arg)
		}
		g.store(RegisterMemoryAccessor{
			Register: g.registerAllocator.GetStackPointer(),
		}, arg.Register)
		g.subtractStackPointer(QWORD_SIZE)
		stackDiff += QWORD_SIZE
	}
	if stackDiff > 0 {
		// we must be 16B aligned before that
		subtract := g.memoryManager.GetStackPointerAlignment(stackDiff) - stackDiff
		g.subtractStackPointer(subtract)
	}
	return subtract + stackDiff
}

func (g *Generator) generateFunction(fun *irs.FunctionIR) {	
	g.asm.EnterFunction(fun.Name)
	augmentedFun := g.prepareAugmentedIr(fun)
	g.registerAllocator.Alloc(augmentedFun)
	frameSizeAfterPrologue := SIZEOF_RETURN_ADDR + SIZEOF_RBP
	stackSubtact := g.memoryManager.AllocStackMemoryAndGetStackSubtract(augmentedFun, 0, frameSizeAfterPrologue)
	stackSubtact += g.pushCalleeSaveRegisters(augmentedFun)

	stackSubtact = g.memoryManager.GetStackPointerAlignment(stackSubtact + frameSizeAfterPrologue) - frameSizeAfterPrologue

	g.generateFunctionPrologue(augmentedFun)
	g.subtractStackPointer(stackSubtact)
	g.storeArgsPassedInRegsOnStack(augmentedFun)
	for _, line := range augmentedFun.Code {
		switch ir := line.(type) {
		case *AugmentedConstantAssignmentLine:
			g.saveConstantInRegister(ir.LhsSymbol.Register, ir.Constant)
			if ir.LhsSymbol.StoreAfterWrite {
				g.store(ir.LhsSymbol.MemoryAccessor, ir.LhsSymbol.Register)
			}
		case *AugmentedStringAssignmentLine:
		case *AugmentedBiSymbolAssignmentLine:
			if ir.RhsSymbol.LoadBeforeRead {
				g.load(ir.RhsSymbol)
			}
			g.storeInLValue(ir.LValue, ir.RhsSymbol.Register)
		case *AugmentedBinaryOperationLine:
			if ir.LeftOperand.LoadBeforeRead {
				g.load(ir.LeftOperand)
			}
			if ir.RightOperand.LoadBeforeRead {
				g.load(ir.RightOperand)
			}
			// TODO some (most) operations allow one operand to be memory, consider using that for shorter code
			resReg := g.performBinaryOperationOnRegisters(ir.LeftOperand.Register, ir.Operator, ir.RightOperand.Register)
			if !resReg.Equals(ir.LhsSymbol.Register) {
				g.copyRegister(ir.LhsSymbol.Register, resReg)	
			}
			if ir.LhsSymbol.StoreAfterWrite {
				g.store(ir.LhsSymbol.MemoryAccessor, ir.LhsSymbol.Register)
			}
		case *AugmentedUnaryOperationLine:
		case *AugmentedFunctionCallLine:
			for _, arg := range ir.ViaRegisterArgs {
				if arg.LoadBeforeRead {
					g.load(arg)
				}
			}
			stackSubtract := g.storeArgsOnStackAndGetSubtract(ir)
			g.call(ir.FunctionSymbol)
			g.increaseStackPointer(stackSubtract)
			if ir.ReturnSymbol != nil {
				if ir.ReturnSymbol.StoreAfterWrite {
					// assuming simple return mode (no structs > 8B)
					g.store(ir.ReturnSymbol.MemoryAccessor, ir.ReturnSymbol.Register)
				}
			}
		case *AugmentedGotoLine:
			g.asm.JumpToLabel(ir.TargetLabel)
		case *AugmentedLabelLine:
			g.asm.PutLabel(ir.Label)
		case *AugmentedIfGotoLine:
			if ir.ConditionSymbol.LoadBeforeRead {
				g.load(ir.ConditionSymbol)
			}
			// TODO maybe try to optimize it even at this point
			g.asm.CompareToZero(ir.ConditionSymbol.Register)
			g.asm.JumpIfZero(ir.TargetLabel)
		case *AugmentedReturnLine:
			g.asm.writeLine("RETURN!")
		default:
			panic("unexpected ir line")
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

