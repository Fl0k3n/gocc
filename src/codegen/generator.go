package codegen

import (
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

func (g *Generator) generateFunctionPrologue(fun *AugmentedFunctionIr) {
	framePtr := g.registerAllocator.GetFramePointer()
	stackPtr := g.registerAllocator.GetStackPointer()
	g.asm.PushIntegralReg(framePtr)
	g.asm.MovIntegralRegisterToIntegralRegister(framePtr, stackPtr)
}

func (g *Generator) checkForGotUnwrapping(asym *AugmentedSymbol) {
	if asym.RequiresGotUnwrapping {
		g.load(asym.GotAddressHolder, asym.MemoryAccessor)
		asym.MemoryAccessor = RegisterMemoryAccessor{Register: asym.GotAddressHolder}
	} else if g.memoryManager.UsesGOTAddressing(asym) {
		panic("GOT addressing requires got unwrapping") // sanity check
	}
}

func (g *Generator) load(destReg Register, srcMem MemoryAccessor) {
	if integralReg, isIntegral := destReg.(IntegralRegister); isIntegral {
		g.asm.MovMemoryToIntegralRegister(integralReg, srcMem)
	}
}

func (g *Generator) loadSymbol(asym *AugmentedSymbol) {
	g.checkForGotUnwrapping(asym)
	g.load(asym.Register, asym.MemoryAccessor)
}

func (g *Generator) store(destMem MemoryAccessor, srcReg Register) {
	if integralReg, isIntegral := srcReg.(IntegralRegister); isIntegral {
		g.asm.MovIntegralRegisterToMemory(destMem, integralReg)
	}
}

func (g *Generator) storeSymbol(asym *AugmentedSymbol) {
	g.checkForGotUnwrapping(asym)
	g.store(asym.MemoryAccessor, asym.Register)
}

func (g *Generator) storeInLValue(dest *AugmentedLValue, srcReg Register) {
	if dest.IsDereferenced {
		g.loadReference(dest.Sym, dest.Sym)
		g.store(RegisterMemoryAccessor{Register: dest.Sym.Register.(IntegralRegister)}, srcReg)
	} else {
		g.checkForGotUnwrapping(dest.Sym)
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
	// TODO
	g.asm.MovIntegralRegisterToIntegralRegister(dest.(IntegralRegister), src.(IntegralRegister))
}

func (g *Generator) call(asym *AugmentedSymbol) {
	if g.memoryManager.RequiresRegisterForCall(asym) {
		g.checkForGotUnwrapping(asym)
		reg := asym.Register.(IntegralRegister)
		// TODO load or just call memory assessor?
		if asym.LoadBeforeRead {
			g.load(reg, asym.MemoryAccessor)
		}
		g.asm.Call(RegisterMemoryAccessor{Register: reg})
	} else {
		g.asm.Call(asym.MemoryAccessor)
	}
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
	switch lreg := leftReg.(type) {
	case IntegralRegister:
		rreg := rightReg.(IntegralRegister)
		switch operator {
		case "+":
			g.asm.AddIntegralRegisters(lreg, rreg)
		case "-":
			g.asm.SubIntegralRegisters(lreg, rreg)
		case "*":
			// TODO unsigned and sign extend both regs to word if they are byte
			g.asm.SignedMultiplyIntegralRegisters(lreg, rreg)
		case "/", "%":
			// TODO this behaves differently for bytes (not dl:al but whole in ax)
			// also clear rax rdx, this assumes that left represents rax or rdx depending on operator
			g.asm.SignedDivideRaxRdxByIntegralRegister(rreg)
		case "==", "<", "<=", ">", ">=":
			g.asm.CompareIntegralRegisters(lreg, rreg)
			g.asm.SetComparisonResult(lreg, RELATIONAL_OPERATOR_TO_CONDITION[operator], false)
		}
	case FloatingRegister:
		// rreg := rightReg.(FloatingRegister)
	default:
		panic("unexpected register")
	} 
	return leftReg
}

func (g *Generator) performUnaryOperationOnRegister(register Register, operator string) Register {
	switch reg := register.(type) {
		case IntegralRegister:
			switch operator {
			case "!":
				g.asm.CompareToZero(reg)
				g.asm.SetComparisonResult(reg, EQUAL, false)
			case "-":
				g.asm.NegateIntegralRegister(reg)
			default:
				panic("unexpected register unary operator")
			}
	}
	return register
}

func (g *Generator) loadReference(lhsSym *AugmentedSymbol, rhsSym *AugmentedSymbol) {
	g.checkForGotUnwrapping(rhsSym)
	g.asm.Reference(lhsSym.Register.(IntegralRegister), rhsSym.MemoryAccessor)
}

func (g *Generator) dereference(lhsSym *AugmentedSymbol, rhsSym *AugmentedSymbol) {
	g.checkForGotUnwrapping(rhsSym)
	g.asm.MovMemoryToIntegralRegister(lhsSym.Register.(IntegralRegister), RegisterMemoryAccessor{
		Register: rhsSym.Register.(IntegralRegister),
	})
}

func (g *Generator) storeCalleeSaveRegisters(fun *AugmentedFunctionIr) {
	for _, regWithMem := range fun.IntegralRegistersToPersist {
		g.store(regWithMem.MemoryAccessor, regWithMem.Register)
	}
	for _, regWithMem := range fun.FloatingRegistersToPersist {
		g.store(regWithMem.MemoryAccessor, regWithMem.Register)
	}
}

func (g *Generator) restoreCalleeSaveRegisters(fun *AugmentedFunctionIr) {
	for _, regWithMem := range fun.IntegralRegistersToPersist {
		g.load(regWithMem.Register, regWithMem.MemoryAccessor)
	}
	for _, regWithMem := range fun.FloatingRegistersToPersist {
		g.load(regWithMem.Register, regWithMem.MemoryAccessor)
	}
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
			g.loadSymbol(arg)
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

func (g *Generator) prepareStackForCodeGeneration(augmentedFun *AugmentedFunctionIr) (frameSize int) {
	frameSizeAfterPrologue := SIZEOF_RETURN_ADDR + SIZEOF_RBP
	stackSubtact := g.memoryManager.AllocStackMemoryAndGetStackSubtract(augmentedFun, 0, frameSizeAfterPrologue)
	stackSubtact = g.memoryManager.GetStackPointerAlignment(stackSubtact + frameSizeAfterPrologue) - frameSizeAfterPrologue

	g.generateFunctionPrologue(augmentedFun)
	g.subtractStackPointer(stackSubtact)
	g.storeCalleeSaveRegisters(augmentedFun)
	g.storeArgsPassedInRegsOnStack(augmentedFun)
	return stackSubtact + frameSizeAfterPrologue
}

// return value (if present) must be stored in appropriate register before this call
func (g *Generator) returnFromFunction(fun *AugmentedFunctionIr) {
	g.asm.PutLabel(fun.ReturnLabel)
	g.restoreCalleeSaveRegisters(fun)
	framePointer := g.registerAllocator.GetFramePointer()
	g.asm.MovIntegralRegisterToIntegralRegister(g.registerAllocator.GetStackPointer(), framePointer)
	g.asm.PopIntegralReg(framePointer)
	g.asm.Return()
}

func (g *Generator) generateFunctionCode(fun *AugmentedFunctionIr) {
	for _, line := range fun.Code {
		switch ir := line.(type) {
		case *AugmentedConstantAssignmentLine:
			g.saveConstantInRegister(ir.LhsSymbol.Register, ir.Constant)
			if ir.LhsSymbol.StoreAfterWrite {
				g.storeSymbol(ir.LhsSymbol)
			}
		case *AugmentedStringAssignmentLine:
		case *AugmentedBiSymbolAssignmentLine:
			if ir.RhsSymbol.LoadBeforeRead {
				g.loadSymbol(ir.RhsSymbol)
			}
			g.storeInLValue(ir.LValue, ir.RhsSymbol.Register)
		case *AugmentedBinaryOperationLine:
			if ir.LeftOperand.LoadBeforeRead {
				g.loadSymbol(ir.LeftOperand)
			}
			if ir.RightOperand.LoadBeforeRead {
				g.loadSymbol(ir.RightOperand)
			}
			// TODO some (most) operations allow one operand to be memory, use that
			resReg := g.performBinaryOperationOnRegisters(ir.LeftOperand.Register, ir.Operator, ir.RightOperand.Register)
			if !resReg.Equals(ir.LhsSymbol.Register) {
				g.copyRegister(ir.LhsSymbol.Register, resReg)	
			}
			if ir.LhsSymbol.StoreAfterWrite {
				g.storeSymbol(ir.LhsSymbol)
			}
		case *AugmentedUnaryOperationLine:
			if ir.Operand.LoadBeforeRead {
				g.loadSymbol(ir.Operand)
			}
			if ir.Operator == "*" {
				g.dereference(ir.LhsSymbol, ir.Operand)
			} else if ir.Operator == "&" {
				g.loadReference(ir.LhsSymbol, ir.Operand)
			} else {
				resReg := g.performUnaryOperationOnRegister(ir.Operand.Register, ir.Operator)
				if !resReg.Equals(ir.LhsSymbol.Register) {
					g.copyRegister(ir.LhsSymbol.Register, resReg)
				}
			}
			if ir.LhsSymbol.StoreAfterWrite {
				g.storeSymbol(ir.LhsSymbol)
			}
		case *AugmentedFunctionCallLine:
			for _, arg := range ir.ViaRegisterArgs {
				if arg.LoadBeforeRead {
					g.loadSymbol(arg)
				}
			}
			stackSubtract := g.storeArgsOnStackAndGetSubtract(ir)
			g.call(ir.FunctionSymbol)
			g.increaseStackPointer(stackSubtract)
			if ir.ReturnSymbol != nil {
				if ir.ReturnSymbol.StoreAfterWrite {
					// assuming simple return mode (no structs > 8B)
					g.storeSymbol(ir.ReturnSymbol)
				}
			}
		case *AugmentedGotoLine:
			g.asm.JumpToLabel(ir.TargetLabel)
		case *AugmentedLabelLine:
			g.asm.PutLabel(ir.Label)
		case *AugmentedIfGotoLine:
			if ir.ConditionSymbol.LoadBeforeRead {
				g.loadSymbol(ir.ConditionSymbol)
			}
			// TODO maybe try to optimize it even at this point
			g.asm.CompareToZero(ir.ConditionSymbol.Register)
			g.asm.JumpIfZero(ir.TargetLabel)
		case *AugmentedReturnLine:
			if ir.ReturnSymbol != nil {
				if ir.ReturnSymbol.LoadBeforeRead {
					g.loadSymbol(ir.ReturnSymbol)
				}
			}
			g.asm.JumpToLabel(fun.ReturnLabel)
		default:
			panic("unexpected ir line")
		}
	}
}

func (g *Generator) generateFunction(fun *irs.FunctionIR) {	
	g.asm.EnterFunction(fun.FunctionSymbol)
	augmentedFun := g.prepareAugmentedIr(fun)
	g.asm.PutLabel(createFunctionLabel(fun.FunctionSymbol.Symbol.Name))

	g.registerAllocator.Alloc(augmentedFun)
	g.prepareStackForCodeGeneration(augmentedFun)

	g.generateFunctionCode(augmentedFun)
	g.returnFromFunction(augmentedFun)
}


func (g *Generator) Generate() []*FunctionCode {
	g.memoryManager.AssignMemoryToGlobals(g.globals)
	for _, fun := range g.functions {
		g.generateFunction(fun)
	}
	return g.asm.GetAssembly()
}
