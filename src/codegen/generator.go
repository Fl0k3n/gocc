package codegen

import (
	"irs"
	"semantics"
)


type Generator struct {
	functions []*irs.FunctionIR
	globals []*AugmentedGlobalSymbol
	asm *X86_64Writer
	registerAllocator RegisterAllocator
	typeEngine *semantics.TypeEngine
	memoryManager *MemoryManager
	labels *irs.LabelProvider
	rodataManager *ReadonlyDataManager
}

func NewGenerator(
	ir *irs.IntermediateRepresentation,
	registerAllocator RegisterAllocator,
	memoryManager *MemoryManager,
	assemblyWriter *X86_64Writer,
) *Generator {
	g := &Generator{
		functions: ir.FunctionIr,
		asm: assemblyWriter,
		registerAllocator: registerAllocator,
		typeEngine: ir.BootstrappedTypeEngine,
		memoryManager: memoryManager,
		labels: irs.NewLabelProvider(),
		rodataManager: newReadonlyDataManager(),
	}
	augmentedGlobals := make([]*AugmentedGlobalSymbol, len(ir.Globals))
	for i, global := range ir.Globals {
		augmentedGlobals[i] = g.augmentGlobal(global)
	}
	g.globals = augmentedGlobals
	g.rodataManager.RegisterReadonlyData(ir)
	return g
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
	} else {
		g.asm.MovMemoryToFloatingRegister(destReg.(FloatingRegister), srcMem)
	}
}

func (g *Generator) loadSymbol(asym *AugmentedSymbol) {
	g.checkForGotUnwrapping(asym)
	g.load(asym.Register, asym.MemoryAccessor)
}

func (g *Generator) store(destMem MemoryAccessor, srcReg Register) {
	if integralReg, isIntegral := srcReg.(IntegralRegister); isIntegral {
		g.asm.MovIntegralRegisterToMemory(destMem, integralReg)
	} else {
		g.asm.MovFloatingRegisterToMemory(destMem, srcReg.(FloatingRegister))
	}
}

func (g *Generator) storeSymbol(asym *AugmentedSymbol) {
	g.checkForGotUnwrapping(asym)
	g.store(asym.MemoryAccessor, asym.Register)
}

func (g *Generator) storeInLValue(dest *AugmentedLValue, srcReg Register) {
	if dest.IsDereferenced {
		// g.loadReference(dest.Sym, dest.Sym)
		// g.store(RegisterMemoryAccessor{Register: dest.Sym.Register.(IntegralRegister)}, srcReg)
		if dest.Sym.LoadBeforeRead {
			g.loadSymbol(dest.Sym)
		}
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
	if _, isIntegral := dest.(IntegralRegister); isIntegral {
		g.asm.MovIntegralRegisterToIntegralRegister(dest.(IntegralRegister), src.(IntegralRegister))
	} else {
		g.asm.MovFloatingRegisterToFloatingRegister(dest.(FloatingRegister), src.(FloatingRegister))
	}
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

func (g *Generator) saveConstantInRegister(dest Register, con *AugmentedProgramConstant) {
	switch c := con.Constant.(type) {
	case semantics.IntegralConstant:
		if integralReg, isIntegral := dest.(IntegralRegister); isIntegral {
			g.asm.MovIntegralConstantToIntegralRegister(integralReg, int(c.Val))
		} else {
			panic("Expected integral register")
		}
	case semantics.FloatingConstant:
		if floatingReg, isFloating := dest.(FloatingRegister); isFloating {
			g.asm.Reference(*con.LoaderRegister, con.ConstantMemoryAccessor)
			g.asm.MovMemoryToFloatingRegister(floatingReg, RegisterMemoryAccessor{Register: *con.LoaderRegister})	
		} else {
			panic("Expected floating register")
		}
	default:
		panic("Unexpected program constant type")
	}
}

func (g *Generator) performBinaryOperationOnIntegralRegisters(
	leftReg IntegralRegister,
	operator string,
	rightReg IntegralRegister,
) (resultReg Register) {
	switch operator {
	case "+":
		g.asm.AddIntegralRegisters(leftReg, rightReg)
	case "-":
		g.asm.SubIntegralRegisters(leftReg, rightReg)
	case "*":
		// TODO unsigned and sign extend both regs to word if they are byte
		g.asm.SignedMultiplyIntegralRegisters(leftReg, rightReg)
	case "/", "%":
		// TODO this behaves differently for bytes (not dl:al but whole in ax)
		// this assumes that left represents rax or rdx depending on operator
		g.asm.ClearIntegralRegister(GetIntegralRegisterFamily(DIV_OP_DIVIDENT_HIGHER_BITS_REG).Use(DWORD))
		g.asm.SignedDivideRaxRdxByIntegralRegister(rightReg)
		if operator == "/"  {
			return GetIntegralRegisterFamily(DIV_OP_DIVIDENT_LOWER_BITS_REG).UseForSize(leftReg.Size())
		} else {
			return GetIntegralRegisterFamily(DIV_OP_DIVIDENT_HIGHER_BITS_REG).UseForSize(leftReg.Size())
		}
	case "==", "<", "<=", ">", ">=", "!=":
		g.asm.CompareIntegralRegisters(leftReg, rightReg)
		negated := false
		if operator == "!=" {
			negated = true
		}
		g.asm.SetComparisonResult(leftReg, RELATIONAL_OPERATOR_TO_CONDITION[operator], negated)
	case "&&":
		doneLabel := g.labels.Next(irs.AND_DONE)
		setZeroLabel := g.labels.Next(irs.AND_ZERO)
		g.asm.CompareToZero(leftReg)
		g.asm.JumpIfZero(setZeroLabel)
		g.asm.CompareToZero(rightReg)
		g.asm.JumpIfZero(setZeroLabel)
		g.asm.MovIntegralConstantToIntegralRegister(leftReg, 1)
		g.asm.JumpToLabel(doneLabel)
		g.asm.PutLabel(setZeroLabel)
		g.asm.MovIntegralConstantToIntegralRegister(leftReg, 0)
		g.asm.PutLabel(doneLabel)
	case "||":
		doneLabel := g.labels.Next(irs.OR_DONE)
		setOneLabel := g.labels.Next(irs.OR_ONE)
		g.asm.CompareToZero(leftReg)
		g.asm.JumpIfNotZero(setOneLabel)
		g.asm.CompareToZero(rightReg)
		g.asm.JumpIfNotZero(setOneLabel)
		g.asm.MovIntegralConstantToIntegralRegister(leftReg, 0)
		g.asm.JumpToLabel(doneLabel)
		g.asm.PutLabel(setOneLabel)
		g.asm.MovIntegralConstantToIntegralRegister(leftReg, 1)
		g.asm.PutLabel(doneLabel)
	default:
		panic("Unsupported integral binary operation: " + operator)
	}
	return leftReg
}

func (g *Generator) performBinaryOperationOnFloatinRegisters(
	leftReg FloatingRegister,
	operator string,
	rightReg FloatingRegister,
) (resultReg Register) {
	switch operator {
	case "+":
		g.asm.AddFloatingRegisters(leftReg, rightReg)
	// case "-":
	// case "*":
	// case "/":
	// case "==", "<", "<=", ">", ">=", "!=":
	default:
		panic("Unsupported floating binary operation: " + operator)
	}
	return leftReg
}

func (g *Generator) performBinaryOperationOnRegisters(leftReg Register, operator string, rightReg Register) (resultReg Register) {
	if leftReg.Size() != rightReg.Size() {
		panic("Register size mismatch for binary operation")
	}
	switch lreg := leftReg.(type) {
	case IntegralRegister:
		rreg := rightReg.(IntegralRegister)
		return g.performBinaryOperationOnIntegralRegisters(lreg, operator, rreg)
	case FloatingRegister:
		rreg := rightReg.(FloatingRegister)
		return g.performBinaryOperationOnFloatinRegisters(lreg, operator, rreg)
	default:
		panic("unexpected register")
	} 
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
			// TODO ~
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
	stackDiff := len(l.ViaStackArgs) * QWORD_SIZE
	if stackDiff > 0 {
		// we must be 16B aligned before call
		if remainder := stackDiff % STACK_ALIGNMENT; remainder != 0 {
			subtract = STACK_ALIGNMENT - remainder
			g.subtractStackPointer(subtract)
		}
	}
	for i := len(l.ViaStackArgs) - 1; i >= 0; i-- {
		arg := l.ViaStackArgs[i]
		if arg.LoadBeforeRead {
			g.loadSymbol(arg)
		}
		g.subtractStackPointer(QWORD_SIZE)
		g.store(RegisterMemoryAccessor{
			Register: g.registerAllocator.GetStackPointer(),
		}, arg.Register)
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

func (g *Generator) typeCast(fromSymbol *AugmentedSymbol, toSymbol *AugmentedSymbol) {
	srcClass := g.memoryManager.classifySymbol(fromSymbol.Sym)
	dstClass := g.memoryManager.classifySymbol(toSymbol.Sym)
	srcSize := fromSymbol.Sym.Ctype.Size()
	dstSize := toSymbol.Sym.Ctype.Size()

	if fromSymbol.LoadBeforeRead {
		g.loadSymbol(fromSymbol)
	}

	if srcClass == INTEGER && dstClass == INTEGER {
		if (srcSize == DWORD_SIZE && dstSize == QWORD_SIZE) || (srcSize == QWORD_SIZE && dstSize == DWORD_SIZE) {
			if fromSymbol.Register.(IntegralRegister).Family != toSymbol.Register.(IntegralRegister).Family {
				panic("should have allocated same reg")
			}
			// x86 arch sign extends dword registers to quad automatically so we don't have to do anything
		} else {
			// if fromSymbol.LoadBeforeRead {
			// 	g.checkForGotUnwrapping(fromSymbol)
			// 	g.asm.MovMemoryToIntegralRegister(toSymbol.Register.(IntegralRegister), fromSymbol.MemoryAccessor)
			// } else {
			// 	g.asm.MovIntegralRegisteraToIntegralRegister()
			// }
		}
		srcUnsigned := g.typeEngine.IsUnsignedType(fromSymbol.Sym.Ctype)
		dstUnsigned := g.typeEngine.IsUnsignedType(toSymbol.Sym.Ctype)
		if srcUnsigned && dstUnsigned {
			if dstSize > srcSize {
				
			}
		}
	} else {
		panic("TODO")
	}

	if toSymbol.StoreAfterWrite {
		g.storeSymbol(toSymbol)
	}
}

func (g *Generator) generateFunctionCode(fun *AugmentedFunctionIr) {
	for _, line := range fun.Code {
		switch ir := line.(type) {
		case *AugmentedConstantAssignmentLine:
			g.saveConstantInRegister(ir.LhsSymbol.Register, ir.AugmentedConstant)
			if ir.LhsSymbol.StoreAfterWrite {
				g.storeSymbol(ir.LhsSymbol)
			}
		case *AugmentedStringAssignmentLine:
			g.asm.Reference(*ir.LoaderRegister, ir.StringMemoryAccessor)
			if !ir.LhsSymbol.Register.Equals(*ir.LoaderRegister) {
				g.copyRegister(ir.LhsSymbol.Register, *ir.LoaderRegister)
			}
			if ir.LhsSymbol.StoreAfterWrite {
				g.storeSymbol(ir.LhsSymbol)
			}
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
		case *AugmentedTypeCastLine:
			g.typeCast(ir.FromSymbol, ir.ToSymbol)
		default:
			panic("unexpected ir line")
		}
	}
}

func (g *Generator) generateFunction(fun *irs.FunctionIR) {	
	g.labels.EnterFunction(fun.FunctionSymbol.Symbol.Name)
	g.asm.EnterFunction(fun.FunctionSymbol)
	augmentedFun := g.prepareAugmentedIr(fun)
	g.asm.PutLabel(createFunctionLabel(fun.FunctionSymbol.Symbol.Name))

	g.rodataManager.AssignMemoryAccessorsToProgramConstantUsages(augmentedFun)
	g.registerAllocator.Alloc(augmentedFun)
	g.prepareStackForCodeGeneration(augmentedFun)

	g.generateFunctionCode(augmentedFun)
	g.returnFromFunction(augmentedFun)
}

func (g *Generator) generateGlobalInitializersData() {
	for _, aglob := range g.globals {
		global := aglob.Global
		if global.IsExtern || global.IsFunction || global.Initializers == nil || len(global.Initializers) == 0 {
			continue
		}
		buff := make([]byte, global.Symbol.Ctype.Size())
		for i := 0; i < len(buff); i++ {
			buff[i] = 0
		}
		for _, initializer := range global.Initializers {
			if g.typeEngine.AreTypesEqual(semantics.STRING_LITERAL_TYPE, global.Symbol.Ctype) {
				panic("Global string constants are unsupported for now")
			}
			encoded := encodeNumericProgramConstant(initializer.Constant, g.typeEngine)
			for i, b := range encoded {
				buff[initializer.Offset + i] = b
			}
		}
		aglob.EncodedInitializerData = buff
	}
}

func (g *Generator) Generate() *CodeRepresentation {
	g.memoryManager.AssignMemoryToGlobals(g.globals)
	for _, fun := range g.functions {
		g.generateFunction(fun)
	}
	g.generateGlobalInitializersData()
	return &CodeRepresentation{
		Code: g.asm.GetAssembly(),
		Globals: g.globals,
		Rodata: g.rodataManager.GetSerialized(),
	}
}
