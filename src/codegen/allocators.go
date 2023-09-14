package codegen

import (
	"irs"
	"utils"
)

type RegisterAllocator interface {
	Alloc(fun *AugmentedFunctionIr)
	GetFramePointer() IntegralRegister
	GetStackPointer() IntegralRegister
}

type FunctionArgPassMode int 

const (
	REGISTER_ONLY FunctionArgPassMode = iota
	MEMORY_ONLY
	REGISTER_AND_MEMORY
)

type ArgToRegisterMapping struct {
	PassMode FunctionArgPassMode
	StorageClass ArgumentStorageClass
	Register Register
}

type FunctionAllocationState struct {
	usedIntegralRegisters *utils.Set[IntegralRegisterFamilyT]
	usedFloatingRegisters *utils.Set[FloatingRegisterFamilyT]
	currentlyUsedIntegralRegisters *utils.Set[IntegralRegisterFamilyT]
	currentlyUsedFloatingRegisters *utils.Set[FloatingRegisterFamilyT]
}

// should be similar to the one that gcc uses with -O0
type BasicRegisterAllocator struct {
	memoryManager *MemoryManager
	allocState FunctionAllocationState
	integralRegisterAllocationOrder []IntegralRegisterFamilyT
	floatingRegisterAllocationOrder []FloatingRegisterFamilyT
}

func NewBasicAllocator(memoryManager *MemoryManager) *BasicRegisterAllocator {
	return &BasicRegisterAllocator{
		memoryManager: memoryManager,
	}
}

func (a *BasicRegisterAllocator) freeAllGeneralPurposeRegisters() {
	a.allocState.currentlyUsedFloatingRegisters = utils.NewSet[FloatingRegisterFamilyT]()
	a.allocState.currentlyUsedIntegralRegisters = utils.NewSet[IntegralRegisterFamilyT]()
}

func (a *BasicRegisterAllocator) markRegisterAsUsedInFunction(reg Register) {
	if ir, isIntegral := reg.(IntegralRegister); isIntegral {
		a.allocState.usedIntegralRegisters.Add(ir.Family.T)
	} else {
		a.allocState.usedFloatingRegisters.Add(reg.(FloatingRegister).Family.T)
	}
}

func (a *BasicRegisterAllocator) nextFreeIntegralRegister() IntegralRegisterFamily {
	for _, reg := range a.integralRegisterAllocationOrder {
		if !a.allocState.currentlyUsedIntegralRegisters.Has(reg) {
			a.allocState.currentlyUsedIntegralRegisters.Add(reg)
			a.allocState.usedIntegralRegisters.Add(reg)
			return GetIntegralRegisterFamily(reg)
		}
	}
	panic("out of integral registers") // this should never happen for this naive allocator
}

func (a *BasicRegisterAllocator) nextFreeFloatingRegister() FloatingRegisterFamily {
	for _, reg := range a.floatingRegisterAllocationOrder {
		if !a.allocState.currentlyUsedFloatingRegisters.Has(reg) {
			a.allocState.currentlyUsedFloatingRegisters.Add(reg)
			a.allocState.usedFloatingRegisters.Add(reg)
			return GetFloatingRegisterFamily(reg)
		}
	}
	panic("out of floating registers") // this should never happen for this naive allocator
}

func (a *BasicRegisterAllocator) allocFunctionReturnRegister(returnSymbol *AugmentedSymbol, storeMode bool) Register {
	switch a.memoryManager.classifySymbol(returnSymbol.Sym) {
	case INTEGER:
		reg := GetIntegralRegisterFamily(SYS_V_RETURN_INTEGRAL_REGISTER).UseForSize(returnSymbol.Sym.Ctype.SymbolSize())
		a.allocState.usedIntegralRegisters.Add(SYS_V_RETURN_INTEGRAL_REGISTER)
		a.allocState.currentlyUsedIntegralRegisters.Add(SYS_V_RETURN_INTEGRAL_REGISTER)
		if a.memoryManager.UsesGOTAddressing(returnSymbol) {
			returnSymbol.RequiresGotUnwrapping = true
			if storeMode {
				returnSymbol.GotAddressHolder = a.nextFreeIntegralRegister().UseForSize(QWORD_SIZE)
			} else {
				returnSymbol.GotAddressHolder = reg.Family.UseForSize(QWORD_SIZE)
			}
		}
		return reg
	case SSE:
		reg := GetFloatingRegisterFamily(SYS_V_RETURN_FLOATING_REGISTER).use()
		a.allocState.usedFloatingRegisters.Add(SYS_V_RETURN_FLOATING_REGISTER)
		a.allocState.currentlyUsedFloatingRegisters.Add(SYS_V_RETURN_FLOATING_REGISTER)
		if a.memoryManager.UsesGOTAddressing(returnSymbol) {
			returnSymbol.RequiresGotUnwrapping = true
			returnSymbol.GotAddressHolder = a.nextFreeIntegralRegister().UseForSize(QWORD_SIZE)
		}
		return reg
	default:
		panic("TODO")
	}
}

func (a *BasicRegisterAllocator) mapArgToMemory(storageClass ArgumentStorageClass) *ArgToRegisterMapping {
	return &ArgToRegisterMapping{
		PassMode: MEMORY_ONLY,
		StorageClass: storageClass,
		Register: nil,
	}
}

func (a *BasicRegisterAllocator) getArgToRegisterMapping(Args []*irs.Symbol) []*ArgToRegisterMapping {
	res := make([]*ArgToRegisterMapping, 0, len(Args))
	passedInIntegralRegisters := 0
	passedInFloatingRegisters := 0	
	for _, arg := range Args {
		switch a.memoryManager.classifySymbol(arg) {
		case INTEGER:
			if passedInIntegralRegisters >= len(SYSV_FUNCTION_ARG_INTEGRAL_REGISTERS) {
				res = append(res, a.mapArgToMemory(INTEGER))
			} else {
				regFamT := SYSV_FUNCTION_ARG_INTEGRAL_REGISTERS[passedInIntegralRegisters]
				passedInIntegralRegisters++
				res = append(res, &ArgToRegisterMapping{
					PassMode: REGISTER_ONLY,
					StorageClass: INTEGER,
					Register: GetIntegralRegisterFamily(regFamT).UseForSize(arg.Ctype.SymbolSize()),
				})
			}
		case SSE:
			if passedInFloatingRegisters >= len(SYSV_FUNCTION_ARG_FLOATING_REGISTERS) {
				res = append(res, a.mapArgToMemory(SSE))
			} else {
				regFamT := SYSV_FUNCTION_ARG_FLOATING_REGISTERS[passedInFloatingRegisters]
				passedInFloatingRegisters++
				res = append(res, &ArgToRegisterMapping{
					PassMode: REGISTER_ONLY,
					StorageClass: SSE,
					Register: GetFloatingRegisterFamily(regFamT).use(),
				})
			}
		default:
			panic("TODO")
		}
	}
	return res
}

func (a *BasicRegisterAllocator) allocGotHoldersToFunctionArgs(call *AugmentedFunctionCallLine) {
	// TODO in this allocator we can always spare a register for GOT loading, but in general if possible some integral register should be reused
	var gotHolder IntegralRegister
	gotHolderChosen := false
	for _, arg := range call.Args {
		if a.memoryManager.UsesGOTAddressing(arg) {
			if !gotHolderChosen {
				gotHolder = a.nextFreeIntegralRegister().Use(QWORD)
				gotHolderChosen = true
			}
			arg.RequiresGotUnwrapping = true
			arg.GotAddressHolder = gotHolder
		}
	}
}

func (a *BasicRegisterAllocator) allocFunctionArgRegisters(call *AugmentedFunctionCallLine) {
	viaStackIntegralArgs := []*AugmentedSymbol{}
	viaStackFloatingArgs := []*AugmentedSymbol{}
	a.allocGotHoldersToFunctionArgs(call)
	mapping := a.getArgToRegisterMapping(asSymbols(call.Args))
	for argNum := range mapping {
		arg := call.Args[argNum]
		if mapping[argNum].PassMode == REGISTER_ONLY {
			call.ViaRegisterArgs = append(call.ViaRegisterArgs, arg)
			arg.Register = mapping[argNum].Register
			if mapping[argNum].StorageClass == INTEGER {
				a.allocState.currentlyUsedIntegralRegisters.Add(arg.Register.(IntegralRegister).Family.T)
			} else {
				a.allocState.currentlyUsedFloatingRegisters.Add(arg.Register.(FloatingRegister).Family.T)
			}
			a.markRegisterAsUsedInFunction(arg.Register)
		} else {
			call.ViaStackArgs = append(call.ViaStackArgs, arg)
			if mapping[argNum].StorageClass == INTEGER {
				viaStackIntegralArgs = append(viaStackIntegralArgs, arg)
			} else {
				// TODO mem
				viaStackFloatingArgs = append(viaStackFloatingArgs, arg)
			}
		}
		arg.LoadBeforeRead = true
		arg.StoreAfterWrite = false
	}
	
	if len(viaStackIntegralArgs) > 0 {
		stackMoverIntegralReg := a.nextFreeIntegralRegister()
		for _, arg := range viaStackIntegralArgs {
			arg.Register = stackMoverIntegralReg.UseForSize(arg.Sym.Ctype.SymbolSize())
		}
	}
	if len(viaStackFloatingArgs) > 0 {
		stackMoverFloatingReg := a.nextFreeFloatingRegister()
		for _, arg := range viaStackFloatingArgs {
			arg.Register = stackMoverFloatingReg.use()
		}
	}
}

func (a *BasicRegisterAllocator) allocAnythingForStoreMode(asym *AugmentedSymbol) {
	if a.memoryManager.UsesGOTAddressing(asym) {
		asym.RequiresGotUnwrapping = true
		asym.GotAddressHolder = a.nextFreeIntegralRegister().Use(QWORD)
	}
	switch a.memoryManager.classifySymbol(asym.Sym) {
	case INTEGER:
		asym.Register = a.nextFreeIntegralRegister().UseForSize(asym.Sym.Ctype.SymbolSize())
	case SSE:
		asym.Register = a.nextFreeFloatingRegister().use()
	case MEMORY, SPLIT:
		panic("TODO")
	}
}

func (a *BasicRegisterAllocator) allocAnythingForLoadMode(asym *AugmentedSymbol) {
	switch a.memoryManager.classifySymbol(asym.Sym) {
	case INTEGER:
		asym.Register = a.nextFreeIntegralRegister().UseForSize(asym.Sym.Ctype.SymbolSize())
		if a.memoryManager.UsesGOTAddressing(asym) {
			asym.RequiresGotUnwrapping = true
			asym.GotAddressHolder = asym.Register.(IntegralRegister).Family.Use(QWORD)
		}
	case SSE:
		asym.Register = a.nextFreeFloatingRegister().use()
		if a.memoryManager.UsesGOTAddressing(asym) {
			asym.RequiresGotUnwrapping = true
			asym.GotAddressHolder = a.nextFreeIntegralRegister().Use(QWORD)
		}
	case MEMORY, SPLIT:
		panic("TODO")
	}
}

func (a *BasicRegisterAllocator) resetFunctionState() {
	a.allocState.usedIntegralRegisters = utils.NewSet[IntegralRegisterFamilyT]()
	a.allocState.usedFloatingRegisters = utils.NewSet[FloatingRegisterFamilyT]()
}

func (a *BasicRegisterAllocator) handleFunctionEnter(fun *AugmentedFunctionIr) {
	argsMapping := a.getArgToRegisterMapping(fun.Snapshot.ArgsSnapshot)
	for argNum, arg := range argsMapping {
		asym := fun.Args[argNum]
		if arg.PassMode == REGISTER_ONLY {
			// alloc space for every arg in callee frame and store it, since this allocator requires reads from mem to reg before any usage
			asym.Register = arg.Register
			fun.InRegisterArgsToPlaceOnCalleeStack = append(fun.InRegisterArgsToPlaceOnCalleeStack, asym.Sym)
			fun.InRegisterArgsToStoreAfterFunctionEnter = append(fun.InRegisterArgsToStoreAfterFunctionEnter, asym)
			if arg.StorageClass == INTEGER {
				a.allocState.usedIntegralRegisters.Add(arg.Register.(IntegralRegister).Family.T)
			} else {
				a.allocState.usedFloatingRegisters.Add(arg.Register.(FloatingRegister).Family.T)
			}
		} else {
			// todo partial passing of composites
			fun.ArgsPlacedOnCallerStack = append(fun.ArgsPlacedOnCallerStack, asym)
		}
	}
}

func (a *BasicRegisterAllocator) setRegistersToPersistByCallee(fun *AugmentedFunctionIr) {
	for _, csr := range SYSV_CALLEE_SAVE_INTEGRAL_REGISTERS {
		if a.allocState.usedIntegralRegisters.Has(csr) {
			fun.IntegralRegistersToPersist = append(fun.IntegralRegistersToPersist, 
				&RegisterWithAccessor{Register: GetIntegralRegisterFamily(csr).Use(QWORD)},
			)
		}
	}
	for _, csr := range SYSV_CALLEE_SAVE_FLOATING_REGISTERS {
		if a.allocState.usedFloatingRegisters.Has(csr) {
			fun.FloatingRegistersToPersist = append(fun.FloatingRegistersToPersist, 
				&RegisterWithAccessor{Register: GetFloatingRegisterFamily(csr).use()},
			)
		}
	}
}

func (a *BasicRegisterAllocator) setRegisterAllocationOrder(fun *AugmentedFunctionIr) {
	// first scratch registers, then arg registers in reversed order and then callee-save-registers
	a.integralRegisterAllocationOrder = []IntegralRegisterFamilyT{
		RAX, R10, R11, R9, R8, RCX, RDX, RSI, RDI, RBX, R12, R13, R14, R15,
	}
	a.floatingRegisterAllocationOrder = []FloatingRegisterFamilyT{
		XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15, XMM7, XMM6, XMM5, XMM4, XMM3, XMM2, XMM1, XMM0,
	}
}

func (a *BasicRegisterAllocator) allocRegistersForBinaryOperation(l *AugmentedBinaryOperationLine) {
	// TODO floats, clean this up
	l.LeftOperand.LoadBeforeRead = true
	l.RightOperand.LoadBeforeRead = true
	switch l.Operator {
	case "/", "%":
		a.allocState.currentlyUsedIntegralRegisters.Add(DIV_OP_DIVIDENT_HIGHER_BITS_REG)
		a.allocState.currentlyUsedIntegralRegisters.Add(DIV_OP_DIVIDENT_LOWER_BITS_REG)
		// this needs special handling with clearing the higher bits or zero extending lower etc
		l.LeftOperand.Register = GetIntegralRegisterFamily(DIV_OP_DIVIDENT_LOWER_BITS_REG).UseForSize(l.LeftOperand.Sym.Ctype.SymbolSize())
		if l.Operator == "/" {
			l.LhsSymbol.Register = GetIntegralRegisterFamily(DIV_OP_DIVIDENT_LOWER_BITS_REG).UseForSize(l.LeftOperand.Sym.Ctype.SymbolSize())
		} else {
			l.LhsSymbol.Register = GetIntegralRegisterFamily(DIV_OP_DIVIDENT_HIGHER_BITS_REG).UseForSize(l.LeftOperand.Sym.Ctype.SymbolSize())
		}
		a.markRegisterAsUsedInFunction(l.LeftOperand.Register)
		a.markRegisterAsUsedInFunction(l.LhsSymbol.Register)
		a.allocAnythingForLoadMode(l.RightOperand)
	default:
		a.allocAnythingForLoadMode(l.LeftOperand)
		a.allocAnythingForLoadMode(l.RightOperand)
		l.LhsSymbol.Register = l.LeftOperand.Register // TODO is this safe got-wise?
	}

	l.LhsSymbol.StoreAfterWrite = true
	if a.memoryManager.UsesGOTAddressing(l.LhsSymbol) {
		l.LhsSymbol.RequiresGotUnwrapping = true
		l.LhsSymbol.Register = a.nextFreeIntegralRegister().Use(QWORD)
	}
}

func (a *BasicRegisterAllocator) allocRegistersForUnaryOperation(l *AugmentedUnaryOperationLine) {
	// TODO floats
	l.Operand.LoadBeforeRead = false
	switch l.Operator {
	case "*":
		reg := a.nextFreeIntegralRegister()
		l.LhsSymbol.Register = reg.UseForSize(l.LhsSymbol.Sym.Ctype.SymbolSize())
		l.Operand.Register = reg.UseForSize(QWORD_SIZE)
		l.Operand.LoadBeforeRead = true
		if a.memoryManager.UsesGOTAddressing(l.Operand) {
			l.Operand.RequiresGotUnwrapping = true
			l.Operand.Register = reg.Use(QWORD)
		}
	case "&":
		l.LhsSymbol.Register = a.nextFreeIntegralRegister().UseForSize(l.LhsSymbol.Sym.Ctype.SymbolSize())
		if a.memoryManager.UsesGOTAddressing(l.Operand) {
			l.Operand.RequiresGotUnwrapping = true
			l.Operand.GotAddressHolder = a.nextFreeIntegralRegister().Use(QWORD)
		}
	case "!", "-":
		a.allocAnythingForLoadMode(l.Operand)
		l.LhsSymbol.Register = l.Operand.Register
	default:
		panic("TODO")
	}
	l.LhsSymbol.StoreAfterWrite = true
	if a.memoryManager.UsesGOTAddressing(l.LhsSymbol) {
		l.LhsSymbol.RequiresGotUnwrapping = true
		l.LhsSymbol.GotAddressHolder = a.nextFreeIntegralRegister().Use(QWORD)
	}
}

func (a *BasicRegisterAllocator) Alloc(fun *AugmentedFunctionIr) {
	a.resetFunctionState()
	a.handleFunctionEnter(fun)
	a.setRegisterAllocationOrder(fun)
	for _, line := range fun.Code {
		a.freeAllGeneralPurposeRegisters()
		switch l := line.(type) {
		case *AugmentedFunctionCallLine:
			if l.ReturnSymbol != nil {
				l.ReturnSymbol.Register = a.allocFunctionReturnRegister(l.ReturnSymbol, true)
				l.ReturnSymbol.StoreAfterWrite = true
			}
			a.allocFunctionArgRegisters(l)
			if a.memoryManager.RequiresRegisterForCall(l.FunctionSymbol) {
				reg := a.nextFreeIntegralRegister().Use(QWORD)
				l.FunctionSymbol.Register = reg
				if a.memoryManager.UsesGOTAddressing(l.FunctionSymbol) {
					l.FunctionSymbol.GotAddressHolder = reg
				}
				l.FunctionSymbol.LoadBeforeRead = true
			} else {
				l.FunctionSymbol.LoadBeforeRead = false
			}
		case *AugmentedReturnLine:
			if l.ReturnSymbol != nil {
				l.ReturnSymbol.Register = a.allocFunctionReturnRegister(l.ReturnSymbol, false)
				l.ReturnSymbol.LoadBeforeRead = true
			}
		case *AugmentedBinaryOperationLine:
			a.allocRegistersForBinaryOperation(l)
		case *AugmentedUnaryOperationLine:
			a.allocRegistersForUnaryOperation(l)
		case *AugmentedConstantAssignmentLine:
			// TODO check if constant requires register (it does for float, for example)
			l.LhsSymbol.StoreAfterWrite = true
			a.allocAnythingForStoreMode(l.LhsSymbol)
		case *AugmentedStringAssignmentLine:
			// TODO register for string?
			l.LhsSymbol.StoreAfterWrite = true
			a.allocAnythingForStoreMode(l.LhsSymbol)
		case *AugmentedBiSymbolAssignmentLine:
			l.RhsSymbol.LoadBeforeRead = true
			l.LValue.Sym.StoreAfterWrite = true
			a.allocAnythingForLoadMode(l.RhsSymbol)
			a.allocAnythingForStoreMode(l.LValue.Sym)
		case *AugmentedIfGotoLine:
			l.ConditionSymbol.LoadBeforeRead = true
			a.allocAnythingForLoadMode(l.ConditionSymbol)
		}
	}
	a.setRegistersToPersistByCallee(fun)
}

func (a *BasicRegisterAllocator) GetFramePointer() IntegralRegister {
	return GetIntegralRegisterFamily(RBP).Use(QWORD)
}

func (a *BasicRegisterAllocator) GetStackPointer() IntegralRegister {
	return GetIntegralRegisterFamily(RSP).Use(QWORD)
}
