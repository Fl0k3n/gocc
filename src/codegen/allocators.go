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

func (a *BasicRegisterAllocator) allocFunctionReturnRegister(returnSymbol *AugmentedSymbol) Register {
	switch a.memoryManager.classifySymbol(returnSymbol.Sym) {
	case INTEGER:
		reg := GetIntegralRegisterFamily(SYS_V_RETURN_INTEGRAL_REGISTER).useForSize(returnSymbol.Sym.Ctype.Size())
		a.allocState.usedIntegralRegisters.Add(SYS_V_RETURN_INTEGRAL_REGISTER)
		a.allocState.currentlyUsedIntegralRegisters.Add(SYS_V_RETURN_INTEGRAL_REGISTER)
		return reg
	case SSE:
		reg := GetFloatingRegisterFamily(SYS_V_RETURN_FLOATING_REGISTER).use()
		a.allocState.usedFloatingRegisters.Add(SYS_V_RETURN_FLOATING_REGISTER)
		a.allocState.currentlyUsedFloatingRegisters.Add(SYS_V_RETURN_FLOATING_REGISTER)
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
					Register: GetIntegralRegisterFamily(regFamT).useForSize(arg.Ctype.Size()),
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

func (a *BasicRegisterAllocator) allocFunctionArgRegisters(call *AugmentedFunctionCallLine) {
	viaStackIntegralArgs := []*AugmentedSymbol{}
	viaStackFloatingArgs := []*AugmentedSymbol{}

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
			arg.Register = stackMoverIntegralReg.useForSize(arg.Sym.Ctype.Size())
		}
	}
	if len(viaStackFloatingArgs) > 0 {
		stackMoverFloatingReg := a.nextFreeFloatingRegister()
		for _, arg := range viaStackFloatingArgs {
			arg.Register = stackMoverFloatingReg.use()
		}
	}
}

func (a *BasicRegisterAllocator) allocAnything(symbols []*AugmentedSymbol) {
	for _, asym := range symbols {
		asym.LoadBeforeRead = true
		asym.StoreAfterWrite = true	
		switch a.memoryManager.classifySymbol(asym.Sym) {
		case INTEGER:
			asym.Register = a.nextFreeIntegralRegister().useForSize(asym.Sym.Ctype.Size())
		case SSE:
			asym.Register = a.nextFreeFloatingRegister().use()
		case MEMORY, SPLIT:
			panic("TODO")
		}
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
			// alloc space for every arg in callee frame and store it, since this allocator reads from mem to reg before any usage
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
				&RegisterWithAccessor{Register: GetIntegralRegisterFamily(csr).use(QWORD)},
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
	switch l.Operator {
	case "/", "%":
		a.allocState.currentlyUsedIntegralRegisters.Add(DIV_OP_DIVIDENT_HIGHER_BITS_REG)
		a.allocState.currentlyUsedIntegralRegisters.Add(DIV_OP_DIVIDENT_LOWER_BITS_REG)
		// this needs special handling with clearing the higher bits or zero extending lower etc
		l.LeftOperand.Register = GetIntegralRegisterFamily(DIV_OP_DIVIDENT_LOWER_BITS_REG).useForSize(l.LeftOperand.Sym.Ctype.Size())
		if l.Operator == "/" {
			l.LhsSymbol.Register = GetIntegralRegisterFamily(DIV_OP_DIVIDENT_LOWER_BITS_REG).useForSize(l.LeftOperand.Sym.Ctype.Size())
		} else {
			l.LhsSymbol.Register = GetIntegralRegisterFamily(DIV_OP_DIVIDENT_HIGHER_BITS_REG).useForSize(l.LeftOperand.Sym.Ctype.Size())
		}
		a.markRegisterAsUsedInFunction(l.LeftOperand.Register)
		a.markRegisterAsUsedInFunction(l.LhsSymbol.Register)
		l.LeftOperand.LoadBeforeRead = true
		a.allocAnything([]*AugmentedSymbol{l.RightOperand})
	default:
		a.allocAnything([]*AugmentedSymbol{l.LeftOperand, l.RightOperand})
		l.LhsSymbol.Register = l.LeftOperand.Register
	}
	l.LhsSymbol.StoreAfterWrite = true
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
				l.ReturnSymbol.Register = a.allocFunctionReturnRegister(l.ReturnSymbol)
				l.ReturnSymbol.StoreAfterWrite = true
			}
			a.allocFunctionArgRegisters(l)
			l.FunctionSymbol.Register = a.nextFreeIntegralRegister().use(QWORD)
			l.FunctionSymbol.LoadBeforeRead = true
		case *AugmentedReturnLine:
			if l.ReturnSymbol != nil {
				l.ReturnSymbol.Register = a.allocFunctionReturnRegister(l.ReturnSymbol)
				l.ReturnSymbol.LoadBeforeRead = true
			}
		case *AugmentedBinaryOperationLine:
			a.allocRegistersForBinaryOperation(l)
		default:
			a.allocAnything(l.GetSymbols())
		}
	}
	a.setRegistersToPersistByCallee(fun)
}

func (a *BasicRegisterAllocator) GetFramePointer() IntegralRegister {
	return GetIntegralRegisterFamily(RBP).use(QWORD)
}

func (a *BasicRegisterAllocator) GetStackPointer() IntegralRegister {
	return GetIntegralRegisterFamily(RSP).use(QWORD)
}
