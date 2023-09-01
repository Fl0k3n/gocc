package codegen

import (
	"utils"
)

type RegisterAllocator interface {
	Alloc(fun *AugmentedFunctionIr)
	GetFramePointer() IntegralRegister
	GetStackPointer() IntegralRegister
}

// should be similar to the one that gcc uses with -O0
type BasicRegisterAllocator struct {
	memoryManager *MemoryManager
	generalPurposeIntegralRegisters *utils.Set[IntegralRegisterFamilyT]
	currentlyUsedIntegralRegisters *utils.Set[IntegralRegisterFamilyT]
	generalPurposeFloatingRegisters *utils.Set[FloatingRegisterFamilyT]
	currentlyUsedFloatingRegisters *utils.Set[FloatingRegisterFamilyT]
}

func NewBasicAllocator(memoryManager *MemoryManager) *BasicRegisterAllocator {
	return &BasicRegisterAllocator{
		memoryManager: memoryManager,
		generalPurposeIntegralRegisters: utils.SetOf[IntegralRegisterFamilyT](
			SYSV_GENERAL_PURPOSE_INTEGRAL_REGISTERS...
		),
		currentlyUsedIntegralRegisters: utils.NewSet[IntegralRegisterFamilyT](),
		generalPurposeFloatingRegisters: utils.SetOf[FloatingRegisterFamilyT](
			SYSV_GENERAL_PURPOSE_FLOATING_REGISTERS...
		),
		currentlyUsedFloatingRegisters: utils.NewSet[FloatingRegisterFamilyT](),
	}
}

func (a *BasicRegisterAllocator) freeAllGeneralPurposeRegisters() {
	a.currentlyUsedFloatingRegisters = utils.NewSet[FloatingRegisterFamilyT]()
	a.currentlyUsedIntegralRegisters = utils.NewSet[IntegralRegisterFamilyT]()
}

func (a *BasicRegisterAllocator) nextFreeIntegralRegister() IntegralRegisterFamily {
	for _, reg := range a.generalPurposeIntegralRegisters.GetAll() {
		if !a.currentlyUsedIntegralRegisters.Has(reg) {
			a.currentlyUsedIntegralRegisters.Add(reg)
			return GetIntegralRegisterFamily(reg)
		}
	}
	panic("out of integral registers") // this should never happen for this naive allocator
}

func (a *BasicRegisterAllocator) nextFreeFloatingRegister() FloatingRegisterFamily {
	for _, reg := range a.generalPurposeFloatingRegisters.GetAll() {
		if !a.currentlyUsedFloatingRegisters.Has(reg) {
			a.currentlyUsedFloatingRegisters.Add(reg)
			return GetFloatingRegisterFamily(reg)
		}
	}
	panic("out of floating registers") // this should never happen for this naive allocator
}

func (a *BasicRegisterAllocator) allocFunctionReturnRegister(returnSymbol *AugmentedSymbol) Register {
	if returnSymbol == nil {
		return nil
	}
	switch a.memoryManager.classifySymbol(returnSymbol) {
	case INTEGER:
		reg := GetIntegralRegisterFamily(SYS_V_RETURN_INTEGRAL_REGISTER).useForSize(returnSymbol.Sym.Ctype.Size())
		a.currentlyUsedIntegralRegisters.Add(SYS_V_RETURN_INTEGRAL_REGISTER)
		return reg
	case SSE:
		reg := GetFloatingRegisterFamily(SYS_V_RETURN_FLOATING_REGISTER).use()
		a.currentlyUsedFloatingRegisters.Add(SYS_V_RETURN_FLOATING_REGISTER)
		return reg
	default:
		panic("TODO")
	}
}

func (a *BasicRegisterAllocator) allocFunctionArgRegisters(call *AugmentedFunctionCallLine) {
	integralRegArgs := []*AugmentedSymbol{}
	floatingRegArgs := []*AugmentedSymbol{}
	stackArgs := utils.NewStack[*AugmentedSymbol]()
	var stackMoverIntegralReg *IntegralRegisterFamily = nil
	var stackMoverFloatingReg *FloatingRegisterFamily = nil

	for _, arg := range call.Args {
		t := arg.Sym.Ctype
		switch a.memoryManager.classifySymbol(arg) {
		case INTEGER:
			if len(integralRegArgs) >= len(SYSV_FUNCTION_ARG_INTEGRAL_REGISTERS) {
				if stackMoverIntegralReg == nil {
					x := a.nextFreeIntegralRegister()
					stackMoverIntegralReg = &x
				}
				arg.Register = stackMoverIntegralReg.useForSize(t.Size())
				stackArgs.Push(arg)
			} else {
				regFamT := SYSV_FUNCTION_ARG_INTEGRAL_REGISTERS[len(integralRegArgs)]
				arg.Register = GetIntegralRegisterFamily(regFamT).useForSize(t.Size())
				a.currentlyUsedIntegralRegisters.Add(regFamT)
				integralRegArgs = append(integralRegArgs, arg)
			}
		case SSE:
			if len(floatingRegArgs) >= len(SYSV_FUNCTION_ARG_FLOATING_REGISTERS) {
				if stackMoverFloatingReg == nil {
					x := a.nextFreeFloatingRegister()
					stackMoverFloatingReg = &x
				}
				arg.Register = stackMoverFloatingReg.use()
				stackArgs.Push(arg)
			} else {
				regFamT := SYSV_CALLEE_SAVE_FLOATING_REGISTERS[len(floatingRegArgs)]
				arg.Register = GetFloatingRegisterFamily(regFamT).use()
				a.currentlyUsedFloatingRegisters.Add(regFamT)
				floatingRegArgs = append(floatingRegArgs, arg)
			}
		default:
			panic("TODO")
		}
		arg.StoreAfterWrite = false
		arg.LoadBeforeRead = true
	}
	call.ViaRegisterArgs = append(call.ViaRegisterArgs, integralRegArgs...)
	call.ViaRegisterArgs = append(call.ViaRegisterArgs, floatingRegArgs...)
	for stackArgs.Size() > 0 {
		call.ViaStackArgs = append(call.ViaStackArgs, stackArgs.Pop())
	}
}

func (a *BasicRegisterAllocator) allocAnything(symbols []*AugmentedSymbol) {
	for _, asym := range symbols {
		asym.LoadBeforeRead = true
		asym.StoreAfterWrite = true	
		switch a.memoryManager.classifySymbol(asym) {
		case INTEGER:
			asym.Register = a.nextFreeIntegralRegister().useForSize(asym.Sym.Ctype.Size())
		case SSE:
			asym.Register = a.nextFreeFloatingRegister().use()
		case MEMORY, SPLIT:
			panic("TODO")
		}
	}	
}

func (a *BasicRegisterAllocator) Alloc(fun *AugmentedFunctionIr) {
	for _, line := range fun.Code {
		a.freeAllGeneralPurposeRegisters()
		switch l := line.(type) {
		case *AugmentedFunctionCallLine:
			l.ReturnSymbol.Register = a.allocFunctionReturnRegister(l.ReturnSymbol)
			a.allocFunctionArgRegisters(l)
			l.FunctionSymbol.Register = a.nextFreeIntegralRegister().use(QWORD)
			l.FunctionSymbol.StoreAfterWrite = true
		case *AugmentedReturnLine:
			l.ReturnSymbol.Register = a.allocFunctionReturnRegister(l.ReturnSymbol)
			l.ReturnSymbol.LoadBeforeRead = true
		case *AugmentedBinaryOperationLine:
			// TODO floats
			if l.Operator == "/" || l.Operator == "%" {
				a.currentlyUsedIntegralRegisters.Add(DIV_OP_DIVIDENT_HIGHER_BITS_REG)
				a.currentlyUsedIntegralRegisters.Add(DIV_OP_DIVIDENT_LOWER_BITS_REG)
				// this needs special handling with clearing the higher bits or zero extending lower etc
				l.LeftOperand.Register = GetIntegralRegisterFamily(DIV_OP_DIVIDENT_LOWER_BITS_REG).useForSize(l.LeftOperand.Sym.Ctype.Size())
				if l.Operator == "/" {
					l.LhsSymbol.Register = GetIntegralRegisterFamily(DIV_OP_DIVIDENT_LOWER_BITS_REG).useForSize(l.LeftOperand.Sym.Ctype.Size())
				} else {
					l.LhsSymbol.Register = GetIntegralRegisterFamily(DIV_OP_DIVIDENT_HIGHER_BITS_REG).useForSize(l.LeftOperand.Sym.Ctype.Size())
				}
				l.LhsSymbol.StoreAfterWrite = true
				l.LeftOperand.LoadBeforeRead = true
				a.allocAnything([]*AugmentedSymbol{l.RightOperand})
			} else {
				a.allocAnything(l.GetSymbols())
			}
		default:
			a.allocAnything(l.GetSymbols())
		}
	}
}

func (a *BasicRegisterAllocator) GetFramePointer() IntegralRegister {
	return GetIntegralRegisterFamily(RBP).use(QWORD)
}

func (a *BasicRegisterAllocator) GetStackPointer() IntegralRegister {
	return GetIntegralRegisterFamily(RSP).use(QWORD)
}
