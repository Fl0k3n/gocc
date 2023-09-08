package codegen

import (
	"fmt"
	"irs"
	"semantics"
)

type GlobalSymbolInfo struct {
	IsExtern bool
	IsStatic bool
	IsFunction bool
}

type AugmentedSymbol struct {
	Sym *irs.Symbol
	Identity string
	MemoryAccessor MemoryAccessor
	Register Register
	LoadBeforeRead bool
	StoreAfterWrite bool
	isGlobal bool
	GlobalInfo *GlobalSymbolInfo
	// StoreOnlyInMemory bool
}

type RegisterWithAccessor struct {
	Register Register
	MemoryAccessor MemoryAccessor
}

type AugmentedFunctionIr struct {
	FunctionSymbol *irs.GlobalSymbol
	Code []AugmentedIRLine
	Snapshot *irs.NonGlobalsSnapshot
	ReturnLabel string
	Args []*AugmentedSymbol
	InRegisterArgsToPlaceOnCalleeStack []*irs.Symbol // args for which memory should be allocated
	InRegisterArgsToStoreAfterFunctionEnter []*AugmentedSymbol // subset of the ones above that should be stored in that memory before any other code is executed
	ArgsPlacedOnCallerStack []*AugmentedSymbol
	IntegralRegistersToPersist []*RegisterWithAccessor
	FloatingRegistersToPersist []*RegisterWithAccessor
}

type AugmentedLValue struct {
	IsDereferenced bool
	Sym *AugmentedSymbol
}

type AugmentedIRLine interface {
	GetSymbols() []*AugmentedSymbol
}

// if int
// mov [lhs], #constant 
// if float or long
// memory allocator shall provide address of float/long constant
// mov tmpReg, [addr of float] # tmpReg integral eitherway
// mov [lhs], tmpReg
type AugmentedConstantAssignmentLine struct {
	LhsSymbol *AugmentedSymbol
	Constant semantics.ProgramConstant
}

func (a AugmentedConstantAssignmentLine) GetSymbols() (res []*AugmentedSymbol) {
	return append(res, a.LhsSymbol)
}


// memory allocator shall provide address of string
// mov lhs, addr_of_string
type AugmentedStringAssignmentLine struct {
	LhsSymbol *AugmentedSymbol
	Val string
}

func (a AugmentedStringAssignmentLine) GetSymbols() (res []*AugmentedSymbol) {
	return append(res, a.LhsSymbol)
}

// depends on type and size but for 64bit ints:
// mov tmpReg, [addr of rhs]
// if not dereferenced:
// mov [addr of lval], tmpReg 
// else:
// mov tmpReg2, [addr of lval]
// mov [tmpReg2], tmpReg
type AugmentedBiSymbolAssignmentLine struct {
	LValue *AugmentedLValue
	RhsSymbol *AugmentedSymbol
}

func (a AugmentedBiSymbolAssignmentLine) GetSymbols() (res []*AugmentedSymbol) {
	return append(res, a.LValue.Sym, a.RhsSymbol)
}

// mov tmpReg1, [leftOp]
// mov tmpReg2, [rightOp]
// e.g. addition:
// add tmpReg1, tmpReg2
// handle lvalue as above then: mov [addr of lval], tmpReg1
type AugmentedBinaryOperationLine struct {
	LhsSymbol *AugmentedSymbol
	LeftOperand *AugmentedSymbol
	Operator string
	RightOperand *AugmentedSymbol	
}

func (a AugmentedBinaryOperationLine) GetSymbols() (res []*AugmentedSymbol) {
	return append(res, a.LhsSymbol, a.LeftOperand, a.RightOperand)
}

// for example dereference
// mov tmpReg, [operand]
// mov [lhs], tmpReg
// for example bitwise not
// mov tmpReg, [opearnd]
// not tmpReg
// mov [operand], tmpReg
type AugmentedUnaryOperationLine struct {
	LhsSymbol *AugmentedSymbol
	Operator string
	Operand *AugmentedSymbol
}

func (a AugmentedUnaryOperationLine) GetSymbols() (res []*AugmentedSymbol) {
	return append(res, a.LhsSymbol, a.Operand)
}

// classify args
// for each reg arg mov reg, args[n]
// for each mem: push (from right to left) args[n]
// mov tmpReg, [functionSymbol]
// call tmpReg
// classify return mode
// if void dont do anything
// if reg: mov [returnSym], reg
// else: ...
type AugmentedFunctionCallLine struct {
	ReturnSymbol *AugmentedSymbol
	FunctionSymbol *AugmentedSymbol
	Args []*AugmentedSymbol
	ViaRegisterArgs []*AugmentedSymbol
	ViaStackArgs []*AugmentedSymbol	// left to right same order as the order in which they should be pushed on stack
}


func (a AugmentedFunctionCallLine) GetSymbols() (res []*AugmentedSymbol) {
	return append(append(res, a.ReturnSymbol, a.FunctionSymbol), a.Args...)
}

// write it as it is
type AugmentedLabelLine struct {
	Label string
}

func (a AugmentedLabelLine) GetSymbols() (res []*AugmentedSymbol) {
	return
}

// mov tempReg, [conditionSymbol]
// cmp tempReg, 0
// jz targetLabel
type AugmentedIfGotoLine struct {
	TargetLabel string
	ConditionSymbol *AugmentedSymbol
}

func (a AugmentedIfGotoLine) GetSymbols() (res []*AugmentedSymbol) {
	return append(res, a.ConditionSymbol)
}

// jmp targetlabel
type AugmentedGotoLine struct {
	TargetLabel string
}

func (a AugmentedGotoLine) GetSymbols() (res []*AugmentedSymbol) {
	return
}

// first make function epilogue, fix stack pointer, restore registers if needed
// if no return value: ret
// else:
// check how it should be returned
// if reg:
// mov tmpReg, [retSym]
// ret
type AugmentedReturnLine struct {
	ReturnSymbol *AugmentedSymbol
}


func (a AugmentedReturnLine) GetSymbols() (res []*AugmentedSymbol) {
	if a.ReturnSymbol != nil {
		res = append(res, a.ReturnSymbol)
	}
	return
}

type AugmentedTypeCastLine struct {
	FromSymbol *AugmentedSymbol
	ToSymbol *AugmentedSymbol	
}

func (a AugmentedTypeCastLine) GetSymbols() (res []*AugmentedSymbol) {
	return append(res, a.FromSymbol, a.ToSymbol)
}

func (g *Generator) getGlobalInfo(sym *irs.Symbol) *GlobalSymbolInfo {
	global := g.globals[sym.Index]
	return &GlobalSymbolInfo{
		IsExtern: global.IsExtern,
		IsStatic: global.IsStatic,
		IsFunction: global.IsFunction,
	}
}

func (g *Generator) augmentSymbol(sym *irs.Symbol) *AugmentedSymbol {
	if sym == nil {
		return nil
	}
	res := &AugmentedSymbol{
		Sym: sym,
		Identity: fmt.Sprintf("%d_%d", int(sym.T), sym.Index),
		isGlobal: sym.T == irs.GLOBAL,
	}
	if res.isGlobal {
		res.GlobalInfo = g.getGlobalInfo(sym)
	}
	return res
}

func (g *Generator) prepareAugmentedIr(fun *irs.FunctionIR) *AugmentedFunctionIr {
	res := []AugmentedIRLine{}
	for _, line := range fun.Code {
		var aline AugmentedIRLine
		switch ir := line.(type) {
		case *irs.BiSymbolAssignmentLine:
			aline = &AugmentedBiSymbolAssignmentLine{
				LValue: &AugmentedLValue{IsDereferenced: ir.LValue.IsDereferenced, Sym: g.augmentSymbol(ir.LValue.Sym)},
				RhsSymbol: g.augmentSymbol(ir.RhsSymbol),
			}
		case *irs.BinaryOperationLine:
			aline = &AugmentedBinaryOperationLine{
				LhsSymbol: g.augmentSymbol(ir.LhsSymbol),
				LeftOperand: g.augmentSymbol(ir.LeftOperand),
				Operator: ir.Operator,
				RightOperand: g.augmentSymbol(ir.RightOperand),
			}
		case *irs.ConstantAssignmentLine:
			aline = &AugmentedConstantAssignmentLine{
				LhsSymbol: g.augmentSymbol(ir.LhsSymbol),
				Constant: ir.Constant,
			}
		case *irs.FunctionCallLine:
			args := []*AugmentedSymbol{}
			for _, arg := range ir.Args {
				args = append(args, g.augmentSymbol(arg))
			}
			aline = &AugmentedFunctionCallLine{
				ReturnSymbol: g.augmentSymbol(ir.ReturnSymbol),
				FunctionSymbol: g.augmentSymbol(ir.FunctionSymbol),
				Args: args,
				ViaRegisterArgs: []*AugmentedSymbol{},
				ViaStackArgs: []*AugmentedSymbol{},
			}
		case *irs.GotoLine:
			aline = &AugmentedGotoLine{
				TargetLabel: ir.TargetLabel,
			}
		case *irs.IfGotoLine:
			aline = &AugmentedIfGotoLine{
				TargetLabel: ir.TargetLabel,
				ConditionSymbol: g.augmentSymbol(ir.ConditionSymbol),
			}
		case *irs.LabelLine:
			aline = &AugmentedLabelLine{
				Label: ir.Label,
			}
		case *irs.ReturnLine:
			aline = &AugmentedReturnLine{
				ReturnSymbol: g.augmentSymbol(ir.ReturnSymbol),
			}
		case *irs.StringAssignmentLine:
			aline = &AugmentedStringAssignmentLine{
				LhsSymbol: g.augmentSymbol(ir.LhsSymbol),
				Val: ir.Val,
			}
		case *irs.TypeCastLine:
			aline = &AugmentedTypeCastLine{
				FromSymbol: g.augmentSymbol(ir.FromSymbol),
				ToSymbol: g.augmentSymbol(ir.ToSymbol),
			}
		case *irs.UnaryOperationLine:
			aline = &AugmentedUnaryOperationLine{
				LhsSymbol: g.augmentSymbol(ir.LhsSymbol),
				Operator: ir.Operator,
				Operand: g.augmentSymbol(ir.Operand),
			}
		default:
			panic("unexpected ir line")
		}
		res = append(res, aline)
	}
	args := make([]*AugmentedSymbol, len(fun.Snapshot.ArgsSnapshot))
	for argNum, arg := range fun.Snapshot.ArgsSnapshot {
		args[argNum] = g.augmentSymbol(arg)
	}
	return &AugmentedFunctionIr{
		FunctionSymbol: fun.FunctionSymbol,
		Code: res,
		Snapshot: fun.Snapshot,
		ReturnLabel: createFunctionReturnLabel(fun.FunctionSymbol.Symbol.Name),
		Args: args,
		InRegisterArgsToPlaceOnCalleeStack: []*irs.Symbol{},
		ArgsPlacedOnCallerStack: []*AugmentedSymbol{},
		IntegralRegistersToPersist: []*RegisterWithAccessor{},
		FloatingRegistersToPersist: []*RegisterWithAccessor{},
		InRegisterArgsToStoreAfterFunctionEnter: []*AugmentedSymbol{},
	}
}
