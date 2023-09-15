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
	RequiresGotUnwrapping bool
	GotAddressHolder IntegralRegister
	// StoreOnlyInMemory bool
}


type AugmentedGlobalSymbol struct {
	Global *irs.GlobalSymbol
	EncodedInitializerData []byte
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
	ArgsPlacedOnCallerStack []*AugmentedSymbol // same order as in the function definition
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

type AugmentedConstantAssignmentLine struct {
	LhsSymbol *AugmentedSymbol
	Constant semantics.ProgramConstant
}

func (a AugmentedConstantAssignmentLine) GetSymbols() (res []*AugmentedSymbol) {
	return append(res, a.LhsSymbol)
}

type AugmentedStringAssignmentLine struct {
	LhsSymbol *AugmentedSymbol
	Val string
}

func (a AugmentedStringAssignmentLine) GetSymbols() (res []*AugmentedSymbol) {
	return append(res, a.LhsSymbol)
}

type AugmentedBiSymbolAssignmentLine struct {
	LValue *AugmentedLValue
	RhsSymbol *AugmentedSymbol
}

func (a AugmentedBiSymbolAssignmentLine) GetSymbols() (res []*AugmentedSymbol) {
	return append(res, a.LValue.Sym, a.RhsSymbol)
}

type AugmentedBinaryOperationLine struct {
	LhsSymbol *AugmentedSymbol
	LeftOperand *AugmentedSymbol
	Operator string
	RightOperand *AugmentedSymbol	
}

func (a AugmentedBinaryOperationLine) GetSymbols() (res []*AugmentedSymbol) {
	return append(res, a.LhsSymbol, a.LeftOperand, a.RightOperand)
}

type AugmentedUnaryOperationLine struct {
	LhsSymbol *AugmentedSymbol
	Operator string
	Operand *AugmentedSymbol
}

func (a AugmentedUnaryOperationLine) GetSymbols() (res []*AugmentedSymbol) {
	return append(res, a.LhsSymbol, a.Operand)
}

type AugmentedFunctionCallLine struct {
	ReturnSymbol *AugmentedSymbol
	FunctionSymbol *AugmentedSymbol
	Args []*AugmentedSymbol
	ViaRegisterArgs []*AugmentedSymbol
	ViaStackArgs []*AugmentedSymbol	// left to right, should be pushed on stack in reverese order according to SYS V ABI
}


func (a AugmentedFunctionCallLine) GetSymbols() (res []*AugmentedSymbol) {
	if a.ReturnSymbol != nil {
		res = append(res, a.ReturnSymbol)
	}
	res = append(res, a.FunctionSymbol)
	return append(res, a.Args...)
}

type AugmentedLabelLine struct {
	Label string
}

func (a AugmentedLabelLine) GetSymbols() (res []*AugmentedSymbol) {
	return
}

type AugmentedIfGotoLine struct {
	TargetLabel string
	ConditionSymbol *AugmentedSymbol
}

func (a AugmentedIfGotoLine) GetSymbols() (res []*AugmentedSymbol) {
	return append(res, a.ConditionSymbol)
}

type AugmentedGotoLine struct {
	TargetLabel string
}

func (a AugmentedGotoLine) GetSymbols() (res []*AugmentedSymbol) {
	return
}

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
		IsExtern: global.Global.IsExtern,
		IsStatic: global.Global.IsStatic,
		IsFunction: global.Global.IsFunction,
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
		RequiresGotUnwrapping: false,
	}
	if res.isGlobal {
		res.GlobalInfo = g.getGlobalInfo(sym)
	}
	return res
}

func (g *Generator) augmentGlobal(sym *irs.GlobalSymbol) *AugmentedGlobalSymbol {
	if sym == nil {
		return nil
	}
	return &AugmentedGlobalSymbol{
		Global: sym,
		EncodedInitializerData: nil,
	}
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
