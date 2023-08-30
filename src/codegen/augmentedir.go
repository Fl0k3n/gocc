package codegen

import (
	"irs"
	"semantics"
)

type StorageClass int


const (
	INTEGER StorageClass = iota
	FLOATING // corresponds to SSE System V ABI class
	MEMORY
)

type AugmentedSymbol struct {
	Sym *irs.Symbol
	Identity string
	Size int
	StorageClass StorageClass
	MemoryAccessor string
	Register Register
}

type AugmentedFunctionIr struct {
	Name string
	Code []AugmentedIRLine
}

type AugmentedLValue struct {
	IsDereferenced bool
	Sym *AugmentedSymbol
}

type AugmentedIRLine interface {}

type AugmentedConstantAssignmentLine struct {
	LhsSymbol *AugmentedSymbol
	Constant semantics.ProgramConstant
}

type AugmentedStringAssignmentLine struct {
	LhsSymbol *AugmentedSymbol
	Val string
}

type AugmentedBiSymbolAssignmentLine struct {
	LValue *AugmentedLValue
	RhsSymbol *AugmentedSymbol
}

type AugmentedBinaryOperationLine struct {
	LhsSymbol *AugmentedSymbol
	LeftOperand *AugmentedSymbol
	Operator string
	RightOperand *AugmentedSymbol	
}

type AugmentedUnaryOperationLine struct {
	LhsSymbol *AugmentedSymbol
	Operator string
	Operand *AugmentedSymbol
}

type AugmentedFunctionCallLine struct {
	ReturnSymbol *AugmentedSymbol
	FunctionSymbol *AugmentedSymbol
	Args []*AugmentedSymbol
}

type AugmentedLabelLine struct {
	Label string
}

type AugmentedIfGotoLine struct {
	TargetLabel string
	ConditionSymbol *AugmentedSymbol
}

type AugmentedGotoLine struct {
	TargetLabel string
}
type AugmentedReturnLine struct {
	ReturnSymbol *AugmentedSymbol
}

type AugmentedTypeCastLine struct {
	FromSymbol *AugmentedSymbol
	ToSymbol *AugmentedSymbol	
}

func (g *Generator) prepareAugmentedIr(fun *irs.FunctionIR) *AugmentedFunctionIr {
	res := []AugmentedIRLine{}
	for _, line := range fun.Code {
		var aline AugmentedIRLine
		switch ir := line.(type) {
		case *irs.BiSymbolAssignmentLine:
			aline = AugmentedBiSymbolAssignmentLine{
				LValue: &AugmentedLValue{IsDereferenced: ir.LValue.IsDereferenced, Sym: g.augmentSymbol(ir.LValue.Sym)},
				RhsSymbol: g.augmentSymbol(ir.RhsSymbol),
			}
		case *irs.BinaryOperationLine:
			aline = AugmentedBinaryOperationLine{
				LhsSymbol: g.augmentSymbol(ir.LhsSymbol),
				LeftOperand: g.augmentSymbol(ir.LeftOperand),
				Operator: ir.Operator,
				RightOperand: g.augmentSymbol(ir.RightOperand),
			}
		case *irs.ConstantAssignmentLine:
			aline = AugmentedConstantAssignmentLine{
				LhsSymbol: g.augmentSymbol(ir.LhsSymbol),
				Constant: ir.Constant,
			}
		case *irs.FunctionCallLine:
			args := []*AugmentedSymbol{}
			for _, arg := range ir.Args {
				args = append(args, g.augmentSymbol(arg))
			}
			aline = AugmentedFunctionCallLine{
				ReturnSymbol: g.augmentSymbol(ir.ReturnSymbol),
				FunctionSymbol: g.augmentSymbol(ir.FunctionSymbol),
				Args: args,
			}
		case *irs.GotoLine:
			aline = AugmentedGotoLine{
				TargetLabel: ir.TargetLabel,
			}
		case *irs.IfGotoLine:
			aline = AugmentedIfGotoLine{
				TargetLabel: ir.TargetLabel,
				ConditionSymbol: g.augmentSymbol(ir.ConditionSymbol),
			}
		case *irs.LabelLine:
			aline = AugmentedLabelLine{
				Label: ir.Label,
			}
		case *irs.ReturnLine:
			aline = AugmentedReturnLine{
				ReturnSymbol: g.augmentSymbol(ir.ReturnSymbol),
			}
		case *irs.StringAssignmentLine:
			aline = AugmentedStringAssignmentLine{
				LhsSymbol: g.augmentSymbol(ir.LhsSymbol),
				Val: ir.Val,
			}
		case *irs.TypeCastLine:
			aline = AugmentedTypeCastLine{
				FromSymbol: g.augmentSymbol(ir.FromSymbol),
				ToSymbol: g.augmentSymbol(ir.ToSymbol),
			}
		case *irs.UnaryOperationLine:
			aline = AugmentedUnaryOperationLine{
				LhsSymbol: g.augmentSymbol(ir.LhsSymbol),
				Operator: ir.Operator,
				Operand: g.augmentSymbol(ir.Operand),
			}
		default:
			panic("unexpected ir line")
		}
		res = append(res, aline)
	}
	return &AugmentedFunctionIr{
		Name: fun.Name,
		Code: res,
	}
}
