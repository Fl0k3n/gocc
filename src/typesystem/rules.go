package types

import (
	"ast"
	"errors"
	"strings"
	"utils"
)

type BuiltinGreaterRule struct {
	greater Builtin
	lower Builtin
}

// must define strict partial order in builtins set
var builtinRules = []BuiltinGreaterRule{
	{LONG_DOUBLE, DOUBLE},
	{DOUBLE, FLOAT},
	{FLOAT, LONG},
	{LONG, UNSIGNED_LONG},
	{LONG, SIGNED_LONG},
	{SIGNED_LONG, UNSIGNED_LONG},
	{UNSIGNED_LONG, INT},
	{INT, SIGNED_INT},
	{SIGNED_INT, SIGNED},
	{SIGNED, UNSIGNED_INT},
	{UNSIGNED_INT, UNSIGNED},
	{UNSIGNED, SHORT},
	{SHORT, CHAR},
}

// graph containing transitive closure of the relation above
// if there exits an edge from node1 to node2 then node1 > node2 wrt the relation above
type BuiltinRulesGraph map[Builtin]*utils.Set[Builtin]

var builtinRulesGraph BuiltinRulesGraph // TODO wrap all in some struct and dont use this as global

func initBuiltinRulesGraph() {
	G := make(BuiltinRulesGraph)
	edgeQueue := utils.NewQueue[BuiltinGreaterRule]()
	for _, br := range builtinRules {
		edgeQueue.Push(br)
	}
	for edgeQueue.Size() > 0 {
		curEdge := edgeQueue.Pop()
		if edges, ok := G[curEdge.greater]; ok {
			if edges.Has(curEdge.lower) {
				continue
			} else {
				edges.Add(curEdge.lower)
			}
		} else {
			G[curEdge.greater] = utils.SetOf[Builtin](curEdge.lower)
		}

		if neighEdges, ok := G[curEdge.lower]; ok {
			for _, lowerNode := range neighEdges.GetAll() {
				edgeQueue.Push(BuiltinGreaterRule{curEdge.greater, lowerNode})
			}
		}
	}
	builtinRulesGraph = G
}

// exact type match, structs are matched by name, typedefs are not resolved, every array dimension must be equal
func isSame(t1 Ctype, t2 Ctype) bool {
	switch ct1 := t2.(type) {
	case StructCtype:
		if ct2, alsoStruct := t1.(StructCtype); alsoStruct {
			return ct1.name == ct2.name
		} 
	case BuiltinCtype:
		if ct2, alsoBuiltin := t1.(BuiltinCtype); alsoBuiltin {
			return ct1.Name() == ct2.Name() 
		}
	case PointerCtype:
		if ct2, alsoPointer := t1.(PointerCtype); alsoPointer {
			return isSame(ct2.Target, ct1.Target)
		}
	case ArrayCtype:
		if ct2, alsoArray := t1.(ArrayCtype); alsoArray {
			if isSame(ct2.NestedType, ct1.NestedType) {
				if len(ct1.DimensionSizes) == len(ct2.DimensionSizes) {
					for dim := range ct1.DimensionSizes {
						if ct1.DimensionSizes[dim] != ct2.DimensionSizes[dim] {
							return false
						}
					}
					return true
				}
			}
		}
	case FunctionPtrCtype:
		if ct2, alsoFPtr := t1.(FunctionPtrCtype); alsoFPtr {
			if isSame(ct2.ReturnType, ct1.ReturnType) {
				if len(ct2.ParamTypes) == len(ct2.ParamTypes) {
					for param := range ct2.ParamTypes {
						if !isSame(ct2.ParamTypes[param], ct1.ParamTypes[param]) {
							return false
						}
					}
					return true
				}
			}
		}
	}
	return false
}

func isIntegralType(t Ctype) bool {
	if b, isBuiltin := t.(BuiltinCtype); isBuiltin {
		return b.Builtin != VOID && b.Builtin != FLOAT && b.Builtin != DOUBLE && b.Builtin != LONG_DOUBLE
	}
	return false
}

func getBinaryOpType(op string, t1 Ctype, t2 Ctype) (Ctype, error) {
	switch op {
	case "AND_OP", "OR_OP":
		if !isBuiltinOrPointer(t1) || !isBuiltinOrPointer(t2) {
			return nil, errors.New("&& and || can be used only on builtins or pointers")
		}
		return BuiltinFrom("int"), nil
	case "|", "^", "&":
		if isIntegralType(t1) && isIntegralType(t2) && t1.Size() == t2.Size() {
			return getGreaterOrEqualType(t1, t2), nil
		}
		return nil, errors.New("Bit operations are allowed only on integral types with equal sizes")
	case "EQ_OP", "NE_OP", "LE_OP", "GE_OP", ">", "<":
		if !isBuiltinOrPointer(t1) || !isBuiltinOrPointer(t2) {
			return nil, errors.New("Can compare only pointers or builtins")
		}
		if !isAutomaticallyCastable(t1, t2) && !isAutomaticallyCastable(t2, t1) {
			return nil, errors.New("Uncompatible types")
		}
		return BuiltinFrom("int"), nil
	case "LEFT_OP", "RIGHT_OP":
		if isIntegralType(t1) && isIntegralType(t2) {
			return t1, nil
		}
		return nil, errors.New("Bit shift operations are allowed only on integral types")
	case "+", "-":
		if isPointer(t1) {
			if !isIntegralType(t2) {
				return nil, errors.New("Only integral types can be added/subtracted from pointers")
			}
			return t1, nil
		} else if isPointer(t2) {
			if !isIntegralType(t1) {
				return nil, errors.New("Only integral types can be added/subtracted from pointers")
			}
			return t2, nil
		} else {
			if !isAutomaticallyCastable(t1, t2) && !isAutomaticallyCastable(t2, t1) {
				return nil, errors.New("Uncompatible types")
			}
			return getGreaterOrEqualType(t1, t2), nil
		}
	case "*", "/", "%":
		if !isBuiltinType(t1) || !isBuiltinType(t2) {
			return nil, errors.New("Multiplicative operations apply only to builtins")
		}
		if !isAutomaticallyCastable(t1, t2) && !isAutomaticallyCastable(t2, t1) {
			return nil, errors.New("Uncompatible types")
		}
		return getGreaterOrEqualType(t1, t2), nil
	}
	panic("Unexpected binary operator  " + op)
}

func getUnaryOpType(op string, castedType Ctype) (Ctype, error) {
	switch op {
	case "&":
		return BuiltinFrom("unsigned long"), nil
	case "*":
		switch ct := castedType.(type) {
		case PointerCtype:
			return ct.Target, nil
		case ArrayCtype:
			return ct.NestedType, nil
		default:
			return nil, errors.New("Type can't be dereferenced")
		}
	case "+":
		switch ct := castedType.(type) {
		case BuiltinCtype:
			if ct.Builtin != VOID {
				return ct, nil	
			} else {
				return nil, errors.New("Unary + doesn't apply to void")
			}
		default:
			return nil, errors.New("Unary + applies only to base types")
		}
	case "-":
		switch ct := castedType.(type) {
		case BuiltinCtype:
			if ct.Builtin != VOID && ct.Builtin != CHAR && ct.Builtin != UNSIGNED &&
					ct.Builtin != UNSIGNED_INT && ct.Builtin != UNSIGNED_LONG {
				return ct, nil	
			} else {
				return nil, errors.New("Unary - doesn't apply to usigned types")
			}
		default:
			return nil, errors.New("Unary - applies only to base types")
		}
	case "~":
		switch ct := castedType.(type) {
		case BuiltinCtype:
			if ct.Builtin != VOID {
				return ct, nil	
			} else {
				return nil, errors.New("Unary ~ doesn't apply to void")
			}
		default:
			return nil, errors.New("Unary ~ applies only to base types")
		}
	case "!":
		if bt, isBuiltin := castedType.(BuiltinCtype); isBuiltin {
			if bt.Builtin == VOID {
				return nil, errors.New("Unary ! doesn't apply to void")
			}	
		}
		if _, isStruct := castedType.(StructCtype); isStruct {
			return nil, errors.New("Unary ! doesn't apply to structs")
		}
		// TODO for now leave the same type, casting to int/long/ulong might be better idea
		return castedType, nil
	}
	panic("unexpected unary operator " + op)
}

func canBeLValue(t Ctype) bool {
	switch t.(type) {
	case ArrayCtype:
		return false
	default:
		return true
	}
}

func getAssignmentOpType(lhsType Ctype, rhsType Ctype, op string) (Ctype, error) {
	if !canBeLValue(lhsType) {
		return nil, errors.New("Not a l-value")
	}
	switch op {
	case "=":
		if isAutomaticallyCastable(rhsType, lhsType) {
			return lhsType, nil
		}
		return nil, errors.New("Uncompatible types")
	case "ADD_ASSIGN", "SUB_ASSIGN":
		if isPointer(lhsType) {
			if !isIntegralType(rhsType) {
				return nil, errors.New("Only integral types can be added/subtracted from pointers")
			}
			return lhsType, nil
		} 
		if !isBuiltinType(lhsType) || !isBuiltinType(rhsType) {
			return nil, errors.New("Only builtins or pointers can be used in add/sub assignment")
		}
		if !isAutomaticallyCastable(rhsType, lhsType) {
			return nil, errors.New("Uncompatible types")
		}
		return lhsType,nil
	case "AND_ASSIGN", "XOR_ASSIGN", "OR_ASSIGN":
		if isIntegralType(lhsType) && isIntegralType(rhsType) && lhsType.Size() == rhsType.Size() {
			return lhsType, nil
		}
		return nil, errors.New("Bit operations are allowed only on integral types with equal sizes")
	case "LEFT_ASSIGHT", "RIGHT_ASSIGN":
		if isIntegralType(lhsType) && isIntegralType(rhsType) {
			return lhsType, nil
		}
		return nil, errors.New("Bit shift operations are allowed only on integral types")
	case "MUL_ASSIGN", "DIV_ASSIGN", "MOD_ASSIGN":
		if !isBuiltinType(lhsType) || !isBuiltinType(rhsType) {
			return nil, errors.New("Only builtins can be used in mul/div assignment")
		}
		if !isAutomaticallyCastable(rhsType, lhsType) {
			return nil, errors.New("Uncompatible types")
		}
		return lhsType, nil
	}
	panic("unexpected assignment operator")
}

func canBeUsedAsBool(t Ctype) bool {
	switch t.(type) {
	case BuiltinCtype, PointerCtype:
		return true
	default:
		return false
	}
}

func isAutomaticallyCastable(from Ctype, to Ctype) bool {
	if isSame(from, to) {
		return true
	}
	switch fromT := from.(type) {
	case BuiltinCtype:
		switch toT := to.(type) {
		case BuiltinCtype:
			if fromT.Builtin == VOID || toT.Builtin == VOID {
				if fromT.Builtin == VOID && toT.Builtin == VOID {
					return true
				}
				return false
			}
			return true
		case PointerCtype, ArrayCtype, FunctionPtrCtype:
			if isIntegralType(fromT) {
				return true
			}
		}
	case PointerCtype, ArrayCtype, FunctionPtrCtype:
		switch toT := to.(type) {
		case PointerCtype, ArrayCtype, FunctionPtrCtype:
			return true
		case BuiltinCtype:
			if isIntegralType(toT) {
				return true
			}
		}
	}
	return false
}

func isExplicitlyCastable(from Ctype, to Ctype) bool {
	if isAutomaticallyCastable(from, to) {
		return true
	}
	return false // TODO
}

// Assumes that one is automatically castable to another, otherwise unspecified behaviour.
// If only one is castable to another the one that can be casted to wins.
// If both are castable to each other custom rules are used, for example double > int.
func getGreaterOrEqualType(t1 Ctype, t2 Ctype) Ctype {
	if isSame(t1, t2) {
		return t1
	}
	canCastFromT1ToT2 := isAutomaticallyCastable(t1, t2) 
	canCastFromT2ToT1 := isAutomaticallyCastable(t2,t1) 
	if canCastFromT1ToT2 && !canCastFromT2ToT1 {
		return t1
	} else if canCastFromT2ToT1 && !canCastFromT1ToT2 {
		return t2
	} else if !canCastFromT1ToT2 && !canCastFromT2ToT1 {
		panic("Uncompatible types")
	}

	switch ct1 := t1.(type) {
	case BuiltinCtype:
		switch ct2 := t2.(type) {
		case BuiltinCtype:
			if n1, ok := builtinRulesGraph[ct1.Builtin]; ok && n1.Has(ct2.Builtin) {
				return t1
			} 
			return t2
		}
	}
	return t1
}

func getTypeOfConstantValExpression(expr ast.ConstantValExpression) BuiltinCtype {
	val := expr.Constant
	if strings.HasPrefix(val, "'") {
		return BuiltinFrom("char")
	}
	lastChar := val[len(val) - 1]
	if lastChar == 'f' || lastChar == 'F' {
		return BuiltinFrom("float")
	}
	if strings.Contains(val, ".") {
		return BuiltinFrom("double")
	}
	if lastChar == 'l' || lastChar == 'L' {
		prevToLast := val[len(val) - 2]
		if prevToLast == 'u' || prevToLast == 'U' {
			return BuiltinFrom("unsigned long")
		}
		return BuiltinFrom("long")
	}
	if lastChar == 'u' || lastChar == 'U' {
		return BuiltinFrom("unsigned int")
	}
	return BuiltinFrom("int")
}

func canBeIndexedNTimes(t Ctype, n int) bool {
	switch ct := t.(type) {
	case PointerCtype:
		return n == 1
	case ArrayCtype:
		return len(ct.DimensionSizes) == n
	default:
		return false
	}
}

func canBeIncremented(t Ctype) bool {
	switch ct := t.(type) {
	case PointerCtype:
		return true
	case BuiltinCtype:
		return ct.Builtin != VOID
	default:
		return false
	}
}

func hasExactlySameOverload(fdef *FunctionDefinition, overloads []*FunctionDefinition) bool {
	for _, overload := range overloads {
		if len(overload.ParamTypes) != len(fdef.ParamTypes) {
			continue
		}
		allSame := true
		for paramNum, param := range overload.ParamTypes {
			if !isSame(param, fdef.ParamTypes[paramNum]) {
				allSame = false
				break
			}
		}
		if allSame {
			return true
		}
	}
	return false
}

func getFunctionOverloadSatisfyingArgs(candidates []*FunctionDefinition, args []Ctype) (*FunctionDefinition, error) {
	feasibleCandidates := []*FunctionDefinition{}
	for _, f := range candidates {
		if len(f.ParamTypes) != len(args) {
			continue
		}
		allParamsOk := true
		allParamsHaveExactMatch := true
		for paramNum, param := range f.ParamTypes {
			if isSame(args[paramNum], param) {
				continue
			}
			allParamsHaveExactMatch = false
			if !isAutomaticallyCastable(args[paramNum], param) {
				allParamsOk = false
				break
			}
		}
		if allParamsHaveExactMatch {
			return f, nil
		}
		if allParamsOk {
			feasibleCandidates = append(feasibleCandidates, f)
		}
	}

	if len(feasibleCandidates) == 1 {
		return feasibleCandidates[0], nil
	} else if len(feasibleCandidates) == 0 {
		return nil, errors.New("No overload matching given arguments")
	} else {
		return nil, errors.New("Multiple ovearloads match given arguments")
	}
}
