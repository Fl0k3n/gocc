package semantics

import (
	"ast"
	"errors"
	"strings"
	"utils"
)

var COMPARISON_TYPE = BuiltinFrom("int")

type BuiltinGreaterRule struct {
	greater Builtin
	lower Builtin
}

// must define strict partial order in the builtins set
var _BUILTIN_RULES = []BuiltinGreaterRule{
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

func buildTopologicalOrder(G BuiltinRulesGraph, cur Builtin, visited map[Builtin]bool, order []Builtin, lastIdx int) int {
	visited[cur] = true
	if neighs, ok := G[cur]; ok {
		for _, neigh := range neighs.GetAll() {
			if !visited[neigh] {
				lastIdx = buildTopologicalOrder(G, neigh, visited, order, lastIdx)
			}
		}
	}
	order[lastIdx + 1] = cur
	return lastIdx + 1
}

func buildBuiltinRulesGraph() BuiltinRulesGraph {
	G := make(BuiltinRulesGraph)
	for _, rule := range _BUILTIN_RULES {
		if neighs, ok := G[rule.greater]; ok {
			neighs.Add(rule.lower)
		} else {
			G[rule.greater] = utils.SetOf[Builtin](rule.lower)
		}
	}
	visited := map[Builtin]bool{}
	for _, bt := range builtinTypes {
		visited[bt] = false
	}
	topologicalOrder := make([]Builtin, len(builtinTypes))
	idx := -1
	for _, bt := range builtinTypes {
		if !visited[bt] {
			idx = buildTopologicalOrder(G, bt, visited, topologicalOrder, idx)
		}
	}
	for _, v := range topologicalOrder {
		if vNeighs, ok := G[v]; ok {
			initialNeighs := vNeighs.GetAll()
			for _, neigh := range initialNeighs {
				if neighsOfNeigh, ok := G[neigh]; ok {
					vNeighs.AddAll(neighsOfNeigh.GetAll())
				}
			}
		}
	}
	return G
}

type TypeRulesManager struct {
	builtinRulesGraph BuiltinRulesGraph
}

func newTypeRulesManager() *TypeRulesManager {
	return &TypeRulesManager{
		builtinRulesGraph: buildBuiltinRulesGraph(),
	}
}

// exact type match, structs are matched by name, typedefs are not resolved, every array dimension must be equal
func (t *TypeRulesManager) isSame(t1 Ctype, t2 Ctype) bool {
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
			return t.isSame(ct2.Target, ct1.Target)
		}
	case ArrayCtype:
		if ct2, alsoArray := t1.(ArrayCtype); alsoArray {
			if t.isSame(ct2.NestedType, ct1.NestedType) {
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
			if t.isSame(ct2.ReturnType, ct1.ReturnType) {
				if len(ct2.ParamTypes) == len(ct2.ParamTypes) {
					for param := range ct2.ParamTypes {
						if !t.isSame(ct2.ParamTypes[param], ct1.ParamTypes[param]) {
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

func (tm *TypeRulesManager) isIntegralType(t Ctype) bool {
	if b, isBuiltin := t.(BuiltinCtype); isBuiltin {
		return b.Builtin != VOID && b.Builtin != FLOAT && b.Builtin != DOUBLE && b.Builtin != LONG_DOUBLE
	}
	return false
}

func (tm *TypeRulesManager) isFloatingType(t Ctype) bool {
	return isBuiltinType(t) && !isVoid(t) && !tm.isIntegralType(t)
}

func (tm *TypeRulesManager) getBinaryOpType(op string, t1 Ctype, t2 Ctype) (Ctype, error) {
	switch op {
	case "&&", "||":
		if !isBuiltinOrPointer(t1) || !isBuiltinOrPointer(t2) {
			return nil, errors.New("&& and || can be used only on builtins or pointers")
		}
		return BuiltinFrom("int"), nil
	case "|", "^", "&":
		if tm.isIntegralType(t1) && tm.isIntegralType(t2) && t1.Size() == t2.Size() {
			return tm.getGreaterOrEqualType(t1, t2), nil
		}
		return nil, errors.New("Bit operations are allowed only on integral types with equal sizes")
	case "==", "!=", "<=", ">=", ">", "<":
		if !isBuiltinOrPointer(t1) || !isBuiltinOrPointer(t2) {
			return nil, errors.New("Can compare only pointers or builtins")
		}
		if !tm.isAutomaticallyCastable(t1, t2) && !tm.isAutomaticallyCastable(t2, t1) {
			return nil, errors.New("Uncompatible types")
		}
		return COMPARISON_TYPE, nil
	case "<<", ">>":
		if tm.isIntegralType(t1) && tm.isIntegralType(t2) {
			return t1, nil
		}
		return nil, errors.New("Bit shift operations are allowed only on integral types")
	case "+", "-":
		if isPointer(t1) {
			if !tm.isIntegralType(t2) {
				return nil, errors.New("Only integral types can be added/subtracted from pointers")
			}
			return t1, nil
		} else if isPointer(t2) {
			if !tm.isIntegralType(t1) {
				return nil, errors.New("Only integral types can be added/subtracted from pointers")
			}
			return t2, nil
		} else {
			if !tm.isAutomaticallyCastable(t1, t2) && !tm.isAutomaticallyCastable(t2, t1) {
				return nil, errors.New("Uncompatible types")
			}
			return tm.getGreaterOrEqualType(t1, t2), nil
		}
	case "*", "/", "%":
		if !isBuiltinType(t1) || !isBuiltinType(t2) {
			return nil, errors.New("Multiplicative operations apply only to builtins")
		}
		if !tm.isAutomaticallyCastable(t1, t2) && !tm.isAutomaticallyCastable(t2, t1) {
			return nil, errors.New("Uncompatible types")
		}
		return tm.getGreaterOrEqualType(t1, t2), nil
	}
	panic("Unexpected binary operator  " + op)
}

func (tm *TypeRulesManager) getUnaryOpType(op string, castedType Ctype) (Ctype, error) {
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
		// TODO for now leave the same type, casting to int/long/ulong might be a better idea
		return castedType, nil
	}
	panic("unexpected unary operator " + op)
}

func (tm *TypeRulesManager) canBeLValue(t Ctype) bool {
	switch t.(type) {
	case ArrayCtype:
		return false
	default:
		return true
	}
}

func (tm *TypeRulesManager) getAssignmentOpType(lhsType Ctype, rhsType Ctype, op string) (Ctype, error) {
	if !tm.canBeLValue(lhsType) {
		return nil, errors.New("Not a l-value")
	}
	switch op {
	case "=":
		if tm.isAutomaticallyCastable(rhsType, lhsType) {
			return lhsType, nil
		}
		return nil, errors.New("Uncompatible types")
	case "+=", "-=":
		if isPointer(lhsType) {
			if !tm.isIntegralType(rhsType) {
				return nil, errors.New("Only integral types can be added/subtracted from pointers")
			}
			return lhsType, nil
		} 
		if !isBuiltinType(lhsType) || !isBuiltinType(rhsType) {
			return nil, errors.New("Only builtins or pointers can be used in add/sub assignment")
		}
		if !tm.isAutomaticallyCastable(rhsType, lhsType) {
			return nil, errors.New("Uncompatible types")
		}
		return lhsType,nil
	case "&=", "^=", "|=":
		if tm.isIntegralType(lhsType) && tm.isIntegralType(rhsType) && lhsType.Size() == rhsType.Size() {
			return lhsType, nil
		}
		return nil, errors.New("Bit operations are allowed only on integral types with equal sizes")
	case "<<=", ">>=":
		if tm.isIntegralType(lhsType) && tm.isIntegralType(rhsType) {
			return lhsType, nil
		}
		return nil, errors.New("Bit shift operations are allowed only on integral types")
	case "*=", "/=", "%=":
		if !isBuiltinType(lhsType) || !isBuiltinType(rhsType) {
			return nil, errors.New("Only builtins can be used in mul/div assignment")
		}
		if !tm.isAutomaticallyCastable(rhsType, lhsType) {
			return nil, errors.New("Uncompatible types")
		}
		return lhsType, nil
	}
	panic("unexpected assignment operator")
}

func (tm *TypeRulesManager) canBeUsedAsBool(t Ctype) bool {
	switch t.(type) {
	case BuiltinCtype, PointerCtype:
		return true
	default:
		return false
	}
}

func (tm *TypeRulesManager) isAutomaticallyCastable(from Ctype, to Ctype) bool {
	if tm.isSame(from, to) {
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
			if tm.isIntegralType(fromT) {
				return true
			}
		}
	case PointerCtype, ArrayCtype, FunctionPtrCtype:
		switch toT := to.(type) {
		case PointerCtype, ArrayCtype, FunctionPtrCtype:
			return true
		case BuiltinCtype:
			if tm.isIntegralType(toT) {
				return true
			}
		}
	}
	return false
}

func (tm *TypeRulesManager) isExplicitlyCastable(from Ctype, to Ctype) bool {
	if tm.isAutomaticallyCastable(from, to) {
		return true
	}
	return false // TODO
}

// Assumes that one is automatically castable to another, otherwise unspecified behaviour.
// If only one is castable to another the one that can be casted to wins.
// If both are castable to each other custom rules are used, for example double > int.
func (tm *TypeRulesManager) getGreaterOrEqualType(t1 Ctype, t2 Ctype) Ctype {
	if tm.isSame(t1, t2) {
		return t1
	}
	canCastFromT1ToT2 := tm.isAutomaticallyCastable(t1, t2) 
	canCastFromT2ToT1 := tm.isAutomaticallyCastable(t2, t1) 
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
			if n1, ok := tm.builtinRulesGraph[ct1.Builtin]; ok && n1.Has(ct2.Builtin) {
				return t1
			} 
			return t2
		}
	}
	return t1
}

func (tm *TypeRulesManager) getTypeOfConstantValExpression(expr ast.ConstantValExpression) BuiltinCtype {
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

func (tm *TypeRulesManager) canBeIndexedNTimes(t Ctype, n int) bool {
	switch ct := t.(type) {
	case PointerCtype:
		return n == 1
	case ArrayCtype:
		return len(ct.DimensionSizes) == n
	default:
		return false
	}
}

func (tm *TypeRulesManager) canBeIncremented(t Ctype) bool {
	switch ct := t.(type) {
	case PointerCtype:
		return true
	case BuiltinCtype:
		return ct.Builtin != VOID
	default:
		return false
	}
}

func (tm *TypeRulesManager) evalConstantIntegerExpression(expr ast.Expression) (int64, error) {
	var err error
	if valExpr, isValExpr := expr.(ast.ConstantValExpression); isValExpr {
		exprT := tm.getTypeOfConstantValExpression(valExpr)
		if exprT.Builtin == INT {
			return evalIntVal(valExpr.Constant)
		} else if exprT.Builtin == UNSIGNED_INT {
			return evalIntVal(valExpr.Constant[:len(valExpr.Constant) - 1])
		}
		err = errors.New("Expression is not integral const")
	} else if arithmExpr, isArithm := expr.(ast.BinaryArithmeticExpression); isArithm {
		var v1, v2 int64
		if v1, err = tm.evalConstantIntegerExpression(arithmExpr.LhsExpression); err != nil {
			goto onerr
		}
		if v2, err = tm.evalConstantIntegerExpression(arithmExpr.RhsExpression); err != nil {
			goto onerr
		}
		return applyArithmeticOperator(v1, v2, arithmExpr.Operator)
	} else if unaryExpr, isUnary := expr.(ast.CastUnaryExpression); isUnary {
		var v int64
		// TODO should this be allowed?
		if unaryExpr.Operator == "-" {
			if v, err = tm.evalConstantIntegerExpression(unaryExpr.CastExpression); err != nil {
				goto onerr
			}
			return -1 * v, nil
		} else {
			err = errors.New("Unsupported compile time unary operator " + unaryExpr.Operator)
		}
	}
onerr:
	return 0, err
}

func (tm *TypeRulesManager) hasExactlySameOverload(fdef *FunctionDefinition, overloads []*FunctionDefinition) bool {
	for _, overload := range overloads {
		if len(overload.ParamTypes) != len(fdef.ParamTypes) {
			continue
		}
		allSame := true
		for paramNum, param := range overload.ParamTypes {
			if !tm.isSame(param, fdef.ParamTypes[paramNum]) {
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

func (tm *TypeRulesManager) getFunctionOverloadSatisfyingArgs(candidates []*FunctionDefinition, args []Ctype) (*FunctionDefinition, error) {
	feasibleCandidates := []*FunctionDefinition{}
	for _, f := range candidates {
		if len(f.ParamTypes) != len(args) {
			continue
		}
		allParamsOk := true
		allParamsHaveExactMatch := true
		for paramNum, param := range f.ParamTypes {
			if tm.isSame(args[paramNum], param) {
				continue
			}
			allParamsHaveExactMatch = false
			if !tm.isAutomaticallyCastable(args[paramNum], param) {
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
		return nil, errors.New("Multiple overloads match given arguments")
	}
}

func (tm *TypeRulesManager) canBeUsedAsSwitchExpression(t Ctype) bool {
	if b, isBuiltin := t.(BuiltinCtype); isBuiltin {
		return b.Builtin != VOID && b.Builtin != LONG_DOUBLE
	}
	return false
}

// func (tm *TypeRulesManager) getBinaryOpTypecast(leftOp Ctype, rightOp Ctype) BinaryOpTypecast {
// 	if tm.isSame(leftOp, rightOp) {
// 		return BinaryOpTypecast{
// 			LeftRequiresCast: false,
// 			RightRequiresCast: false,
// 		}
// 	}
// 	// TODO long double
// 	switch lt := leftOp.(type) {
// 	case BuiltinCtype:
// 		lftIsIntegral := tm.isIntegralType(lt)
// 		switch rt := rightOp.(type) {
// 		case BuiltinCtype:
// 			rightIsIntegral := tm.isIntegralType(rt)
// 			if lftIsIntegral && rightIsIntegral {

// 			} else if lftIsIntegral && !rightIsIntegral {

// 			} else if !lftIsIntegral && rightIsIntegral {

// 			} else {

// 			}
// 		}

// 	}
// }