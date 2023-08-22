package types

import (
	"ast"
	"errors"
	"strings"
)

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
			return isSame(ct2, ct1)
		}
	case ArrayCtype:
		if ct2, alsoArray := t1.(ArrayCtype); alsoArray {
			if isSame(ct2, ct1) {
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

func getArithmOpTypeDeterminedByOperator(op string) (t Ctype, isDetermined bool) {
	if op == "==" || op == "!=" || op == ">" || op == ">=" || op == "<" || op == "<=" {
		return BuiltinFrom("int"), true
	}
	return nil, false
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
			return nil, errors.New("Unary - applies only to base types")
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

func isAutomaticallyCastable(from Ctype, to Ctype) bool {
	if isSame(from, to) {
		return true
	}
	return true
}


// assumes that one is automatically castable to another, otherwise unspecified behaviour.
// if both are castable to each other custom rules are used, for example double > int.
// if only one is castable to another the one that can be casted to wins.
func getGreaterOrEqualType(t0 Ctype, t1 Ctype) Ctype {
	return t0
}

func isExplicitlyCastable(from Ctype, to Ctype) bool {
	return false
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
