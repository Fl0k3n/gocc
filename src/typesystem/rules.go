package types

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
