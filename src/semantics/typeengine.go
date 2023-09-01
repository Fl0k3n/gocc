package semantics

import (
	"ast"
	"errors"
	"fmt"
	"math"
	"utils"
)

type SymbolTypeProvider interface {
	TypeOf(symname string) (t Ctype, ok bool)
} 

type TypeEngine struct {
	definitionTab map[string]TypeDefinition
	symbolTypeProvider SymbolTypeProvider
	errorTracker *ErrorTracker
	typeRulesManager *TypeRulesManager
}

func NewEngine(symbolTypeProvider SymbolTypeProvider, errorTracker *ErrorTracker) *TypeEngine {
	e := TypeEngine{
		definitionTab: make(map[string]TypeDefinition),
		symbolTypeProvider: symbolTypeProvider,
		errorTracker: errorTracker,
		typeRulesManager: newTypeRulesManager(),
	}
	return &e
}

func (e *TypeEngine) assert(ok bool, err string, line ast.LineInfo) (passed bool) {
	if !ok {
		e.errorTracker.registerTypeError(err, line)
	}
	return ok
}

func (e *TypeEngine) getDefinedType(name string) (Ctype, error) {
	if IsBuiltin(name) {
		return BuiltinFrom(name), nil
	}
	if res, ok := e.definitionTab[name]; ok {
		return res.Ctype, nil
	}
	return VOID_POINTER, errors.New("Undefined type " + name)
}

func (e *TypeEngine) getDefinedTypeFromSpecifier(ts ast.TypeSpecifier) (Ctype, error) {
	switch dts := ts.(type) {
	case ast.DirectTypeSpecifier:
		return e.getDefinedType(dts.TypeName)
	case ast.EnumTypeSpecifier:
		return e.getDefinedType(*dts.Identifier)
	case ast.StructTypeSpecifier:
		return e.getDefinedType(makeStructFullName(*dts.Identifier))
	}
	panic("Unexpected type specifier")
}

func (e *TypeEngine) wrapInAnonymousPointers(ptr *ast.Pointer, target Ctype) PointerCtype {
	// TODO qualifiers
	wrapped := PointerCtype{name: ANONYMOUS, Target: target}
	for ptr.NestedPointer != nil {
		wrapped = PointerCtype{name: ANONYMOUS, Target: wrapped}
		ptr = ptr.NestedPointer
	}
	return wrapped
}

func (e *TypeEngine) getPartialTypeFromSpecifiers(tss []ast.TypeSpecifier) (Ctype, error) {
	if len(tss) > 1 {
		return e.getCombinedBuiltin(tss)
	} else {
		return e.getDefinedTypeFromSpecifier(tss[0])
	}
}

func (e *TypeEngine) extractFunctionParamTypesAndNames(paramList *ast.ParameterTypeList) ([]Ctype, []string) {
	// TODO varargs
	types := make([]Ctype, 0)
	orderedNames := make([]string, 0)
	names := utils.NewSet[string]()
	for _, param := range paramList.ParameterList.ParameterDeclarations {
		// TODO handle qualifiers
		partialType, err := e.getPartialTypeFromSpecifiers(param.DeclarationSpecifiers.TypeSpecifiers)
		if err != nil {
			partialType = VOID_POINTER
			e.errorTracker.registerTypeError(err.Error(), getLineInfo(param.LineInfo))
		}
		paramName := ANONYMOUS
		if param.AbstractDeclarator != nil {
			types = append(types, e.extractType(param.AbstractDeclarator, partialType))
		} else if param.Declarator != nil {
			t, name := e.extractTypeAndName(param.Declarator, partialType)
			types = append(types, t)
			if names.Has(name) {
				e.errorTracker.registerTypeError("Parameter list names must be unique", param.LineInfo)
				paramName = ANONYMOUS
			} else {
				names.Add(name)
				paramName = name
			}
		} else {
			types = append(types, partialType)
		}
		orderedNames = append(orderedNames, paramName)
	}
	return types, orderedNames
}

// concrete variable/struct field oriented, dimension sizes inside arrays required etc
func (e *TypeEngine) extractDirectTypeAndName(ddec ast.DirectDeclarator, declaratorPointer *PointerCtype, lhsType Ctype) (t Ctype, name string) {
	switch directDec := ddec.(type) {
	case ast.DirectIdentifierDeclarator:
		if declaratorPointer != nil {
			t = setPointersLowestLevel(declaratorPointer, lhsType)
		} else {
			t = lhsType
		}
		name = directDec.Identifier
	case ast.Declarator:
		if declaratorPointer != nil {
			lhsType = declaratorPointer.WithTargetOnLowestLevel(lhsType)
		}
		t, name = e.extractTypeAndName(&directDec, lhsType)
	case ast.DirectArrayDeclarator:
		arrSize := 1 // keep it as 1 and continue finding other errors
		if e.assert(directDec.ArrayExpression != nil, "Array size is required", directDec.LineInfo) {
			sz, err := e.typeRulesManager.evalConstantIntegerExpression(directDec.ArrayExpression)
			if e.assert(err == nil && sz > 0 , "Array expression must be a constant positive integer", directDec.LineInfo) {
				if sz > math.MaxInt {
					e.errorTracker.registerTypeError("Array too large, size must fit into 32bits", ast.LineInfo{})
				}
				arrSize = int(sz) // TODO uint
			}
		}
		if declaratorPointer != nil {
			lhsType = setPointersLowestLevel(declaratorPointer, lhsType)
		}
		var arr ArrayCtype
		if lhsArr, isArr := lhsType.(ArrayCtype); isArr {
			newDimensions := []int{arrSize}
			newDimensions = append(newDimensions, lhsArr.DimensionSizes...)
			arr = NewArray(ANONYMOUS, newDimensions, lhsArr.NestedType)
		} else {
			arr = NewArray(ANONYMOUS, []int{arrSize}, lhsType)
		}
		t, name = e.extractDirectTypeAndName(directDec.Declarator, nil, arr)
	case ast.DirectFunctionDeclarator:
		if !e.assert(directDec.IndentifierList == nil, "Expected function parameter, not identifiers", directDec.LineInfo) {
			t = UNKNOWN_OR_PARTIAL
			return
		}
		funcParams := []Ctype{}
		paramNames := []string{}

		if directDec.ParameterTypeList != nil {
			funcParams, paramNames = e.extractFunctionParamTypesAndNames(directDec.ParameterTypeList)
		}
		if declaratorPointer != nil {
			lhsType = setPointersLowestLevel(declaratorPointer, lhsType)
		}
		fptr := FunctionPtrCtype{name: ANONYMOUS, ReturnType: lhsType, ParamTypes: funcParams, ParamNames: paramNames}
		t, name = e.extractDirectTypeAndName(directDec.Declarator, nil, fptr)
		switch t.(type) {
		case PointerCtype, ArrayCtype, FunctionPtrCtype:
		default:
			t = fptr
			e.errorTracker.registerTypeError("Expected function pointer or array of function pointers", directDec.LineInfo)
		}
	}
	return
}

func (e *TypeEngine) extractTypeAndName(dec *ast.Declarator, lhsType Ctype) (t Ctype, name string) {
	var ptr *PointerCtype = nil
	if dec.Pointer != nil {
		p := e.wrapInAnonymousPointers(dec.Pointer, UNKNOWN_OR_PARTIAL)	
		ptr = &p
	}
	return e.extractDirectTypeAndName(dec.DirectDeclarator, ptr, lhsType)
}

func (e *TypeEngine) extractDirectType(ddec ast.DirectAbstractDeclarator, declaratorPointer *PointerCtype, lhsType Ctype) (t Ctype) {
	if ddec == nil {
		if declaratorPointer != nil {
			return setPointersLowestLevel(declaratorPointer, lhsType)
		}
		return lhsType
	}
	switch directDec := ddec.(type) {
	case ast.AbstractDeclarator:
		if declaratorPointer != nil {
			lhsType = declaratorPointer.WithTargetOnLowestLevel(lhsType)
		}
		t = e.extractType(&directDec, lhsType)
	case ast.DirectAbstractArrayDeclarator:
		arrSize := UNSPECIFIED_ARR_SIZE
		if directDec.Expression != nil {
			sz, err := e.typeRulesManager.evalConstantIntegerExpression(directDec.Expression)
			if e.assert(err == nil && sz > 0 , "Array expression must be a constant positive integer", directDec.LineInfo) {
				if sz > math.MaxInt {
					e.errorTracker.registerTypeError("Array too large, size must fit into 32bits", ast.LineInfo{})
				}
				arrSize = int(sz) // TODO uint
			}
		}
		if declaratorPointer != nil {
			lhsType = setPointersLowestLevel(declaratorPointer, lhsType)
		}
		var arr ArrayCtype
		if lhsArr, isArr := lhsType.(ArrayCtype); isArr {
			newDimensions := []int{arrSize}
			newDimensions = append(newDimensions, lhsArr.DimensionSizes...)
			if newDimensions[len(newDimensions) - 2] == UNSPECIFIED_ARR_SIZE {
				newDimensions[len(newDimensions) - 2] = 1 // set it to 1 and continue finding other errors
				e.errorTracker.registerTypeError("multidimensional array must have bounds for all dimensions except the first", directDec.LineInfo)
			}
			arr = NewArray(ANONYMOUS, newDimensions, lhsArr.NestedType)
		} else {
			arr = NewArray(ANONYMOUS, []int{arrSize}, lhsType)
		}
		t = e.extractDirectType(directDec.DirectAbstractDeclarator, nil, arr)
	case ast.DirectAbstractFunctionDeclarator:
		funcParams := []Ctype{}
		paramNames := []string{}
		if directDec.ParameterTypeList != nil {
			funcParams, paramNames = e.extractFunctionParamTypesAndNames(directDec.ParameterTypeList)
		}
		if declaratorPointer != nil {
			lhsType = setPointersLowestLevel(declaratorPointer, lhsType)
		}
		fptr := FunctionPtrCtype{name: ANONYMOUS, ReturnType: lhsType, ParamTypes: funcParams, ParamNames: paramNames}
		t = e.extractDirectType(directDec.DirectAbstractDeclarator, nil, fptr)
		switch t.(type) {
		case PointerCtype, ArrayCtype, FunctionPtrCtype:
		default:
			t = fptr
			e.errorTracker.registerTypeError("Expected function pointer or array of function pointers", directDec.LineInfo)
		}
	}
	return
}

func (e *TypeEngine) extractType(dec *ast.AbstractDeclarator, lhsType Ctype) (t Ctype) {
	var ptr *PointerCtype = nil
	if dec.Pointer != nil {
		p := e.wrapInAnonymousPointers(dec.Pointer, UNKNOWN_OR_PARTIAL)	
		ptr = &p
	}
	if dec.DirectAbstractDeclarator == nil {
		return setPointersLowestLevel(ptr, lhsType)
	}
	return e.extractDirectType(dec.DirectAbstractDeclarator, ptr, lhsType)
}

func (e *TypeEngine) makeStructCtype(ts *ast.StructTypeSpecifier) (StructCtype, error) {
	var name string = ANONYMOUS
	if ts.Identifier != nil {
		name = *ts.Identifier
	}
	fields := make([]Ctype, 0)
	names := make([]string, 0)
	for _, dec := range ts.StructDeclarationList.StructDeclarations {
		e.assert(len(dec.SpecifierQulifierList.TypeSpecifiers) == 1, "Struct field must have single type specifier", dec.LineInfo)
		// TODO handle qualifiers
		partialType, err := e.getPartialTypeFromSpecifiers(dec.SpecifierQulifierList.TypeSpecifiers)
		if err != nil {
			e.errorTracker.registerTypeError(err.Error(), dec.LineInfo)
			partialType = VOID_POINTER
		}
		for _, declaratorsForThatType := range dec.StructDeclaratorList.StructDeclarators {
			e.assert(declaratorsForThatType.Expression == nil, "Bit fields are unsupported atm", dec.LineInfo)
			fieldType, name := e.extractTypeAndName(declaratorsForThatType.Declarator, partialType)
			fields = append(fields, fieldType)
			names = append(names, name)
		}
	}

	return NewStruct(name, fields, names), nil
}

func (e *TypeEngine) defineType(t Ctype, name string, definitionLine ast.LineInfo) {
	if prevDef, alreadyDefined := e.definitionTab[name]; alreadyDefined {
		if t.Name() != name {
			e.errorTracker.registerTypeError(fmt.Sprintf("Redefinition of type %s, previously defined at %d",
										t.Name(), prevDef.DeclarationLine), definitionLine)
		}
	} else {
		e.definitionTab[name] = TypeDefinition{Ctype: t, DeclarationLine: definitionLine.LineNumber}
	}
}

func (e *TypeEngine) getCombinedBuiltin(tss []ast.TypeSpecifier) (Ctype, error) {
	strRepr := ""
	for _, ts := range tss {
		if directTs, isDirect := ts.(ast.DirectTypeSpecifier); !isDirect {
			e.errorTracker.registerTypeError("Illegal declaration", directTs.LineInfo)
			return VOID_POINTER, errors.New("Invalid combined builtin")
		} else {
			strRepr += " " + directTs.TypeName
		}
	}
	if !IsBuiltin(strRepr) {
		e.errorTracker.registerTypeError("Undefined type", tss[0].(ast.DirectTypeSpecifier).LineInfo)
		return VOID_POINTER, errors.New("Invalid combined builtin")
	}
	return BuiltinFrom(strRepr), nil
}

func (e *TypeEngine) createSimpleType(ds *ast.DeclarationSpecifiers, allowStorageClassSpecifiers bool) Ctype {
	if !allowStorageClassSpecifiers {
		e.assert(len(ds.StorageClassSpecifiers) == 0, "Storage specifier is illegal in this context", ds.LineInfo)
		ds.StorageClassSpecifiers = make([]*ast.StorageClassSpecifier, 0)
	}
	e.assert(len(ds.StorageClassSpecifiers) <= 1, "Only single storage class specifier is allowed", ds.LineInfo)
	if !e.assert(len(ds.TypeSpecifiers) > 0, "Illegal declaration", ds.LineInfo) {
		return VOID_POINTER
	}

	if len(ds.TypeSpecifiers) > 1 {
		res, err := e.getCombinedBuiltin(ds.TypeSpecifiers)
		if err != nil {
			e.errorTracker.registerTypeError(err.Error(), ds.LineInfo)
			res = VOID_POINTER
		}
		return res
	}

	typespec := ds.TypeSpecifiers[0]

	switch dts := typespec.(type) {
	case ast.DirectTypeSpecifier:
		if IsBuiltin(dts.TypeName) {
			return BuiltinFrom(dts.TypeName)
		} else {
			if res, err := e.getDefinedType(dts.TypeName); err != nil {
				e.errorTracker.registerTypeError("Undefined type " + dts.TypeName, dts.LineInfo)
				return VOID_POINTER
			} else {
				return res
			}
		}
	case ast.StructTypeSpecifier:
		if structType, err := e.makeStructCtype(&dts); err != nil {
			return VOID_POINTER
		} else {
			if structType.Name() != ANONYMOUS {
				e.defineType(structType, makeStructFullName(structType.Name()), dts.LineInfo)
			}
			return structType
		}
	case ast.EnumTypeSpecifier:
		return BuiltinFrom("int") // TODO
	}
	panic("Unexpected Type Specifier")
}

func (e *TypeEngine) GetSymbolsForTopLevelDeclarationAndDefineNewTypes(dec *ast.Declaration) []Symbol {
	symbols := []Symbol{}
	ds := dec.DeclarationSpecifiers
	e.assert(len(ds.StorageClassSpecifiers) <= 1, "Only single storage class specifier is allowed", ds.LineInfo)
	if !e.assert(len(ds.TypeSpecifiers) > 0, "Illegal declaration", ds.LineInfo) {
		return symbols
	}
	
	hasTypedef := false
	if len(ds.StorageClassSpecifiers) > 0 && ds.StorageClassSpecifiers[0].Val == string(TYPEDEF) {
		hasTypedef = true
	}

	if dec.InitDeclaratorList == nil {
		e.createSimpleType(dec.DeclarationSpecifiers, true)
		return symbols
	} 

	partialType, err := e.getPartialTypeFromSpecifiers(ds.TypeSpecifiers)
	partialTypeExtractionFailed := false
	if err != nil {
		if sts, isStruct := ds.TypeSpecifiers[0].(ast.StructTypeSpecifier); isStruct && sts.StructDeclarationList != nil {
			partialType = e.createSimpleType(ds, false)
		} else {
			e.errorTracker.registerTypeError(err.Error(), ds.LineInfo)
			partialTypeExtractionFailed = true
			partialType = VOID_POINTER
		}
	}
	for _, initDec := range dec.InitDeclaratorList.InitDeclarators {
		t, name := e.extractTypeAndName(initDec.Declarator, partialType)
		if partialTypeExtractionFailed {
			t = VOID_POINTER
		}
		if hasTypedef {
			e.defineType(t, name, initDec.LineInfo);
		} else {
			symbols = append(symbols, Symbol{Name: name, Type: t, LineInfo: initDec.LineInfo})
		}
		if initDec.Initializer != nil {
			if hasTypedef {
				e.errorTracker.registerTypeError("Initialization is invalid after typedef", initDec.LineInfo)
			} else if !partialTypeExtractionFailed {
				e.typeCheckInitializer(t, initDec.Initializer)
			}
		}
	}
	return symbols
}

func (e *TypeEngine) typeCheckInitializer(targetType Ctype, initializer *ast.Initializer) error {
	if structType, isStruct := targetType.(StructCtype); isStruct {
		if initializer.Expression != nil {
			rhsT, err  := e.getTypeOfExpression(initializer.Expression)
			if err != nil {
				return err
			}
			if !e.typeRulesManager.isSame(targetType, rhsT) {
				return errors.New("Struct can be assigned only to struct of the same type")
			}
		} else {
			fieldTypes, fieldNames, err := e.getInitializerMetaForStruct(initializer)
			if err != nil {
				return err
			}
			if err := e.compareStructTypeWithInitializer(structType, fieldTypes, fieldNames); err != nil {
				return err 
			}
		}
	} else {
		expr := initializer.Expression
		if _, isAssignment := expr.(ast.AssignmentExpression); isAssignment {
			return errors.New("Illegal assignment expression in initializer")
		}
		if initializerType, err := e.getTypeOfExpression(expr); err != nil {
			if !e.typeRulesManager.isAutomaticallyCastable(initializerType, targetType) {
				return errors.New("Invalid initializer type")
			}
		}
	}
	return nil
}

func (e *TypeEngine) getGreaterOrEqualTypeIfCompatible(e1 ast.Expression, e2 ast.Expression) (t Ctype, err error) {
	t1, err := e.getTypeOfExpression(e1)
	if err != nil {
		return nil, err
	}
	t2, err := e.getTypeOfExpression(e2)
	if err != nil {
		return nil, err
	}
	if e.typeRulesManager.isAutomaticallyCastable(t1, t2) || e.typeRulesManager.isAutomaticallyCastable(t2, t1) {
		return e.typeRulesManager.getGreaterOrEqualType(t1, t2), nil
	} else {
		return nil, errors.New("Uncompatible types")
	}
}

func (e *TypeEngine) isLValue(expression ast.Expression, wrappedInPointer bool) bool {
	switch expr := expression.(type) {
	case ast.IdentifierExpression:
		// TODO we should check if its not constant, e.g. global function
		return true
	case ast.CastUnaryExpression:
		if expr.Operator == "*" {
			nestedT, err := e.getTypeOfExpression(expr.CastExpression)
			if err != nil {
				return false
			}
			return e.isLValue(expr.CastExpression, true) && e.IsPointer(nestedT)
		}
		return false
	case ast.ArrayAccessPostfixExpression:
		return true
	case ast.StructAccessPostfixExpression:
		return true
	case ast.BinaryArithmeticExpression:
		return wrappedInPointer // upper level will check if this resolves to pointer
	}
	return false
}

func (e *TypeEngine) getTypeOfExpression(expression ast.Expression) (Ctype, error) {
	switch expr := expression.(type) {
	case ast.IdentifierExpression:
		if t, ok := e.symbolTypeProvider.TypeOf(expr.Identifier); ok {
			return t, nil
		} else {
			return nil, errors.New("Undefined identifier " + expr.Identifier)
		}
	case ast.ConstantValExpression:
		t := e.typeRulesManager.getTypeOfConstantValExpression(expr)
		var err error
		if e.typeRulesManager.isIntegralType(t) {
			_, err = evalIntVal(expr.Constant)
		} else {
			_, err = evalFloatVal(expr.Constant)
		}
		return t, err
	case ast.StringLiteralExpression:
		return STRING_LITERAL_TYPE, nil
	case ast.ArrayAccessPostfixExpression:
		dimensions := 0
		curExpr := expr
		for {
			arrayAccessorT, err := e.getTypeOfExpression(curExpr.ArrayExpression)
			if err != nil {
				return nil, err
			}
			if !e.typeRulesManager.isIntegralType(arrayAccessorT) {
				return nil, errors.New("Array accessor must have integral type")
			}
			dimensions++
			if nestedExpr, isArrayAccess := curExpr.PostfixExpression.(ast.ArrayAccessPostfixExpression); isArrayAccess {
				curExpr = nestedExpr
			} else {
				break
			}
		}
		arrayT, err := e.getTypeOfExpression(curExpr.PostfixExpression) 
		if err != nil {
			return nil, err
		}
		if !e.typeRulesManager.canBeIndexedNTimes(arrayT, dimensions) {
			return nil, errors.New("Illegal number of array accesses")
		}
		switch concreteT := arrayT.(type) {
		case PointerCtype:
			return concreteT.Target, nil
		case ArrayCtype:
			return concreteT.NestedType, nil
		default:
			panic("Unexpected type")
		}
	case ast.FunctionCallPostfixExpression:
		argTypes := []Ctype{}
		if expr.Args != nil {
			for _, arg := range expr.Args.Expressions {
				if t, err := e.getTypeOfExpression(arg); err != nil {
					return nil, err
				} else {
					argTypes = append(argTypes, t)
				}
			}
		}
		if t, err := e.getTypeOfExpression(expr.FunctionAccessor); err != nil {
			return nil, err
		} else {
			if f, isFunc := t.(FunctionPtrCtype); isFunc {
				if len(f.ParamTypes) != len(argTypes) {
					return nil, errors.New("Invalid number of arguments")
				}
				for paramNum, paramType := range f.ParamTypes {
					if !e.typeRulesManager.isAutomaticallyCastable(argTypes[paramNum], paramType) {
						return nil, errors.New(fmt.Sprintf("Uncompatible parameter %d", paramNum))
					}
				}
				return f.ReturnType, nil
			} else {
				return nil, errors.New("Type is not callable")
			}
		}
	case ast.StructAccessPostfixExpression:
		t, err := e.getTypeOfExpression(expr.StructAccessor)
		if err != nil {
			return nil, err
		}
		var maybeStructT Ctype
		if expr.AccessMethod == ast.POINTER_ACCESS {
			if ptrToStruct, isPtr := t.(PointerCtype); isPtr {
				maybeStructT = ptrToStruct.Target
			} else {
				return nil, errors.New("Pointer field access valid only for pointers to structs")
			}
		} else {
			maybeStructT = t
		}
		if structType, isStruct := maybeStructT.(StructCtype); isStruct {
			fieldType, _ ,err := structType.MaybeField(expr.FieldIdentifier)
			return fieldType, err
		} else {
			return nil, errors.New("Field access is permitted only for structs")
		}
	case ast.IncDecPostfixExpression:
		if t, err := e.getTypeOfExpression(expr.PostfixExpression); err != nil {
			return nil, err
		} else {
			if e.typeRulesManager.canBeIncremented(t) && e.isLValue(expr.PostfixExpression, false) {
				return t, nil
			}
			return nil, errors.New("Type can't be incremented or decremented")
		}
	case ast.IncDecUnaryExpression:
		if t, err := e.getTypeOfExpression(expr.UnaryExpression); err != nil {
			return nil, err
		} else {
			if e.typeRulesManager.canBeIncremented(t) && e.isLValue(expr.UnaryExpression, false) {
				return t, nil
			}
			return nil, errors.New("Type can't be incremented or decremented")
		}
	case ast.CastUnaryExpression:
		if castedType, err := e.getTypeOfExpression(expr.CastExpression); err != nil {
			return nil, err
		} else {
			return e.typeRulesManager.getUnaryOpType(expr.Operator, castedType)
		}
	case ast.SizeofUnaryExpression:
		return BuiltinFrom("unsigned long"), nil
	case ast.TypeCastCastExpression:
		if len(expr.Typename.SpecifierQulifierList.TypeQualifiers) > 0 {
			return nil, errors.New("Illegal type casting with type qualifiers")
		}
		if partialType, err := e.getPartialTypeFromSpecifiers(expr.Typename.SpecifierQulifierList.TypeSpecifiers); err != nil {
			return nil, err
		} else {
			resType := e.extractType(expr.Typename.AbstractDeclarator, partialType)
			castedType, err := e.getTypeOfExpression(expr.Expression)
			if err != nil {
				return nil, err
			}
			if e.typeRulesManager.isExplicitlyCastable(castedType, resType) {
				return castedType, nil
			} else {
				return nil, errors.New("Invalid type cast")
			}
		}
	case ast.BinaryArithmeticExpression:
		t1, err := e.getTypeOfExpression(expr.LhsExpression)
		if err != nil {
			return nil, err
		}
		t2, err := e.getTypeOfExpression(expr.RhsExpression)
		if err != nil {
			return nil, err
		}
		return e.typeRulesManager.getBinaryOpType(expr.Operator, t1, t2)
	case ast.ConditionalExpression:
		conditionType, err := e.getTypeOfExpression(expr.Condition)
		if err != nil {
			return nil, err
		}
		if !e.typeRulesManager.canBeUsedAsBool(conditionType) {
			return nil, errors.New("Type of condition must be bool'ish")
		}
		return e.getGreaterOrEqualTypeIfCompatible(expr.IfTrueExpression, expr.ElseExpression)
	case ast.AssignmentExpression:
		if !e.isLValue(expr.LhsExpression, false) {
			return nil, errors.New("Left hand side of assignment must be a L-value")
		}
		lhsType, err := e.getTypeOfExpression(expr.LhsExpression)
		if err != nil {
			return nil, err
		}
		rhsType, err := e.getTypeOfExpression(expr.RhsExpression)
		if err != nil {
			return nil, err
		}	
		return e.typeRulesManager.getAssignmentOpType(lhsType, rhsType, expr.Operator)
	}
	panic("unexpected expression type")
}

func (e *TypeEngine) getInitializerMetaForStruct(initializer *ast.Initializer) (fieldTypes []Ctype, fieldNames []string, err error) {
	if initializer.InitializerList == nil {
		err = errors.New("Expected struct initializer")
		return
	}

	foundNamed := false
	for _, ini := range initializer.InitializerList.Initializers {
		if ini.InitializerList != nil {
			err = errors.New("Nested struct initializers are illegal")
			return
		}
		expr := ini.Expression
		if ae, isAssignment := expr.(ast.AssignmentExpression); isAssignment {
			if ae.Operator != "=" {
				err = errors.New("Illegal assignment operator for struct initializer, expected =")
				return
			}
			foundNamed = true
			if fieldName, er := extractStructFieldInitializerIdentifierName(&ae); er != nil {
				err = er
				return
			} else {
				fieldNames = append(fieldNames, fieldName)
				if t, typeErr := e.getTypeOfExpression(ae.RhsExpression); typeErr != nil {
					err = typeErr
					return
				} else {
					fieldTypes = append(fieldTypes, t)
				}
			}
		} else {
			if foundNamed {
				err = errors.New("Either all fields must be named or none")
				return
			}
			fieldNames = append(fieldNames, ANONYMOUS)
			if t, er := e.getTypeOfExpression(expr); er != nil {
				err = er
				return
			} else {
				fieldTypes = append(fieldTypes, t)
			}
		}
	}
	return
}

func (e *TypeEngine) GetFunctionDeclaration(fun *ast.FunctionDefinition) FunctionPtrCtype {
	if fun.DeclarationSpecifiers == nil {
		panic("didnt get declaration specifiers")
	}
	// TODO qualifiers
	e.assert(len(fun.DeclarationSpecifiers.StorageClassSpecifiers) == 0,
			"Function definition can't have storage specifiers", fun.LineInfo)
	baseRetType, err := e.getPartialTypeFromSpecifiers(fun.DeclarationSpecifiers.TypeSpecifiers)
	if err != nil {
		baseRetType = VOID_POINTER
		e.errorTracker.registerTypeError(err.Error(), fun.LineInfo)
	}
	t, name := e.extractTypeAndName(fun.Declarator, baseRetType)
	funPtr := t.(FunctionPtrCtype)
	funPtr.name = name
	return funPtr
}

func (e *TypeEngine) compareStructTypeWithInitializer(structType StructCtype, fieldTypes []Ctype, fieldNames []string) (err error) {
	if len(fieldTypes) > len(structType.NestedFieldTypes) {
		return errors.New("Too many initializer fields")
	}
	for idx, fieldType := range fieldTypes {
		name := fieldNames[idx]
		if name == ANONYMOUS {
			if !e.typeRulesManager.isAutomaticallyCastable(fieldType, structType.NestedFieldTypes[idx]) {
				return errors.New("Invalid initializer type")
			}
		} else {
			if sfieldType, _, err  := structType.MaybeField(name); err != nil {
				return errors.New("No field named " + name)
			} else if !e.typeRulesManager.isAutomaticallyCastable(fieldType, sfieldType) {
				return errors.New("Invalid initializer type")
			}
		}
	}
	return nil
}

func (e *TypeEngine) checkCondition(expr ast.Expression) error {
	if condType, err := e.getTypeOfExpression(expr); err != nil {
		return err
	} else if !e.typeRulesManager.canBeUsedAsBool(condType) {
		return errors.New("Condition doesn't have bool-ish value")
	}
	return nil
}

func (e *TypeEngine) checkSwitchExpressionType(t Ctype) error {
	if !e.typeRulesManager.canBeUsedAsSwitchExpression(t) {
		return errors.New("Invalid switch expression type")
	}
	return nil
}

func (e *TypeEngine) checkCaseExpression(expr ast.Expression, expectedType Ctype) error {
	if exprT, err := e.getTypeOfExpression(expr); err != nil {
		return err
	} else if !e.typeRulesManager.isAutomaticallyCastable(exprT, expectedType) {
		return errors.New("Invalid type of case expression")
	}
	return nil
}

func (e *TypeEngine) checkReturnExpression(expr ast.Expression, expectedType Ctype) error {
	if expr != nil {
		if exprT, err := e.getTypeOfExpression(expr); err != nil {
			return err
		} else if !e.typeRulesManager.isAutomaticallyCastable(exprT, expectedType) {
			return errors.New("Uncompatible return type")
		}
	} else if !isVoid(expectedType) {
		return errors.New("Expected return value")
	}
	return nil
}

func (e *TypeEngine) GetNestedType(arrOrPtrT Ctype) Ctype {
	if arr, isArr := arrOrPtrT.(ArrayCtype); isArr {
		return arr.NestedType
	} else {
		return arrOrPtrT.(PointerCtype).Target
	}
}

func (e *TypeEngine) GetTypeOfExpression(expr ast.Expression) Ctype {
	if t, err := e.getTypeOfExpression(expr); err != nil {
		panic(err)
	} else {
		return t
	}
}

func (e *TypeEngine) ConvertToCtype(tn *ast.TypeName) Ctype {
	if partialType, err := e.getPartialTypeFromSpecifiers(tn.SpecifierQulifierList.TypeSpecifiers); err != nil {
		panic(err)
	} else {
		return e.extractType(tn.AbstractDeclarator, partialType)
	}
}

func (e *TypeEngine) GetDeclaredSymbols(dec *ast.Declaration) []*SymbolDeclaration {
	partialType, err := e.getPartialTypeFromSpecifiers(dec.DeclarationSpecifiers.TypeSpecifiers)
	if err != nil {
		panic(err)
	}
	res := []*SymbolDeclaration{}
	for _, initDecl := range dec.InitDeclaratorList.InitDeclarators {
		t, name := e.extractTypeAndName(initDecl.Declarator, partialType)
		res = append(res, &SymbolDeclaration{
			Name: name,
			T: t,
			Initializer: initDecl.Initializer,
		})
	}
	return res
}

func (e *TypeEngine) GetDeclaredGlobals(dec *ast.Declaration) []*GlobalDeclaration {
	partialType, err := e.getPartialTypeFromSpecifiers(dec.DeclarationSpecifiers.TypeSpecifiers)
	if err != nil {
		panic(err)
	}
	res := []*GlobalDeclaration{}
	ds := dec.DeclarationSpecifiers
	isExtern := false
	isStatic := false
	if len(ds.StorageClassSpecifiers) > 0 {
		switch ds.StorageClassSpecifiers[0].Val {
		case string(TYPEDEF):
			return res
		case string(EXTERN):
			isExtern = true
		case string(STATIC):
			isStatic = true
		}
	}
	for _, initDecl := range dec.InitDeclaratorList.InitDeclarators {
		t, name := e.extractTypeAndName(initDecl.Declarator, partialType)
		res = append(res, &GlobalDeclaration{
			Name: name,
			T: t,
			Initializer: initDecl.Initializer,
			Extern: isExtern,
			Static: isStatic,
		})
	}
	return res
}

func (e *TypeEngine) GetStructFieldInitializers(structT StructCtype, initializer *ast.Initializer) (fieldNames []string, rhsExpressions []ast.Expression){
	for _, ini := range initializer.InitializerList.Initializers {
		expr := ini.Expression
		if ae, isAssignment := expr.(ast.AssignmentExpression); isAssignment {
			if fieldName, er := extractStructFieldInitializerIdentifierName(&ae); er != nil {
				panic(er)
			} else {
				fieldNames = append(fieldNames, fieldName)
				rhsExpressions = append(rhsExpressions, ae.RhsExpression)
			}
		} else {
			fieldNames = append(fieldNames, structT.NestedFieldNames[len(fieldNames)])
		}
	}
	return
}

func (e *TypeEngine) GetRequiredTypeCastForBinaryArithmeticOperation(leftOperand Ctype, rightOperand Ctype) BinaryOpTypecast {
	if e.typeRulesManager.isSame(leftOperand, rightOperand) {
		return BinaryOpTypecast{
			LeftRequiresCast: false,
			RightRequiresCast: false,
		}
	}
	greaterT := e.typeRulesManager.getGreaterOrEqualType(leftOperand, rightOperand)
	if e.typeRulesManager.isSame(leftOperand, greaterT) {
		return BinaryOpTypecast{
			LeftRequiresCast: false,
			RightRequiresCast: true,
			RightTargetType: greaterT,
		}
	} else {
		return BinaryOpTypecast{
			LeftRequiresCast: true,
			RightRequiresCast: false,
			LeftTargetType: greaterT,
		}
	}
}

func (e *TypeEngine) GetConstantInfo(expr ast.ConstantValExpression) ProgramConstant {
	exprT := e.GetTypeOfExpression(expr)
	if e.typeRulesManager.isIntegralType(exprT) {
		v, _ := evalIntVal(expr.Constant)
		return IntegralConstant{
			T: exprT,
			Val: v,
		}
	} else {
		v, _ := evalFloatVal(expr.Constant)
		return FloatingConstant{
			T: exprT,
			Val: v,
		}
	}
}

func (e *TypeEngine) GetAssignmentCastInfo(lhs Ctype, rhs Ctype) (castTo Ctype, requiresCast bool) {
	if e.typeRulesManager.isSame(lhs, rhs) {
		return nil, false
	}
	return lhs, true
}

func (e *TypeEngine) ReturnsVoid(fun FunctionPtrCtype) bool {
	return isVoid(fun.ReturnType)
}

func (e *TypeEngine) IsIntegralType(t Ctype) bool {
	return e.typeRulesManager.isIntegralType(t)
}

func (e *TypeEngine) IsFloatingType(t Ctype) bool {
	return e.typeRulesManager.isFloatingType(t)
}

func (e *TypeEngine) IsPointer(t Ctype) bool {
	return isPointer(t) // TODO arrays without first dim
}

func (e *TypeEngine) WrapInPointer(t Ctype) PointerCtype {
	return PointerCtype{Target: t}
}
