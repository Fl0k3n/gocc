package types

import (
	"ast"
	"errors"
	"fmt"
	"symtabs"
	"utils"
)

type Engine struct {
	definitionTab map[string]TypeDefinition
	symtab *symtabs.Symtab[Symbol]
	funcdefs map[string][]*FunctionDefinition
	typeErrors []error
	funcGotoLabels *utils.Set[string]
	funcGotosToCheck *utils.Set[string]
}

func NewEngine() *Engine {
	initBuiltinRulesGraph()
	e := Engine{
		definitionTab: make(map[string]TypeDefinition),
		symtab: symtabs.NewSymtab[Symbol](),
		funcdefs: map[string][]*FunctionDefinition{},
	}
	e.enterGlobalScope()
	return &e
}

func (e *Engine) registerTypeError(err string, line ast.LineInfo) {
		e.typeErrors = append(e.typeErrors, errors.New(err))
}

func (e *Engine) assert(ok bool, err string, line ast.LineInfo) (passed bool) {
	if !ok {
		e.registerTypeError(err, line)
	}
	return ok
}

func (e *Engine) getDefinedType(name string) (Ctype, error) {
	if IsBuiltin(name) {
		return BuiltinFrom(name), nil
	}
	if res, ok := e.definitionTab[name]; ok {
		return res.Ctype, nil
	}
	return VOID_POINTER, errors.New("Undefined type " + name)
}

func (e *Engine) getDefinedTypeFromSpecifier(ts ast.TypeSpecifier) (Ctype, error) {
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

func (e *Engine) wrapInAnonymousPointers(ptr *ast.Pointer, target Ctype) PointerCtype {
	// TODO qualifiers
	wrapped := PointerCtype{name: ANONYMOUS, Target: target}
	for ptr.NestedPointer != nil {
		wrapped = PointerCtype{name: ANONYMOUS, Target: wrapped}
		ptr = ptr.NestedPointer
	}
	return wrapped
}

func (e *Engine) getPartialTypeFromSpecifiers(tss []ast.TypeSpecifier) (Ctype, error) {
	if len(tss) > 1 {
		return e.getCombinedBuiltin(tss)
	} else {
		return e.getDefinedTypeFromSpecifier(tss[0])
	}
}

func (e *Engine) extractFunctionParamTypesAndNames(paramList *ast.ParameterTypeList) ([]Ctype, []string) {
	// TODO varargs
	types := make([]Ctype, 0)
	orderedNames := make([]string, 0)
	names := utils.NewSet[string]()
	for _, param := range paramList.ParameterList.ParameterDeclarations {
		// TODO handle qualifiers
		partialType, err := e.getPartialTypeFromSpecifiers(param.DeclarationSpecifiers.TypeSpecifiers)
		if err != nil {
			partialType = VOID_POINTER
			e.registerTypeError(err.Error(), getLineInfo(param.LineInfo))
		}
		paramName := ANONYMOUS
		if param.AbstractDeclarator != nil {
			types = append(types, e.extractType(param.AbstractDeclarator, partialType))
		} else if param.Declarator != nil {
			t, name := e.extractTypeAndName(param.Declarator, partialType)
			types = append(types, t)
			if names.Has(name) {
				e.registerTypeError("Parameter list names must be unique", param.LineInfo)
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
func (e *Engine) extractDirectTypeAndName(ddec ast.DirectDeclarator, declaratorPointer *PointerCtype, lhsType Ctype) (t Ctype, name string) {
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
			sz, err := evalConstantIntegerExpression(directDec.ArrayExpression)
			if e.assert(err == nil && sz > 0 , "Array expression must be a constant positive integer", directDec.LineInfo) {
				arrSize = sz
			}
		}
		if declaratorPointer != nil {
			lhsType = setPointersLowestLevel(declaratorPointer, lhsType)
		}
		var arr ArrayCtype
		if lhsArr, isArr := lhsType.(ArrayCtype); isArr {
			newDimensions := []int{}
			newDimensions = append(newDimensions, lhsArr.DimensionSizes...)
			newDimensions = append(newDimensions, arrSize)
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
			e.registerTypeError("Expected function pointer or array of function pointers", directDec.LineInfo)
		}
	}
	return
}

func (e *Engine) extractTypeAndName(dec *ast.Declarator, lhsType Ctype) (t Ctype, name string) {
	var ptr *PointerCtype = nil
	if dec.Pointer != nil {
		p := e.wrapInAnonymousPointers(dec.Pointer, UNKNOWN_OR_PARTIAL)	
		ptr = &p
	}
	return e.extractDirectTypeAndName(dec.DirectDeclarator, ptr, lhsType)
}

func (e *Engine) extractDirectType(ddec ast.DirectAbstractDeclarator, declaratorPointer *PointerCtype, lhsType Ctype) (t Ctype) {
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
			sz, err := evalConstantIntegerExpression(directDec.Expression)
			if e.assert(err == nil && sz > 0 , "Array expression must be a constant positive integer", directDec.LineInfo) {
				arrSize = sz
			}
		}
		if declaratorPointer != nil {
			lhsType = setPointersLowestLevel(declaratorPointer, lhsType)
		}
		var arr ArrayCtype
		if lhsArr, isArr := lhsType.(ArrayCtype); isArr {
			newDimensions := []int{}
			newDimensions = append(newDimensions, lhsArr.DimensionSizes...)
			newDimensions = append(newDimensions, arrSize)
			if newDimensions[len(newDimensions) - 2] == UNSPECIFIED_ARR_SIZE {
				newDimensions[len(newDimensions) - 2] = 1 // set it to 1 and continue finding other errors
				e.registerTypeError("multidimensional array must have bounds for all dimensions except the first", directDec.LineInfo)
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
			e.registerTypeError("Expected function pointer or array of function pointers", directDec.LineInfo)
		}
	}
	return
}

func (e *Engine) extractType(dec *ast.AbstractDeclarator, lhsType Ctype) (t Ctype) {
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

func (e *Engine) makeStructCtype(ts *ast.StructTypeSpecifier) (StructCtype, error) {
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
			e.registerTypeError(err.Error(), dec.LineInfo)
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

func (e *Engine) defineType(t Ctype, name string, definitionLine ast.LineInfo) {
	if prevDef, alreadyDefined := e.definitionTab[name]; alreadyDefined {
		if t.Name() != name {
			e.registerTypeError(fmt.Sprintf("Redefinition of type %s, previously defined at %d",
										t.Name(), prevDef.DeclarationLine), definitionLine)
		}
	} else {
		e.definitionTab[name] = TypeDefinition{Ctype: t, DeclarationLine: definitionLine.LineNumber}
	}
}

func (e *Engine) getCombinedBuiltin(tss []ast.TypeSpecifier) (Ctype, error) {
	strRepr := ""
	for _, ts := range tss {
		if directTs, isDirect := ts.(ast.DirectTypeSpecifier); !isDirect {
			e.registerTypeError("Illegal declaration", directTs.LineInfo)
			return VOID_POINTER, errors.New("Invalid combined builtin")
		} else {
			strRepr += " " + directTs.TypeName
		}
	}
	if !IsBuiltin(strRepr) {
		e.registerTypeError("Undefined type", tss[0].(ast.DirectTypeSpecifier).LineInfo)
		return VOID_POINTER, errors.New("Invalid combined builtin")
	}
	return BuiltinFrom(strRepr), nil
}

func (e *Engine) createSimpleType(ds *ast.DeclarationSpecifiers, allowStorageClassSpecifiers bool) Ctype {
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
			e.registerTypeError(err.Error(), ds.LineInfo)
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
				e.registerTypeError("Undefined type " + dts.TypeName, dts.LineInfo)
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

func (e *Engine) getSymbolsForTopLevelDeclarationAndDefineNewTypes(dec *ast.Declaration) []Symbol {
	symbols := []Symbol{}
	ds := dec.DeclarationSpecifiers
	e.assert(len(ds.StorageClassSpecifiers) <= 1, "Only single storage class specifier is allowed", ds.LineInfo)
	if !e.assert(len(ds.TypeSpecifiers) > 0, "Illegal declaration", ds.LineInfo) {
		return symbols
	}
	
	hasTypedef := false
	if len(ds.StorageClassSpecifiers) > 0 && ds.StorageClassSpecifiers[0].Val == TYPEDEF {
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
			e.registerTypeError(err.Error(), ds.LineInfo)
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
				e.registerTypeError("Initialization is invalid after typedef", initDec.LineInfo)
			} else if !partialTypeExtractionFailed {
				e.typeCheckInitializer(t, initDec.Initializer)
			}
		}
	}
	return symbols
}

func (e *Engine) createFunctionDefinition(fun *ast.FunctionDefinition) FunctionDefinition {
	if fun.DeclarationSpecifiers == nil {
		panic("didnt get declaration specifiers")
	}
	// TODO qualifiers
	e.assert(len(fun.DeclarationSpecifiers.StorageClassSpecifiers) == 0,
			"Function definition can't have storage specifiers", fun.LineInfo)
	baseRetType, err := e.getPartialTypeFromSpecifiers(fun.DeclarationSpecifiers.TypeSpecifiers)
	if err != nil {
		baseRetType = VOID_POINTER
		e.registerTypeError(err.Error(), fun.LineInfo)
	}
	t, name := e.extractTypeAndName(fun.Declarator, baseRetType)
	funPtr := t.(FunctionPtrCtype)
	return FunctionDefinition{
		Name: name,
		ReturnType: funPtr.ReturnType,
		ParamTypes: funPtr.ParamTypes,
		ParamNames: funPtr.ParamNames,
	}
}

func (e *Engine) getGreaterOrEqualTypeIfCompatible(e1 ast.Expression, e2 ast.Expression) (t Ctype, err error) {
	t1, err := e.getTypeOfExpression(e1)
	if err != nil {
		return nil, err
	}
	t2, err := e.getTypeOfExpression(e2)
	if err != nil {
		return nil, err
	}
	if isAutomaticallyCastable(t1, t2) || isAutomaticallyCastable(t2, t1) {
		return getGreaterOrEqualType(t1, t2), nil
	} else {
		return nil, errors.New("Uncompatible types")
	}
}

func (e *Engine) getTypeOfExpression(expression ast.Expression) (Ctype, error) {
	switch expr := expression.(type) {
	case ast.IdentifierExpression:
		if sym, ok := e.symtab.Lookup(expr.Identifier); ok {
			return sym.Type, nil
		} else if fdef, ok := e.funcdefs[expr.Identifier]; ok { 
			// TODO cleanup overloading code
			f := fdef[0]
			return FunctionPtrCtype{
				name: f.Name,
				ReturnType: f.ReturnType,
				ParamTypes: f.ParamTypes,
				ParamNames: f.ParamNames,
			}, nil
		} else {
			return nil, errors.New("Undefined identifier " + expr.Identifier)
		}
	case ast.ConstantValExpression:
		return getTypeOfConstantValExpression(expr), nil
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
			if !isIntegralType(arrayAccessorT) {
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
		if !canBeIndexedNTimes(arrayT, dimensions) {
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
		for _, arg := range expr.Args.Expressions {
			if t, err := e.getTypeOfExpression(arg); err != nil {
				return nil, err
			} else {
				argTypes = append(argTypes, t)
			}
		} 
		if t, err := e.getTypeOfExpression(expr.FunctionAccessor); err != nil {
			// if identExpr, isIdentExpr := expr.FunctionAccessor.(ast.IdentifierExpression); isIdentExpr {
			// 	if overloadedFuncs, ok := e.funcdefs[identExpr.Identifier]; ok {
			// 		choosenFunc, err := getFunctionOverloadSatisfyingArgs(overloadedFuncs, argTypes)
			// 		if err != nil {
			// 			return nil, err
			// 		}
			// 		return choosenFunc.ReturnType, nil
			// 	}
			// } else {
			return nil, err
			// }
		} else {
			if f, isFunc := t.(FunctionPtrCtype); isFunc {
				if len(f.ParamTypes) != len(argTypes) {
					return nil, errors.New("Invalid number of arguments")
				}
				for paramNum, paramType := range f.ParamTypes {
					if !isAutomaticallyCastable(argTypes[paramNum], paramType) {
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
			if canBeIncremented(t) {
				return t, nil
			}
			return nil, errors.New("Type can't be incremented or decremented")
		}
	case ast.IncDecUnaryExpression:
		if t, err := e.getTypeOfExpression(expr.UnaryExpression); err != nil {
			return nil, err
		} else {
			if canBeIncremented(t) {
				return t, nil
			}
			return nil, errors.New("Type can't be incremented or decremented")
		}
	case ast.CastUnaryExpression:
		if castedType, err := e.getTypeOfExpression(expr.CastExpression); err != nil {
			return nil, err
		} else {
			return getUnaryOpType(expr.Operator, castedType)
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
			if isExplicitlyCastable(castedType, resType) {
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
		return getBinaryOpType(expr.Operator, t1, t2)
	case ast.ConditionalExpression:
		conditionType, err := e.getTypeOfExpression(expr.Condition)
		if err != nil {
			return nil, err
		}
		if !canBeUsedAsBool(conditionType) {
			return nil, errors.New("Type of condition must be bool'ish")
		}
		return e.getGreaterOrEqualTypeIfCompatible(expr.IfTrueExpression, expr.ElseExpression)
	case ast.AssignmentExpression:
		lhsType, err := e.getTypeOfExpression(expr.LhsExpression)
		if err != nil {
			return nil, err
		}
		rhsType, err := e.getTypeOfExpression(expr.RhsExpression)
		if err != nil {
			return nil, err
		}	
		return getAssignmentOpType(lhsType, rhsType, expr.Operator)
	}
	panic("unexpected expression type")
}

func (e *Engine) getInitializerMetaForStruct(initializer *ast.Initializer) (fieldTypes []Ctype, fieldNames []string, err error) {
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

func (e *Engine) compareStructTypeWithInitializer(structType StructCtype, fieldTypes []Ctype, fieldNames []string) (err error) {
	if len(fieldTypes) > len(structType.NestedFieldTypes) {
		return errors.New("Too many initializer fields")
	}
	for idx, fieldType := range fieldTypes {
		name := fieldNames[idx]
		if name == ANONYMOUS {
			if !isAutomaticallyCastable(fieldType, structType.NestedFieldTypes[idx]) {
				return errors.New("Invalid initializer type")
			}
		} else {
			if sfieldType, _, err  := structType.MaybeField(name); err != nil {
				return errors.New("No field named " + name)
			} else if !isAutomaticallyCastable(fieldType, sfieldType) {
				return errors.New("Invalid initializer type")
			}
		}
	}
	return nil
}

func (e *Engine) typeCheckInitializer(targetType Ctype, initializer *ast.Initializer) error {
	if structType, isStruct := targetType.(StructCtype); isStruct {
		fieldTypes, fieldNames, err := e.getInitializerMetaForStruct(initializer)
		if err != nil {
			return err
		}
		if err := e.compareStructTypeWithInitializer(structType, fieldTypes, fieldNames); err != nil {
			return err 
		}
	} else {
		expr := initializer.Expression
		if _, isAssignment := expr.(ast.AssignmentExpression); isAssignment {
			return errors.New("Illegal assignment expression in initializer")
		}
		if initializerType, err := e.getTypeOfExpression(expr); err != nil {
			if !isAutomaticallyCastable(initializerType, targetType) {
				return errors.New("Invalid initializer type")
			}
		}
	}
	return nil
}

func (e *Engine) getDeclarationSymbolsAndTypeCheckInitializers(dec *ast.Declaration) ([]Symbol, error) {
	if dec.InitDeclaratorList == nil {
		return nil, errors.New("Missing indentifier")
	}
	e.assert(len(dec.DeclarationSpecifiers.StorageClassSpecifiers) == 0, "Storage class specifiers illegal here", dec.LineInfo)

	partialType, err := e.getPartialTypeFromSpecifiers(dec.DeclarationSpecifiers.TypeSpecifiers)
	if err != nil {
		return nil, err // or fallback to void* for further type checking?
	}	

	symbols := make([]Symbol, 0)
	for _, initDecl := range dec.InitDeclaratorList.InitDeclarators {
		t, name := e.extractTypeAndName(initDecl.Declarator, partialType)
		if initDecl.Initializer != nil {
			if err := e.typeCheckInitializer(t, initDecl.Initializer); err != nil {
				e.registerTypeError("Invalid initializer type", initDecl.LineInfo)
			}
		}
		if b, isBuiltin := t.(BuiltinCtype); isBuiltin {
			if b.Builtin == VOID {
				e.registerTypeError("Can't declare void variables", initDecl.LineInfo)
				t = VOID_POINTER
			}
		}
		symbols = append(symbols, Symbol{Name: name, Type: t, LineInfo: initDecl.LineInfo})
	}
	return symbols, nil
}

func (e *Engine) defineSymbol(sym Symbol) {
	if prevDef, ok := e.symtab.HasInCurrentScope(sym.Name); ok {
		e.registerTypeError(fmt.Sprintf("Redefinition of symbol %s previously defined in %d",
							sym.Name, prevDef.LineInfo.LineNumber), sym.LineInfo)
	} else {
		e.symtab.Define(sym.Name, sym)
	}
}

func (e *Engine) enterGlobalScope() {
	e.symtab.EnterScope()
}

func (e *Engine) addSymbolsFromDeclarationListToCurrentScope(dl *ast.DeclarationList) {
	for _, dec := range dl.Declarations {
		declaredSymbols, err := e.getDeclarationSymbolsAndTypeCheckInitializers(dec)
		if err != nil {
			e.registerTypeError(err.Error(), dl.LineInfo)
		} else {
			for _, sym := range declaredSymbols {
				e.defineSymbol(sym)
			}
		}
	}
}

func (e *Engine) checkCondition(expr ast.Expression, lineInfo ast.LineInfo) {
	if condType, err := e.getTypeOfExpression(expr); err != nil {
		e.registerTypeError(err.Error(), lineInfo)
	} else {
		if !canBeUsedAsBool(condType) {
			e.registerTypeError("Condition doesn't have bool-ish value", lineInfo)
		}
	}
}

func (e *Engine) checkSwitchExpression(expr ast.Expression, lineInfo ast.LineInfo) Ctype {
	if condType, err := e.getTypeOfExpression(expr); err != nil {
		e.registerTypeError(err.Error(), lineInfo)
		return condType
	} else {
		if !canBeUsedAsBool(condType) {
			e.registerTypeError("Condition doesn't have bool-ish value", lineInfo)
		}
	}
	return VOID_POINTER // TODO
}

func (e *Engine) handleStatement(stmnt ast.Statement, ctx StatementContext) {
	if ctx.ExpectsCase {
		switch stmnt.(type) {
		case ast.CaseLabeledStatement, ast.DefaultLabeledStatement:
		default:
			e.registerTypeError("Expected case or default statement", ast.LineInfo{}) // TODO lineinfo
		}
	}
	switch s := stmnt.(type) {
	case ast.CompoundStatement:
		e.symtab.EnterScope()
		defer e.symtab.LeaveScope()
		e.handleCompoundStatement(&s, ctx)
	case ast.CaseLabeledStatement:
		if !ctx.ExpectsCase {
			e.registerTypeError("unexpected case statement not inside switch", s.LineInfo)
		} else {
			if exprT, err := e.getTypeOfExpression(s.Expression); err != nil {
				if !isAutomaticallyCastable(exprT, ctx.CaseExpressionType) {
					e.registerTypeError("Invalid type of case expression", s.LineInfo)
				}
			}
			e.handleStatement(s.Statement, ctx.WithDisallowedCase())
		}
	case ast.DefaultLabeledStatement:
		if !ctx.ExpectsCase {
			e.registerTypeError("unexpected default statement not inside switch", s.LineInfo)
		} else {
			e.handleStatement(s.Statement, ctx.WithDisallowedCase())
		}
	case ast.IdentifierLabeledStatement:
		if e.funcGotoLabels.Has(s.Identifier) {
			e.registerTypeError("Illegal multiple usage of same label", s.LineInfo)
		} else {
			e.funcGotoLabels.Add(s.Identifier)
		}
		e.handleStatement(s.Statement, ctx)
	case ast.ExpressionStatement:
		if s.Expression != nil {
			if _, err := e.getTypeOfExpression(s.Expression); err != nil {
				e.registerTypeError(err.Error(), s.LineInfo)
			}
		}
	case ast.IfSelectionStatement:
		e.checkCondition(s.Condition, s.LineInfo)
		e.handleStatement(s.IfStatement, ctx)
		if s.ElseStatement != nil {
			e.handleStatement(s.ElseStatement, ctx)
		}
	case ast.SwitchSelectionStatement:
		switchExprT := e.checkSwitchExpression(s.SwitchExpression, s.LineInfo)
		e.handleStatement(s.SwitchBody, ctx.WithAllowedBreak().And().WithExpectedCase(switchExprT))
	case ast.WhileIterationStatement:
		e.checkCondition(s.Condition, s.LineInfo)
		e.handleStatement(s.Body, ctx.WithAllowedBreak())
	case ast.DoWhileIterationStatement:
		e.checkCondition(s.Condition, s.LineInfo)
		e.handleStatement(s.Body, ctx.WithAllowedBreak())
	case ast.ForIterationStatement:
		e.handleStatement(s.Initializer, ctx)
		if s.Condition.Expression != nil {
			e.checkCondition(s.Condition.Expression, s.LineInfo)
		}
		if s.Updater != nil {
			if _, err := e.getTypeOfExpression(s.Updater); err != nil {
				e.registerTypeError(err.Error(), s.LineInfo)
			}
		}
		e.handleStatement(s.Body, ctx.WithAllowedBreak())
	case ast.LoopControlJumpStatement:
		if !ctx.CanUseBreak {
			// TODO this (and a couple of others) should probably be semantic error
			e.registerTypeError("Illegal break not inside in loop or switch", s.LineInfo)
		}
	case ast.GotoJumpStatement:
		e.funcGotosToCheck.Add(s.Label)
	case ast.ReturnJumpStatement:
		if s.Expression != nil {
			if exprT, err := e.getTypeOfExpression(s.Expression); err != nil {
				e.registerTypeError(err.Error(), s.LineInfo)
			} else if !isAutomaticallyCastable(exprT, ctx.RequiredReturnType) {
				e.registerTypeError("Uncompatible return type", s.LineInfo)
			}
		} else {
			if !isVoid(ctx.RequiredReturnType) {
				e.registerTypeError("Expected return value", s.LineInfo)
			}
		}
	}
}

func (e *Engine) handleCompoundStatement(cs *ast.CompoundStatement, ctx StatementContext) {
	if cs.DeclarationList != nil {
		e.addSymbolsFromDeclarationListToCurrentScope(cs.DeclarationList)
	}
	if cs.StatementList != nil {
		for _, stmnt := range cs.StatementList.Statements {
			e.handleStatement(stmnt, ctx)
		}
	}
}

func (e *Engine) handleFunctionDefinition(fun *ast.FunctionDefinition) {
	e.funcGotoLabels = utils.NewSet[string]()
	e.funcGotosToCheck = utils.NewSet[string]()
	fdef := e.createFunctionDefinition(fun)
	if overloads, isOverloaded := e.funcdefs[fdef.Name]; isOverloaded {
		if hasExactlySameOverload(&fdef, overloads) {
			e.registerTypeError("redefinition of function with same name and argument types", fun.LineInfo)
		} else {
			overloads = append(overloads, &fdef)
		} 
	} else {
		e.funcdefs[fdef.Name] = []*FunctionDefinition{&fdef}
	}
	e.symtab.EnterScope()
	defer e.symtab.LeaveScope()
	for paramNum := range fdef.ParamTypes {
		e.defineSymbol(Symbol{Name: fdef.ParamNames[paramNum], Type: fdef.ParamTypes[paramNum]})
	}
	e.handleCompoundStatement(fun.Body, StatementContext{
		CanUseBreak: false,
		ExpectsCase: false,
		RequiredReturnType: fdef.ReturnType,
	})

	for _, label := range e.funcGotosToCheck.GetAll() {
		if !e.funcGotoLabels.Has(label) {
			e.registerTypeError("Undefined label", fun.LineInfo) // TODO more specific line info
		}
	}
}

func (e *Engine) handleTopLevelDeclaration(dec *ast.Declaration) {
	definedSymbols := e.getSymbolsForTopLevelDeclarationAndDefineNewTypes(dec)
	for _, sym := range definedSymbols {
		e.defineSymbol(sym)
	}
}

func (e *Engine) DefineTypesAndRunTypeChecking(root *ast.TranslationUnit) {
	e.typeErrors = make([]error, 0)
	for _, dec := range root.ExternalDeclarations {
		switch declaration := dec.(type) {
		case ast.FunctionDefinition:
			e.handleFunctionDefinition(&declaration)
		case ast.Declaration:
			e.handleTopLevelDeclaration(&declaration)
		}
	}
	fmt.Println(e.typeErrors)
}

func (e *Engine) GetTypeOfExpression(expression ast.Expression) Ctype {
	t, err := e.getTypeOfExpression(expression)
	if err != nil {
		panic("unexpected err")
	}
	return t
}
