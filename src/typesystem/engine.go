package types

import (
	"ast"
	"errors"
	"fmt"
	"utils"
)

type Engine struct {
	definitionTab map[string]TypeDefinition
	typeErrors []error
}

func NewEngine() *Engine {
	return &Engine{
		definitionTab: make(map[string]TypeDefinition),
	}
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
	return Ctype(nil), errors.New("Undefined type " + name)
}

func (e *Engine) getDefinedTypeFromSpecifier(ts ast.TypeSpecifier) (Ctype, error) {
	switch dts := ts.(type) {
	case ast.DirectTypeSpecifier:
		return e.getDefinedType(dts.TypeName)
	case ast.EnumTypeSpecifier:
		e.assert(dts.EnumeratorList == nil, "Defined enum must be just its name, without declaration", dts.LineInfo)
		return e.getDefinedType(*dts.Identifier)
	case ast.StructTypeSpecifier:
		e.assert(dts.StructDeclarationList == nil, "Defined struct type must be just its name, without fields", dts.LineInfo)
		return e.getDefinedType(makeStructFullName(*dts.Identifier))
	}
	panic("Unexpected type specifier")
}

func (e *Engine) augmentFunctionDefinition(fun *ast.FunctionDefinition) {
	fmt.Println("ok")
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

func (e *Engine) extractFunctionParamTypes(paramList *ast.ParameterTypeList) []Ctype {
	// TODO varargs
	res := make([]Ctype, 0)
	names := utils.NewSet[string]()
	for _, param := range paramList.ParameterList.ParameterDeclarations {
		// TODO handle qualifiers and unsigned/signed
		spec := param.DeclarationSpecifiers
		e.assert(len(spec.TypeSpecifiers) == 1, "Struct field must have single type specifier", param.LineInfo)
		typeSpec := param.DeclarationSpecifiers.TypeSpecifiers[0]
		if partialType, err := e.getDefinedTypeFromSpecifier(typeSpec); err != nil {
			e.registerTypeError("Undefined type " + getTypeName(typeSpec), getLineInfo(typeSpec))
			// TODO cast this to void* 
		} else if param.AbstractDeclarator != nil {
			res = append(res, e.extractType(param.AbstractDeclarator, partialType))
		} else if param.Declarator != nil {
			t, name := e.extractTypeAndName(param.Declarator, partialType)
			res = append(res, t)
			if names.Has(name) {
				e.registerTypeError("Parameter list names must be unique", param.LineInfo)
			} else {
				names.Add(name)
			}
		} else {
			res = append(res, partialType)
		}
	}
	return res
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
		if directDec.ParameterTypeList != nil {
			funcParams = e.extractFunctionParamTypes(directDec.ParameterTypeList)
		}
		if declaratorPointer != nil {
			lhsType = setPointersLowestLevel(declaratorPointer, lhsType)
		}
		fptr := FunctionPtrCtype{name: ANONYMOUS, ReturnType: lhsType, ParamTypes:  funcParams}
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
		if directDec.ParameterTypeList != nil {
			funcParams = e.extractFunctionParamTypes(directDec.ParameterTypeList)
		}
		if declaratorPointer != nil {
			lhsType = setPointersLowestLevel(declaratorPointer, lhsType)
		}
		fptr := FunctionPtrCtype{name: ANONYMOUS, ReturnType: lhsType, ParamTypes:  funcParams}
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

func (e *Engine) makeStructCtype(ts *ast.StructTypeSpecifier, typedefName *string) (StructCtype, error) {
	e.assert(typedefName != nil || ts.Identifier != nil, "Invalid anonymous struct with no type name", ts.LineInfo)
	var name string = ANONYMOUS
	if ts.Identifier != nil {
		name = *ts.Identifier
	}
	fields := make([]Ctype, 0)
	names := make([]string, 0)
	for _, dec := range ts.StructDeclarationList.StructDeclarations {
		e.assert(len(dec.SpecifierQulifierList.TypeSpecifiers) == 1, "Struct field must have single type specifier", dec.LineInfo)
		// TODO handle qualifiers
		typeSpec := dec.SpecifierQulifierList.TypeSpecifiers[0]
		if partialType, err := e.getDefinedTypeFromSpecifier(typeSpec); err != nil {
			e.registerTypeError("Undefined type " + getTypeName(typeSpec), getLineInfo(typeSpec))
			// just ignore this field and continue
		} else {
			for _, declaratorsForThatType := range dec.StructDeclaratorList.StructDeclarators {
				e.assert(declaratorsForThatType.Expression == nil, "Bit fields are unsupported atm", dec.LineInfo)
				fieldType, name := e.extractTypeAndName(declaratorsForThatType.Declarator, partialType)
				fields = append(fields, fieldType)
				names = append(names, name)
			}
		}
	}

	return NewStruct(name, fields, names), nil
}

func (e *Engine) defineStruct(structType StructCtype, name string, definitionLine ast.LineInfo) {
	if prevDef, alreadyDefined := e.definitionTab[name]; alreadyDefined {
		e.registerTypeError(fmt.Sprintf("Redefinition of struct %s, previously defined at %d",
									 structType.name, prevDef.DeclarationLine), definitionLine)
	} else {
		e.definitionTab[name] = TypeDefinition{Ctype: structType, DeclarationLine: definitionLine.LineNumber}
	}
}

func (e *Engine) createAndDefineType(ts ast.TypeSpecifier, typedefName *string) (Ctype, error) {
	switch dts := ts.(type) {
	case ast.DirectTypeSpecifier:
		if IsBuiltin(dts.TypeName) {
			return BuiltinFrom(dts.TypeName), nil
		} else {
			return e.getDefinedType(dts.TypeName)
		}
	case ast.StructTypeSpecifier:
		if structType, err := e.makeStructCtype(&dts, typedefName); err != nil {
			return nil, err
		} else {
			if structType.Name() != ANONYMOUS {
				e.defineStruct(structType, makeStructFullName(structType.Name()), dts.LineInfo)
			}
			if typedefName != nil {
				e.defineStruct(structType, *typedefName, dts.LineInfo)
			}
			return structType, nil
		}
	case ast.EnumTypeSpecifier:
		return BuiltinFrom("int"), nil // TODO
	default:
		panic("Unexpected Type Specifier")
	}
}

func (e *Engine) handleDeclaration(dec *ast.Declaration) {
	ds := dec.DeclarationSpecifiers
	hasTypedef := false
	e.assert(len(ds.StorageClassSpecifiers) <= 1, "Only single storage class specifier is allowed", ds.LineInfo)
	if len(ds.StorageClassSpecifiers) == 1 {
		if ds.StorageClassSpecifiers[0].Val == "extern" {
			e.assert(dec.InitDeclaratorList == nil, "Extern value can't be initialized", dec.LineInfo)
		} else if ds.StorageClassSpecifiers[0].Val == "typedef" {
			e.assert(dec.InitDeclaratorList == nil, "Typedef value can't be initialized", dec.LineInfo)
			hasTypedef = true
		}
	}
	// TODO qualifiers, and combine type specifiers (unsigned/signed), and typedef
	if hasTypedef {
		panic("support typedef")
	}
	t, err := e.createAndDefineType(ds.TypeSpecifiers[0], nil)
	if err != nil {
		//TODO
		panic(err)
	}
	fmt.Println(t)
}

func (e *Engine) AugmentASTWithTypeInfo(root *ast.TranslationUnit) []error {
	e.typeErrors = make([]error, 0)
	for _, dec := range root.ExternalDeclarations {
		switch declaration := dec.(type) {
		case ast.FunctionDefinition:
			e.augmentFunctionDefinition(&declaration)
		case ast.Declaration:
			e.handleDeclaration(&declaration)
		}
	}
	return e.typeErrors
}

