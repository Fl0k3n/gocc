package types

import (
	"ast"
	"errors"
	"fmt"
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
		return e.getDefinedType(*dts.Identifier)
	}
	panic("Unexpected type specifier")
}

func (e *Engine) augmentFunctionDefinition(fun *ast.FunctionDefinition) {
	fmt.Println("ok")
}

func (e *Engine) wrapInAnonymousPointers(ptr *ast.Pointer, target Ctype) Ctype {
	// TODO qualifiers
	wrapped := PointerCtype{name: ANONYMOUS, Target: target}
	for ptr.NestedPointer != nil {
		wrapped = PointerCtype{name: ANONYMOUS, Target: &wrapped}
		ptr = ptr.NestedPointer
	}
	return wrapped
}

func (e *Engine) extractFunctionParamTypes(paramList *ast.ParameterTypeList) []Ctype {
	return nil
}

// concrete variable/struct field oriented, dimension sizes inside arrays required etc
func (e *Engine) extractDirectPartialTypeAndName(ddec ast.DirectDeclarator) (t Ctype, name string) {
	switch directDec := ddec.(type) {
	case ast.DirectIdentifierDeclarator:
		t = UNKNOWN_OR_PARTIAL
		name = directDec.Identifier
	case ast.Declarator:
		t, name = e.extractPartialTypeAndName(&directDec)
	case ast.DirectArrayDeclarator:
		arrSize := 1 // keep it as 1 and continue finding other errors
		if e.assert(directDec.ArrayExpression != nil, "Array size is required", directDec.LineInfo) {
			sz, err := evalConstantIntegerExpression(directDec.ArrayExpression)
			if e.assert(err == nil && sz > 0 , "Array expression must be a constant positive integer", directDec.LineInfo) {
				arrSize = sz
			}
		}
		nestedT, n := e.extractDirectPartialTypeAndName(directDec.Declarator)
		name = n
		if nestedT == UNKNOWN_OR_PARTIAL {
			t = NewArray(name, []int{arrSize}, UNKNOWN_OR_PARTIAL)	
		} else {
			switch nested := nestedT.(type) {
			case ArrayCtype:
				newDimensions := []int{arrSize}
				newDimensions = append(newDimensions, nested.DimensionSizes...)
				t = NewArray(nested.name, newDimensions, nested.NestedType)
			case PointerCtype:
				lowestLvlPtr := getPointersLowestLevel(&nested)
				lowestLvlPtr.Target = NewArray(ANONYMOUS, []int{arrSize}, UNKNOWN_OR_PARTIAL)
				t = nested
			default:
				e.registerTypeError("Invalid type", directDec.LineInfo)
				t = UNKNOWN_OR_PARTIAL
			}
		}
	case ast.DirectFunctionDeclarator:
		if !e.assert(directDec.IndentifierList == nil, "Expected function parameter, not identifiers", directDec.LineInfo) {
			t = UNKNOWN_OR_PARTIAL
			return
		}
		funcParams := []Ctype{}
		if directDec.ParameterTypeList != nil {
			funcParams = e.extractFunctionParamTypes(directDec.ParameterTypeList)
		}
		nestedT, n := e.extractDirectPartialTypeAndName(directDec.Declarator)
		if nestedT == UNKNOWN_OR_PARTIAL {
			t = FunctionPtrCtype{name: n, ReturnType: UNKNOWN_OR_PARTIAL, ParamTypes: funcParams}
			name = n
		} else {
			switch nested := nestedT.(type) {
			case PointerCtype:
				penultimateLvlPtr := getPenultimatePointersLevel(&nested)
				if penultimateLvlPtr == nil {
					t = FunctionPtrCtype{name: n, ReturnType: UNKNOWN_OR_PARTIAL, ParamTypes: funcParams}
				} else {
					penultimateLvlPtr.Target = FunctionPtrCtype{name: n, ReturnType: UNKNOWN_OR_PARTIAL, ParamTypes: funcParams}
					t = nested
				}
			case ArrayCtype:
				//?
			default:
				e.registerTypeError("Invalid type", directDec.LineInfo)
				t = UNKNOWN_OR_PARTIAL
			} 
		}
	}
}

func (e *Engine) extractPartialTypeAndName(dec *ast.Declarator) (t Ctype, name string) {
	switch directDec := dec.DirectDeclarator.(type) {
	case ast.DirectIdentifierDeclarator:
		t = nil
		name = directDec.Identifier
	case ast.Declarator:
		t, name = e.extractPartialTypeAndName(&directDec)
	case ast.DirectArrayDeclarator:


	}

	if dec.Pointer != nil {
		t = e.wrapInAnonymousPointers(dec.Pointer, t)
	}
	return
}

func (e *Engine) makePossiblyMoreComplexStructFieldCtype(baseTypeWithSpecs Ctype, declarator *ast.StructDeclarator) (t Ctype, fieldName string) {
	if declarator.Expression != nil {
		panic("Bit fields are not supported atm")
	}
	dec := declarator.Declarator
	if dec.Pointer != nil {
		baseTypeWithSpecs = e.wrapInAnonymousPointers(dec.Pointer, baseTypeWithSpecs)
	}
	if nestedDeclarator, is := dec.DirectDeclarator.(ast.Declarator); is {

	}
}

func (e *Engine) makeStructCtype(ts *ast.StructTypeSpecifier, typedefName *string) (Ctype, error) {
	e.assert(typedefName != nil || ts.Identifier != nil, "Invalid anonymous struct with no type name", ts.LineInfo)
	if ts.Identifier == nil {
		ts.Identifier = typedefName
	}

	fields := make([]Ctype, 0)
	names := make([]string, 0)
	for _, dec := range ts.StructDeclarationList.StructDeclarations {
		e.assert(len(dec.SpecifierQulifierList.TypeSpecifiers) == 1, "Struct field must have single type specifier", dec.LineInfo)
		// TODO qualifiers, are they even valid inside struct?
		typeSpec := dec.SpecifierQulifierList.TypeSpecifiers[0]
		if fieldType, err := e.getDefinedTypeFromSpecifier(typeSpec); err != nil {
			e.registerTypeError("Undefined type " + getTypeName(typeSpec), getLineInfo(typeSpec))
			// just ignore this field and continue
		} else {
			for _, declaratorsForThatType := range dec.StructDeclaratorList.StructDeclarators {
				fields = append(fields, fieldType)
			}
		}
	}

	return NewStruct(*ts.Identifier, fields, names), nil
}


// creates type, assumes this is not inside typedef
func (e *Engine) makeType(ts ast.TypeSpecifier, typedefName *string) Ctype {
	switch dts := ts.(type) {
	case ast.DirectTypeSpecifier:
		if IsBuiltin(dts.TypeName) {
			return BuiltinFrom(dts.TypeName)
		} else {
			res := e.getDefinedType(dts.TypeName);
			if res == nil {
				e.registerTypeError("Undeclared type: " + dts.TypeName, dts.LineInfo)
				res = BuiltinFrom("void") // TODO
			}
			return res
		}
	case ast.StructTypeSpecifier:

	default:
		panic("Unexpected Type Specifier")
	}
}

func (e *Engine) handleDeclarationSpecifiers(dec *ast.Declaration) {
	ds := dec.DeclarationSpecifiers
	e.assert(len(ds.StorageClassSpecifiers) <= 1, "Only single storage class specifier is allowed", ds.LineInfo)
	if len(ds.StorageClassSpecifiers) == 1 {
		if ds.StorageClassSpecifiers[0].Val == "extern" {
			e.assert(dec.InitDeclaratorList == nil, "Extern value can't be initialized", dec.LineInfo)
		} else if ds.StorageClassSpecifiers[0].Val == "typedef" {
			e.assert(dec.InitDeclaratorList == nil, "Typedef value can't be initialized", dec.LineInfo)

		}
	}
}

func (e *Engine) handleDeclaration(fun *ast.Declaration) {
	fmt.Println("ok")
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

