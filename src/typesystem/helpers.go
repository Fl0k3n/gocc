package types

import (
	"ast"
)

type FunctionDefinition struct {
	Name string
	ReturnType Ctype
	ParamTypes []Ctype
	ParamNames []string
}

type DeclarationContext struct {
	allowsStorageClassSpecifiers bool
	allowsInitialization bool
}

func getTypeName(ts ast.TypeSpecifier) string {
	switch dts := ts.(type) {
	case ast.DirectTypeSpecifier:
		return dts.TypeName
	case ast.EnumTypeSpecifier:
		return *dts.Identifier
	case ast.StructTypeSpecifier:
		return *dts.Identifier
	}
	panic("Unexpected type specifier")
}

func getLineInfo(ts ast.TypeSpecifier) ast.LineInfo {
	switch dts := ts.(type) {
	case ast.DirectTypeSpecifier:
		return dts.LineInfo
	case ast.EnumTypeSpecifier:
		return dts.LineInfo
	case ast.StructTypeSpecifier:
		return dts.LineInfo
	}
	panic("Unexpected type specifier")
}

func makeStructFullName(identifier string) string {
	return "struct " + identifier;
}

func setPointersLowestLevel(ptr *PointerCtype, lowestLvl Ctype) Ctype {
	if fncPtr, isFunc := lowestLvl.(FunctionPtrCtype); isFunc {
		return ptr.AsFunctionPointerAtLowestLevel(&fncPtr)
	} else {
		return ptr.WithTargetOnLowestLevel(lowestLvl)
	}
}

func evalConstantIntegerExpression(expr ast.Expression) (int, error) {
	return 0, nil
}