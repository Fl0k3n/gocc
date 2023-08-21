package types

import (
	"ast"
	"errors"
	"strings"
)

type FunctionDefinition struct {
	Name string
	ReturnType Ctype
	ParamTypes []Ctype
	ParamNames []string
}

type Symbol struct {
	Name string
	Type Ctype
	LineInfo ast.LineInfo
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

func extractStructFieldInitializerIdentifierName(ae *ast.AssigmentExpression) (string, error) {
	if ie, isIdentifier := ae.LhsExpression.(ast.IdentifierExpression); isIdentifier {
		ident := ie.Identifier
		if !strings.HasPrefix(ident, ".") {
			return "", errors.New("Field must be prefixed with dot")
		}
		return ident[1:], nil
	}
	return "", errors.New("Expression is not an identifier")
}

func evalConstantIntegerExpression(expr ast.Expression) (int, error) {
	return 0, nil
}