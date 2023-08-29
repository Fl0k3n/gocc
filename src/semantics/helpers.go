package semantics

import (
	"ast"
	"errors"
	"strconv"
	"strings"
)

type FunctionDefinition struct {
	Name string
	ReturnType Ctype
	ParamTypes []Ctype
	ParamNames []string
}

type SymbolDeclaration struct {
	Name string
	T Ctype
	Initializer *ast.Initializer
}

type StorageClassSpecifier string

const (
	TYPEDEF StorageClassSpecifier = "typedef"
	EXTERN = "extern"
	STATIC = "static"
	// ignore auto and register 
)

type GlobalDeclaration struct {
	Name string
	T Ctype
	Initializer *ast.Initializer
	Extern bool
	Static bool
}

type SymbolInitializer struct {
	SymbolName string
	FieldName *string
	Initializer *ast.Expression
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

func extractStructFieldInitializerIdentifierName(ae *ast.AssignmentExpression) (string, error) {
	if ie, isIdentifier := ae.LhsExpression.(ast.IdentifierExpression); isIdentifier {
		ident := ie.Identifier
		if !strings.HasPrefix(ident, ".") {
			return "", errors.New("Field must be prefixed with dot")
		}
		return ident[1:], nil
	}
	return "", errors.New("Expression is not an identifier")
}

// only +, -, *, /, %
func applyArithmeticOperator(v1 int, v2 int, op string) (v int, err error) {
	switch op {
	case "+": v = v1 + v2
	case "-": v = v1 - v2
	case "*": v = v1 * v2
	case "/":
		if v2 == 0 {
			return -1, errors.New("Zero division error")
		}
		v = v1 / v2
	case "%":
		if v2 == 0 {
			return -1, errors.New("Zero division error")
		}
		v = v1 % v2
	default:
		err = errors.New("Unsupported compile time constant operator: " + op)
	}
	return
}

func evalIntVal(val string) (int, error) {
	var v int64
	var e error
	switch {
		case strings.HasPrefix(val, "0b"):
			v, e = strconv.ParseInt(val[2:], 2, 0)
		case strings.HasPrefix(val, "0x"):
			v, e = strconv.ParseInt(val[2:], 16, 0)
		case strings.HasPrefix(val, "0"):
			v, e = strconv.ParseInt(val[1:], 8, 0)
		default:
			v, e = strconv.ParseInt(val, 10, 0)
	}
	if e != nil {
		return 0, e
	}
	return int(v), nil
}

func isStruct(t Ctype) bool {
	_, ok := t.(StructCtype)
	return ok
}

func isBuiltinType(t Ctype) bool {
	_, ok := t.(BuiltinCtype)
	return ok
}

func isPointer(t Ctype) bool {
	_, ok := t.(PointerCtype)
	return ok
}

func isVoid(t Ctype) bool {
	if b, ok := t.(BuiltinCtype); ok {
		return b.Builtin == VOID
	}
	return false
}

func isBuiltinOrPointer(t Ctype) bool {
	return isBuiltinType(t) || isPointer(t)
}
