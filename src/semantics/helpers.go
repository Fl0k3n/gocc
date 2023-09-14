package semantics

import (
	"ast"
	"errors"
	"fmt"
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
	Function bool
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

type BinaryOpTypecast struct {
	LeftRequiresCast bool
	LeftTargetType Ctype
	RightRequiresCast bool
	RightTargetType Ctype
}

type ProgramConstant interface {
	String() string
}

type IntegralConstant struct {
	Val int64
	T Ctype
}

func (i IntegralConstant) String() string {
	return fmt.Sprintf("%d", i.Val)
}

type FloatingConstant struct {
	Val float64
	T Ctype
}

func (f FloatingConstant) String() string {
	return fmt.Sprintf("%f", f.Val)
}

type StringConstanst struct {
	Val string
}

func (s StringConstanst) String() string {
	return s.Val
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
func applyArithmeticOperatorOnInts(v1 int64, v2 int64, op string) (v int64, err error) {
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
		err = errors.New("Unsupported compile time constant operator for integral values: " + op)
	}
	return
}

func applyArithmeticOperatorOnFloats(v1 float64, v2 float64, op string) (v float64, err error) {
	switch op {
	case "+": v = v1 + v2
	case "-": v = v1 - v2
	case "*": v = v1 * v2
	case "/":
		if v2 == 0 {
			return -1, errors.New("Zero division error")
		}
		v = v1 / v2
	default:
		err = errors.New("Unsupported compile time constant operator for floating values: " + op)
	}
	return
} 

func stripNumericTypeSuffix(val string) string {
	for {
		lastChar := val[len(val) - 1]
		if lastChar == 'f' || lastChar == 'F' || lastChar == 'l' || 
		   lastChar == 'L' || lastChar == 'u' || lastChar == 'U' {
			val = val[:len(val) - 1]
		} else {
			return val
		}
	}
}

func evalIntVal(val string) (int64, error) {
	// TODO this has issue with unsigned long out of bounds which may be flipped to negative
	var v int64
	var e error
	val = stripNumericTypeSuffix(val)
	if strings.HasPrefix(val, "'") {
		return int64(val[1]), nil
	}
	switch {
		case strings.HasPrefix(val, "0b"):
			v, e = strconv.ParseInt(val[2:], 2, 64)
		case strings.HasPrefix(val, "0x"):
			v, e = strconv.ParseInt(val[2:], 16, 64)
		case strings.HasPrefix(val, "0"):
			if val == "0" {
				v = 0
			} else {
				v, e = strconv.ParseInt(val[1:], 8, 64)
			}
		default:
			v, e = strconv.ParseInt(val, 10, 64)
	}
	if e != nil {
		return 0, e
	}
	return v, nil
}

func evalFloatVal(val string) (float64, error) {
	val = stripNumericTypeSuffix(val)
	return strconv.ParseFloat(val, 64)
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

func isString(t Ctype) bool {
	if ptr, isPtr := t.(PointerCtype); isPtr {
		if target, targetIsBuiltin := ptr.Target.(BuiltinCtype); targetIsBuiltin {
			return target.Builtin == CHAR
		}
	}
	return false
}