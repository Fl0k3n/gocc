package types

type Builtin string

const POINTER_SIZE int = 8
const POINTER_ALIGNMENT int = 8

const (
	VOID Builtin = "void"
	CHAR = "char"
	SHORT = "short"
	INT = "int"
	LONG = "long"
	FLOAT = "float"
	DOUBLE = "double"
	SIGNED = "signed"
	UNSIGNED = "unsigned"
)

var builtinTypes = []Builtin{VOID, CHAR, SHORT, INT, LONG, FLOAT, DOUBLE, SIGNED, UNSIGNED}

func IsBuiltin(typeName string) bool {
	for _, bt := range builtinTypes {
		if string(bt) == typeName {
			return true
		}
	}
	 return false
}

var sizeof = map[Builtin]int {
	VOID: 0,
	CHAR: 1,
	SHORT: 2,
	INT: 4,
	LONG: 8,
	FLOAT: 4,
	DOUBLE: 8,
	SIGNED: 4,
	UNSIGNED: 4,
}

var alignment = map[Builtin]int {
	VOID: 0,
	CHAR: 1,
	SHORT: 2,
	INT: 4,
	LONG: 8,
	FLOAT: 4,
	DOUBLE: 8,
	SIGNED: 4,
	UNSIGNED: 4,
}
