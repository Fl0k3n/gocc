package ast

type Node interface {}

type TypeName struct {
	SpecifierQulifierList *SpecifierQulifierList
	AbstractDeclarator *AbstractDeclarator
	LineInfo
}

type StringPrimitive struct {
	val string
	LineInfo
}

type TypeQualifier StringPrimitive

type SpecifierQulifierList struct {
	TypeSpecifiers []*TypeSpecifier
	TypeQualifiers []*TypeQualifier
	LineInfo
}

type StorageClassSpecifier StringPrimitive

type DeclarationSpecifiers struct {
	TypeSpecifiers []TypeSpecifier
	TypeQualifiers []*TypeQualifier
	StorageClassSpecifiers []*StorageClassSpecifier
	LineInfo
}

type DirectAbstractFunctionDeclarator struct {
	ParameterTypeList *ParameterTypeList
	DirectAbstractDeclarator *DirectAbstractDeclarator
	LineInfo
}

type DirectAbstractArrayDeclarator struct {
	Expression *Expression
	DirectAbstractDeclarator *DirectAbstractDeclarator
	LineInfo
}

type DirectAbstractDeclarator interface { }

type AbstractDeclarator struct {
	Pointer *Pointer
	DirectAbstractDeclarator *DirectAbstractDeclarator
	LineInfo
}

type ParameterDeclaration struct {
	DeclarationSpecifiers *DeclarationSpecifiers
	Declarator *Declarator
	AbstractDeclarator *AbstractDeclarator
	LineInfo
}

type ParameterList struct {
	ParameterDeclarations []*ParameterDeclaration
	LineInfo
}

type ParameterTypeList struct {
	ParameterList *ParameterList
	LineInfo
	// TODO handle ELLIPSIS
}

type IdentifierList struct {
	Identifiers []string
	LineInfo
}

type DirectFunctionDeclarator struct {
	Declarator *DirectDeclarator
	ParameterTypeList *ParameterTypeList
	IndentifierList *IdentifierList
	LineInfo
}

type DirectArrayDeclarator struct {
	Declarator *DirectDeclarator
	ArrayExpression *Expression
	LineInfo
}

type DirectIdentifierDeclarator struct {
	Identifier string
	LineInfo
}

type DirectDeclarator interface {}


type TypeQualifierList struct {
	TypeQualifiers []*TypeQualifier
	LineInfo
}

type Pointer struct {
	TypeQualifierList *TypeQualifierList
	NestedPointer *Pointer
	LineInfo
}

type Declarator struct {
	DirectDeclarator *DirectDeclarator
	Pointer *Pointer
	LineInfo
}

type StructDeclarator struct {
	Declarator *Declarator
	Expression *Expression
	LineInfo
}

type StructDeclaratorList struct {
	StructDeclarators []*StructDeclarator
	LineInfo
}

type StructDeclaration struct {
	SpecifierQulifierList *SpecifierQulifierList
	StructDeclaratorList *StructDeclaratorList
	LineInfo
}

type StructDeclarationList struct {
	StructDeclarations []*StructDeclaration
	LineInfo
}

type Enumerator struct {
	Identifier string
	Expression *Expression
	LineInfo
}

type EnumeratorList struct {
	Enumerators []*Enumerator
	LineInfo
}

type StructTypeSpecifier struct {
	Identifier *string
	StructDeclarationList *StructDeclarationList
	LineInfo
}

type DirectTypeSpecifier struct {
	TypeName string
	LineInfo
}

type EnumTypeSpecifier struct {
	Identifier *string
	EnumeratorList *EnumeratorList
	LineInfo
}

type TypeSpecifier interface {}

type InitializerList struct {
	Initlizers []*Initializer
	LineInfo
}

type Initializer struct {
	Expression *Expression
	InitializerList *InitializerList
	LineInfo
}

type InitDeclarator struct {
	Declarator *Declarator
	Initializer *Initializer
	LineInfo
}

type InitDeclaratorList struct {
	InitDeclarators []*InitDeclarator
	LineInfo
}

type Declaration struct {
	DeclarationSpecifiers *DeclarationSpecifiers
	InitDeclaratorList *InitDeclaratorList
	LineInfo
}

type DeclarationList struct {
	Declarations []*Declaration
	LineInfo
}

