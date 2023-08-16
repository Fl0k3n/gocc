package parsers

type ASTNode interface {}


type TypeName struct {
	SpecifierQulifierList *SpecifierQulifierList
	AbstractDeclarator *AbstractDeclarator
}

type TypeQualifier string

type SpecifierQulifierList struct {
	TypeSpecifiers []*TypeSpecifier
	TypeQualifiers []*TypeQualifier
}

type StorageClassSpecifier string

type DeclarationSpecifiers struct {
	TypeSpecifiers []*TypeSpecifier
	TypeQualifiers []*TypeQualifier
	StorageClassSpecifiers []*StorageClassSpecifier
}

type DirectAbstractFunctionDeclarator struct {
	ParameterTypeList *ParameterTypeList
	DirectAbstractDeclarator *DirectAbstractDeclarator
}

type DirectAbstractArrayDeclarator struct {
	Expression *Expression
	DirectAbstractDeclarator *DirectAbstractDeclarator
}

type DirectAbstractDeclarator interface { }

type AbstractDeclarator struct {
	Pointer *Pointer
	DirectAbstractDeclarator *DirectAbstractDeclarator
}

type ParameterDeclaration struct {
	DeclarationSpecifiers *DeclarationSpecifiers
	Declarator *Declarator
	AbstractDeclarator *AbstractDeclarator
}

type ParameterList struct {
	ParameterDeclarations []*ParameterDeclaration
}

type ParameterTypeList struct {
	ParameterList *ParameterList
	// TODO handle ELLIPSIS
}

type IdentifierList struct {
	Identifiers []string
}

type DirectFunctionDeclarator struct {
	Declarator *DirectDeclarator
	ParameterTypeList *ParameterTypeList
	IndentifierList *IdentifierList
}

type DirectArrayDeclarator struct {
	Declarator *DirectDeclarator
	ArrayExpression *Expression
}

type DirectIdentifierDeclarator struct {
	Identifier string
}

type DirectDeclarator interface {}


type TypeQualifierList struct {
	TypeQualifiers []*TypeQualifier
}

type Pointer struct {
	TypeQualifierList *TypeQualifierList
	NestedPointer *Pointer
}

type Declarator struct {
	DirectDeclarator *DirectDeclarator
	Pointer *Pointer
}

type StructDeclarator struct {
	Declarator *Declarator
	Expression *Expression
}

type StructDeclaratorList struct {
	StructDeclarators []*StructDeclarator
}

type StructDeclaration struct {
	SpecifierQulifierList *SpecifierQulifierList
	StructDeclaratorList *StructDeclaratorList
}

type StructDeclarationList struct {
	StructDeclarations []*StructDeclaration
}

type Enumerator struct {
	Identifier string
	Expression *Expression
}

type EnumeratorList struct {
	Enumerators []*Enumerator
}

type StructTypeSpecifier struct {
	Identifier *string
	StructDeclarationList *StructDeclarationList
}

type DirectTypeSpecifier struct {
	TypeName string
}

type EnumTypeSpecifier struct {
	Identifier *string
	EnumeratorList *EnumeratorList
}

type TypeSpecifier interface {}

type InitializerList struct {
	Initlizers []*Initializer
}

type Initializer struct {
	Expression *Expression
	InitializerList *InitializerList
}

type InitDeclarator struct {
	Declarator *Declarator
	Initializer *Initializer
}

type InitDeclaratorList struct {
	InitDeclarators []*InitDeclarator
}

type Declaration struct {
	DeclarationSpecifiers *DeclarationSpecifiers
	InitDeclaratorList *InitDeclaratorList
}

type DeclarationList struct {
	Declarations []*Declaration
}

