package ast

type ReturnJumpStatement struct {
	Expression *Expression
}

type LoopControlOption int
const (
	BREAK LoopControlOption = iota
	CONTINUE
)

type LoopControlJumpStatement struct {
	ControlOption LoopControlOption
}

type GotoJumpStatement struct {
	Label string
}

type JumpStatement interface {}

type ForIterationStatement struct {
	Initializer *ExpressionStatement
	Condition *ExpressionStatement
	Updater *Expression
	Body *Statement
}

type DoWhileIterationStatement struct {
	Body *Statement
	Condition *Expression
}

type WhileIterationStatement struct {
	Condition *Expression
	Body *Statement
}

type IterationStatement interface {}

type SwitchSelectionStatement struct {
	SwitchExpression *Expression
	SwitchBody *Statement
}

type IfSelectionStatement struct {
	Condition *Expression
	IfStatement *Statement
	ElseStatement *Statement
}

type SelectionStatement interface {}

type ExpressionStatement struct {
	Expression *Expression
}

type IdentifierLabeledStatement struct {
	Identifier string
	Statement *Statement
}

type CaseLabeledStatement struct {
	Expression *Expression
	Statement *Statement
}

type DefaultLabeledStatement struct {
	Statement *Statement
}

type LabeledStatement interface {}

type Statement interface {}

type StatementList struct {
	Statements []*Statement
}

type CompoundStatement struct {
	DeclarationList *DeclarationList
	StatementList *StatementList
}

type FunctionDefinition struct {
	DeclarationSpecifiers *DeclarationSpecifiers
	Declarator *Declarator
	DeclarationList *DeclarationList
	Body *CompoundStatement
}

type ExternalDeclaration interface {}

type TranslationUnit struct {
	ExternalDeclarations []*ExternalDeclaration
}
