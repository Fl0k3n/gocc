package ast

type ReturnJumpStatement struct {
	Expression Expression
	LineInfo
}

type LoopControlOption int
const (
	BREAK LoopControlOption = iota
	CONTINUE
)

type LoopControlJumpStatement struct {
	ControlOption LoopControlOption
	LineInfo
}

type GotoJumpStatement struct {
	Label string
	LineInfo
}

type JumpStatement interface {}

type ForIterationStatement struct {
	Initializer ExpressionStatement
	Condition ExpressionStatement
	Updater Expression
	Body Statement
	LineInfo
}

type DoWhileIterationStatement struct {
	Body Statement
	Condition Expression
	LineInfo
}

type WhileIterationStatement struct {
	Condition Expression
	Body Statement
	LineInfo
}

type IterationStatement interface {}

type SwitchSelectionStatement struct {
	SwitchExpression Expression
	SwitchBody Statement
	LineInfo
}

type IfSelectionStatement struct {
	Condition Expression
	IfStatement Statement
	ElseStatement Statement
	LineInfo
}

type SelectionStatement interface {}

type ExpressionStatement struct {
	Expression Expression
	LineInfo
}

type IdentifierLabeledStatement struct {
	Identifier string
	Statement Statement
	LineInfo
}

type CaseLabeledStatement struct {
	Expression Expression
	Statement Statement
	LineInfo
}

type DefaultLabeledStatement struct {
	Statement Statement
	LineInfo
}

type LabeledStatement interface {}

type Statement interface {}

type StatementList struct {
	Statements []Statement
	LineInfo
}

type CompoundStatement struct {
	DeclarationList *DeclarationList
	StatementList *StatementList
	LineInfo
}

type FunctionDefinition struct {
	DeclarationSpecifiers *DeclarationSpecifiers
	Declarator *Declarator
	DeclarationList *DeclarationList
	Body *CompoundStatement
	LineInfo
}

type ExternalDeclaration interface {}

type TranslationUnit struct {
	ExternalDeclarations []ExternalDeclaration
	LineInfo
}

