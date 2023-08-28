package semantics

type StatementContext struct {
	CanUseBreak bool
	ExpectsCase bool
	CaseExpressionType Ctype
	RequiredReturnType Ctype
}

func (sc StatementContext) WithAllowedBreak() StatementContext {
	sc.CanUseBreak = true
	return sc
}

func (sc StatementContext) WithDisallowedBreak() StatementContext {
	sc.CanUseBreak = false
	return sc
}

func (sc StatementContext) WithExpectedCase(caseExprType Ctype) StatementContext {
	sc.ExpectsCase = true
	sc.CaseExpressionType = caseExprType
	return sc
}

func (sc StatementContext) WithDisallowedCase() StatementContext {
	sc.ExpectsCase = true
	return sc
}

func (sc StatementContext) And() StatementContext {
	return sc
}
