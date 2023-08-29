package semantics

import (
	"ast"
	"errors"
	"fmt"
)

type ErrorInfo struct {
	err error
	line ast.LineInfo
}

type ErrorTracker struct {
	typeErrors []ErrorInfo
	semanticErrors []ErrorInfo
}

func NewErrorTracker() *ErrorTracker {
	return &ErrorTracker{
		typeErrors: []ErrorInfo{},
		semanticErrors: []ErrorInfo{},
	}
}

func (et *ErrorTracker) HasError() bool {
	return len(et.typeErrors) > 0 || len(et.semanticErrors) > 0
}

func (et *ErrorTracker) PrintErrors() {
	if len(et.semanticErrors) > 0 {
		fmt.Println("Semantic errors:")
		for _, err := range et.semanticErrors {
			fmt.Printf("In %d:\n\t%s\n", err.line.LineNumber, err.err.Error())
		}
		if len(et.typeErrors) > 0 {
			fmt.Println("******************************************")
		}
	} 
	if len(et.typeErrors) > 0 {
		fmt.Println("Type errors:")
		for _, err := range et.typeErrors {
			fmt.Printf("In %d:\n\t%s\n", err.line.LineNumber, err.err.Error())
		}
	}
}

func (et *ErrorTracker) registerTypeError(err string, line ast.LineInfo) {
	et.typeErrors = append(et.typeErrors, ErrorInfo{err: errors.New(err), line: line})
}

func (et *ErrorTracker) registerSemanticError(err string, line ast.LineInfo) {
	et.semanticErrors = append(et.semanticErrors, ErrorInfo{err: errors.New(err), line: line})
}
