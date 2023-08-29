package irs

import "fmt"

type LabelT string

const (
	END_IF = "ENDIF"
	ELSE = "ELSE"
	BEGIN_WHILE = "WHILE"
	END_WHILE = "ENDWHILE"
	BEGIN_FOR = "FOR"
	END_FOR = "ENDFOR"
	END_SWITCH = "ENDSWITCH"
	CASE = "CASE"
	DEFAULT = "DEFAULT"
	TERNARY_ENDIF = "TENDIF"
	TERNARY_ELSE = "TELSE"
)

type LabelProvider struct {
	counters map[LabelT]int
	curFunName string
}

func newLabelProvider() *LabelProvider {
	return &LabelProvider{}
}

func (l *LabelProvider) EnterFunction(funName string) {
	l.counters = map[LabelT]int{}
	l.curFunName = funName
}

func (l *LabelProvider) Next(labelT LabelT) string {
	var count int
	if res, ok := l.counters[labelT]; ok {
		count = res
	} else {
		count = 0
	}
	l.counters[labelT] = count + 1
	return l.WithFunctionName(fmt.Sprintf("%d%s", count, string(labelT)))
}

func (l *LabelProvider) WithFunctionName(label string) string {
	return fmt.Sprintf("%s.%s", label, l.curFunName)
}
