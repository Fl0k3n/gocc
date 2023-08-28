package irs

import "fmt"

type LabelT string

const (
	END_IF = "ENDIF"
	ELSE = "ELSE"
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
	return fmt.Sprintf("%d%s.%s", count, string(labelT), l.curFunName)
}
