package tokenizers

import (
	"strings"
)

type Token struct {
	T string
	V string
}

// returns -1 if no singleline comment
func indexOfSingleLineComment(line string) int {
	return strings.Index(line, SINGLE_LINE_COMMENT)
}

// returns -1 if no multiline comment
func indexOfMultilineCommentStart(line string) int {
	return strings.Index(line, MULTI_LINE_COMMENT_START)
}

// returns -1 if no multiline comment
func indexOfMultilineCommentEnd(line string) int {
	return strings.Index(line, MULTI_LINE_COMMENT_END)
}

// assumes that it ends at this line
func removeFullyContainedMultilineCommentBetween(line string, startIdx int, endIdx int) string {
	lineSuffix := line[endIdx + len(MULTI_LINE_COMMENT_END):]
	return line[:startIdx] + lineSuffix
}

// line with removed comment and info if it was fully contained
func removeMultilineCommentIfContainedInLine(line string, startIdx int) (string, bool) {
	if endIdx := indexOfMultilineCommentEnd(line); endIdx != -1 {
		return removeFullyContainedMultilineCommentBetween(line, startIdx, endIdx), true
	} 
	return line[:startIdx], false
}

// returns line without comments and bool informing if multiline comment context starts in this line
func removeCommentsInLine(line string) (string, bool) {
	var fullyContained bool
	for {
		singleLineCommentIdx := indexOfSingleLineComment(line)
		multiLineCommentIdx := indexOfMultilineCommentStart(line)
		singleLineCommentPreceedsMultiline := singleLineCommentIdx != -1 && multiLineCommentIdx != -1 &&
											  singleLineCommentIdx < multiLineCommentIdx
		if (multiLineCommentIdx == -1 && singleLineCommentIdx != -1) || singleLineCommentPreceedsMultiline {
			line = line[:singleLineCommentIdx]
			break
		} else if multiLineCommentIdx != -1 {
			line, fullyContained = removeMultilineCommentIfContainedInLine(line, multiLineCommentIdx)
			if !fullyContained {
				return line, true
			}
		} else {
			break
		}
	} 
	return line, false
}

// returns len of input if predicate doesn't match any
func nextIndexHaving(line string, startInclusive int, predicate func(byte) bool) int {
	for i := startInclusive; i < len(line); i++ {
		if predicate(line[i]) {
			return i;
		}
	}
	return len(line)
}

func nextIndexNotHaving(line string, startInclusive int, predicate func(byte) bool) int {
	return nextIndexHaving(line, startInclusive, func(char byte) bool { return !predicate(char) })
}

// returns len of input if only spaces left
func nextIndexOfNotSpace(line string, startInclusive int) int {
	return nextIndexNotHaving(line, startInclusive, func(char byte) bool { return char == ' ' } )
}

func nextIndexOfNotToken(line string, startInclusive int) int {
	curChar := line[startInclusive]
	if curChar == '\'' || curChar == '"' {
		return nextIndexHaving(line, startInclusive + 1, func (char byte) bool { return char == curChar }) + 1
	}
	// TODO handle f float suffix, scientific notation, hex, etc...
	if isNumber(curChar) {
		// 123.123 or just 123, multidots should be handled by parser
		return nextIndexNotHaving(line, startInclusive+1, isNumberOrDot)
	}
	if curChar == '.' && startInclusive + 1 < len(line) && isNumber(line[startInclusive+1]) {
		// .123
		return nextIndexNotHaving(line, startInclusive+1, isNumber)
	}
	if isLetterOrUnderscore(curChar) {
		return nextIndexNotHaving(line, startInclusive+1, func (char byte) bool {return isNumber(char) || isLetterOrUnderscore(char) })
	}
	if startInclusive + 2 < len(line) && is3CharOperator(line[startInclusive:startInclusive+3]) {
		return startInclusive + 3
	}
	if startInclusive + 1 < len(line) && is2CharOperator(line[startInclusive:startInclusive+2]) {
		return startInclusive + 2
	}
	return startInclusive + 1
}

func isLetter(char byte) bool {
	return (char >= 97 && char <= 122) || (char >= 65 && char <= 90)
}

func isLetterOrUnderscore(char byte) bool {
	return isLetter(char) || char == '_'
}

func isNumber(char byte) bool {
	return char >= 48 && char <= 57
}

func isNumberOrDot(char byte) bool {
	return isNumber(char) || char == '.'
}

func isStringConstBoundary(char byte) bool {
	return char == '"'
}

func is2CharOperator(x string) bool {
	return DOUBLE_CHAR_OPERATORS.Has(x)
}

func is3CharOperator(x string) bool {
	return TRIPLE_CHAR_OPERATORS.Has(x)
}
