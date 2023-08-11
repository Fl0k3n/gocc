package tokenizers

import (
	"bufio"
	"os"
	"strings"
	"utils"
)

const BUFF_BOUND = 8

type Tokenizer struct {
	buff *utils.BoundedList[string]
	inputPath string
	inputFile *os.File
	inputScanner *bufio.Scanner
	LineIdx int
	currentLine string
	lineParseIdx int
	multilineCommentInContext bool
	panicOnFailedAdvance bool
	moveBackRequestsCounter int
}

func New(inputPath string) (*Tokenizer, error) {
	file, err := os.Open(inputPath)
	if err != nil {
		return nil, err
	}

	return &Tokenizer{
		inputPath:  inputPath,
		inputFile:  file,
		inputScanner: bufio.NewScanner(file),
		buff: utils.NewBoundedList[string](BUFF_BOUND),
		LineIdx: 0,
		currentLine: "",
		lineParseIdx: 0,
		multilineCommentInContext: false,
		moveBackRequestsCounter: 0,
	}, nil
}

func (this *Tokenizer) Finish() {
	this.inputFile.Close()
}

func (this *Tokenizer) readNextNonCommentLine() bool {
	for this.inputScanner.Scan() {
		this.LineIdx++
		line := strings.TrimSpace(this.inputScanner.Text())

		if this.multilineCommentInContext {
			if idx := indexOfMultilineCommentEnd(line); idx != -1 {
				line = line[idx + len(MULTI_LINE_COMMENT_END):]
				this.multilineCommentInContext = false
			} else {
				continue
			}
		}

		line, multilineCommentInContext := removeCommentsInLine(line)
		this.multilineCommentInContext = multilineCommentInContext

		if len(line) > 0 {
			this.currentLine = line
			return true
		}
	}
	return false
}

func (this *Tokenizer) getNextToken() string {
	startIdx := this.lineParseIdx
	lastIdx := nextIndexOfNotToken(this.currentLine, startIdx)
	token := this.currentLine[startIdx:lastIdx]
	this.lineParseIdx = nextIndexOfNotSpace(this.currentLine, lastIdx)
	return token
}

func (this *Tokenizer) MoveBack() {
	this.moveBackRequestsCounter++
	if this.moveBackRequestsCounter > this.buff.Size || this.moveBackRequestsCounter > this.buff.Bound {
		panic("Can't move back further, not enought tokens were read")
	}
}

func (this *Tokenizer) Lookahead() string {
	this.Advance()
	token := this.LastToken()
	this.MoveBack()
	return token
}

func (this *Tokenizer) Advance() {
	if this.moveBackRequestsCounter > 0 {
		this.moveBackRequestsCounter--
		return
	}

	if this.lineParseIdx >= len(this.currentLine) {
		if !this.readNextNonCommentLine() {
			this.buff.Append(EOF)
			return
		}
		this.lineParseIdx = 0
	}

	token := this.getNextToken()
	this.buff.Append(token)
}

func (this *Tokenizer) LastToken() string {
	return this.buff.NthNewest(this.moveBackRequestsCounter)
}
