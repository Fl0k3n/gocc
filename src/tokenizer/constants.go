package tokenizers

import "utils"

const SINGLE_LINE_COMMENT = "//"
const MULTI_LINE_COMMENT_START = "/*"
const MULTI_LINE_COMMENT_END = "*/"
const EOF = "$"

const UNKNOWN_TOKEN = "___UNKNOWN___"

var EOF_TOKEN = Token{T: EOF}

var DOUBLE_CHAR_OPERATORS = utils.SetOf[string](
	"==", "||", "&&", "++", "--", 
	"->", "!=", ">=", "<=", "<<",
	">>", "+=", "-=", "*=", "/=",
	"%=", "&=", "^=", "|=",
)

 
var TRIPLE_CHAR_OPERATORS = utils.SetOf[string](
	">>=", "<<=",
)
