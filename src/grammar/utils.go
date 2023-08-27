package grammars

func ConvertAssignmentOpToBinaryOp(op string) string {
	var binaryOp string
	switch op {
	case "MUL_ASSIGN":
		binaryOp = "*"
	case "DIV_ASSIGN":
		binaryOp = "/"
	case "MOD_ASSIGN":
		binaryOp = "%"
	case "ADD_ASSIGN":
		binaryOp = "+"
	case "SUB_ASSIGN":
		binaryOp = "-"
	case "LEFT_ASSIGN":
		binaryOp = "LEFT_OP"
	case "RIGHT_ASSIGN":
		binaryOp = "RIGHT_OP"
	case "AND_ASSIGN":
		binaryOp = "AND_OP"
	case "XOR_ASSIGN":
		binaryOp = "^"
	case "OR_ASSIGN":
		binaryOp = "OR_OP"
	default:
		panic("Unexpected assignment op: " + op)
	}
	return binaryOp
}