package grammars

func ConvertAssignmentOpToBinaryOp(op string) string {
	return op[:len(op) - 1]	
}
