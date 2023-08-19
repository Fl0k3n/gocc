package types

import "ast"

func getTypeName(ts ast.TypeSpecifier) string {
	switch dts := ts.(type) {
	case ast.DirectTypeSpecifier:
		return dts.TypeName
	case ast.EnumTypeSpecifier:
		return *dts.Identifier
	case ast.StructTypeSpecifier:
		return *dts.Identifier
	}
	panic("Unexpected type specifier")
}

func getLineInfo(ts ast.TypeSpecifier) ast.LineInfo {
	switch dts := ts.(type) {
	case ast.DirectTypeSpecifier:
		return dts.LineInfo
	case ast.EnumTypeSpecifier:
		return dts.LineInfo
	case ast.StructTypeSpecifier:
		return dts.LineInfo
	}
	panic("Unexpected type specifier")
}

func getPointersLowestLevel(ptr *PointerCtype) *PointerCtype {
	lowestLvlPtr := ptr
	for {
		if nestedPtr, isNested := lowestLvlPtr.Target.(PointerCtype); isNested {
			lowestLvlPtr = &nestedPtr
		} else {
			break
		}
	}
	return lowestLvlPtr
}

func getPenultimatePointersLevel(ptr *PointerCtype) *PointerCtype {
	var prev *PointerCtype = nil
	lowestLvlPtr := ptr
	for {
		if nestedPtr, isNested := lowestLvlPtr.Target.(PointerCtype); isNested {
			prev = lowestLvlPtr
			lowestLvlPtr = &nestedPtr
		} else {
			break
		}
	}
	return prev

}

func evalConstantIntegerExpression(expr ast.Expression) (int, error) {
	return 0, nil
}