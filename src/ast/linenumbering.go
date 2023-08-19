package ast

import (
	"reflect"
)


type LineInfoNode interface {
	GetChildren() []LineInfoNode
	GetLineBounds() (min int, max int)
}

type LineInfo struct {
	LineNumber int
	Owner Node
}

func (li LineInfo) GetChildren() []LineInfoNode {
	res := make([]LineInfoNode, 0)
	for _, f := range reflect.VisibleFields(reflect.Indirect(reflect.ValueOf(li.Owner)).Type()) {
		if f.Name == "Owner" {
			continue
		}
		if f.Type.Kind() == reflect.Ptr {
			val := reflect.Indirect(reflect.Indirect(reflect.ValueOf(li.Owner))).FieldByName(f.Name)
			if v, implements := val.Interface().(LineInfoNode); implements {
				// need to use reflection to check for nil value, probably not needed as nil doesnt pass type assertion
				// if (reflect.Indirect(reflect.ValueOf(v)) != reflect.Value{}) {
				res = append(res, v)
				// } 
			}
		} else if f.Type.Kind() == reflect.Slice {
			slice := reflect.Indirect(reflect.Indirect(reflect.ValueOf(li.Owner))).FieldByName(f.Name)
			for i := 0; i < slice.Len(); i++ {
				elem := slice.Index(i)
				if v, implements := elem.Interface().(LineInfoNode); implements {
					res = append(res, v)
				} else {
					break
				}
			}
		}
	}
	return res
}

func (li LineInfo) GetLineBounds() (min int, max int) {
	min = li.LineNumber
	max = li.LineNumber
	for _, child := range li.GetChildren() {
		cMin, cMax := child.GetLineBounds()
		if cMin < min {
			min = cMin
		}
		if cMax > max {
			max = cMax
		}
	}
	return min, max
}
