package utils

type Pair [T any] struct {
	First T
	Second T
}

func zip[T any](it1 []T, it2 []T) []Pair[T] {
	resLen := len(it1)
	if len(it2) < resLen {
		resLen = len(it2)
	}
	res := make([]Pair[T], resLen)
	for i := 0; i < resLen; i++ {
		res[i] = Pair[T]{it1[i], it2[i]}
	}
	return res
}

func all[T any](it []T, pred func (T) bool) bool {
	for _, el := range it {
		if !pred(el) {
			return false
		}
	}
	return true
}
