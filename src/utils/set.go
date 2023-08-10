package utils

type Set [T comparable] struct {
	data map[T]struct{}
}

func NewSet[T comparable]() *Set[T] {
	return &Set[T]{
		data: make(map[T]struct{}),
	}
}

func (s *Set[T]) Add(val T) {
	s.data[val] = struct{}{}
}

func (s *Set[T]) GetAll() []T {
	res := make([]T, 0, len(s.data))
	for key := range s.data {
		res = append(res, key)
	}
	return res
}

func (s *Set[T]) AddAll(vals []T) {
	for _, v := range vals {
		s.data[v] = struct{}{}
	}
}

func (s* Set[T]) Has(val T) bool {
	_, ok := s.data[val]
	return ok
}
