package utils

type Stack[T any] struct {
	data []T
}

func NewStack[T any]() *Stack[T] {
	return &Stack[T]{
		data: make([]T, 0),
	}
}

func (s *Stack[T]) Push(val T) {
	s.data = append(s.data, val)
}

func (s *Stack[T]) Size() int {
	return len(s.data)
}

func (s *Stack[T]) Pop() T {
	// optimize it if needed
	res := s.data[len(s.data) - 1]
	s.data = s.data[:len(s.data) - 1]
	return res
}

func (s *Stack[T]) PopMany(num int) {
	s.data = s.data[:len(s.data) - num]
}

func (s *Stack[T]) Peek() T {
	return s.data[len(s.data) - 1]
}