package utils

import "fmt"

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

// returns new set that contains union of items in this and other set
func (s *Set[T]) Union(other *Set[T]) *Set[T] {
	res := NewSet[T]()
	res.AddAll(s.GetAll())
	res.AddAll(other.GetAll())
	return res
}

func (s* Set[T]) Has(val T) bool {
	_, ok := s.data[val]
	return ok
}

func (s* Set[T]) Size() int {
	return len(s.data)
}

// returs copy of the set with vals added
func (s* Set[T]) With(vals ...T) *Set[T] {
	res := NewSet[T]()
	res.AddAll(vals)
	for k := range s.data {
		res.Add(k)
	}
	return res
}

func (s* Set[T]) Equals(other *Set[T]) bool {
	if s.Size() != other.Size() {
		return false
	}
	for k := range s.data {
		if !other.Has(k) {
			return false
		}
	}
	return true
}

func (s *Set[T]) Difference(other *Set[T]) *Set[T] {
	res := NewSet[T]()
	for k := range s.data {
		if !other.Has(k) {
			res.Add(k)
		}
	}
	return res
}

func (s *Set[T]) IsEmpty() bool {
	return len(s.data) == 0
}

func (s *Set[T]) Print() {
	vals := make([]T, 0)
	for k := range s.data {
		vals = append(vals, k)
	}
	fmt.Println(vals)
}

func SetOf[T comparable](vals ...T) *Set[T] {
	s := NewSet[T]()
	s.AddAll(vals)
	return s
}
