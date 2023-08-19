package symtab

import "utils"

type Symbol[T any] struct {
	Name string
	Info T
}

type SymtabScope [T any] struct {
	tab map[string]T
}

func (ss *SymtabScope[T]) Lookup(symname string) (T, bool) {
	res, ok := ss.tab[symname]
	return res, ok
}

func (ss *SymtabScope[T]) Define(symname string, info T) {
	ss.tab[symname] = info
}

func (ss *SymtabScope[T]) GetAll() []Symbol[T] {
	res := make([]Symbol[T], len(ss.tab))
	idx := 0
	for k, v := range ss.tab {
		res[idx] = Symbol[T]{
			Name: k,
			Info: v,
		}
		idx++
	}
	return res
}

func newScope[T any]() *SymtabScope[T] {
	return &SymtabScope[T]{
		tab: make(map[string]T),
	}
}

type Symtab [T any] struct {
	scopeStack *utils.Stack[*SymtabScope[T]]
}

func NewSymtab[T any]() *Symtab[T] {
	return &Symtab[T]{
		scopeStack: utils.NewStack[*SymtabScope[T]](),
	}
}

func (s *Symtab[T])	Lookup(symname string) (res T, ok bool) {
	for i := 0; i < s.scopeStack.Size(); i++ {
		if res, ok := s.scopeStack.GetNthFifo(i).Lookup(symname); ok {
			return res, true
		}
	}
	return res, false
}

func (s *Symtab[T]) Define(symname string, info T) {
	s.scopeStack.Peek().Define(symname, info)
}

func (s *Symtab[T]) EnterScope() {
	scope := newScope[T]()
	s.scopeStack.Push(scope)
}

func (s *Symtab[T])	LeaveScope() {
	s.scopeStack.Pop()
}

