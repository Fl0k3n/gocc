package utils


type Node[T any] struct {
	next *Node[T]
	prev *Node[T]
	data T
}

type BoundedList[T any] struct {
	head *Node[T]
	tail *Node[T]
	Size int
	Bound int
}

func NewBoundedList[T any](bound int) (*BoundedList[T]) {
	if bound < 1 {
		panic("Bound must be positive")
	}
	return &BoundedList[T]{
		head: nil,
		tail: nil,
		Size: 0,
		Bound: bound,
	}
}

// assumes is not empty
func (list *BoundedList[T]) popOldest() {
	newHead := list.head.next
	list.Size--
	if newHead == nil {
		list.head = nil
		list.tail = nil
		return
	}

	newHead.prev = nil
	list.head = newHead
}

func (list *BoundedList[T]) Append(data T) {
	if list.Size == list.Bound {
		list.popOldest()
	} 
	list.Size++

	newNode := &Node[T]{
		data: data,
		next: nil,
		prev: list.tail,
	}

	if list.head == nil {
		list.head = newNode
		list.tail = newNode
	} else {
		list.tail.next = newNode
		list.tail = newNode
	}
}

// panics if empty
func (list *BoundedList[T]) Newest() T {
	if list.Size == 0 {
		panic("Taking value from empty list")
	}
	return list.tail.data
}

// NthNewest(0) == Newest()
func (list *BoundedList[T]) NthNewest(n int) T {
	if n >= list.Size {
		panic("Don't have this many values")
	}

	cur := list.tail
	for i := 0; i < n; i++ {
		cur = cur.prev	
	}
	return cur.data
}
