package utils


type Queue[T any] struct {
	head *Node[T]
	tail *Node[T]
	size int
}

func NewQueue[T any]() *Queue[T] {
	return &Queue[T]{
		head: nil,
		tail: nil,
		size: 0,
	}
}

func (q *Queue[T]) Push(val T) {
	newNode := &Node[T]{
		data: val,
		next: nil,
		prev: q.tail,
	}
	q.size++

	if q.head == nil {
		q.head = newNode
	} else {
		q.tail.next = newNode
	}
	q.tail = newNode
}

func (q *Queue[T]) Size() int {
	return q.size
}

func (q *Queue[T]) Pop() T {
	if q.size == 0 {
		panic("Pop from empty queue")
	}
	res := q.head.data

	if q.size == 1 {
		q.head = nil
		q.tail = nil
	} else {
		nxt := q.head.next
		nxt.prev = nil
		q.head = nxt
	}

	q.size--
	return res
}
