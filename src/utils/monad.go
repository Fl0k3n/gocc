package utils

type ThenFunc func() error

type Monad struct {
	err error
}

func Pipeline() Monad {
	return Monad{err: nil}
}

func (m Monad) Error() error {
	return m.err
}

func (m Monad) Then(f ThenFunc) Monad {
	if m.err != nil {
		return Monad{m.err}
	}
	return Monad{err: f()}
}
