package linkers

import "elf"


type DependencyHelper struct {
	dependencyPaths []string
}

func newDependencyHelper(dependencyPaths []string) (*DependencyHelper, error) {
	return nil, nil
}

func (d *DependencyHelper) LookupSymbol(name string) (sym *elf.Symbol, ok bool) {
	return
}

