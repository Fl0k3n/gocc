package linkers

import "elf"


type DependencyHelper struct {
	dependencyDirs []string
	dependencyShortNames []string
	shortNameToFullName map[string]string
	fullNameToFullPath map[string]string
}

func scanForDependencies(
	dirs []string,
	shortNames []string,
) (shortNameToFullName map[string]string, fullNameToFullPath map[string]string, err error) {
	return map[string]string{
		"s3": "libs3.so.0",
	}, map[string]string{
		"libs3.so.0": "/home/flok3n/develop/from_scratch/gocc/resources/csrc/link/simple/libs3.so.0.0",
	}, nil
}

func newDependencyHelper(dependencyDirs []string, dependencyShortNames []string) (*DependencyHelper, error) {
	shortNameToFullNames, fullNameToFullPath, err := scanForDependencies(dependencyDirs, dependencyShortNames)
	if err != nil {
		return nil, err
	}
	return &DependencyHelper{
		dependencyDirs: dependencyDirs,
		dependencyShortNames: dependencyShortNames,
		shortNameToFullName: shortNameToFullNames,
		fullNameToFullPath: fullNameToFullPath,
	}, nil
}

func (d *DependencyHelper) GetNamesForDynamicNeededEntries() []string {
	// return []string{}
	res := make([]string, len(d.shortNameToFullName))
	i := 0
	for _, fullName := range d.shortNameToFullName {
		res[i] = fullName
		i++
	}
	return res
}

func (d *DependencyHelper) LookupSymbol(name string) (sym *elf.Symbol, ok bool) {
	return
}

