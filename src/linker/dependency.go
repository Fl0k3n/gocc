package linkers

import (
	"elf"
	"errors"
	"fmt"
	"os"
	"path"
	"strings"
	"utils"
)

type DependencyLUT struct {
	dynsym *elf.Symtab
	dynHash *elf.SymbolHashTab
	dynstr *elf.Strtab
}

func (d *DependencyLUT) Find(symbolName string) (idx uint32, sym *elf.Symbol, ok bool) {
	idx, ok = d.dynHash.Lookup(symbolName, d.dynsym, d.dynstr)
	if ok {
		sym = d.dynsym.GetSymbolWithIdx(idx)
	}
	return
}

type DependencyHelper struct {
	dependencyDirs []string
	dependencyShortNames []string
	shortNameToFullName map[string]string
	fullNameToFullPath map[string]string
	fileNameCoreToShortName map[string]string
	luts []DependencyLUT
}

func (d *DependencyHelper) findShortName(filename string) (res string, ok bool) {
	// TODO use trie or smth
	for fileNameCore, shortName := range d.fileNameCoreToShortName {
		if strings.HasPrefix(filename, fileNameCore) {
			return shortName, true
		}
	}
	return "", false
}

// TODO versioning
func (d *DependencyHelper) scanForDependencies() error {
	d.shortNameToFullName = map[string]string{}
	d.fullNameToFullPath = map[string]string{}

	d.fileNameCoreToShortName = map[string]string{}
	for _, name := range d.dependencyShortNames {
		d.fileNameCoreToShortName[fmt.Sprintf("lib%s.so", name)] = name
	}

	for _, dir := range d.dependencyDirs {
		files, err := os.ReadDir(dir)
		if err != nil {
			return err
		}
		if len(d.dependencyShortNames) == len(d.shortNameToFullName) {
			break
		}
		for _, file := range files {
			if !file.Type().IsRegular() {
				continue // TODO scan nested?
			}
			if shortName, ok := d.findShortName(file.Name()); ok {
				if _, ok := d.shortNameToFullName[shortName]; !ok {
					d.shortNameToFullName[shortName] = file.Name()
					d.fullNameToFullPath[file.Name()] = path.Join(dir, file.Name())
				}
			}
		}
	}

	if len(d.dependencyShortNames) != len(d.shortNameToFullName) {
		missing := []string{}
		for _, name := range d.dependencyShortNames {
			if _, ok := d.shortNameToFullName[name]; !ok {
				missing = append(missing, name)
			}
		}
		return errors.New("Failed to find dependencies: " + strings.Join(missing, ", "))
	}
	return nil
}

func newDependencyHelper(dependencyDirs []string, dependencyShortNames []string) (*DependencyHelper, error) {
	dh := &DependencyHelper{
		dependencyDirs: dependencyDirs,
		dependencyShortNames: dependencyShortNames,
	}
	err := utils.Pipeline().Then(dh.scanForDependencies).Then(dh.cacheDependencyData).Error()
	return dh, err
}

func (d *DependencyHelper) cacheDependencyData() error {
	d.luts = make([]DependencyLUT, len(d.fullNameToFullPath))
	i := 0
	for _, path := range d.fullNameToFullPath {
		_, dynsym, dynHash, dynstr, err := elf.NewDeserializer().DeserializeDynamicInfo(path)
		if err != nil {
			return err
		}
		d.luts[i] = DependencyLUT{
			dynsym: dynsym,
			dynHash: dynHash,
			dynstr: dynstr,
		}	
	}
	return nil
}

func (d *DependencyHelper) GetNamesForDynamicNeededEntries() []string {
	res := make([]string, len(d.shortNameToFullName))
	i := 0
	for _, fullName := range d.shortNameToFullName {
		res[i] = fullName
		i++
	}
	return res
}

func (d *DependencyHelper) LookupSymbol(name string) (sym *elf.Symbol, ok bool) {
	for _, lut := range d.luts {
		_, sym, ok = lut.Find(name)
		if ok {
			return
		}
	}
	return nil, false
}

