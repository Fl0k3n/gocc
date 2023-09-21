package elf

import "utils"

const DYNAMIC_ENTRY_SIZE = 16


type DynamicTag = uint64

const (
	DT_NULL DynamicTag = iota
	DT_NEEDED; DT_PLTRELSZ; DT_PLTGOT
	DT_HASH; DT_STRTAB; DT_SYMTAB
	DT_RELA; DT_RELASZ; DT_RELAENT
	DT_STRSZ; DT_SYMENT; DT_INIT
	DT_FINI; DT_SONAME; DT_RPATH
	DT_SYMBOLIC; DT_REL; DT_RELSZ
	DT_RELENT; DT_PLTREL; DT_DEBUG
	DT_TEXTREL; DT_JMPREL; DT_BIND_NOW
)

type DynamicEntry struct {
	Dtag DynamicTag
	DvalOrPtr uint64
}

type DynamicTab struct {
	entries []DynamicEntry
}

func NewDynamicTab() *DynamicTab {
	return &DynamicTab{
		entries: []DynamicEntry{},
	}
}

func (d *DynamicTab) Alloc(size int) {
	if len(d.entries) != 0 {
		panic("Can alloc only for empty dynamic tabs")
	}
	d.entries = make([]DynamicEntry, size)
}

func (d *DynamicTab) AddDynamicEntry(tag DynamicTag, val uint64) int {
	d.entries = append(d.entries, DynamicEntry{
		Dtag: tag,
		DvalOrPtr: val,
	})
	return len(d.entries) - 1
}

func (d *DynamicTab) GetEntry(idx int) *DynamicEntry {
	return &d.entries[idx]
}

func (d *DynamicTab) SetDynamicEntry(idx int, tag DynamicTag, val uint64) {
	d.entries[idx].Dtag = tag
	d.entries[idx].DvalOrPtr = val
}

func (d *DynamicTab) ToBytes() []byte {
	res := make([]byte, d.BinarySize())	
	offset := 0
	for _, entry := range d.entries {
		utils.EncodeUnsignedIntsToLittleEndianU2(res, offset, entry.Dtag, entry.DvalOrPtr)
		offset += DYNAMIC_ENTRY_SIZE
	}
	return res
}

func (d *DynamicTab) BinarySize() int {
	return len(d.entries) * DYNAMIC_ENTRY_SIZE
}
