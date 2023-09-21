package elf

import "utils"

const HASH_ENTRY_SIZE = 4

type SymbolHashTab struct {
	Buckets []uint64
	Chains []uint64
}

// https://www.uclibc.org/docs/elf-64-gen.pdf p17
func hashSymbolName(name string) uint64 {
	var h uint64 = 0
	var g uint64
	for _, nameByte := range []byte(name) {
		h = (h << 4) + uint64(nameByte)
		if g == h & 0xf0000000 {
			h ^= g >> 24
		}
		h &= 0x0fffffff
	}
	return h
}

func BuildSymbolHashTab(nbuckets uint64, symtab *Symtab, strtab *Strtab) *SymbolHashTab {
	buckets := make([]uint64, nbuckets)
	bucketEnds := make([]uint64, nbuckets)
	chains := make([]uint64, symtab.Size())

	for idx := range buckets {
		buckets[idx] = 0
	}
	for idx := range chains {
		chains[idx] = 0
	}
	bucketEnds[0] = 0

	for idx := uint64(1); idx < uint64(symtab.Size()); idx++ {
		symbol := symtab.symbols[idx]
		name := strtab.GetStringForIndex(symbol.Sname)

		hash := hashSymbolName(name)
		bucketIdx := hash % nbuckets

		if bucketIdx != 0 && buckets[bucketIdx] == 0 {
			buckets[bucketIdx] = idx
			bucketEnds[bucketIdx] = idx
			chains[idx] = idx
		} else {
			lastChainIdx := bucketEnds[bucketIdx]
			chains[lastChainIdx] = idx
			bucketEnds[bucketIdx] = idx
		}
	}

	return &SymbolHashTab{
		Buckets: buckets,
		Chains: chains,
	}
}

func (s *SymbolHashTab) Lookup(symName string, symtab *Symtab, strtab *Strtab) (res uint64, ok bool) {
	bucketIdx := hashSymbolName(symName) % uint64(len(s.Buckets))
	chainIdx := s.Buckets[bucketIdx]
	for {
		if chainIdx == 0 {
			return 0, false
		}
		sname := strtab.GetStringForIndex(symtab.GetSymbolWithIdx(uint32(chainIdx)).Sname)
		if sname == symName {
			return chainIdx, true
		}
		if s.Chains[chainIdx] == chainIdx {
			return 0, false
		}
		chainIdx = s.Chains[chainIdx]
	}
}

func (s *SymbolHashTab) ToBytes() []byte {
	const QWORD_SIZE = 8
	res := make([]byte, s.BinarySize())
	utils.EncodeUnsignedIntsToLittleEndianU2(res, 0, uint64(len(s.Buckets)), uint64(len(s.Chains)))
	offset := 2 * QWORD_SIZE
	// TODO memcpy
	for _, bucket := range s.Buckets {
		utils.EncodeUnsignedIntsToLittleEndianU2(res, offset, bucket)
		offset += QWORD_SIZE
	}
	for _, chain := range s.Chains {
		utils.EncodeUnsignedIntsToLittleEndianU2(res, offset, chain)
		offset += QWORD_SIZE
	}
	return res
}

func (s *SymbolHashTab) BinarySize() int {
	const QWORD_SIZE = 8
	return (2 + len(s.Buckets) + len(s.Chains)) * QWORD_SIZE
}
