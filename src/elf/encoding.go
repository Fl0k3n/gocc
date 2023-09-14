package elf

func EncodeSymbolInfo(symbolBind SymbolBinding, symbolType SymbolType) uint8 {
	return (uint8(symbolBind) << 4) + (uint8(symbolType) & 0xF)
}

func GetSymbolBinding(symbolInfo uint8) SymbolBinding {
	return SymbolBinding(symbolInfo >> 4)
}

func GetSymbolType(symbolInfo uint8) SymbolType {
	return SymbolType(symbolInfo & 0xF)
}

func EncodeRelocationInfo(symbolIdx uint32, relocationType RelocationType) uint64 {
	return uint64(relocationType) | (uint64(symbolIdx) << 32) 
}

func GetSymbolIdx(relocInfo uint64) uint32 {
	return uint32(relocInfo >> 32)
}

func GetRelocationType(relocInfo uint64) RelocationType {
	return RelocationType(uint32(relocInfo))
}

func EncodeRelocationValue(v int32) []byte {
	return []byte{uint8(v & 0xFF), uint8((v >> 8) & 0xFF), uint8((v >> 16) & 0xFF), uint8((v >> 24) & 0xFF)}
}
