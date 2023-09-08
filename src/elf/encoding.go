package elf

func encodeSymbolInfo(symbolBind SymbolBinding, symbolType SymbolType) uint8 {
	return (uint8(symbolBind) << 4) + (uint8(symbolType) & 0xF)
}

func getSymbolBinding(symbolInfo uint8) SymbolBinding {
	return SymbolBinding(symbolInfo >> 4)
}

func getSymbolType(symbolInfo uint8) SymbolType {
	return SymbolType(symbolInfo & 0xF)
}
