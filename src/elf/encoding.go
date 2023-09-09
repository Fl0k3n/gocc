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

func encodeUnsignedIntToLittleEndianU2(val interface{}) []byte{
	switch v := val.(type) {
	case uint8:
		return []byte{v}
	case uint16:
		return []byte{uint8(v & 0xFF), uint8((v >> 8) & 0xFF)}
	case uint32:
		return []byte{uint8(v & 0xFF), uint8((v >> 8) & 0xFF), uint8((v >> 16) & 0xFF), uint8((v >> 24) & 0xFF)}
	case uint64:
		return []byte{uint8(v & 0xFF), uint8((v >> 8) & 0xFF), uint8((v >> 16) & 0xFF), uint8((v >> 24) & 0xFF),
					  uint8((v >> 32) & 0xFF), uint8((v >> 40) & 0xFF), uint8((v >> 48) & 0xFF), uint8((v >> 56) & 0xFF)}
	default:
		panic("Unexpected type")
	}
}

func encodeUnsignedIntsToLittleEndianU2(res []byte, offset int, vals ...interface{}) {
	for _, field := range vals {
		encoded := encodeUnsignedIntToLittleEndianU2(field)
		for i, b := range encoded {
			res[offset + i] = b
		}
		offset += len(encoded)
	}
}