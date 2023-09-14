package utils

func EncodeUnsignedIntToLittleEndianU2(val interface{}) []byte{
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

func DecodeUnsignedIntFromLittleEndianU2(buff []byte, buffOffset int, resPtr interface{}) (size int) {
	switch v := resPtr.(type) {
	case *uint8:
		*v = buff[buffOffset]
		return 1
	case *uint16:
		*v = uint16(buff[buffOffset]) | (uint16(buff[buffOffset + 1]) << 8)
		return 2
	case *uint32:
		*v = uint32(buff[buffOffset]) | (uint32(buff[buffOffset + 1]) << 8) |
			(uint32(buff[buffOffset + 2]) << 16) | (uint32(buff[buffOffset + 3]) << 24)
		return 4
	case *uint64:
		*v = uint64(buff[buffOffset]) | (uint64(buff[buffOffset + 1]) << 8) |
			(uint64(buff[buffOffset + 2]) << 16) | (uint64(buff[buffOffset + 3]) << 24) |
			(uint64(buff[buffOffset + 4]) << 32) | (uint64(buff[buffOffset + 5]) << 40) | 
			(uint64(buff[buffOffset + 6]) << 48) | (uint64(buff[buffOffset + 7]) << 56)
		return 8
	default:
		panic("Unexpected type")
	}
}

func EncodeIntToLittleEndianU2(val interface{}) []byte {
	switch v := val.(type) {
	case int8:
		return []byte{uint8(v)}
	case int16:
		return []byte{uint8(v & 0xFF), uint8((v >> 8) & 0xFF)}
	case int32:
		return []byte{uint8(v & 0xFF), uint8((v >> 8) & 0xFF), uint8((v >> 16) & 0xFF), uint8((v >> 24) & 0xFF)}
	case int64:
		return []byte{uint8(v & 0xFF), uint8((v >> 8) & 0xFF), uint8((v >> 16) & 0xFF), uint8((v >> 24) & 0xFF),
					  uint8((v >> 32) & 0xFF), uint8((v >> 40) & 0xFF), uint8((v >> 48) & 0xFF), uint8((v >> 56) & 0xFF)}
	default:
		panic("Unexpected type")
	}
}

func DecodeIntFromLittleEndianU2(buff []byte, buffOffset int, resPtr *int64) {
	// consider using unsafa pckg and just memcpy in reverse order
	*resPtr = int64(buff[buffOffset]) | (int64(buff[buffOffset + 1]) << 8) |
		(int64(buff[buffOffset + 2]) << 16) | (int64(buff[buffOffset + 3]) << 24) |
		(int64(buff[buffOffset + 4]) << 32) | (int64(buff[buffOffset + 5]) << 40) | 
		(int64(buff[buffOffset + 6]) << 48) | (int64(buff[buffOffset + 7]) << 56)
}

func EncodeUnsignedIntsToLittleEndianU2(res []byte, offset int, vals ...interface{}) {
	for _, field := range vals {
		encoded := EncodeUnsignedIntToLittleEndianU2(field)
		for i, b := range encoded {
			res[offset + i] = b
		}
		offset += len(encoded)
	}
}

func DecodeUnsignedIntsFromLittleEndianU2(buff []byte, offset int, resPtrs ...interface{}) {
	for _, ptr := range resPtrs {
		size := DecodeUnsignedIntFromLittleEndianU2(buff, offset, ptr)
		offset += size
	}
}