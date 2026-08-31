fp-native dump: format=MachO arch=Aarch64 entry=none

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global CODE ty=I64 constant=true initializer=Some(Bytes([2, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_0 ty=Array(I8, 6) constant=true initializer=Some(Bytes([112, 111, 105, 110, 116, 0]))
global __const_data_1 ty=Array(I8, 7) constant=true initializer=Some(Bytes([99, 105, 114, 99, 108, 101, 0]))
global __const_data_2 ty=Array(I8, 10) constant=true initializer=Some(Bytes([114, 101, 99, 116, 97, 110, 103, 108, 101, 0]))
fn value_code
  bb0 bb0
    alloca Virtual { id: 2, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 3, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    alloca Virtual { id: 5, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 6, bank: General, size_bits: 64 }, Virtual { id: 3, bank: General, size_bits: 64 }
    load Virtual { id: 7, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 6, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 8, bank: General, size_bits: 8 }, Virtual { id: 7, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 8, bank: General, size_bits: 8 }
    load Virtual { id: 10, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 11, bank: General, size_bits: 8 }, Virtual { id: 10, bank: General, size_bits: 8 }, 1
    condbr
  bb2 bb2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    br
  bb3 bb3
    alloca Virtual { id: 13, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 14, bank: General, size_bits: 64 }, Virtual { id: 3, bank: General, size_bits: 64 }
    load Virtual { id: 15, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 14, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 16, bank: General, size_bits: 8 }, Virtual { id: 15, bank: General, size_bits: 64 }, 2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 13, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 16, bank: General, size_bits: 8 }
    load Virtual { id: 18, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 13, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 19, bank: General, size_bits: 8 }, Virtual { id: 18, bank: General, size_bits: 8 }, 1
    condbr
  bb1 bb1
    load Virtual { id: 20, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb4 bb4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 2
    br
  bb5 bb5
    alloca Virtual { id: 22, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 23, bank: General, size_bits: 64 }, Virtual { id: 3, bank: General, size_bits: 64 }
    load Virtual { id: 24, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 23, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 25, bank: General, size_bits: 8 }, Virtual { id: 24, bank: General, size_bits: 64 }, 5
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 22, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 25, bank: General, size_bits: 8 }
    load Virtual { id: 27, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 22, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 28, bank: General, size_bits: 8 }, Virtual { id: 27, bank: General, size_bits: 8 }, 1
    condbr
  bb6 bb6
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 5
    br
  bb7 bb7
    br
fn Shape__describe
  bb0 bb0
    alloca Virtual { id: 30, bank: General, size_bits: 64 }, 16
    alloca Virtual { id: 31, bank: General, size_bits: 64 }, 24
    load Virtual { id: 32, bank: General, size_bits: 64 }, symbol(frame.local.1)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 31, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 32, bank: General, size_bits: 64 }
    alloca Virtual { id: 34, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 35, bank: General, size_bits: 64 }, Virtual { id: 31, bank: General, size_bits: 64 }
    load Virtual { id: 36, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 35, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 37, bank: General, size_bits: 8 }, Virtual { id: 36, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 34, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 37, bank: General, size_bits: 8 }
    load Virtual { id: 39, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 34, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 40, bank: General, size_bits: 8 }, Virtual { id: 39, bank: General, size_bits: 8 }, 1
    condbr
  bb2 bb2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 30, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb3 bb3
    alloca Virtual { id: 42, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 43, bank: General, size_bits: 64 }, Virtual { id: 31, bank: General, size_bits: 64 }
    load Virtual { id: 44, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 43, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 45, bank: General, size_bits: 8 }, Virtual { id: 44, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 42, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 45, bank: General, size_bits: 8 }
    load Virtual { id: 47, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 42, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 48, bank: General, size_bits: 8 }, Virtual { id: 47, bank: General, size_bits: 8 }, 1
    condbr
  bb1 bb1
    load Virtual { id: 49, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 30, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb4 bb4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 30, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb5 bb5
    br
  bb6 bb6
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 30, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb7 bb7
    load Virtual { id: 52, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 30, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret


Symbols:
  value_code                       0x00000000
  Shape__describe                  0x000001d4

Text relocations:
  offset=0x000002e8 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000003ec kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x00000428 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0

.text (1212 bytes):
  00000000  ff 03 09 d1 f0 03 00 91  10 c2 08 91 1d 7a 00 a9 
  00000010  fd 03 00 91 e0 bb 00 f9  1f 20 03 d5 f0 03 00 91 
  00000020  10 62 06 91 f0 03 00 f9  f0 03 00 91 10 62 07 91 
  00000030  f0 07 00 f9 f1 07 40 f9  f0 bb 40 f9 30 02 00 f9 
  00000040  f0 03 00 91 10 62 08 91  f0 0f 00 f9 f0 07 40 f9 
  00000050  f0 13 00 f9 f0 13 40 f9  11 02 40 f9 f1 17 00 f9 
  00000060  f0 17 40 f9 1f 06 00 f1  f0 17 9f 9a f0 1b 00 f9 
  00000070  f1 0f 40 f9 f0 c3 40 39  30 02 00 39 f0 0f 40 f9 
  00000080  11 02 40 39 f1 23 00 f9  f0 03 41 39 1f 06 00 f1 
  00000090  f0 17 9f 9a f0 27 00 f9  f0 27 40 f9 1f 02 00 f1 
  000000a0  41 00 00 54 05 00 00 14  f1 03 40 f9 30 00 80 d2 
  000000b0  30 02 00 f9 1b 00 00 14  f0 03 00 91 10 82 08 91 
  000000c0  f0 2f 00 f9 f0 07 40 f9  f0 33 00 f9 f0 33 40 f9 
  000000d0  11 02 40 f9 f1 37 00 f9  f0 37 40 f9 1f 0a 00 f1 
  000000e0  f0 17 9f 9a f0 3b 00 f9  f1 2f 40 f9 f0 c3 41 39 
  000000f0  30 02 00 39 f0 2f 40 f9  11 02 40 39 f1 43 00 f9 
  00000100  f0 03 42 39 1f 06 00 f1  f0 17 9f 9a f0 47 00 f9 
  00000110  f0 47 40 f9 1f 02 00 f1  81 01 00 54 0f 00 00 14 
  00000120  f0 03 40 f9 11 02 40 f9  f1 4b 00 f9 e0 4b 40 f9 
  00000130  bf 03 00 91 f0 03 00 91  10 c2 08 91 1d 7a 40 a9 
  00000140  ff 03 09 91 c0 03 5f d6  f1 03 40 f9 50 00 80 d2 
  00000150  30 02 00 f9 f3 ff ff 17  f0 03 00 91 10 a2 08 91 
  00000160  f0 53 00 f9 f0 07 40 f9  f0 57 00 f9 f0 57 40 f9 
  00000170  11 02 40 f9 f1 5b 00 f9  f0 5b 40 f9 1f 16 00 f1 
  00000180  f0 17 9f 9a f0 5f 00 f9  f1 53 40 f9 f0 e3 42 39 
  00000190  30 02 00 39 f0 53 40 f9  11 02 40 39 f1 67 00 f9 
  000001a0  f0 23 43 39 1f 06 00 f1  f0 17 9f 9a f0 6b 00 f9 
  000001b0  f0 6b 40 f9 1f 02 00 f1  41 00 00 54 05 00 00 14 
  000001c0  f1 03 40 f9 b0 00 80 d2  30 02 00 f9 d5 ff ff 17 
  000001d0  d4 ff ff 17 ff 03 15 d1  f0 03 00 91 10 c2 14 91 
  000001e0  1d 7a 00 a9 fd 03 00 91  e0 d3 00 f9 e1 bb 00 f9 
  000001f0  1f 20 03 d5 f0 03 00 91  10 82 07 91 f0 57 00 f9 
  00000200  f0 03 00 91 10 82 0b 91  f0 5b 00 f9 f1 bb 40 f9 
  00000210  e9 03 11 aa 30 01 40 f9  f0 d7 00 f9 e9 03 11 aa 
  00000220  29 21 00 91 30 01 40 f9  f0 db 00 f9 e9 03 11 aa 
  00000230  29 41 00 91 30 01 40 f9  f0 df 00 f9 f0 03 00 91 
  00000240  10 a2 06 91 f0 5f 00 f9  f1 5b 40 f9 f0 d7 40 f9 
  00000250  e9 03 11 aa 30 01 00 f9  f0 db 40 f9 e9 03 11 aa 
  00000260  29 21 00 91 30 01 00 f9  f0 df 40 f9 e9 03 11 aa 
  00000270  29 41 00 91 30 01 00 f9  f0 03 00 91 10 82 14 91 
  00000280  f0 67 00 f9 f0 5b 40 f9  f0 6b 00 f9 f0 6b 40 f9 
  00000290  11 02 40 f9 f1 6f 00 f9  f0 6f 40 f9 1f 02 00 f1 
  000002a0  f0 17 9f 9a f0 73 00 f9  f1 67 40 f9 f0 83 43 39 
  000002b0  30 02 00 39 f0 67 40 f9  11 02 40 39 f1 7b 00 f9 
  000002c0  f0 c3 43 39 1f 06 00 f1  f0 17 9f 9a f0 7f 00 f9 
  000002d0  f0 7f 40 f9 1f 02 00 f1  41 00 00 54 0f 00 00 14 
  000002e0  f1 57 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  000002f0  ea 03 0b aa 50 01 00 f9  b0 00 80 d2 10 00 a0 f2 
  00000300  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  00000310  50 01 00 f9 1b 00 00 14  f0 03 00 91 10 a2 14 91 
  00000320  f0 87 00 f9 f0 5b 40 f9  f0 8b 00 f9 f0 8b 40 f9 
  00000330  11 02 40 f9 f1 8f 00 f9  f0 8f 40 f9 1f 06 00 f1 
  00000340  f0 17 9f 9a f0 93 00 f9  f1 87 40 f9 f0 83 44 39 
  00000350  30 02 00 39 f0 87 40 f9  11 02 40 39 f1 9b 00 f9 
  00000360  f0 c3 44 39 1f 06 00 f1  f0 17 9f 9a f0 9f 00 f9 
  00000370  f0 9f 40 f9 1f 02 00 f1  61 03 00 54 28 00 00 14 
  00000380  f1 57 40 f9 e9 03 11 aa  30 01 40 f9 f0 e3 00 f9 
  00000390  e9 03 11 aa 29 21 00 91  30 01 40 f9 f0 e7 00 f9 
  000003a0  f0 03 00 91 10 02 07 91  f0 a3 00 f9 f1 d3 40 f9 
  000003b0  f0 e3 40 f9 e9 03 11 aa  30 01 00 f9 f0 e7 40 f9 
  000003c0  e9 03 11 aa 29 21 00 91  30 01 00 f9 bf 03 00 91 
  000003d0  f0 03 00 91 10 c2 14 91  1d 7a 40 a9 ff 03 15 91 
  000003e0  c0 03 5f d6 f1 57 40 f9  eb 03 11 aa 10 00 00 90 
  000003f0  10 02 00 91 ea 03 0b aa  50 01 00 f9 d0 00 80 d2 
  00000400  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 ea 03 0b aa 
  00000410  4a 21 00 91 50 01 00 f9  da ff ff 17 01 00 00 14 
  00000420  f1 57 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  00000430  ea 03 0b aa 50 01 00 f9  30 01 80 d2 10 00 a0 f2 
  00000440  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  00000450  50 01 00 f9 cb ff ff 17  f1 57 40 f9 e9 03 11 aa 
  00000460  30 01 40 f9 f0 eb 00 f9  e9 03 11 aa 29 21 00 91 
  00000470  30 01 40 f9 f0 ef 00 f9  f0 03 00 91 10 42 07 91 
  00000480  f0 af 00 f9 f1 d3 40 f9  f0 eb 40 f9 e9 03 11 aa 
  00000490  30 01 00 f9 f0 ef 40 f9  e9 03 11 aa 29 21 00 91 
  000004a0  30 01 00 f9 bf 03 00 91  f0 03 00 91 10 c2 14 91 
  000004b0  1d 7a 40 a9 ff 03 15 91  c0 03 5f d6 

.rodata (31 bytes):
  00000000  02 00 00 00 00 00 00 00  70 6f 69 6e 74 00 63 69 
  00000010  72 63 6c 65 00 72 65 63  74 61 6e 67 6c 65 00 
