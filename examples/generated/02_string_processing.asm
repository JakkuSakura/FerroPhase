fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data_0 ty=Array(I8, 11) constant=true initializer=Some(Bytes([70, 101, 114, 114, 111, 80, 104, 97, 115, 101, 0]))
global NAME ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_1 ty=Array(I8, 6) constant=true initializer=Some(Bytes([48, 46, 49, 46, 48, 0]))
global VERSION ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global NAME_LEN ty=I64 constant=true initializer=Some(Bytes([10, 0, 0, 0, 0, 0, 0, 0]))
global VERSION_LEN ty=I64 constant=true initializer=Some(Bytes([5, 0, 0, 0, 0, 0, 0, 0]))
global PREFIX_OK ty=I1 constant=true initializer=Some(Bytes([1]))
global SUFFIX_OK ty=I1 constant=true initializer=Some(Bytes([1]))
global HAS_PHASE ty=I1 constant=true initializer=Some(Bytes([1]))
global __const_data_2 ty=Array(I8, 6) constant=true initializer=Some(Bytes([70, 101, 114, 114, 111, 0]))
global SHORT ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_3 ty=Array(I8, 6) constant=true initializer=Some(Bytes([80, 104, 97, 115, 101, 0]))
global TAIL ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_4 ty=Array(I8, 6) constant=true initializer=Some(Bytes([97, 108, 112, 104, 97, 0]))
global __const_data_5 ty=Array(I8, 5) constant=true initializer=Some(Bytes([98, 101, 116, 97, 0]))
global __const_data_6 ty=Array(I8, 6) constant=true initializer=Some(Bytes([103, 97, 109, 109, 97, 0]))
global __const_data_7 ty=Array(I8, 6) constant=true initializer=Some(Bytes([100, 101, 108, 116, 97, 0]))
global WORDS ty=Array(Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") }, 4) constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global WORD_LENGTHS ty=Array(I64, 4) constant=true initializer=Some(Bytes([5, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global TOTAL_WORD_LEN ty=I64 constant=true initializer=Some(Bytes([19, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_8 ty=Array(I8, 18) constant=true initializer=Some(Bytes([70, 101, 114, 114, 111, 80, 104, 97, 115, 101, 32, 118, 48, 46, 49, 46, 48, 0]))
global BANNER ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 0, 0, 0, 0]))
fn main
  bb0 bb0
    alloca Virtual { id: 35, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 36, bank: General, size_bits: 64 }, 8
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println), symbol(__const_data_0), 10
    intrinsic.call symbol(intrinsic.println), symbol(__const_data_1), 5
    intrinsic.call symbol(intrinsic.println), 1, 1, 1
    intrinsic.call symbol(intrinsic.println), symbol(__const_data_2), symbol(__const_data_3)
    intrinsic.call symbol(intrinsic.println)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    br
  bb1 bb1
    alloca Virtual { id: 48, bank: General, size_bits: 64 }, 1
    load Virtual { id: 49, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 50, bank: General, size_bits: 8 }, Virtual { id: 49, bank: General, size_bits: 64 }, 4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 48, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 50, bank: General, size_bits: 8 }
    load Virtual { id: 52, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 48, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 53, bank: General, size_bits: 8 }, Virtual { id: 52, bank: General, size_bits: 8 }, 1
    condbr
  bb2 bb2
    alloca Virtual { id: 54, bank: General, size_bits: 64 }, 64
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 54, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.array)
    alloca Virtual { id: 56, bank: General, size_bits: 64 }, 8
    load Virtual { id: 57, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 56, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 57, bank: General, size_bits: 64 }
    alloca Virtual { id: 59, bank: General, size_bits: 64 }, 32
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 59, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.array)
    alloca Virtual { id: 61, bank: General, size_bits: 64 }, 8
    load Virtual { id: 62, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 61, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 62, bank: General, size_bits: 64 }
    load Virtual { id: 64, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 56, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 65, bank: General, size_bits: 64 }, Virtual { id: 64, bank: General, size_bits: 64 }, 16
    bitcast Virtual { id: 66, bank: General, size_bits: 64 }, Virtual { id: 54, bank: General, size_bits: 64 }
    gep Virtual { id: 67, bank: General, size_bits: 64 }, Virtual { id: 66, bank: General, size_bits: 64 }, Virtual { id: 65, bank: General, size_bits: 64 }
    bitcast Virtual { id: 68, bank: General, size_bits: 64 }, Virtual { id: 67, bank: General, size_bits: 64 }
    bitcast Virtual { id: 69, bank: General, size_bits: 64 }, Virtual { id: 68, bank: General, size_bits: 64 }
    load Virtual { id: 70, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 69, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 71, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 61, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 72, bank: General, size_bits: 64 }, Virtual { id: 71, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 73, bank: General, size_bits: 64 }, Virtual { id: 59, bank: General, size_bits: 64 }
    gep Virtual { id: 74, bank: General, size_bits: 64 }, Virtual { id: 73, bank: General, size_bits: 64 }, Virtual { id: 72, bank: General, size_bits: 64 }
    bitcast Virtual { id: 75, bank: General, size_bits: 64 }, Virtual { id: 74, bank: General, size_bits: 64 }
    load Virtual { id: 76, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 75, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 70, bank: General, size_bits: 64 }, Virtual { id: 76, bank: General, size_bits: 64 }
    load Virtual { id: 78, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 79, bank: General, size_bits: 64 }, Virtual { id: 78, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 79, bank: General, size_bits: 64 }
    br
  bb3 bb3
    intrinsic.call symbol(intrinsic.println), 19
    alloca Virtual { id: 82, bank: General, size_bits: 64 }, 1
    eq Virtual { id: 83, bank: General, size_bits: 8 }, 10, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 82, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 83, bank: General, size_bits: 8 }
    alloca Virtual { id: 85, bank: General, size_bits: 64 }, 1
    gt Virtual { id: 86, bank: General, size_bits: 8 }, 10, 5
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 85, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 86, bank: General, size_bits: 8 }
    load Virtual { id: 88, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 82, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 89, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 85, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 88, bank: General, size_bits: 8 }, Virtual { id: 89, bank: General, size_bits: 8 }
    intrinsic.call symbol(intrinsic.println), symbol(__const_data_8)
    alloca Virtual { id: 92, bank: General, size_bits: 64 }, 1
    gt Virtual { id: 93, bank: General, size_bits: 8 }, 10, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 93, bank: General, size_bits: 8 }
    load Virtual { id: 95, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 96, bank: General, size_bits: 8 }, Virtual { id: 95, bank: General, size_bits: 8 }, 1
    condbr
  bb4 bb4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 35, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 256
    br
  bb5 bb5
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 35, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 128
    br
  bb6 bb6
    load Virtual { id: 99, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 35, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 99, bank: General, size_bits: 64 }
    ret


Symbols:
  main                             0x00000000

Text relocations:
  offset=0x0000004c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000058 kind=CallRel32 symbol=printf addend=0
  offset=0x0000005c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000068 kind=CallRel32 symbol=printf addend=0
  offset=0x0000006c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000078 kind=CallRel32 symbol=printf addend=0
  offset=0x0000007c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000088 kind=CallRel32 symbol=printf addend=0
  offset=0x0000008c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000098 kind=CallRel32 symbol=printf addend=0
  offset=0x0000009c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000a8 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000000b0 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000000c8 kind=CallRel32 symbol=printf addend=0
  offset=0x000000cc kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000d8 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x000000e0 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x000000f8 kind=CallRel32 symbol=printf addend=0
  offset=0x000000fc kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000012c kind=CallRel32 symbol=printf addend=0
  offset=0x00000130 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000013c kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x00000144 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x00000150 kind=Aarch64AdrpAdd symbol=__const_data_3 addend=0
  offset=0x00000158 kind=Aarch64AdrpAdd symbol=__const_data_3 addend=0
  offset=0x00000164 kind=CallRel32 symbol=printf addend=0
  offset=0x00000168 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000174 kind=CallRel32 symbol=printf addend=0
  offset=0x000001fc kind=Aarch64AdrpAdd symbol=__const_data_4 addend=0
  offset=0x00000228 kind=Aarch64AdrpAdd symbol=__const_data_5 addend=0
  offset=0x00000254 kind=Aarch64AdrpAdd symbol=__const_data_6 addend=0
  offset=0x00000280 kind=Aarch64AdrpAdd symbol=__const_data_7 addend=0
  offset=0x0000040c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000430 kind=CallRel32 symbol=printf addend=0
  offset=0x0000045c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000474 kind=CallRel32 symbol=printf addend=0
  offset=0x000004e8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000050c kind=CallRel32 symbol=printf addend=0
  offset=0x00000510 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000051c kind=Aarch64AdrpAdd symbol=__const_data_8 addend=0
  offset=0x00000524 kind=Aarch64AdrpAdd symbol=__const_data_8 addend=0
  offset=0x00000530 kind=CallRel32 symbol=printf addend=0
  offset=0x000005b8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000005d0 kind=CallRel32 symbol=printf addend=0

Section relocations:
  section=Data offset=0x00000000 kind=Abs64 symbol=__const_data_0 addend=0
  section=Data offset=0x00000010 kind=Abs64 symbol=__const_data_1 addend=0
  section=Data offset=0x00000020 kind=Abs64 symbol=__const_data_2 addend=0
  section=Data offset=0x00000030 kind=Abs64 symbol=__const_data_3 addend=0
  section=Data offset=0x00000040 kind=Abs64 symbol=__const_data_4 addend=0
  section=Data offset=0x00000050 kind=Abs64 symbol=__const_data_5 addend=0
  section=Data offset=0x00000060 kind=Abs64 symbol=__const_data_6 addend=0
  section=Data offset=0x00000070 kind=Abs64 symbol=__const_data_7 addend=0
  section=Data offset=0x00000080 kind=Abs64 symbol=__const_data_8 addend=0

.text (1548 bytes):
  00000000  f0 03 00 91 11 12 83 d2  11 00 a0 f2 11 00 c0 f2 
  00000010  11 00 e0 f2 10 02 11 cb  1f 02 00 91 f0 03 00 91 
  00000020  11 10 83 d2 10 02 11 8b  1d 7a 00 a9 fd 03 00 91 
  00000030  1f 20 03 d5 f0 03 00 91  10 82 0d 91 f0 13 00 f9 
  00000040  f0 03 00 91 10 82 0e 91  f0 17 00 f9 00 00 00 90 
  00000050  00 00 00 91 00 40 02 91  00 00 00 94 00 00 00 90 
  00000060  00 00 00 91 00 e0 02 91  00 00 00 94 00 00 00 90 
  00000070  00 00 00 91 00 e0 03 91  00 00 00 94 00 00 00 90 
  00000080  00 00 00 91 00 a0 04 91  00 00 00 94 00 00 00 90 
  00000090  00 00 00 91 00 40 05 91  00 00 00 94 00 00 00 90 
  000000a0  00 00 00 91 00 60 05 91  01 00 00 90 21 00 00 91 
  000000b0  10 00 00 90 10 02 00 91  f0 03 00 f9 42 01 80 d2 
  000000c0  50 01 80 d2 f0 07 00 f9  00 00 00 94 00 00 00 90 
  000000d0  00 00 00 91 00 c0 05 91  01 00 00 90 21 00 00 91 
  000000e0  10 00 00 90 10 02 00 91  f0 03 00 f9 a2 00 80 d2 
  000000f0  b0 00 80 d2 f0 07 00 f9  00 00 00 94 00 00 00 90 
  00000100  00 00 00 91 00 20 06 91  21 00 80 d2 30 00 80 d2 
  00000110  f0 03 00 f9 22 00 80 d2  30 00 80 d2 f0 07 00 f9 
  00000120  23 00 80 d2 30 00 80 d2  f0 0b 00 f9 00 00 00 94 
  00000130  00 00 00 90 00 00 00 91  00 e0 06 91 01 00 00 90 
  00000140  21 00 00 91 10 00 00 90  10 02 00 91 f0 03 00 f9 
  00000150  02 00 00 90 42 00 00 91  10 00 00 90 10 02 00 91 
  00000160  f0 07 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  00000170  00 60 07 91 00 00 00 94  f1 17 40 f9 10 00 80 d2 
  00000180  30 02 00 f9 01 00 00 14  f0 03 00 91 10 82 0f 91 
  00000190  f0 47 00 f9 f0 17 40 f9  11 02 40 f9 f1 4b 00 f9 
  000001a0  f0 4b 40 f9 1f 12 00 f1  f0 a7 9f 9a f0 4f 00 f9 
  000001b0  f1 47 40 f9 f0 63 42 39  30 02 00 39 f0 47 40 f9 
  000001c0  11 02 40 39 f1 57 00 f9  f0 a3 42 39 1f 06 00 f1 
  000001d0  f0 17 9f 9a f0 5b 00 f9  f0 5b 40 f9 1f 02 00 f1 
  000001e0  41 00 00 54 9e 00 00 14  f0 03 00 91 10 a2 0f 91 
  000001f0  f0 5f 00 f9 f1 5f 40 f9  e9 03 11 aa 10 00 00 90 
  00000200  10 02 00 91 30 01 00 f9  b0 00 80 d2 10 00 a0 f2 
  00000210  10 00 c0 f2 10 00 e0 f2  29 21 00 91 30 01 00 f9 
  00000220  e9 03 11 aa 29 41 00 91  10 00 00 90 10 02 00 91 
  00000230  30 01 00 f9 90 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000240  10 00 e0 f2 29 21 00 91  30 01 00 f9 e9 03 11 aa 
  00000250  29 81 00 91 10 00 00 90  10 02 00 91 30 01 00 f9 
  00000260  b0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000270  29 21 00 91 30 01 00 f9  e9 03 11 aa 29 c1 00 91 
  00000280  10 00 00 90 10 02 00 91  30 01 00 f9 b0 00 80 d2 
  00000290  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 29 21 00 91 
  000002a0  30 01 00 f9 f0 03 00 91  11 7d 82 d2 10 02 11 8b 
  000002b0  f0 67 00 f9 f0 17 40 f9  11 02 40 f9 f1 6b 00 f9 
  000002c0  f1 67 40 f9 f0 6b 40 f9  30 02 00 f9 f0 03 00 91 
  000002d0  11 85 82 d2 10 02 11 8b  f0 73 00 f9 f1 73 40 f9 
  000002e0  b0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  000002f0  e9 03 11 aa 30 01 00 f9  90 00 80 d2 10 00 a0 f2 
  00000300  10 00 c0 f2 10 00 e0 f2  e9 03 11 aa 29 21 00 91 
  00000310  30 01 00 f9 b0 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000320  10 00 e0 f2 e9 03 11 aa  29 41 00 91 30 01 00 f9 
  00000330  b0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000340  e9 03 11 aa 29 61 00 91  30 01 00 f9 f0 03 00 91 
  00000350  11 05 83 d2 10 02 11 8b  f0 7b 00 f9 f0 17 40 f9 
  00000360  11 02 40 f9 f1 7f 00 f9  f1 7b 40 f9 f0 7f 40 f9 
  00000370  30 02 00 f9 f0 67 40 f9  11 02 40 f9 f1 87 00 f9 
  00000380  f0 87 40 f9 11 02 80 d2  10 7e 11 9b f0 8b 00 f9 
  00000390  f0 5f 40 f9 f0 8f 00 f9  f0 8f 40 f9 f1 8b 40 f9 
  000003a0  10 02 11 8b f0 93 00 f9  f0 93 40 f9 f0 97 00 f9 
  000003b0  f0 97 40 f9 f0 9b 00 f9  f0 9b 40 f9 11 02 40 f9 
  000003c0  f1 9f 00 f9 f0 7b 40 f9  11 02 40 f9 f1 a3 00 f9 
  000003d0  f0 a3 40 f9 11 01 80 d2  10 7e 11 9b f0 a7 00 f9 
  000003e0  f0 73 40 f9 f0 ab 00 f9  f0 ab 40 f9 f1 a7 40 f9 
  000003f0  10 02 11 8b f0 af 00 f9  f0 af 40 f9 f0 b3 00 f9 
  00000400  f0 b3 40 f9 11 02 40 f9  f1 b7 00 f9 00 00 00 90 
  00000410  00 00 00 91 00 80 07 91  e1 9f 40 f9 f0 9f 40 f9 
  00000420  f0 03 00 f9 e2 b7 40 f9  f0 b7 40 f9 f0 07 00 f9 
  00000430  00 00 00 94 f0 17 40 f9  11 02 40 f9 f1 bf 00 f9 
  00000440  f0 bf 40 f9 10 06 00 91  f0 c3 00 f9 f1 17 40 f9 
  00000450  f0 c3 40 f9 30 02 00 f9  4c ff ff 17 00 00 00 90 
  00000460  00 00 00 91 00 e0 07 91  61 02 80 d2 70 02 80 d2 
  00000470  f0 03 00 f9 00 00 00 94  f0 03 00 91 11 0d 83 d2 
  00000480  10 02 11 8b f0 cf 00 f9  50 01 80 d2 1f 02 00 f1 
  00000490  f0 17 9f 9a f0 d3 00 f9  f1 cf 40 f9 f0 83 46 39 
  000004a0  30 02 00 39 f0 03 00 91  11 0e 83 d2 10 02 11 8b 
  000004b0  f0 db 00 f9 50 01 80 d2  1f 16 00 f1 f0 d7 9f 9a 
  000004c0  f0 df 00 f9 f1 db 40 f9  f0 e3 46 39 30 02 00 39 
  000004d0  f0 cf 40 f9 11 02 40 39  f1 e7 00 f9 f0 db 40 f9 
  000004e0  11 02 40 39 f1 eb 00 f9  00 00 00 90 00 00 00 91 
  000004f0  00 40 08 91 e1 23 47 39  f0 23 47 39 f0 03 00 f9 
  00000500  e2 43 47 39 f0 43 47 39  f0 07 00 f9 00 00 00 94 
  00000510  00 00 00 90 00 00 00 91  00 a0 08 91 01 00 00 90 
  00000520  21 00 00 91 10 00 00 90  10 02 00 91 f0 03 00 f9 
  00000530  00 00 00 94 f0 03 00 91  11 0f 83 d2 10 02 11 8b 
  00000540  f0 f7 00 f9 50 01 80 d2  1f 22 00 f1 f0 d7 9f 9a 
  00000550  f0 fb 00 f9 f1 f7 40 f9  f0 c3 47 39 30 02 00 39 
  00000560  f0 f7 40 f9 11 02 40 39  f1 03 01 f9 f0 03 48 39 
  00000570  1f 06 00 f1 f0 17 9f 9a  f0 07 01 f9 f0 07 41 f9 
  00000580  1f 02 00 f1 41 00 00 54  05 00 00 14 f1 13 40 f9 
  00000590  10 20 80 d2 30 02 00 f9  05 00 00 14 f1 13 40 f9 
  000005a0  10 10 80 d2 30 02 00 f9  01 00 00 14 f0 13 40 f9 
  000005b0  11 02 40 f9 f1 13 01 f9  00 00 00 90 00 00 00 91 
  000005c0  00 e0 08 91 e1 13 41 f9  f0 13 41 f9 f0 03 00 f9 
  000005d0  00 00 00 94 bf 03 00 91  f0 03 00 91 11 10 83 d2 
  000005e0  10 02 11 8b 1d 7a 40 a9  f0 03 00 91 11 12 83 d2 
  000005f0  11 00 a0 f2 11 00 c0 f2  11 00 e0 f2 10 02 11 8b 
  00000600  1f 02 00 91 00 00 80 d2  c0 03 5f d6 

.rodata (586 bytes):
  00000000  46 65 72 72 6f 50 68 61  73 65 00 30 2e 31 2e 30 
  00000010  00 00 00 00 00 00 00 00  0a 00 00 00 00 00 00 00 
  00000020  05 00 00 00 00 00 00 00  01 01 01 46 65 72 72 6f 
  00000030  00 50 68 61 73 65 00 61  6c 70 68 61 00 62 65 74 
  00000040  61 00 67 61 6d 6d 61 00  64 65 6c 74 61 00 00 00 
  00000050  05 00 00 00 00 00 00 00  04 00 00 00 00 00 00 00 
  00000060  05 00 00 00 00 00 00 00  05 00 00 00 00 00 00 00 
  00000070  13 00 00 00 00 00 00 00  46 65 72 72 6f 50 68 61 
  00000080  73 65 20 76 30 2e 31 2e  30 00 00 00 00 00 00 00 
  00000090  f0 9f 93 98 20 54 75 74  6f 72 69 61 6c 3a 20 30 
  000000a0  32 5f 73 74 72 69 6e 67  5f 70 72 6f 63 65 73 73 
  000000b0  69 6e 67 2e 66 70 0a 00  f0 9f a7 ad 20 46 6f 63 
  000000c0  75 73 3a 20 43 6f 6d 70  69 6c 65 2d 74 69 6d 65 
  000000d0  20 73 74 72 69 6e 67 20  6f 70 65 72 61 74 69 6f 
  000000e0  6e 73 20 61 6e 64 20 69  6e 74 72 69 6e 73 69 63 
  000000f0  73 0a 00 00 00 00 00 00  f0 9f a7 aa 20 57 68 61 
  00000100  74 20 74 6f 20 6c 6f 6f  6b 20 66 6f 72 3a 20 6c 
  00000110  61 62 65 6c 65 64 20 6f  75 74 70 75 74 73 20 62 
  00000120  65 6c 6f 77 0a 00 00 00  e2 9c 85 20 45 78 70 65 
  00000130  63 74 61 74 69 6f 6e 3a  20 6f 75 74 70 75 74 73 
  00000140  20 6d 61 74 63 68 20 6c  61 62 65 6c 73 0a 00 00 
  00000150  0a 00 00 00 00 00 00 00  6e 61 6d 65 3d 27 25 73 
  00000160  27 20 6c 65 6e 3d 25 6c  6c 75 0a 00 00 00 00 00 
  00000170  76 65 72 73 69 6f 6e 3d  27 25 73 27 20 6c 65 6e 
  00000180  3d 25 6c 6c 75 0a 00 00  70 72 65 66 69 78 5f 6f 
  00000190  6b 3d 25 64 2c 20 73 75  66 66 69 78 5f 6f 6b 3d 
  000001a0  25 64 2c 20 63 6f 6e 74  61 69 6e 73 5f 70 68 61 
  000001b0  73 65 3d 25 64 0a 00 00  73 6c 69 63 65 73 3a 20 
  000001c0  73 68 6f 72 74 3d 27 25  73 27 20 74 61 69 6c 3d 
  000001d0  27 25 73 27 0a 00 00 00  77 6f 72 64 73 3a 0a 00 
  000001e0  20 20 25 73 20 2d 3e 20  6c 65 6e 3d 25 6c 6c 75 
  000001f0  0a 00 00 00 00 00 00 00  74 6f 74 61 6c 20 77 6f 
  00000200  72 64 20 6c 65 6e 67 74  68 3d 25 6c 6c 75 0a 00 
  00000210  65 6d 70 74 79 3d 25 64  2c 20 6c 6f 6e 67 3d 25 
  00000220  64 0a 00 00 00 00 00 00  62 61 6e 6e 65 72 3d 27 
  00000230  25 73 27 0a 00 00 00 00  62 75 66 66 65 72 5f 73 
  00000240  69 7a 65 3d 25 6c 6c 75  0a 00 
