fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data_0 ty=Array(I8, 11) constant=true initializer=Some(Bytes([70, 101, 114, 114, 111, 80, 104, 97, 115, 101, 0]))
global __const_data_1 ty=Array(I8, 6) constant=true initializer=Some(Bytes([48, 46, 49, 46, 48, 0]))
global __const_data_2 ty=Array(I8, 6) constant=true initializer=Some(Bytes([70, 101, 114, 114, 111, 0]))
global __const_data_3 ty=Array(I8, 6) constant=true initializer=Some(Bytes([80, 104, 97, 115, 101, 0]))
global __const_data_4 ty=Array(I8, 6) constant=true initializer=Some(Bytes([97, 108, 112, 104, 97, 0]))
global __const_data_5 ty=Array(I8, 5) constant=true initializer=Some(Bytes([98, 101, 116, 97, 0]))
global __const_data_6 ty=Array(I8, 6) constant=true initializer=Some(Bytes([103, 97, 109, 109, 97, 0]))
global __const_data_7 ty=Array(I8, 6) constant=true initializer=Some(Bytes([100, 101, 108, 116, 97, 0]))
global __const_data_8 ty=Array(I8, 18) constant=true initializer=Some(Bytes([70, 101, 114, 114, 111, 80, 104, 97, 115, 101, 32, 118, 48, 46, 49, 46, 48, 0]))
global ::NAME ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0]))
global ::VERSION ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global ::NAME_LEN ty=I64 constant=true initializer=Some(Bytes([10, 0, 0, 0, 0, 0, 0, 0]))
global ::VERSION_LEN ty=I64 constant=true initializer=Some(Bytes([5, 0, 0, 0, 0, 0, 0, 0]))
global ::PREFIX_OK ty=I1 constant=true initializer=Some(Bytes([1]))
global ::SUFFIX_OK ty=I1 constant=true initializer=Some(Bytes([1]))
global ::HAS_PHASE ty=I1 constant=true initializer=Some(Bytes([1]))
global ::SHORT ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global ::TAIL ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global ::WORDS ty=Array(Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") }, 4) constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global ::WORD_LENGTHS ty=Array(I64, 4) constant=true initializer=Some(Bytes([5, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global ::TOTAL_WORD_LEN ty=I64 constant=true initializer=Some(Bytes([19, 0, 0, 0, 0, 0, 0, 0]))
global ::BANNER ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 0, 0, 0, 0]))
fn main
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 1
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
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    br
  bb1 bb1
    alloca Virtual { id: 12, bank: General, size_bits: 64 }, 1
    load Virtual { id: 13, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 14, bank: General, size_bits: 8 }, Virtual { id: 13, bank: General, size_bits: 64 }, 4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 12, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 14, bank: General, size_bits: 8 }
    load Virtual { id: 16, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 12, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 17, bank: General, size_bits: 8 }, Virtual { id: 16, bank: General, size_bits: 8 }, 1
    condbr
  bb2 bb2
    alloca Virtual { id: 18, bank: General, size_bits: 64 }, 1
    load Virtual { id: 19, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 18, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 19, bank: General, size_bits: 64 }
    alloca Virtual { id: 21, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.array)
    alloca Virtual { id: 23, bank: General, size_bits: 64 }, 1
    load Virtual { id: 24, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 23, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 24, bank: General, size_bits: 64 }
    alloca Virtual { id: 26, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 26, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.array)
    load Virtual { id: 28, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 18, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 29, bank: General, size_bits: 64 }, Virtual { id: 28, bank: General, size_bits: 64 }, 16
    bitcast Virtual { id: 30, bank: General, size_bits: 64 }, Virtual { id: 21, bank: General, size_bits: 64 }
    gep Virtual { id: 31, bank: General, size_bits: 64 }, Virtual { id: 30, bank: General, size_bits: 64 }, Virtual { id: 29, bank: General, size_bits: 64 }
    bitcast Virtual { id: 32, bank: General, size_bits: 64 }, Virtual { id: 31, bank: General, size_bits: 64 }
    bitcast Virtual { id: 33, bank: General, size_bits: 64 }, Virtual { id: 32, bank: General, size_bits: 64 }
    load Virtual { id: 34, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 33, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 35, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 23, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 36, bank: General, size_bits: 64 }, Virtual { id: 35, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 37, bank: General, size_bits: 64 }, Virtual { id: 26, bank: General, size_bits: 64 }
    gep Virtual { id: 38, bank: General, size_bits: 64 }, Virtual { id: 37, bank: General, size_bits: 64 }, Virtual { id: 36, bank: General, size_bits: 64 }
    bitcast Virtual { id: 39, bank: General, size_bits: 64 }, Virtual { id: 38, bank: General, size_bits: 64 }
    load Virtual { id: 40, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 39, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 34, bank: General, size_bits: 64 }, Virtual { id: 40, bank: General, size_bits: 64 }
    load Virtual { id: 42, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 43, bank: General, size_bits: 64 }, Virtual { id: 42, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 43, bank: General, size_bits: 64 }
    br
  bb3 bb3
    intrinsic.call symbol(intrinsic.println), 19
    intrinsic.call symbol(intrinsic.println), 0, 1
    intrinsic.call symbol(intrinsic.println), symbol(__const_data_8)
    intrinsic.call symbol(intrinsic.println), 256
    ret
fn __fp_comptime_const_IS_EMPTY_2183903305011928236
  bb0 bb0
    alloca Virtual { id: 49, bank: General, size_bits: 64 }, 1
    eq Virtual { id: 50, bank: General, size_bits: 8 }, 10, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 49, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 50, bank: General, size_bits: 8 }
    load Virtual { id: 52, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 49, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_IS_LONG_10589113863933626846
  bb0 bb0
    alloca Virtual { id: 53, bank: General, size_bits: 64 }, 1
    gt Virtual { id: 54, bank: General, size_bits: 8 }, 10, 5
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 53, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 54, bank: General, size_bits: 8 }
    load Virtual { id: 56, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 53, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_BUFFER_SIZE_5203167445245413666
  bb0 bb0
    alloca Virtual { id: 57, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 58, bank: General, size_bits: 64 }, 1
    gt Virtual { id: 59, bank: General, size_bits: 8 }, 10, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 58, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 59, bank: General, size_bits: 8 }
    load Virtual { id: 61, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 58, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 62, bank: General, size_bits: 8 }, Virtual { id: 61, bank: General, size_bits: 8 }, 1
    condbr
  bb1 bb1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 57, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 256
    br
  bb2 bb2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 57, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 128
    br
  bb3 bb3
    load Virtual { id: 65, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 57, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret


Symbols:
  main                             0x00000000
  __fp_comptime_const_IS_EMPTY_2183903305011928236 0x000004c4
  __fp_comptime_const_IS_LONG_10589113863933626846 0x00000518
  __fp_comptime_const_BUFFER_SIZE_5203167445245413666 0x0000056c

Text relocations:
  offset=0x00000020 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000002c kind=CallRel32 symbol=printf addend=0
  offset=0x00000030 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000003c kind=CallRel32 symbol=printf addend=0
  offset=0x00000040 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000004c kind=CallRel32 symbol=printf addend=0
  offset=0x00000050 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000005c kind=CallRel32 symbol=printf addend=0
  offset=0x00000060 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000006c kind=CallRel32 symbol=printf addend=0
  offset=0x00000070 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000007c kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000084 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x0000009c kind=CallRel32 symbol=printf addend=0
  offset=0x000000a0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000ac kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x000000b4 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x000000cc kind=CallRel32 symbol=printf addend=0
  offset=0x000000d0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000100 kind=CallRel32 symbol=printf addend=0
  offset=0x00000104 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000110 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x00000118 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x00000124 kind=Aarch64AdrpAdd symbol=__const_data_3 addend=0
  offset=0x0000012c kind=Aarch64AdrpAdd symbol=__const_data_3 addend=0
  offset=0x00000138 kind=CallRel32 symbol=printf addend=0
  offset=0x0000013c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000148 kind=CallRel32 symbol=printf addend=0
  offset=0x000001f4 kind=Aarch64AdrpAdd symbol=__const_data_4 addend=0
  offset=0x00000220 kind=Aarch64AdrpAdd symbol=__const_data_5 addend=0
  offset=0x0000024c kind=Aarch64AdrpAdd symbol=__const_data_6 addend=0
  offset=0x00000278 kind=Aarch64AdrpAdd symbol=__const_data_7 addend=0
  offset=0x000003d4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000003f8 kind=CallRel32 symbol=printf addend=0
  offset=0x00000424 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000043c kind=CallRel32 symbol=printf addend=0
  offset=0x00000440 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000464 kind=CallRel32 symbol=printf addend=0
  offset=0x00000468 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000474 kind=Aarch64AdrpAdd symbol=__const_data_8 addend=0
  offset=0x0000047c kind=Aarch64AdrpAdd symbol=__const_data_8 addend=0
  offset=0x00000488 kind=CallRel32 symbol=printf addend=0
  offset=0x0000048c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000004a4 kind=CallRel32 symbol=printf addend=0

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

.text (1560 bytes):
  00000000  ff 03 0d d1 f0 03 00 91  10 c2 0c 91 1d 7a 00 a9 
  00000010  fd 03 00 91 f0 03 00 91  10 a2 0a 91 f0 13 00 f9 
  00000020  00 00 00 90 00 00 00 91  00 20 02 91 00 00 00 94 
  00000030  00 00 00 90 00 00 00 91  00 c0 02 91 00 00 00 94 
  00000040  00 00 00 90 00 00 00 91  00 c0 03 91 00 00 00 94 
  00000050  00 00 00 90 00 00 00 91  00 80 04 91 00 00 00 94 
  00000060  00 00 00 90 00 00 00 91  00 20 05 91 00 00 00 94 
  00000070  00 00 00 90 00 00 00 91  00 40 05 91 01 00 00 90 
  00000080  21 00 00 91 10 00 00 90  10 02 00 91 f0 03 00 f9 
  00000090  42 01 80 d2 50 01 80 d2  f0 07 00 f9 00 00 00 94 
  000000a0  00 00 00 90 00 00 00 91  00 a0 05 91 01 00 00 90 
  000000b0  21 00 00 91 10 00 00 90  10 02 00 91 f0 03 00 f9 
  000000c0  a2 00 80 d2 b0 00 80 d2  f0 07 00 f9 00 00 00 94 
  000000d0  00 00 00 90 00 00 00 91  00 00 06 91 21 00 80 d2 
  000000e0  30 00 80 d2 f0 03 00 f9  22 00 80 d2 30 00 80 d2 
  000000f0  f0 07 00 f9 23 00 80 d2  30 00 80 d2 f0 0b 00 f9 
  00000100  00 00 00 94 00 00 00 90  00 00 00 91 00 c0 06 91 
  00000110  01 00 00 90 21 00 00 91  10 00 00 90 10 02 00 91 
  00000120  f0 03 00 f9 02 00 00 90  42 00 00 91 10 00 00 90 
  00000130  10 02 00 91 f0 07 00 f9  00 00 00 94 00 00 00 90 
  00000140  00 00 00 91 00 40 07 91  00 00 00 94 f1 13 40 f9 
  00000150  10 00 80 d2 30 02 00 f9  01 00 00 14 f0 03 00 91 
  00000160  10 c2 0a 91 f0 43 00 f9  f0 13 40 f9 11 02 40 f9 
  00000170  f1 47 00 f9 f0 47 40 f9  1f 12 00 f1 f0 a7 9f 9a 
  00000180  f0 4b 00 f9 f1 43 40 f9  f0 43 42 39 30 02 00 39 
  00000190  f0 43 40 f9 11 02 40 39  f1 53 00 f9 f0 83 42 39 
  000001a0  1f 06 00 f1 f0 17 9f 9a  f0 57 00 f9 f0 57 40 f9 
  000001b0  1f 02 00 f1 41 00 00 54  9b 00 00 14 f0 03 00 91 
  000001c0  10 e2 0a 91 f0 5b 00 f9  f0 13 40 f9 11 02 40 f9 
  000001d0  f1 5f 00 f9 f1 5b 40 f9  f0 5f 40 f9 30 02 00 f9 
  000001e0  f0 03 00 91 10 02 0b 91  f0 67 00 f9 f1 67 40 f9 
  000001f0  e9 03 11 aa 10 00 00 90  10 02 00 91 30 01 00 f9 
  00000200  b0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000210  29 21 00 91 30 01 00 f9  e9 03 11 aa 29 41 00 91 
  00000220  10 00 00 90 10 02 00 91  30 01 00 f9 90 00 80 d2 
  00000230  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 29 21 00 91 
  00000240  30 01 00 f9 e9 03 11 aa  29 81 00 91 10 00 00 90 
  00000250  10 02 00 91 30 01 00 f9  b0 00 80 d2 10 00 a0 f2 
  00000260  10 00 c0 f2 10 00 e0 f2  29 21 00 91 30 01 00 f9 
  00000270  e9 03 11 aa 29 c1 00 91  10 00 00 90 10 02 00 91 
  00000280  30 01 00 f9 b0 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000290  10 00 e0 f2 29 21 00 91  30 01 00 f9 f0 03 00 91 
  000002a0  10 02 0c 91 f0 6f 00 f9  f0 13 40 f9 11 02 40 f9 
  000002b0  f1 73 00 f9 f1 6f 40 f9  f0 73 40 f9 30 02 00 f9 
  000002c0  f0 03 00 91 10 22 0c 91  f0 7b 00 f9 f1 7b 40 f9 
  000002d0  b0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  000002e0  e9 03 11 aa 30 01 00 f9  90 00 80 d2 10 00 a0 f2 
  000002f0  10 00 c0 f2 10 00 e0 f2  e9 03 11 aa 29 21 00 91 
  00000300  30 01 00 f9 b0 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000310  10 00 e0 f2 e9 03 11 aa  29 41 00 91 30 01 00 f9 
  00000320  b0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000330  e9 03 11 aa 29 61 00 91  30 01 00 f9 f0 5b 40 f9 
  00000340  11 02 40 f9 f1 83 00 f9  f0 83 40 f9 11 02 80 d2 
  00000350  10 7e 11 9b f0 87 00 f9  f0 67 40 f9 f0 8b 00 f9 
  00000360  f0 8b 40 f9 f1 87 40 f9  10 02 11 8b f0 8f 00 f9 
  00000370  f0 8f 40 f9 f0 93 00 f9  f0 93 40 f9 f0 97 00 f9 
  00000380  f0 97 40 f9 11 02 40 f9  f1 9b 00 f9 f0 6f 40 f9 
  00000390  11 02 40 f9 f1 9f 00 f9  f0 9f 40 f9 11 01 80 d2 
  000003a0  10 7e 11 9b f0 a3 00 f9  f0 7b 40 f9 f0 a7 00 f9 
  000003b0  f0 a7 40 f9 f1 a3 40 f9  10 02 11 8b f0 ab 00 f9 
  000003c0  f0 ab 40 f9 f0 af 00 f9  f0 af 40 f9 11 02 40 f9 
  000003d0  f1 b3 00 f9 00 00 00 90  00 00 00 91 00 60 07 91 
  000003e0  e1 9b 40 f9 f0 9b 40 f9  f0 03 00 f9 e2 b3 40 f9 
  000003f0  f0 b3 40 f9 f0 07 00 f9  00 00 00 94 f0 13 40 f9 
  00000400  11 02 40 f9 f1 bb 00 f9  f0 bb 40 f9 10 06 00 91 
  00000410  f0 bf 00 f9 f1 13 40 f9  f0 bf 40 f9 30 02 00 f9 
  00000420  4f ff ff 17 00 00 00 90  00 00 00 91 00 c0 07 91 
  00000430  61 02 80 d2 70 02 80 d2  f0 03 00 f9 00 00 00 94 
  00000440  00 00 00 90 00 00 00 91  00 20 08 91 01 00 80 d2 
  00000450  10 00 80 d2 f0 03 00 f9  22 00 80 d2 30 00 80 d2 
  00000460  f0 07 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  00000470  00 80 08 91 01 00 00 90  21 00 00 91 10 00 00 90 
  00000480  10 02 00 91 f0 03 00 f9  00 00 00 94 00 00 00 90 
  00000490  00 00 00 91 00 c0 08 91  01 20 80 d2 10 20 80 d2 
  000004a0  f0 03 00 f9 00 00 00 94  bf 03 00 91 f0 03 00 91 
  000004b0  10 c2 0c 91 1d 7a 40 a9  ff 03 0d 91 00 00 80 d2 
  000004c0  c0 03 5f d6 ff 03 01 d1  fd 7b 03 a9 fd 03 00 91 
  000004d0  f0 03 00 91 10 a2 00 91  f0 03 00 f9 50 01 80 d2 
  000004e0  1f 02 00 f1 f0 17 9f 9a  f0 07 00 f9 f1 03 40 f9 
  000004f0  f0 23 40 39 30 02 00 39  f0 03 40 f9 11 02 40 39 
  00000500  f1 0f 00 f9 e0 63 40 39  bf 03 00 91 fd 7b 43 a9 
  00000510  ff 03 01 91 c0 03 5f d6  ff 03 01 d1 fd 7b 03 a9 
  00000520  fd 03 00 91 f0 03 00 91  10 a2 00 91 f0 03 00 f9 
  00000530  50 01 80 d2 1f 16 00 f1  f0 d7 9f 9a f0 07 00 f9 
  00000540  f1 03 40 f9 f0 23 40 39  30 02 00 39 f0 03 40 f9 
  00000550  11 02 40 39 f1 0f 00 f9  e0 63 40 39 bf 03 00 91 
  00000560  fd 7b 43 a9 ff 03 01 91  c0 03 5f d6 ff 03 02 d1 
  00000570  fd 7b 07 a9 fd 03 00 91  f0 03 00 91 10 62 01 91 
  00000580  f0 03 00 f9 f0 03 00 91  10 82 01 91 f0 07 00 f9 
  00000590  50 01 80 d2 1f 22 00 f1  f0 d7 9f 9a f0 0b 00 f9 
  000005a0  f1 07 40 f9 f0 43 40 39  30 02 00 39 f0 07 40 f9 
  000005b0  11 02 40 39 f1 13 00 f9  f0 83 40 39 1f 06 00 f1 
  000005c0  f0 17 9f 9a f0 17 00 f9  f0 17 40 f9 1f 02 00 f1 
  000005d0  41 00 00 54 05 00 00 14  f1 03 40 f9 10 20 80 d2 
  000005e0  30 02 00 f9 05 00 00 14  f1 03 40 f9 10 10 80 d2 
  000005f0  30 02 00 f9 01 00 00 14  f0 03 40 f9 11 02 40 f9 
  00000600  f1 23 00 f9 e0 23 40 f9  bf 03 00 91 fd 7b 47 a9 
  00000610  ff 03 02 91 c0 03 5f d6 

.rodata (578 bytes):
  00000000  46 65 72 72 6f 50 68 61  73 65 00 30 2e 31 2e 30 
  00000010  00 46 65 72 72 6f 00 50  68 61 73 65 00 61 6c 70 
  00000020  68 61 00 62 65 74 61 00  67 61 6d 6d 61 00 64 65 
  00000030  6c 74 61 00 46 65 72 72  6f 50 68 61 73 65 20 76 
  00000040  30 2e 31 2e 30 00 00 00  0a 00 00 00 00 00 00 00 
  00000050  05 00 00 00 00 00 00 00  01 01 01 00 00 00 00 00 
  00000060  05 00 00 00 00 00 00 00  04 00 00 00 00 00 00 00 
  00000070  05 00 00 00 00 00 00 00  05 00 00 00 00 00 00 00 
  00000080  13 00 00 00 00 00 00 00  f0 9f 93 98 20 54 75 74 
  00000090  6f 72 69 61 6c 3a 20 30  32 5f 73 74 72 69 6e 67 
  000000a0  5f 70 72 6f 63 65 73 73  69 6e 67 2e 66 70 0a 00 
  000000b0  f0 9f a7 ad 20 46 6f 63  75 73 3a 20 43 6f 6d 70 
  000000c0  69 6c 65 2d 74 69 6d 65  20 73 74 72 69 6e 67 20 
  000000d0  6f 70 65 72 61 74 69 6f  6e 73 20 61 6e 64 20 69 
  000000e0  6e 74 72 69 6e 73 69 63  73 0a 00 00 00 00 00 00 
  000000f0  f0 9f a7 aa 20 57 68 61  74 20 74 6f 20 6c 6f 6f 
  00000100  6b 20 66 6f 72 3a 20 6c  61 62 65 6c 65 64 20 6f 
  00000110  75 74 70 75 74 73 20 62  65 6c 6f 77 0a 00 00 00 
  00000120  e2 9c 85 20 45 78 70 65  63 74 61 74 69 6f 6e 3a 
  00000130  20 6f 75 74 70 75 74 73  20 6d 61 74 63 68 20 6c 
  00000140  61 62 65 6c 73 0a 00 00  0a 00 00 00 00 00 00 00 
  00000150  6e 61 6d 65 3d 27 25 73  27 20 6c 65 6e 3d 25 6c 
  00000160  6c 75 0a 00 00 00 00 00  76 65 72 73 69 6f 6e 3d 
  00000170  27 25 73 27 20 6c 65 6e  3d 25 6c 6c 75 0a 00 00 
  00000180  70 72 65 66 69 78 5f 6f  6b 3d 25 64 2c 20 73 75 
  00000190  66 66 69 78 5f 6f 6b 3d  25 64 2c 20 63 6f 6e 74 
  000001a0  61 69 6e 73 5f 70 68 61  73 65 3d 25 64 0a 00 00 
  000001b0  73 6c 69 63 65 73 3a 20  73 68 6f 72 74 3d 27 25 
  000001c0  73 27 20 74 61 69 6c 3d  27 25 73 27 0a 00 00 00 
  000001d0  77 6f 72 64 73 3a 0a 00  20 20 25 73 20 2d 3e 20 
  000001e0  6c 65 6e 3d 25 6c 6c 75  0a 00 00 00 00 00 00 00 
  000001f0  74 6f 74 61 6c 20 77 6f  72 64 20 6c 65 6e 67 74 
  00000200  68 3d 25 6c 6c 75 0a 00  65 6d 70 74 79 3d 25 64 
  00000210  2c 20 6c 6f 6e 67 3d 25  64 0a 00 00 00 00 00 00 
  00000220  62 61 6e 6e 65 72 3d 27  25 73 27 0a 00 00 00 00 
  00000230  62 75 66 66 65 72 5f 73  69 7a 65 3d 25 6c 6c 75 
  00000240  0a 00 
