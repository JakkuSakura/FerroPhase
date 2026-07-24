fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
fn main
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 1
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println), 25, symbol(__const_data_0)
    intrinsic.call symbol(intrinsic.println), symbol(__const_data_1)
    intrinsic.call symbol(intrinsic.println), 85, symbol(__const_data_2)
    alloca Virtual { id: 9, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 9, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 42
    alloca Virtual { id: 11, bank: General, size_bits: 64 }, 1
    load Virtual { id: 12, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 9, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    gt Virtual { id: 13, bank: General, size_bits: 8 }, Virtual { id: 12, bank: General, size_bits: 64 }, 50
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 11, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 13, bank: General, size_bits: 64 }
    load Virtual { id: 15, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 11, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 16, bank: General, size_bits: 8 }, Virtual { id: 15, bank: General, size_bits: 64 }, 1
    condbr
  bb1 bb1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb2 bb2
    alloca Virtual { id: 18, bank: General, size_bits: 64 }, 1
    load Virtual { id: 19, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 9, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    gt Virtual { id: 20, bank: General, size_bits: 8 }, Virtual { id: 19, bank: General, size_bits: 64 }, 25
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 18, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 20, bank: General, size_bits: 64 }
    load Virtual { id: 22, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 18, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 23, bank: General, size_bits: 8 }, Virtual { id: 22, bank: General, size_bits: 64 }, 1
    condbr
  bb3 bb3
    alloca Virtual { id: 24, bank: General, size_bits: 64 }, 1
    load Virtual { id: 25, bank: General, size_bits: 128 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 24, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 25, bank: General, size_bits: 64 }
    load Virtual { id: 27, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 9, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 28, bank: General, size_bits: 64 }, Virtual { id: 24, bank: General, size_bits: 64 }
    load Virtual { id: 29, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 28, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 27, bank: General, size_bits: 64 }, Virtual { id: 29, bank: General, size_bits: 64 }
    ret
  bb4 bb4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb5 bb5
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb6 bb6
    br
fn __fp_comptime_const_WEATHER_6870412767832006559
  bb0 bb0
    alloca Virtual { id: 34, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 35, bank: General, size_bits: 64 }, 1
    gt Virtual { id: 36, bank: General, size_bits: 8 }, 25, 30
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 35, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 36, bank: General, size_bits: 64 }
    load Virtual { id: 38, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 35, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 39, bank: General, size_bits: 8 }, Virtual { id: 38, bank: General, size_bits: 64 }, 1
    condbr
  bb1 bb1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 34, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb2 bb2
    alloca Virtual { id: 41, bank: General, size_bits: 64 }, 1
    gt Virtual { id: 42, bank: General, size_bits: 8 }, 25, 20
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 42, bank: General, size_bits: 64 }
    load Virtual { id: 44, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 45, bank: General, size_bits: 8 }, Virtual { id: 44, bank: General, size_bits: 64 }, 1
    condbr
  bb3 bb3
    load Virtual { id: 46, bank: General, size_bits: 128 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 34, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb4 bb4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 34, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb5 bb5
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 34, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb6 bb6
    br
fn __fp_comptime_const_ACTIVITY_5758356623994275759
  bb0 bb0
    alloca Virtual { id: 51, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 52, bank: General, size_bits: 64 }, 1
    and Virtual { id: 53, bank: General, size_bits: 8 }, 1, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 52, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 53, bank: General, size_bits: 64 }
    load Virtual { id: 55, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 52, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 56, bank: General, size_bits: 8 }, Virtual { id: 55, bank: General, size_bits: 64 }, 1
    condbr
  bb1 bb1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 51, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb2 bb2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 51, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb3 bb3
    load Virtual { id: 59, bank: General, size_bits: 128 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 51, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_GRADE_8022588198739462396
  bb0 bb0
    alloca Virtual { id: 61, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 62, bank: General, size_bits: 64 }, 1
    ge Virtual { id: 63, bank: General, size_bits: 8 }, 85, 90
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 62, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 63, bank: General, size_bits: 64 }
    load Virtual { id: 65, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 62, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 66, bank: General, size_bits: 8 }, Virtual { id: 65, bank: General, size_bits: 64 }, 1
    condbr
  bb1 bb1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 61, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb2 bb2
    alloca Virtual { id: 68, bank: General, size_bits: 64 }, 1
    ge Virtual { id: 69, bank: General, size_bits: 8 }, 85, 80
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 68, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 69, bank: General, size_bits: 64 }
    load Virtual { id: 71, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 68, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 72, bank: General, size_bits: 8 }, Virtual { id: 71, bank: General, size_bits: 64 }, 1
    condbr
  bb3 bb3
    load Virtual { id: 73, bank: General, size_bits: 128 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 61, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb4 bb4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 61, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb5 bb5
    alloca Virtual { id: 75, bank: General, size_bits: 64 }, 1
    ge Virtual { id: 76, bank: General, size_bits: 8 }, 85, 70
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 75, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 76, bank: General, size_bits: 64 }
    load Virtual { id: 78, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 75, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 79, bank: General, size_bits: 8 }, Virtual { id: 78, bank: General, size_bits: 64 }, 1
    condbr
  bb6 bb6
    br
  bb7 bb7
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 61, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb8 bb8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 61, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb9 bb9
    br


Symbols:
  main                             0x00000000
  __fp_comptime_const_WEATHER_6870412767832006559 0x00000318
  __fp_comptime_const_ACTIVITY_5758356623994275759 0x000004d8
  __fp_comptime_const_GRADE_8022588198739462396 0x0000060c

Text relocations:
  offset=0x00000018 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000024 kind=CallRel32 symbol=printf addend=0
  offset=0x00000028 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000034 kind=CallRel32 symbol=printf addend=0
  offset=0x00000038 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000044 kind=CallRel32 symbol=printf addend=0
  offset=0x00000048 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000054 kind=CallRel32 symbol=printf addend=0
  offset=0x00000058 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000064 kind=CallRel32 symbol=printf addend=0
  offset=0x00000068 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000080 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000088 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000094 kind=CallRel32 symbol=printf addend=0
  offset=0x00000098 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000a4 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x000000ac kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x000000b8 kind=CallRel32 symbol=printf addend=0
  offset=0x000000bc kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000d4 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x000000dc kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x000000e8 kind=CallRel32 symbol=printf addend=0
  offset=0x00000168 kind=Aarch64AdrpAdd symbol=__const_data_3 addend=0
  offset=0x00000270 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000294 kind=CallRel32 symbol=printf addend=0
  offset=0x000002b0 kind=Aarch64AdrpAdd symbol=__const_data_4 addend=0
  offset=0x000002e4 kind=Aarch64AdrpAdd symbol=__const_data_5 addend=0
  offset=0x0000038c kind=Aarch64AdrpAdd symbol=__const_data_6 addend=0
  offset=0x00000470 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000004a4 kind=Aarch64AdrpAdd symbol=__const_data_7 addend=0
  offset=0x0000054c kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x00000580 kind=Aarch64AdrpAdd symbol=__const_data_8 addend=0
  offset=0x00000680 kind=Aarch64AdrpAdd symbol=__const_data_9 addend=0
  offset=0x00000764 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x000007f0 kind=Aarch64AdrpAdd symbol=__const_data_10 addend=0
  offset=0x00000824 kind=Aarch64AdrpAdd symbol=__const_data_11 addend=0

.text (2136 bytes):
  00000000  ff 03 08 d1 fd 7b 1f a9  fd 03 00 91 f0 03 00 91 
  00000010  10 c2 06 91 f0 0b 00 f9  00 00 00 90 00 00 00 91 
  00000020  00 40 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000030  00 e0 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000040  00 40 03 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000050  00 00 04 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000060  00 a0 04 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000070  00 c0 04 91 21 03 80 d2  30 03 80 d2 f0 03 00 f9 
  00000080  02 00 00 90 42 00 00 91  10 00 00 90 10 02 00 91 
  00000090  f0 07 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  000000a0  00 00 05 91 01 00 00 90  21 00 00 91 10 00 00 90 
  000000b0  10 02 00 91 f0 03 00 f9  00 00 00 94 00 00 00 90 
  000000c0  00 00 00 91 00 40 05 91  a1 0a 80 d2 b0 0a 80 d2 
  000000d0  f0 03 00 f9 02 00 00 90  42 00 00 91 10 00 00 90 
  000000e0  10 02 00 91 f0 07 00 f9  00 00 00 94 f0 03 00 91 
  000000f0  10 02 07 91 f0 2f 00 f9  f1 2f 40 f9 50 05 80 d2 
  00000100  30 02 00 f9 f0 03 00 91  10 22 07 91 f0 37 00 f9 
  00000110  f0 2f 40 f9 11 02 40 f9  f1 3b 00 f9 f0 3b 40 f9 
  00000120  1f ca 00 f1 f0 d7 9f 9a  f0 3f 00 f9 f1 37 40 f9 
  00000130  f0 e3 41 39 30 02 00 39  f0 37 40 f9 11 02 40 39 
  00000140  f1 47 00 f9 f0 23 42 39  1f 06 00 f1 f0 17 9f 9a 
  00000150  f0 4b 00 f9 f0 4b 40 f9  1f 02 00 f1 41 00 00 54 
  00000160  0e 00 00 14 f1 0b 40 f9  10 00 00 90 10 02 00 91 
  00000170  e9 03 11 aa 30 01 00 f9  90 00 80 d2 10 00 a0 f2 
  00000180  10 00 c0 f2 10 00 e0 f2  e9 03 11 aa 29 21 00 91 
  00000190  30 01 00 f9 19 00 00 14  f0 03 00 91 10 42 07 91 
  000001a0  f0 53 00 f9 f0 2f 40 f9  11 02 40 f9 f1 57 00 f9 
  000001b0  f0 57 40 f9 1f 66 00 f1  f0 d7 9f 9a f0 5b 00 f9 
  000001c0  f1 53 40 f9 f0 c3 42 39  30 02 00 39 f0 53 40 f9 
  000001d0  11 02 40 39 f1 63 00 f9  f0 03 43 39 1f 06 00 f1 
  000001e0  f0 17 9f 9a f0 67 00 f9  f0 67 40 f9 1f 02 00 f1 
  000001f0  e1 05 00 54 3b 00 00 14  f0 03 00 91 10 62 07 91 
  00000200  f0 6b 00 f9 f1 0b 40 f9  e9 03 11 aa 30 01 40 f9 
  00000210  f0 d3 00 f9 e9 03 11 aa  29 21 00 91 30 01 40 f9 
  00000220  f0 d7 00 f9 f0 03 00 91  10 82 06 91 f0 6f 00 f9 
  00000230  f1 6b 40 f9 f0 d3 40 f9  e9 03 11 aa 30 01 00 f9 
  00000240  f0 d7 40 f9 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  00000250  f0 2f 40 f9 11 02 40 f9  f1 77 00 f9 f0 6b 40 f9 
  00000260  f0 7b 00 f9 f0 7b 40 f9  11 02 40 f9 f1 7f 00 f9 
  00000270  00 00 00 90 00 00 00 91  00 a0 05 91 e1 77 40 f9 
  00000280  f0 77 40 f9 f0 03 00 f9  e2 7f 40 f9 f0 7f 40 f9 
  00000290  f0 07 00 f9 00 00 00 94  bf 03 00 91 fd 7b 5f a9 
  000002a0  ff 03 08 91 00 00 80 d2  c0 03 5f d6 f1 0b 40 f9 
  000002b0  10 00 00 90 10 02 00 91  e9 03 11 aa 30 01 00 f9 
  000002c0  d0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  000002d0  e9 03 11 aa 29 21 00 91  30 01 00 f9 0e 00 00 14 
  000002e0  f1 0b 40 f9 10 00 00 90  10 02 00 91 e9 03 11 aa 
  000002f0  30 01 00 f9 70 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000300  10 00 e0 f2 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  00000310  01 00 00 14 b9 ff ff 17  ff 83 03 d1 fd 7b 0d a9 
  00000320  fd 03 00 91 e0 4f 00 f9  f0 03 00 91 10 c2 02 91 
  00000330  f0 03 00 f9 f0 03 00 91  10 02 03 91 f0 07 00 f9 
  00000340  30 03 80 d2 1f 7a 00 f1  f0 d7 9f 9a f0 0b 00 f9 
  00000350  f1 07 40 f9 f0 43 40 39  30 02 00 39 f0 07 40 f9 
  00000360  11 02 40 39 f1 13 00 f9  f0 83 40 39 1f 06 00 f1 
  00000370  f0 17 9f 9a f0 17 00 f9  f0 17 40 f9 1f 02 00 f1 
  00000380  41 00 00 54 0e 00 00 14  f1 03 40 f9 10 00 00 90 
  00000390  10 02 00 91 e9 03 11 aa  30 01 00 f9 70 00 80 d2 
  000003a0  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 e9 03 11 aa 
  000003b0  29 21 00 91 30 01 00 f9  16 00 00 14 f0 03 00 91 
  000003c0  10 22 03 91 f0 1f 00 f9  30 03 80 d2 1f 52 00 f1 
  000003d0  f0 d7 9f 9a f0 23 00 f9  f1 1f 40 f9 f0 03 41 39 
  000003e0  30 02 00 39 f0 1f 40 f9  11 02 40 39 f1 2b 00 f9 
  000003f0  f0 43 41 39 1f 06 00 f1  f0 17 9f 9a f0 2f 00 f9 
  00000400  f0 2f 40 f9 1f 02 00 f1  21 03 00 54 25 00 00 14 
  00000410  f1 03 40 f9 e9 03 11 aa  30 01 40 f9 f0 53 00 f9 
  00000420  e9 03 11 aa 29 21 00 91  30 01 40 f9 f0 57 00 f9 
  00000430  f0 03 00 91 10 82 02 91  f0 33 00 f9 f1 4f 40 f9 
  00000440  f0 53 40 f9 e9 03 11 aa  30 01 00 f9 f0 57 40 f9 
  00000450  e9 03 11 aa 29 21 00 91  30 01 00 f9 bf 03 00 91 
  00000460  fd 7b 4d a9 ff 83 03 91  c0 03 5f d6 f1 03 40 f9 
  00000470  10 00 00 90 10 02 00 91  e9 03 11 aa 30 01 00 f9 
  00000480  90 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000490  e9 03 11 aa 29 21 00 91  30 01 00 f9 0e 00 00 14 
  000004a0  f1 03 40 f9 10 00 00 90  10 02 00 91 e9 03 11 aa 
  000004b0  30 01 00 f9 90 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  000004c0  10 00 e0 f2 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  000004d0  01 00 00 14 cf ff ff 17  ff 83 02 d1 fd 7b 09 a9 
  000004e0  fd 03 00 91 e0 33 00 f9  f0 03 00 91 10 e2 01 91 
  000004f0  f0 03 00 f9 f0 03 00 91  10 22 02 91 f0 07 00 f9 
  00000500  30 00 80 d2 31 00 80 d2  10 02 11 8a f0 0b 00 f9 
  00000510  f1 07 40 f9 f0 43 40 39  30 02 00 39 f0 07 40 f9 
  00000520  11 02 40 39 f1 13 00 f9  f0 83 40 39 1f 06 00 f1 
  00000530  f0 17 9f 9a f0 17 00 f9  f0 17 40 f9 1f 02 00 f1 
  00000540  41 00 00 54 0e 00 00 14  f1 03 40 f9 10 00 00 90 
  00000550  10 02 00 91 e9 03 11 aa  30 01 00 f9 f0 00 80 d2 
  00000560  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 e9 03 11 aa 
  00000570  29 21 00 91 30 01 00 f9  0e 00 00 14 f1 03 40 f9 
  00000580  10 00 00 90 10 02 00 91  e9 03 11 aa 30 01 00 f9 
  00000590  d0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  000005a0  e9 03 11 aa 29 21 00 91  30 01 00 f9 01 00 00 14 
  000005b0  f1 03 40 f9 e9 03 11 aa  30 01 40 f9 f0 37 00 f9 
  000005c0  e9 03 11 aa 29 21 00 91  30 01 40 f9 f0 3b 00 f9 
  000005d0  f0 03 00 91 10 a2 01 91  f0 23 00 f9 f1 33 40 f9 
  000005e0  f0 37 40 f9 e9 03 11 aa  30 01 00 f9 f0 3b 40 f9 
  000005f0  e9 03 11 aa 29 21 00 91  30 01 00 f9 bf 03 00 91 
  00000600  fd 7b 49 a9 ff 83 02 91  c0 03 5f d6 ff 83 04 d1 
  00000610  fd 7b 11 a9 fd 03 00 91  e0 6b 00 f9 f0 03 00 91 
  00000620  10 a2 03 91 f0 03 00 f9  f0 03 00 91 10 e2 03 91 
  00000630  f0 07 00 f9 b0 0a 80 d2  1f 6a 01 f1 f0 b7 9f 9a 
  00000640  f0 0b 00 f9 f1 07 40 f9  f0 43 40 39 30 02 00 39 
  00000650  f0 07 40 f9 11 02 40 39  f1 13 00 f9 f0 83 40 39 
  00000660  1f 06 00 f1 f0 17 9f 9a  f0 17 00 f9 f0 17 40 f9 
  00000670  1f 02 00 f1 41 00 00 54  0e 00 00 14 f1 03 40 f9 
  00000680  10 00 00 90 10 02 00 91  e9 03 11 aa 30 01 00 f9 
  00000690  30 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  000006a0  e9 03 11 aa 29 21 00 91  30 01 00 f9 16 00 00 14 
  000006b0  f0 03 00 91 10 02 04 91  f0 1f 00 f9 b0 0a 80 d2 
  000006c0  1f 42 01 f1 f0 b7 9f 9a  f0 23 00 f9 f1 1f 40 f9 
  000006d0  f0 03 41 39 30 02 00 39  f0 1f 40 f9 11 02 40 39 
  000006e0  f1 2b 00 f9 f0 43 41 39  1f 06 00 f1 f0 17 9f 9a 
  000006f0  f0 2f 00 f9 f0 2f 40 f9  1f 02 00 f1 21 03 00 54 
  00000700  25 00 00 14 f1 03 40 f9  e9 03 11 aa 30 01 40 f9 
  00000710  f0 6f 00 f9 e9 03 11 aa  29 21 00 91 30 01 40 f9 
  00000720  f0 73 00 f9 f0 03 00 91  10 62 03 91 f0 33 00 f9 
  00000730  f1 6b 40 f9 f0 6f 40 f9  e9 03 11 aa 30 01 00 f9 
  00000740  f0 73 40 f9 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  00000750  bf 03 00 91 fd 7b 51 a9  ff 83 04 91 c0 03 5f d6 
  00000760  f1 03 40 f9 10 00 00 90  10 02 00 91 e9 03 11 aa 
  00000770  30 01 00 f9 30 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000780  10 00 e0 f2 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  00000790  16 00 00 14 f0 03 00 91  10 22 04 91 f0 3b 00 f9 
  000007a0  b0 0a 80 d2 1f 1a 01 f1  f0 b7 9f 9a f0 3f 00 f9 
  000007b0  f1 3b 40 f9 f0 e3 41 39  30 02 00 39 f0 3b 40 f9 
  000007c0  11 02 40 39 f1 47 00 f9  f0 23 42 39 1f 06 00 f1 
  000007d0  f0 17 9f 9a f0 4b 00 f9  f0 4b 40 f9 1f 02 00 f1 
  000007e0  61 00 00 54 0f 00 00 14  c7 ff ff 17 f1 03 40 f9 
  000007f0  10 00 00 90 10 02 00 91  e9 03 11 aa 30 01 00 f9 
  00000800  30 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000810  e9 03 11 aa 29 21 00 91  30 01 00 f9 0e 00 00 14 
  00000820  f1 03 40 f9 10 00 00 90  10 02 00 91 e9 03 11 aa 
  00000830  30 01 00 f9 30 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000840  10 00 e0 f2 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  00000850  01 00 00 14 e5 ff ff 17 

.rodata (378 bytes):
  00000000  77 61 72 6d 00 6f 75 74  64 6f 6f 72 00 42 00 68 
  00000010  69 67 68 00 6d 65 64 69  75 6d 00 6c 6f 77 00 68 
  00000020  6f 74 00 63 6f 6c 64 00  69 6e 64 6f 6f 72 00 41 
  00000030  00 43 00 46 00 00 00 00  19 00 00 00 00 00 00 00 
  00000040  01 01 00 00 00 00 00 00  55 00 00 00 00 00 00 00 
  00000050  f0 9f 93 98 20 54 75 74  6f 72 69 61 6c 3a 20 30 
  00000060  33 5f 63 6f 6e 74 72 6f  6c 5f 66 6c 6f 77 2e 66 
  00000070  70 0a 00 00 00 00 00 00  f0 9f a7 ad 20 46 6f 63 
  00000080  75 73 3a 20 43 6f 6e 74  72 6f 6c 20 66 6c 6f 77 
  00000090  3a 20 69 66 2f 65 6c 73  65 20 65 78 70 72 65 73 
  000000a0  73 69 6f 6e 73 20 77 69  74 68 20 63 6f 6e 73 74 
  000000b0  20 61 6e 64 20 72 75 6e  74 69 6d 65 20 65 76 61 
  000000c0  6c 75 61 74 69 6f 6e 0a  00 00 00 00 00 00 00 00 
  000000d0  f0 9f a7 aa 20 57 68 61  74 20 74 6f 20 6c 6f 6f 
  000000e0  6b 20 66 6f 72 3a 20 6c  61 62 65 6c 65 64 20 6f 
  000000f0  75 74 70 75 74 73 20 62  65 6c 6f 77 0a 00 00 00 
  00000100  e2 9c 85 20 45 78 70 65  63 74 61 74 69 6f 6e 3a 
  00000110  20 6f 75 74 70 75 74 73  20 6d 61 74 63 68 20 6c 
  00000120  61 62 65 6c 73 0a 00 00  0a 00 00 00 00 00 00 00 
  00000130  25 6c 6c 64 c2 b0 43 20  69 73 20 25 73 0a 00 00 
  00000140  53 75 67 67 65 73 74 65  64 3a 20 25 73 0a 00 00 
  00000150  53 63 6f 72 65 20 25 6c  6c 64 20 3d 20 67 72 61 
  00000160  64 65 20 25 73 0a 00 00  56 61 6c 75 65 20 25 6c 
  00000170  6c 64 20 69 73 20 25 73  0a 00 
