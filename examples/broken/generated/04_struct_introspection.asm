fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println), 16
    intrinsic.call symbol(intrinsic.println), 3
    intrinsic.call symbol(intrinsic.println), 2
    intrinsic.call symbol(intrinsic.println), 3
    intrinsic.call symbol(intrinsic.println), 1
    intrinsic.call symbol(intrinsic.println), 0
    intrinsic.call symbol(intrinsic.println), 0
    intrinsic.call symbol(intrinsic.println), 0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    alloca Virtual { id: 16, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 16, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    alloca Virtual { id: 18, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 18, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println), 16
    intrinsic.call symbol(intrinsic.println), 3
    intrinsic.call symbol(intrinsic.println), 19
    intrinsic.call symbol(intrinsic.println)
    bitcast Virtual { id: 25, bank: General, size_bits: 64 }, Virtual { id: 16, bank: General, size_bits: 64 }
    load Virtual { id: 26, bank: Float, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 25, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 27, bank: General, size_bits: 64 }, Virtual { id: 16, bank: General, size_bits: 64 }
    gep Virtual { id: 28, bank: General, size_bits: 64 }, Virtual { id: 27, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 29, bank: General, size_bits: 64 }, Virtual { id: 28, bank: General, size_bits: 64 }
    load Virtual { id: 30, bank: Float, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 29, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 26, bank: General, size_bits: 64 }, Virtual { id: 30, bank: General, size_bits: 64 }
    bitcast Virtual { id: 32, bank: General, size_bits: 64 }, Virtual { id: 18, bank: General, size_bits: 64 }
    load Virtual { id: 33, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 32, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 34, bank: General, size_bits: 64 }, Virtual { id: 18, bank: General, size_bits: 64 }
    gep Virtual { id: 35, bank: General, size_bits: 64 }, Virtual { id: 34, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 36, bank: General, size_bits: 64 }, Virtual { id: 35, bank: General, size_bits: 64 }
    load Virtual { id: 37, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 38, bank: General, size_bits: 64 }, Virtual { id: 18, bank: General, size_bits: 64 }
    gep Virtual { id: 39, bank: General, size_bits: 64 }, Virtual { id: 38, bank: General, size_bits: 64 }, 2
    bitcast Virtual { id: 40, bank: General, size_bits: 64 }, Virtual { id: 39, bank: General, size_bits: 64 }
    load Virtual { id: 41, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 40, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 33, bank: General, size_bits: 64 }, Virtual { id: 37, bank: General, size_bits: 64 }, Virtual { id: 41, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    ret
fn __fp_comptime_const_POINT_SIZE_3894461749992024038
  bb0 bb0
    alloca Virtual { id: 44, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 44, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 16
    load Virtual { id: 46, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 44, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_COLOR_SIZE_1649646852462366576
  bb0 bb0
    alloca Virtual { id: 47, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 47, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 3
    load Virtual { id: 49, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 47, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_POINT_SIZE_CONST_15941031107507122034
  bb0 bb0
    alloca Virtual { id: 56, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 56, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 16
    load Virtual { id: 58, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 56, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __fp_comptime_const_COLOR_SIZE_CONST_16348495656534198754
  bb0 bb0
    alloca Virtual { id: 59, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 59, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 3
    load Virtual { id: 61, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 59, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret


Symbols:
  main                             0x00000000
  __fp_comptime_const_POINT_SIZE_3894461749992024038 0x00000384
  __fp_comptime_const_COLOR_SIZE_1649646852462366576 0x000003c8
  __fp_comptime_const_POINT_SIZE_CONST_15941031107507122034 0x0000040c
  __fp_comptime_const_COLOR_SIZE_CONST_16348495656534198754 0x00000450

Text relocations:
  offset=0x00000014 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000020 kind=CallRel32 symbol=printf addend=0
  offset=0x00000024 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000030 kind=CallRel32 symbol=printf addend=0
  offset=0x00000034 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000040 kind=CallRel32 symbol=printf addend=0
  offset=0x00000044 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000050 kind=CallRel32 symbol=printf addend=0
  offset=0x00000054 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000060 kind=CallRel32 symbol=printf addend=0
  offset=0x00000064 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000070 kind=CallRel32 symbol=printf addend=0
  offset=0x00000074 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000008c kind=CallRel32 symbol=printf addend=0
  offset=0x00000090 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000a8 kind=CallRel32 symbol=printf addend=0
  offset=0x000000ac kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000c4 kind=CallRel32 symbol=printf addend=0
  offset=0x000000c8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000e0 kind=CallRel32 symbol=printf addend=0
  offset=0x000000e4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000fc kind=CallRel32 symbol=printf addend=0
  offset=0x00000100 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000118 kind=CallRel32 symbol=printf addend=0
  offset=0x0000011c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000134 kind=CallRel32 symbol=printf addend=0
  offset=0x00000138 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000150 kind=CallRel32 symbol=printf addend=0
  offset=0x00000154 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000160 kind=CallRel32 symbol=printf addend=0
  offset=0x00000164 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000170 kind=CallRel32 symbol=printf addend=0
  offset=0x000001dc kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001e8 kind=CallRel32 symbol=printf addend=0
  offset=0x000001ec kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000204 kind=CallRel32 symbol=printf addend=0
  offset=0x00000208 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000220 kind=CallRel32 symbol=printf addend=0
  offset=0x00000224 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000023c kind=CallRel32 symbol=printf addend=0
  offset=0x00000240 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000024c kind=CallRel32 symbol=printf addend=0
  offset=0x00000290 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000002b4 kind=CallRel32 symbol=printf addend=0
  offset=0x00000324 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000354 kind=CallRel32 symbol=printf addend=0
  offset=0x00000358 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000364 kind=CallRel32 symbol=printf addend=0

.text (1172 bytes):
  00000000  ff 43 0a d1 f0 03 00 91  10 02 0a 91 1d 7a 00 a9 
  00000010  fd 03 00 91 00 00 00 90  00 00 00 91 00 c0 00 91 
  00000020  00 00 00 94 00 00 00 90  00 00 00 91 00 80 01 91 
  00000030  00 00 00 94 00 00 00 90  00 00 00 91 00 40 02 91 
  00000040  00 00 00 94 00 00 00 90  00 00 00 91 00 00 03 91 
  00000050  00 00 00 94 00 00 00 90  00 00 00 91 00 a0 03 91 
  00000060  00 00 00 94 00 00 00 90  00 00 00 91 00 c0 03 91 
  00000070  00 00 00 94 00 00 00 90  00 00 00 91 00 40 04 91 
  00000080  01 02 80 d2 10 02 80 d2  f0 03 00 f9 00 00 00 94 
  00000090  00 00 00 90 00 00 00 91  00 a0 04 91 61 00 80 d2 
  000000a0  70 00 80 d2 f0 03 00 f9  00 00 00 94 00 00 00 90 
  000000b0  00 00 00 91 00 00 05 91  41 00 80 d2 50 00 80 d2 
  000000c0  f0 03 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  000000d0  00 60 05 91 61 00 80 d2  70 00 80 d2 f0 03 00 f9 
  000000e0  00 00 00 94 00 00 00 90  00 00 00 91 00 c0 05 91 
  000000f0  21 00 80 d2 30 00 80 d2  f0 03 00 f9 00 00 00 94 
  00000100  00 00 00 90 00 00 00 91  00 20 06 91 01 00 80 d2 
  00000110  10 00 80 d2 f0 03 00 f9  00 00 00 94 00 00 00 90 
  00000120  00 00 00 91 00 80 06 91  01 00 80 d2 10 00 80 d2 
  00000130  f0 03 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  00000140  00 e0 06 91 01 00 80 d2  10 00 80 d2 f0 03 00 f9 
  00000150  00 00 00 94 00 00 00 90  00 00 00 91 00 40 07 91 
  00000160  00 00 00 94 00 00 00 90  00 00 00 91 00 c0 07 91 
  00000170  00 00 00 94 f0 03 00 91  10 82 09 91 f0 53 00 f9 
  00000180  f1 53 40 f9 10 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000190  10 00 e0 f2 e9 03 11 aa  30 01 00 f9 10 00 80 d2 
  000001a0  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 e9 03 11 aa 
  000001b0  29 21 00 91 30 01 00 f9  f0 03 00 91 10 c2 09 91 
  000001c0  f0 5b 00 f9 f0 1f 80 d2  10 00 a0 f2 10 00 c0 f2 
  000001d0  10 00 e0 f2 f1 5b 40 f9  30 02 00 f9 00 00 00 90 
  000001e0  00 00 00 91 00 40 08 91  00 00 00 94 00 00 00 90 
  000001f0  00 00 00 91 00 c0 08 91  01 02 80 d2 10 02 80 d2 
  00000200  f0 03 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  00000210  00 40 09 91 61 00 80 d2  70 00 80 d2 f0 03 00 f9 
  00000220  00 00 00 94 00 00 00 90  00 00 00 91 00 c0 09 91 
  00000230  61 02 80 d2 70 02 80 d2  f0 03 00 f9 00 00 00 94 
  00000240  00 00 00 90 00 00 00 91  00 20 0a 91 00 00 00 94 
  00000250  f0 53 40 f9 f0 77 00 f9  f0 77 40 f9 00 02 40 fd 
  00000260  e0 7b 00 fd f0 53 40 f9  f0 7f 00 f9 f0 7f 40 f9 
  00000270  11 01 80 d2 10 02 11 8b  f0 83 00 f9 f0 83 40 f9 
  00000280  f0 87 00 f9 f0 87 40 f9  00 02 40 fd e0 8b 00 fd 
  00000290  00 00 00 90 00 00 00 91  00 80 0a 91 e0 7b 40 fd 
  000002a0  e0 7b 40 fd e0 03 00 fd  e1 8b 40 fd e0 8b 40 fd 
  000002b0  e0 07 00 fd 00 00 00 94  f0 5b 40 f9 f0 93 00 f9 
  000002c0  f0 93 40 f9 11 02 c0 39  f1 97 00 f9 f0 5b 40 f9 
  000002d0  f0 9b 00 f9 f0 9b 40 f9  31 00 80 d2 10 02 11 8b 
  000002e0  f0 9f 00 f9 f0 9f 40 f9  f0 a3 00 f9 f0 a3 40 f9 
  000002f0  11 02 c0 39 f1 a7 00 f9  f0 5b 40 f9 f0 ab 00 f9 
  00000300  f0 ab 40 f9 51 00 80 d2  10 02 11 8b f0 af 00 f9 
  00000310  f0 af 40 f9 f0 b3 00 f9  f0 b3 40 f9 11 02 c0 39 
  00000320  f1 b7 00 f9 00 00 00 90  00 00 00 91 00 e0 0a 91 
  00000330  e1 a3 c4 39 f0 a3 c4 39  f0 03 00 f9 e2 23 c5 39 
  00000340  f0 23 c5 39 f0 07 00 f9  e3 a3 c5 39 f0 a3 c5 39 
  00000350  f0 0b 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  00000360  00 60 0b 91 00 00 00 94  bf 03 00 91 f0 03 00 91 
  00000370  10 02 0a 91 1d 7a 40 a9  ff 43 0a 91 00 00 80 d2 
  00000380  c0 03 5f d6 ff 03 01 d1  fd 7b 03 a9 fd 03 00 91 
  00000390  f0 03 00 91 10 82 00 91  f0 03 00 f9 f1 03 40 f9 
  000003a0  10 02 80 d2 30 02 00 f9  f0 03 40 f9 11 02 40 f9 
  000003b0  f1 0b 00 f9 e0 0b 40 f9  bf 03 00 91 fd 7b 43 a9 
  000003c0  ff 03 01 91 c0 03 5f d6  ff 03 01 d1 fd 7b 03 a9 
  000003d0  fd 03 00 91 f0 03 00 91  10 82 00 91 f0 03 00 f9 
  000003e0  f1 03 40 f9 70 00 80 d2  30 02 00 f9 f0 03 40 f9 
  000003f0  11 02 40 f9 f1 0b 00 f9  e0 0b 40 f9 bf 03 00 91 
  00000400  fd 7b 43 a9 ff 03 01 91  c0 03 5f d6 ff 03 01 d1 
  00000410  fd 7b 03 a9 fd 03 00 91  f0 03 00 91 10 82 00 91 
  00000420  f0 03 00 f9 f1 03 40 f9  10 02 80 d2 30 02 00 f9 
  00000430  f0 03 40 f9 11 02 40 f9  f1 0b 00 f9 e0 0b 40 f9 
  00000440  bf 03 00 91 fd 7b 43 a9  ff 03 01 91 c0 03 5f d6 
  00000450  ff 03 01 d1 fd 7b 03 a9  fd 03 00 91 f0 03 00 91 
  00000460  10 82 00 91 f0 03 00 f9  f1 03 40 f9 70 00 80 d2 
  00000470  30 02 00 f9 f0 03 40 f9  11 02 40 f9 f1 0b 00 f9 
  00000480  e0 0b 40 f9 bf 03 00 91  fd 7b 43 a9 ff 03 01 91 
  00000490  c0 03 5f d6 

.rodata (782 bytes):
  00000000  02 00 00 00 00 00 00 00  03 00 00 00 00 00 00 00 
  00000010  01 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00 
  00000020  00 00 00 00 00 00 00 00  13 00 00 00 00 00 00 00 
  00000030  f0 9f 93 98 20 54 75 74  6f 72 69 61 6c 3a 20 30 
  00000040  34 5f 73 74 72 75 63 74  5f 69 6e 74 72 6f 73 70 
  00000050  65 63 74 69 6f 6e 2e 66  70 0a 00 00 00 00 00 00 
  00000060  f0 9f a7 ad 20 46 6f 63  75 73 3a 20 53 74 72 75 
  00000070  63 74 20 69 6e 74 72 6f  73 70 65 63 74 69 6f 6e 
  00000080  20 64 65 6d 6f 6e 73 74  72 61 74 69 6f 6e 0a 00 
  00000090  f0 9f a7 aa 20 57 68 61  74 20 74 6f 20 6c 6f 6f 
  000000a0  6b 20 66 6f 72 3a 20 6c  61 62 65 6c 65 64 20 6f 
  000000b0  75 74 70 75 74 73 20 62  65 6c 6f 77 0a 00 00 00 
  000000c0  e2 9c 85 20 45 78 70 65  63 74 61 74 69 6f 6e 3a 
  000000d0  20 6f 75 74 70 75 74 73  20 6d 61 74 63 68 20 6c 
  000000e0  61 62 65 6c 73 0a 00 00  0a 00 00 00 00 00 00 00 
  000000f0  3d 3d 3d 20 53 74 72 75  63 74 20 49 6e 74 72 6f 
  00000100  73 70 65 63 74 69 6f 6e  20 3d 3d 3d 0a 00 00 00 
  00000110  50 6f 69 6e 74 20 73 69  7a 65 3a 20 25 6c 6c 75 
  00000120  20 62 79 74 65 73 0a 00  43 6f 6c 6f 72 20 73 69 
  00000130  7a 65 3a 20 25 6c 6c 75  20 62 79 74 65 73 0a 00 
  00000140  50 6f 69 6e 74 20 66 69  65 6c 64 73 3a 20 25 6c 
  00000150  6c 75 0a 00 00 00 00 00  43 6f 6c 6f 72 20 66 69 
  00000160  65 6c 64 73 3a 20 25 6c  6c 75 0a 00 00 00 00 00 
  00000170  50 6f 69 6e 74 20 68 61  73 20 78 3a 20 25 64 0a 
  00000180  00 00 00 00 00 00 00 00  50 6f 69 6e 74 20 68 61 
  00000190  73 20 7a 3a 20 25 64 0a  00 00 00 00 00 00 00 00 
  000001a0  50 6f 69 6e 74 20 6d 65  74 68 6f 64 73 3a 20 25 
  000001b0  6c 6c 75 0a 00 00 00 00  43 6f 6c 6f 72 20 6d 65 
  000001c0  74 68 6f 64 73 3a 20 25  6c 6c 75 0a 00 00 00 00 
  000001d0  0a e2 9c 93 20 49 6e 74  72 6f 73 70 65 63 74 69 
  000001e0  6f 6e 20 63 6f 6d 70 6c  65 74 65 64 21 0a 00 00 
  000001f0  0a 3d 3d 3d 20 54 72 61  6e 73 70 69 6c 61 74 69 
  00000200  6f 6e 20 44 65 6d 6f 20  3d 3d 3d 0a 00 00 00 00 
  00000210  54 72 61 6e 73 70 69 6c  61 74 69 6f 6e 20 74 61 
  00000220  72 67 65 74 20 73 69 7a  65 73 3a 0a 00 00 00 00 
  00000230  20 20 50 6f 69 6e 74 3a  20 25 6c 6c 75 20 62 79 
  00000240  74 65 73 20 28 63 6f 6e  73 74 29 0a 00 00 00 00 
  00000250  20 20 43 6f 6c 6f 72 3a  20 25 6c 6c 75 20 62 79 
  00000260  74 65 73 20 28 63 6f 6e  73 74 29 0a 00 00 00 00 
  00000270  20 20 43 6f 6d 62 69 6e  65 64 3a 20 25 6c 6c 75 
  00000280  20 62 79 74 65 73 0a 00  52 75 6e 74 69 6d 65 20 
  00000290  69 6e 73 74 61 6e 63 65  73 3a 0a 00 00 00 00 00 
  000002a0  20 20 4f 72 69 67 69 6e  3a 20 28 25 66 2c 20 25 
  000002b0  66 29 0a 00 00 00 00 00  20 20 52 65 64 3a 20 72 
  000002c0  67 62 28 25 68 68 75 2c  20 25 68 68 75 2c 20 25 
  000002d0  68 68 75 29 0a 00 00 00  0a e2 9c 93 20 49 6e 74 
  000002e0  72 6f 73 70 65 63 74 69  6f 6e 20 65 6e 61 62 6c 
  000002f0  65 73 20 65 78 74 65 72  6e 61 6c 20 63 6f 64 65 
  00000300  20 67 65 6e 65 72 61 74  69 6f 6e 21 0a 00 
