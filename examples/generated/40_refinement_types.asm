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
    alloca Virtual { id: 6, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 6, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 42
    load Virtual { id: 8, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 6, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 8, bank: General, size_bits: 64 }
    alloca Virtual { id: 10, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 10, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 7
    alloca Virtual { id: 12, bank: General, size_bits: 64 }, 8
    load Virtual { id: 13, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 10, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 12, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 13, bank: General, size_bits: 64 }
    load Virtual { id: 15, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 12, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 15, bank: General, size_bits: 64 }
    alloca Virtual { id: 17, bank: General, size_bits: 64 }, 8
    load Virtual { id: 18, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 12, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 19, bank: General, size_bits: 64 }, Virtual { id: 18, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 17, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 19, bank: General, size_bits: 64 }
    load Virtual { id: 21, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 17, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 21, bank: General, size_bits: 64 }
    alloca Virtual { id: 23, bank: General, size_bits: 64 }, 1
    load Virtual { id: 24, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 6, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ge Virtual { id: 25, bank: General, size_bits: 8 }, Virtual { id: 24, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 23, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 25, bank: General, size_bits: 8 }
    alloca Virtual { id: 27, bank: General, size_bits: 64 }, 1
    load Virtual { id: 28, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 6, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    le Virtual { id: 29, bank: General, size_bits: 8 }, Virtual { id: 28, bank: General, size_bits: 64 }, 100
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 27, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 29, bank: General, size_bits: 8 }
    alloca Virtual { id: 31, bank: General, size_bits: 64 }, 1
    load Virtual { id: 32, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 23, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 33, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 27, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    and Virtual { id: 34, bank: General, size_bits: 8 }, Virtual { id: 32, bank: General, size_bits: 8 }, Virtual { id: 33, bank: General, size_bits: 8 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 31, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 34, bank: General, size_bits: 8 }
    load Virtual { id: 36, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 31, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 36, bank: General, size_bits: 8 }
    ret


Symbols:
  main                             0x00000000

Text relocations:
  offset=0x00000018 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000020 kind=CallRel32 symbol=printf addend=0
  offset=0x00000024 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000030 kind=CallRel32 symbol=printf addend=0
  offset=0x00000034 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000040 kind=CallRel32 symbol=printf addend=0
  offset=0x00000044 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000050 kind=CallRel32 symbol=printf addend=0
  offset=0x00000054 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000060 kind=CallRel32 symbol=printf addend=0
  offset=0x00000088 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000a0 kind=CallRel32 symbol=printf addend=0
  offset=0x000000ec kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000104 kind=CallRel32 symbol=printf addend=0
  offset=0x00000144 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000015c kind=CallRel32 symbol=printf addend=0
  offset=0x00000214 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000022c kind=CallRel32 symbol=printf addend=0

.text (588 bytes):
  00000000  ff c3 0b d1 f0 03 00 91  10 82 0b 91 1d 7a 00 a9 
  00000010  fd 03 00 91 1f 20 03 d5  00 00 00 90 00 00 00 91 
  00000020  00 00 00 94 00 00 00 90  00 00 00 91 00 a0 00 91 
  00000030  00 00 00 94 00 00 00 90  00 00 00 91 00 c0 01 91 
  00000040  00 00 00 94 00 00 00 90  00 00 00 91 00 80 02 91 
  00000050  00 00 00 94 00 00 00 90  00 00 00 91 00 20 03 91 
  00000060  00 00 00 94 f0 03 00 91  10 02 07 91 f0 1f 00 f9 
  00000070  f1 1f 40 f9 50 05 80 d2  30 02 00 f9 f0 1f 40 f9 
  00000080  11 02 40 f9 f1 27 00 f9  00 00 00 90 00 00 00 91 
  00000090  00 40 03 91 e1 27 40 f9  f0 27 40 f9 f0 03 00 f9 
  000000a0  00 00 00 94 f0 03 00 91  10 02 08 91 f0 2f 00 f9 
  000000b0  f1 2f 40 f9 f0 00 80 d2  30 02 00 f9 f0 03 00 91 
  000000c0  10 02 09 91 f0 37 00 f9  f0 2f 40 f9 11 02 40 f9 
  000000d0  f1 3b 00 f9 f1 37 40 f9  f0 3b 40 f9 30 02 00 f9 
  000000e0  f0 37 40 f9 11 02 40 f9  f1 43 00 f9 00 00 00 90 
  000000f0  00 00 00 91 00 00 04 91  e1 43 40 f9 f0 43 40 f9 
  00000100  f0 03 00 f9 00 00 00 94  f0 03 00 91 10 02 0a 91 
  00000110  f0 4b 00 f9 f0 37 40 f9  11 02 40 f9 f1 4f 00 f9 
  00000120  f0 4f 40 f9 10 06 00 91  f0 53 00 f9 f1 4b 40 f9 
  00000130  f0 53 40 f9 30 02 00 f9  f0 4b 40 f9 11 02 40 f9 
  00000140  f1 5b 00 f9 00 00 00 90  00 00 00 91 00 a0 04 91 
  00000150  e1 5b 40 f9 f0 5b 40 f9  f0 03 00 f9 00 00 00 94 
  00000160  f0 03 00 91 10 02 0b 91  f0 63 00 f9 f0 1f 40 f9 
  00000170  11 02 40 f9 f1 67 00 f9  f0 67 40 f9 1f 02 00 f1 
  00000180  f0 b7 9f 9a f0 6b 00 f9  f1 63 40 f9 f0 43 43 39 
  00000190  30 02 00 39 f0 03 00 91  10 22 0b 91 f0 73 00 f9 
  000001a0  f0 1f 40 f9 11 02 40 f9  f1 77 00 f9 f0 77 40 f9 
  000001b0  1f 92 01 f1 f0 c7 9f 9a  f0 7b 00 f9 f1 73 40 f9 
  000001c0  f0 c3 43 39 30 02 00 39  f0 03 00 91 10 42 0b 91 
  000001d0  f0 83 00 f9 f0 63 40 f9  11 02 40 39 f1 87 00 f9 
  000001e0  f0 73 40 f9 11 02 40 39  f1 8b 00 f9 f0 23 44 39 
  000001f0  f1 43 44 39 10 02 11 8a  f0 8f 00 f9 f1 83 40 f9 
  00000200  f0 63 44 39 30 02 00 39  f0 83 40 f9 11 02 40 39 
  00000210  f1 97 00 f9 00 00 00 90  00 00 00 91 00 e0 04 91 
  00000220  e1 a3 44 39 f0 a3 44 39  f0 03 00 f9 00 00 00 94 
  00000230  bf 03 00 91 f0 03 00 91  10 82 0b 91 1d 7a 40 a9 
  00000240  ff c3 0b 91 00 00 80 d2  c0 03 5f d6 

.rodata (348 bytes):
  00000000  f0 9f 93 98 20 54 75 74  6f 72 69 61 6c 3a 20 34 
  00000010  30 5f 72 65 66 69 6e 65  6d 65 6e 74 5f 74 79 70 
  00000020  65 73 2e 66 70 0a 00 00  f0 9f a7 ad 20 46 6f 63 
  00000030  75 73 3a 20 72 65 66 69  6e 65 6d 65 6e 74 20 74 
  00000040  79 70 65 73 20 63 68 65  63 6b 65 64 20 62 79 20 
  00000050  64 65 63 69 64 65 2f 6f  6d 65 67 61 20 61 74 20 
  00000060  63 6f 6d 70 69 6c 65 20  74 69 6d 65 0a 00 00 00 
  00000070  f0 9f a7 aa 20 57 68 61  74 20 74 6f 20 6c 6f 6f 
  00000080  6b 20 66 6f 72 3a 20 6c  61 62 65 6c 65 64 20 6f 
  00000090  75 74 70 75 74 73 20 62  65 6c 6f 77 0a 00 00 00 
  000000a0  e2 9c 85 20 45 78 70 65  63 74 61 74 69 6f 6e 3a 
  000000b0  20 6f 75 74 70 75 74 73  20 6d 61 74 63 68 20 6c 
  000000c0  61 62 65 6c 73 0a 00 00  0a 00 00 00 00 00 00 00 
  000000d0  70 65 72 63 65 6e 74 20  28 6c 69 74 65 72 61 6c 
  000000e0  2c 20 64 65 63 69 64 65  2d 63 68 65 63 6b 65 64 
  000000f0  29 20 3d 20 25 6c 6c 64  0a 00 00 00 00 00 00 00 
  00000100  6e 20 28 73 79 6d 62 6f  6c 69 63 2c 20 6f 6d 65 
  00000110  67 61 2d 63 68 65 63 6b  65 64 29 20 3d 20 25 6c 
  00000120  6c 75 0a 00 00 00 00 00  6e 20 2b 20 31 20 3d 20 
  00000130  25 6c 6c 75 0a 00 00 00  70 65 72 63 65 6e 74 20 
  00000140  69 73 20 61 20 76 61 6c  69 64 20 30 2d 31 30 30 
  00000150  20 76 61 6c 75 65 3a 20  25 64 0a 00 
