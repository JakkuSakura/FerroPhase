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
    alloca Virtual { id: 85, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 85, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    alloca Virtual { id: 87, bank: General, size_bits: 64 }, 3
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 87, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    intrinsic.call symbol(intrinsic.println)
    alloca Virtual { id: 90, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 90, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 16
    load Virtual { id: 92, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 90, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 92, bank: General, size_bits: 64 }
    alloca Virtual { id: 94, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 94, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 3
    load Virtual { id: 96, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 94, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 96, bank: General, size_bits: 64 }
    alloca Virtual { id: 98, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 98, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 16
    alloca Virtual { id: 100, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 100, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 3
    alloca Virtual { id: 102, bank: General, size_bits: 64 }, 8
    load Virtual { id: 103, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 98, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 104, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 100, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 105, bank: General, size_bits: 64 }, Virtual { id: 103, bank: General, size_bits: 64 }, Virtual { id: 104, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 102, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 105, bank: General, size_bits: 64 }
    load Virtual { id: 107, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 102, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 107, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    bitcast Virtual { id: 110, bank: General, size_bits: 64 }, Virtual { id: 85, bank: General, size_bits: 64 }
    load Virtual { id: 111, bank: Float, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 110, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 112, bank: General, size_bits: 64 }, Virtual { id: 85, bank: General, size_bits: 64 }
    gep Virtual { id: 113, bank: General, size_bits: 64 }, Virtual { id: 112, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 114, bank: General, size_bits: 64 }, Virtual { id: 113, bank: General, size_bits: 64 }
    load Virtual { id: 115, bank: Float, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 114, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 111, bank: Float, size_bits: 64 }, Virtual { id: 115, bank: Float, size_bits: 64 }
    bitcast Virtual { id: 117, bank: General, size_bits: 64 }, Virtual { id: 87, bank: General, size_bits: 64 }
    load Virtual { id: 118, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 117, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 119, bank: General, size_bits: 64 }, Virtual { id: 87, bank: General, size_bits: 64 }
    gep Virtual { id: 120, bank: General, size_bits: 64 }, Virtual { id: 119, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 121, bank: General, size_bits: 64 }, Virtual { id: 120, bank: General, size_bits: 64 }
    load Virtual { id: 122, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 121, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 123, bank: General, size_bits: 64 }, Virtual { id: 87, bank: General, size_bits: 64 }
    gep Virtual { id: 124, bank: General, size_bits: 64 }, Virtual { id: 123, bank: General, size_bits: 64 }, 2
    bitcast Virtual { id: 125, bank: General, size_bits: 64 }, Virtual { id: 124, bank: General, size_bits: 64 }
    load Virtual { id: 126, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 125, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 118, bank: General, size_bits: 8 }, Virtual { id: 122, bank: General, size_bits: 8 }, Virtual { id: 126, bank: General, size_bits: 8 }
    intrinsic.call symbol(intrinsic.println)
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
  offset=0x000001e0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001ec kind=CallRel32 symbol=printf addend=0
  offset=0x00000214 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000022c kind=CallRel32 symbol=printf addend=0
  offset=0x00000254 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000026c kind=CallRel32 symbol=printf addend=0
  offset=0x000002ec kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000304 kind=CallRel32 symbol=printf addend=0
  offset=0x00000308 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000314 kind=CallRel32 symbol=printf addend=0
  offset=0x00000358 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000037c kind=CallRel32 symbol=printf addend=0
  offset=0x000003ec kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000041c kind=CallRel32 symbol=printf addend=0
  offset=0x00000420 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000042c kind=CallRel32 symbol=printf addend=0

.text (1100 bytes):
  00000000  ff 03 16 d1 f0 03 00 91  10 c2 15 91 1d 7a 00 a9 
  00000010  fd 03 00 91 1f 20 03 d5  00 00 00 90 00 00 00 91 
  00000020  00 00 00 94 00 00 00 90  00 00 00 91 00 c0 00 91 
  00000030  00 00 00 94 00 00 00 90  00 00 00 91 00 80 01 91 
  00000040  00 00 00 94 00 00 00 90  00 00 00 91 00 40 02 91 
  00000050  00 00 00 94 00 00 00 90  00 00 00 91 00 e0 02 91 
  00000060  00 00 00 94 00 00 00 90  00 00 00 91 00 00 03 91 
  00000070  00 00 00 94 00 00 00 90  00 00 00 91 00 80 03 91 
  00000080  01 02 80 d2 10 02 80 d2  f0 03 00 f9 00 00 00 94 
  00000090  00 00 00 90 00 00 00 91  00 e0 03 91 61 00 80 d2 
  000000a0  70 00 80 d2 f0 03 00 f9  00 00 00 94 00 00 00 90 
  000000b0  00 00 00 91 00 40 04 91  41 00 80 d2 50 00 80 d2 
  000000c0  f0 03 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  000000d0  00 a0 04 91 61 00 80 d2  70 00 80 d2 f0 03 00 f9 
  000000e0  00 00 00 94 00 00 00 90  00 00 00 91 00 00 05 91 
  000000f0  21 00 80 d2 30 00 80 d2  f0 03 00 f9 00 00 00 94 
  00000100  00 00 00 90 00 00 00 91  00 60 05 91 01 00 80 d2 
  00000110  10 00 80 d2 f0 03 00 f9  00 00 00 94 00 00 00 90 
  00000120  00 00 00 91 00 c0 05 91  01 00 80 d2 10 00 80 d2 
  00000130  f0 03 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  00000140  00 20 06 91 01 00 80 d2  10 00 80 d2 f0 03 00 f9 
  00000150  00 00 00 94 00 00 00 90  00 00 00 91 00 80 06 91 
  00000160  00 00 00 94 00 00 00 90  00 00 00 91 00 00 07 91 
  00000170  00 00 00 94 f0 03 00 91  10 62 0c 91 f0 53 00 f9 
  00000180  f1 53 40 f9 eb 03 11 aa  10 00 80 d2 10 00 a0 f2 
  00000190  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 50 01 00 f9 
  000001a0  10 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  000001b0  ea 03 0b aa 4a 21 00 91  50 01 00 f9 f0 03 00 91 
  000001c0  10 62 10 91 f0 5b 00 f9  f0 1f 80 d2 10 00 a0 f2 
  000001d0  10 00 c0 f2 10 00 e0 f2  f1 5b 40 f9 30 02 00 f9 
  000001e0  00 00 00 90 00 00 00 91  00 80 07 91 00 00 00 94 
  000001f0  f0 03 00 91 10 a2 10 91  f0 67 00 f9 f1 67 40 f9 
  00000200  10 02 80 d2 30 02 00 f9  f0 67 40 f9 11 02 40 f9 
  00000210  f1 6f 00 f9 00 00 00 90  00 00 00 91 00 00 08 91 
  00000220  e1 6f 40 f9 f0 6f 40 f9  f0 03 00 f9 00 00 00 94 
  00000230  f0 03 00 91 10 a2 11 91  f0 77 00 f9 f1 77 40 f9 
  00000240  70 00 80 d2 30 02 00 f9  f0 77 40 f9 11 02 40 f9 
  00000250  f1 7f 00 f9 00 00 00 90  00 00 00 91 00 80 08 91 
  00000260  e1 7f 40 f9 f0 7f 40 f9  f0 03 00 f9 00 00 00 94 
  00000270  f0 03 00 91 10 a2 12 91  f0 87 00 f9 f1 87 40 f9 
  00000280  10 02 80 d2 30 02 00 f9  f0 03 00 91 10 a2 13 91 
  00000290  f0 8f 00 f9 f1 8f 40 f9  70 00 80 d2 30 02 00 f9 
  000002a0  f0 03 00 91 10 a2 14 91  f0 97 00 f9 f0 87 40 f9 
  000002b0  11 02 40 f9 f1 9b 00 f9  f0 8f 40 f9 11 02 40 f9 
  000002c0  f1 9f 00 f9 f0 9b 40 f9  f1 9f 40 f9 10 02 11 8b 
  000002d0  f0 a3 00 f9 f1 97 40 f9  f0 a3 40 f9 30 02 00 f9 
  000002e0  f0 97 40 f9 11 02 40 f9  f1 ab 00 f9 00 00 00 90 
  000002f0  00 00 00 91 00 00 09 91  e1 ab 40 f9 f0 ab 40 f9 
  00000300  f0 03 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  00000310  00 60 09 91 00 00 00 94  f0 53 40 f9 f0 b7 00 f9 
  00000320  f0 b7 40 f9 00 02 40 fd  e0 bb 00 fd f0 53 40 f9 
  00000330  f0 bf 00 f9 f0 bf 40 f9  11 01 80 d2 10 02 11 8b 
  00000340  f0 c3 00 f9 f0 c3 40 f9  f0 c7 00 f9 f0 c7 40 f9 
  00000350  00 02 40 fd e0 cb 00 fd  00 00 00 90 00 00 00 91 
  00000360  00 c0 09 91 e0 bb 40 fd  e0 bb 40 fd e0 03 00 fd 
  00000370  e1 cb 40 fd e0 cb 40 fd  e0 07 00 fd 00 00 00 94 
  00000380  f0 5b 40 f9 f0 d3 00 f9  f0 d3 40 f9 11 02 c0 39 
  00000390  f1 d7 00 f9 f0 5b 40 f9  f0 db 00 f9 f0 db 40 f9 
  000003a0  31 00 80 d2 10 02 11 8b  f0 df 00 f9 f0 df 40 f9 
  000003b0  f0 e3 00 f9 f0 e3 40 f9  11 02 c0 39 f1 e7 00 f9 
  000003c0  f0 5b 40 f9 f0 eb 00 f9  f0 eb 40 f9 51 00 80 d2 
  000003d0  10 02 11 8b f0 ef 00 f9  f0 ef 40 f9 f0 f3 00 f9 
  000003e0  f0 f3 40 f9 11 02 c0 39  f1 f7 00 f9 00 00 00 90 
  000003f0  00 00 00 91 00 20 0a 91  e1 a3 c6 39 f0 a3 c6 39 
  00000400  f0 03 00 f9 e2 23 c7 39  f0 23 c7 39 f0 07 00 f9 
  00000410  e3 a3 c7 39 f0 a3 c7 39  f0 0b 00 f9 00 00 00 94 
  00000420  00 00 00 90 00 00 00 91  00 a0 0a 91 00 00 00 94 
  00000430  bf 03 00 91 f0 03 00 91  10 c2 15 91 1d 7a 40 a9 
  00000440  ff 03 16 91 00 00 80 d2  c0 03 5f d6 

.rodata (734 bytes):
  00000000  f0 9f 93 98 20 54 75 74  6f 72 69 61 6c 3a 20 30 
  00000010  34 5f 73 74 72 75 63 74  5f 69 6e 74 72 6f 73 70 
  00000020  65 63 74 69 6f 6e 2e 66  70 0a 00 00 00 00 00 00 
  00000030  f0 9f a7 ad 20 46 6f 63  75 73 3a 20 53 74 72 75 
  00000040  63 74 20 69 6e 74 72 6f  73 70 65 63 74 69 6f 6e 
  00000050  20 64 65 6d 6f 6e 73 74  72 61 74 69 6f 6e 0a 00 
  00000060  f0 9f a7 aa 20 57 68 61  74 20 74 6f 20 6c 6f 6f 
  00000070  6b 20 66 6f 72 3a 20 6c  61 62 65 6c 65 64 20 6f 
  00000080  75 74 70 75 74 73 20 62  65 6c 6f 77 0a 00 00 00 
  00000090  e2 9c 85 20 45 78 70 65  63 74 61 74 69 6f 6e 3a 
  000000a0  20 6f 75 74 70 75 74 73  20 6d 61 74 63 68 20 6c 
  000000b0  61 62 65 6c 73 0a 00 00  0a 00 00 00 00 00 00 00 
  000000c0  3d 3d 3d 20 53 74 72 75  63 74 20 49 6e 74 72 6f 
  000000d0  73 70 65 63 74 69 6f 6e  20 3d 3d 3d 0a 00 00 00 
  000000e0  50 6f 69 6e 74 20 73 69  7a 65 3a 20 25 6c 6c 75 
  000000f0  20 62 79 74 65 73 0a 00  43 6f 6c 6f 72 20 73 69 
  00000100  7a 65 3a 20 25 6c 6c 75  20 62 79 74 65 73 0a 00 
  00000110  50 6f 69 6e 74 20 66 69  65 6c 64 73 3a 20 25 6c 
  00000120  6c 75 0a 00 00 00 00 00  43 6f 6c 6f 72 20 66 69 
  00000130  65 6c 64 73 3a 20 25 6c  6c 75 0a 00 00 00 00 00 
  00000140  50 6f 69 6e 74 20 68 61  73 20 78 3a 20 25 64 0a 
  00000150  00 00 00 00 00 00 00 00  50 6f 69 6e 74 20 68 61 
  00000160  73 20 7a 3a 20 25 64 0a  00 00 00 00 00 00 00 00 
  00000170  50 6f 69 6e 74 20 6d 65  74 68 6f 64 73 3a 20 25 
  00000180  6c 6c 75 0a 00 00 00 00  43 6f 6c 6f 72 20 6d 65 
  00000190  74 68 6f 64 73 3a 20 25  6c 6c 75 0a 00 00 00 00 
  000001a0  0a e2 9c 93 20 49 6e 74  72 6f 73 70 65 63 74 69 
  000001b0  6f 6e 20 63 6f 6d 70 6c  65 74 65 64 21 0a 00 00 
  000001c0  0a 3d 3d 3d 20 54 72 61  6e 73 70 69 6c 61 74 69 
  000001d0  6f 6e 20 44 65 6d 6f 20  3d 3d 3d 0a 00 00 00 00 
  000001e0  54 72 61 6e 73 70 69 6c  61 74 69 6f 6e 20 74 61 
  000001f0  72 67 65 74 20 73 69 7a  65 73 3a 0a 00 00 00 00 
  00000200  20 20 50 6f 69 6e 74 3a  20 25 6c 6c 75 20 62 79 
  00000210  74 65 73 20 28 63 6f 6e  73 74 29 0a 00 00 00 00 
  00000220  20 20 43 6f 6c 6f 72 3a  20 25 6c 6c 75 20 62 79 
  00000230  74 65 73 20 28 63 6f 6e  73 74 29 0a 00 00 00 00 
  00000240  20 20 43 6f 6d 62 69 6e  65 64 3a 20 25 6c 6c 75 
  00000250  20 62 79 74 65 73 0a 00  52 75 6e 74 69 6d 65 20 
  00000260  69 6e 73 74 61 6e 63 65  73 3a 0a 00 00 00 00 00 
  00000270  20 20 4f 72 69 67 69 6e  3a 20 28 25 66 2c 20 25 
  00000280  66 29 0a 00 00 00 00 00  20 20 52 65 64 3a 20 72 
  00000290  67 62 28 25 68 68 75 2c  20 25 68 68 75 2c 20 25 
  000002a0  68 68 75 29 0a 00 00 00  0a e2 9c 93 20 49 6e 74 
  000002b0  72 6f 73 70 65 63 74 69  6f 6e 20 65 6e 61 62 6c 
  000002c0  65 73 20 65 78 74 65 72  6e 61 6c 20 63 6f 64 65 
  000002d0  20 67 65 6e 65 72 61 74  69 6f 6e 21 0a 00 
