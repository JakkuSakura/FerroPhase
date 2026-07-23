fp-native dump: format=MachO arch=Aarch64 entry=0x1a4

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
fn sum
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 1, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 2, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    br
  bb1 bb1
    alloca Virtual { id: 5, bank: General, size_bits: 64 }, 1
    load Virtual { id: 6, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 7, bank: General, size_bits: 8 }, Virtual { id: 6, bank: General, size_bits: 64 }, 4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 7, bank: General, size_bits: 64 }
    load Virtual { id: 9, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 10, bank: General, size_bits: 8 }, Virtual { id: 9, bank: General, size_bits: 64 }, 1
    condbr
  bb2 bb2
    alloca Virtual { id: 11, bank: General, size_bits: 64 }, 1
    load Virtual { id: 12, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 11, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 12, bank: General, size_bits: 64 }
    load Virtual { id: 14, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 15, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 11, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 16, bank: General, size_bits: 64 }, Virtual { id: 15, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 17, bank: General, size_bits: 64 }, symbol(local.1)
    gep Virtual { id: 18, bank: General, size_bits: 64 }, Virtual { id: 17, bank: General, size_bits: 64 }, Virtual { id: 16, bank: General, size_bits: 64 }
    bitcast Virtual { id: 19, bank: General, size_bits: 64 }, Virtual { id: 18, bank: General, size_bits: 64 }
    load Virtual { id: 20, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 19, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 21, bank: General, size_bits: 64 }, Virtual { id: 14, bank: General, size_bits: 64 }, Virtual { id: 20, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 21, bank: General, size_bits: 64 }
    load Virtual { id: 23, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 24, bank: General, size_bits: 64 }, Virtual { id: 23, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 24, bank: General, size_bits: 64 }
    br
  bb3 bb3
    load Virtual { id: 26, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 26, bank: General, size_bits: 64 }
    load Virtual { id: 28, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn main
  bb0 bb0
    alloca Virtual { id: 29, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 29, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_0)
    alloca Virtual { id: 31, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 31, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.array)
    alloca Virtual { id: 33, bank: General, size_bits: 64 }, 1
    load Virtual { id: 34, bank: General, size_bits: 256 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 31, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(32), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 33, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 34, bank: General, size_bits: 64 }
    alloca Virtual { id: 36, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 33, bank: General, size_bits: 64 }
    load Virtual { id: 38, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(sum)(v38) cc=C tail=false
    br
  bb1 bb1
    load Virtual { id: 40, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 29, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 40, bank: General, size_bits: 64 }, Virtual { id: 39, bank: General, size_bits: 64 }
    ret


Symbols:
  sum                              0x00000000
  main                             0x000001a4

Relocations:
  offset=0x000001c0 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000320 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000344 kind=CallRel32 symbol=printf addend=0

.text (860 bytes):
  00000000  ff 43 05 d1 fd 7b 14 a9  fd 03 00 91 e0 7b 00 f9 
  00000010  f0 03 00 91 10 62 04 91  f0 03 00 f9 f0 03 00 91 
  00000020  10 82 04 91 f0 07 00 f9  f0 03 00 91 10 a2 04 91 
  00000030  f0 0b 00 f9 f1 03 40 f9  10 00 80 d2 30 02 00 f9 
  00000040  f1 07 40 f9 10 00 80 d2  30 02 00 f9 01 00 00 14 
  00000050  f0 03 00 91 10 c2 04 91  f0 17 00 f9 f0 03 40 f9 
  00000060  11 02 40 f9 f1 1b 00 f9  f0 1b 40 f9 1f 12 00 f1 
  00000070  f0 a7 9f 9a f0 1f 00 f9  f1 17 40 f9 f0 e3 40 39 
  00000080  30 02 00 39 f0 17 40 f9  11 02 40 39 f1 27 00 f9 
  00000090  f0 23 41 39 1f 06 00 f1  f0 17 9f 9a f0 2b 00 f9 
  000000a0  f0 2b 40 f9 1f 02 00 f1  41 00 00 54 30 00 00 14 
  000000b0  f0 03 00 91 10 e2 04 91  f0 2f 00 f9 f0 03 40 f9 
  000000c0  11 02 40 f9 f1 33 00 f9  f1 2f 40 f9 f0 33 40 f9 
  000000d0  30 02 00 f9 f0 07 40 f9  11 02 40 f9 f1 3b 00 f9 
  000000e0  f0 2f 40 f9 11 02 40 f9  f1 3f 00 f9 f0 3f 40 f9 
  000000f0  11 01 80 d2 10 7e 11 9b  f0 43 00 f9 f0 7b 40 f9 
  00000100  f0 47 00 f9 f0 47 40 f9  f1 43 40 f9 10 02 11 8b 
  00000110  f0 4b 00 f9 f0 4b 40 f9  f0 4f 00 f9 f0 4f 40 f9 
  00000120  11 02 40 f9 f1 53 00 f9  f0 3b 40 f9 f1 53 40 f9 
  00000130  10 02 11 8b f0 57 00 f9  f1 07 40 f9 f0 57 40 f9 
  00000140  30 02 00 f9 f0 03 40 f9  11 02 40 f9 f1 5f 00 f9 
  00000150  f0 5f 40 f9 10 06 00 91  f0 63 00 f9 f1 03 40 f9 
  00000160  f0 63 40 f9 30 02 00 f9  ba ff ff 17 f0 07 40 f9 
  00000170  11 02 40 f9 f1 6b 00 f9  f1 0b 40 f9 f0 6b 40 f9 
  00000180  30 02 00 f9 f0 0b 40 f9  11 02 40 f9 f1 73 00 f9 
  00000190  e0 73 40 f9 bf 03 00 91  fd 7b 54 a9 ff 43 05 91 
  000001a0  c0 03 5f d6 ff 83 05 d1  fd 7b 15 a9 fd 03 00 91 
  000001b0  f0 03 00 91 10 02 04 91  f0 0b 00 f9 f1 0b 40 f9 
  000001c0  10 00 00 90 10 02 00 91  30 02 00 f9 f0 03 00 91 
  000001d0  10 22 04 91 f0 13 00 f9  f1 13 40 f9 50 01 80 d2 
  000001e0  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 e9 03 11 aa 
  000001f0  30 01 00 f9 90 02 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000200  10 00 e0 f2 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  00000210  d0 03 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000220  e9 03 11 aa 29 41 00 91  30 01 00 f9 10 05 80 d2 
  00000230  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 e9 03 11 aa 
  00000240  29 61 00 91 30 01 00 f9  f0 03 00 91 10 a2 04 91 
  00000250  f0 1b 00 f9 f1 13 40 f9  e9 03 11 aa 30 01 40 f9 
  00000260  f0 73 00 f9 e9 03 11 aa  29 21 00 91 30 01 40 f9 
  00000270  f0 77 00 f9 e9 03 11 aa  29 41 00 91 30 01 40 f9 
  00000280  f0 7b 00 f9 e9 03 11 aa  29 61 00 91 30 01 40 f9 
  00000290  f0 7f 00 f9 f0 03 00 91  10 82 03 91 f0 1f 00 f9 
  000002a0  f1 1b 40 f9 f0 73 40 f9  e9 03 11 aa 30 01 00 f9 
  000002b0  f0 77 40 f9 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  000002c0  f0 7b 40 f9 e9 03 11 aa  29 41 00 91 30 01 00 f9 
  000002d0  f0 7f 40 f9 e9 03 11 aa  29 61 00 91 30 01 00 f9 
  000002e0  f0 03 00 91 10 22 05 91  f0 27 00 f9 f1 27 40 f9 
  000002f0  f0 1b 40 f9 30 02 00 f9  f0 27 40 f9 11 02 40 f9 
  00000300  f1 2f 00 f9 e0 2f 40 f9  3e ff ff 97 e0 33 00 f9 
  00000310  01 00 00 14 f0 0b 40 f9  11 02 40 f9 f1 37 00 f9 
  00000320  00 00 00 90 00 00 00 91  00 60 00 91 e1 37 40 f9 
  00000330  f0 37 40 f9 f0 03 00 f9  e2 33 40 f9 f0 33 40 f9 
  00000340  f0 07 00 f9 00 00 00 94  bf 03 00 91 fd 7b 55 a9 
  00000350  ff 83 05 91 00 00 80 d2  c0 03 5f d6 

.rodata (38 bytes):
  00000000  45 4c 46 20 50 49 45 20  72 6f 64 61 74 61 20 63 
  00000010  68 65 63 6b 00 00 00 00  25 73 3a 20 73 75 6d 3d 
  00000020  25 6c 6c 64 0a 00 
