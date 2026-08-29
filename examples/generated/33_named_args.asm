fp-native dump: format=MachO arch=Aarch64 entry=0x1b0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data_0 ty=Array(I8, 6) constant=true initializer=Some(Bytes([97, 108, 112, 104, 97, 0]))
global __const_data_1 ty=Array(I8, 5) constant=true initializer=Some(Bytes([98, 101, 116, 97, 0]))
fn summarize
  bb0 bb0
    alloca Virtual { id: 4, bank: General, size_bits: 64 }, 16
    alloca Virtual { id: 5, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    bitcast Virtual { id: 7, bank: General, size_bits: 64 }, Virtual { id: 5, bank: General, size_bits: 64 }
    load Virtual { id: 8, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 7, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.format), Virtual { id: 8, bank: General, size_bits: 64 }, symbol(local.2), symbol(local.3)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 4, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 9, bank: General, size_bits: 64 }
    load Virtual { id: 11, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 4, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    call symbol(summarize)(struct(len=2), 3, true) cc=C tail=false
    alloca Virtual { id: 17, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 17, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 16, bank: General, size_bits: 64 }
    br
  bb1 bb1
    bitcast Virtual { id: 19, bank: General, size_bits: 64 }, Virtual { id: 17, bank: General, size_bits: 64 }
    load Virtual { id: 20, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 19, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 20, bank: General, size_bits: 64 }
    call symbol(summarize)(struct(len=2), 7, false) cc=C tail=false
    alloca Virtual { id: 23, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 23, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 22, bank: General, size_bits: 64 }
    br
  bb2 bb2
    bitcast Virtual { id: 25, bank: General, size_bits: 64 }, Virtual { id: 23, bank: General, size_bits: 64 }
    load Virtual { id: 26, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 25, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 26, bank: General, size_bits: 64 }
    call symbol(add)(5, 2) cc=C tail=false
    br
  bb3 bb3
    intrinsic.call symbol(intrinsic.println), Virtual { id: 28, bank: General, size_bits: 64 }
    ret
fn add
  bb0 bb0
    alloca Virtual { id: 30, bank: General, size_bits: 64 }, 8
    add Virtual { id: 31, bank: General, size_bits: 64 }, symbol(local.1), symbol(local.2)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 30, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 31, bank: General, size_bits: 64 }
    load Virtual { id: 33, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 30, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret


Symbols:
  summarize                        0x00000000
  main                             0x000001b0
  add                              0x000003c4

Text relocations:
  offset=0x00000094 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000c4 kind=CallRel32 symbol=snprintf addend=0
  offset=0x000000dc kind=CallRel32 symbol=malloc addend=0
  offset=0x000000f0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000120 kind=CallRel32 symbol=snprintf addend=0
  offset=0x000001c8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001d4 kind=CallRel32 symbol=printf addend=0
  offset=0x000001d8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001e4 kind=CallRel32 symbol=printf addend=0
  offset=0x000001e8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001f4 kind=CallRel32 symbol=printf addend=0
  offset=0x000001f8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000204 kind=CallRel32 symbol=printf addend=0
  offset=0x00000218 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000002a4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000002bc kind=CallRel32 symbol=printf addend=0
  offset=0x000002d0 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x0000035c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000374 kind=CallRel32 symbol=printf addend=0
  offset=0x0000038c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000003a4 kind=CallRel32 symbol=printf addend=0

.text (1060 bytes):
  00000000  ff c3 0c d1 f0 03 00 91  10 82 0c 91 1d 7a 00 a9 
  00000010  fd 03 00 91 e0 7b 00 f9  e9 03 01 aa 30 01 40 f9 
  00000020  f0 6b 00 f9 e9 03 01 aa  29 21 00 91 30 01 40 f9 
  00000030  f0 6f 00 f9 e2 73 00 f9  e3 a3 03 39 1f 20 03 d5 
  00000040  f0 03 00 91 10 62 04 91  f0 13 00 f9 f0 03 00 91 
  00000050  10 62 08 91 f0 17 00 f9  f1 17 40 f9 f0 6b 40 f9 
  00000060  e9 03 11 aa 30 01 00 f9  f0 6f 40 f9 e9 03 11 aa 
  00000070  29 21 00 91 30 01 00 f9  f0 17 40 f9 f0 1f 00 f9 
  00000080  f0 1f 40 f9 11 02 40 f9  f1 23 00 f9 00 00 80 d2 
  00000090  01 00 80 d2 02 00 00 90  42 00 00 91 42 40 00 91 
  000000a0  e3 23 40 f9 f0 23 40 f9  f0 03 00 f9 e4 73 40 f9 
  000000b0  f0 73 40 f9 f0 07 00 f9  e5 a3 43 39 f0 a3 43 39 
  000000c0  f0 0b 00 f9 00 00 00 94  f0 03 00 aa f0 83 00 f9 
  000000d0  10 06 00 91 f0 27 00 f9  e0 03 10 aa 00 00 00 94 
  000000e0  e9 03 00 aa e0 03 09 aa  e1 27 40 f9 e9 27 00 f9 
  000000f0  02 00 00 90 42 00 00 91  42 40 00 91 e3 23 40 f9 
  00000100  f0 23 40 f9 f0 03 00 f9  e4 73 40 f9 f0 73 40 f9 
  00000110  f0 07 00 f9 e5 a3 43 39  f0 a3 43 39 f0 0b 00 f9 
  00000120  00 00 00 94 e9 27 40 f9  e9 7f 00 f9 f1 13 40 f9 
  00000130  f0 7f 40 f9 e9 03 11 aa  30 01 00 f9 f0 83 40 f9 
  00000140  e9 03 11 aa 29 21 00 91  30 01 00 f9 f1 13 40 f9 
  00000150  e9 03 11 aa 30 01 40 f9  f0 87 00 f9 e9 03 11 aa 
  00000160  29 21 00 91 30 01 40 f9  f0 8b 00 f9 f0 03 00 91 
  00000170  10 22 04 91 f0 2f 00 f9  f1 7b 40 f9 f0 87 40 f9 
  00000180  e9 03 11 aa 30 01 00 f9  f0 8b 40 f9 e9 03 11 aa 
  00000190  29 21 00 91 30 01 00 f9  bf 03 00 91 f0 03 00 91 
  000001a0  10 82 0c 91 1d 7a 40 a9  ff c3 0c 91 c0 03 5f d6 
  000001b0  ff 43 0e d1 f0 03 00 91  10 02 0e 91 1d 7a 00 a9 
  000001c0  fd 03 00 91 1f 20 03 d5  00 00 00 90 00 00 00 91 
  000001d0  00 c0 00 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000001e0  00 40 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000001f0  00 00 02 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000200  00 e0 02 91 00 00 00 94  e0 03 00 91 00 80 05 91 
  00000210  f1 03 00 91 31 42 05 91  10 00 00 90 10 02 00 91 
  00000220  e9 03 11 aa 30 01 00 f9  b0 00 80 d2 10 00 a0 f2 
  00000230  10 00 c0 f2 10 00 e0 f2  e9 03 11 aa 29 21 00 91 
  00000240  30 01 00 f9 e1 03 11 aa  62 00 80 d2 23 00 80 d2 
  00000250  6c ff ff 97 f0 03 00 91  10 82 05 91 f0 33 00 f9 
  00000260  f0 03 00 91 10 02 06 91  f0 37 00 f9 f1 37 40 f9 
  00000270  f0 b3 40 f9 e9 03 11 aa  30 01 00 f9 f0 b7 40 f9 
  00000280  e9 03 11 aa 29 21 00 91  30 01 00 f9 01 00 00 14 
  00000290  f0 37 40 f9 f0 3f 00 f9  f0 3f 40 f9 11 02 40 f9 
  000002a0  f1 43 00 f9 00 00 00 90  00 00 00 91 00 00 03 91 
  000002b0  e1 43 40 f9 f0 43 40 f9  f0 03 00 f9 00 00 00 94 
  000002c0  e0 03 00 91 00 c0 05 91  f1 03 00 91 31 42 05 91 
  000002d0  10 00 00 90 10 02 00 91  e9 03 11 aa 30 01 00 f9 
  000002e0  90 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  000002f0  e9 03 11 aa 29 21 00 91  30 01 00 f9 e1 03 11 aa 
  00000300  e2 00 80 d2 03 00 80 d2  3e ff ff 97 f0 03 00 91 
  00000310  10 c2 05 91 f0 4b 00 f9  f0 03 00 91 10 02 0a 91 
  00000320  f0 4f 00 f9 f1 4f 40 f9  f0 bb 40 f9 e9 03 11 aa 
  00000330  30 01 00 f9 f0 bf 40 f9  e9 03 11 aa 29 21 00 91 
  00000340  30 01 00 f9 01 00 00 14  f0 4f 40 f9 f0 57 00 f9 
  00000350  f0 57 40 f9 11 02 40 f9  f1 5b 00 f9 00 00 00 90 
  00000360  00 00 00 91 00 40 03 91  e1 5b 40 f9 f0 5b 40 f9 
  00000370  f0 03 00 f9 00 00 00 94  a0 00 80 d2 41 00 80 d2 
  00000380  11 00 00 94 e0 63 00 f9  01 00 00 14 00 00 00 90 
  00000390  00 00 00 91 00 80 03 91  e1 63 40 f9 f0 63 40 f9 
  000003a0  f0 03 00 f9 00 00 00 94  bf 03 00 91 f0 03 00 91 
  000003b0  10 02 0e 91 1d 7a 40 a9  ff 43 0e 91 00 00 80 d2 
  000003c0  c0 03 5f d6 ff 03 04 d1  fd 7b 0f a9 fd 03 00 91 
  000003d0  e0 53 00 f9 e1 57 00 f9  1f 20 03 d5 f0 03 00 91 
  000003e0  10 c2 02 91 f0 3f 00 f9  f0 53 40 f9 f1 57 40 f9 
  000003f0  10 02 11 8b f0 43 00 f9  f1 3f 40 f9 f0 43 40 f9 
  00000400  30 02 00 f9 f0 3f 40 f9  11 02 40 f9 f1 4b 00 f9 
  00000410  e0 4b 40 f9 bf 03 00 91  fd 7b 4f a9 ff 03 04 91 
  00000420  c0 03 5f d6 

.rodata (235 bytes):
  00000000  61 6c 70 68 61 00 62 65  74 61 00 00 00 00 00 00 
  00000010  6c 61 62 65 6c 3d 25 73  20 63 6f 75 6e 74 3d 25 
  00000020  6c 6c 64 20 61 63 74 69  76 65 3d 25 64 00 00 00 
  00000030  54 75 74 6f 72 69 61 6c  3a 20 33 33 5f 6e 61 6d 
  00000040  65 64 5f 61 72 67 73 2e  66 70 0a 00 00 00 00 00 
  00000050  46 6f 63 75 73 3a 20 4e  61 6d 65 64 20 61 72 67 
  00000060  75 6d 65 6e 74 73 20 69  6e 20 66 75 6e 63 74 69 
  00000070  6f 6e 20 63 61 6c 6c 73  0a 00 00 00 00 00 00 00 
  00000080  45 78 70 65 63 74 61 74  69 6f 6e 3a 20 6b 65 79 
  00000090  77 6f 72 64 20 61 72 67  75 6d 65 6e 74 73 20 63 
  000000a0  61 6e 20 62 65 20 72 65  6f 72 64 65 72 65 64 0a 
  000000b0  00 00 00 00 00 00 00 00  0a 00 00 00 00 00 00 00 
  000000c0  66 69 72 73 74 3a 20 25  73 0a 00 00 00 00 00 00 
  000000d0  73 65 63 6f 6e 64 3a 20  25 73 0a 00 00 00 00 00 
  000000e0  61 64 64 3a 20 25 6c 6c  64 0a 00 
