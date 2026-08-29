fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data_0 ty=Array(I8, 6) constant=true initializer=Some(Bytes([97, 108, 112, 104, 97, 0]))
global __const_data_1 ty=Array(I8, 5) constant=true initializer=Some(Bytes([98, 101, 116, 97, 0]))
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    zext Virtual { id: 4, bank: General, size_bits: 32 }, 1
    call symbol(_33_named_args__summarize)(struct(len=2), 3, true, struct(len=2), 3, v4) cc=C tail=false
    alloca Virtual { id: 6, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 6, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 5, bank: General, size_bits: 64 }
    br
  bb1 bb1
    bitcast Virtual { id: 8, bank: General, size_bits: 64 }, Virtual { id: 6, bank: General, size_bits: 64 }
    load Virtual { id: 9, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 8, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 9, bank: General, size_bits: 64 }
    zext Virtual { id: 11, bank: General, size_bits: 32 }, 0
    call symbol(_33_named_args__summarize)(struct(len=2), 7, false, struct(len=2), 7, v11) cc=C tail=false
    alloca Virtual { id: 13, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 13, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 12, bank: General, size_bits: 64 }
    br
  bb2 bb2
    bitcast Virtual { id: 15, bank: General, size_bits: 64 }, Virtual { id: 13, bank: General, size_bits: 64 }
    load Virtual { id: 16, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 15, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 16, bank: General, size_bits: 64 }
    call symbol(_33_named_args__add)(5, 2, 5, 2) cc=C tail=false
    br
  bb3 bb3
    intrinsic.call symbol(intrinsic.println), Virtual { id: 18, bank: General, size_bits: 64 }
    ret
fn _33_named_args__add
  bb0 bb0
    alloca Virtual { id: 20, bank: General, size_bits: 64 }, 8
    add Virtual { id: 21, bank: General, size_bits: 64 }, symbol(local.1), symbol(local.2)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 20, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 21, bank: General, size_bits: 64 }
    load Virtual { id: 23, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 20, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn _33_named_args__summarize
  bb0 bb0
    alloca Virtual { id: 24, bank: General, size_bits: 64 }, 16
    alloca Virtual { id: 25, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 25, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    bitcast Virtual { id: 27, bank: General, size_bits: 64 }, Virtual { id: 25, bank: General, size_bits: 64 }
    load Virtual { id: 28, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 27, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.format), Virtual { id: 28, bank: General, size_bits: 64 }, symbol(local.2), symbol(local.3)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 24, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 29, bank: General, size_bits: 64 }
    load Virtual { id: 31, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 24, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret


Symbols:
  main                             0x00000000
  _33_named_args__add              0x000002d4
  _33_named_args__summarize        0x00000334

Text relocations:
  offset=0x00000018 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000024 kind=CallRel32 symbol=printf addend=0
  offset=0x00000028 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000034 kind=CallRel32 symbol=printf addend=0
  offset=0x00000038 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000044 kind=CallRel32 symbol=printf addend=0
  offset=0x00000048 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000054 kind=CallRel32 symbol=printf addend=0
  offset=0x00000084 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000000c4 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000150 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000168 kind=CallRel32 symbol=printf addend=0
  offset=0x00000198 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x000001d8 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x00000264 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000027c kind=CallRel32 symbol=printf addend=0
  offset=0x0000029c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000002b4 kind=CallRel32 symbol=printf addend=0
  offset=0x000003c8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000003f8 kind=CallRel32 symbol=snprintf addend=0
  offset=0x00000410 kind=CallRel32 symbol=malloc addend=0
  offset=0x00000424 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000454 kind=CallRel32 symbol=snprintf addend=0

.text (1252 bytes):
  00000000  ff 83 0e d1 f0 03 00 91  10 42 0e 91 1d 7a 00 a9 
  00000010  fd 03 00 91 1f 20 03 d5  00 00 00 90 00 00 00 91 
  00000020  00 40 00 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000030  00 c0 00 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000040  00 80 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000050  00 60 02 91 00 00 00 94  30 00 80 d2 31 00 80 d2 
  00000060  11 00 a0 f2 11 00 c0 f2  11 00 e0 f2 10 02 11 8a 
  00000070  f0 1b 00 f9 e0 03 00 91  00 c0 05 91 f1 03 00 91 
  00000080  31 82 05 91 10 00 00 90  10 02 00 91 e9 03 11 aa 
  00000090  30 01 00 f9 b0 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  000000a0  10 00 e0 f2 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  000000b0  e1 03 11 aa 62 00 80 d2  23 00 80 d2 f1 03 00 91 
  000000c0  31 82 05 91 10 00 00 90  10 02 00 91 e9 03 11 aa 
  000000d0  30 01 00 f9 b0 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  000000e0  10 00 e0 f2 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  000000f0  e4 03 11 aa 65 00 80 d2  e6 33 80 b9 8e 00 00 94 
  00000100  f0 03 00 91 10 c2 05 91  f0 1f 00 f9 f0 03 00 91 
  00000110  10 42 06 91 f0 23 00 f9  f1 23 40 f9 f0 bb 40 f9 
  00000120  e9 03 11 aa 30 01 00 f9  f0 bf 40 f9 e9 03 11 aa 
  00000130  29 21 00 91 30 01 00 f9  01 00 00 14 f0 23 40 f9 
  00000140  f0 2b 00 f9 f0 2b 40 f9  11 02 40 f9 f1 2f 00 f9 
  00000150  00 00 00 90 00 00 00 91  00 80 02 91 e1 2f 40 f9 
  00000160  f0 2f 40 f9 f0 03 00 f9  00 00 00 94 10 00 80 d2 
  00000170  31 00 80 d2 11 00 a0 f2  11 00 c0 f2 11 00 e0 f2 
  00000180  10 02 11 8a f0 37 00 f9  e0 03 00 91 00 00 06 91 
  00000190  f1 03 00 91 31 82 05 91  10 00 00 90 10 02 00 91 
  000001a0  e9 03 11 aa 30 01 00 f9  90 00 80 d2 10 00 a0 f2 
  000001b0  10 00 c0 f2 10 00 e0 f2  e9 03 11 aa 29 21 00 91 
  000001c0  30 01 00 f9 e1 03 11 aa  e2 00 80 d2 03 00 80 d2 
  000001d0  f1 03 00 91 31 82 05 91  10 00 00 90 10 02 00 91 
  000001e0  e9 03 11 aa 30 01 00 f9  90 00 80 d2 10 00 a0 f2 
  000001f0  10 00 c0 f2 10 00 e0 f2  e9 03 11 aa 29 21 00 91 
  00000200  30 01 00 f9 e4 03 11 aa  e5 00 80 d2 e6 6b 80 b9 
  00000210  49 00 00 94 f0 03 00 91  10 02 06 91 f0 3b 00 f9 
  00000220  f0 03 00 91 10 42 0a 91  f0 3f 00 f9 f1 3f 40 f9 
  00000230  f0 c3 40 f9 e9 03 11 aa  30 01 00 f9 f0 c7 40 f9 
  00000240  e9 03 11 aa 29 21 00 91  30 01 00 f9 01 00 00 14 
  00000250  f0 3f 40 f9 f0 47 00 f9  f0 47 40 f9 11 02 40 f9 
  00000260  f1 4b 00 f9 00 00 00 90  00 00 00 91 00 c0 02 91 
  00000270  e1 4b 40 f9 f0 4b 40 f9  f0 03 00 f9 00 00 00 94 
  00000280  a0 00 80 d2 41 00 80 d2  a2 00 80 d2 43 00 80 d2 
  00000290  11 00 00 94 e0 53 00 f9  01 00 00 14 00 00 00 90 
  000002a0  00 00 00 91 00 00 03 91  e1 53 40 f9 f0 53 40 f9 
  000002b0  f0 03 00 f9 00 00 00 94  bf 03 00 91 f0 03 00 91 
  000002c0  10 42 0e 91 1d 7a 40 a9  ff 83 0e 91 00 00 80 d2 
  000002d0  c0 03 5f d6 ff 43 04 d1  fd 7b 10 a9 fd 03 00 91 
  000002e0  e0 5b 00 f9 e1 5f 00 f9  1f 20 03 d5 f0 03 00 91 
  000002f0  10 02 03 91 f0 2f 00 f9  f0 5b 40 f9 f1 5f 40 f9 
  00000300  10 02 11 8b f0 33 00 f9  f1 2f 40 f9 f0 33 40 f9 
  00000310  30 02 00 f9 f0 2f 40 f9  11 02 40 f9 f1 3b 00 f9 
  00000320  e0 3b 40 f9 bf 03 00 91  fd 7b 50 a9 ff 43 04 91 
  00000330  c0 03 5f d6 ff 03 0d d1  f0 03 00 91 10 c2 0c 91 
  00000340  1d 7a 00 a9 fd 03 00 91  e0 83 00 f9 e9 03 01 aa 
  00000350  30 01 40 f9 f0 73 00 f9  e9 03 01 aa 29 21 00 91 
  00000360  30 01 40 f9 f0 77 00 f9  e2 7b 00 f9 e3 e3 03 39 
  00000370  1f 20 03 d5 f0 03 00 91  10 a2 04 91 f0 4b 00 f9 
  00000380  f0 03 00 91 10 a2 08 91  f0 4f 00 f9 f1 4f 40 f9 
  00000390  f0 73 40 f9 e9 03 11 aa  30 01 00 f9 f0 77 40 f9 
  000003a0  e9 03 11 aa 29 21 00 91  30 01 00 f9 f0 4f 40 f9 
  000003b0  f0 57 00 f9 f0 57 40 f9  11 02 40 f9 f1 5b 00 f9 
  000003c0  00 00 80 d2 01 00 80 d2  02 00 00 90 42 00 00 91 
  000003d0  42 40 03 91 e3 5b 40 f9  f0 5b 40 f9 f0 03 00 f9 
  000003e0  e4 7b 40 f9 f0 7b 40 f9  f0 07 00 f9 e5 e3 43 39 
  000003f0  f0 e3 43 39 f0 0b 00 f9  00 00 00 94 f0 03 00 aa 
  00000400  f0 8b 00 f9 10 06 00 91  f0 5f 00 f9 e0 03 10 aa 
  00000410  00 00 00 94 e9 03 00 aa  e0 03 09 aa e1 5f 40 f9 
  00000420  e9 5f 00 f9 02 00 00 90  42 00 00 91 42 40 03 91 
  00000430  e3 5b 40 f9 f0 5b 40 f9  f0 03 00 f9 e4 7b 40 f9 
  00000440  f0 7b 40 f9 f0 07 00 f9  e5 e3 43 39 f0 e3 43 39 
  00000450  f0 0b 00 f9 00 00 00 94  e9 5f 40 f9 e9 87 00 f9 
  00000460  f1 4b 40 f9 f0 87 40 f9  e9 03 11 aa 30 01 00 f9 
  00000470  f0 8b 40 f9 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  00000480  f1 4b 40 f9 e9 03 11 aa  30 01 40 f9 f0 8f 00 f9 
  00000490  e9 03 11 aa 29 21 00 91  30 01 40 f9 f0 93 00 f9 
  000004a0  f0 03 00 91 10 62 04 91  f0 67 00 f9 f1 83 40 f9 
  000004b0  f0 8f 40 f9 e9 03 11 aa  30 01 00 f9 f0 93 40 f9 
  000004c0  e9 03 11 aa 29 21 00 91  30 01 00 f9 bf 03 00 91 
  000004d0  f0 03 00 91 10 c2 0c 91  1d 7a 40 a9 ff 03 0d 91 
  000004e0  c0 03 5f d6 

.rodata (238 bytes):
  00000000  61 6c 70 68 61 00 62 65  74 61 00 00 00 00 00 00 
  00000010  54 75 74 6f 72 69 61 6c  3a 20 33 33 5f 6e 61 6d 
  00000020  65 64 5f 61 72 67 73 2e  66 70 0a 00 00 00 00 00 
  00000030  46 6f 63 75 73 3a 20 4e  61 6d 65 64 20 61 72 67 
  00000040  75 6d 65 6e 74 73 20 69  6e 20 66 75 6e 63 74 69 
  00000050  6f 6e 20 63 61 6c 6c 73  0a 00 00 00 00 00 00 00 
  00000060  45 78 70 65 63 74 61 74  69 6f 6e 3a 20 6b 65 79 
  00000070  77 6f 72 64 20 61 72 67  75 6d 65 6e 74 73 20 63 
  00000080  61 6e 20 62 65 20 72 65  6f 72 64 65 72 65 64 0a 
  00000090  00 00 00 00 00 00 00 00  0a 00 00 00 00 00 00 00 
  000000a0  66 69 72 73 74 3a 20 25  73 0a 00 00 00 00 00 00 
  000000b0  73 65 63 6f 6e 64 3a 20  25 73 0a 00 00 00 00 00 
  000000c0  61 64 64 3a 20 25 6c 6c  64 0a 00 00 00 00 00 00 
  000000d0  6c 61 62 65 6c 3d 25 73  20 63 6f 75 6e 74 3d 25 
  000000e0  6c 6c 64 20 61 63 74 69  76 65 3d 25 64 00 
