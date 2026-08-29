fp-native dump: format=MachO arch=Aarch64 entry=0xec

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
fn Rectangle__area
  bb0 bb0
    alloca Virtual { id: 2, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 3, bank: General, size_bits: 64 }, symbol(local.1)
    load Virtual { id: 4, bank: Float, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 5, bank: General, size_bits: 64 }, symbol(local.1)
    gep Virtual { id: 6, bank: General, size_bits: 64 }, Virtual { id: 5, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 7, bank: General, size_bits: 64 }, Virtual { id: 6, bank: General, size_bits: 64 }
    load Virtual { id: 8, bank: Float, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 7, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 9, bank: Float, size_bits: 64 }, Virtual { id: 4, bank: Float, size_bits: 64 }, Virtual { id: 8, bank: Float, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 9, bank: Float, size_bits: 64 }
    load Virtual { id: 11, bank: Float, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn Rectangle__describe
  bb0 bb0
    call symbol(Rectangle__area)(local.1) cc=C tail=false
    br
  bb1 bb1
    intrinsic.call symbol(intrinsic.println), Virtual { id: 12, bank: Float, size_bits: 64 }
    ret
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    alloca Virtual { id: 19, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 19, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    alloca Virtual { id: 21, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    alloca Virtual { id: 23, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 23, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 19, bank: General, size_bits: 64 }
    load Virtual { id: 25, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 23, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(Circle__describe)(v25) cc=C tail=false
    br
  bb1 bb1
    alloca Virtual { id: 27, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 27, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 21, bank: General, size_bits: 64 }
    load Virtual { id: 29, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 27, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(Rectangle__describe)(v29) cc=C tail=false
    br
  bb2 bb2
    alloca Virtual { id: 31, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 31, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 19, bank: General, size_bits: 64 }
    load Virtual { id: 33, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 31, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(Circle__area)(v33) cc=C tail=false
    br
  bb3 bb3
    intrinsic.call symbol(intrinsic.println), Virtual { id: 34, bank: Float, size_bits: 64 }
    alloca Virtual { id: 36, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 21, bank: General, size_bits: 64 }
    load Virtual { id: 38, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(Rectangle__area)(v38) cc=C tail=false
    br
  bb4 bb4
    intrinsic.call symbol(intrinsic.println), Virtual { id: 39, bank: Float, size_bits: 64 }
    ret
fn Circle__area
  bb0 bb0
    alloca Virtual { id: 41, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 42, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 43, bank: General, size_bits: 64 }, symbol(local.1)
    load Virtual { id: 44, bank: Float, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 43, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 45, bank: Float, size_bits: 64 }, 4614256650576692846, Virtual { id: 44, bank: Float, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 42, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 45, bank: Float, size_bits: 64 }
    load Virtual { id: 47, bank: Float, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 42, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 48, bank: General, size_bits: 64 }, symbol(local.1)
    load Virtual { id: 49, bank: Float, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 48, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 50, bank: Float, size_bits: 64 }, Virtual { id: 47, bank: Float, size_bits: 64 }, Virtual { id: 49, bank: Float, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 50, bank: Float, size_bits: 64 }
    load Virtual { id: 52, bank: Float, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn Circle__describe
  bb0 bb0
    call symbol(Circle__area)(local.1) cc=C tail=false
    br
  bb1 bb1
    intrinsic.call symbol(intrinsic.println), Virtual { id: 53, bank: Float, size_bits: 64 }
    ret


Symbols:
  Rectangle__area                  0x00000000
  Rectangle__describe              0x0000009c
  main                             0x000000ec
  Circle__area                     0x000002dc
  Circle__describe                 0x000003a4

Text relocations:
  offset=0x000000c0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000d4 kind=CallRel32 symbol=printf addend=0
  offset=0x00000104 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000110 kind=CallRel32 symbol=printf addend=0
  offset=0x00000114 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000120 kind=CallRel32 symbol=printf addend=0
  offset=0x00000124 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000130 kind=CallRel32 symbol=printf addend=0
  offset=0x00000134 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000140 kind=CallRel32 symbol=printf addend=0
  offset=0x00000144 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000150 kind=CallRel32 symbol=printf addend=0
  offset=0x00000254 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000026c kind=CallRel32 symbol=printf addend=0
  offset=0x000002a4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000002bc kind=CallRel32 symbol=printf addend=0
  offset=0x000003c8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000003dc kind=CallRel32 symbol=printf addend=0

.text (1012 bytes):
  00000000  ff c3 05 d1 fd 7b 16 a9  fd 03 00 91 e0 8f 00 f9 
  00000010  1f 20 03 d5 f0 03 00 91  10 82 04 91 f0 03 00 f9 
  00000020  f0 8f 40 f9 f0 07 00 f9  f0 07 40 f9 00 02 40 fd 
  00000030  e0 0b 00 fd f0 8f 40 f9  f0 0f 00 f9 f0 0f 40 f9 
  00000040  11 01 80 d2 10 02 11 8b  f0 13 00 f9 f0 13 40 f9 
  00000050  f0 17 00 f9 f0 17 40 f9  00 02 40 fd e0 1b 00 fd 
  00000060  e0 0b 40 fd e1 1b 40 fd  00 08 61 1e e0 1f 00 fd 
  00000070  f1 03 40 f9 e0 1f 40 fd  20 02 00 fd f0 03 40 f9 
  00000080  00 02 40 fd e0 27 00 fd  e0 27 40 fd bf 03 00 91 
  00000090  fd 7b 56 a9 ff c3 05 91  c0 03 5f d6 ff 43 05 d1 
  000000a0  fd 7b 14 a9 fd 03 00 91  e0 97 00 f9 1f 20 03 d5 
  000000b0  e0 97 40 f9 d3 ff ff 97  e0 2f 00 fd 01 00 00 14 
  000000c0  00 00 00 90 00 00 00 91  e0 2f 40 fd e0 2f 40 fd 
  000000d0  e0 03 00 fd 00 00 00 94  bf 03 00 91 fd 7b 54 a9 
  000000e0  ff 43 05 91 00 00 80 d2  c0 03 5f d6 ff 03 12 d1 
  000000f0  f0 03 00 91 10 c2 11 91  1d 7a 00 a9 fd 03 00 91 
  00000100  1f 20 03 d5 00 00 00 90  00 00 00 91 00 40 00 91 
  00000110  00 00 00 94 00 00 00 90  00 00 00 91 00 c0 00 91 
  00000120  00 00 00 94 00 00 00 90  00 00 00 91 00 e0 01 91 
  00000130  00 00 00 94 00 00 00 90  00 00 00 91 00 a0 02 91 
  00000140  00 00 00 94 00 00 00 90  00 00 00 91 00 40 03 91 
  00000150  00 00 00 94 f0 03 00 91  10 a2 08 91 f0 47 00 f9 
  00000160  10 00 80 d2 10 00 a0 f2  10 00 c0 f2 90 02 e8 f2 
  00000170  f1 47 40 f9 30 02 00 f9  f0 03 00 91 10 a2 09 91 
  00000180  f0 4f 00 f9 f1 4f 40 f9  eb 03 11 aa 10 00 80 d2 
  00000190  10 00 a0 f2 10 00 c0 f2  10 02 e8 f2 ea 03 0b aa 
  000001a0  50 01 00 f9 10 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  000001b0  10 03 e8 f2 ea 03 0b aa  4a 21 00 91 50 01 00 f9 
  000001c0  f0 03 00 91 10 a2 0d 91  f0 57 00 f9 f1 57 40 f9 
  000001d0  f0 47 40 f9 30 02 00 f9  f0 57 40 f9 11 02 40 f9 
  000001e0  f1 5f 00 f9 e0 5f 40 f9  6f 00 00 94 01 00 00 14 
  000001f0  f0 03 00 91 10 a2 0e 91  f0 67 00 f9 f1 67 40 f9 
  00000200  f0 4f 40 f9 30 02 00 f9  f0 67 40 f9 11 02 40 f9 
  00000210  f1 6f 00 f9 e0 6f 40 f9  a1 ff ff 97 01 00 00 14 
  00000220  f0 03 00 91 10 a2 0f 91  f0 77 00 f9 f1 77 40 f9 
  00000230  f0 47 40 f9 30 02 00 f9  f0 77 40 f9 11 02 40 f9 
  00000240  f1 7f 00 f9 e0 7f 40 f9  25 00 00 94 e0 83 00 fd 
  00000250  01 00 00 14 00 00 00 90  00 00 00 91 00 60 03 91 
  00000260  e0 83 40 fd e0 83 40 fd  e0 03 00 fd 00 00 00 94 
  00000270  f0 03 00 91 10 a2 10 91  f0 8b 00 f9 f1 8b 40 f9 
  00000280  f0 4f 40 f9 30 02 00 f9  f0 8b 40 f9 11 02 40 f9 
  00000290  f1 93 00 f9 e0 93 40 f9  5a ff ff 97 e0 97 00 fd 
  000002a0  01 00 00 14 00 00 00 90  00 00 00 91 00 c0 03 91 
  000002b0  e0 97 40 fd e0 97 40 fd  e0 03 00 fd 00 00 00 94 
  000002c0  bf 03 00 91 f0 03 00 91  10 c2 11 91 1d 7a 40 a9 
  000002d0  ff 03 12 91 00 00 80 d2  c0 03 5f d6 ff 03 07 d1 
  000002e0  fd 7b 1b a9 fd 03 00 91  e0 93 00 f9 1f 20 03 d5 
  000002f0  f0 03 00 91 10 c2 04 91  f0 5b 00 f9 f0 03 00 91 
  00000300  10 c2 05 91 f0 5f 00 f9  f0 93 40 f9 f0 63 00 f9 
  00000310  f0 63 40 f9 00 02 40 fd  e0 67 00 fd d0 cd 90 d2 
  00000320  70 03 be f2 30 3f c4 f2  30 01 e8 f2 00 02 67 9e 
  00000330  e1 67 40 fd 00 08 61 1e  e0 6b 00 fd f1 5f 40 f9 
  00000340  e0 6b 40 fd 20 02 00 fd  f0 5f 40 f9 00 02 40 fd 
  00000350  e0 73 00 fd f0 93 40 f9  f0 77 00 f9 f0 77 40 f9 
  00000360  00 02 40 fd e0 7b 00 fd  e0 73 40 fd e1 7b 40 fd 
  00000370  00 08 61 1e e0 7f 00 fd  f1 5b 40 f9 e0 7f 40 fd 
  00000380  20 02 00 fd f0 5b 40 f9  00 02 40 fd e0 87 00 fd 
  00000390  e0 87 40 fd bf 03 00 91  fd 7b 5b a9 ff 03 07 91 
  000003a0  c0 03 5f d6 ff 43 05 d1  fd 7b 14 a9 fd 03 00 91 
  000003b0  e0 97 00 f9 1f 20 03 d5  e0 97 40 f9 c8 ff ff 97 
  000003c0  e0 8b 00 fd 01 00 00 14  00 00 00 90 00 00 00 91 
  000003d0  e0 8b 40 fd e0 8b 40 fd  e0 03 00 fd 00 00 00 94 
  000003e0  bf 03 00 91 fd 7b 54 a9  ff 43 05 91 00 00 80 d2 
  000003f0  c0 03 5f d6 

.rodata (261 bytes):
  00000000  61 72 65 61 3d 25 2e 32  66 0a 00 00 00 00 00 00 
  00000010  f0 9f 93 98 20 54 75 74  6f 72 69 61 6c 3a 20 31 
  00000020  36 5f 74 72 61 69 74 73  2e 66 70 0a 00 00 00 00 
  00000030  f0 9f a7 ad 20 46 6f 63  75 73 3a 20 54 72 61 69 
  00000040  74 73 3a 20 64 65 66 69  6e 69 6e 67 20 73 68 61 
  00000050  72 65 64 20 62 65 68 61  76 69 6f 72 20 77 69 74 
  00000060  68 20 64 65 66 61 75 6c  74 20 6d 65 74 68 6f 64 
  00000070  73 0a 00 00 00 00 00 00  f0 9f a7 aa 20 57 68 61 
  00000080  74 20 74 6f 20 6c 6f 6f  6b 20 66 6f 72 3a 20 6c 
  00000090  61 62 65 6c 65 64 20 6f  75 74 70 75 74 73 20 62 
  000000a0  65 6c 6f 77 0a 00 00 00  e2 9c 85 20 45 78 70 65 
  000000b0  63 74 61 74 69 6f 6e 3a  20 6f 75 74 70 75 74 73 
  000000c0  20 6d 61 74 63 68 20 6c  61 62 65 6c 73 0a 00 00 
  000000d0  0a 00 00 00 00 00 00 00  63 69 72 63 6c 65 20 61 
  000000e0  72 65 61 3d 25 2e 32 66  0a 00 00 00 00 00 00 00 
  000000f0  72 65 63 74 61 6e 67 6c  65 20 61 72 65 61 3d 25 
  00000100  2e 32 66 0a 00 
