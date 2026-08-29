fp-native dump: format=MachO arch=Aarch64 entry=0x1b8

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data_0 ty=Array(I8, 21) constant=true initializer=Some(Bytes([69, 76, 70, 32, 80, 73, 69, 32, 114, 111, 100, 97, 116, 97, 32, 99, 104, 101, 99, 107, 0]))
fn _28_elf_pie_rodata__sum
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 1, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 2, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    br
  bb1 bb1
    alloca Virtual { id: 5, bank: General, size_bits: 64 }, 1
    load Virtual { id: 6, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 7, bank: General, size_bits: 8 }, Virtual { id: 6, bank: General, size_bits: 64 }, 4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 7, bank: General, size_bits: 8 }
    load Virtual { id: 9, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 10, bank: General, size_bits: 8 }, Virtual { id: 9, bank: General, size_bits: 8 }, 1
    condbr
  bb2 bb2
    alloca Virtual { id: 11, bank: General, size_bits: 64 }, 8
    load Virtual { id: 12, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 11, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 12, bank: General, size_bits: 64 }
    load Virtual { id: 14, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 15, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 11, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 16, bank: General, size_bits: 64 }, Virtual { id: 15, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 17, bank: General, size_bits: 64 }, symbol(local.1)
    gep Virtual { id: 18, bank: General, size_bits: 64 }, Virtual { id: 17, bank: General, size_bits: 64 }, Virtual { id: 16, bank: General, size_bits: 64 }
    bitcast Virtual { id: 19, bank: General, size_bits: 64 }, Virtual { id: 18, bank: General, size_bits: 64 }
    load Virtual { id: 20, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 19, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 21, bank: General, size_bits: 64 }, Virtual { id: 14, bank: General, size_bits: 64 }, Virtual { id: 20, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 21, bank: General, size_bits: 64 }
    load Virtual { id: 23, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 24, bank: General, size_bits: 64 }, Virtual { id: 23, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 24, bank: General, size_bits: 64 }
    br
  bb3 bb3
    load Virtual { id: 26, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 26, bank: General, size_bits: 64 }
    load Virtual { id: 28, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn main
  bb0 bb0
    alloca Virtual { id: 29, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 29, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    alloca Virtual { id: 31, bank: General, size_bits: 64 }, 32
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 31, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.array)
    alloca Virtual { id: 33, bank: General, size_bits: 64 }, 32
    load Virtual { id: 34, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 31, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(32), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 33, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 34, bank: General, size_bits: 64 }
    alloca Virtual { id: 36, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 33, bank: General, size_bits: 64 }
    alloca Virtual { id: 38, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 38, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 33, bank: General, size_bits: 64 }
    load Virtual { id: 40, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 41, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 38, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(_28_elf_pie_rodata__sum)(v40, v41) cc=C tail=false
    br
  bb1 bb1
    bitcast Virtual { id: 43, bank: General, size_bits: 64 }, Virtual { id: 29, bank: General, size_bits: 64 }
    load Virtual { id: 44, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 43, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 44, bank: General, size_bits: 64 }, Virtual { id: 42, bank: General, size_bits: 64 }
    ret


Symbols:
  _28_elf_pie_rodata__sum          0x00000000
  main                             0x000001b8

Text relocations:
  offset=0x000001e4 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000394 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000003b8 kind=CallRel32 symbol=printf addend=0

.text (984 bytes):
  00000000  ff 43 0a d1 f0 03 00 91  10 02 0a 91 1d 7a 00 a9 
  00000010  fd 03 00 91 e0 a7 00 f9  1f 20 03 d5 f0 03 00 91 
  00000020  10 c2 05 91 f0 03 00 f9  f0 03 00 91 10 c2 06 91 
  00000030  f0 07 00 f9 f0 03 00 91  10 c2 07 91 f0 0b 00 f9 
  00000040  f1 0b 40 f9 10 00 80 d2  30 02 00 f9 f1 03 40 f9 
  00000050  10 00 80 d2 30 02 00 f9  01 00 00 14 f0 03 00 91 
  00000060  10 c2 08 91 f0 17 00 f9  f0 0b 40 f9 11 02 40 f9 
  00000070  f1 1b 00 f9 f0 1b 40 f9  1f 12 00 f1 f0 a7 9f 9a 
  00000080  f0 1f 00 f9 f1 17 40 f9  f0 e3 40 39 30 02 00 39 
  00000090  f0 17 40 f9 11 02 40 39  f1 27 00 f9 f0 23 41 39 
  000000a0  1f 06 00 f1 f0 17 9f 9a  f0 2b 00 f9 f0 2b 40 f9 
  000000b0  1f 02 00 f1 41 00 00 54  30 00 00 14 f0 03 00 91 
  000000c0  10 e2 08 91 f0 2f 00 f9  f0 0b 40 f9 11 02 40 f9 
  000000d0  f1 33 00 f9 f1 2f 40 f9  f0 33 40 f9 30 02 00 f9 
  000000e0  f0 03 40 f9 11 02 40 f9  f1 3b 00 f9 f0 2f 40 f9 
  000000f0  11 02 40 f9 f1 3f 00 f9  f0 3f 40 f9 11 01 80 d2 
  00000100  10 7e 11 9b f0 43 00 f9  f0 a7 40 f9 f0 47 00 f9 
  00000110  f0 47 40 f9 f1 43 40 f9  10 02 11 8b f0 4b 00 f9 
  00000120  f0 4b 40 f9 f0 4f 00 f9  f0 4f 40 f9 11 02 40 f9 
  00000130  f1 53 00 f9 f0 3b 40 f9  f1 53 40 f9 10 02 11 8b 
  00000140  f0 57 00 f9 f1 03 40 f9  f0 57 40 f9 30 02 00 f9 
  00000150  f0 0b 40 f9 11 02 40 f9  f1 5f 00 f9 f0 5f 40 f9 
  00000160  10 06 00 91 f0 63 00 f9  f1 0b 40 f9 f0 63 40 f9 
  00000170  30 02 00 f9 ba ff ff 17  f0 03 40 f9 11 02 40 f9 
  00000180  f1 6b 00 f9 f1 07 40 f9  f0 6b 40 f9 30 02 00 f9 
  00000190  f0 07 40 f9 11 02 40 f9  f1 73 00 f9 e0 73 40 f9 
  000001a0  bf 03 00 91 f0 03 00 91  10 02 0a 91 1d 7a 40 a9 
  000001b0  ff 43 0a 91 c0 03 5f d6  ff c3 2d d1 f0 03 00 91 
  000001c0  10 82 2d 91 1d 7a 00 a9  fd 03 00 91 1f 20 03 d5 
  000001d0  f0 03 00 91 10 82 07 91  f0 63 00 f9 f1 63 40 f9 
  000001e0  eb 03 11 aa 10 00 00 90  10 02 00 91 ea 03 0b aa 
  000001f0  50 01 00 f9 90 02 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000200  10 00 e0 f2 ea 03 0b aa  4a 21 00 91 50 01 00 f9 
  00000210  f0 03 00 91 10 82 0b 91  f0 6b 00 f9 f1 6b 40 f9 
  00000220  50 01 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000230  e9 03 11 aa 30 01 00 f9  90 02 80 d2 10 00 a0 f2 
  00000240  10 00 c0 f2 10 00 e0 f2  e9 03 11 aa 29 21 00 91 
  00000250  30 01 00 f9 d0 03 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000260  10 00 e0 f2 e9 03 11 aa  29 41 00 91 30 01 00 f9 
  00000270  10 05 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000280  e9 03 11 aa 29 61 00 91  30 01 00 f9 f0 03 00 91 
  00000290  10 82 1b 91 f0 73 00 f9  f1 6b 40 f9 e9 03 11 aa 
  000002a0  30 01 40 f9 f0 e3 00 f9  e9 03 11 aa 29 21 00 91 
  000002b0  30 01 40 f9 f0 e7 00 f9  e9 03 11 aa 29 41 00 91 
  000002c0  30 01 40 f9 f0 eb 00 f9  e9 03 11 aa 29 61 00 91 
  000002d0  30 01 40 f9 f0 ef 00 f9  f0 03 00 91 10 02 07 91 
  000002e0  f0 77 00 f9 f1 73 40 f9  f0 e3 40 f9 e9 03 11 aa 
  000002f0  30 01 00 f9 f0 e7 40 f9  e9 03 11 aa 29 21 00 91 
  00000300  30 01 00 f9 f0 eb 40 f9  e9 03 11 aa 29 41 00 91 
  00000310  30 01 00 f9 f0 ef 40 f9  e9 03 11 aa 29 61 00 91 
  00000320  30 01 00 f9 f0 03 00 91  10 82 2b 91 f0 7f 00 f9 
  00000330  f1 7f 40 f9 f0 73 40 f9  30 02 00 f9 f0 03 00 91 
  00000340  10 82 2c 91 f0 87 00 f9  f1 87 40 f9 f0 73 40 f9 
  00000350  30 02 00 f9 f0 7f 40 f9  11 02 40 f9 f1 8f 00 f9 
  00000360  f0 87 40 f9 11 02 40 f9  f1 93 00 f9 e0 8f 40 f9 
  00000370  e1 93 40 f9 23 ff ff 97  e0 97 00 f9 01 00 00 14 
  00000380  f0 63 40 f9 f0 9b 00 f9  f0 9b 40 f9 11 02 40 f9 
  00000390  f1 9f 00 f9 00 00 00 90  00 00 00 91 00 60 00 91 
  000003a0  e1 9f 40 f9 f0 9f 40 f9  f0 03 00 f9 e2 97 40 f9 
  000003b0  f0 97 40 f9 f0 07 00 f9  00 00 00 94 bf 03 00 91 
  000003c0  f0 03 00 91 10 82 2d 91  1d 7a 40 a9 ff c3 2d 91 
  000003d0  00 00 80 d2 c0 03 5f d6 

.rodata (38 bytes):
  00000000  45 4c 46 20 50 49 45 20  72 6f 64 61 74 61 20 63 
  00000010  68 65 63 6b 00 00 00 00  25 73 3a 20 73 75 6d 3d 
  00000020  25 6c 6c 64 0a 00 
