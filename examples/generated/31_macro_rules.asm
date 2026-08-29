fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
fn main
  bb0 bb0
    alloca Virtual { id: 1, bank: General, size_bits: 64 }, 8
    add Virtual { id: 2, bank: General, size_bits: 64 }, 10, 32
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 2, bank: General, size_bits: 64 }
    alloca Virtual { id: 4, bank: General, size_bits: 64 }, 8
    load Virtual { id: 5, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 4, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 5, bank: General, size_bits: 64 }
    call symbol(add_two)(5) cc=C tail=false
    br
  bb1 bb1
    load Virtual { id: 8, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 4, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 8, bank: General, size_bits: 64 }, Virtual { id: 7, bank: General, size_bits: 64 }
    ret
fn add_two
  bb0 bb0
    alloca Virtual { id: 10, bank: General, size_bits: 64 }, 8
    add Virtual { id: 11, bank: General, size_bits: 64 }, symbol(local.1), 2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 10, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 11, bank: General, size_bits: 64 }
    load Virtual { id: 13, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 10, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret


Symbols:
  main                             0x00000000
  add_two                          0x000000ac

Text relocations:
  offset=0x00000074 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000094 kind=CallRel32 symbol=printf addend=0

.text (260 bytes):
  00000000  ff c3 04 d1 fd 7b 12 a9  fd 03 00 91 1f 20 03 d5 
  00000010  f0 03 00 91 10 62 02 91  f0 0b 00 f9 50 01 80 d2 
  00000020  10 82 00 91 f0 0f 00 f9  f1 0b 40 f9 f0 0f 40 f9 
  00000030  30 02 00 f9 f0 03 00 91  10 62 03 91 f0 17 00 f9 
  00000040  f0 0b 40 f9 11 02 40 f9  f1 1b 00 f9 f1 17 40 f9 
  00000050  f0 1b 40 f9 30 02 00 f9  a0 00 80 d2 14 00 00 94 
  00000060  e0 23 00 f9 01 00 00 14  f0 17 40 f9 11 02 40 f9 
  00000070  f1 27 00 f9 00 00 00 90  00 00 00 91 e1 27 40 f9 
  00000080  f0 27 40 f9 f0 03 00 f9  e2 23 40 f9 f0 23 40 f9 
  00000090  f0 07 00 f9 00 00 00 94  bf 03 00 91 fd 7b 52 a9 
  000000a0  ff c3 04 91 00 00 80 d2  c0 03 5f d6 ff c3 02 d1 
  000000b0  fd 7b 0a a9 fd 03 00 91  e0 2f 00 f9 1f 20 03 d5 
  000000c0  f0 03 00 91 10 82 01 91  f0 1b 00 f9 f0 2f 40 f9 
  000000d0  10 0a 00 91 f0 1f 00 f9  f1 1b 40 f9 f0 1f 40 f9 
  000000e0  30 02 00 f9 f0 1b 40 f9  11 02 40 f9 f1 27 00 f9 
  000000f0  e0 27 40 f9 bf 03 00 91  fd 7b 4a a9 ff c3 02 91 
  00000100  c0 03 5f d6 

.rodata (31 bytes):
  00000000  73 75 6d 20 3d 20 25 6c  6c 64 2c 20 61 64 64 5f 
  00000010  74 77 6f 28 35 29 20 3d  20 25 6c 6c 64 0a 00 
