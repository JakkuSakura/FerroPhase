fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
fn strlen
fn main
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    alloca Virtual { id: 2, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 0, bank: General, size_bits: 64 }
    load Virtual { id: 4, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(strlen)(v4) cc=C tail=false
    br
  bb1 bb1
    bitcast Virtual { id: 6, bank: General, size_bits: 64 }, Virtual { id: 0, bank: General, size_bits: 64 }
    load Virtual { id: 7, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 6, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 7, bank: General, size_bits: 64 }, Virtual { id: 5, bank: General, size_bits: 64 }
    ret


Symbols:
  main                             0x00000000

Text relocations:
  offset=0x0000001c kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000070 kind=CallRel32 symbol=strlen addend=0
  offset=0x00000090 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000b4 kind=CallRel32 symbol=printf addend=0

.text (204 bytes):
  00000000  ff c3 02 d1 fd 7b 0a a9  fd 03 00 91 f0 03 00 91 
  00000010  10 22 02 91 f0 0b 00 f9  f1 0b 40 f9 10 00 00 90 
  00000020  10 02 00 91 e9 03 11 aa  30 01 00 f9 d0 01 80 d2 
  00000030  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 e9 03 11 aa 
  00000040  29 21 00 91 30 01 00 f9  f0 03 00 91 10 62 02 91 
  00000050  f0 13 00 f9 f1 13 40 f9  f0 0b 40 f9 30 02 00 f9 
  00000060  f0 13 40 f9 11 02 40 f9  f1 1b 00 f9 e0 1b 40 f9 
  00000070  00 00 00 94 e0 1f 00 f9  01 00 00 14 f0 0b 40 f9 
  00000080  f0 23 00 f9 f0 23 40 f9  11 02 40 f9 f1 27 00 f9 
  00000090  00 00 00 90 00 00 00 91  00 40 00 91 e1 27 40 f9 
  000000a0  f0 27 40 f9 f0 03 00 f9  e2 1f 40 f9 f0 1f 40 f9 
  000000b0  f0 07 00 f9 00 00 00 94  bf 03 00 91 fd 7b 4a a9 
  000000c0  ff c3 02 91 00 00 80 d2  c0 03 5f d6 

.rodata (37 bytes):
  00000000  68 65 6c 6c 6f 20 66 72  6f 6d 20 66 66 69 00 00 
  00000010  73 74 72 6c 65 6e 28 27  25 73 27 29 20 3d 20 25 
  00000020  6c 6c 64 0a 00 
