fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data_0 ty=Array(I8, 15) constant=true initializer=Some(Bytes([104, 101, 108, 108, 111, 32, 102, 114, 111, 109, 32, 102, 102, 105, 0]))
fn strlen
fn main
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_0)
    alloca Virtual { id: 2, bank: General, size_bits: 64 }, 8
    load Virtual { id: 3, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 3, bank: General, size_bits: 64 }
    load Virtual { id: 5, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 6, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(strlen)(v5, v6) cc=C tail=false
    br
  bb1 bb1
    intrinsic.call symbol(intrinsic.println), Virtual { id: 7, bank: General, size_bits: 64 }
    ret


Symbols:
  main                             0x00000000

Text relocations:
  offset=0x00000020 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000070 kind=CallRel32 symbol=strlen addend=0
  offset=0x0000007c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000094 kind=CallRel32 symbol=printf addend=0

.text (172 bytes):
  00000000  ff 43 04 d1 fd 7b 10 a9  fd 03 00 91 1f 20 03 d5 
  00000010  f0 03 00 91 10 02 02 91  f0 0b 00 f9 f1 0b 40 f9 
  00000020  10 00 00 90 10 02 00 91  30 02 00 f9 f0 03 00 91 
  00000030  10 02 03 91 f0 13 00 f9  f0 0b 40 f9 11 02 40 f9 
  00000040  f1 17 00 f9 f1 13 40 f9  f0 17 40 f9 30 02 00 f9 
  00000050  f0 13 40 f9 11 02 40 f9  f1 1f 00 f9 f0 0b 40 f9 
  00000060  11 02 40 f9 f1 23 00 f9  e0 1f 40 f9 e1 23 40 f9 
  00000070  00 00 00 94 e0 27 00 f9  01 00 00 14 00 00 00 90 
  00000080  00 00 00 91 00 40 00 91  e1 27 40 f9 f0 27 40 f9 
  00000090  f0 03 00 f9 00 00 00 94  bf 03 00 91 fd 7b 50 a9 
  000000a0  ff 43 04 91 00 00 80 d2  c0 03 5f d6 

.rodata (49 bytes):
  00000000  68 65 6c 6c 6f 20 66 72  6f 6d 20 66 66 69 00 00 
  00000010  73 74 72 6c 65 6e 28 27  68 65 6c 6c 6f 20 66 72 
  00000020  6f 6d 20 66 66 69 27 29  20 3d 20 25 6c 6c 64 0a 
  00000030  00 
