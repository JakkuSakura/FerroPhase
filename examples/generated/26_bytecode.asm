fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
fn main
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 40
    alloca Virtual { id: 2, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 2
    alloca Virtual { id: 4, bank: General, size_bits: 64 }, 8
    load Virtual { id: 5, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 6, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 7, bank: General, size_bits: 64 }, Virtual { id: 5, bank: General, size_bits: 64 }, Virtual { id: 6, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 4, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 7, bank: General, size_bits: 64 }
    load Virtual { id: 9, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 4, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 9, bank: General, size_bits: 64 }
    ret


Symbols:
  main                             0x00000000

Text relocations:
  offset=0x0000008c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000a0 kind=CallRel32 symbol=printf addend=0

.text (184 bytes):
  00000000  ff 83 05 d1 fd 7b 15 a9  fd 03 00 91 1f 20 03 d5 
  00000010  f0 03 00 91 10 42 02 91  f0 0b 00 f9 f1 0b 40 f9 
  00000020  10 05 80 d2 30 02 00 f9  f0 03 00 91 10 42 03 91 
  00000030  f0 13 00 f9 f1 13 40 f9  50 00 80 d2 30 02 00 f9 
  00000040  f0 03 00 91 10 42 04 91  f0 1b 00 f9 f0 0b 40 f9 
  00000050  11 02 40 f9 f1 1f 00 f9  f0 13 40 f9 11 02 40 f9 
  00000060  f1 23 00 f9 f0 1f 40 f9  f1 23 40 f9 10 02 11 8b 
  00000070  f0 27 00 f9 f1 1b 40 f9  f0 27 40 f9 30 02 00 f9 
  00000080  f0 1b 40 f9 11 02 40 f9  f1 2f 00 f9 00 00 00 90 
  00000090  00 00 00 91 e1 2f 40 f9  f0 2f 40 f9 f0 03 00 f9 
  000000a0  00 00 00 94 bf 03 00 91  fd 7b 55 a9 ff 83 05 91 
  000000b0  00 00 80 d2 c0 03 5f d6 

.rodata (24 bytes):
  00000000  42 79 74 65 63 6f 64 65  20 56 4d 20 73 61 79 73 
  00000010  3a 20 25 6c 6c 64 0a 00 
