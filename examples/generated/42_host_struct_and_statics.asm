fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global HOST_HANDLE ty=Struct { fields: [I64], packed: false, name: None } constant=true initializer=None
global HOST_STATE ty=Struct { fields: [I64], packed: false, name: None } constant=false initializer=None
fn main
  bb0 bb0
    call symbol(42_host_struct_and_statics:4)() cc=C tail=false
    br
  bb1 bb1
    intrinsic.call symbol(intrinsic.println), Virtual { id: 1, bank: General, size_bits: 64 }
    alloca Virtual { id: 3, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    bitcast Virtual { id: 5, bank: General, size_bits: 64 }, Virtual { id: 3, bank: General, size_bits: 64 }
    load Virtual { id: 6, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 6, bank: General, size_bits: 64 }
    ret
fn read_host_handle
  bb0 bb0
    alloca Virtual { id: 8, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 9, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 9, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    bitcast Virtual { id: 11, bank: General, size_bits: 64 }, Virtual { id: 9, bank: General, size_bits: 64 }
    load Virtual { id: 12, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 11, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 8, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 12, bank: General, size_bits: 64 }
    load Virtual { id: 14, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 8, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret


Symbols:
  main                             0x00000000
  read_host_handle                 0x0000009c

Text relocations:
  offset=0x00000010 kind=CallRel32 symbol=42_host_struct_and_statics:4 addend=0
  offset=0x0000001c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000030 kind=CallRel32 symbol=printf addend=0
  offset=0x0000006c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000084 kind=CallRel32 symbol=printf addend=0

.text (284 bytes):
  00000000  ff 03 04 d1 fd 7b 0f a9  fd 03 00 91 1f 20 03 d5 
  00000010  00 00 00 94 e0 0b 00 f9  01 00 00 14 00 00 00 90 
  00000020  00 00 00 91 e1 0b 40 f9  f0 0b 40 f9 f0 03 00 f9 
  00000030  00 00 00 94 f0 03 00 91  10 a2 02 91 f0 13 00 f9 
  00000040  10 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000050  f1 13 40 f9 30 02 00 f9  f0 13 40 f9 f0 1b 00 f9 
  00000060  f0 1b 40 f9 11 02 40 f9  f1 1f 00 f9 00 00 00 90 
  00000070  00 00 00 91 00 60 00 91  e1 1f 40 f9 f0 1f 40 f9 
  00000080  f0 03 00 f9 00 00 00 94  bf 03 00 91 fd 7b 4f a9 
  00000090  ff 03 04 91 00 00 80 d2  c0 03 5f d6 ff 03 04 d1 
  000000a0  fd 7b 0f a9 fd 03 00 91  1f 20 03 d5 f0 03 00 91 
  000000b0  10 a2 01 91 f0 13 00 f9  f0 03 00 91 10 a2 02 91 
  000000c0  f0 17 00 f9 10 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  000000d0  10 00 e0 f2 f1 17 40 f9  30 02 00 f9 f0 17 40 f9 
  000000e0  f0 1f 00 f9 f0 1f 40 f9  11 02 40 f9 f1 23 00 f9 
  000000f0  f1 13 40 f9 f0 23 40 f9  30 02 00 f9 f0 13 40 f9 
  00000100  11 02 40 f9 f1 2b 00 f9  e0 2b 40 f9 bf 03 00 91 
  00000110  fd 7b 4f a9 ff 03 04 91  c0 03 5f d6 

.rodata (47 bytes):
  00000000  68 6f 73 74 20 68 61 6e  64 6c 65 20 72 61 77 20 
  00000010  3d 20 25 6c 6c 75 0a 00  68 6f 73 74 20 73 74 61 
  00000020  74 65 20 72 61 77 20 3d  20 25 6c 6c 75 0a 00 
