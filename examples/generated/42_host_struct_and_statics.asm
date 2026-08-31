fp-native dump: format=MachO arch=Aarch64 entry=0x80

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global HOST_HANDLE ty=Struct { fields: [I64], packed: false, name: None } constant=true initializer=None
global HOST_STATE ty=Struct { fields: [I64], packed: false, name: None } constant=false initializer=None
fn read_host_handle
  bb0 bb0
    alloca Virtual { id: 1, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 2, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    bitcast Virtual { id: 4, bank: General, size_bits: 64 }, Virtual { id: 2, bank: General, size_bits: 64 }
    load Virtual { id: 5, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 4, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 5, bank: General, size_bits: 64 }
    load Virtual { id: 7, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn main
  bb0 bb0
    call symbol(read_host_handle)() cc=C tail=false
    br
  bb1 bb1
    intrinsic.call symbol(intrinsic.println), Virtual { id: 8, bank: General, size_bits: 64 }
    alloca Virtual { id: 10, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 10, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    bitcast Virtual { id: 12, bank: General, size_bits: 64 }, Virtual { id: 10, bank: General, size_bits: 64 }
    load Virtual { id: 13, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 12, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 13, bank: General, size_bits: 64 }
    ret


Symbols:
  read_host_handle                 0x00000000
  main                             0x00000080

Text relocations:
  offset=0x0000009c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000b0 kind=CallRel32 symbol=printf addend=0
  offset=0x000000ec kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000104 kind=CallRel32 symbol=printf addend=0

.text (284 bytes):
  00000000  ff 03 04 d1 fd 7b 0f a9  fd 03 00 91 1f 20 03 d5 
  00000010  f0 03 00 91 10 a2 01 91  f0 03 00 f9 f0 03 00 91 
  00000020  10 a2 02 91 f0 07 00 f9  10 00 80 d2 10 00 a0 f2 
  00000030  10 00 c0 f2 10 00 e0 f2  f1 07 40 f9 30 02 00 f9 
  00000040  f0 07 40 f9 f0 0f 00 f9  f0 0f 40 f9 11 02 40 f9 
  00000050  f1 13 00 f9 f1 03 40 f9  f0 13 40 f9 30 02 00 f9 
  00000060  f0 03 40 f9 11 02 40 f9  f1 1b 00 f9 e0 1b 40 f9 
  00000070  bf 03 00 91 fd 7b 4f a9  ff 03 04 91 c0 03 5f d6 
  00000080  ff 03 04 d1 fd 7b 0f a9  fd 03 00 91 1f 20 03 d5 
  00000090  dc ff ff 97 e0 1f 00 f9  01 00 00 14 00 00 00 90 
  000000a0  00 00 00 91 e1 1f 40 f9  f0 1f 40 f9 f0 03 00 f9 
  000000b0  00 00 00 94 f0 03 00 91  10 a2 02 91 f0 27 00 f9 
  000000c0  10 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  000000d0  f1 27 40 f9 30 02 00 f9  f0 27 40 f9 f0 2f 00 f9 
  000000e0  f0 2f 40 f9 11 02 40 f9  f1 33 00 f9 00 00 00 90 
  000000f0  00 00 00 91 00 60 00 91  e1 33 40 f9 f0 33 40 f9 
  00000100  f0 03 00 f9 00 00 00 94  bf 03 00 91 fd 7b 4f a9 
  00000110  ff 03 04 91 00 00 80 d2  c0 03 5f d6 

.rodata (47 bytes):
  00000000  68 6f 73 74 20 68 61 6e  64 6c 65 20 72 61 77 20 
  00000010  3d 20 25 6c 6c 75 0a 00  68 6f 73 74 20 73 74 61 
  00000020  74 65 20 72 61 77 20 3d  20 25 6c 6c 75 0a 00 
