fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    alloca Virtual { id: 5, bank: General, size_bits: 64 }, 1
    div Virtual { id: 6, bank: General, size_bits: 64 }, 4096, 1024
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 6, bank: General, size_bits: 64 }
    load Virtual { id: 8, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 8, bank: General, size_bits: 64 }, 120, 1
    alloca Virtual { id: 10, bank: General, size_bits: 64 }, 1
    div Virtual { id: 11, bank: General, size_bits: 64 }, 0, 1024
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 10, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 11, bank: General, size_bits: 64 }
    load Virtual { id: 13, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 10, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 13, bank: General, size_bits: 64 }, symbol(__const_data_0)
    alloca Virtual { id: 15, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 15, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 3
    alloca Virtual { id: 17, bank: General, size_bits: 64 }, 1
    load Virtual { id: 18, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 15, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 19, bank: General, size_bits: 64 }, Virtual { id: 18, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 17, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 19, bank: General, size_bits: 64 }
    alloca Virtual { id: 21, bank: General, size_bits: 64 }, 1
    load Virtual { id: 22, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 17, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 22, bank: General, size_bits: 64 }
    load Virtual { id: 24, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), symbol(__const_data_0), symbol(__const_data_0), Virtual { id: 24, bank: General, size_bits: 64 }
    ret


Symbols:
  main                             0x00000000

Text relocations:
  offset=0x0000000c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000018 kind=CallRel32 symbol=printf addend=0
  offset=0x0000001c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000028 kind=CallRel32 symbol=printf addend=0
  offset=0x0000002c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000038 kind=CallRel32 symbol=printf addend=0
  offset=0x0000003c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000048 kind=CallRel32 symbol=printf addend=0
  offset=0x0000004c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000058 kind=CallRel32 symbol=printf addend=0
  offset=0x00000094 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000c4 kind=CallRel32 symbol=printf addend=0
  offset=0x00000100 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000118 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000120 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x0000012c kind=CallRel32 symbol=printf addend=0
  offset=0x000001ac kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001b8 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000001c0 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000001cc kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000001d4 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000001ec kind=CallRel32 symbol=printf addend=0

.text (516 bytes):
  00000000  ff 43 07 d1 fd 7b 1c a9  fd 03 00 91 00 00 00 90 
  00000010  00 00 00 91 00 e0 00 91  00 00 00 94 00 00 00 90 
  00000020  00 00 00 91 00 80 01 91  00 00 00 94 00 00 00 90 
  00000030  00 00 00 91 00 e0 02 91  00 00 00 94 00 00 00 90 
  00000040  00 00 00 91 00 a0 03 91  00 00 00 94 00 00 00 90 
  00000050  00 00 00 91 00 40 04 91  00 00 00 94 f0 03 00 91 
  00000060  10 62 06 91 f0 27 00 f9  10 00 82 d2 11 80 80 d2 
  00000070  09 0e d1 9a f0 03 09 aa  f0 2b 00 f9 f1 27 40 f9 
  00000080  f0 2b 40 f9 30 02 00 f9  f0 27 40 f9 11 02 40 f9 
  00000090  f1 33 00 f9 00 00 00 90  00 00 00 91 00 60 04 91 
  000000a0  e1 33 40 f9 f0 33 40 f9  f0 03 00 f9 02 0f 80 d2 
  000000b0  10 0f 80 d2 f0 07 00 f9  23 00 80 d2 30 00 80 d2 
  000000c0  f0 0b 00 f9 00 00 00 94  f0 03 00 91 10 82 06 91 
  000000d0  f0 3b 00 f9 10 00 80 d2  11 80 80 d2 09 0e d1 9a 
  000000e0  f0 03 09 aa f0 3f 00 f9  f1 3b 40 f9 f0 3f 40 f9 
  000000f0  30 02 00 f9 f0 3b 40 f9  11 02 40 f9 f1 47 00 f9 
  00000100  00 00 00 90 00 00 00 91  00 20 05 91 e1 47 40 f9 
  00000110  f0 47 40 f9 f0 03 00 f9  02 00 00 90 42 00 00 91 
  00000120  10 00 00 90 10 02 00 91  f0 07 00 f9 00 00 00 94 
  00000130  f0 03 00 91 10 a2 06 91  f0 4f 00 f9 f1 4f 40 f9 
  00000140  70 00 80 d2 30 02 00 f9  f0 03 00 91 10 c2 06 91 
  00000150  f0 57 00 f9 f0 4f 40 f9  11 02 40 f9 f1 5b 00 f9 
  00000160  f0 5b 40 f9 11 00 80 d2  10 7e 11 9b f0 5f 00 f9 
  00000170  f1 57 40 f9 f0 5f 40 f9  30 02 00 f9 f0 03 00 91 
  00000180  10 e2 06 91 f0 67 00 f9  f0 57 40 f9 11 02 40 f9 
  00000190  f1 6b 00 f9 f1 67 40 f9  f0 6b 40 f9 30 02 00 f9 
  000001a0  f0 67 40 f9 11 02 40 f9  f1 73 00 f9 00 00 00 90 
  000001b0  00 00 00 91 00 c0 05 91  01 00 00 90 21 00 00 91 
  000001c0  10 00 00 90 10 02 00 91  f0 03 00 f9 02 00 00 90 
  000001d0  42 00 00 91 10 00 00 90  10 02 00 91 f0 07 00 f9 
  000001e0  e3 73 40 f9 f0 73 40 f9  f0 0b 00 f9 00 00 00 94 
  000001f0  bf 03 00 91 fd 7b 5c a9  ff 43 07 91 00 00 80 d2 
  00000200  c0 03 5f d6 

.rodata (417 bytes):
  00000000  28 29 00 00 00 00 00 00  00 10 00 00 00 00 00 00 
  00000010  96 00 00 00 00 00 00 00  78 00 00 00 00 00 00 00 
  00000020  01 00 00 00 00 00 00 00  00 10 00 00 00 00 00 00 
  00000030  96 00 00 00 00 00 00 00  f0 9f 93 98 20 54 75 74 
  00000040  6f 72 69 61 6c 3a 20 30  31 5f 63 6f 6e 73 74 5f 
  00000050  65 76 61 6c 5f 62 61 73  69 63 73 2e 66 70 0a 00 
  00000060  f0 9f a7 ad 20 46 6f 63  75 73 3a 20 42 61 73 69 
  00000070  63 20 63 6f 6e 73 74 20  65 76 61 6c 75 61 74 69 
  00000080  6f 6e 20 77 69 74 68 20  63 6f 6d 70 69 6c 65 2d 
  00000090  74 69 6d 65 20 61 72 69  74 68 6d 65 74 69 63 20 
  000000a0  61 6e 64 20 63 6f 6e 73  74 20 62 6c 6f 63 6b 73 
  000000b0  0a 00 00 00 00 00 00 00  f0 9f a7 aa 20 57 68 61 
  000000c0  74 20 74 6f 20 6c 6f 6f  6b 20 66 6f 72 3a 20 6c 
  000000d0  61 62 65 6c 65 64 20 6f  75 74 70 75 74 73 20 62 
  000000e0  65 6c 6f 77 0a 00 00 00  e2 9c 85 20 45 78 70 65 
  000000f0  63 74 61 74 69 6f 6e 3a  20 6f 75 74 70 75 74 73 
  00000100  20 6d 61 74 63 68 20 6c  61 62 65 6c 73 0a 00 00 
  00000110  0a 00 00 00 00 00 00 00  42 75 66 66 65 72 3a 20 
  00000120  25 6c 6c 64 4b 42 2c 20  66 61 63 74 6f 72 69 61 
  00000130  6c 28 35 29 3d 25 6c 6c  64 2c 20 6c 61 72 67 65 
  00000140  3d 25 64 0a 00 00 00 00  43 6f 6e 66 69 67 3a 20 
  00000150  25 6c 6c 64 4b 42 20 62  75 66 66 65 72 2c 20 25 
  00000160  73 20 63 6f 6e 6e 65 63  74 69 6f 6e 73 0a 00 00 
  00000170  43 6f 6e 73 74 20 62 6c  6f 63 6b 73 3a 20 73 69 
  00000180  7a 65 3d 25 73 2c 20 73  74 72 61 74 65 67 79 3d 
  00000190  25 73 2c 20 6d 65 6d 6f  72 79 3d 25 6c 6c 64 0a 
  000001a0  00 
