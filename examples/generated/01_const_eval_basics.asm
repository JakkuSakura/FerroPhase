fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data__01_const_eval_basics___fp_comptime_const___const_block_01_const_eval_basics_9_3964631407444695604_g1_0 ty=Array(I8, 6) constant=true initializer=Some(Bytes([108, 97, 114, 103, 101, 0]))
global __const_data__01_const_eval_basics___fp_comptime_const___const_block_01_const_eval_basics_9_3964631407444695604_g1_1 ty=Array(I8, 6) constant=true initializer=Some(Bytes([115, 109, 97, 108, 108, 0]))
global __const_data__01_const_eval_basics___fp_comptime_const___const_block_01_const_eval_basics_9_3964631407444695604_g3_2 ty=Array(I8, 6) constant=true initializer=Some(Bytes([108, 97, 114, 103, 101, 0]))
global __const_data__01_const_eval_basics___fp_comptime_const___const_block_01_const_eval_basics_9_3964631407444695604_g3_3 ty=Array(I8, 6) constant=true initializer=Some(Bytes([115, 109, 97, 108, 108, 0]))
global BUFFER_SIZE ty=I64 constant=true initializer=Some(Bytes([0, 16, 0, 0, 0, 0, 0, 0]))
global FACTORIAL_5 ty=I64 constant=true initializer=Some(Bytes([120, 0, 0, 0, 0, 0, 0, 0]))
global IS_LARGE ty=I1 constant=true initializer=Some(Bytes([1]))
global DEFAULT_CONFIG ty=Struct { fields: [I64, I64], packed: false, name: None } constant=true initializer=Some(Bytes([0, 16, 0, 0, 0, 0, 0, 0, 150, 0, 0, 0, 0, 0, 0, 0]))
global MAX_CONNECTIONS ty=I64 constant=true initializer=Some(Bytes([150, 0, 0, 0, 0, 0, 0, 0]))
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    alloca Virtual { id: 36, bank: General, size_bits: 64 }, 8
    div Virtual { id: 37, bank: General, size_bits: 64 }, 4096, 1024
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 37, bank: General, size_bits: 64 }
    load Virtual { id: 39, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 39, bank: General, size_bits: 64 }, 120, 1
    alloca Virtual { id: 41, bank: General, size_bits: 64 }, 8
    div Virtual { id: 42, bank: General, size_bits: 64 }, 4096, 1024
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 42, bank: General, size_bits: 64 }
    load Virtual { id: 44, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 44, bank: General, size_bits: 64 }, 150
    alloca Virtual { id: 46, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 46, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 3
    alloca Virtual { id: 48, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 48, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 8192
    alloca Virtual { id: 50, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 50, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    alloca Virtual { id: 52, bank: General, size_bits: 64 }, 8
    load Virtual { id: 53, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 46, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 54, bank: General, size_bits: 64 }, Virtual { id: 53, bank: General, size_bits: 64 }, 614400
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 52, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 54, bank: General, size_bits: 64 }
    alloca Virtual { id: 56, bank: General, size_bits: 64 }, 8
    load Virtual { id: 57, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 52, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 56, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 57, bank: General, size_bits: 64 }
    load Virtual { id: 59, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 48, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 60, bank: General, size_bits: 64 }, Virtual { id: 50, bank: General, size_bits: 64 }
    load Virtual { id: 61, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 60, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 62, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 56, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 59, bank: General, size_bits: 64 }, Virtual { id: 61, bank: General, size_bits: 64 }, Virtual { id: 62, bank: General, size_bits: 64 }
    ret


Symbols:
  main                             0x00000000

Text relocations:
  offset=0x00000018 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000024 kind=CallRel32 symbol=printf addend=0
  offset=0x00000028 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000034 kind=CallRel32 symbol=printf addend=0
  offset=0x00000038 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000044 kind=CallRel32 symbol=printf addend=0
  offset=0x00000048 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000054 kind=CallRel32 symbol=printf addend=0
  offset=0x00000058 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000064 kind=CallRel32 symbol=printf addend=0
  offset=0x000000a0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000d0 kind=CallRel32 symbol=printf addend=0
  offset=0x0000010c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000130 kind=CallRel32 symbol=printf addend=0
  offset=0x00000178 kind=Aarch64AdrpAdd symbol=__const_data__01_const_eval_basics___fp_comptime_const___const_block_01_const_eval_basics_9_3964631407444695604_g3_2 addend=0
  offset=0x00000234 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000264 kind=CallRel32 symbol=printf addend=0

.text (644 bytes):
  00000000  ff 43 11 d1 f0 03 00 91  10 02 11 91 1d 7a 00 a9 
  00000010  fd 03 00 91 1f 20 03 d5  00 00 00 90 00 00 00 91 
  00000020  00 20 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000030  00 c0 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000040  00 20 03 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000050  00 e0 03 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000060  00 80 04 91 00 00 00 94  f0 03 00 91 10 02 07 91 
  00000070  f0 27 00 f9 10 00 82 d2  11 80 80 d2 09 0e d1 9a 
  00000080  f0 03 09 aa f0 2b 00 f9  f1 27 40 f9 f0 2b 40 f9 
  00000090  30 02 00 f9 f0 27 40 f9  11 02 40 f9 f1 33 00 f9 
  000000a0  00 00 00 90 00 00 00 91  00 a0 04 91 e1 33 40 f9 
  000000b0  f0 33 40 f9 f0 03 00 f9  02 0f 80 d2 10 0f 80 d2 
  000000c0  f0 07 00 f9 23 00 80 d2  30 00 80 d2 f0 0b 00 f9 
  000000d0  00 00 00 94 f0 03 00 91  10 02 08 91 f0 3b 00 f9 
  000000e0  10 00 82 d2 11 80 80 d2  09 0e d1 9a f0 03 09 aa 
  000000f0  f0 3f 00 f9 f1 3b 40 f9  f0 3f 40 f9 30 02 00 f9 
  00000100  f0 3b 40 f9 11 02 40 f9  f1 47 00 f9 00 00 00 90 
  00000110  00 00 00 91 00 60 05 91  e1 47 40 f9 f0 47 40 f9 
  00000120  f0 03 00 f9 c2 12 80 d2  d0 12 80 d2 f0 07 00 f9 
  00000130  00 00 00 94 f0 03 00 91  10 02 09 91 f0 4f 00 f9 
  00000140  f1 4f 40 f9 70 00 80 d2  30 02 00 f9 f0 03 00 91 
  00000150  10 02 0a 91 f0 57 00 f9  f1 57 40 f9 10 00 84 d2 
  00000160  30 02 00 f9 f0 03 00 91  10 02 0b 91 f0 5f 00 f9 
  00000170  f1 5f 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  00000180  ea 03 0b aa 50 01 00 f9  b0 00 80 d2 10 00 a0 f2 
  00000190  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  000001a0  50 01 00 f9 f0 03 00 91  10 02 0f 91 f0 67 00 f9 
  000001b0  f0 4f 40 f9 11 02 40 f9  f1 6b 00 f9 f0 6b 40 f9 
  000001c0  11 00 8c d2 31 01 a0 f2  11 00 c0 f2 11 00 e0 f2 
  000001d0  10 7e 11 9b f0 6f 00 f9  f1 67 40 f9 f0 6f 40 f9 
  000001e0  30 02 00 f9 f0 03 00 91  10 02 10 91 f0 77 00 f9 
  000001f0  f0 67 40 f9 11 02 40 f9  f1 7b 00 f9 f1 77 40 f9 
  00000200  f0 7b 40 f9 30 02 00 f9  f0 57 40 f9 11 02 40 f9 
  00000210  f1 83 00 f9 f0 5f 40 f9  f0 87 00 f9 f0 87 40 f9 
  00000220  11 02 40 f9 f1 8b 00 f9  f0 77 40 f9 11 02 40 f9 
  00000230  f1 8f 00 f9 00 00 00 90  00 00 00 91 00 20 06 91 
  00000240  e1 83 40 f9 f0 83 40 f9  f0 03 00 f9 e2 8b 40 f9 
  00000250  f0 8b 40 f9 f0 07 00 f9  e3 8f 40 f9 f0 8f 40 f9 
  00000260  f0 0b 00 f9 00 00 00 94  bf 03 00 91 f0 03 00 91 
  00000270  10 02 11 91 1d 7a 40 a9  ff 43 11 91 00 00 80 d2 
  00000280  c0 03 5f d6 

.rodata (443 bytes):
  00000000  6c 61 72 67 65 00 73 6d  61 6c 6c 00 6c 61 72 67 
  00000010  65 00 73 6d 61 6c 6c 00  00 10 00 00 00 00 00 00 
  00000020  78 00 00 00 00 00 00 00  01 00 00 00 00 00 00 00 
  00000030  00 10 00 00 00 00 00 00  96 00 00 00 00 00 00 00 
  00000040  96 00 00 00 00 00 00 00  f0 9f 93 98 20 54 75 74 
  00000050  6f 72 69 61 6c 3a 20 30  31 5f 63 6f 6e 73 74 5f 
  00000060  65 76 61 6c 5f 62 61 73  69 63 73 2e 66 70 0a 00 
  00000070  f0 9f a7 ad 20 46 6f 63  75 73 3a 20 42 61 73 69 
  00000080  63 20 63 6f 6e 73 74 20  65 76 61 6c 75 61 74 69 
  00000090  6f 6e 20 77 69 74 68 20  63 6f 6d 70 69 6c 65 2d 
  000000a0  74 69 6d 65 20 61 72 69  74 68 6d 65 74 69 63 20 
  000000b0  61 6e 64 20 63 6f 6e 73  74 20 62 6c 6f 63 6b 73 
  000000c0  0a 00 00 00 00 00 00 00  f0 9f a7 aa 20 57 68 61 
  000000d0  74 20 74 6f 20 6c 6f 6f  6b 20 66 6f 72 3a 20 6c 
  000000e0  61 62 65 6c 65 64 20 6f  75 74 70 75 74 73 20 62 
  000000f0  65 6c 6f 77 0a 00 00 00  e2 9c 85 20 45 78 70 65 
  00000100  63 74 61 74 69 6f 6e 3a  20 6f 75 74 70 75 74 73 
  00000110  20 6d 61 74 63 68 20 6c  61 62 65 6c 73 0a 00 00 
  00000120  0a 00 00 00 00 00 00 00  42 75 66 66 65 72 3a 20 
  00000130  25 6c 6c 64 4b 42 2c 20  66 61 63 74 6f 72 69 61 
  00000140  6c 28 35 29 3d 25 6c 6c  64 2c 20 6c 61 72 67 65 
  00000150  3d 25 64 0a 00 00 00 00  43 6f 6e 66 69 67 3a 20 
  00000160  25 6c 6c 64 4b 42 20 62  75 66 66 65 72 2c 20 25 
  00000170  6c 6c 64 20 63 6f 6e 6e  65 63 74 69 6f 6e 73 0a 
  00000180  00 00 00 00 00 00 00 00  43 6f 6e 73 74 20 62 6c 
  00000190  6f 63 6b 73 3a 20 73 69  7a 65 3d 25 6c 6c 64 2c 
  000001a0  20 73 74 72 61 74 65 67  79 3d 25 73 2c 20 6d 65 
  000001b0  6d 6f 72 79 3d 25 6c 6c  64 0a 00 
