fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data__10_print_showcase_main_g0_0 ty=Array(I8, 12) constant=true initializer=Some(Bytes([108, 105, 110, 101, 49, 10, 108, 105, 110, 101, 50, 0]))
global __const_data__10_print_showcase_main_g0_1 ty=Array(I8, 8) constant=true initializer=Some(Bytes([116, 97, 98, 9, 101, 110, 100, 0]))
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.println)
    alloca Virtual { id: 15, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 15, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 7
    load Virtual { id: 17, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 15, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 17, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println), 2, 3, 5
    intrinsic.call symbol(intrinsic.println), 4614256650576692846
    intrinsic.call symbol(intrinsic.println), 97, 90
    intrinsic.call symbol(intrinsic.println), 1, 2
    intrinsic.call symbol(intrinsic.println), 1, 0
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println), symbol(__const_data__10_print_showcase_main_g0_0), symbol(__const_data__10_print_showcase_main_g0_1)
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
  offset=0x00000068 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000074 kind=CallRel32 symbol=printf addend=0
  offset=0x00000078 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000084 kind=CallRel32 symbol=printf addend=0
  offset=0x00000088 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000094 kind=CallRel32 symbol=printf addend=0
  offset=0x00000098 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000a4 kind=CallRel32 symbol=printf addend=0
  offset=0x000000a8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000b4 kind=CallRel32 symbol=printf addend=0
  offset=0x000000b8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000c4 kind=CallRel32 symbol=printf addend=0
  offset=0x000000c8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000d4 kind=CallRel32 symbol=printf addend=0
  offset=0x000000d8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000e4 kind=CallRel32 symbol=printf addend=0
  offset=0x000000e8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000f4 kind=CallRel32 symbol=printf addend=0
  offset=0x0000011c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000134 kind=CallRel32 symbol=printf addend=0
  offset=0x00000138 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000168 kind=CallRel32 symbol=printf addend=0
  offset=0x0000016c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001a4 kind=CallRel32 symbol=printf addend=0
  offset=0x000001a8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001cc kind=CallRel32 symbol=printf addend=0
  offset=0x000001d0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001f4 kind=CallRel32 symbol=printf addend=0
  offset=0x000001f8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000021c kind=CallRel32 symbol=printf addend=0
  offset=0x00000220 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000022c kind=CallRel32 symbol=printf addend=0
  offset=0x00000230 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000023c kind=CallRel32 symbol=printf addend=0
  offset=0x00000240 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000024c kind=CallRel32 symbol=printf addend=0
  offset=0x00000250 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000025c kind=CallRel32 symbol=printf addend=0
  offset=0x00000260 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000026c kind=CallRel32 symbol=printf addend=0
  offset=0x00000270 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000027c kind=CallRel32 symbol=printf addend=0
  offset=0x00000280 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000028c kind=CallRel32 symbol=printf addend=0
  offset=0x00000290 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000029c kind=CallRel32 symbol=printf addend=0
  offset=0x000002a0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000002ac kind=Aarch64AdrpAdd symbol=__const_data__10_print_showcase_main_g0_0 addend=0
  offset=0x000002b4 kind=Aarch64AdrpAdd symbol=__const_data__10_print_showcase_main_g0_0 addend=0
  offset=0x000002c0 kind=Aarch64AdrpAdd symbol=__const_data__10_print_showcase_main_g0_1 addend=0
  offset=0x000002c8 kind=Aarch64AdrpAdd symbol=__const_data__10_print_showcase_main_g0_1 addend=0
  offset=0x000002d4 kind=CallRel32 symbol=printf addend=0

.text (756 bytes):
  00000000  ff 03 0a d1 f0 03 00 91  10 c2 09 91 1d 7a 00 a9 
  00000010  fd 03 00 91 1f 20 03 d5  00 00 00 90 00 00 00 91 
  00000020  00 60 00 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000030  00 00 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000040  00 a0 02 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000050  00 60 03 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000060  00 00 04 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000070  00 20 04 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000080  00 40 04 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000090  00 00 04 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000000a0  00 a0 04 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000000b0  00 c0 04 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000000c0  00 00 05 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000000d0  00 00 04 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000000e0  00 20 05 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000000f0  00 00 04 91 00 00 00 94  f0 03 00 91 10 c2 08 91 
  00000100  f0 4b 00 f9 f1 4b 40 f9  f0 00 80 d2 30 02 00 f9 
  00000110  f0 4b 40 f9 11 02 40 f9  f1 53 00 f9 00 00 00 90 
  00000120  00 00 00 91 00 60 05 91  e1 53 40 f9 f0 53 40 f9 
  00000130  f0 03 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  00000140  00 a0 05 91 41 00 80 d2  50 00 80 d2 f0 03 00 f9 
  00000150  62 00 80 d2 70 00 80 d2  f0 07 00 f9 a3 00 80 d2 
  00000160  b0 00 80 d2 f0 0b 00 f9  00 00 00 94 00 00 00 90 
  00000170  00 00 00 91 00 20 06 91  d0 cd 90 d2 70 03 be f2 
  00000180  30 3f c4 f2 30 01 e8 f2  00 02 67 9e d0 cd 90 d2 
  00000190  70 03 be f2 30 3f c4 f2  30 01 e8 f2 00 02 67 9e 
  000001a0  e0 03 00 fd 00 00 00 94  00 00 00 90 00 00 00 91 
  000001b0  00 60 06 91 21 0c 80 d2  30 0c 80 d2 f0 03 00 f9 
  000001c0  42 0b 80 d2 50 0b 80 d2  f0 07 00 f9 00 00 00 94 
  000001d0  00 00 00 90 00 00 00 91  00 a0 06 91 21 00 80 d2 
  000001e0  30 00 80 d2 f0 03 00 f9  42 00 80 d2 50 00 80 d2 
  000001f0  f0 07 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  00000200  00 00 07 91 21 00 80 d2  30 00 80 d2 f0 03 00 f9 
  00000210  02 00 80 d2 10 00 80 d2  f0 07 00 f9 00 00 00 94 
  00000220  00 00 00 90 00 00 00 91  00 40 07 91 00 00 00 94 
  00000230  00 00 00 90 00 00 00 91  00 00 04 91 00 00 00 94 
  00000240  00 00 00 90 00 00 00 91  00 60 07 91 00 00 00 94 
  00000250  00 00 00 90 00 00 00 91  00 e0 07 91 00 00 00 94 
  00000260  00 00 00 90 00 00 00 91  00 00 04 91 00 00 00 94 
  00000270  00 00 00 90 00 00 00 91  00 40 08 91 00 00 00 94 
  00000280  00 00 00 90 00 00 00 91  00 60 08 91 00 00 00 94 
  00000290  00 00 00 90 00 00 00 91  00 00 04 91 00 00 00 94 
  000002a0  00 00 00 90 00 00 00 91  00 80 08 91 01 00 00 90 
  000002b0  21 00 00 91 10 00 00 90  10 02 00 91 f0 03 00 f9 
  000002c0  02 00 00 90 42 00 00 91  10 00 00 90 10 02 00 91 
  000002d0  f0 07 00 f9 00 00 00 94  bf 03 00 91 f0 03 00 91 
  000002e0  10 c2 09 91 1d 7a 40 a9  ff 03 0a 91 00 00 80 d2 
  000002f0  c0 03 5f d6 

.rodata (560 bytes):
  00000000  6c 69 6e 65 31 0a 6c 69  6e 65 32 00 74 61 62 09 
  00000010  65 6e 64 00 00 00 00 00  f0 9f 93 98 20 54 75 74 
  00000020  6f 72 69 61 6c 3a 20 31  30 5f 70 72 69 6e 74 5f 
  00000030  73 68 6f 77 63 61 73 65  2e 66 70 0a 00 00 00 00 
  00000040  f0 9f a7 ad 20 46 6f 63  75 73 3a 20 43 6f 6d 70 
  00000050  72 65 68 65 6e 73 69 76  65 20 70 72 69 6e 74 6c 
  00000060  6e 21 2f 70 72 69 6e 74  20 73 68 6f 77 63 61 73 
  00000070  65 20 63 6f 76 65 72 69  6e 67 20 76 61 72 69 61 
  00000080  64 69 63 20 61 72 67 75  6d 65 6e 74 73 20 61 6e 
  00000090  64 20 72 75 6e 74 69 6d  65 20 66 6f 72 6d 61 74 
  000000a0  74 69 6e 67 0a 00 00 00  f0 9f a7 aa 20 57 68 61 
  000000b0  74 20 74 6f 20 6c 6f 6f  6b 20 66 6f 72 3a 20 6c 
  000000c0  61 62 65 6c 65 64 20 6f  75 74 70 75 74 73 20 62 
  000000d0  65 6c 6f 77 0a 00 00 00  e2 9c 85 20 45 78 70 65 
  000000e0  63 74 61 74 69 6f 6e 3a  20 6f 75 74 70 75 74 73 
  000000f0  20 6d 61 74 63 68 20 6c  61 62 65 6c 73 0a 00 00 
  00000100  0a 00 00 00 00 00 00 00  48 65 6c 6c 6f 00 00 00 
  00000110  57 6f 72 6c 64 20 77 69  74 68 20 6e 65 77 6c 69 
  00000120  6e 65 73 00 00 00 00 00  4e 75 6d 62 65 72 3a 00 
  00000130  42 6f 6f 6c 65 61 6e 3a  00 00 00 00 00 00 00 00 
  00000140  4d 69 78 65 64 3a 00 00  4e 61 6d 65 73 70 61 63 
  00000150  65 20 74 65 73 74 00 00  76 61 6c 75 65 20 3d 20 
  00000160  25 6c 6c 64 0a 00 00 00  6d 61 74 68 3a 20 25 6c 
  00000170  6c 64 20 2b 20 25 6c 6c  64 20 3d 20 25 6c 6c 64 
  00000180  0a 00 00 00 00 00 00 00  66 6c 6f 61 74 3a 20 25 
  00000190  66 0a 00 00 00 00 00 00  63 68 61 72 73 3a 20 25 
  000001a0  64 20 25 64 0a 00 00 00  74 75 70 6c 65 3a 20 28 
  000001b0  25 6c 6c 64 2c 20 25 6c  6c 64 29 0a 00 00 00 00 
  000001c0  62 6f 6f 6c 73 3a 20 25  64 20 25 64 0a 00 00 00 
  000001d0  54 68 69 73 00 00 00 00  43 6f 6e 74 69 6e 75 69 
  000001e0  6e 67 20 77 69 74 68 6f  75 74 20 6e 65 77 6c 69 
  000001f0  6e 65 00 00 00 00 00 00  20 2d 20 61 70 70 65 6e 
  00000200  64 65 64 20 63 6f 6e 74  65 6e 74 00 00 00 00 00 
  00000210  55 6e 69 74 3a 00 00 00  4e 75 6c 6c 3a 00 00 00 
  00000220  65 73 63 61 70 65 64 3a  20 25 73 20 25 73 0a 00 
