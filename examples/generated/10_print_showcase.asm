fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data_0 ty=Array(I8, 5) constant=true initializer=Some(Bytes([116, 101, 120, 116, 0]))
global __const_data_1 ty=Array(I8, 12) constant=true initializer=Some(Bytes([115, 116, 105, 108, 108, 32, 119, 111, 114, 107, 115, 0]))
global __const_data_2 ty=Array(I8, 6) constant=true initializer=Some(Bytes([115, 116, 97, 121, 115, 0]))
global __const_data_3 ty=Array(I8, 3) constant=true initializer=Some(Bytes([111, 110, 0]))
global __const_data_4 ty=Array(I8, 4) constant=true initializer=Some(Bytes([111, 110, 101, 0]))
global __const_data_5 ty=Array(I8, 5) constant=true initializer=Some(Bytes([108, 105, 110, 101, 0]))
global __const_data_6 ty=Array(I8, 3) constant=true initializer=Some(Bytes([40, 41, 0]))
global __const_data_7 ty=Array(I8, 5) constant=true initializer=Some(Bytes([110, 117, 108, 108, 0]))
global __const_data_8 ty=Array(I8, 12) constant=true initializer=Some(Bytes([108, 105, 110, 101, 49, 10, 108, 105, 110, 101, 50, 0]))
global __const_data_9 ty=Array(I8, 8) constant=true initializer=Some(Bytes([116, 97, 98, 9, 101, 110, 100, 0]))
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
    intrinsic.call symbol(intrinsic.print), 42
    intrinsic.call symbol(intrinsic.print), 1, 0
    intrinsic.call symbol(intrinsic.print), 1, 4612811918334230528, symbol(__const_data_0), 1
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.print), symbol(__const_data_1)
    intrinsic.call symbol(intrinsic.println)
    alloca Virtual { id: 14, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 14, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 7
    load Virtual { id: 16, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 14, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 16, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println), 2, 3, 5
    intrinsic.call symbol(intrinsic.println), 4614256650576692846
    intrinsic.call symbol(intrinsic.println), 97, 90
    intrinsic.call symbol(intrinsic.println), 1, 2
    intrinsic.call symbol(intrinsic.println), 1, 0
    intrinsic.call symbol(intrinsic.print), symbol(__const_data_2), symbol(__const_data_3), symbol(__const_data_4), symbol(__const_data_5)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.print)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.print), symbol(__const_data_6)
    intrinsic.call symbol(intrinsic.print), symbol(__const_data_7)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println), symbol(__const_data_8), symbol(__const_data_9)
    ret


Symbols:
  main                             0x00000000

Text relocations:
  offset=0x00000014 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000020 kind=CallRel32 symbol=printf addend=0
  offset=0x00000024 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000030 kind=CallRel32 symbol=printf addend=0
  offset=0x00000034 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000040 kind=CallRel32 symbol=printf addend=0
  offset=0x00000044 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000050 kind=CallRel32 symbol=printf addend=0
  offset=0x00000054 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000060 kind=CallRel32 symbol=printf addend=0
  offset=0x00000064 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000070 kind=CallRel32 symbol=printf addend=0
  offset=0x00000074 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000080 kind=CallRel32 symbol=printf addend=0
  offset=0x00000084 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000090 kind=CallRel32 symbol=printf addend=0
  offset=0x00000094 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000ac kind=CallRel32 symbol=printf addend=0
  offset=0x000000b0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000d4 kind=CallRel32 symbol=printf addend=0
  offset=0x000000d8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000011c kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000124 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x0000013c kind=CallRel32 symbol=printf addend=0
  offset=0x00000140 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000014c kind=CallRel32 symbol=printf addend=0
  offset=0x00000150 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000015c kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x00000164 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x00000170 kind=CallRel32 symbol=printf addend=0
  offset=0x00000174 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000180 kind=CallRel32 symbol=printf addend=0
  offset=0x000001a8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001c0 kind=CallRel32 symbol=printf addend=0
  offset=0x000001c4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001f4 kind=CallRel32 symbol=printf addend=0
  offset=0x000001f8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000230 kind=CallRel32 symbol=printf addend=0
  offset=0x00000234 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000258 kind=CallRel32 symbol=printf addend=0
  offset=0x0000025c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000280 kind=CallRel32 symbol=printf addend=0
  offset=0x00000284 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000002a8 kind=CallRel32 symbol=printf addend=0
  offset=0x000002ac kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000002b8 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x000002c0 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x000002cc kind=Aarch64AdrpAdd symbol=__const_data_3 addend=0
  offset=0x000002d4 kind=Aarch64AdrpAdd symbol=__const_data_3 addend=0
  offset=0x000002e0 kind=Aarch64AdrpAdd symbol=__const_data_4 addend=0
  offset=0x000002e8 kind=Aarch64AdrpAdd symbol=__const_data_4 addend=0
  offset=0x000002f4 kind=Aarch64AdrpAdd symbol=__const_data_5 addend=0
  offset=0x000002fc kind=Aarch64AdrpAdd symbol=__const_data_5 addend=0
  offset=0x00000308 kind=CallRel32 symbol=printf addend=0
  offset=0x0000030c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000318 kind=CallRel32 symbol=printf addend=0
  offset=0x0000031c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000328 kind=CallRel32 symbol=printf addend=0
  offset=0x0000032c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000338 kind=CallRel32 symbol=printf addend=0
  offset=0x0000033c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000348 kind=CallRel32 symbol=printf addend=0
  offset=0x0000034c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000358 kind=Aarch64AdrpAdd symbol=__const_data_6 addend=0
  offset=0x00000360 kind=Aarch64AdrpAdd symbol=__const_data_6 addend=0
  offset=0x0000036c kind=CallRel32 symbol=printf addend=0
  offset=0x00000370 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000037c kind=Aarch64AdrpAdd symbol=__const_data_7 addend=0
  offset=0x00000384 kind=Aarch64AdrpAdd symbol=__const_data_7 addend=0
  offset=0x00000390 kind=CallRel32 symbol=printf addend=0
  offset=0x00000394 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000003a0 kind=CallRel32 symbol=printf addend=0
  offset=0x000003a4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000003b0 kind=Aarch64AdrpAdd symbol=__const_data_8 addend=0
  offset=0x000003b8 kind=Aarch64AdrpAdd symbol=__const_data_8 addend=0
  offset=0x000003c4 kind=Aarch64AdrpAdd symbol=__const_data_9 addend=0
  offset=0x000003cc kind=Aarch64AdrpAdd symbol=__const_data_9 addend=0
  offset=0x000003d8 kind=CallRel32 symbol=printf addend=0

.text (1016 bytes):
  00000000  ff 03 09 d1 f0 03 00 91  10 c2 08 91 1d 7a 00 a9 
  00000010  fd 03 00 91 00 00 00 90  00 00 00 91 00 00 01 91 
  00000020  00 00 00 94 00 00 00 90  00 00 00 91 00 a0 01 91 
  00000030  00 00 00 94 00 00 00 90  00 00 00 91 00 40 03 91 
  00000040  00 00 00 94 00 00 00 90  00 00 00 91 00 00 04 91 
  00000050  00 00 00 94 00 00 00 90  00 00 00 91 00 a0 04 91 
  00000060  00 00 00 94 00 00 00 90  00 00 00 91 00 c0 04 91 
  00000070  00 00 00 94 00 00 00 90  00 00 00 91 00 e0 04 91 
  00000080  00 00 00 94 00 00 00 90  00 00 00 91 00 a0 04 91 
  00000090  00 00 00 94 00 00 00 90  00 00 00 91 00 40 05 91 
  000000a0  41 05 80 d2 50 05 80 d2  f0 03 00 f9 00 00 00 94 
  000000b0  00 00 00 90 00 00 00 91  00 80 05 91 21 00 80 d2 
  000000c0  30 00 80 d2 f0 03 00 f9  02 00 80 d2 10 00 80 d2 
  000000d0  f0 07 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  000000e0  00 c0 05 91 21 00 80 d2  30 00 80 d2 f0 03 00 f9 
  000000f0  10 00 80 d2 10 00 a0 f2  10 00 c0 f2 90 00 e8 f2 
  00000100  00 02 67 9e 10 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000110  90 00 e8 f2 00 02 67 9e  e0 07 00 fd 02 00 00 90 
  00000120  42 00 00 91 10 00 00 90  10 02 00 91 f0 0b 00 f9 
  00000130  23 00 80 d2 30 00 80 d2  f0 0f 00 f9 00 00 00 94 
  00000140  00 00 00 90 00 00 00 91  00 a0 04 91 00 00 00 94 
  00000150  00 00 00 90 00 00 00 91  00 20 06 91 01 00 00 90 
  00000160  21 00 00 91 10 00 00 90  10 02 00 91 f0 03 00 f9 
  00000170  00 00 00 94 00 00 00 90  00 00 00 91 00 a0 04 91 
  00000180  00 00 00 94 f0 03 00 91  10 82 08 91 f0 4b 00 f9 
  00000190  f1 4b 40 f9 f0 00 80 d2  30 02 00 f9 f0 4b 40 f9 
  000001a0  11 02 40 f9 f1 53 00 f9  00 00 00 90 00 00 00 91 
  000001b0  00 80 06 91 e1 53 40 f9  f0 53 40 f9 f0 03 00 f9 
  000001c0  00 00 00 94 00 00 00 90  00 00 00 91 00 c0 06 91 
  000001d0  41 00 80 d2 50 00 80 d2  f0 03 00 f9 62 00 80 d2 
  000001e0  70 00 80 d2 f0 07 00 f9  a3 00 80 d2 b0 00 80 d2 
  000001f0  f0 0b 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  00000200  00 40 07 91 d0 cd 90 d2  70 03 be f2 30 3f c4 f2 
  00000210  30 01 e8 f2 00 02 67 9e  d0 cd 90 d2 70 03 be f2 
  00000220  30 3f c4 f2 30 01 e8 f2  00 02 67 9e e0 03 00 fd 
  00000230  00 00 00 94 00 00 00 90  00 00 00 91 00 80 07 91 
  00000240  21 0c 80 d2 30 0c 80 d2  f0 03 00 f9 42 0b 80 d2 
  00000250  50 0b 80 d2 f0 07 00 f9  00 00 00 94 00 00 00 90 
  00000260  00 00 00 91 00 c0 07 91  21 00 80 d2 30 00 80 d2 
  00000270  f0 03 00 f9 42 00 80 d2  50 00 80 d2 f0 07 00 f9 
  00000280  00 00 00 94 00 00 00 90  00 00 00 91 00 20 08 91 
  00000290  21 00 80 d2 30 00 80 d2  f0 03 00 f9 02 00 80 d2 
  000002a0  10 00 80 d2 f0 07 00 f9  00 00 00 94 00 00 00 90 
  000002b0  00 00 00 91 00 60 08 91  01 00 00 90 21 00 00 91 
  000002c0  10 00 00 90 10 02 00 91  f0 03 00 f9 02 00 00 90 
  000002d0  42 00 00 91 10 00 00 90  10 02 00 91 f0 07 00 f9 
  000002e0  03 00 00 90 63 00 00 91  10 00 00 90 10 02 00 91 
  000002f0  f0 0b 00 f9 04 00 00 90  84 00 00 91 10 00 00 90 
  00000300  10 02 00 91 f0 0f 00 f9  00 00 00 94 00 00 00 90 
  00000310  00 00 00 91 00 a0 04 91  00 00 00 94 00 00 00 90 
  00000320  00 00 00 91 00 c0 08 91  00 00 00 94 00 00 00 90 
  00000330  00 00 00 91 00 40 09 91  00 00 00 94 00 00 00 90 
  00000340  00 00 00 91 00 a0 04 91  00 00 00 94 00 00 00 90 
  00000350  00 00 00 91 00 a0 09 91  01 00 00 90 21 00 00 91 
  00000360  10 00 00 90 10 02 00 91  f0 03 00 f9 00 00 00 94 
  00000370  00 00 00 90 00 00 00 91  00 e0 09 91 01 00 00 90 
  00000380  21 00 00 91 10 00 00 90  10 02 00 91 f0 03 00 f9 
  00000390  00 00 00 94 00 00 00 90  00 00 00 91 00 a0 04 91 
  000003a0  00 00 00 94 00 00 00 90  00 00 00 91 00 20 0a 91 
  000003b0  01 00 00 90 21 00 00 91  10 00 00 90 10 02 00 91 
  000003c0  f0 03 00 f9 02 00 00 90  42 00 00 91 10 00 00 90 
  000003d0  10 02 00 91 f0 07 00 f9  00 00 00 94 bf 03 00 91 
  000003e0  f0 03 00 91 10 c2 08 91  1d 7a 40 a9 ff 03 09 91 
  000003f0  00 00 80 d2 c0 03 5f d6 

.rodata (664 bytes):
  00000000  74 65 78 74 00 73 74 69  6c 6c 20 77 6f 72 6b 73 
  00000010  00 73 74 61 79 73 00 6f  6e 00 6f 6e 65 00 6c 69 
  00000020  6e 65 00 28 29 00 6e 75  6c 6c 00 6c 69 6e 65 31 
  00000030  0a 6c 69 6e 65 32 00 74  61 62 09 65 6e 64 00 00 
  00000040  f0 9f 93 98 20 54 75 74  6f 72 69 61 6c 3a 20 31 
  00000050  30 5f 70 72 69 6e 74 5f  73 68 6f 77 63 61 73 65 
  00000060  2e 66 70 0a 00 00 00 00  f0 9f a7 ad 20 46 6f 63 
  00000070  75 73 3a 20 43 6f 6d 70  72 65 68 65 6e 73 69 76 
  00000080  65 20 70 72 69 6e 74 6c  6e 21 2f 70 72 69 6e 74 
  00000090  20 73 68 6f 77 63 61 73  65 20 63 6f 76 65 72 69 
  000000a0  6e 67 20 76 61 72 69 61  64 69 63 20 61 72 67 75 
  000000b0  6d 65 6e 74 73 20 61 6e  64 20 72 75 6e 74 69 6d 
  000000c0  65 20 66 6f 72 6d 61 74  74 69 6e 67 0a 00 00 00 
  000000d0  f0 9f a7 aa 20 57 68 61  74 20 74 6f 20 6c 6f 6f 
  000000e0  6b 20 66 6f 72 3a 20 6c  61 62 65 6c 65 64 20 6f 
  000000f0  75 74 70 75 74 73 20 62  65 6c 6f 77 0a 00 00 00 
  00000100  e2 9c 85 20 45 78 70 65  63 74 61 74 69 6f 6e 3a 
  00000110  20 6f 75 74 70 75 74 73  20 6d 61 74 63 68 20 6c 
  00000120  61 62 65 6c 73 0a 00 00  0a 00 00 00 00 00 00 00 
  00000130  48 65 6c 6c 6f 00 00 00  57 6f 72 6c 64 20 77 69 
  00000140  74 68 20 6e 65 77 6c 69  6e 65 73 00 00 00 00 00 
  00000150  4e 75 6d 62 65 72 3a 20  25 6c 6c 64 00 00 00 00 
  00000160  42 6f 6f 6c 65 61 6e 3a  20 25 64 20 25 64 00 00 
  00000170  4d 69 78 65 64 3a 20 25  6c 6c 64 20 25 66 20 25 
  00000180  73 20 25 64 00 00 00 00  4e 61 6d 65 73 70 61 63 
  00000190  65 20 74 65 73 74 20 25  73 00 00 00 00 00 00 00 
  000001a0  76 61 6c 75 65 20 3d 20  25 6c 6c 64 0a 00 00 00 
  000001b0  6d 61 74 68 3a 20 25 6c  6c 64 20 2b 20 25 6c 6c 
  000001c0  64 20 3d 20 25 6c 6c 64  0a 00 00 00 00 00 00 00 
  000001d0  66 6c 6f 61 74 3a 20 25  66 0a 00 00 00 00 00 00 
  000001e0  63 68 61 72 73 3a 20 25  64 20 25 64 0a 00 00 00 
  000001f0  74 75 70 6c 65 3a 20 28  25 6c 6c 64 2c 20 25 6c 
  00000200  6c 64 29 0a 00 00 00 00  62 6f 6f 6c 73 3a 20 25 
  00000210  64 20 25 64 0a 00 00 00  54 68 69 73 20 25 73 20 
  00000220  25 73 20 25 73 20 25 73  00 00 00 00 00 00 00 00 
  00000230  43 6f 6e 74 69 6e 75 69  6e 67 20 77 69 74 68 6f 
  00000240  75 74 20 6e 65 77 6c 69  6e 65 00 00 00 00 00 00 
  00000250  20 2d 20 61 70 70 65 6e  64 65 64 20 63 6f 6e 74 
  00000260  65 6e 74 00 00 00 00 00  55 6e 69 74 3a 20 25 73 
  00000270  00 00 00 00 00 00 00 00  4e 75 6c 6c 3a 20 25 73 
  00000280  00 00 00 00 00 00 00 00  65 73 63 61 70 65 64 3a 
  00000290  20 25 73 20 25 73 0a 00 
