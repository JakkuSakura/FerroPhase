fp-native dump: format=MachO arch=Aarch64 entry=0x1c0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data_0 ty=Array(I8, 6) constant=true initializer=Some(Bytes([72, 101, 108, 108, 111, 0]))
global ::GREETING ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_1 ty=Array(I8, 6) constant=true initializer=Some(Bytes([70, 101, 114, 114, 111, 0]))
global __const_data_2 ty=Array(I8, 15) constant=true initializer=Some(Bytes([109, 111, 100, 117, 108, 101, 32, 105, 109, 112, 111, 114, 116, 115, 0]))
global __const_data_3 ty=Array(I8, 16) constant=true initializer=Some(Bytes([101, 120, 116, 101, 114, 110, 97, 108, 32, 109, 111, 100, 117, 108, 101, 0]))
global __const_data_4 ty=Array(I8, 12) constant=true initializer=Some(Bytes([102, 105, 108, 101, 32, 109, 111, 100, 117, 108, 101, 0]))
global ::SOURCE ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 11, 0, 0, 0, 0, 0, 0, 0]))
fn examples__25_module_imports__helpers__greet
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 1, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    load Virtual { id: 3, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 3, bank: General, size_bits: 64 }
    load Virtual { id: 5, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    extractvalue Virtual { id: 6, bank: General, size_bits: 64 }, Virtual { id: 5, bank: General, size_bits: 64 }, 0
    intrinsic.call symbol(intrinsic.println), symbol(__const_data_0), Virtual { id: 6, bank: General, size_bits: 64 }
    ret
fn examples__25_module_imports__helpers__math__add
  bb0 bb0
    alloca Virtual { id: 8, bank: General, size_bits: 64 }, 1
    add Virtual { id: 9, bank: General, size_bits: 64 }, symbol(local.1), symbol(local.2)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 8, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 9, bank: General, size_bits: 64 }
    load Virtual { id: 11, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 8, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn examples__25_module_imports__echo
  bb0 bb0
    intrinsic.call symbol(intrinsic.println), symbol(local.1)
    ret
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    call symbol(examples__25_module_imports__helpers__greet)(struct(len=2)) cc=C tail=false
    br
  bb1 bb1
    call symbol(examples__25_module_imports__helpers__greet)(struct(len=2)) cc=C tail=false
    br
  bb2 bb2
    call symbol(examples__25_module_imports__modules__helpers__greet_from_file)(struct(len=2)) cc=C tail=false
    br
  bb3 bb3
    call symbol(examples__25_module_imports__echo)(2025) cc=C tail=false
    br
  bb4 bb4
    call symbol(examples__25_module_imports__helpers__math__add)(8, 34) cc=C tail=false
    br
  bb5 bb5
    intrinsic.call symbol(intrinsic.println), Virtual { id: 21, bank: General, size_bits: 64 }
    call symbol(examples__25_module_imports__modules__helpers__math__add)(10, 32) cc=C tail=false
    br
  bb6 bb6
    intrinsic.call symbol(intrinsic.println), Virtual { id: 23, bank: General, size_bits: 64 }
    ret
fn examples__25_module_imports__modules__helpers__greet_from_file
  bb0 bb0
    alloca Virtual { id: 25, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 26, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 26, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    load Virtual { id: 28, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 26, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 25, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 28, bank: General, size_bits: 64 }
    load Virtual { id: 30, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 25, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    extractvalue Virtual { id: 31, bank: General, size_bits: 64 }, Virtual { id: 30, bank: General, size_bits: 64 }, 0
    intrinsic.call symbol(intrinsic.println), symbol(__const_data_4), Virtual { id: 31, bank: General, size_bits: 64 }
    ret
fn examples__25_module_imports__modules__helpers__math__add
  bb0 bb0
    alloca Virtual { id: 33, bank: General, size_bits: 64 }, 1
    add Virtual { id: 34, bank: General, size_bits: 64 }, symbol(local.1), symbol(local.2)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 33, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 34, bank: General, size_bits: 64 }
    load Virtual { id: 36, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 33, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret


Symbols:
  examples__25_module_imports__helpers__greet 0x00000000
  examples__25_module_imports__helpers__math__add 0x00000124
  examples__25_module_imports__echo 0x00000180
  main                             0x000001c0
  examples__25_module_imports__modules__helpers__greet_from_file 0x0000034c
  examples__25_module_imports__modules__helpers__math__add 0x00000470

Text relocations:
  offset=0x000000e0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000ec kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000000f4 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x0000010c kind=CallRel32 symbol=printf addend=0
  offset=0x00000190 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001a8 kind=CallRel32 symbol=printf addend=0
  offset=0x000001cc kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001d8 kind=CallRel32 symbol=printf addend=0
  offset=0x000001dc kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001e8 kind=CallRel32 symbol=printf addend=0
  offset=0x000001ec kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001f8 kind=CallRel32 symbol=printf addend=0
  offset=0x000001fc kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000208 kind=CallRel32 symbol=printf addend=0
  offset=0x00000214 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x00000254 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x00000294 kind=Aarch64AdrpAdd symbol=__const_data_3 addend=0
  offset=0x000002ec kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000304 kind=CallRel32 symbol=printf addend=0
  offset=0x0000031c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000334 kind=CallRel32 symbol=printf addend=0
  offset=0x0000042c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000438 kind=Aarch64AdrpAdd symbol=__const_data_4 addend=0
  offset=0x00000440 kind=Aarch64AdrpAdd symbol=__const_data_4 addend=0
  offset=0x00000458 kind=CallRel32 symbol=printf addend=0

Section relocations:
  section=Data offset=0x00000000 kind=Abs64 symbol=__const_data_0 addend=0
  section=Data offset=0x00000010 kind=Abs64 symbol=__const_data_4 addend=0

.text (1228 bytes):
  00000000  ff 43 03 d1 fd 7b 0c a9  fd 03 00 91 e9 03 00 aa 
  00000010  30 01 40 f9 f0 2f 00 f9  e9 03 00 aa 29 21 00 91 
  00000020  30 01 40 f9 f0 33 00 f9  f0 03 00 91 10 82 02 91 
  00000030  f0 0b 00 f9 f0 03 00 91  10 c2 02 91 f0 0f 00 f9 
  00000040  f1 0f 40 f9 f0 2f 40 f9  e9 03 11 aa 30 01 00 f9 
  00000050  f0 33 40 f9 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  00000060  f1 0f 40 f9 e9 03 11 aa  30 01 40 f9 f0 43 00 f9 
  00000070  e9 03 11 aa 29 21 00 91  30 01 40 f9 f0 47 00 f9 
  00000080  f0 03 00 91 10 02 02 91  f0 17 00 f9 f1 0b 40 f9 
  00000090  f0 43 40 f9 e9 03 11 aa  30 01 00 f9 f0 47 40 f9 
  000000a0  e9 03 11 aa 29 21 00 91  30 01 00 f9 f1 0b 40 f9 
  000000b0  e9 03 11 aa 30 01 40 f9  f0 4b 00 f9 e9 03 11 aa 
  000000c0  29 21 00 91 30 01 40 f9  f0 4f 00 f9 f0 03 00 91 
  000000d0  10 42 02 91 f0 1f 00 f9  f0 4b 40 f9 f0 23 00 f9 
  000000e0  00 00 00 90 00 00 00 91  00 e0 00 91 01 00 00 90 
  000000f0  21 00 00 91 10 00 00 90  10 02 00 91 f0 03 00 f9 
  00000100  e2 23 40 f9 f0 23 40 f9  f0 07 00 f9 00 00 00 94 
  00000110  bf 03 00 91 fd 7b 4c a9  ff 43 03 91 00 00 80 d2 
  00000120  c0 03 5f d6 ff 43 01 d1  fd 7b 04 a9 fd 03 00 91 
  00000130  e0 17 00 f9 e1 1b 00 f9  f0 03 00 91 10 e2 00 91 
  00000140  f0 03 00 f9 f0 17 40 f9  f1 1b 40 f9 10 02 11 8b 
  00000150  f0 07 00 f9 f1 03 40 f9  f0 07 40 f9 30 02 00 f9 
  00000160  f0 03 40 f9 11 02 40 f9  f1 0f 00 f9 e0 0f 40 f9 
  00000170  bf 03 00 91 fd 7b 44 a9  ff 43 01 91 c0 03 5f d6 
  00000180  ff 03 01 d1 fd 7b 03 a9  fd 03 00 91 e0 13 00 f9 
  00000190  00 00 00 90 00 00 00 91  00 00 01 91 e1 13 40 f9 
  000001a0  f0 13 40 f9 f0 03 00 f9  00 00 00 94 bf 03 00 91 
  000001b0  fd 7b 43 a9 ff 03 01 91  00 00 80 d2 c0 03 5f d6 
  000001c0  ff 03 04 d1 fd 7b 0f a9  fd 03 00 91 00 00 00 90 
  000001d0  00 00 00 91 00 40 01 91  00 00 00 94 00 00 00 90 
  000001e0  00 00 00 91 00 e0 01 91  00 00 00 94 00 00 00 90 
  000001f0  00 00 00 91 00 00 03 91  00 00 00 94 00 00 00 90 
  00000200  00 00 00 91 00 00 04 91  00 00 00 94 f1 03 00 91 
  00000210  31 62 03 91 10 00 00 90  10 02 00 91 e9 03 11 aa 
  00000220  30 01 00 f9 b0 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000230  10 00 e0 f2 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  00000240  e0 03 11 aa 6f ff ff 97  01 00 00 14 f1 03 00 91 
  00000250  31 62 03 91 10 00 00 90  10 02 00 91 e9 03 11 aa 
  00000260  30 01 00 f9 d0 01 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000270  10 00 e0 f2 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  00000280  e0 03 11 aa 5f ff ff 97  01 00 00 14 f1 03 00 91 
  00000290  31 62 03 91 10 00 00 90  10 02 00 91 e9 03 11 aa 
  000002a0  30 01 00 f9 f0 01 80 d2  10 00 a0 f2 10 00 c0 f2 
  000002b0  10 00 e0 f2 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  000002c0  e0 03 11 aa 22 00 00 94  01 00 00 14 20 fd 80 d2 
  000002d0  ac ff ff 97 01 00 00 14  00 01 80 d2 41 04 80 d2 
  000002e0  91 ff ff 97 e0 2b 00 f9  01 00 00 14 00 00 00 90 
  000002f0  00 00 00 91 00 20 04 91  e1 2b 40 f9 f0 2b 40 f9 
  00000300  f0 03 00 f9 00 00 00 94  40 01 80 d2 01 04 80 d2 
  00000310  58 00 00 94 e0 33 00 f9  01 00 00 14 00 00 00 90 
  00000320  00 00 00 91 00 80 04 91  e1 33 40 f9 f0 33 40 f9 
  00000330  f0 03 00 f9 00 00 00 94  bf 03 00 91 fd 7b 4f a9 
  00000340  ff 03 04 91 00 00 80 d2  c0 03 5f d6 ff 43 03 d1 
  00000350  fd 7b 0c a9 fd 03 00 91  e9 03 00 aa 30 01 40 f9 
  00000360  f0 2f 00 f9 e9 03 00 aa  29 21 00 91 30 01 40 f9 
  00000370  f0 33 00 f9 f0 03 00 91  10 82 02 91 f0 0b 00 f9 
  00000380  f0 03 00 91 10 c2 02 91  f0 0f 00 f9 f1 0f 40 f9 
  00000390  f0 2f 40 f9 e9 03 11 aa  30 01 00 f9 f0 33 40 f9 
  000003a0  e9 03 11 aa 29 21 00 91  30 01 00 f9 f1 0f 40 f9 
  000003b0  e9 03 11 aa 30 01 40 f9  f0 43 00 f9 e9 03 11 aa 
  000003c0  29 21 00 91 30 01 40 f9  f0 47 00 f9 f0 03 00 91 
  000003d0  10 02 02 91 f0 17 00 f9  f1 0b 40 f9 f0 43 40 f9 
  000003e0  e9 03 11 aa 30 01 00 f9  f0 47 40 f9 e9 03 11 aa 
  000003f0  29 21 00 91 30 01 00 f9  f1 0b 40 f9 e9 03 11 aa 
  00000400  30 01 40 f9 f0 4b 00 f9  e9 03 11 aa 29 21 00 91 
  00000410  30 01 40 f9 f0 4f 00 f9  f0 03 00 91 10 42 02 91 
  00000420  f0 1f 00 f9 f0 4b 40 f9  f0 23 00 f9 00 00 00 90 
  00000430  00 00 00 91 00 00 05 91  01 00 00 90 21 00 00 91 
  00000440  10 00 00 90 10 02 00 91  f0 03 00 f9 e2 23 40 f9 
  00000450  f0 23 40 f9 f0 07 00 f9  00 00 00 94 bf 03 00 91 
  00000460  fd 7b 4c a9 ff 43 03 91  00 00 80 d2 c0 03 5f d6 
  00000470  ff 43 01 d1 fd 7b 04 a9  fd 03 00 91 e0 17 00 f9 
  00000480  e1 1b 00 f9 f0 03 00 91  10 e2 00 91 f0 03 00 f9 
  00000490  f0 17 40 f9 f1 1b 40 f9  10 02 11 8b f0 07 00 f9 
  000004a0  f1 03 40 f9 f0 07 40 f9  30 02 00 f9 f0 03 40 f9 
  000004b0  11 02 40 f9 f1 0f 00 f9  e0 0f 40 f9 bf 03 00 91 
  000004c0  fd 7b 44 a9 ff 43 01 91  c0 03 5f d6 

.rodata (337 bytes):
  00000000  48 65 6c 6c 6f 00 46 65  72 72 6f 00 6d 6f 64 75 
  00000010  6c 65 20 69 6d 70 6f 72  74 73 00 65 78 74 65 72 
  00000020  6e 61 6c 20 6d 6f 64 75  6c 65 00 66 69 6c 65 20 
  00000030  6d 6f 64 75 6c 65 00 00  25 73 20 25 73 21 0a 00 
  00000040  65 63 68 6f 3a 20 25 6c  6c 64 0a 00 00 00 00 00 
  00000050  f0 9f 93 a6 20 45 78 61  6d 70 6c 65 3a 20 32 35 
  00000060  5f 6d 6f 64 75 6c 65 5f  69 6d 70 6f 72 74 73 2e 
  00000070  66 70 0a 00 00 00 00 00  f0 9f 94 a7 20 46 6f 63 
  00000080  75 73 3a 20 69 6e 6c 69  6e 65 20 6d 6f 64 75 6c 
  00000090  65 73 20 2b 20 65 78 74  65 72 6e 61 6c 20 66 69 
  000000a0  6c 65 20 6d 6f 64 75 6c  65 73 20 2b 20 73 74 64 
  000000b0  20 69 6d 70 6f 72 74 73  0a 00 00 00 00 00 00 00 
  000000c0  f0 9f 94 8d 20 45 78 70  65 63 74 3a 20 67 72 65 
  000000d0  65 74 69 6e 67 73 20 2b  20 6d 61 74 68 20 6f 75 
  000000e0  74 70 75 74 20 75 73 69  6e 67 20 69 6d 70 6f 72 
  000000f0  74 65 64 20 6e 61 6d 65  73 0a 00 00 00 00 00 00 
  00000100  0a 00 00 00 00 00 00 00  6d 61 74 68 2e 61 64 64 
  00000110  28 38 2c 20 33 34 29 20  3d 20 25 6c 6c 64 0a 00 
  00000120  66 69 6c 65 5f 6d 61 74  68 2e 61 64 64 28 31 30 
  00000130  2c 20 33 32 29 20 3d 20  25 6c 6c 64 0a 00 00 00 
  00000140  5b 25 73 5d 20 48 65 6c  6c 6f 2c 20 25 73 21 0a 
  00000150  00 
