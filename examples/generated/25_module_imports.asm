fp-native dump: format=MachO arch=Aarch64 entry=0x1b4

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data__25_module_imports_echo_g0_0 ty=Array(I8, 6) constant=true initializer=Some(Bytes([72, 101, 108, 108, 111, 0]))
global GREETING ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0]))
global __const_data__25_module_imports_echo_g0_1 ty=Array(I8, 12) constant=true initializer=Some(Bytes([102, 105, 108, 101, 32, 109, 111, 100, 117, 108, 101, 0]))
global SOURCE ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 11, 0, 0, 0, 0, 0, 0, 0]))
global __const_data__25_module_imports_main_g0_2 ty=Array(I8, 6) constant=true initializer=Some(Bytes([70, 101, 114, 114, 111, 0]))
global __const_data__25_module_imports_main_g0_3 ty=Array(I8, 15) constant=true initializer=Some(Bytes([109, 111, 100, 117, 108, 101, 32, 105, 109, 112, 111, 114, 116, 115, 0]))
global __const_data__25_module_imports_main_g0_4 ty=Array(I8, 16) constant=true initializer=Some(Bytes([101, 120, 116, 101, 114, 110, 97, 108, 32, 109, 111, 100, 117, 108, 101, 0]))
fn helpers__math__add
  bb0 bb0
    alloca Virtual { id: 3, bank: General, size_bits: 64 }, 8
    add Virtual { id: 4, bank: General, size_bits: 64 }, symbol(local.1), symbol(local.2)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 4, bank: General, size_bits: 64 }
    load Virtual { id: 6, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn echo
  bb0 bb0
    intrinsic.call symbol(intrinsic.println), symbol(local.1)
    ret
fn modules__helpers__greet_from_file
  bb0 bb0
    alloca Virtual { id: 8, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 8, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    bitcast Virtual { id: 10, bank: General, size_bits: 64 }, Virtual { id: 8, bank: General, size_bits: 64 }
    load Virtual { id: 11, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 10, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), symbol(__const_data__25_module_imports_echo_g0_1), Virtual { id: 11, bank: General, size_bits: 64 }
    ret
fn modules__helpers__math__add
  bb0 bb0
    alloca Virtual { id: 13, bank: General, size_bits: 64 }, 8
    add Virtual { id: 14, bank: General, size_bits: 64 }, symbol(local.1), symbol(local.2)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 13, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 14, bank: General, size_bits: 64 }
    load Virtual { id: 16, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 13, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    call symbol(helpers__greet)(struct(len=2)) cc=C tail=false
    br
  bb1 bb1
    call symbol(helpers__greet)(struct(len=2)) cc=C tail=false
    br
  bb2 bb2
    call symbol(modules__helpers__greet_from_file)(struct(len=2)) cc=C tail=false
    br
  bb3 bb3
    call symbol(echo)(2025) cc=C tail=false
    br
  bb4 bb4
    call symbol(helpers__math__add)(8, 34) cc=C tail=false
    br
  bb5 bb5
    intrinsic.call symbol(intrinsic.println), Virtual { id: 25, bank: General, size_bits: 64 }
    call symbol(modules__helpers__math__add)(10, 32) cc=C tail=false
    br
  bb6 bb6
    intrinsic.call symbol(intrinsic.println), Virtual { id: 27, bank: General, size_bits: 64 }
    ret
fn helpers__greet
  bb0 bb0
    alloca Virtual { id: 29, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 29, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    bitcast Virtual { id: 31, bank: General, size_bits: 64 }, Virtual { id: 29, bank: General, size_bits: 64 }
    load Virtual { id: 32, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 31, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), symbol(__const_data__25_module_imports_echo_g0_0), Virtual { id: 32, bank: General, size_bits: 64 }
    ret


Symbols:
  helpers__math__add               0x00000000
  echo                             0x00000060
  modules__helpers__greet_from_file 0x000000a4
  modules__helpers__math__add      0x00000154
  main                             0x000001b4
  helpers__greet                   0x00000344

Text relocations:
  offset=0x00000074 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000008c kind=CallRel32 symbol=printf addend=0
  offset=0x00000110 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000011c kind=Aarch64AdrpAdd symbol=__const_data__25_module_imports_echo_g0_1 addend=0
  offset=0x00000124 kind=Aarch64AdrpAdd symbol=__const_data__25_module_imports_echo_g0_1 addend=0
  offset=0x0000013c kind=CallRel32 symbol=printf addend=0
  offset=0x000001c4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001d0 kind=CallRel32 symbol=printf addend=0
  offset=0x000001d4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001e0 kind=CallRel32 symbol=printf addend=0
  offset=0x000001e4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001f0 kind=CallRel32 symbol=printf addend=0
  offset=0x000001f4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000200 kind=CallRel32 symbol=printf addend=0
  offset=0x0000020c kind=Aarch64AdrpAdd symbol=__const_data__25_module_imports_main_g0_2 addend=0
  offset=0x0000024c kind=Aarch64AdrpAdd symbol=__const_data__25_module_imports_main_g0_3 addend=0
  offset=0x0000028c kind=Aarch64AdrpAdd symbol=__const_data__25_module_imports_main_g0_4 addend=0
  offset=0x000002e4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000002fc kind=CallRel32 symbol=printf addend=0
  offset=0x00000314 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000032c kind=CallRel32 symbol=printf addend=0
  offset=0x000003b0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000003bc kind=Aarch64AdrpAdd symbol=__const_data__25_module_imports_echo_g0_0 addend=0
  offset=0x000003c4 kind=Aarch64AdrpAdd symbol=__const_data__25_module_imports_echo_g0_0 addend=0
  offset=0x000003dc kind=CallRel32 symbol=printf addend=0

Section relocations:
  section=Data offset=0x00000000 kind=Abs64 symbol=__const_data__25_module_imports_echo_g0_0 addend=0
  section=Data offset=0x00000010 kind=Abs64 symbol=__const_data__25_module_imports_echo_g0_1 addend=0

.text (1012 bytes):
  00000000  ff 83 03 d1 fd 7b 0d a9  fd 03 00 91 e0 43 00 f9 
  00000010  e1 47 00 f9 1f 20 03 d5  f0 03 00 91 10 42 02 91 
  00000020  f0 03 00 f9 f0 43 40 f9  f1 47 40 f9 10 02 11 8b 
  00000030  f0 07 00 f9 f1 03 40 f9  f0 07 40 f9 30 02 00 f9 
  00000040  f0 03 40 f9 11 02 40 f9  f1 0f 00 f9 e0 0f 40 f9 
  00000050  bf 03 00 91 fd 7b 4d a9  ff 83 03 91 c0 03 5f d6 
  00000060  ff 03 03 d1 fd 7b 0b a9  fd 03 00 91 e0 4b 00 f9 
  00000070  1f 20 03 d5 00 00 00 90  00 00 00 91 00 e0 00 91 
  00000080  e1 4b 40 f9 f0 4b 40 f9  f0 03 00 f9 00 00 00 94 
  00000090  bf 03 00 91 fd 7b 4b a9  ff 03 03 91 00 00 80 d2 
  000000a0  c0 03 5f d6 ff 43 07 d1  fd 7b 1c a9 fd 03 00 91 
  000000b0  e9 03 00 aa 30 01 40 f9  f0 4f 00 f9 e9 03 00 aa 
  000000c0  29 21 00 91 30 01 40 f9  f0 53 00 f9 1f 20 03 d5 
  000000d0  f0 03 00 91 10 02 03 91  f0 17 00 f9 f1 17 40 f9 
  000000e0  f0 4f 40 f9 e9 03 11 aa  30 01 00 f9 f0 53 40 f9 
  000000f0  e9 03 11 aa 29 21 00 91  30 01 00 f9 f0 17 40 f9 
  00000100  f0 1f 00 f9 f0 1f 40 f9  11 02 40 f9 f1 23 00 f9 
  00000110  00 00 00 90 00 00 00 91  00 20 01 91 01 00 00 90 
  00000120  21 00 00 91 10 00 00 90  10 02 00 91 f0 03 00 f9 
  00000130  e2 23 40 f9 f0 23 40 f9  f0 07 00 f9 00 00 00 94 
  00000140  bf 03 00 91 fd 7b 5c a9  ff 43 07 91 00 00 80 d2 
  00000150  c0 03 5f d6 ff 83 03 d1  fd 7b 0d a9 fd 03 00 91 
  00000160  e0 43 00 f9 e1 47 00 f9  1f 20 03 d5 f0 03 00 91 
  00000170  10 42 02 91 f0 1b 00 f9  f0 43 40 f9 f1 47 40 f9 
  00000180  10 02 11 8b f0 1f 00 f9  f1 1b 40 f9 f0 1f 40 f9 
  00000190  30 02 00 f9 f0 1b 40 f9  11 02 40 f9 f1 27 00 f9 
  000001a0  e0 27 40 f9 bf 03 00 91  fd 7b 4d a9 ff 83 03 91 
  000001b0  c0 03 5f d6 ff 83 05 d1  fd 7b 15 a9 fd 03 00 91 
  000001c0  1f 20 03 d5 00 00 00 90  00 00 00 91 00 80 01 91 
  000001d0  00 00 00 94 00 00 00 90  00 00 00 91 00 20 02 91 
  000001e0  00 00 00 94 00 00 00 90  00 00 00 91 00 40 03 91 
  000001f0  00 00 00 94 00 00 00 90  00 00 00 91 00 40 04 91 
  00000200  00 00 00 94 f1 03 00 91  31 e2 04 91 10 00 00 90 
  00000210  10 02 00 91 e9 03 11 aa  30 01 00 f9 b0 00 80 d2 
  00000220  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 e9 03 11 aa 
  00000230  29 21 00 91 30 01 00 f9  e0 03 11 aa 42 00 00 94 
  00000240  01 00 00 14 f1 03 00 91  31 e2 04 91 10 00 00 90 
  00000250  10 02 00 91 e9 03 11 aa  30 01 00 f9 d0 01 80 d2 
  00000260  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 e9 03 11 aa 
  00000270  29 21 00 91 30 01 00 f9  e0 03 11 aa 32 00 00 94 
  00000280  01 00 00 14 f1 03 00 91  31 e2 04 91 10 00 00 90 
  00000290  10 02 00 91 e9 03 11 aa  30 01 00 f9 f0 01 80 d2 
  000002a0  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 e9 03 11 aa 
  000002b0  29 21 00 91 30 01 00 f9  e0 03 11 aa 7a ff ff 97 
  000002c0  01 00 00 14 20 fd 80 d2  66 ff ff 97 01 00 00 14 
  000002d0  00 01 80 d2 41 04 80 d2  4a ff ff 97 e0 4f 00 f9 
  000002e0  01 00 00 14 00 00 00 90  00 00 00 91 00 60 04 91 
  000002f0  e1 4f 40 f9 f0 4f 40 f9  f0 03 00 f9 00 00 00 94 
  00000300  40 01 80 d2 01 04 80 d2  93 ff ff 97 e0 57 00 f9 
  00000310  01 00 00 14 00 00 00 90  00 00 00 91 00 c0 04 91 
  00000320  e1 57 40 f9 f0 57 40 f9  f0 03 00 f9 00 00 00 94 
  00000330  bf 03 00 91 fd 7b 55 a9  ff 83 05 91 00 00 80 d2 
  00000340  c0 03 5f d6 ff 43 07 d1  fd 7b 1c a9 fd 03 00 91 
  00000350  e9 03 00 aa 30 01 40 f9  f0 4f 00 f9 e9 03 00 aa 
  00000360  29 21 00 91 30 01 40 f9  f0 53 00 f9 1f 20 03 d5 
  00000370  f0 03 00 91 10 02 03 91  f0 37 00 f9 f1 37 40 f9 
  00000380  f0 4f 40 f9 e9 03 11 aa  30 01 00 f9 f0 53 40 f9 
  00000390  e9 03 11 aa 29 21 00 91  30 01 00 f9 f0 37 40 f9 
  000003a0  f0 3f 00 f9 f0 3f 40 f9  11 02 40 f9 f1 43 00 f9 
  000003b0  00 00 00 90 00 00 00 91  00 40 05 91 01 00 00 90 
  000003c0  21 00 00 91 10 00 00 90  10 02 00 91 f0 03 00 f9 
  000003d0  e2 43 40 f9 f0 43 40 f9  f0 07 00 f9 00 00 00 94 
  000003e0  bf 03 00 91 fd 7b 5c a9  ff 43 07 91 00 00 80 d2 
  000003f0  c0 03 5f d6 

.rodata (344 bytes):
  00000000  48 65 6c 6c 6f 00 66 69  6c 65 20 6d 6f 64 75 6c 
  00000010  65 00 46 65 72 72 6f 00  6d 6f 64 75 6c 65 20 69 
  00000020  6d 70 6f 72 74 73 00 65  78 74 65 72 6e 61 6c 20 
  00000030  6d 6f 64 75 6c 65 00 00  65 63 68 6f 3a 20 25 6c 
  00000040  6c 64 0a 00 00 00 00 00  5b 25 73 5d 20 48 65 6c 
  00000050  6c 6f 2c 20 25 73 21 0a  00 00 00 00 00 00 00 00 
  00000060  f0 9f 93 a6 20 45 78 61  6d 70 6c 65 3a 20 32 35 
  00000070  5f 6d 6f 64 75 6c 65 5f  69 6d 70 6f 72 74 73 2e 
  00000080  66 70 0a 00 00 00 00 00  f0 9f 94 a7 20 46 6f 63 
  00000090  75 73 3a 20 69 6e 6c 69  6e 65 20 6d 6f 64 75 6c 
  000000a0  65 73 20 2b 20 65 78 74  65 72 6e 61 6c 20 66 69 
  000000b0  6c 65 20 6d 6f 64 75 6c  65 73 20 2b 20 73 74 64 
  000000c0  20 69 6d 70 6f 72 74 73  0a 00 00 00 00 00 00 00 
  000000d0  f0 9f 94 8d 20 45 78 70  65 63 74 3a 20 67 72 65 
  000000e0  65 74 69 6e 67 73 20 2b  20 6d 61 74 68 20 6f 75 
  000000f0  74 70 75 74 20 75 73 69  6e 67 20 69 6d 70 6f 72 
  00000100  74 65 64 20 6e 61 6d 65  73 0a 00 00 00 00 00 00 
  00000110  0a 00 00 00 00 00 00 00  6d 61 74 68 2e 61 64 64 
  00000120  28 38 2c 20 33 34 29 20  3d 20 25 6c 6c 64 0a 00 
  00000130  66 69 6c 65 5f 6d 61 74  68 2e 61 64 64 28 31 30 
  00000140  2c 20 33 32 29 20 3d 20  25 6c 6c 64 0a 00 00 00 
  00000150  25 73 20 25 73 21 0a 00 
