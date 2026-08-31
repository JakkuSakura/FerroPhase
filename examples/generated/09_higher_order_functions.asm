fp-native dump: format=MachO arch=Aarch64 entry=0x3bc

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
fn add_i64
  bb0 bb0
    alloca Virtual { id: 3, bank: General, size_bits: 64 }, 8
    add Virtual { id: 4, bank: General, size_bits: 64 }, symbol(local.1), symbol(local.2)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 4, bank: General, size_bits: 64 }
    load Virtual { id: 6, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn add_f64
  bb0 bb0
    alloca Virtual { id: 7, bank: General, size_bits: 64 }, 8
    add Virtual { id: 8, bank: Float, size_bits: 64 }, symbol(local.1), symbol(local.2)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 7, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 8, bank: Float, size_bits: 64 }
    load Virtual { id: 10, bank: Float, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 7, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn apply_f64
  bb0 bb0
    call local.3(local.1, local.2) cc=C tail=false
    br
  bb1 bb1
    intrinsic.call symbol(intrinsic.println), symbol(local.1), symbol(local.2), Virtual { id: 11, bank: Float, size_bits: 64 }
    ret
fn make_adder
  bb0 bb0
    alloca Virtual { id: 13, bank: General, size_bits: 64 }, 8
    insertvalue Virtual { id: 14, bank: General, size_bits: 64 }, 0, symbol(local.1), 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 13, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 14, bank: General, size_bits: 64 }
    load Virtual { id: 16, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 13, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn apply_if
  bb0 bb0
    alloca Virtual { id: 17, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 18, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 18, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    load Virtual { id: 20, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 18, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 21, bank: General, size_bits: 8 }, Virtual { id: 20, bank: General, size_bits: 8 }, 1
    condbr
  bb1 bb1
    call local.4(local.2, local.3) cc=C tail=false
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 17, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 22, bank: General, size_bits: 64 }
    br
  bb2 bb2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 17, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    br
  bb4 bb4
    br
  bb3 bb3
    load Virtual { id: 25, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 17, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn apply_i64
  bb0 bb0
    call local.3(local.1, local.2) cc=C tail=false
    br
  bb1 bb1
    intrinsic.call symbol(intrinsic.println), symbol(local.1), symbol(local.2), Virtual { id: 26, bank: General, size_bits: 64 }
    ret
fn __closure09_higher_order_functions_0_call
  bb0 bb0
    alloca Virtual { id: 28, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 29, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 29, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    bitcast Virtual { id: 31, bank: General, size_bits: 64 }, Virtual { id: 29, bank: General, size_bits: 64 }
    load Virtual { id: 32, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 31, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 33, bank: General, size_bits: 64 }, symbol(local.2), Virtual { id: 32, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 28, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 33, bank: General, size_bits: 64 }
    load Virtual { id: 35, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 28, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn __closure09_higher_order_functions_1_call
  bb0 bb0
    alloca Virtual { id: 36, bank: General, size_bits: 64 }, 8
    mul Virtual { id: 37, bank: General, size_bits: 64 }, symbol(local.2), 2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 37, bank: General, size_bits: 64 }
    load Virtual { id: 39, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    call symbol(apply_i64)(10, 20, symbol(add_i64)) cc=C tail=false
    br
  bb1 bb1
    call symbol(apply_f64)(1.5, 2.5, symbol(add_f64)) cc=C tail=false
    br
  bb2 bb2
    intrinsic.call symbol(intrinsic.println)
    zext Virtual { id: 49, bank: General, size_bits: 32 }, 1
    call symbol(apply_if)(v49, 5, 3, symbol(add_i64)) cc=C tail=false
    br
  bb3 bb3
    intrinsic.call symbol(intrinsic.println), Virtual { id: 50, bank: General, size_bits: 64 }
    zext Virtual { id: 52, bank: General, size_bits: 32 }, 0
    call symbol(apply_if)(v52, 5, 3, symbol(add_i64)) cc=C tail=false
    br
  bb4 bb4
    intrinsic.call symbol(intrinsic.println), Virtual { id: 53, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    call symbol(make_adder)(10) cc=C tail=false
    alloca Virtual { id: 57, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 57, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 56, bank: General, size_bits: 64 }
    br
  bb5 bb5
    load Virtual { id: 59, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 57, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(__closure09_higher_order_functions_0_call)(v59, 5) cc=C tail=false
    br
  bb6 bb6
    intrinsic.call symbol(intrinsic.println), Virtual { id: 60, bank: General, size_bits: 64 }
    alloca Virtual { id: 62, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 62, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    load Virtual { id: 64, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 62, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(__closure09_higher_order_functions_1_call)(v64, 7) cc=C tail=false
    br
  bb7 bb7
    intrinsic.call symbol(intrinsic.println), Virtual { id: 65, bank: General, size_bits: 64 }
    ret


Symbols:
  add_i64                          0x00000000
  add_f64                          0x00000060
  apply_f64                        0x000000c0
  make_adder                       0x00000138
  apply_if                         0x0000018c
  apply_i64                        0x00000254
  __closure09_higher_order_functions_0_call 0x000002d0
  __closure09_higher_order_functions_1_call 0x0000035c
  main                             0x000003bc

Text relocations:
  offset=0x000000f4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000120 kind=CallRel32 symbol=printf addend=0
  offset=0x00000288 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000002b8 kind=CallRel32 symbol=printf addend=0
  offset=0x000003d4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000003e0 kind=CallRel32 symbol=printf addend=0
  offset=0x000003e4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000003f0 kind=CallRel32 symbol=printf addend=0
  offset=0x000003f4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000400 kind=CallRel32 symbol=printf addend=0
  offset=0x00000404 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000410 kind=CallRel32 symbol=printf addend=0
  offset=0x00000414 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000420 kind=CallRel32 symbol=printf addend=0
  offset=0x00000424 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000430 kind=CallRel32 symbol=printf addend=0
  offset=0x0000043c kind=Aarch64AdrpAdd symbol=add_i64 addend=0
  offset=0x00000474 kind=Aarch64AdrpAdd symbol=add_f64 addend=0
  offset=0x00000484 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000490 kind=CallRel32 symbol=printf addend=0
  offset=0x000004bc kind=Aarch64AdrpAdd symbol=add_i64 addend=0
  offset=0x000004d0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000004e8 kind=CallRel32 symbol=printf addend=0
  offset=0x00000514 kind=Aarch64AdrpAdd symbol=add_i64 addend=0
  offset=0x00000528 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000540 kind=CallRel32 symbol=printf addend=0
  offset=0x00000544 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000550 kind=CallRel32 symbol=printf addend=0
  offset=0x0000059c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000005b4 kind=CallRel32 symbol=printf addend=0
  offset=0x000005fc kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000614 kind=CallRel32 symbol=printf addend=0

.text (1588 bytes):
  00000000  ff 83 06 d1 fd 7b 19 a9  fd 03 00 91 e0 9f 00 f9 
  00000010  e1 a3 00 f9 1f 20 03 d5  f0 03 00 91 10 22 05 91 
  00000020  f0 03 00 f9 f0 9f 40 f9  f1 a3 40 f9 10 02 11 8b 
  00000030  f0 07 00 f9 f1 03 40 f9  f0 07 40 f9 30 02 00 f9 
  00000040  f0 03 40 f9 11 02 40 f9  f1 0f 00 f9 e0 0f 40 f9 
  00000050  bf 03 00 91 fd 7b 59 a9  ff 83 06 91 c0 03 5f d6 
  00000060  ff 83 06 d1 fd 7b 19 a9  fd 03 00 91 e0 9f 00 fd 
  00000070  e1 a3 00 fd 1f 20 03 d5  f0 03 00 91 10 22 05 91 
  00000080  f0 0f 00 f9 e0 9f 40 fd  e1 a3 40 fd 00 28 61 1e 
  00000090  e0 13 00 fd f1 0f 40 f9  e0 13 40 fd 20 02 00 fd 
  000000a0  f0 0f 40 f9 00 02 40 fd  e0 1b 00 fd e0 1b 40 fd 
  000000b0  bf 03 00 91 fd 7b 59 a9  ff 83 06 91 c0 03 5f d6 
  000000c0  ff 83 06 d1 fd 7b 19 a9  fd 03 00 91 e0 af 00 fd 
  000000d0  e1 b3 00 fd e0 b7 00 f9  1f 20 03 d5 e0 af 40 fd 
  000000e0  e1 b3 40 fd f0 b7 40 f9  00 02 3f d6 e0 2b 00 fd 
  000000f0  01 00 00 14 00 00 00 90  00 00 00 91 e0 af 40 fd 
  00000100  e0 af 40 fd e0 03 00 fd  e1 b3 40 fd e0 b3 40 fd 
  00000110  e0 07 00 fd e2 2b 40 fd  e0 2b 40 fd e0 0b 00 fd 
  00000120  00 00 00 94 bf 03 00 91  fd 7b 59 a9 ff 83 06 91 
  00000130  00 00 80 d2 c0 03 5f d6  ff 43 06 d1 fd 7b 18 a9 
  00000140  fd 03 00 91 e0 9f 00 f9  1f 20 03 d5 f0 03 00 91 
  00000150  10 02 05 91 f0 1f 00 f9  f0 9f 40 f9 f0 23 00 f9 
  00000160  f1 1f 40 f9 f0 23 40 f9  30 02 00 f9 f0 1f 40 f9 
  00000170  11 02 40 f9 f1 2b 00 f9  e0 2b 40 f9 bf 03 00 91 
  00000180  fd 7b 58 a9 ff 43 06 91  c0 03 5f d6 ff 83 07 d1 
  00000190  fd 7b 1d a9 fd 03 00 91  e0 23 05 39 e1 ab 00 f9 
  000001a0  e2 af 00 f9 e3 b3 00 f9  1f 20 03 d5 f0 03 00 91 
  000001b0  10 02 06 91 f0 2b 00 f9  f0 03 00 91 10 02 07 91 
  000001c0  f0 2f 00 f9 f1 2f 40 f9  f0 23 45 39 30 02 00 39 
  000001d0  f0 2f 40 f9 11 02 40 39  f1 37 00 f9 f0 a3 41 39 
  000001e0  1f 06 00 f1 f0 17 9f 9a  f0 3b 00 f9 f0 3b 40 f9 
  000001f0  1f 02 00 f1 41 00 00 54  0a 00 00 14 e0 ab 40 f9 
  00000200  e1 af 40 f9 f0 b3 40 f9  00 02 3f d6 e0 3f 00 f9 
  00000210  f1 2b 40 f9 f0 3f 40 f9  30 02 00 f9 05 00 00 14 
  00000220  f1 2b 40 f9 10 00 80 d2  30 02 00 f9 02 00 00 14 
  00000230  01 00 00 14 f0 2b 40 f9  11 02 40 f9 f1 4b 00 f9 
  00000240  e0 4b 40 f9 bf 03 00 91  fd 7b 5d a9 ff 83 07 91 
  00000250  c0 03 5f d6 ff 83 06 d1  fd 7b 19 a9 fd 03 00 91 
  00000260  e0 af 00 f9 e1 b3 00 f9  e2 b7 00 f9 1f 20 03 d5 
  00000270  e0 af 40 f9 e1 b3 40 f9  f0 b7 40 f9 00 02 3f d6 
  00000280  e0 53 00 f9 01 00 00 14  00 00 00 90 00 00 00 91 
  00000290  00 60 00 91 e1 af 40 f9  f0 af 40 f9 f0 03 00 f9 
  000002a0  e2 b3 40 f9 f0 b3 40 f9  f0 07 00 f9 e3 53 40 f9 
  000002b0  f0 53 40 f9 f0 0b 00 f9  00 00 00 94 bf 03 00 91 
  000002c0  fd 7b 59 a9 ff 83 06 91  00 00 80 d2 c0 03 5f d6 
  000002d0  ff 83 07 d1 fd 7b 1d a9  fd 03 00 91 e0 a3 00 f9 
  000002e0  e1 a7 00 f9 1f 20 03 d5  f0 03 00 91 10 42 05 91 
  000002f0  f0 47 00 f9 f0 03 00 91  10 42 06 91 f0 4b 00 f9 
  00000300  f1 4b 40 f9 f0 a3 40 f9  30 02 00 f9 f0 4b 40 f9 
  00000310  f0 53 00 f9 f0 53 40 f9  11 02 40 f9 f1 57 00 f9 
  00000320  f0 a7 40 f9 f1 57 40 f9  10 02 11 8b f0 5b 00 f9 
  00000330  f1 47 40 f9 f0 5b 40 f9  30 02 00 f9 f0 47 40 f9 
  00000340  11 02 40 f9 f1 63 00 f9  e0 63 40 f9 bf 03 00 91 
  00000350  fd 7b 5d a9 ff 83 07 91  c0 03 5f d6 ff 83 06 d1 
  00000360  fd 7b 19 a9 fd 03 00 91  e0 e3 04 39 e1 a3 00 f9 
  00000370  1f 20 03 d5 f0 03 00 91  10 22 05 91 f0 5f 00 f9 
  00000380  f0 a3 40 f9 51 00 80 d2  10 7e 11 9b f0 63 00 f9 
  00000390  f1 5f 40 f9 f0 63 40 f9  30 02 00 f9 f0 5f 40 f9 
  000003a0  11 02 40 f9 f1 6b 00 f9  e0 6b 40 f9 bf 03 00 91 
  000003b0  fd 7b 59 a9 ff 83 06 91  c0 03 5f d6 ff 43 0b d1 
  000003c0  f0 03 00 91 10 02 0b 91  1d 7a 00 a9 fd 03 00 91 
  000003d0  1f 20 03 d5 00 00 00 90  00 00 00 91 00 e0 00 91 
  000003e0  00 00 00 94 00 00 00 90  00 00 00 91 00 a0 01 91 
  000003f0  00 00 00 94 00 00 00 90  00 00 00 91 00 00 03 91 
  00000400  00 00 00 94 00 00 00 90  00 00 00 91 00 c0 03 91 
  00000410  00 00 00 94 00 00 00 90  00 00 00 91 00 60 04 91 
  00000420  00 00 00 94 00 00 00 90  00 00 00 91 00 80 04 91 
  00000430  00 00 00 94 40 01 80 d2  81 02 80 d2 02 00 00 90 
  00000440  42 00 00 91 84 ff ff 97  01 00 00 14 10 00 80 d2 
  00000450  10 00 a0 f2 10 00 c0 f2  10 ff e7 f2 00 02 67 9e 
  00000460  10 00 80 d2 10 00 a0 f2  10 00 c0 f2 90 00 e8 f2 
  00000470  01 02 67 9e 00 00 00 90  00 00 00 91 11 ff ff 97 
  00000480  01 00 00 14 00 00 00 90  00 00 00 91 00 e0 04 91 
  00000490  00 00 00 94 30 00 80 d2  31 00 80 d2 11 00 a0 f2 
  000004a0  11 00 c0 f2 11 00 e0 f2  10 02 11 8a f0 97 00 f9 
  000004b0  e0 2b 81 b9 a1 00 80 d2  62 00 80 d2 03 00 00 90 
  000004c0  63 00 00 91 32 ff ff 97  e0 9b 00 f9 01 00 00 14 
  000004d0  00 00 00 90 00 00 00 91  00 20 05 91 e1 9b 40 f9 
  000004e0  f0 9b 40 f9 f0 03 00 f9  00 00 00 94 10 00 80 d2 
  000004f0  31 00 80 d2 11 00 a0 f2  11 00 c0 f2 11 00 e0 f2 
  00000500  10 02 11 8a f0 a3 00 f9  e0 43 81 b9 a1 00 80 d2 
  00000510  62 00 80 d2 03 00 00 90  63 00 00 91 1c ff ff 97 
  00000520  e0 a7 00 f9 01 00 00 14  00 00 00 90 00 00 00 91 
  00000530  00 a0 05 91 e1 a7 40 f9  f0 a7 40 f9 f0 03 00 f9 
  00000540  00 00 00 94 00 00 00 90  00 00 00 91 00 20 06 91 
  00000550  00 00 00 94 40 01 80 d2  f8 fe ff 97 e0 b3 00 f9 
  00000560  f0 03 00 91 10 c2 09 91  f0 b7 00 f9 f1 b7 40 f9 
  00000570  f0 b3 40 f9 30 02 00 f9  01 00 00 14 f0 b7 40 f9 
  00000580  11 02 40 f9 f1 bf 00 f9  e0 bf 40 f9 a1 00 80 d2 
  00000590  50 ff ff 97 e0 c3 00 f9  01 00 00 14 00 00 00 90 
  000005a0  00 00 00 91 00 80 06 91  e1 c3 40 f9 f0 c3 40 f9 
  000005b0  f0 03 00 f9 00 00 00 94  f0 03 00 91 10 c2 0a 91 
  000005c0  f0 cb 00 f9 10 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  000005d0  10 00 e0 f2 f1 cb 40 f9  30 02 00 f9 f0 cb 40 f9 
  000005e0  11 02 40 f9 f1 d3 00 f9  e0 d3 40 f9 e1 00 80 d2 
  000005f0  5b ff ff 97 e0 d7 00 f9  01 00 00 14 00 00 00 90 
  00000600  00 00 00 91 00 e0 06 91  e1 d7 40 f9 f0 d7 40 f9 
  00000610  f0 03 00 f9 00 00 00 94  bf 03 00 91 f0 03 00 91 
  00000620  10 02 0b 91 1d 7a 40 a9  ff 43 0b 91 00 00 80 d2 
  00000630  c0 03 5f d6 

.rodata (458 bytes):
  00000000  61 70 70 6c 79 28 25 66  2c 20 25 66 29 20 3d 20 
  00000010  25 66 0a 00 00 00 00 00  61 70 70 6c 79 28 25 6c 
  00000020  6c 64 2c 20 25 6c 6c 64  29 20 3d 20 25 6c 6c 64 
  00000030  0a 00 00 00 00 00 00 00  f0 9f 93 98 20 54 75 74 
  00000040  6f 72 69 61 6c 3a 20 30  39 5f 68 69 67 68 65 72 
  00000050  5f 6f 72 64 65 72 5f 66  75 6e 63 74 69 6f 6e 73 
  00000060  2e 66 70 0a 00 00 00 00  f0 9f a7 ad 20 46 6f 63 
  00000070  75 73 3a 20 48 69 67 68  65 72 2d 6f 72 64 65 72 
  00000080  20 66 75 6e 63 74 69 6f  6e 73 3a 20 70 61 73 73 
  00000090  69 6e 67 20 66 75 6e 63  74 69 6f 6e 73 20 61 73 
  000000a0  20 61 72 67 75 6d 65 6e  74 73 20 61 6e 64 20 63 
  000000b0  6c 6f 73 75 72 65 73 0a  00 00 00 00 00 00 00 00 
  000000c0  f0 9f a7 aa 20 57 68 61  74 20 74 6f 20 6c 6f 6f 
  000000d0  6b 20 66 6f 72 3a 20 6c  61 62 65 6c 65 64 20 6f 
  000000e0  75 74 70 75 74 73 20 62  65 6c 6f 77 0a 00 00 00 
  000000f0  e2 9c 85 20 45 78 70 65  63 74 61 74 69 6f 6e 3a 
  00000100  20 6f 75 74 70 75 74 73  20 6d 61 74 63 68 20 6c 
  00000110  61 62 65 6c 73 0a 00 00  0a 00 00 00 00 00 00 00 
  00000120  47 65 6e 65 72 69 63 20  6f 70 65 72 61 74 69 6f 
  00000130  6e 73 3a 0a 00 00 00 00  0a 43 6f 6e 64 69 74 69 
  00000140  6f 6e 61 6c 3a 0a 00 00  61 70 70 6c 79 5f 69 66 
  00000150  28 74 72 75 65 2c 20 35  2c 20 33 29 20 3d 20 25 
  00000160  6c 6c 64 0a 00 00 00 00  61 70 70 6c 79 5f 69 66 
  00000170  28 66 61 6c 73 65 2c 20  35 2c 20 33 29 20 3d 20 
  00000180  25 6c 6c 64 0a 00 00 00  0a 43 6c 6f 73 75 72 65 
  00000190  20 66 61 63 74 6f 72 79  3a 0a 00 00 00 00 00 00 
  000001a0  61 64 64 5f 31 30 28 35  29 20 3d 20 25 6c 6c 64 
  000001b0  0a 00 00 00 00 00 00 00  64 6f 75 62 6c 65 28 37 
  000001c0  29 20 3d 20 25 6c 6c 64  0a 00 
