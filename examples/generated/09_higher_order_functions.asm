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
fn apply_if
  bb0 bb0
    alloca Virtual { id: 7, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 8, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 8, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    load Virtual { id: 10, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 8, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 11, bank: General, size_bits: 8 }, Virtual { id: 10, bank: General, size_bits: 8 }, 1
    condbr
  bb1 bb1
    call local.4(local.2, local.3) cc=C tail=false
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 7, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 12, bank: General, size_bits: 64 }
    br
  bb2 bb2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 7, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    br
  bb4 bb4
    br
  bb3 bb3
    load Virtual { id: 15, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 7, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn apply_f64
  bb0 bb0
    call local.3(local.1, local.2) cc=C tail=false
    br
  bb1 bb1
    intrinsic.call symbol(intrinsic.println), symbol(local.1), symbol(local.2), Virtual { id: 16, bank: Float, size_bits: 64 }
    ret
fn apply_i64
  bb0 bb0
    call local.3(local.1, local.2) cc=C tail=false
    br
  bb1 bb1
    intrinsic.call symbol(intrinsic.println), symbol(local.1), symbol(local.2), Virtual { id: 18, bank: General, size_bits: 64 }
    ret
fn make_adder
  bb0 bb0
    alloca Virtual { id: 20, bank: General, size_bits: 64 }, 8
    insertvalue Virtual { id: 21, bank: General, size_bits: 64 }, 0, symbol(local.1), 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 20, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 21, bank: General, size_bits: 64 }
    load Virtual { id: 23, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 20, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn add_f64
  bb0 bb0
    alloca Virtual { id: 24, bank: General, size_bits: 64 }, 8
    add Virtual { id: 25, bank: Float, size_bits: 64 }, symbol(local.1), symbol(local.2)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 24, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 25, bank: Float, size_bits: 64 }
    load Virtual { id: 27, bank: Float, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 24, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
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
    call symbol(apply_if)(true, 5, 3, symbol(add_i64)) cc=C tail=false
    br
  bb3 bb3
    intrinsic.call symbol(intrinsic.println), Virtual { id: 49, bank: General, size_bits: 64 }
    call symbol(apply_if)(false, 5, 3, symbol(add_i64)) cc=C tail=false
    br
  bb4 bb4
    intrinsic.call symbol(intrinsic.println), Virtual { id: 51, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    call symbol(make_adder)(10) cc=C tail=false
    alloca Virtual { id: 55, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 55, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 54, bank: General, size_bits: 64 }
    br
  bb5 bb5
    load Virtual { id: 57, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 55, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(__closure09_higher_order_functions_0_call)(v57, 5) cc=C tail=false
    br
  bb6 bb6
    intrinsic.call symbol(intrinsic.println), Virtual { id: 58, bank: General, size_bits: 64 }
    alloca Virtual { id: 60, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 60, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    load Virtual { id: 62, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 60, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(__closure09_higher_order_functions_1_call)(v62, 7) cc=C tail=false
    br
  bb7 bb7
    intrinsic.call symbol(intrinsic.println), Virtual { id: 63, bank: General, size_bits: 64 }
    ret


Symbols:
  add_i64                          0x00000000
  apply_if                         0x00000060
  apply_f64                        0x00000128
  apply_i64                        0x000001a0
  make_adder                       0x0000021c
  add_f64                          0x00000270
  __closure09_higher_order_functions_0_call 0x000002d0
  __closure09_higher_order_functions_1_call 0x0000035c
  main                             0x000003bc

Text relocations:
  offset=0x0000015c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000188 kind=CallRel32 symbol=printf addend=0
  offset=0x000001d4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000204 kind=CallRel32 symbol=printf addend=0
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
  offset=0x000004a0 kind=Aarch64AdrpAdd symbol=add_i64 addend=0
  offset=0x000004b4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000004cc kind=CallRel32 symbol=printf addend=0
  offset=0x000004dc kind=Aarch64AdrpAdd symbol=add_i64 addend=0
  offset=0x000004f0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000508 kind=CallRel32 symbol=printf addend=0
  offset=0x0000050c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000518 kind=CallRel32 symbol=printf addend=0
  offset=0x00000564 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000057c kind=CallRel32 symbol=printf addend=0
  offset=0x000005c4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000005dc kind=CallRel32 symbol=printf addend=0

.text (1532 bytes):
  00000000  ff 43 06 d1 fd 7b 18 a9  fd 03 00 91 e0 97 00 f9 
  00000010  e1 9b 00 f9 1f 20 03 d5  f0 03 00 91 10 e2 04 91 
  00000020  f0 03 00 f9 f0 97 40 f9  f1 9b 40 f9 10 02 11 8b 
  00000030  f0 07 00 f9 f1 03 40 f9  f0 07 40 f9 30 02 00 f9 
  00000040  f0 03 40 f9 11 02 40 f9  f1 0f 00 f9 e0 0f 40 f9 
  00000050  bf 03 00 91 fd 7b 58 a9  ff 43 06 91 c0 03 5f d6 
  00000060  ff 03 07 d1 fd 7b 1b a9  fd 03 00 91 e0 e3 04 39 
  00000070  e1 a3 00 f9 e2 a7 00 f9  e3 ab 00 f9 1f 20 03 d5 
  00000080  f0 03 00 91 10 82 05 91  f0 0f 00 f9 f0 03 00 91 
  00000090  10 82 06 91 f0 13 00 f9  f1 13 40 f9 f0 e3 44 39 
  000000a0  30 02 00 39 f0 13 40 f9  11 02 40 39 f1 1b 00 f9 
  000000b0  f0 c3 40 39 1f 06 00 f1  f0 17 9f 9a f0 1f 00 f9 
  000000c0  f0 1f 40 f9 1f 02 00 f1  41 00 00 54 0a 00 00 14 
  000000d0  e0 a3 40 f9 e1 a7 40 f9  f0 ab 40 f9 00 02 3f d6 
  000000e0  e0 23 00 f9 f1 0f 40 f9  f0 23 40 f9 30 02 00 f9 
  000000f0  05 00 00 14 f1 0f 40 f9  10 00 80 d2 30 02 00 f9 
  00000100  02 00 00 14 01 00 00 14  f0 0f 40 f9 11 02 40 f9 
  00000110  f1 2f 00 f9 e0 2f 40 f9  bf 03 00 91 fd 7b 5b a9 
  00000120  ff 03 07 91 c0 03 5f d6  ff 03 06 d1 fd 7b 17 a9 
  00000130  fd 03 00 91 e0 a7 00 fd  e1 ab 00 fd e0 af 00 f9 
  00000140  1f 20 03 d5 e0 a7 40 fd  e1 ab 40 fd f0 af 40 f9 
  00000150  00 02 3f d6 e0 37 00 fd  01 00 00 14 00 00 00 90 
  00000160  00 00 00 91 e0 a7 40 fd  e0 a7 40 fd e0 03 00 fd 
  00000170  e1 ab 40 fd e0 ab 40 fd  e0 07 00 fd e2 37 40 fd 
  00000180  e0 37 40 fd e0 0b 00 fd  00 00 00 94 bf 03 00 91 
  00000190  fd 7b 57 a9 ff 03 06 91  00 00 80 d2 c0 03 5f d6 
  000001a0  ff 03 06 d1 fd 7b 17 a9  fd 03 00 91 e0 a7 00 f9 
  000001b0  e1 ab 00 f9 e2 af 00 f9  1f 20 03 d5 e0 a7 40 f9 
  000001c0  e1 ab 40 f9 f0 af 40 f9  00 02 3f d6 e0 3b 00 f9 
  000001d0  01 00 00 14 00 00 00 90  00 00 00 91 00 60 00 91 
  000001e0  e1 a7 40 f9 f0 a7 40 f9  f0 03 00 f9 e2 ab 40 f9 
  000001f0  f0 ab 40 f9 f0 07 00 f9  e3 3b 40 f9 f0 3b 40 f9 
  00000200  f0 0b 00 f9 00 00 00 94  bf 03 00 91 fd 7b 57 a9 
  00000210  ff 03 06 91 00 00 80 d2  c0 03 5f d6 ff 03 06 d1 
  00000220  fd 7b 17 a9 fd 03 00 91  e0 97 00 f9 1f 20 03 d5 
  00000230  f0 03 00 91 10 c2 04 91  f0 2f 00 f9 f0 97 40 f9 
  00000240  f0 33 00 f9 f1 2f 40 f9  f0 33 40 f9 30 02 00 f9 
  00000250  f0 2f 40 f9 11 02 40 f9  f1 3b 00 f9 e0 3b 40 f9 
  00000260  bf 03 00 91 fd 7b 57 a9  ff 03 06 91 c0 03 5f d6 
  00000270  ff 43 06 d1 fd 7b 18 a9  fd 03 00 91 e0 97 00 fd 
  00000280  e1 9b 00 fd 1f 20 03 d5  f0 03 00 91 10 e2 04 91 
  00000290  f0 3b 00 f9 e0 97 40 fd  e1 9b 40 fd 00 28 61 1e 
  000002a0  e0 3f 00 fd f1 3b 40 f9  e0 3f 40 fd 20 02 00 fd 
  000002b0  f0 3b 40 f9 00 02 40 fd  e0 47 00 fd e0 47 40 fd 
  000002c0  bf 03 00 91 fd 7b 58 a9  ff 43 06 91 c0 03 5f d6 
  000002d0  ff 43 07 d1 fd 7b 1c a9  fd 03 00 91 e0 9b 00 f9 
  000002e0  e1 9f 00 f9 1f 20 03 d5  f0 03 00 91 10 02 05 91 
  000002f0  f0 47 00 f9 f0 03 00 91  10 02 06 91 f0 4b 00 f9 
  00000300  f1 4b 40 f9 f0 9b 40 f9  30 02 00 f9 f0 4b 40 f9 
  00000310  f0 53 00 f9 f0 53 40 f9  11 02 40 f9 f1 57 00 f9 
  00000320  f0 9f 40 f9 f1 57 40 f9  10 02 11 8b f0 5b 00 f9 
  00000330  f1 47 40 f9 f0 5b 40 f9  30 02 00 f9 f0 47 40 f9 
  00000340  11 02 40 f9 f1 63 00 f9  e0 63 40 f9 bf 03 00 91 
  00000350  fd 7b 5c a9 ff 43 07 91  c0 03 5f d6 ff 43 06 d1 
  00000360  fd 7b 18 a9 fd 03 00 91  e0 a3 04 39 e1 9b 00 f9 
  00000370  1f 20 03 d5 f0 03 00 91  10 e2 04 91 f0 5f 00 f9 
  00000380  f0 9b 40 f9 51 00 80 d2  10 7e 11 9b f0 63 00 f9 
  00000390  f1 5f 40 f9 f0 63 40 f9  30 02 00 f9 f0 5f 40 f9 
  000003a0  11 02 40 f9 f1 6b 00 f9  e0 6b 40 f9 bf 03 00 91 
  000003b0  fd 7b 58 a9 ff 43 06 91  c0 03 5f d6 ff c3 0a d1 
  000003c0  f0 03 00 91 10 82 0a 91  1d 7a 00 a9 fd 03 00 91 
  000003d0  1f 20 03 d5 00 00 00 90  00 00 00 91 00 e0 00 91 
  000003e0  00 00 00 94 00 00 00 90  00 00 00 91 00 a0 01 91 
  000003f0  00 00 00 94 00 00 00 90  00 00 00 91 00 00 03 91 
  00000400  00 00 00 94 00 00 00 90  00 00 00 91 00 c0 03 91 
  00000410  00 00 00 94 00 00 00 90  00 00 00 91 00 60 04 91 
  00000420  00 00 00 94 00 00 00 90  00 00 00 91 00 80 04 91 
  00000430  00 00 00 94 40 01 80 d2  81 02 80 d2 02 00 00 90 
  00000440  42 00 00 91 57 ff ff 97  01 00 00 14 10 00 80 d2 
  00000450  10 00 a0 f2 10 00 c0 f2  10 ff e7 f2 00 02 67 9e 
  00000460  10 00 80 d2 10 00 a0 f2  10 00 c0 f2 90 00 e8 f2 
  00000470  01 02 67 9e 00 00 00 90  00 00 00 91 2b ff ff 97 
  00000480  01 00 00 14 00 00 00 90  00 00 00 91 00 e0 04 91 
  00000490  00 00 00 94 20 00 80 d2  a1 00 80 d2 62 00 80 d2 
  000004a0  03 00 00 90 63 00 00 91  ee fe ff 97 e0 97 00 f9 
  000004b0  01 00 00 14 00 00 00 90  00 00 00 91 00 20 05 91 
  000004c0  e1 97 40 f9 f0 97 40 f9  f0 03 00 f9 00 00 00 94 
  000004d0  00 00 80 d2 a1 00 80 d2  62 00 80 d2 03 00 00 90 
  000004e0  63 00 00 91 df fe ff 97  e0 9f 00 f9 01 00 00 14 
  000004f0  00 00 00 90 00 00 00 91  00 a0 05 91 e1 9f 40 f9 
  00000500  f0 9f 40 f9 f0 03 00 f9  00 00 00 94 00 00 00 90 
  00000510  00 00 00 91 00 20 06 91  00 00 00 94 40 01 80 d2 
  00000520  3f ff ff 97 e0 ab 00 f9  f0 03 00 91 10 42 09 91 
  00000530  f0 af 00 f9 f1 af 40 f9  f0 ab 40 f9 30 02 00 f9 
  00000540  01 00 00 14 f0 af 40 f9  11 02 40 f9 f1 b7 00 f9 
  00000550  e0 b7 40 f9 a1 00 80 d2  5e ff ff 97 e0 bb 00 f9 
  00000560  01 00 00 14 00 00 00 90  00 00 00 91 00 80 06 91 
  00000570  e1 bb 40 f9 f0 bb 40 f9  f0 03 00 f9 00 00 00 94 
  00000580  f0 03 00 91 10 42 0a 91  f0 c3 00 f9 10 00 80 d2 
  00000590  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 f1 c3 40 f9 
  000005a0  30 02 00 f9 f0 c3 40 f9  11 02 40 f9 f1 cb 00 f9 
  000005b0  e0 cb 40 f9 e1 00 80 d2  69 ff ff 97 e0 cf 00 f9 
  000005c0  01 00 00 14 00 00 00 90  00 00 00 91 00 e0 06 91 
  000005d0  e1 cf 40 f9 f0 cf 40 f9  f0 03 00 f9 00 00 00 94 
  000005e0  bf 03 00 91 f0 03 00 91  10 82 0a 91 1d 7a 40 a9 
  000005f0  ff c3 0a 91 00 00 80 d2  c0 03 5f d6 

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
