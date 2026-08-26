fp-native dump: format=MachO arch=Aarch64 entry=0x234

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
fn count_fields_plus_n
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 1, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 2, bank: General, size_bits: 64 }, 2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 2, bank: General, size_bits: 64 }
    alloca Virtual { id: 4, bank: General, size_bits: 64 }, 1
    load Virtual { id: 5, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 4, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 5, bank: General, size_bits: 64 }
    alloca Virtual { id: 7, bank: General, size_bits: 64 }, 1
    sextortrunc Virtual { id: 8, bank: General, size_bits: 64 }, symbol(local.2)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 7, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 8, bank: General, size_bits: 64 }
    load Virtual { id: 10, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 4, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 11, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 7, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 12, bank: General, size_bits: 64 }, Virtual { id: 10, bank: General, size_bits: 64 }, Virtual { id: 11, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 12, bank: General, size_bits: 64 }
    load Virtual { id: 14, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn count_fields_plus_1
  bb0 bb0
    alloca Virtual { id: 15, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 16, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 16, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    load Virtual { id: 18, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 16, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(count_fields_plus_n)(v18, 1) cc=C tail=false
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 15, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 19, bank: General, size_bits: 64 }
    br
  bb1 bb1
    load Virtual { id: 21, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 15, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn const_count_fields_plus_1
  bb0 bb0
    alloca Virtual { id: 22, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 23, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 24, bank: General, size_bits: 64 }, 2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 23, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 24, bank: General, size_bits: 64 }
    alloca Virtual { id: 26, bank: General, size_bits: 64 }, 1
    load Virtual { id: 27, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 23, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 26, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 27, bank: General, size_bits: 64 }
    alloca Virtual { id: 29, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 30, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 29, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 30, bank: General, size_bits: 64 }
    load Virtual { id: 32, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 26, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 33, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 29, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 34, bank: General, size_bits: 64 }, Virtual { id: 32, bank: General, size_bits: 64 }, Virtual { id: 33, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 22, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 34, bank: General, size_bits: 64 }
    load Virtual { id: 36, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 22, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn main
  bb0 bb0
    alloca Virtual { id: 37, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 37, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    alloca Virtual { id: 39, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 39, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 37, bank: General, size_bits: 64 }
    load Virtual { id: 41, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 39, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(count_fields_plus_n)(v41, 3) cc=C tail=false
    br
  bb1 bb1
    alloca Virtual { id: 43, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 43, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 37, bank: General, size_bits: 64 }
    load Virtual { id: 45, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 43, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(count_fields_plus_1)(v45) cc=C tail=false
    br
  bb2 bb2
    call symbol(const_count_fields_plus_1)() cc=C tail=false
    br
  bb3 bb3
    alloca Virtual { id: 48, bank: General, size_bits: 64 }, 1
    eq Virtual { id: 49, bank: General, size_bits: 8 }, Virtual { id: 42, bank: General, size_bits: 64 }, 5
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 48, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 49, bank: General, size_bits: 64 }
    alloca Virtual { id: 51, bank: General, size_bits: 64 }, 1
    load Virtual { id: 52, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 48, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    not Virtual { id: 53, bank: General, size_bits: 8 }, Virtual { id: 52, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 51, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 53, bank: General, size_bits: 64 }
    load Virtual { id: 55, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 51, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 56, bank: General, size_bits: 8 }, Virtual { id: 55, bank: General, size_bits: 64 }, 1
    condbr
  bb4 bb4
    call symbol(fp_panic)(symbol(__const_data_0)) cc=C tail=false
    br
  bb5 bb5
    br
  bb7 bb7
    unreachable
  bb6 bb6
    alloca Virtual { id: 58, bank: General, size_bits: 64 }, 1
    eq Virtual { id: 59, bank: General, size_bits: 8 }, Virtual { id: 46, bank: General, size_bits: 64 }, 3
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 58, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 59, bank: General, size_bits: 64 }
    alloca Virtual { id: 61, bank: General, size_bits: 64 }, 1
    load Virtual { id: 62, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 58, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    not Virtual { id: 63, bank: General, size_bits: 8 }, Virtual { id: 62, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 61, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 63, bank: General, size_bits: 64 }
    load Virtual { id: 65, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 61, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 66, bank: General, size_bits: 8 }, Virtual { id: 65, bank: General, size_bits: 64 }, 1
    condbr
  bb9 bb9
    call symbol(fp_panic)(symbol(__const_data_0)) cc=C tail=false
    br
  bb10 bb10
    br
  bb12 bb12
    unreachable
  bb11 bb11
    alloca Virtual { id: 68, bank: General, size_bits: 64 }, 1
    eq Virtual { id: 69, bank: General, size_bits: 8 }, Virtual { id: 47, bank: General, size_bits: 64 }, 3
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 68, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 69, bank: General, size_bits: 64 }
    alloca Virtual { id: 71, bank: General, size_bits: 64 }, 1
    load Virtual { id: 72, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 68, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    not Virtual { id: 73, bank: General, size_bits: 8 }, Virtual { id: 72, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 71, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 73, bank: General, size_bits: 64 }
    load Virtual { id: 75, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 71, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 76, bank: General, size_bits: 8 }, Virtual { id: 75, bank: General, size_bits: 64 }, 1
    condbr
  bb14 bb14
    call symbol(fp_panic)(symbol(__const_data_0)) cc=C tail=false
    br
  bb15 bb15
    br
  bb17 bb17
    unreachable
  bb16 bb16
    ret
  bb8 bb8
    br
  bb13 bb13
    br
  bb18 bb18
    br


Symbols:
  count_fields_plus_n              0x00000000
  count_fields_plus_1              0x000000e4
  const_count_fields_plus_1        0x00000164
  main                             0x00000234
  fp_panic                         0x00000518

Text relocations:
  offset=0x00000390 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000434 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000004d8 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000518 kind=CallRel32 symbol=abort addend=0

.text (1312 bytes):
  00000000  ff 83 03 d1 fd 7b 0d a9  fd 03 00 91 e0 43 00 f9 
  00000010  e1 8b 00 b9 f0 03 00 91  10 a2 02 91 f0 03 00 f9 
  00000020  f0 03 00 91 10 c2 02 91  f0 07 00 f9 50 00 80 d2 
  00000030  f0 0b 00 f9 f1 07 40 f9  f0 0b 40 f9 30 02 00 f9 
  00000040  f0 03 00 91 10 e2 02 91  f0 13 00 f9 f0 07 40 f9 
  00000050  11 02 40 f9 f1 17 00 f9  f1 13 40 f9 f0 17 40 f9 
  00000060  30 02 00 f9 f0 03 00 91  10 02 03 91 f0 1f 00 f9 
  00000070  f0 8b 80 b9 11 04 80 d2  10 22 d1 1a 10 2a d1 1a 
  00000080  f0 23 00 f9 f1 1f 40 f9  f0 23 40 f9 30 02 00 f9 
  00000090  f0 13 40 f9 11 02 40 f9  f1 2b 00 f9 f0 1f 40 f9 
  000000a0  11 02 40 f9 f1 2f 00 f9  f0 2b 40 f9 f1 2f 40 f9 
  000000b0  10 02 11 8b f0 33 00 f9  f1 03 40 f9 f0 33 40 f9 
  000000c0  30 02 00 f9 f0 03 40 f9  11 02 40 f9 f1 3b 00 f9 
  000000d0  e0 3b 40 f9 bf 03 00 91  fd 7b 4d a9 ff 83 03 91 
  000000e0  c0 03 5f d6 ff c3 01 d1  fd 7b 06 a9 fd 03 00 91 
  000000f0  e0 23 00 f9 f0 03 00 91  10 42 01 91 f0 03 00 f9 
  00000100  f0 03 00 91 10 62 01 91  f0 07 00 f9 f1 07 40 f9 
  00000110  f0 23 40 f9 30 02 00 f9  f0 07 40 f9 11 02 40 f9 
  00000120  f1 0f 00 f9 e0 0f 40 f9  21 00 80 d2 b5 ff ff 97 
  00000130  e0 13 00 f9 f1 03 40 f9  f0 13 40 f9 30 02 00 f9 
  00000140  01 00 00 14 f0 03 40 f9  11 02 40 f9 f1 1b 00 f9 
  00000150  e0 1b 40 f9 bf 03 00 91  fd 7b 46 a9 ff c3 01 91 
  00000160  c0 03 5f d6 ff 43 03 d1  fd 7b 0c a9 fd 03 00 91 
  00000170  f0 03 00 91 10 62 02 91  f0 03 00 f9 f0 03 00 91 
  00000180  10 82 02 91 f0 07 00 f9  50 00 80 d2 f0 0b 00 f9 
  00000190  f1 07 40 f9 f0 0b 40 f9  30 02 00 f9 f0 03 00 91 
  000001a0  10 a2 02 91 f0 13 00 f9  f0 07 40 f9 11 02 40 f9 
  000001b0  f1 17 00 f9 f1 13 40 f9  f0 17 40 f9 30 02 00 f9 
  000001c0  f0 03 00 91 10 c2 02 91  f0 1f 00 f9 30 00 80 d2 
  000001d0  f0 23 00 f9 f1 1f 40 f9  f0 23 40 f9 30 02 00 f9 
  000001e0  f0 13 40 f9 11 02 40 f9  f1 2b 00 f9 f0 1f 40 f9 
  000001f0  11 02 40 f9 f1 2f 00 f9  f0 2b 40 f9 f1 2f 40 f9 
  00000200  10 02 11 8b f0 33 00 f9  f1 03 40 f9 f0 33 40 f9 
  00000210  30 02 00 f9 f0 03 40 f9  11 02 40 f9 f1 3b 00 f9 
  00000220  e0 3b 40 f9 bf 03 00 91  fd 7b 4c a9 ff 43 03 91 
  00000230  c0 03 5f d6 ff c3 08 d1  f0 03 00 91 10 82 08 91 
  00000240  1d 7a 00 a9 fd 03 00 91  f0 03 00 91 10 42 07 91 
  00000250  f0 03 00 f9 f1 03 40 f9  50 01 80 d2 10 00 a0 f2 
  00000260  10 00 c0 f2 10 00 e0 f2  e9 03 11 aa 30 01 00 b9 
  00000270  10 00 80 d2 10 00 a0 f2  10 00 c0 f2 90 06 e8 f2 
  00000280  e9 03 11 aa 29 21 00 91  30 01 00 f9 f0 03 00 91 
  00000290  10 82 07 91 f0 0b 00 f9  f1 0b 40 f9 f0 03 40 f9 
  000002a0  30 02 00 f9 f0 0b 40 f9  11 02 40 f9 f1 13 00 f9 
  000002b0  e0 13 40 f9 61 00 80 d2  52 ff ff 97 e0 17 00 f9 
  000002c0  01 00 00 14 f0 03 00 91  10 a2 07 91 f0 1b 00 f9 
  000002d0  f1 1b 40 f9 f0 03 40 f9  30 02 00 f9 f0 1b 40 f9 
  000002e0  11 02 40 f9 f1 23 00 f9  e0 23 40 f9 7e ff ff 97 
  000002f0  e0 27 00 f9 01 00 00 14  9b ff ff 97 e0 2b 00 f9 
  00000300  01 00 00 14 f0 03 00 91  10 c2 07 91 f0 2f 00 f9 
  00000310  f0 17 40 f9 1f 16 00 f1  f0 17 9f 9a f0 33 00 f9 
  00000320  f1 2f 40 f9 f0 83 41 39  30 02 00 39 f0 03 00 91 
  00000330  10 e2 07 91 f0 3b 00 f9  f0 2f 40 f9 11 02 40 39 
  00000340  f1 3f 00 f9 f0 e3 41 39  11 00 80 d2 31 06 00 d1 
  00000350  30 02 10 cb f0 43 00 f9  f1 3b 40 f9 f0 03 42 39 
  00000360  30 02 00 39 f0 3b 40 f9  11 02 40 39 f1 4b 00 f9 
  00000370  f0 43 42 39 1f 06 00 f1  f0 17 9f 9a f0 4f 00 f9 
  00000380  f0 4f 40 f9 1f 02 00 f1  41 00 00 54 05 00 00 14 
  00000390  00 00 00 90 00 00 00 91  60 00 00 94 02 00 00 14 
  000003a0  02 00 00 14 00 00 20 d4  f0 03 00 91 10 02 08 91 
  000003b0  f0 57 00 f9 f0 27 40 f9  1f 0e 00 f1 f0 17 9f 9a 
  000003c0  f0 5b 00 f9 f1 57 40 f9  f0 c3 42 39 30 02 00 39 
  000003d0  f0 03 00 91 10 22 08 91  f0 63 00 f9 f0 57 40 f9 
  000003e0  11 02 40 39 f1 67 00 f9  f0 23 43 39 11 00 80 d2 
  000003f0  31 06 00 d1 30 02 10 cb  f0 6b 00 f9 f1 63 40 f9 
  00000400  f0 43 43 39 30 02 00 39  f0 63 40 f9 11 02 40 39 
  00000410  f1 73 00 f9 f0 83 43 39  1f 06 00 f1 f0 17 9f 9a 
  00000420  f0 77 00 f9 f0 77 40 f9  1f 02 00 f1 41 00 00 54 
  00000430  05 00 00 14 00 00 00 90  00 00 00 91 37 00 00 94 
  00000440  02 00 00 14 02 00 00 14  00 00 20 d4 f0 03 00 91 
  00000450  10 42 08 91 f0 7f 00 f9  f0 2b 40 f9 1f 0e 00 f1 
  00000460  f0 17 9f 9a f0 83 00 f9  f1 7f 40 f9 f0 03 44 39 
  00000470  30 02 00 39 f0 03 00 91  10 62 08 91 f0 8b 00 f9 
  00000480  f0 7f 40 f9 11 02 40 39  f1 8f 00 f9 f0 63 44 39 
  00000490  11 00 80 d2 31 06 00 d1  30 02 10 cb f0 93 00 f9 
  000004a0  f1 8b 40 f9 f0 83 44 39  30 02 00 39 f0 8b 40 f9 
  000004b0  11 02 40 39 f1 9b 00 f9  f0 c3 44 39 1f 06 00 f1 
  000004c0  f0 17 9f 9a f0 9f 00 f9  f0 9f 40 f9 1f 02 00 f1 
  000004d0  41 00 00 54 05 00 00 14  00 00 00 90 00 00 00 91 
  000004e0  0e 00 00 94 02 00 00 14  02 00 00 14 00 00 20 d4 
  000004f0  bf 03 00 91 f0 03 00 91  10 82 08 91 1d 7a 40 a9 
  00000500  ff c3 08 91 00 00 80 d2  c0 03 5f d6 a7 ff ff 17 
  00000510  cf ff ff 17 f7 ff ff 17  00 00 00 94 c0 03 5f d6 

.rodata (28 bytes):
  00000000  61 73 73 65 72 74 69 6f  6e 20 66 61 69 6c 65 64 
  00000010  00 00 00 00 01 00 00 00  01 00 00 00 
