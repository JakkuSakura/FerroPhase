fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global CODE ty=I64 constant=true initializer=Some(Bytes([2, 0, 0, 0, 0, 0, 0, 0]))
global __const_data__15_enums_Shape__describe_g0_0 ty=Array(I8, 6) constant=true initializer=Some(Bytes([112, 111, 105, 110, 116, 0]))
global __const_data__15_enums_Shape__describe_g0_1 ty=Array(I8, 7) constant=true initializer=Some(Bytes([99, 105, 114, 99, 108, 101, 0]))
global __const_data__15_enums_Shape__describe_g0_2 ty=Array(I8, 10) constant=true initializer=Some(Bytes([114, 101, 99, 116, 97, 110, 103, 108, 101, 0]))
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    alloca Virtual { id: 6, bank: General, size_bits: 64 }, 24
    insertvalue Virtual { id: 7, bank: General, size_bits: 64 }, 0, 0, 0
    insertvalue Virtual { id: 8, bank: General, size_bits: 64 }, Virtual { id: 7, bank: General, size_bits: 64 }, 0, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 6, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 8, bank: General, size_bits: 64 }
    alloca Virtual { id: 10, bank: General, size_bits: 64 }, 24
    load Virtual { id: 11, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 6, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(24), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 10, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 11, bank: General, size_bits: 64 }
    alloca Virtual { id: 13, bank: General, size_bits: 64 }, 16
    bitcast Virtual { id: 14, bank: General, size_bits: 64 }, Virtual { id: 13, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 14, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 10
    alloca Virtual { id: 16, bank: General, size_bits: 64 }, 24
    load Virtual { id: 17, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 13, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    insertvalue Virtual { id: 18, bank: General, size_bits: 64 }, 0, 1, 0
    insertvalue Virtual { id: 19, bank: General, size_bits: 64 }, Virtual { id: 18, bank: General, size_bits: 64 }, Virtual { id: 17, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 16, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 19, bank: General, size_bits: 64 }
    alloca Virtual { id: 21, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    alloca Virtual { id: 23, bank: General, size_bits: 64 }, 16
    bitcast Virtual { id: 24, bank: General, size_bits: 64 }, Virtual { id: 23, bank: General, size_bits: 64 }
    load Virtual { id: 25, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 24, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 25, bank: General, size_bits: 64 }
    alloca Virtual { id: 27, bank: General, size_bits: 64 }, 24
    load Virtual { id: 28, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 23, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    insertvalue Virtual { id: 29, bank: General, size_bits: 64 }, 0, 2, 0
    insertvalue Virtual { id: 30, bank: General, size_bits: 64 }, Virtual { id: 29, bank: General, size_bits: 64 }, Virtual { id: 28, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 27, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 30, bank: General, size_bits: 64 }
    alloca Virtual { id: 32, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 32, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 10, bank: General, size_bits: 64 }
    load Virtual { id: 34, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 32, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(Shape__describe)(v34) cc=C tail=false
    alloca Virtual { id: 36, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 35, bank: General, size_bits: 64 }
    br
  bb1 bb1
    bitcast Virtual { id: 38, bank: General, size_bits: 64 }, Virtual { id: 36, bank: General, size_bits: 64 }
    load Virtual { id: 39, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 38, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 39, bank: General, size_bits: 64 }
    alloca Virtual { id: 41, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 16, bank: General, size_bits: 64 }
    load Virtual { id: 43, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(Shape__describe)(v43) cc=C tail=false
    alloca Virtual { id: 45, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 45, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 44, bank: General, size_bits: 64 }
    br
  bb2 bb2
    bitcast Virtual { id: 47, bank: General, size_bits: 64 }, Virtual { id: 45, bank: General, size_bits: 64 }
    load Virtual { id: 48, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 47, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 48, bank: General, size_bits: 64 }
    alloca Virtual { id: 50, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 50, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 27, bank: General, size_bits: 64 }
    load Virtual { id: 52, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 50, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(Shape__describe)(v52) cc=C tail=false
    alloca Virtual { id: 54, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 54, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 53, bank: General, size_bits: 64 }
    br
  bb3 bb3
    bitcast Virtual { id: 56, bank: General, size_bits: 64 }, Virtual { id: 54, bank: General, size_bits: 64 }
    load Virtual { id: 57, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 56, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 57, bank: General, size_bits: 64 }
    alloca Virtual { id: 59, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 59, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    load Virtual { id: 61, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 59, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(value_code)(v61) cc=C tail=false
    br
  bb4 bb4
    intrinsic.call symbol(intrinsic.println), Virtual { id: 62, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println), 2
    ret
fn Shape__describe
  bb0 bb0
    alloca Virtual { id: 65, bank: General, size_bits: 64 }, 16
    alloca Virtual { id: 66, bank: General, size_bits: 64 }, 24
    load Virtual { id: 67, bank: General, size_bits: 64 }, symbol(frame.local.1)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 66, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 67, bank: General, size_bits: 64 }
    alloca Virtual { id: 69, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 70, bank: General, size_bits: 64 }, Virtual { id: 66, bank: General, size_bits: 64 }
    load Virtual { id: 71, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 70, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 72, bank: General, size_bits: 8 }, Virtual { id: 71, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 69, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 72, bank: General, size_bits: 8 }
    load Virtual { id: 74, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 69, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 75, bank: General, size_bits: 8 }, Virtual { id: 74, bank: General, size_bits: 8 }, 1
    condbr
  bb2 bb2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb3 bb3
    alloca Virtual { id: 77, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 78, bank: General, size_bits: 64 }, Virtual { id: 66, bank: General, size_bits: 64 }
    load Virtual { id: 79, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 78, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 80, bank: General, size_bits: 8 }, Virtual { id: 79, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 77, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 80, bank: General, size_bits: 8 }
    load Virtual { id: 82, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 77, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 83, bank: General, size_bits: 8 }, Virtual { id: 82, bank: General, size_bits: 8 }, 1
    condbr
  bb1 bb1
    load Virtual { id: 84, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb4 bb4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb5 bb5
    br
  bb6 bb6
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb7 bb7
    load Virtual { id: 87, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn value_code
  bb0 bb0
    alloca Virtual { id: 88, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 89, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 89, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    alloca Virtual { id: 91, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 92, bank: General, size_bits: 64 }, Virtual { id: 89, bank: General, size_bits: 64 }
    load Virtual { id: 93, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 94, bank: General, size_bits: 8 }, Virtual { id: 93, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 91, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 94, bank: General, size_bits: 8 }
    load Virtual { id: 96, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 91, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 97, bank: General, size_bits: 8 }, Virtual { id: 96, bank: General, size_bits: 8 }, 1
    condbr
  bb2 bb2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 88, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    br
  bb3 bb3
    alloca Virtual { id: 99, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 100, bank: General, size_bits: 64 }, Virtual { id: 89, bank: General, size_bits: 64 }
    load Virtual { id: 101, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 100, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 102, bank: General, size_bits: 8 }, Virtual { id: 101, bank: General, size_bits: 64 }, 2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 99, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 102, bank: General, size_bits: 8 }
    load Virtual { id: 104, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 99, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 105, bank: General, size_bits: 8 }, Virtual { id: 104, bank: General, size_bits: 8 }, 1
    condbr
  bb1 bb1
    load Virtual { id: 106, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 88, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb4 bb4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 88, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 2
    br
  bb5 bb5
    alloca Virtual { id: 108, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 109, bank: General, size_bits: 64 }, Virtual { id: 89, bank: General, size_bits: 64 }
    load Virtual { id: 110, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 109, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 111, bank: General, size_bits: 8 }, Virtual { id: 110, bank: General, size_bits: 64 }, 5
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 108, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 111, bank: General, size_bits: 8 }
    load Virtual { id: 113, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 108, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 114, bank: General, size_bits: 8 }, Virtual { id: 113, bank: General, size_bits: 8 }, 1
    condbr
  bb6 bb6
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 88, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 5
    br
  bb7 bb7
    br


Symbols:
  main                             0x00000000
  Shape__describe                  0x00000680
  value_code                       0x00000968

Text relocations:
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
  offset=0x00000460 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000478 kind=CallRel32 symbol=printf addend=0
  offset=0x00000508 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000520 kind=CallRel32 symbol=printf addend=0
  offset=0x000005b0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000005c8 kind=CallRel32 symbol=printf addend=0
  offset=0x00000610 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000628 kind=CallRel32 symbol=printf addend=0
  offset=0x0000062c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000644 kind=CallRel32 symbol=printf addend=0
  offset=0x00000794 kind=Aarch64AdrpAdd symbol=__const_data__15_enums_Shape__describe_g0_0 addend=0
  offset=0x00000898 kind=Aarch64AdrpAdd symbol=__const_data__15_enums_Shape__describe_g0_1 addend=0
  offset=0x000008d4 kind=Aarch64AdrpAdd symbol=__const_data__15_enums_Shape__describe_g0_2 addend=0

.text (2876 bytes):
  00000000  f0 03 00 91 11 b6 82 d2  11 00 a0 f2 11 00 c0 f2 
  00000010  11 00 e0 f2 10 02 11 cb  1f 02 00 91 f0 03 00 91 
  00000020  11 b4 82 d2 10 02 11 8b  1d 7a 00 a9 fd 03 00 91 
  00000030  1f 20 03 d5 00 00 00 90  00 00 00 91 00 80 00 91 
  00000040  00 00 00 94 00 00 00 90  00 00 00 91 00 00 01 91 
  00000050  00 00 00 94 00 00 00 90  00 00 00 91 00 40 02 91 
  00000060  00 00 00 94 00 00 00 90  00 00 00 91 00 00 03 91 
  00000070  00 00 00 94 00 00 00 90  00 00 00 91 00 a0 03 91 
  00000080  00 00 00 94 f0 03 00 91  10 62 16 91 f0 1f 00 f9 
  00000090  10 00 80 d2 f0 4b 02 f9  f0 4f 02 f9 f0 53 02 f9 
  000000a0  10 00 80 d2 f0 4b 02 f9  f0 03 00 91 10 42 12 91 
  000000b0  f0 23 00 f9 f0 4b 42 f9  f0 57 02 f9 f0 4f 42 f9 
  000000c0  f0 5b 02 f9 f0 53 42 f9  f0 5f 02 f9 10 00 80 d2 
  000000d0  f0 5b 02 f9 f0 5f 02 f9  f0 03 00 91 10 a2 12 91 
  000000e0  f0 27 00 f9 f1 1f 40 f9  f0 57 42 f9 e9 03 11 aa 
  000000f0  30 01 00 f9 f0 5b 42 f9  e9 03 11 aa 29 21 00 91 
  00000100  30 01 00 f9 f0 5f 42 f9  e9 03 11 aa 29 41 00 91 
  00000110  30 01 00 f9 f0 03 00 91  10 62 1f 91 f0 2f 00 f9 
  00000120  f1 1f 40 f9 e9 03 11 aa  30 01 40 f9 f0 63 02 f9 
  00000130  e9 03 11 aa 29 21 00 91  30 01 40 f9 f0 67 02 f9 
  00000140  e9 03 11 aa 29 41 00 91  30 01 40 f9 f0 6b 02 f9 
  00000150  f0 03 00 91 10 02 13 91  f0 33 00 f9 f1 2f 40 f9 
  00000160  f0 63 42 f9 e9 03 11 aa  30 01 00 f9 f0 67 42 f9 
  00000170  e9 03 11 aa 29 21 00 91  30 01 00 f9 f0 6b 42 f9 
  00000180  e9 03 11 aa 29 41 00 91  30 01 00 f9 f0 03 00 91 
  00000190  10 62 28 91 f0 3b 00 f9  f0 3b 40 f9 f0 3f 00 f9 
  000001a0  f1 3f 40 f9 50 01 80 d2  30 02 00 f9 f0 03 00 91 
  000001b0  10 62 2c 91 f0 47 00 f9  f1 3b 40 f9 e9 03 11 aa 
  000001c0  30 01 40 f9 f0 6f 02 f9  e9 03 11 aa 29 21 00 91 
  000001d0  30 01 40 f9 f0 73 02 f9  f0 03 00 91 10 62 13 91 
  000001e0  f0 4b 00 f9 10 00 80 d2  f0 77 02 f9 f0 7b 02 f9 
  000001f0  f0 7f 02 f9 30 00 80 d2  f0 77 02 f9 f0 03 00 91 
  00000200  10 a2 13 91 f0 4f 00 f9  f0 77 42 f9 f0 83 02 f9 
  00000210  f0 7b 42 f9 f0 87 02 f9  f0 7f 42 f9 f0 8b 02 f9 
  00000220  f0 6f 42 f9 f0 87 02 f9  f0 73 42 f9 f0 8b 02 f9 
  00000230  f0 03 00 91 10 02 14 91  f0 53 00 f9 f1 47 40 f9 
  00000240  f0 83 42 f9 e9 03 11 aa  30 01 00 f9 f0 87 42 f9 
  00000250  e9 03 11 aa 29 21 00 91  30 01 00 f9 f0 8b 42 f9 
  00000260  e9 03 11 aa 29 41 00 91  30 01 00 f9 f0 03 00 91 
  00000270  10 62 35 91 f0 5b 00 f9  f1 5b 40 f9 eb 03 11 aa 
  00000280  b0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000290  ea 03 0b aa 50 01 00 f9  70 00 80 d2 10 00 a0 f2 
  000002a0  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  000002b0  50 01 00 f9 f0 03 00 91  10 62 39 91 f0 63 00 f9 
  000002c0  f0 63 40 f9 f0 67 00 f9  f1 5b 40 f9 e9 03 11 aa 
  000002d0  30 01 40 f9 f0 8f 02 f9  e9 03 11 aa 29 21 00 91 
  000002e0  30 01 40 f9 f0 93 02 f9  f0 03 00 91 10 62 14 91 
  000002f0  f0 6b 00 f9 f1 67 40 f9  f0 8f 42 f9 e9 03 11 aa 
  00000300  30 01 00 f9 f0 93 42 f9  e9 03 11 aa 29 21 00 91 
  00000310  30 01 00 f9 f0 03 00 91  10 62 3d 91 f0 73 00 f9 
  00000320  f1 63 40 f9 e9 03 11 aa  30 01 40 f9 f0 97 02 f9 
  00000330  e9 03 11 aa 29 21 00 91  30 01 40 f9 f0 9b 02 f9 
  00000340  f0 03 00 91 10 a2 14 91  f0 77 00 f9 10 00 80 d2 
  00000350  f0 9f 02 f9 f0 a3 02 f9  f0 a7 02 f9 50 00 80 d2 
  00000360  f0 9f 02 f9 f0 03 00 91  10 e2 14 91 f0 7b 00 f9 
  00000370  f0 9f 42 f9 f0 ab 02 f9  f0 a3 42 f9 f0 af 02 f9 
  00000380  f0 a7 42 f9 f0 b3 02 f9  f0 97 42 f9 f0 af 02 f9 
  00000390  f0 9b 42 f9 f0 b3 02 f9  f0 03 00 91 10 42 15 91 
  000003a0  f0 7f 00 f9 f1 73 40 f9  f0 ab 42 f9 e9 03 11 aa 
  000003b0  30 01 00 f9 f0 af 42 f9  e9 03 11 aa 29 21 00 91 
  000003c0  30 01 00 f9 f0 b3 42 f9  e9 03 11 aa 29 41 00 91 
  000003d0  30 01 00 f9 f0 03 00 91  11 33 82 d2 10 02 11 8b 
  000003e0  f0 87 00 f9 f1 87 40 f9  f0 2f 40 f9 30 02 00 f9 
  000003f0  f0 87 40 f9 11 02 40 f9  f1 8f 00 f9 e0 03 00 91 
  00000400  00 a0 15 91 e1 8f 40 f9  9e 00 00 94 f0 03 00 91 
  00000410  10 a2 15 91 f0 93 00 f9  f0 03 00 91 11 3b 82 d2 
  00000420  10 02 11 8b f0 97 00 f9  f1 97 40 f9 f0 b7 42 f9 
  00000430  e9 03 11 aa 30 01 00 f9  f0 bb 42 f9 e9 03 11 aa 
  00000440  29 21 00 91 30 01 00 f9  01 00 00 14 f0 97 40 f9 
  00000450  f0 9f 00 f9 f0 9f 40 f9  11 02 40 f9 f1 a3 00 f9 
  00000460  00 00 00 90 00 00 00 91  00 c0 03 91 e1 a3 40 f9 
  00000470  f0 a3 40 f9 f0 03 00 f9  00 00 00 94 f0 03 00 91 
  00000480  11 5b 82 d2 10 02 11 8b  f0 ab 00 f9 f1 ab 40 f9 
  00000490  f0 47 40 f9 30 02 00 f9  f0 ab 40 f9 11 02 40 f9 
  000004a0  f1 b3 00 f9 e0 03 00 91  00 e0 15 91 e1 b3 40 f9 
  000004b0  74 00 00 94 f0 03 00 91  10 e2 15 91 f0 b7 00 f9 
  000004c0  f0 03 00 91 11 63 82 d2  10 02 11 8b f0 bb 00 f9 
  000004d0  f1 bb 40 f9 f0 bf 42 f9  e9 03 11 aa 30 01 00 f9 
  000004e0  f0 c3 42 f9 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  000004f0  01 00 00 14 f0 bb 40 f9  f0 c3 00 f9 f0 c3 40 f9 
  00000500  11 02 40 f9 f1 c7 00 f9  00 00 00 90 00 00 00 91 
  00000510  00 20 04 91 e1 c7 40 f9  f0 c7 40 f9 f0 03 00 f9 
  00000520  00 00 00 94 f0 03 00 91  11 83 82 d2 10 02 11 8b 
  00000530  f0 cf 00 f9 f1 cf 40 f9  f0 73 40 f9 30 02 00 f9 
  00000540  f0 cf 40 f9 11 02 40 f9  f1 d7 00 f9 e0 03 00 91 
  00000550  00 20 16 91 e1 d7 40 f9  4a 00 00 94 f0 03 00 91 
  00000560  10 22 16 91 f0 db 00 f9  f0 03 00 91 11 8b 82 d2 
  00000570  10 02 11 8b f0 df 00 f9  f1 df 40 f9 f0 c7 42 f9 
  00000580  e9 03 11 aa 30 01 00 f9  f0 cb 42 f9 e9 03 11 aa 
  00000590  29 21 00 91 30 01 00 f9  01 00 00 14 f0 df 40 f9 
  000005a0  f0 e7 00 f9 f0 e7 40 f9  11 02 40 f9 f1 eb 00 f9 
  000005b0  00 00 00 90 00 00 00 91  00 80 04 91 e1 eb 40 f9 
  000005c0  f0 eb 40 f9 f0 03 00 f9  00 00 00 94 f0 03 00 91 
  000005d0  11 ab 82 d2 10 02 11 8b  f0 f3 00 f9 b0 00 80 d2 
  000005e0  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 f1 f3 40 f9 
  000005f0  30 02 00 f9 f0 f3 40 f9  11 02 40 f9 f1 fb 00 f9 
  00000600  e0 fb 40 f9 d9 00 00 94  e0 ff 00 f9 01 00 00 14 
  00000610  00 00 00 90 00 00 00 91  00 e0 04 91 e1 ff 40 f9 
  00000620  f0 ff 40 f9 f0 03 00 f9  00 00 00 94 00 00 00 90 
  00000630  00 00 00 91 00 40 05 91  41 00 80 d2 50 00 80 d2 
  00000640  f0 03 00 f9 00 00 00 94  bf 03 00 91 f0 03 00 91 
  00000650  11 b4 82 d2 10 02 11 8b  1d 7a 40 a9 f0 03 00 91 
  00000660  11 b6 82 d2 11 00 a0 f2  11 00 c0 f2 11 00 e0 f2 
  00000670  10 02 11 8b 1f 02 00 91  00 00 80 d2 c0 03 5f d6 
  00000680  ff 03 1a d1 f0 03 00 91  10 c2 19 91 1d 7a 00 a9 
  00000690  fd 03 00 91 e0 73 01 f9  e1 5b 01 f9 1f 20 03 d5 
  000006a0  f0 03 00 91 10 82 0c 91  f0 a3 00 f9 f0 03 00 91 
  000006b0  10 82 10 91 f0 a7 00 f9  f1 5b 41 f9 e9 03 11 aa 
  000006c0  30 01 40 f9 f0 77 01 f9  e9 03 11 aa 29 21 00 91 
  000006d0  30 01 40 f9 f0 7b 01 f9  e9 03 11 aa 29 41 00 91 
  000006e0  30 01 40 f9 f0 7f 01 f9  f0 03 00 91 10 a2 0b 91 
  000006f0  f0 ab 00 f9 f1 a7 40 f9  f0 77 41 f9 e9 03 11 aa 
  00000700  30 01 00 f9 f0 7b 41 f9  e9 03 11 aa 29 21 00 91 
  00000710  30 01 00 f9 f0 7f 41 f9  e9 03 11 aa 29 41 00 91 
  00000720  30 01 00 f9 f0 03 00 91  10 82 19 91 f0 b3 00 f9 
  00000730  f0 a7 40 f9 f0 b7 00 f9  f0 b7 40 f9 11 02 40 f9 
  00000740  f1 bb 00 f9 f0 bb 40 f9  1f 02 00 f1 f0 17 9f 9a 
  00000750  f0 bf 00 f9 f1 b3 40 f9  f0 e3 45 39 30 02 00 39 
  00000760  f0 b3 40 f9 11 02 40 39  f1 c7 00 f9 f0 23 46 39 
  00000770  1f 06 00 f1 f0 17 9f 9a  f0 cb 00 f9 f0 cb 40 f9 
  00000780  1f 02 00 f1 41 00 00 54  0f 00 00 14 f1 a3 40 f9 
  00000790  eb 03 11 aa 10 00 00 90  10 02 00 91 ea 03 0b aa 
  000007a0  50 01 00 f9 b0 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  000007b0  10 00 e0 f2 ea 03 0b aa  4a 21 00 91 50 01 00 f9 
  000007c0  1b 00 00 14 f0 03 00 91  10 a2 19 91 f0 d3 00 f9 
  000007d0  f0 a7 40 f9 f0 d7 00 f9  f0 d7 40 f9 11 02 40 f9 
  000007e0  f1 db 00 f9 f0 db 40 f9  1f 06 00 f1 f0 17 9f 9a 
  000007f0  f0 df 00 f9 f1 d3 40 f9  f0 e3 46 39 30 02 00 39 
  00000800  f0 d3 40 f9 11 02 40 39  f1 e7 00 f9 f0 23 47 39 
  00000810  1f 06 00 f1 f0 17 9f 9a  f0 eb 00 f9 f0 eb 40 f9 
  00000820  1f 02 00 f1 61 03 00 54  28 00 00 14 f1 a3 40 f9 
  00000830  e9 03 11 aa 30 01 40 f9  f0 83 01 f9 e9 03 11 aa 
  00000840  29 21 00 91 30 01 40 f9  f0 87 01 f9 f0 03 00 91 
  00000850  10 02 0c 91 f0 ef 00 f9  f1 73 41 f9 f0 83 41 f9 
  00000860  e9 03 11 aa 30 01 00 f9  f0 87 41 f9 e9 03 11 aa 
  00000870  29 21 00 91 30 01 00 f9  bf 03 00 91 f0 03 00 91 
  00000880  10 c2 19 91 1d 7a 40 a9  ff 03 1a 91 c0 03 5f d6 
  00000890  f1 a3 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  000008a0  ea 03 0b aa 50 01 00 f9  d0 00 80 d2 10 00 a0 f2 
  000008b0  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  000008c0  50 01 00 f9 da ff ff 17  01 00 00 14 f1 a3 40 f9 
  000008d0  eb 03 11 aa 10 00 00 90  10 02 00 91 ea 03 0b aa 
  000008e0  50 01 00 f9 30 01 80 d2  10 00 a0 f2 10 00 c0 f2 
  000008f0  10 00 e0 f2 ea 03 0b aa  4a 21 00 91 50 01 00 f9 
  00000900  cb ff ff 17 f1 a3 40 f9  e9 03 11 aa 30 01 40 f9 
  00000910  f0 8b 01 f9 e9 03 11 aa  29 21 00 91 30 01 40 f9 
  00000920  f0 8f 01 f9 f0 03 00 91  10 42 0c 91 f0 fb 00 f9 
  00000930  f1 73 41 f9 f0 8b 41 f9  e9 03 11 aa 30 01 00 f9 
  00000940  f0 8f 41 f9 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  00000950  bf 03 00 91 f0 03 00 91  10 c2 19 91 1d 7a 40 a9 
  00000960  ff 03 1a 91 c0 03 5f d6  ff 03 0e d1 f0 03 00 91 
  00000970  10 c2 0d 91 1d 7a 00 a9  fd 03 00 91 e0 5b 01 f9 
  00000980  1f 20 03 d5 f0 03 00 91  10 62 0b 91 f0 e7 00 f9 
  00000990  f0 03 00 91 10 62 0c 91  f0 eb 00 f9 f1 eb 40 f9 
  000009a0  f0 5b 41 f9 30 02 00 f9  f0 03 00 91 10 62 0d 91 
  000009b0  f0 f3 00 f9 f0 eb 40 f9  f0 f7 00 f9 f0 f7 40 f9 
  000009c0  11 02 40 f9 f1 fb 00 f9  f0 fb 40 f9 1f 06 00 f1 
  000009d0  f0 17 9f 9a f0 ff 00 f9  f1 f3 40 f9 f0 e3 47 39 
  000009e0  30 02 00 39 f0 f3 40 f9  11 02 40 39 f1 07 01 f9 
  000009f0  f0 23 48 39 1f 06 00 f1  f0 17 9f 9a f0 0b 01 f9 
  00000a00  f0 0b 41 f9 1f 02 00 f1  41 00 00 54 05 00 00 14 
  00000a10  f1 e7 40 f9 30 00 80 d2  30 02 00 f9 1b 00 00 14 
  00000a20  f0 03 00 91 10 82 0d 91  f0 13 01 f9 f0 eb 40 f9 
  00000a30  f0 17 01 f9 f0 17 41 f9  11 02 40 f9 f1 1b 01 f9 
  00000a40  f0 1b 41 f9 1f 0a 00 f1  f0 17 9f 9a f0 1f 01 f9 
  00000a50  f1 13 41 f9 f0 e3 48 39  30 02 00 39 f0 13 41 f9 
  00000a60  11 02 40 39 f1 27 01 f9  f0 23 49 39 1f 06 00 f1 
  00000a70  f0 17 9f 9a f0 2b 01 f9  f0 2b 41 f9 1f 02 00 f1 
  00000a80  81 01 00 54 0f 00 00 14  f0 e7 40 f9 11 02 40 f9 
  00000a90  f1 2f 01 f9 e0 2f 41 f9  bf 03 00 91 f0 03 00 91 
  00000aa0  10 c2 0d 91 1d 7a 40 a9  ff 03 0e 91 c0 03 5f d6 
  00000ab0  f1 e7 40 f9 50 00 80 d2  30 02 00 f9 f3 ff ff 17 
  00000ac0  f0 03 00 91 10 a2 0d 91  f0 37 01 f9 f0 eb 40 f9 
  00000ad0  f0 3b 01 f9 f0 3b 41 f9  11 02 40 f9 f1 3f 01 f9 
  00000ae0  f0 3f 41 f9 1f 16 00 f1  f0 17 9f 9a f0 43 01 f9 
  00000af0  f1 37 41 f9 f0 03 4a 39  30 02 00 39 f0 37 41 f9 
  00000b00  11 02 40 39 f1 4b 01 f9  f0 43 4a 39 1f 06 00 f1 
  00000b10  f0 17 9f 9a f0 4f 01 f9  f0 4f 41 f9 1f 02 00 f1 
  00000b20  41 00 00 54 05 00 00 14  f1 e7 40 f9 b0 00 80 d2 
  00000b30  30 02 00 f9 d5 ff ff 17  d4 ff ff 17 

.rodata (349 bytes):
  00000000  02 00 00 00 00 00 00 00  70 6f 69 6e 74 00 63 69 
  00000010  72 63 6c 65 00 72 65 63  74 61 6e 67 6c 65 00 00 
  00000020  f0 9f 93 98 20 54 75 74  6f 72 69 61 6c 3a 20 31 
  00000030  35 5f 65 6e 75 6d 73 2e  66 70 0a 00 00 00 00 00 
  00000040  f0 9f a7 ad 20 46 6f 63  75 73 3a 20 45 6e 75 6d 
  00000050  20 76 61 72 69 61 6e 74  73 3a 20 75 6e 69 74 2c 
  00000060  20 74 75 70 6c 65 2c 20  73 74 72 75 63 74 20 76 
  00000070  61 72 69 61 6e 74 73 20  61 6e 64 20 64 69 73 63 
  00000080  72 69 6d 69 6e 61 6e 74  73 0a 00 00 00 00 00 00 
  00000090  f0 9f a7 aa 20 57 68 61  74 20 74 6f 20 6c 6f 6f 
  000000a0  6b 20 66 6f 72 3a 20 6c  61 62 65 6c 65 64 20 6f 
  000000b0  75 74 70 75 74 73 20 62  65 6c 6f 77 0a 00 00 00 
  000000c0  e2 9c 85 20 45 78 70 65  63 74 61 74 69 6f 6e 3a 
  000000d0  20 6f 75 74 70 75 74 73  20 6d 61 74 63 68 20 6c 
  000000e0  61 62 65 6c 73 0a 00 00  0a 00 00 00 00 00 00 00 
  000000f0  73 68 61 70 65 20 70 6f  69 6e 74 20 2d 3e 20 25 
  00000100  73 0a 00 00 00 00 00 00  73 68 61 70 65 20 63 69 
  00000110  72 63 6c 65 20 2d 3e 20  25 73 0a 00 00 00 00 00 
  00000120  73 68 61 70 65 20 72 65  63 74 61 6e 67 6c 65 20 
  00000130  2d 3e 20 25 73 0a 00 00  64 69 73 63 72 69 6d 69 
  00000140  6e 61 6e 74 3a 20 25 6c  6c 64 0a 00 00 00 00 00 
  00000150  63 6f 6e 73 74 3a 20 25  6c 6c 64 0a 00 
