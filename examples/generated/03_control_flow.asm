fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
fn main
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 1, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 2, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 3, bank: General, size_bits: 64 }, 1
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    alloca Virtual { id: 9, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 9, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 25
    alloca Virtual { id: 11, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 11, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 25
    alloca Virtual { id: 13, bank: General, size_bits: 64 }, 1
    load Virtual { id: 14, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 11, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    gt Virtual { id: 15, bank: General, size_bits: 8 }, Virtual { id: 14, bank: General, size_bits: 64 }, 30
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 13, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 15, bank: General, size_bits: 64 }
    load Virtual { id: 17, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 13, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 18, bank: General, size_bits: 8 }, Virtual { id: 17, bank: General, size_bits: 64 }, 1
    condbr
  bb1 bb1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_0)
    br
  bb2 bb2
    alloca Virtual { id: 20, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 20, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 25
    alloca Virtual { id: 22, bank: General, size_bits: 64 }, 1
    load Virtual { id: 23, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 20, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    gt Virtual { id: 24, bank: General, size_bits: 8 }, Virtual { id: 23, bank: General, size_bits: 64 }, 20
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 22, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 24, bank: General, size_bits: 64 }
    load Virtual { id: 26, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 22, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 27, bank: General, size_bits: 8 }, Virtual { id: 26, bank: General, size_bits: 64 }, 1
    condbr
  bb3 bb3
    load Virtual { id: 28, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 9, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 29, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 28, bank: General, size_bits: 64 }, Virtual { id: 29, bank: General, size_bits: 64 }
    alloca Virtual { id: 31, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 31, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    alloca Virtual { id: 33, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 33, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 25
    alloca Virtual { id: 35, bank: General, size_bits: 64 }, 1
    load Virtual { id: 36, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 33, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    gt Virtual { id: 37, bank: General, size_bits: 8 }, Virtual { id: 36, bank: General, size_bits: 64 }, 20
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 35, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 37, bank: General, size_bits: 64 }
    alloca Virtual { id: 39, bank: General, size_bits: 64 }, 1
    load Virtual { id: 40, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 31, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 41, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 35, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    and Virtual { id: 42, bank: General, size_bits: 8 }, Virtual { id: 40, bank: General, size_bits: 64 }, Virtual { id: 41, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 39, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 42, bank: General, size_bits: 64 }
    load Virtual { id: 44, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 39, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 45, bank: General, size_bits: 8 }, Virtual { id: 44, bank: General, size_bits: 64 }, 1
    condbr
  bb4 bb4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_1)
    br
  bb5 bb5
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_2)
    br
  bb7 bb7
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_3)
    br
  bb8 bb8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_4)
    br
  bb6 bb6
    br
  bb9 bb9
    load Virtual { id: 50, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 50, bank: General, size_bits: 64 }
    alloca Virtual { id: 52, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 52, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 85
    alloca Virtual { id: 54, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 54, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 85
    alloca Virtual { id: 56, bank: General, size_bits: 64 }, 1
    load Virtual { id: 57, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 54, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ge Virtual { id: 58, bank: General, size_bits: 8 }, Virtual { id: 57, bank: General, size_bits: 64 }, 90
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 56, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 58, bank: General, size_bits: 64 }
    load Virtual { id: 60, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 56, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 61, bank: General, size_bits: 8 }, Virtual { id: 60, bank: General, size_bits: 64 }, 1
    condbr
  bb10 bb10
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_5)
    br
  bb11 bb11
    alloca Virtual { id: 63, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 63, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 85
    alloca Virtual { id: 65, bank: General, size_bits: 64 }, 1
    load Virtual { id: 66, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 63, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ge Virtual { id: 67, bank: General, size_bits: 8 }, Virtual { id: 66, bank: General, size_bits: 64 }, 80
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 67, bank: General, size_bits: 64 }
    load Virtual { id: 69, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 70, bank: General, size_bits: 8 }, Virtual { id: 69, bank: General, size_bits: 64 }, 1
    condbr
  bb12 bb12
    load Virtual { id: 71, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 52, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 72, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 71, bank: General, size_bits: 64 }, Virtual { id: 72, bank: General, size_bits: 64 }
    alloca Virtual { id: 74, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 74, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 42
    alloca Virtual { id: 76, bank: General, size_bits: 64 }, 1
    load Virtual { id: 77, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 74, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    gt Virtual { id: 78, bank: General, size_bits: 8 }, Virtual { id: 77, bank: General, size_bits: 64 }, 50
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 76, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 78, bank: General, size_bits: 64 }
    load Virtual { id: 80, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 76, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 81, bank: General, size_bits: 8 }, Virtual { id: 80, bank: General, size_bits: 64 }, 1
    condbr
  bb13 bb13
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_6)
    br
  bb14 bb14
    alloca Virtual { id: 83, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 83, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 85
    alloca Virtual { id: 85, bank: General, size_bits: 64 }, 1
    load Virtual { id: 86, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 83, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ge Virtual { id: 87, bank: General, size_bits: 8 }, Virtual { id: 86, bank: General, size_bits: 64 }, 70
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 85, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 87, bank: General, size_bits: 64 }
    load Virtual { id: 89, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 85, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 90, bank: General, size_bits: 8 }, Virtual { id: 89, bank: General, size_bits: 64 }, 1
    condbr
  bb19 bb19
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_7)
    br
  bb20 bb20
    alloca Virtual { id: 92, bank: General, size_bits: 64 }, 1
    load Virtual { id: 93, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 74, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    gt Virtual { id: 94, bank: General, size_bits: 8 }, Virtual { id: 93, bank: General, size_bits: 64 }, 25
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 94, bank: General, size_bits: 64 }
    load Virtual { id: 96, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 97, bank: General, size_bits: 8 }, Virtual { id: 96, bank: General, size_bits: 64 }, 1
    condbr
  bb15 bb15
    br
  bb16 bb16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_8)
    br
  bb17 bb17
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_9)
    br
  bb21 bb21
    alloca Virtual { id: 100, bank: General, size_bits: 64 }, 1
    load Virtual { id: 101, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 100, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 101, bank: General, size_bits: 64 }
    load Virtual { id: 103, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 74, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 104, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 100, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 103, bank: General, size_bits: 64 }, Virtual { id: 104, bank: General, size_bits: 64 }
    ret
  bb22 bb22
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_10)
    br
  bb23 bb23
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_11)
    br
  bb18 bb18
    br
  bb24 bb24
    br


Symbols:
  main                             0x00000000

Relocations:
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
  offset=0x00000128 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000001c8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001ec kind=CallRel32 symbol=printf addend=0
  offset=0x000002c4 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x000002d8 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x000002ec kind=Aarch64AdrpAdd symbol=__const_data_3 addend=0
  offset=0x00000300 kind=Aarch64AdrpAdd symbol=__const_data_4 addend=0
  offset=0x00000320 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000338 kind=CallRel32 symbol=printf addend=0
  offset=0x000003d0 kind=Aarch64AdrpAdd symbol=__const_data_5 addend=0
  offset=0x00000470 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000494 kind=CallRel32 symbol=printf addend=0
  offset=0x00000514 kind=Aarch64AdrpAdd symbol=__const_data_6 addend=0
  offset=0x000005a0 kind=Aarch64AdrpAdd symbol=__const_data_7 addend=0
  offset=0x00000618 kind=Aarch64AdrpAdd symbol=__const_data_8 addend=0
  offset=0x0000062c kind=Aarch64AdrpAdd symbol=__const_data_9 addend=0
  offset=0x00000678 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000069c kind=CallRel32 symbol=printf addend=0
  offset=0x000006c0 kind=Aarch64AdrpAdd symbol=__const_data_10 addend=0
  offset=0x000006d4 kind=Aarch64AdrpAdd symbol=__const_data_11 addend=0

.text (1772 bytes):
  00000000  ff 43 15 d1 f0 03 00 91  10 02 15 91 1d 7a 00 a9 
  00000010  fd 03 00 91 f0 03 00 91  10 02 12 91 f0 0b 00 f9 
  00000020  f0 03 00 91 10 22 12 91  f0 0f 00 f9 f0 03 00 91 
  00000030  10 42 12 91 f0 13 00 f9  f0 03 00 91 10 62 12 91 
  00000040  f0 17 00 f9 00 00 00 90  00 00 00 91 00 e0 00 91 
  00000050  00 00 00 94 00 00 00 90  00 00 00 91 00 80 01 91 
  00000060  00 00 00 94 00 00 00 90  00 00 00 91 00 e0 02 91 
  00000070  00 00 00 94 00 00 00 90  00 00 00 91 00 a0 03 91 
  00000080  00 00 00 94 00 00 00 90  00 00 00 91 00 40 04 91 
  00000090  00 00 00 94 f0 03 00 91  10 82 12 91 f0 2f 00 f9 
  000000a0  f1 2f 40 f9 30 03 80 d2  30 02 00 f9 f0 03 00 91 
  000000b0  10 a2 12 91 f0 37 00 f9  f1 37 40 f9 30 03 80 d2 
  000000c0  30 02 00 f9 f0 03 00 91  10 c2 12 91 f0 3f 00 f9 
  000000d0  f0 37 40 f9 11 02 40 f9  f1 43 00 f9 f0 43 40 f9 
  000000e0  1f 7a 00 f1 f0 d7 9f 9a  f0 47 00 f9 f1 3f 40 f9 
  000000f0  f0 23 42 39 30 02 00 39  f0 3f 40 f9 11 02 40 39 
  00000100  f1 4f 00 f9 f0 63 42 39  1f 06 00 f1 f0 17 9f 9a 
  00000110  f0 53 00 f9 f0 53 40 f9  1f 02 00 f1 41 00 00 54 
  00000120  06 00 00 14 f1 13 40 f9  10 00 00 90 10 02 00 91 
  00000130  30 02 00 f9 1f 00 00 14  f0 03 00 91 10 e2 12 91 
  00000140  f0 5b 00 f9 f1 5b 40 f9  30 03 80 d2 30 02 00 f9 
  00000150  f0 03 00 91 10 02 13 91  f0 63 00 f9 f0 5b 40 f9 
  00000160  11 02 40 f9 f1 67 00 f9  f0 67 40 f9 1f 52 00 f1 
  00000170  f0 d7 9f 9a f0 6b 00 f9  f1 63 40 f9 f0 43 43 39 
  00000180  30 02 00 39 f0 63 40 f9  11 02 40 39 f1 73 00 f9 
  00000190  f0 83 43 39 1f 06 00 f1  f0 17 9f 9a f0 77 00 f9 
  000001a0  f0 77 40 f9 1f 02 00 f1  c1 08 00 54 4a 00 00 14 
  000001b0  f0 2f 40 f9 11 02 40 f9  f1 7b 00 f9 f0 13 40 f9 
  000001c0  11 02 40 f9 f1 7f 00 f9  00 00 00 90 00 00 00 91 
  000001d0  00 60 04 91 e1 7b 40 f9  f0 7b 40 f9 f0 03 00 f9 
  000001e0  e2 7f 40 f9 f0 7f 40 f9  f0 07 00 f9 00 00 00 94 
  000001f0  f0 03 00 91 10 22 13 91  f0 87 00 f9 f1 87 40 f9 
  00000200  30 00 80 d2 30 02 00 39  f0 03 00 91 10 42 13 91 
  00000210  f0 8f 00 f9 f1 8f 40 f9  30 03 80 d2 30 02 00 f9 
  00000220  f0 03 00 91 10 62 13 91  f0 97 00 f9 f0 8f 40 f9 
  00000230  11 02 40 f9 f1 9b 00 f9  f0 9b 40 f9 1f 52 00 f1 
  00000240  f0 d7 9f 9a f0 9f 00 f9  f1 97 40 f9 f0 e3 44 39 
  00000250  30 02 00 39 f0 03 00 91  10 82 13 91 f0 a7 00 f9 
  00000260  f0 87 40 f9 11 02 40 39  f1 ab 00 f9 f0 97 40 f9 
  00000270  11 02 40 39 f1 af 00 f9  f0 43 45 39 f1 63 45 39 
  00000280  10 02 11 8a f0 b3 00 f9  f1 a7 40 f9 f0 83 45 39 
  00000290  30 02 00 39 f0 a7 40 f9  11 02 40 39 f1 bb 00 f9 
  000002a0  f0 c3 45 39 1f 06 00 f1  f0 17 9f 9a f0 bf 00 f9 
  000002b0  f0 bf 40 f9 1f 02 00 f1  81 01 00 54 10 00 00 14 
  000002c0  f1 13 40 f9 10 00 00 90  10 02 00 91 30 02 00 f9 
  000002d0  10 00 00 14 f1 13 40 f9  10 00 00 90 10 02 00 91 
  000002e0  30 02 00 f9 0b 00 00 14  f1 0f 40 f9 10 00 00 90 
  000002f0  10 02 00 91 30 02 00 f9  07 00 00 14 f1 0f 40 f9 
  00000300  10 00 00 90 10 02 00 91  30 02 00 f9 02 00 00 14 
  00000310  a8 ff ff 17 f0 0f 40 f9  11 02 40 f9 f1 d3 00 f9 
  00000320  00 00 00 90 00 00 00 91  00 a0 04 91 e1 d3 40 f9 
  00000330  f0 d3 40 f9 f0 03 00 f9  00 00 00 94 f0 03 00 91 
  00000340  10 a2 13 91 f0 db 00 f9  f1 db 40 f9 b0 0a 80 d2 
  00000350  30 02 00 f9 f0 03 00 91  10 c2 13 91 f0 e3 00 f9 
  00000360  f1 e3 40 f9 b0 0a 80 d2  30 02 00 f9 f0 03 00 91 
  00000370  10 e2 13 91 f0 eb 00 f9  f0 e3 40 f9 11 02 40 f9 
  00000380  f1 ef 00 f9 f0 ef 40 f9  1f 6a 01 f1 f0 b7 9f 9a 
  00000390  f0 f3 00 f9 f1 eb 40 f9  f0 83 47 39 30 02 00 39 
  000003a0  f0 eb 40 f9 11 02 40 39  f1 fb 00 f9 f0 c3 47 39 
  000003b0  1f 06 00 f1 f0 17 9f 9a  f0 ff 00 f9 f0 ff 40 f9 
  000003c0  1f 02 00 f1 41 00 00 54  06 00 00 14 f1 17 40 f9 
  000003d0  10 00 00 90 10 02 00 91  30 02 00 f9 1f 00 00 14 
  000003e0  f0 03 00 91 10 02 14 91  f0 07 01 f9 f1 07 41 f9 
  000003f0  b0 0a 80 d2 30 02 00 f9  f0 03 00 91 10 22 14 91 
  00000400  f0 0f 01 f9 f0 07 41 f9  11 02 40 f9 f1 13 01 f9 
  00000410  f0 13 41 f9 1f 42 01 f1  f0 b7 9f 9a f0 17 01 f9 
  00000420  f1 0f 41 f9 f0 a3 48 39  30 02 00 39 f0 0f 41 f9 
  00000430  11 02 40 39 f1 1f 01 f9  f0 e3 48 39 1f 06 00 f1 
  00000440  f0 17 9f 9a f0 23 01 f9  f0 23 41 f9 1f 02 00 f1 
  00000450  01 06 00 54 34 00 00 14  f0 db 40 f9 11 02 40 f9 
  00000460  f1 27 01 f9 f0 17 40 f9  11 02 40 f9 f1 2b 01 f9 
  00000470  00 00 00 90 00 00 00 91  00 e0 04 91 e1 27 41 f9 
  00000480  f0 27 41 f9 f0 03 00 f9  e2 2b 41 f9 f0 2b 41 f9 
  00000490  f0 07 00 f9 00 00 00 94  f0 03 00 91 10 42 14 91 
  000004a0  f0 33 01 f9 f1 33 41 f9  50 05 80 d2 30 02 00 f9 
  000004b0  f0 03 00 91 10 62 14 91  f0 3b 01 f9 f0 33 41 f9 
  000004c0  11 02 40 f9 f1 3f 01 f9  f0 3f 41 f9 1f ca 00 f1 
  000004d0  f0 d7 9f 9a f0 43 01 f9  f1 3b 41 f9 f0 03 4a 39 
  000004e0  30 02 00 39 f0 3b 41 f9  11 02 40 39 f1 4b 01 f9 
  000004f0  f0 43 4a 39 1f 06 00 f1  f0 17 9f 9a f0 4f 01 f9 
  00000500  f0 4f 41 f9 1f 02 00 f1  a1 04 00 54 29 00 00 14 
  00000510  f1 17 40 f9 10 00 00 90  10 02 00 91 30 02 00 f9 
  00000520  3c 00 00 14 f0 03 00 91  10 82 14 91 f0 57 01 f9 
  00000530  f1 57 41 f9 b0 0a 80 d2  30 02 00 f9 f0 03 00 91 
  00000540  10 a2 14 91 f0 5f 01 f9  f0 57 41 f9 11 02 40 f9 
  00000550  f1 63 01 f9 f0 63 41 f9  1f 1a 01 f1 f0 b7 9f 9a 
  00000560  f0 67 01 f9 f1 5f 41 f9  f0 23 4b 39 30 02 00 39 
  00000570  f0 5f 41 f9 11 02 40 39  f1 6f 01 f9 f0 63 4b 39 
  00000580  1f 06 00 f1 f0 17 9f 9a  f0 73 01 f9 f0 73 41 f9 
  00000590  1f 02 00 f1 01 04 00 54  24 00 00 14 f1 0b 40 f9 
  000005a0  10 00 00 90 10 02 00 91  30 02 00 f9 24 00 00 14 
  000005b0  f0 03 00 91 10 c2 14 91  f0 7b 01 f9 f0 33 41 f9 
  000005c0  11 02 40 f9 f1 7f 01 f9  f0 7f 41 f9 1f 66 00 f1 
  000005d0  f0 d7 9f 9a f0 83 01 f9  f1 7b 41 f9 f0 03 4c 39 
  000005e0  30 02 00 39 f0 7b 41 f9  11 02 40 39 f1 8b 01 f9 
  000005f0  f0 43 4c 39 1f 06 00 f1  f0 17 9f 9a f0 8f 01 f9 
  00000600  f0 8f 41 f9 1f 02 00 f1  a1 05 00 54 31 00 00 14 
  00000610  92 ff ff 17 f1 17 40 f9  10 00 00 90 10 02 00 91 
  00000620  30 02 00 f9 30 00 00 14  f1 17 40 f9 10 00 00 90 
  00000630  10 02 00 91 30 02 00 f9  2b 00 00 14 f0 03 00 91 
  00000640  10 e2 14 91 f0 9b 01 f9  f0 0b 40 f9 11 02 40 f9 
  00000650  f1 9f 01 f9 f1 9b 41 f9  f0 9f 41 f9 30 02 00 f9 
  00000660  f0 33 41 f9 11 02 40 f9  f1 a7 01 f9 f0 9b 41 f9 
  00000670  11 02 40 f9 f1 ab 01 f9  00 00 00 90 00 00 00 91 
  00000680  00 40 05 91 e1 a7 41 f9  f0 a7 41 f9 f0 03 00 f9 
  00000690  e2 ab 41 f9 f0 ab 41 f9  f0 07 00 f9 00 00 00 94 
  000006a0  bf 03 00 91 f0 03 00 91  10 02 15 91 1d 7a 40 a9 
  000006b0  ff 43 15 91 00 00 80 d2  c0 03 5f d6 f1 0b 40 f9 
  000006c0  10 00 00 90 10 02 00 91  30 02 00 f9 07 00 00 14 
  000006d0  f1 0b 40 f9 10 00 00 90  10 02 00 91 30 02 00 f9 
  000006e0  02 00 00 14 cb ff ff 17  d5 ff ff 17 

.rodata (354 bytes):
  00000000  68 6f 74 00 77 61 72 6d  00 63 6f 6c 64 00 6f 75 
  00000010  74 64 6f 6f 72 00 69 6e  64 6f 6f 72 00 41 00 42 
  00000020  00 68 69 67 68 00 43 00  46 00 6d 65 64 69 75 6d 
  00000030  00 6c 6f 77 00 00 00 00  f0 9f 93 98 20 54 75 74 
  00000040  6f 72 69 61 6c 3a 20 30  33 5f 63 6f 6e 74 72 6f 
  00000050  6c 5f 66 6c 6f 77 2e 66  70 0a 00 00 00 00 00 00 
  00000060  f0 9f a7 ad 20 46 6f 63  75 73 3a 20 43 6f 6e 74 
  00000070  72 6f 6c 20 66 6c 6f 77  3a 20 69 66 2f 65 6c 73 
  00000080  65 20 65 78 70 72 65 73  73 69 6f 6e 73 20 77 69 
  00000090  74 68 20 63 6f 6e 73 74  20 61 6e 64 20 72 75 6e 
  000000a0  74 69 6d 65 20 65 76 61  6c 75 61 74 69 6f 6e 0a 
  000000b0  00 00 00 00 00 00 00 00  f0 9f a7 aa 20 57 68 61 
  000000c0  74 20 74 6f 20 6c 6f 6f  6b 20 66 6f 72 3a 20 6c 
  000000d0  61 62 65 6c 65 64 20 6f  75 74 70 75 74 73 20 62 
  000000e0  65 6c 6f 77 0a 00 00 00  e2 9c 85 20 45 78 70 65 
  000000f0  63 74 61 74 69 6f 6e 3a  20 6f 75 74 70 75 74 73 
  00000100  20 6d 61 74 63 68 20 6c  61 62 65 6c 73 0a 00 00 
  00000110  0a 00 00 00 00 00 00 00  25 6c 6c 64 c2 b0 43 20 
  00000120  69 73 20 25 73 0a 00 00  53 75 67 67 65 73 74 65 
  00000130  64 3a 20 25 73 0a 00 00  53 63 6f 72 65 20 25 6c 
  00000140  6c 64 20 3d 20 67 72 61  64 65 20 25 73 0a 00 00 
  00000150  56 61 6c 75 65 20 25 6c  6c 64 20 69 73 20 25 73 
  00000160  0a 00 
