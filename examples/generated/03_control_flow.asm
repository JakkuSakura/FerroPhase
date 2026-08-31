fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global TEMP ty=I64 constant=true initializer=Some(Bytes([25, 0, 0, 0, 0, 0, 0, 0]))
global __const_data__03_control_flow___fp_comptime_const_WEATHER_12562947600243404313_g0_0 ty=Array(I8, 4) constant=true initializer=Some(Bytes([104, 111, 116, 0]))
global __const_data__03_control_flow___fp_comptime_const_WEATHER_12562947600243404313_g0_1 ty=Array(I8, 5) constant=true initializer=Some(Bytes([119, 97, 114, 109, 0]))
global __const_data__03_control_flow___fp_comptime_const_WEATHER_12562947600243404313_g0_2 ty=Array(I8, 5) constant=true initializer=Some(Bytes([99, 111, 108, 100, 0]))
global IS_SUNNY ty=I1 constant=true initializer=Some(Bytes([1]))
global IS_WARM ty=I1 constant=true initializer=Some(Bytes([1]))
global __const_data__03_control_flow___fp_comptime_const_ACTIVITY_1011881808258253794_g0_3 ty=Array(I8, 8) constant=true initializer=Some(Bytes([111, 117, 116, 100, 111, 111, 114, 0]))
global __const_data__03_control_flow___fp_comptime_const_ACTIVITY_1011881808258253794_g0_4 ty=Array(I8, 7) constant=true initializer=Some(Bytes([105, 110, 100, 111, 111, 114, 0]))
global SCORE ty=I64 constant=true initializer=Some(Bytes([85, 0, 0, 0, 0, 0, 0, 0]))
global __const_data__03_control_flow___fp_comptime_const_GRADE_3147398172033932994_g0_5 ty=Array(I8, 2) constant=true initializer=Some(Bytes([65, 0]))
global __const_data__03_control_flow___fp_comptime_const_GRADE_3147398172033932994_g0_6 ty=Array(I8, 2) constant=true initializer=Some(Bytes([66, 0]))
global __const_data__03_control_flow___fp_comptime_const_GRADE_3147398172033932994_g0_7 ty=Array(I8, 2) constant=true initializer=Some(Bytes([67, 0]))
global __const_data__03_control_flow___fp_comptime_const_GRADE_3147398172033932994_g0_8 ty=Array(I8, 2) constant=true initializer=Some(Bytes([70, 0]))
global __const_data__03_control_flow_main_g0_9 ty=Array(I8, 5) constant=true initializer=Some(Bytes([104, 105, 103, 104, 0]))
global __const_data__03_control_flow_main_g0_10 ty=Array(I8, 7) constant=true initializer=Some(Bytes([109, 101, 100, 105, 117, 109, 0]))
global __const_data__03_control_flow_main_g0_11 ty=Array(I8, 4) constant=true initializer=Some(Bytes([108, 111, 119, 0]))
fn main
  bb0 bb0
    alloca Virtual { id: 91, bank: General, size_bits: 64 }, 16
    alloca Virtual { id: 92, bank: General, size_bits: 64 }, 16
    alloca Virtual { id: 93, bank: General, size_bits: 64 }, 16
    alloca Virtual { id: 94, bank: General, size_bits: 64 }, 16
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    alloca Virtual { id: 100, bank: General, size_bits: 64 }, 1
    gt Virtual { id: 101, bank: General, size_bits: 8 }, 25, 30
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 100, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 101, bank: General, size_bits: 8 }
    load Virtual { id: 103, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 100, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 104, bank: General, size_bits: 8 }, Virtual { id: 103, bank: General, size_bits: 8 }, 1
    condbr
  bb1 bb1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 91, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb2 bb2
    alloca Virtual { id: 106, bank: General, size_bits: 64 }, 1
    gt Virtual { id: 107, bank: General, size_bits: 8 }, 25, 20
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 106, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 107, bank: General, size_bits: 8 }
    load Virtual { id: 109, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 106, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 110, bank: General, size_bits: 8 }, Virtual { id: 109, bank: General, size_bits: 8 }, 1
    condbr
  bb3 bb3
    bitcast Virtual { id: 111, bank: General, size_bits: 64 }, Virtual { id: 91, bank: General, size_bits: 64 }
    load Virtual { id: 112, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 111, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), 25, Virtual { id: 112, bank: General, size_bits: 64 }
    alloca Virtual { id: 114, bank: General, size_bits: 64 }, 1
    and Virtual { id: 115, bank: General, size_bits: 8 }, 1, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 114, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 115, bank: General, size_bits: 8 }
    load Virtual { id: 117, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 114, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 118, bank: General, size_bits: 8 }, Virtual { id: 117, bank: General, size_bits: 8 }, 1
    condbr
  bb4 bb4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 91, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb5 bb5
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 91, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb7 bb7
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 93, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb8 bb8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 93, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb6 bb6
    br
  bb9 bb9
    bitcast Virtual { id: 123, bank: General, size_bits: 64 }, Virtual { id: 93, bank: General, size_bits: 64 }
    load Virtual { id: 124, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 123, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 124, bank: General, size_bits: 64 }
    alloca Virtual { id: 126, bank: General, size_bits: 64 }, 1
    ge Virtual { id: 127, bank: General, size_bits: 8 }, 85, 90
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 126, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 127, bank: General, size_bits: 8 }
    load Virtual { id: 129, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 126, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 130, bank: General, size_bits: 8 }, Virtual { id: 129, bank: General, size_bits: 8 }, 1
    condbr
  bb10 bb10
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 94, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb11 bb11
    alloca Virtual { id: 132, bank: General, size_bits: 64 }, 1
    ge Virtual { id: 133, bank: General, size_bits: 8 }, 85, 80
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 132, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 133, bank: General, size_bits: 8 }
    load Virtual { id: 135, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 132, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 136, bank: General, size_bits: 8 }, Virtual { id: 135, bank: General, size_bits: 8 }, 1
    condbr
  bb12 bb12
    bitcast Virtual { id: 137, bank: General, size_bits: 64 }, Virtual { id: 94, bank: General, size_bits: 64 }
    load Virtual { id: 138, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 137, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), 85, Virtual { id: 138, bank: General, size_bits: 64 }
    alloca Virtual { id: 140, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 140, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 42
    alloca Virtual { id: 142, bank: General, size_bits: 64 }, 1
    load Virtual { id: 143, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 140, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    gt Virtual { id: 144, bank: General, size_bits: 8 }, Virtual { id: 143, bank: General, size_bits: 64 }, 50
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 142, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 144, bank: General, size_bits: 8 }
    load Virtual { id: 146, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 142, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 147, bank: General, size_bits: 8 }, Virtual { id: 146, bank: General, size_bits: 8 }, 1
    condbr
  bb13 bb13
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 94, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb14 bb14
    alloca Virtual { id: 149, bank: General, size_bits: 64 }, 1
    ge Virtual { id: 150, bank: General, size_bits: 8 }, 85, 70
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 149, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 150, bank: General, size_bits: 8 }
    load Virtual { id: 152, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 149, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 153, bank: General, size_bits: 8 }, Virtual { id: 152, bank: General, size_bits: 8 }, 1
    condbr
  bb19 bb19
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb20 bb20
    alloca Virtual { id: 155, bank: General, size_bits: 64 }, 1
    load Virtual { id: 156, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 140, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    gt Virtual { id: 157, bank: General, size_bits: 8 }, Virtual { id: 156, bank: General, size_bits: 64 }, 25
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 155, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 157, bank: General, size_bits: 8 }
    load Virtual { id: 159, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 155, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 160, bank: General, size_bits: 8 }, Virtual { id: 159, bank: General, size_bits: 8 }, 1
    condbr
  bb15 bb15
    br
  bb16 bb16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 94, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb17 bb17
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 94, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb21 bb21
    alloca Virtual { id: 163, bank: General, size_bits: 64 }, 16
    load Virtual { id: 164, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 163, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 164, bank: General, size_bits: 64 }
    load Virtual { id: 166, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 140, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 167, bank: General, size_bits: 64 }, Virtual { id: 163, bank: General, size_bits: 64 }
    load Virtual { id: 168, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 167, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 166, bank: General, size_bits: 64 }, Virtual { id: 168, bank: General, size_bits: 64 }
    ret
  bb22 bb22
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb23 bb23
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb18 bb18
    br
  bb24 bb24
    br


Symbols:
  main                             0x00000000

Text relocations:
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
  offset=0x000000f4 kind=Aarch64AdrpAdd symbol=__const_data__03_control_flow___fp_comptime_const_WEATHER_12562947600243404313_g0_0 addend=0
  offset=0x0000018c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001b0 kind=CallRel32 symbol=printf addend=0
  offset=0x00000210 kind=Aarch64AdrpAdd symbol=__const_data__03_control_flow___fp_comptime_const_WEATHER_12562947600243404313_g0_1 addend=0
  offset=0x00000248 kind=Aarch64AdrpAdd symbol=__const_data__03_control_flow___fp_comptime_const_WEATHER_12562947600243404313_g0_2 addend=0
  offset=0x00000280 kind=Aarch64AdrpAdd symbol=__const_data__03_control_flow___fp_comptime_const_ACTIVITY_1011881808258253794_g0_3 addend=0
  offset=0x000002b8 kind=Aarch64AdrpAdd symbol=__const_data__03_control_flow___fp_comptime_const_ACTIVITY_1011881808258253794_g0_4 addend=0
  offset=0x00000300 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000318 kind=CallRel32 symbol=printf addend=0
  offset=0x00000378 kind=Aarch64AdrpAdd symbol=__const_data__03_control_flow___fp_comptime_const_GRADE_3147398172033932994_g0_5 addend=0
  offset=0x00000410 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000434 kind=CallRel32 symbol=printf addend=0
  offset=0x000004b8 kind=Aarch64AdrpAdd symbol=__const_data__03_control_flow___fp_comptime_const_GRADE_3147398172033932994_g0_6 addend=0
  offset=0x00000544 kind=Aarch64AdrpAdd symbol=__const_data__03_control_flow_main_g0_9 addend=0
  offset=0x000005e0 kind=Aarch64AdrpAdd symbol=__const_data__03_control_flow___fp_comptime_const_GRADE_3147398172033932994_g0_7 addend=0
  offset=0x00000618 kind=Aarch64AdrpAdd symbol=__const_data__03_control_flow___fp_comptime_const_GRADE_3147398172033932994_g0_8 addend=0
  offset=0x000006c0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000006e4 kind=CallRel32 symbol=printf addend=0
  offset=0x0000070c kind=Aarch64AdrpAdd symbol=__const_data__03_control_flow_main_g0_10 addend=0
  offset=0x00000744 kind=Aarch64AdrpAdd symbol=__const_data__03_control_flow_main_g0_11 addend=0

.text (1916 bytes):
  00000000  ff c3 24 d1 f0 03 00 91  10 82 24 91 1d 7a 00 a9 
  00000010  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 82 0e 91 
  00000020  f0 0b 00 f9 f0 03 00 91  10 82 12 91 f0 0f 00 f9 
  00000030  f0 03 00 91 10 82 16 91  f0 13 00 f9 f0 03 00 91 
  00000040  10 82 1a 91 f0 17 00 f9  00 00 00 90 00 00 00 91 
  00000050  00 20 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000060  00 c0 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000070  00 20 03 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000080  00 e0 03 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000090  00 80 04 91 00 00 00 94  f0 03 00 91 10 82 1e 91 
  000000a0  f0 2f 00 f9 30 03 80 d2  1f 7a 00 f1 f0 d7 9f 9a 
  000000b0  f0 33 00 f9 f1 2f 40 f9  f0 83 41 39 30 02 00 39 
  000000c0  f0 2f 40 f9 11 02 40 39  f1 3b 00 f9 f0 c3 41 39 
  000000d0  1f 06 00 f1 f0 17 9f 9a  f0 3f 00 f9 f0 3f 40 f9 
  000000e0  1f 02 00 f1 41 00 00 54  0f 00 00 14 f1 0b 40 f9 
  000000f0  eb 03 11 aa 10 00 00 90  10 02 00 91 ea 03 0b aa 
  00000100  50 01 00 f9 70 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000110  10 00 e0 f2 ea 03 0b aa  4a 21 00 91 50 01 00 f9 
  00000120  16 00 00 14 f0 03 00 91  10 a2 1e 91 f0 47 00 f9 
  00000130  30 03 80 d2 1f 52 00 f1  f0 d7 9f 9a f0 4b 00 f9 
  00000140  f1 47 40 f9 f0 43 42 39  30 02 00 39 f0 47 40 f9 
  00000150  11 02 40 39 f1 53 00 f9  f0 83 42 39 1f 06 00 f1 
  00000160  f0 17 9f 9a f0 57 00 f9  f0 57 40 f9 1f 02 00 f1 
  00000170  c1 04 00 54 33 00 00 14  f0 0b 40 f9 f0 5b 00 f9 
  00000180  f0 5b 40 f9 11 02 40 f9  f1 5f 00 f9 00 00 00 90 
  00000190  00 00 00 91 00 a0 04 91  21 03 80 d2 30 03 80 d2 
  000001a0  f0 03 00 f9 e2 5f 40 f9  f0 5f 40 f9 f0 07 00 f9 
  000001b0  00 00 00 94 f0 03 00 91  10 c2 1e 91 f0 67 00 f9 
  000001c0  30 00 80 d2 31 00 80 d2  10 02 11 8a f0 6b 00 f9 
  000001d0  f1 67 40 f9 f0 43 43 39  30 02 00 39 f0 67 40 f9 
  000001e0  11 02 40 39 f1 73 00 f9  f0 83 43 39 1f 06 00 f1 
  000001f0  f0 17 9f 9a f0 77 00 f9  f0 77 40 f9 1f 02 00 f1 
  00000200  c1 03 00 54 2b 00 00 14  f1 0b 40 f9 eb 03 11 aa 
  00000210  10 00 00 90 10 02 00 91  ea 03 0b aa 50 01 00 f9 
  00000220  90 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000230  ea 03 0b aa 4a 21 00 91  50 01 00 f9 2b 00 00 14 
  00000240  f1 0b 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  00000250  ea 03 0b aa 50 01 00 f9  90 00 80 d2 10 00 a0 f2 
  00000260  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  00000270  50 01 00 f9 1d 00 00 14  f1 13 40 f9 eb 03 11 aa 
  00000280  10 00 00 90 10 02 00 91  ea 03 0b aa 50 01 00 f9 
  00000290  f0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  000002a0  ea 03 0b aa 4a 21 00 91  50 01 00 f9 10 00 00 14 
  000002b0  f1 13 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  000002c0  ea 03 0b aa 50 01 00 f9  d0 00 80 d2 10 00 a0 f2 
  000002d0  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  000002e0  50 01 00 f9 02 00 00 14  a4 ff ff 17 f0 13 40 f9 
  000002f0  f0 8b 00 f9 f0 8b 40 f9  11 02 40 f9 f1 8f 00 f9 
  00000300  00 00 00 90 00 00 00 91  00 e0 04 91 e1 8f 40 f9 
  00000310  f0 8f 40 f9 f0 03 00 f9  00 00 00 94 f0 03 00 91 
  00000320  10 e2 1e 91 f0 97 00 f9  b0 0a 80 d2 1f 6a 01 f1 
  00000330  f0 b7 9f 9a f0 9b 00 f9  f1 97 40 f9 f0 c3 44 39 
  00000340  30 02 00 39 f0 97 40 f9  11 02 40 39 f1 a3 00 f9 
  00000350  f0 03 45 39 1f 06 00 f1  f0 17 9f 9a f0 a7 00 f9 
  00000360  f0 a7 40 f9 1f 02 00 f1  41 00 00 54 0f 00 00 14 
  00000370  f1 17 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  00000380  ea 03 0b aa 50 01 00 f9  30 00 80 d2 10 00 a0 f2 
  00000390  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  000003a0  50 01 00 f9 16 00 00 14  f0 03 00 91 10 02 1f 91 
  000003b0  f0 af 00 f9 b0 0a 80 d2  1f 42 01 f1 f0 b7 9f 9a 
  000003c0  f0 b3 00 f9 f1 af 40 f9  f0 83 45 39 30 02 00 39 
  000003d0  f0 af 40 f9 11 02 40 39  f1 bb 00 f9 f0 c3 45 39 
  000003e0  1f 06 00 f1 f0 17 9f 9a  f0 bf 00 f9 f0 bf 40 f9 
  000003f0  1f 02 00 f1 e1 05 00 54  3c 00 00 14 f0 17 40 f9 
  00000400  f0 c3 00 f9 f0 c3 40 f9  11 02 40 f9 f1 c7 00 f9 
  00000410  00 00 00 90 00 00 00 91  00 20 05 91 a1 0a 80 d2 
  00000420  b0 0a 80 d2 f0 03 00 f9  e2 c7 40 f9 f0 c7 40 f9 
  00000430  f0 07 00 f9 00 00 00 94  f0 03 00 91 10 22 1f 91 
  00000440  f0 cf 00 f9 f1 cf 40 f9  50 05 80 d2 30 02 00 f9 
  00000450  f0 03 00 91 10 22 20 91  f0 d7 00 f9 f0 cf 40 f9 
  00000460  11 02 40 f9 f1 db 00 f9  f0 db 40 f9 1f ca 00 f1 
  00000470  f0 d7 9f 9a f0 df 00 f9  f1 d7 40 f9 f0 e3 46 39 
  00000480  30 02 00 39 f0 d7 40 f9  11 02 40 39 f1 e7 00 f9 
  00000490  f0 23 47 39 1f 06 00 f1  f0 17 9f 9a f0 eb 00 f9 
  000004a0  f0 eb 40 f9 1f 02 00 f1  a1 04 00 54 32 00 00 14 
  000004b0  f1 17 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  000004c0  ea 03 0b aa 50 01 00 f9  30 00 80 d2 10 00 a0 f2 
  000004d0  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  000004e0  50 01 00 f9 3c 00 00 14  f0 03 00 91 10 42 20 91 
  000004f0  f0 f3 00 f9 b0 0a 80 d2  1f 1a 01 f1 f0 b7 9f 9a 
  00000500  f0 f7 00 f9 f1 f3 40 f9  f0 a3 47 39 30 02 00 39 
  00000510  f0 f3 40 f9 11 02 40 39  f1 ff 00 f9 f0 e3 47 39 
  00000520  1f 06 00 f1 f0 17 9f 9a  f0 03 01 f9 f0 03 41 f9 
  00000530  1f 02 00 f1 21 05 00 54  36 00 00 14 f1 0f 40 f9 
  00000540  eb 03 11 aa 10 00 00 90  10 02 00 91 ea 03 0b aa 
  00000550  50 01 00 f9 90 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000560  10 00 e0 f2 ea 03 0b aa  4a 21 00 91 50 01 00 f9 
  00000570  36 00 00 14 f0 03 00 91  10 62 20 91 f0 0b 01 f9 
  00000580  f0 cf 40 f9 11 02 40 f9  f1 0f 01 f9 f0 0f 41 f9 
  00000590  1f 66 00 f1 f0 d7 9f 9a  f0 13 01 f9 f1 0b 41 f9 
  000005a0  f0 83 48 39 30 02 00 39  f0 0b 41 f9 11 02 40 39 
  000005b0  f1 1b 01 f9 f0 c3 48 39  1f 06 00 f1 f0 17 9f 9a 
  000005c0  f0 1f 01 f9 f0 1f 41 f9  1f 02 00 f1 c1 09 00 54 
  000005d0  5b 00 00 14 8a ff ff 17  f1 17 40 f9 eb 03 11 aa 
  000005e0  10 00 00 90 10 02 00 91  ea 03 0b aa 50 01 00 f9 
  000005f0  30 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000600  ea 03 0b aa 4a 21 00 91  50 01 00 f9 5a 00 00 14 
  00000610  f1 17 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  00000620  ea 03 0b aa 50 01 00 f9  30 00 80 d2 10 00 a0 f2 
  00000630  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  00000640  50 01 00 f9 4c 00 00 14  f0 03 00 91 10 82 20 91 
  00000650  f0 2b 01 f9 f1 0f 40 f9  e9 03 11 aa 30 01 40 f9 
  00000660  f0 cb 01 f9 e9 03 11 aa  29 21 00 91 30 01 40 f9 
  00000670  f0 cf 01 f9 f0 03 00 91  10 42 0e 91 f0 2f 01 f9 
  00000680  f1 2b 41 f9 f0 cb 41 f9  e9 03 11 aa 30 01 00 f9 
  00000690  f0 cf 41 f9 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  000006a0  f0 cf 40 f9 11 02 40 f9  f1 37 01 f9 f0 2b 41 f9 
  000006b0  f0 3b 01 f9 f0 3b 41 f9  11 02 40 f9 f1 3f 01 f9 
  000006c0  00 00 00 90 00 00 00 91  00 80 05 91 e1 37 41 f9 
  000006d0  f0 37 41 f9 f0 03 00 f9  e2 3f 41 f9 f0 3f 41 f9 
  000006e0  f0 07 00 f9 00 00 00 94  bf 03 00 91 f0 03 00 91 
  000006f0  10 82 24 91 1d 7a 40 a9  ff c3 24 91 00 00 80 d2 
  00000700  c0 03 5f d6 f1 0f 40 f9  eb 03 11 aa 10 00 00 90 
  00000710  10 02 00 91 ea 03 0b aa  50 01 00 f9 d0 00 80 d2 
  00000720  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 ea 03 0b aa 
  00000730  4a 21 00 91 50 01 00 f9  10 00 00 14 f1 0f 40 f9 
  00000740  eb 03 11 aa 10 00 00 90  10 02 00 91 ea 03 0b aa 
  00000750  50 01 00 f9 70 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000760  10 00 e0 f2 ea 03 0b aa  4a 21 00 91 50 01 00 f9 
  00000770  02 00 00 14 98 ff ff 17  b4 ff ff 17 

.rodata (370 bytes):
  00000000  19 00 00 00 00 00 00 00  68 6f 74 00 77 61 72 6d 
  00000010  00 63 6f 6c 64 00 01 01  6f 75 74 64 6f 6f 72 00 
  00000020  69 6e 64 6f 6f 72 00 00  55 00 00 00 00 00 00 00 
  00000030  41 00 42 00 43 00 46 00  68 69 67 68 00 6d 65 64 
  00000040  69 75 6d 00 6c 6f 77 00  f0 9f 93 98 20 54 75 74 
  00000050  6f 72 69 61 6c 3a 20 30  33 5f 63 6f 6e 74 72 6f 
  00000060  6c 5f 66 6c 6f 77 2e 66  70 0a 00 00 00 00 00 00 
  00000070  f0 9f a7 ad 20 46 6f 63  75 73 3a 20 43 6f 6e 74 
  00000080  72 6f 6c 20 66 6c 6f 77  3a 20 69 66 2f 65 6c 73 
  00000090  65 20 65 78 70 72 65 73  73 69 6f 6e 73 20 77 69 
  000000a0  74 68 20 63 6f 6e 73 74  20 61 6e 64 20 72 75 6e 
  000000b0  74 69 6d 65 20 65 76 61  6c 75 61 74 69 6f 6e 0a 
  000000c0  00 00 00 00 00 00 00 00  f0 9f a7 aa 20 57 68 61 
  000000d0  74 20 74 6f 20 6c 6f 6f  6b 20 66 6f 72 3a 20 6c 
  000000e0  61 62 65 6c 65 64 20 6f  75 74 70 75 74 73 20 62 
  000000f0  65 6c 6f 77 0a 00 00 00  e2 9c 85 20 45 78 70 65 
  00000100  63 74 61 74 69 6f 6e 3a  20 6f 75 74 70 75 74 73 
  00000110  20 6d 61 74 63 68 20 6c  61 62 65 6c 73 0a 00 00 
  00000120  0a 00 00 00 00 00 00 00  25 6c 6c 64 c2 b0 43 20 
  00000130  69 73 20 25 73 0a 00 00  53 75 67 67 65 73 74 65 
  00000140  64 3a 20 25 73 0a 00 00  53 63 6f 72 65 20 25 6c 
  00000150  6c 64 20 3d 20 67 72 61  64 65 20 25 73 0a 00 00 
  00000160  56 61 6c 75 65 20 25 6c  6c 64 20 69 73 20 25 73 
  00000170  0a 00 
