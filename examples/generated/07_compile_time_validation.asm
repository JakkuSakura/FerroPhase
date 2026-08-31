fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data__07_compile_time_validation___fp_comptime_const_HEADER_HAS_VERSION_14485270751929506700_g0_0 ty=Array(I8, 12) constant=true initializer=Some(Bytes([115, 116, 114, 117, 99, 116, 32, 68, 97, 116, 97, 0]))
global DATA_TYPE_NAME ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 11, 0, 0, 0, 0, 0, 0, 0]))
global __const_data__07_compile_time_validation___fp_comptime_const_HEADER_HAS_VERSION_14485270751929506700_g0_1 ty=Array(I8, 4) constant=true initializer=Some(Bytes([105, 54, 52, 0]))
global DATA_FIELD_A_TYPE ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0]))
global __const_data__07_compile_time_validation___fp_comptime_const_HEADER_HAS_VERSION_14485270751929506700_g0_2 ty=Array(I8, 4) constant=true initializer=Some(Bytes([117, 54, 52, 0]))
global HEADER_FIELD_VERSION_TYPE ty=Struct { fields: [Ptr(I8), I64], packed: false, name: Some("__slice") } constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0]))
global MAX_SIZE ty=I64 constant=true initializer=Some(Bytes([64, 0, 0, 0, 0, 0, 0, 0]))
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    alloca Virtual { id: 86, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 86, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 32
    alloca Virtual { id: 88, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 88, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 3
    load Virtual { id: 90, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 86, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 91, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 88, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 90, bank: General, size_bits: 64 }, Virtual { id: 91, bank: General, size_bits: 64 }
    alloca Virtual { id: 93, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 93, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    alloca Virtual { id: 95, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 95, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    load Virtual { id: 97, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 93, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 98, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 95, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 97, bank: General, size_bits: 8 }, Virtual { id: 98, bank: General, size_bits: 8 }
    alloca Virtual { id: 100, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 100, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 16
    alloca Virtual { id: 102, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 102, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 4
    alloca Virtual { id: 104, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 104, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    load Virtual { id: 106, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 100, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 107, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 102, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 108, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 104, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 106, bank: General, size_bits: 64 }, Virtual { id: 107, bank: General, size_bits: 64 }, Virtual { id: 108, bank: General, size_bits: 8 }
    intrinsic.call symbol(intrinsic.println), symbol(__const_data__07_compile_time_validation___fp_comptime_const_HEADER_HAS_VERSION_14485270751929506700_g0_0), symbol(__const_data__07_compile_time_validation___fp_comptime_const_HEADER_HAS_VERSION_14485270751929506700_g0_1), symbol(__const_data__07_compile_time_validation___fp_comptime_const_HEADER_HAS_VERSION_14485270751929506700_g0_2)
    alloca Virtual { id: 111, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 111, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    load Virtual { id: 113, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 111, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 113, bank: General, size_bits: 8 }
    alloca Virtual { id: 115, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 115, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 32
    alloca Virtual { id: 117, bank: General, size_bits: 64 }, 1
    load Virtual { id: 118, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 115, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    le Virtual { id: 119, bank: General, size_bits: 8 }, Virtual { id: 118, bank: General, size_bits: 64 }, 64
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 117, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 119, bank: General, size_bits: 8 }
    alloca Virtual { id: 121, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 121, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 16
    alloca Virtual { id: 123, bank: General, size_bits: 64 }, 1
    load Virtual { id: 124, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 121, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    le Virtual { id: 125, bank: General, size_bits: 8 }, Virtual { id: 124, bank: General, size_bits: 64 }, 64
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 123, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 125, bank: General, size_bits: 8 }
    alloca Virtual { id: 127, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 127, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 32
    alloca Virtual { id: 129, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 129, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 16
    alloca Virtual { id: 131, bank: General, size_bits: 64 }, 8
    load Virtual { id: 132, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 127, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 133, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 129, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 134, bank: General, size_bits: 64 }, Virtual { id: 132, bank: General, size_bits: 64 }, Virtual { id: 133, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 131, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 134, bank: General, size_bits: 64 }
    alloca Virtual { id: 136, bank: General, size_bits: 64 }, 1
    load Virtual { id: 137, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 131, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    le Virtual { id: 138, bank: General, size_bits: 8 }, Virtual { id: 137, bank: General, size_bits: 64 }, 96
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 136, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 138, bank: General, size_bits: 8 }
    alloca Virtual { id: 140, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 140, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 32
    alloca Virtual { id: 142, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 142, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 16
    alloca Virtual { id: 144, bank: General, size_bits: 64 }, 8
    load Virtual { id: 145, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 140, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 146, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 142, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 147, bank: General, size_bits: 64 }, Virtual { id: 145, bank: General, size_bits: 64 }, Virtual { id: 146, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 144, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 147, bank: General, size_bits: 64 }
    load Virtual { id: 149, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 117, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 150, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 123, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 151, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 136, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 152, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 144, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 149, bank: General, size_bits: 8 }, Virtual { id: 150, bank: General, size_bits: 8 }, Virtual { id: 151, bank: General, size_bits: 8 }, Virtual { id: 152, bank: General, size_bits: 64 }
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
  offset=0x000000b0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000d4 kind=CallRel32 symbol=printf addend=0
  offset=0x00000120 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000144 kind=CallRel32 symbol=printf addend=0
  offset=0x000001b4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001e4 kind=CallRel32 symbol=printf addend=0
  offset=0x000001e8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000001f4 kind=Aarch64AdrpAdd symbol=__const_data__07_compile_time_validation___fp_comptime_const_HEADER_HAS_VERSION_14485270751929506700_g0_0 addend=0
  offset=0x000001fc kind=Aarch64AdrpAdd symbol=__const_data__07_compile_time_validation___fp_comptime_const_HEADER_HAS_VERSION_14485270751929506700_g0_0 addend=0
  offset=0x00000208 kind=Aarch64AdrpAdd symbol=__const_data__07_compile_time_validation___fp_comptime_const_HEADER_HAS_VERSION_14485270751929506700_g0_1 addend=0
  offset=0x00000210 kind=Aarch64AdrpAdd symbol=__const_data__07_compile_time_validation___fp_comptime_const_HEADER_HAS_VERSION_14485270751929506700_g0_1 addend=0
  offset=0x0000021c kind=Aarch64AdrpAdd symbol=__const_data__07_compile_time_validation___fp_comptime_const_HEADER_HAS_VERSION_14485270751929506700_g0_2 addend=0
  offset=0x00000224 kind=Aarch64AdrpAdd symbol=__const_data__07_compile_time_validation___fp_comptime_const_HEADER_HAS_VERSION_14485270751929506700_g0_2 addend=0
  offset=0x00000230 kind=CallRel32 symbol=printf addend=0
  offset=0x00000258 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000270 kind=CallRel32 symbol=printf addend=0
  offset=0x00000450 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000048c kind=CallRel32 symbol=printf addend=0

Section relocations:
  section=Data offset=0x00000000 kind=Abs64 symbol=__const_data__07_compile_time_validation___fp_comptime_const_HEADER_HAS_VERSION_14485270751929506700_g0_0 addend=0
  section=Data offset=0x00000010 kind=Abs64 symbol=__const_data__07_compile_time_validation___fp_comptime_const_HEADER_HAS_VERSION_14485270751929506700_g0_1 addend=0
  section=Data offset=0x00000020 kind=Abs64 symbol=__const_data__07_compile_time_validation___fp_comptime_const_HEADER_HAS_VERSION_14485270751929506700_g0_2 addend=0

.text (1196 bytes):
  00000000  ff 03 1b d1 f0 03 00 91  10 c2 1a 91 1d 7a 00 a9 
  00000010  fd 03 00 91 1f 20 03 d5  00 00 00 90 00 00 00 91 
  00000020  00 80 00 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000030  00 40 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000040  00 80 02 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000050  00 40 03 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000060  00 e0 03 91 00 00 00 94  f0 03 00 91 10 c2 0d 91 
  00000070  f0 27 00 f9 f1 27 40 f9  10 04 80 d2 30 02 00 f9 
  00000080  f0 03 00 91 10 c2 0e 91  f0 2f 00 f9 f1 2f 40 f9 
  00000090  70 00 80 d2 30 02 00 f9  f0 27 40 f9 11 02 40 f9 
  000000a0  f1 37 00 f9 f0 2f 40 f9  11 02 40 f9 f1 3b 00 f9 
  000000b0  00 00 00 90 00 00 00 91  00 00 04 91 e1 37 40 f9 
  000000c0  f0 37 40 f9 f0 03 00 f9  e2 3b 40 f9 f0 3b 40 f9 
  000000d0  f0 07 00 f9 00 00 00 94  f0 03 00 91 10 c2 0f 91 
  000000e0  f0 43 00 f9 f1 43 40 f9  30 00 80 d2 30 02 00 39 
  000000f0  f0 03 00 91 10 e2 0f 91  f0 4b 00 f9 f1 4b 40 f9 
  00000100  10 00 80 d2 30 02 00 39  f0 43 40 f9 11 02 40 39 
  00000110  f1 53 00 f9 f0 4b 40 f9  11 02 40 39 f1 57 00 f9 
  00000120  00 00 00 90 00 00 00 91  00 80 04 91 e1 83 42 39 
  00000130  f0 83 42 39 f0 03 00 f9  e2 a3 42 39 f0 a3 42 39 
  00000140  f0 07 00 f9 00 00 00 94  f0 03 00 91 10 02 10 91 
  00000150  f0 5f 00 f9 f1 5f 40 f9  10 02 80 d2 30 02 00 f9 
  00000160  f0 03 00 91 10 02 11 91  f0 67 00 f9 f1 67 40 f9 
  00000170  90 00 80 d2 30 02 00 f9  f0 03 00 91 10 02 12 91 
  00000180  f0 6f 00 f9 f1 6f 40 f9  30 00 80 d2 30 02 00 39 
  00000190  f0 5f 40 f9 11 02 40 f9  f1 77 00 f9 f0 67 40 f9 
  000001a0  11 02 40 f9 f1 7b 00 f9  f0 6f 40 f9 11 02 40 39 
  000001b0  f1 7f 00 f9 00 00 00 90  00 00 00 91 00 00 05 91 
  000001c0  e1 77 40 f9 f0 77 40 f9  f0 03 00 f9 e2 7b 40 f9 
  000001d0  f0 7b 40 f9 f0 07 00 f9  e3 e3 43 39 f0 e3 43 39 
  000001e0  f0 0b 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  000001f0  00 e0 05 91 01 00 00 90  21 00 00 91 10 00 00 90 
  00000200  10 02 00 91 f0 03 00 f9  02 00 00 90 42 00 00 91 
  00000210  10 00 00 90 10 02 00 91  f0 07 00 f9 03 00 00 90 
  00000220  63 00 00 91 10 00 00 90  10 02 00 91 f0 0b 00 f9 
  00000230  00 00 00 94 f0 03 00 91  10 22 12 91 f0 8b 00 f9 
  00000240  f1 8b 40 f9 10 00 80 d2  30 02 00 39 f0 8b 40 f9 
  00000250  11 02 40 39 f1 93 00 f9  00 00 00 90 00 00 00 91 
  00000260  00 80 06 91 e1 83 44 39  f0 83 44 39 f0 03 00 f9 
  00000270  00 00 00 94 f0 03 00 91  10 42 12 91 f0 9b 00 f9 
  00000280  f1 9b 40 f9 10 04 80 d2  30 02 00 f9 f0 03 00 91 
  00000290  10 42 13 91 f0 a3 00 f9  f0 9b 40 f9 11 02 40 f9 
  000002a0  f1 a7 00 f9 f0 a7 40 f9  1f 02 01 f1 f0 c7 9f 9a 
  000002b0  f0 ab 00 f9 f1 a3 40 f9  f0 43 45 39 30 02 00 39 
  000002c0  f0 03 00 91 10 62 13 91  f0 b3 00 f9 f1 b3 40 f9 
  000002d0  10 02 80 d2 30 02 00 f9  f0 03 00 91 10 62 14 91 
  000002e0  f0 bb 00 f9 f0 b3 40 f9  11 02 40 f9 f1 bf 00 f9 
  000002f0  f0 bf 40 f9 1f 02 01 f1  f0 c7 9f 9a f0 c3 00 f9 
  00000300  f1 bb 40 f9 f0 03 46 39  30 02 00 39 f0 03 00 91 
  00000310  10 82 14 91 f0 cb 00 f9  f1 cb 40 f9 10 04 80 d2 
  00000320  30 02 00 f9 f0 03 00 91  10 82 15 91 f0 d3 00 f9 
  00000330  f1 d3 40 f9 10 02 80 d2  30 02 00 f9 f0 03 00 91 
  00000340  10 82 16 91 f0 db 00 f9  f0 cb 40 f9 11 02 40 f9 
  00000350  f1 df 00 f9 f0 d3 40 f9  11 02 40 f9 f1 e3 00 f9 
  00000360  f0 df 40 f9 f1 e3 40 f9  10 02 11 8b f0 e7 00 f9 
  00000370  f1 db 40 f9 f0 e7 40 f9  30 02 00 f9 f0 03 00 91 
  00000380  10 82 17 91 f0 ef 00 f9  f0 db 40 f9 11 02 40 f9 
  00000390  f1 f3 00 f9 f0 f3 40 f9  1f 82 01 f1 f0 c7 9f 9a 
  000003a0  f0 f7 00 f9 f1 ef 40 f9  f0 a3 47 39 30 02 00 39 
  000003b0  f0 03 00 91 10 a2 17 91  f0 ff 00 f9 f1 ff 40 f9 
  000003c0  10 04 80 d2 30 02 00 f9  f0 03 00 91 10 a2 18 91 
  000003d0  f0 07 01 f9 f1 07 41 f9  10 02 80 d2 30 02 00 f9 
  000003e0  f0 03 00 91 10 a2 19 91  f0 0f 01 f9 f0 ff 40 f9 
  000003f0  11 02 40 f9 f1 13 01 f9  f0 07 41 f9 11 02 40 f9 
  00000400  f1 17 01 f9 f0 13 41 f9  f1 17 41 f9 10 02 11 8b 
  00000410  f0 1b 01 f9 f1 0f 41 f9  f0 1b 41 f9 30 02 00 f9 
  00000420  f0 a3 40 f9 11 02 40 39  f1 23 01 f9 f0 bb 40 f9 
  00000430  11 02 40 39 f1 27 01 f9  f0 ef 40 f9 11 02 40 39 
  00000440  f1 2b 01 f9 f0 0f 41 f9  11 02 40 f9 f1 2f 01 f9 
  00000450  00 00 00 90 00 00 00 91  00 e0 06 91 e1 03 49 39 
  00000460  f0 03 49 39 f0 03 00 f9  e2 23 49 39 f0 23 49 39 
  00000470  f0 07 00 f9 e3 43 49 39  f0 43 49 39 f0 0b 00 f9 
  00000480  e4 2f 41 f9 f0 2f 41 f9  f0 0f 00 f9 00 00 00 94 
  00000490  bf 03 00 91 f0 03 00 91  10 c2 1a 91 1d 7a 40 a9 
  000004a0  ff 03 1b 91 00 00 80 d2  c0 03 5f d6 

.rodata (504 bytes):
  00000000  73 74 72 75 63 74 20 44  61 74 61 00 69 36 34 00 
  00000010  75 36 34 00 00 00 00 00  40 00 00 00 00 00 00 00 
  00000020  f0 9f 93 98 20 54 75 74  6f 72 69 61 6c 3a 20 30 
  00000030  37 5f 63 6f 6d 70 69 6c  65 5f 74 69 6d 65 5f 76 
  00000040  61 6c 69 64 61 74 69 6f  6e 2e 66 70 0a 00 00 00 
  00000050  f0 9f a7 ad 20 46 6f 63  75 73 3a 20 43 6f 6d 70 
  00000060  69 6c 65 2d 74 69 6d 65  20 76 61 6c 69 64 61 74 
  00000070  69 6f 6e 20 75 73 69 6e  67 20 63 6f 6e 73 74 20 
  00000080  65 78 70 72 65 73 73 69  6f 6e 73 20 61 6e 64 20 
  00000090  69 6e 74 72 6f 73 70 65  63 74 69 6f 6e 0a 00 00 
  000000a0  f0 9f a7 aa 20 57 68 61  74 20 74 6f 20 6c 6f 6f 
  000000b0  6b 20 66 6f 72 3a 20 6c  61 62 65 6c 65 64 20 6f 
  000000c0  75 74 70 75 74 73 20 62  65 6c 6f 77 0a 00 00 00 
  000000d0  e2 9c 85 20 45 78 70 65  63 74 61 74 69 6f 6e 3a 
  000000e0  20 6f 75 74 70 75 74 73  20 6d 61 74 63 68 20 6c 
  000000f0  61 62 65 6c 73 0a 00 00  0a 00 00 00 00 00 00 00 
  00000100  64 61 74 61 3a 20 73 69  7a 65 6f 66 3d 25 6c 6c 
  00000110  75 2c 20 66 69 65 6c 64  73 3d 25 6c 6c 64 0a 00 
  00000120  64 61 74 61 3a 20 68 61  73 5f 61 3d 25 64 2c 20 
  00000130  68 61 73 5f 78 3d 25 64  0a 00 00 00 00 00 00 00 
  00000140  68 65 61 64 65 72 3a 20  73 69 7a 65 6f 66 3d 25 
  00000150  6c 6c 75 2c 20 66 69 65  6c 64 73 3d 25 6c 6c 64 
  00000160  2c 20 68 61 73 5f 76 65  72 73 69 6f 6e 3d 25 64 
  00000170  0a 00 00 00 00 00 00 00  74 79 70 65 73 3a 20 64 
  00000180  61 74 61 3d 27 25 73 27  20 61 3d 27 25 73 27 20 
  00000190  76 65 72 73 69 6f 6e 3d  27 25 73 27 0a 00 00 00 
  000001a0  64 61 74 61 20 68 61 73  20 74 6f 5f 73 74 72 69 
  000001b0  6e 67 3a 20 25 64 0a 00  6c 61 79 6f 75 74 3a 20 
  000001c0  64 61 74 61 5f 6f 6b 3d  25 64 2c 20 68 65 61 64 
  000001d0  65 72 5f 6f 6b 3d 25 64  2c 20 74 6f 74 61 6c 5f 
  000001e0  6f 6b 3d 25 64 2c 20 74  6f 74 61 6c 5f 73 69 7a 
  000001f0  65 3d 25 6c 6c 75 0a 00 
