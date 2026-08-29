fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data_0 ty=Array(I8, 4) constant=true initializer=Some(Bytes([104, 111, 116, 0]))
global __const_data_1 ty=Array(I8, 5) constant=true initializer=Some(Bytes([119, 97, 114, 109, 0]))
global __const_data_2 ty=Array(I8, 5) constant=true initializer=Some(Bytes([99, 111, 108, 100, 0]))
global IS_SUNNY ty=I1 constant=true initializer=Some(Bytes([1]))
global __const_data_3 ty=Array(I8, 8) constant=true initializer=Some(Bytes([111, 117, 116, 100, 111, 111, 114, 0]))
global __const_data_4 ty=Array(I8, 7) constant=true initializer=Some(Bytes([105, 110, 100, 111, 111, 114, 0]))
global __const_data_5 ty=Array(I8, 2) constant=true initializer=Some(Bytes([65, 0]))
global __const_data_6 ty=Array(I8, 2) constant=true initializer=Some(Bytes([66, 0]))
global __const_data_7 ty=Array(I8, 2) constant=true initializer=Some(Bytes([67, 0]))
global __const_data_8 ty=Array(I8, 2) constant=true initializer=Some(Bytes([70, 0]))
global __const_data_9 ty=Array(I8, 5) constant=true initializer=Some(Bytes([104, 105, 103, 104, 0]))
global __const_data_10 ty=Array(I8, 7) constant=true initializer=Some(Bytes([109, 101, 100, 105, 117, 109, 0]))
global __const_data_11 ty=Array(I8, 4) constant=true initializer=Some(Bytes([108, 111, 119, 0]))
fn main
  bb0 bb0
    alloca Virtual { id: 114, bank: General, size_bits: 64 }, 16
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    alloca Virtual { id: 120, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 120, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__fp_const_03_control_flow_3)
    bitcast Virtual { id: 122, bank: General, size_bits: 64 }, Virtual { id: 120, bank: General, size_bits: 64 }
    load Virtual { id: 123, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 122, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), symbol(__fp_const_03_control_flow_2), Virtual { id: 123, bank: General, size_bits: 64 }
    alloca Virtual { id: 125, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 125, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__fp_const_03_control_flow_6)
    bitcast Virtual { id: 127, bank: General, size_bits: 64 }, Virtual { id: 125, bank: General, size_bits: 64 }
    load Virtual { id: 128, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 127, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 128, bank: General, size_bits: 64 }
    alloca Virtual { id: 130, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 130, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__fp_const_03_control_flow_8)
    bitcast Virtual { id: 132, bank: General, size_bits: 64 }, Virtual { id: 130, bank: General, size_bits: 64 }
    load Virtual { id: 133, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 132, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), symbol(__fp_const_03_control_flow_7), Virtual { id: 133, bank: General, size_bits: 64 }
    alloca Virtual { id: 135, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 135, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 42
    alloca Virtual { id: 137, bank: General, size_bits: 64 }, 1
    load Virtual { id: 138, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 135, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    gt Virtual { id: 139, bank: General, size_bits: 8 }, Virtual { id: 138, bank: General, size_bits: 64 }, 50
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 137, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 139, bank: General, size_bits: 8 }
    load Virtual { id: 141, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 137, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 142, bank: General, size_bits: 8 }, Virtual { id: 141, bank: General, size_bits: 8 }, 1
    condbr
  bb1 bb1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 114, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb2 bb2
    alloca Virtual { id: 144, bank: General, size_bits: 64 }, 1
    load Virtual { id: 145, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 135, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    gt Virtual { id: 146, bank: General, size_bits: 8 }, Virtual { id: 145, bank: General, size_bits: 64 }, 25
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 144, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 146, bank: General, size_bits: 8 }
    load Virtual { id: 148, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 144, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 149, bank: General, size_bits: 8 }, Virtual { id: 148, bank: General, size_bits: 8 }, 1
    condbr
  bb3 bb3
    alloca Virtual { id: 150, bank: General, size_bits: 64 }, 16
    load Virtual { id: 151, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 114, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 150, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 151, bank: General, size_bits: 64 }
    load Virtual { id: 153, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 135, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 154, bank: General, size_bits: 64 }, Virtual { id: 150, bank: General, size_bits: 64 }
    load Virtual { id: 155, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 154, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 153, bank: General, size_bits: 64 }, Virtual { id: 155, bank: General, size_bits: 64 }
    ret
  bb4 bb4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 114, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb5 bb5
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 114, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb6 bb6
    br


Symbols:
  main                             0x00000000

Text relocations:
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
  offset=0x00000080 kind=Aarch64GotLoad symbol=__fp_const_03_control_flow_3 addend=0
  offset=0x000000b8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000c4 kind=Aarch64GotLoad symbol=__fp_const_03_control_flow_2 addend=0
  offset=0x000000cc kind=Aarch64GotLoad symbol=__fp_const_03_control_flow_2 addend=0
  offset=0x000000e4 kind=CallRel32 symbol=printf addend=0
  offset=0x000000f4 kind=Aarch64GotLoad symbol=__fp_const_03_control_flow_6 addend=0
  offset=0x0000012c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000144 kind=CallRel32 symbol=printf addend=0
  offset=0x00000154 kind=Aarch64GotLoad symbol=__fp_const_03_control_flow_8 addend=0
  offset=0x0000018c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000198 kind=Aarch64GotLoad symbol=__fp_const_03_control_flow_7 addend=0
  offset=0x000001a0 kind=Aarch64GotLoad symbol=__fp_const_03_control_flow_7 addend=0
  offset=0x000001b8 kind=CallRel32 symbol=printf addend=0
  offset=0x0000023c kind=Aarch64AdrpAdd symbol=__const_data_9 addend=0
  offset=0x00000344 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000368 kind=CallRel32 symbol=printf addend=0
  offset=0x00000390 kind=Aarch64AdrpAdd symbol=__const_data_10 addend=0
  offset=0x000003c8 kind=Aarch64AdrpAdd symbol=__const_data_11 addend=0

.text (1020 bytes):
  00000000  ff 83 1e d1 f0 03 00 91  10 42 1e 91 1d 7a 00 a9 
  00000010  fd 03 00 91 1f 20 03 d5  f0 03 00 91 10 02 09 91 
  00000020  f0 0b 00 f9 00 00 00 90  00 00 00 91 00 e0 00 91 
  00000030  00 00 00 94 00 00 00 90  00 00 00 91 00 80 01 91 
  00000040  00 00 00 94 00 00 00 90  00 00 00 91 00 e0 02 91 
  00000050  00 00 00 94 00 00 00 90  00 00 00 91 00 a0 03 91 
  00000060  00 00 00 94 00 00 00 90  00 00 00 91 00 40 04 91 
  00000070  00 00 00 94 f0 03 00 91  10 02 0d 91 f0 23 00 f9 
  00000080  10 00 00 90 10 02 40 f9  e9 23 40 f9 11 02 40 f9 
  00000090  31 01 00 f9 10 22 00 91  29 21 00 91 11 02 40 f9 
  000000a0  31 01 00 f9 f0 23 40 f9  f0 2b 00 f9 f0 2b 40 f9 
  000000b0  11 02 40 f9 f1 2f 00 f9  00 00 00 90 00 00 00 91 
  000000c0  00 60 04 91 01 00 00 90  21 00 40 f9 10 00 00 90 
  000000d0  10 02 40 f9 f0 03 00 f9  e2 2f 40 f9 f0 2f 40 f9 
  000000e0  f0 07 00 f9 00 00 00 94  f0 03 00 91 10 02 11 91 
  000000f0  f0 37 00 f9 10 00 00 90  10 02 40 f9 e9 37 40 f9 
  00000100  11 02 40 f9 31 01 00 f9  10 22 00 91 29 21 00 91 
  00000110  11 02 40 f9 31 01 00 f9  f0 37 40 f9 f0 3f 00 f9 
  00000120  f0 3f 40 f9 11 02 40 f9  f1 43 00 f9 00 00 00 90 
  00000130  00 00 00 91 00 a0 04 91  e1 43 40 f9 f0 43 40 f9 
  00000140  f0 03 00 f9 00 00 00 94  f0 03 00 91 10 02 15 91 
  00000150  f0 4b 00 f9 10 00 00 90  10 02 40 f9 e9 4b 40 f9 
  00000160  11 02 40 f9 31 01 00 f9  10 22 00 91 29 21 00 91 
  00000170  11 02 40 f9 31 01 00 f9  f0 4b 40 f9 f0 53 00 f9 
  00000180  f0 53 40 f9 11 02 40 f9  f1 57 00 f9 00 00 00 90 
  00000190  00 00 00 91 00 e0 04 91  01 00 00 90 21 00 40 f9 
  000001a0  10 00 00 90 10 02 40 f9  f0 03 00 f9 e2 57 40 f9 
  000001b0  f0 57 40 f9 f0 07 00 f9  00 00 00 94 f0 03 00 91 
  000001c0  10 02 19 91 f0 5f 00 f9  f1 5f 40 f9 50 05 80 d2 
  000001d0  30 02 00 f9 f0 03 00 91  10 02 1a 91 f0 67 00 f9 
  000001e0  f0 5f 40 f9 11 02 40 f9  f1 6b 00 f9 f0 6b 40 f9 
  000001f0  1f ca 00 f1 f0 d7 9f 9a  f0 6f 00 f9 f1 67 40 f9 
  00000200  f0 63 43 39 30 02 00 39  f0 67 40 f9 11 02 40 39 
  00000210  f1 77 00 f9 f0 a3 43 39  1f 06 00 f1 f0 17 9f 9a 
  00000220  f0 7b 00 f9 f0 7b 40 f9  1f 02 00 f1 41 00 00 54 
  00000230  0f 00 00 14 f1 0b 40 f9  eb 03 11 aa 10 00 00 90 
  00000240  10 02 00 91 ea 03 0b aa  50 01 00 f9 90 00 80 d2 
  00000250  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 ea 03 0b aa 
  00000260  4a 21 00 91 50 01 00 f9  19 00 00 14 f0 03 00 91 
  00000270  10 22 1a 91 f0 83 00 f9  f0 5f 40 f9 11 02 40 f9 
  00000280  f1 87 00 f9 f0 87 40 f9  1f 66 00 f1 f0 d7 9f 9a 
  00000290  f0 8b 00 f9 f1 83 40 f9  f0 43 44 39 30 02 00 39 
  000002a0  f0 83 40 f9 11 02 40 39  f1 93 00 f9 f0 83 44 39 
  000002b0  1f 06 00 f1 f0 17 9f 9a  f0 97 00 f9 f0 97 40 f9 
  000002c0  1f 02 00 f1 21 06 00 54  3e 00 00 14 f0 03 00 91 
  000002d0  10 42 1a 91 f0 9b 00 f9  f1 0b 40 f9 e9 03 11 aa 
  000002e0  30 01 40 f9 f0 1b 01 f9  e9 03 11 aa 29 21 00 91 
  000002f0  30 01 40 f9 f0 1f 01 f9  f0 03 00 91 10 c2 08 91 
  00000300  f0 9f 00 f9 f1 9b 40 f9  f0 1b 41 f9 e9 03 11 aa 
  00000310  30 01 00 f9 f0 1f 41 f9  e9 03 11 aa 29 21 00 91 
  00000320  30 01 00 f9 f0 5f 40 f9  11 02 40 f9 f1 a7 00 f9 
  00000330  f0 9b 40 f9 f0 ab 00 f9  f0 ab 40 f9 11 02 40 f9 
  00000340  f1 af 00 f9 00 00 00 90  00 00 00 91 00 40 05 91 
  00000350  e1 a7 40 f9 f0 a7 40 f9  f0 03 00 f9 e2 af 40 f9 
  00000360  f0 af 40 f9 f0 07 00 f9  00 00 00 94 bf 03 00 91 
  00000370  f0 03 00 91 10 42 1e 91  1d 7a 40 a9 ff 83 1e 91 
  00000380  00 00 80 d2 c0 03 5f d6  f1 0b 40 f9 eb 03 11 aa 
  00000390  10 00 00 90 10 02 00 91  ea 03 0b aa 50 01 00 f9 
  000003a0  d0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  000003b0  ea 03 0b aa 4a 21 00 91  50 01 00 f9 0f 00 00 14 
  000003c0  f1 0b 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  000003d0  ea 03 0b aa 50 01 00 f9  70 00 80 d2 10 00 a0 f2 
  000003e0  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  000003f0  50 01 00 f9 01 00 00 14  b5 ff ff 17 

.rodata (354 bytes):
  00000000  68 6f 74 00 77 61 72 6d  00 63 6f 6c 64 00 01 6f 
  00000010  75 74 64 6f 6f 72 00 69  6e 64 6f 6f 72 00 41 00 
  00000020  42 00 43 00 46 00 68 69  67 68 00 6d 65 64 69 75 
  00000030  6d 00 6c 6f 77 00 00 00  f0 9f 93 98 20 54 75 74 
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
