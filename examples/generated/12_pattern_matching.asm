fp-native dump: format=MachO arch=Aarch64 entry=0xb3c

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data_0 ty=Array(I8, 4) constant=true initializer=Some(Bytes([114, 101, 100, 0]))
global __const_data_1 ty=Array(I8, 6) constant=true initializer=Some(Bytes([103, 114, 101, 101, 110, 0]))
global __const_data_2 ty=Array(I8, 8) constant=true initializer=Some(Bytes([114, 101, 100, 32, 114, 103, 98, 0]))
global __const_data_3 ty=Array(I8, 11) constant=true initializer=Some(Bytes([99, 117, 115, 116, 111, 109, 32, 114, 103, 98, 0]))
global __const_data_4 ty=Array(I8, 5) constant=true initializer=Some(Bytes([122, 101, 114, 111, 0]))
global __const_data_5 ty=Array(I8, 9) constant=true initializer=Some(Bytes([110, 101, 103, 97, 116, 105, 118, 101, 0]))
global __const_data_6 ty=Array(I8, 5) constant=true initializer=Some(Bytes([101, 118, 101, 110, 0]))
global __const_data_7 ty=Array(I8, 4) constant=true initializer=Some(Bytes([111, 100, 100, 0]))
fn examples__12_pattern_matching__describe
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 1, bank: General, size_bits: 64 }, 1
    load Virtual { id: 2, bank: General, size_bits: 64 }, symbol(frame.local.1)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 2, bank: General, size_bits: 64 }
    alloca Virtual { id: 4, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 5, bank: General, size_bits: 64 }, Virtual { id: 1, bank: General, size_bits: 64 }
    load Virtual { id: 6, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 7, bank: General, size_bits: 8 }, Virtual { id: 6, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 4, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 7, bank: General, size_bits: 8 }
    load Virtual { id: 9, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 4, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 10, bank: General, size_bits: 8 }, Virtual { id: 9, bank: General, size_bits: 8 }, 1
    condbr
  bb2 bb2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb3 bb3
    alloca Virtual { id: 12, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 13, bank: General, size_bits: 64 }, Virtual { id: 1, bank: General, size_bits: 64 }
    load Virtual { id: 14, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 13, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 15, bank: General, size_bits: 8 }, Virtual { id: 14, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 12, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 15, bank: General, size_bits: 8 }
    load Virtual { id: 17, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 12, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 18, bank: General, size_bits: 8 }, Virtual { id: 17, bank: General, size_bits: 8 }, 1
    condbr
  bb1 bb1
    load Virtual { id: 19, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb4 bb4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb5 bb5
    alloca Virtual { id: 21, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 22, bank: General, size_bits: 64 }, Virtual { id: 1, bank: General, size_bits: 64 }
    load Virtual { id: 23, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 22, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 24, bank: General, size_bits: 8 }, Virtual { id: 23, bank: General, size_bits: 64 }, 2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 24, bank: General, size_bits: 8 }
    alloca Virtual { id: 26, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 27, bank: General, size_bits: 64 }, Virtual { id: 1, bank: General, size_bits: 64 }
    gep Virtual { id: 28, bank: General, size_bits: 64 }, Virtual { id: 27, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 29, bank: General, size_bits: 64 }, Virtual { id: 28, bank: General, size_bits: 64 }
    load Virtual { id: 30, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 29, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 31, bank: General, size_bits: 8 }, Virtual { id: 30, bank: General, size_bits: 8 }, 255
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 26, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 31, bank: General, size_bits: 8 }
    alloca Virtual { id: 33, bank: General, size_bits: 64 }, 1
    load Virtual { id: 34, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 35, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 26, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    and Virtual { id: 36, bank: General, size_bits: 8 }, Virtual { id: 34, bank: General, size_bits: 8 }, Virtual { id: 35, bank: General, size_bits: 8 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 33, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 36, bank: General, size_bits: 8 }
    alloca Virtual { id: 38, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 39, bank: General, size_bits: 64 }, Virtual { id: 1, bank: General, size_bits: 64 }
    gep Virtual { id: 40, bank: General, size_bits: 64 }, Virtual { id: 39, bank: General, size_bits: 64 }, 9
    bitcast Virtual { id: 41, bank: General, size_bits: 64 }, Virtual { id: 40, bank: General, size_bits: 64 }
    load Virtual { id: 42, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 43, bank: General, size_bits: 8 }, Virtual { id: 42, bank: General, size_bits: 8 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 38, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 43, bank: General, size_bits: 8 }
    alloca Virtual { id: 45, bank: General, size_bits: 64 }, 1
    load Virtual { id: 46, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 33, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 47, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 38, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    and Virtual { id: 48, bank: General, size_bits: 8 }, Virtual { id: 46, bank: General, size_bits: 8 }, Virtual { id: 47, bank: General, size_bits: 8 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 45, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 48, bank: General, size_bits: 8 }
    alloca Virtual { id: 50, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 51, bank: General, size_bits: 64 }, Virtual { id: 1, bank: General, size_bits: 64 }
    gep Virtual { id: 52, bank: General, size_bits: 64 }, Virtual { id: 51, bank: General, size_bits: 64 }, 10
    bitcast Virtual { id: 53, bank: General, size_bits: 64 }, Virtual { id: 52, bank: General, size_bits: 64 }
    load Virtual { id: 54, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 53, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 55, bank: General, size_bits: 8 }, Virtual { id: 54, bank: General, size_bits: 8 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 50, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 55, bank: General, size_bits: 8 }
    alloca Virtual { id: 57, bank: General, size_bits: 64 }, 1
    load Virtual { id: 58, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 45, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 59, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 50, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    and Virtual { id: 60, bank: General, size_bits: 8 }, Virtual { id: 58, bank: General, size_bits: 8 }, Virtual { id: 59, bank: General, size_bits: 8 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 57, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 60, bank: General, size_bits: 8 }
    load Virtual { id: 62, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 57, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 63, bank: General, size_bits: 8 }, Virtual { id: 62, bank: General, size_bits: 8 }, 1
    condbr
  bb6 bb6
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb7 bb7
    alloca Virtual { id: 65, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 66, bank: General, size_bits: 64 }, Virtual { id: 1, bank: General, size_bits: 64 }
    load Virtual { id: 67, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 66, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 68, bank: General, size_bits: 8 }, Virtual { id: 67, bank: General, size_bits: 64 }, 2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 68, bank: General, size_bits: 8 }
    load Virtual { id: 70, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 71, bank: General, size_bits: 8 }, Virtual { id: 70, bank: General, size_bits: 8 }, 1
    condbr
  bb8 bb8
    alloca Virtual { id: 72, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 73, bank: General, size_bits: 64 }, Virtual { id: 1, bank: General, size_bits: 64 }
    gep Virtual { id: 74, bank: General, size_bits: 64 }, Virtual { id: 73, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 75, bank: General, size_bits: 64 }, Virtual { id: 74, bank: General, size_bits: 64 }
    load Virtual { id: 76, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 75, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 72, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 76, bank: General, size_bits: 8 }
    alloca Virtual { id: 78, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 79, bank: General, size_bits: 64 }, Virtual { id: 1, bank: General, size_bits: 64 }
    gep Virtual { id: 80, bank: General, size_bits: 64 }, Virtual { id: 79, bank: General, size_bits: 64 }, 9
    bitcast Virtual { id: 81, bank: General, size_bits: 64 }, Virtual { id: 80, bank: General, size_bits: 64 }
    load Virtual { id: 82, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 81, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 78, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 82, bank: General, size_bits: 8 }
    alloca Virtual { id: 84, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 85, bank: General, size_bits: 64 }, Virtual { id: 1, bank: General, size_bits: 64 }
    gep Virtual { id: 86, bank: General, size_bits: 64 }, Virtual { id: 85, bank: General, size_bits: 64 }, 10
    bitcast Virtual { id: 87, bank: General, size_bits: 64 }, Virtual { id: 86, bank: General, size_bits: 64 }
    load Virtual { id: 88, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 87, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 84, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 88, bank: General, size_bits: 8 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb9 bb9
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
fn examples__12_pattern_matching__classify
  bb0 bb0
    alloca Virtual { id: 92, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 93, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 93, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    alloca Virtual { id: 95, bank: General, size_bits: 64 }, 1
    load Virtual { id: 96, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 93, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 97, bank: General, size_bits: 8 }, Virtual { id: 96, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 95, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 97, bank: General, size_bits: 8 }
    load Virtual { id: 99, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 95, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 100, bank: General, size_bits: 8 }, Virtual { id: 99, bank: General, size_bits: 8 }, 1
    condbr
  bb2 bb2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb3 bb3
    br
  bb1 bb1
    load Virtual { id: 102, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb4 bb4
    alloca Virtual { id: 103, bank: General, size_bits: 64 }, 1
    load Virtual { id: 104, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 93, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 103, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 104, bank: General, size_bits: 64 }
    alloca Virtual { id: 106, bank: General, size_bits: 64 }, 1
    load Virtual { id: 107, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 103, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 108, bank: General, size_bits: 8 }, Virtual { id: 107, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 106, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 108, bank: General, size_bits: 8 }
    load Virtual { id: 110, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 106, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 111, bank: General, size_bits: 8 }, Virtual { id: 110, bank: General, size_bits: 8 }, 1
    condbr
  bb5 bb5
    br
  bb6 bb6
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb7 bb7
    alloca Virtual { id: 113, bank: General, size_bits: 64 }, 1
    load Virtual { id: 114, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 93, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 113, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 114, bank: General, size_bits: 64 }
    alloca Virtual { id: 116, bank: General, size_bits: 64 }, 1
    load Virtual { id: 117, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 113, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    rem Virtual { id: 118, bank: General, size_bits: 64 }, Virtual { id: 117, bank: General, size_bits: 64 }, 2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 116, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 118, bank: General, size_bits: 64 }
    alloca Virtual { id: 120, bank: General, size_bits: 64 }, 1
    load Virtual { id: 121, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 116, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 122, bank: General, size_bits: 8 }, Virtual { id: 121, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 120, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 122, bank: General, size_bits: 8 }
    load Virtual { id: 124, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 120, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 125, bank: General, size_bits: 8 }, Virtual { id: 124, bank: General, size_bits: 8 }, 1
    condbr
  bb8 bb8
    br
  bb9 bb9
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb10 bb10
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    br
  bb11 bb11
    load Virtual { id: 128, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn examples__12_pattern_matching__unwrap_or
  bb0 bb0
    alloca Virtual { id: 129, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 130, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 130, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    alloca Virtual { id: 132, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 133, bank: General, size_bits: 64 }, Virtual { id: 130, bank: General, size_bits: 64 }
    load Virtual { id: 134, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 133, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 135, bank: General, size_bits: 8 }, Virtual { id: 134, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 132, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 135, bank: General, size_bits: 8 }
    load Virtual { id: 137, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 132, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 138, bank: General, size_bits: 8 }, Virtual { id: 137, bank: General, size_bits: 8 }, 1
    condbr
  bb2 bb2
    alloca Virtual { id: 139, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 140, bank: General, size_bits: 64 }, Virtual { id: 130, bank: General, size_bits: 64 }
    gep Virtual { id: 141, bank: General, size_bits: 64 }, Virtual { id: 140, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 142, bank: General, size_bits: 64 }, Virtual { id: 141, bank: General, size_bits: 64 }
    load Virtual { id: 143, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 142, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 139, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 143, bank: General, size_bits: 64 }
    load Virtual { id: 145, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 139, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 129, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 145, bank: General, size_bits: 64 }
    br
  bb3 bb3
    alloca Virtual { id: 147, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 148, bank: General, size_bits: 64 }, Virtual { id: 130, bank: General, size_bits: 64 }
    load Virtual { id: 149, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 148, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 150, bank: General, size_bits: 8 }, Virtual { id: 149, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 147, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 150, bank: General, size_bits: 8 }
    load Virtual { id: 152, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 147, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 153, bank: General, size_bits: 8 }, Virtual { id: 152, bank: General, size_bits: 8 }, 1
    condbr
  bb1 bb1
    load Virtual { id: 154, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 129, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb4 bb4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 129, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.2)
    br
  bb5 bb5
    br
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    alloca Virtual { id: 161, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 161, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    alloca Virtual { id: 163, bank: General, size_bits: 64 }, 1
    load Virtual { id: 164, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 161, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(11), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 163, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 164, bank: General, size_bits: 64 }
    alloca Virtual { id: 166, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 166, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    alloca Virtual { id: 168, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 168, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 163, bank: General, size_bits: 64 }
    load Virtual { id: 170, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 168, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(examples__12_pattern_matching__describe)(v170) cc=C tail=false
    alloca Virtual { id: 172, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 172, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 171, bank: General, size_bits: 64 }
    br
  bb1 bb1
    bitcast Virtual { id: 174, bank: General, size_bits: 64 }, Virtual { id: 172, bank: General, size_bits: 64 }
    load Virtual { id: 175, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 174, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 175, bank: General, size_bits: 64 }
    alloca Virtual { id: 177, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 177, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 166, bank: General, size_bits: 64 }
    load Virtual { id: 179, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 177, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(examples__12_pattern_matching__describe)(v179) cc=C tail=false
    alloca Virtual { id: 181, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 181, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 180, bank: General, size_bits: 64 }
    br
  bb2 bb2
    bitcast Virtual { id: 183, bank: General, size_bits: 64 }, Virtual { id: 181, bank: General, size_bits: 64 }
    load Virtual { id: 184, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 183, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 184, bank: General, size_bits: 64 }
    alloca Virtual { id: 186, bank: General, size_bits: 64 }, 1
    sub Virtual { id: 187, bank: General, size_bits: 64 }, 0, 5
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 186, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 187, bank: General, size_bits: 64 }
    load Virtual { id: 189, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 186, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(examples__12_pattern_matching__classify)(v189) cc=C tail=false
    alloca Virtual { id: 191, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 191, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 190, bank: General, size_bits: 64 }
    br
  bb3 bb3
    bitcast Virtual { id: 193, bank: General, size_bits: 64 }, Virtual { id: 191, bank: General, size_bits: 64 }
    load Virtual { id: 194, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 193, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 194, bank: General, size_bits: 64 }
    call symbol(examples__12_pattern_matching__classify)(0) cc=C tail=false
    alloca Virtual { id: 197, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 197, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 196, bank: General, size_bits: 64 }
    br
  bb4 bb4
    bitcast Virtual { id: 199, bank: General, size_bits: 64 }, Virtual { id: 197, bank: General, size_bits: 64 }
    load Virtual { id: 200, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 199, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 200, bank: General, size_bits: 64 }
    call symbol(examples__12_pattern_matching__classify)(4) cc=C tail=false
    alloca Virtual { id: 203, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 203, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 202, bank: General, size_bits: 64 }
    br
  bb5 bb5
    bitcast Virtual { id: 205, bank: General, size_bits: 64 }, Virtual { id: 203, bank: General, size_bits: 64 }
    load Virtual { id: 206, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 205, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 206, bank: General, size_bits: 64 }
    call symbol(examples__12_pattern_matching__classify)(7) cc=C tail=false
    alloca Virtual { id: 209, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 209, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 208, bank: General, size_bits: 64 }
    br
  bb6 bb6
    bitcast Virtual { id: 211, bank: General, size_bits: 64 }, Virtual { id: 209, bank: General, size_bits: 64 }
    load Virtual { id: 212, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 211, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 212, bank: General, size_bits: 64 }
    alloca Virtual { id: 214, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 214, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    load Virtual { id: 216, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 214, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(examples__12_pattern_matching__unwrap_or)(v216, 0) cc=C tail=false
    br
  bb7 bb7
    intrinsic.call symbol(intrinsic.println), Virtual { id: 217, bank: General, size_bits: 64 }
    alloca Virtual { id: 219, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 219, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(const.struct)
    load Virtual { id: 221, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 219, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(examples__12_pattern_matching__unwrap_or)(v221, 99) cc=C tail=false
    br
  bb8 bb8
    intrinsic.call symbol(intrinsic.println), Virtual { id: 222, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println), 65280
    ret
fn __fp_comptime_const_CODE_877573538394199265
  bb0 bb0
    alloca Virtual { id: 225, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 226, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 226, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    alloca Virtual { id: 228, bank: General, size_bits: 64 }, 1
    load Virtual { id: 229, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 226, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 230, bank: General, size_bits: 8 }, Virtual { id: 229, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 228, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 230, bank: General, size_bits: 8 }
    load Virtual { id: 232, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 228, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 233, bank: General, size_bits: 8 }, Virtual { id: 232, bank: General, size_bits: 8 }, 1
    condbr
  bb2 bb2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 225, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 16711680
    br
  bb3 bb3
    alloca Virtual { id: 235, bank: General, size_bits: 64 }, 1
    load Virtual { id: 236, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 226, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 237, bank: General, size_bits: 8 }, Virtual { id: 236, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 235, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 237, bank: General, size_bits: 8 }
    load Virtual { id: 239, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 235, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 240, bank: General, size_bits: 8 }, Virtual { id: 239, bank: General, size_bits: 8 }, 1
    condbr
  bb1 bb1
    load Virtual { id: 241, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 225, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb4 bb4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 225, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 65280
    br
  bb5 bb5
    br
  bb6 bb6
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 225, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    br
  bb7 bb7
    load Virtual { id: 244, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 225, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret


Symbols:
  examples__12_pattern_matching__describe 0x00000000
  examples__12_pattern_matching__classify 0x000005f8
  examples__12_pattern_matching__unwrap_or 0x00000974
  main                             0x00000b3c
  __fp_comptime_const_CODE_877573538394199265 0x000011a8

Text relocations:
  offset=0x000000f0 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x000001f4 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x00000450 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x000005bc kind=Aarch64AdrpAdd symbol=__const_data_3 addend=0
  offset=0x00000698 kind=Aarch64AdrpAdd symbol=__const_data_4 addend=0
  offset=0x000007b8 kind=Aarch64AdrpAdd symbol=__const_data_5 addend=0
  offset=0x000008b0 kind=Aarch64AdrpAdd symbol=__const_data_6 addend=0
  offset=0x000008e8 kind=Aarch64AdrpAdd symbol=__const_data_7 addend=0
  offset=0x00000b50 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000b5c kind=CallRel32 symbol=printf addend=0
  offset=0x00000b60 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000b6c kind=CallRel32 symbol=printf addend=0
  offset=0x00000b70 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000b7c kind=CallRel32 symbol=printf addend=0
  offset=0x00000b80 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000b8c kind=CallRel32 symbol=printf addend=0
  offset=0x00000b90 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000b9c kind=CallRel32 symbol=printf addend=0
  offset=0x00000d58 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000d70 kind=CallRel32 symbol=printf addend=0
  offset=0x00000df8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000e10 kind=CallRel32 symbol=printf addend=0
  offset=0x00000ea4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000ebc kind=CallRel32 symbol=printf addend=0
  offset=0x00000f20 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000f38 kind=CallRel32 symbol=printf addend=0
  offset=0x00000f9c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000fb4 kind=CallRel32 symbol=printf addend=0
  offset=0x00001018 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00001030 kind=CallRel32 symbol=printf addend=0
  offset=0x000010bc kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000010d4 kind=CallRel32 symbol=printf addend=0
  offset=0x00001154 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000116c kind=CallRel32 symbol=printf addend=0
  offset=0x00001170 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00001188 kind=CallRel32 symbol=printf addend=0

.text (4888 bytes):
  00000000  ff c3 10 d1 f0 03 00 91  10 82 10 91 1d 7a 00 a9 
  00000010  fd 03 00 91 e0 bb 01 f9  e1 7b 01 f9 f0 03 00 91 
  00000020  10 62 0e 91 f0 03 00 f9  f0 03 00 91 10 a2 0e 91 
  00000030  f0 07 00 f9 f1 7b 41 f9  e9 03 11 aa 30 01 40 f9 
  00000040  f0 bf 01 f9 e9 03 11 aa  29 21 00 91 30 01 40 f9 
  00000050  f0 c3 01 f9 f0 03 00 91  10 e2 0d 91 f0 0b 00 f9 
  00000060  f1 07 40 f9 f0 bf 41 f9  e9 03 11 aa 30 01 00 f9 
  00000070  f0 c3 41 f9 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  00000080  f0 03 00 91 10 e2 0e 91  f0 13 00 f9 f0 07 40 f9 
  00000090  f0 17 00 f9 f0 17 40 f9  11 02 40 f9 f1 1b 00 f9 
  000000a0  f0 1b 40 f9 1f 02 00 f1  f0 17 9f 9a f0 1f 00 f9 
  000000b0  f1 13 40 f9 f0 e3 40 39  30 02 00 39 f0 13 40 f9 
  000000c0  11 02 40 39 f1 27 00 f9  f0 23 41 39 1f 06 00 f1 
  000000d0  f0 17 9f 9a f0 2b 00 f9  f0 2b 40 f9 1f 02 00 f1 
  000000e0  41 00 00 54 0f 00 00 14  f1 03 40 f9 eb 03 11 aa 
  000000f0  10 00 00 90 10 02 00 91  ea 03 0b aa 50 01 00 f9 
  00000100  70 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000110  ea 03 0b aa 4a 21 00 91  50 01 00 f9 1b 00 00 14 
  00000120  f0 03 00 91 10 02 0f 91  f0 33 00 f9 f0 07 40 f9 
  00000130  f0 37 00 f9 f0 37 40 f9  11 02 40 f9 f1 3b 00 f9 
  00000140  f0 3b 40 f9 1f 06 00 f1  f0 17 9f 9a f0 3f 00 f9 
  00000150  f1 33 40 f9 f0 e3 41 39  30 02 00 39 f0 33 40 f9 
  00000160  11 02 40 39 f1 47 00 f9  f0 23 42 39 1f 06 00 f1 
  00000170  f0 17 9f 9a f0 4b 00 f9  f0 4b 40 f9 1f 02 00 f1 
  00000180  61 03 00 54 28 00 00 14  f1 03 40 f9 e9 03 11 aa 
  00000190  30 01 40 f9 f0 c7 01 f9  e9 03 11 aa 29 21 00 91 
  000001a0  30 01 40 f9 f0 cb 01 f9  f0 03 00 91 10 22 0e 91 
  000001b0  f0 4f 00 f9 f1 bb 41 f9  f0 c7 41 f9 e9 03 11 aa 
  000001c0  30 01 00 f9 f0 cb 41 f9  e9 03 11 aa 29 21 00 91 
  000001d0  30 01 00 f9 bf 03 00 91  f0 03 00 91 10 82 10 91 
  000001e0  1d 7a 40 a9 ff c3 10 91  c0 03 5f d6 f1 03 40 f9 
  000001f0  eb 03 11 aa 10 00 00 90  10 02 00 91 ea 03 0b aa 
  00000200  50 01 00 f9 b0 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000210  10 00 e0 f2 ea 03 0b aa  4a 21 00 91 50 01 00 f9 
  00000220  da ff ff 17 f0 03 00 91  10 22 0f 91 f0 57 00 f9 
  00000230  f0 07 40 f9 f0 5b 00 f9  f0 5b 40 f9 11 02 40 f9 
  00000240  f1 5f 00 f9 f0 5f 40 f9  1f 0a 00 f1 f0 17 9f 9a 
  00000250  f0 63 00 f9 f1 57 40 f9  f0 03 43 39 30 02 00 39 
  00000260  f0 03 00 91 10 42 0f 91  f0 6b 00 f9 f0 07 40 f9 
  00000270  f0 6f 00 f9 f0 6f 40 f9  11 01 80 d2 10 02 11 8b 
  00000280  f0 73 00 f9 f0 73 40 f9  f0 77 00 f9 f0 77 40 f9 
  00000290  11 02 c0 39 f1 7b 00 f9  f0 c3 c3 39 1f fe 03 f1 
  000002a0  f0 17 9f 9a f0 7f 00 f9  f1 6b 40 f9 f0 e3 43 39 
  000002b0  30 02 00 39 f0 03 00 91  10 62 0f 91 f0 87 00 f9 
  000002c0  f0 57 40 f9 11 02 40 39  f1 8b 00 f9 f0 6b 40 f9 
  000002d0  11 02 40 39 f1 8f 00 f9  f0 43 44 39 f1 63 44 39 
  000002e0  10 02 11 8a f0 93 00 f9  f1 87 40 f9 f0 83 44 39 
  000002f0  30 02 00 39 f0 03 00 91  10 82 0f 91 f0 9b 00 f9 
  00000300  f0 07 40 f9 f0 9f 00 f9  f0 9f 40 f9 31 01 80 d2 
  00000310  10 02 11 8b f0 a3 00 f9  f0 a3 40 f9 f0 a7 00 f9 
  00000320  f0 a7 40 f9 11 02 c0 39  f1 ab 00 f9 f0 43 c5 39 
  00000330  1f 02 00 f1 f0 17 9f 9a  f0 af 00 f9 f1 9b 40 f9 
  00000340  f0 63 45 39 30 02 00 39  f0 03 00 91 10 a2 0f 91 
  00000350  f0 b7 00 f9 f0 87 40 f9  11 02 40 39 f1 bb 00 f9 
  00000360  f0 9b 40 f9 11 02 40 39  f1 bf 00 f9 f0 c3 45 39 
  00000370  f1 e3 45 39 10 02 11 8a  f0 c3 00 f9 f1 b7 40 f9 
  00000380  f0 03 46 39 30 02 00 39  f0 03 00 91 10 c2 0f 91 
  00000390  f0 cb 00 f9 f0 07 40 f9  f0 cf 00 f9 f0 cf 40 f9 
  000003a0  51 01 80 d2 10 02 11 8b  f0 d3 00 f9 f0 d3 40 f9 
  000003b0  f0 d7 00 f9 f0 d7 40 f9  11 02 c0 39 f1 db 00 f9 
  000003c0  f0 c3 c6 39 1f 02 00 f1  f0 17 9f 9a f0 df 00 f9 
  000003d0  f1 cb 40 f9 f0 e3 46 39  30 02 00 39 f0 03 00 91 
  000003e0  10 e2 0f 91 f0 e7 00 f9  f0 b7 40 f9 11 02 40 39 
  000003f0  f1 eb 00 f9 f0 cb 40 f9  11 02 40 39 f1 ef 00 f9 
  00000400  f0 43 47 39 f1 63 47 39  10 02 11 8a f0 f3 00 f9 
  00000410  f1 e7 40 f9 f0 83 47 39  30 02 00 39 f0 e7 40 f9 
  00000420  11 02 40 39 f1 fb 00 f9  f0 c3 47 39 1f 06 00 f1 
  00000430  f0 17 9f 9a f0 ff 00 f9  f0 ff 40 f9 1f 02 00 f1 
  00000440  41 00 00 54 0f 00 00 14  f1 03 40 f9 eb 03 11 aa 
  00000450  10 00 00 90 10 02 00 91  ea 03 0b aa 50 01 00 f9 
  00000460  f0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000470  ea 03 0b aa 4a 21 00 91  50 01 00 f9 43 ff ff 17 
  00000480  f0 03 00 91 10 02 10 91  f0 07 01 f9 f0 07 40 f9 
  00000490  f0 0b 01 f9 f0 0b 41 f9  11 02 40 f9 f1 0f 01 f9 
  000004a0  f0 0f 41 f9 1f 0a 00 f1  f0 17 9f 9a f0 13 01 f9 
  000004b0  f1 07 41 f9 f0 83 48 39  30 02 00 39 f0 07 41 f9 
  000004c0  11 02 40 39 f1 1b 01 f9  f0 c3 48 39 1f 06 00 f1 
  000004d0  f0 17 9f 9a f0 1f 01 f9  f0 1f 41 f9 1f 02 00 f1 
  000004e0  41 00 00 54 42 00 00 14  f0 03 00 91 10 22 10 91 
  000004f0  f0 23 01 f9 f0 07 40 f9  f0 27 01 f9 f0 27 41 f9 
  00000500  11 01 80 d2 10 02 11 8b  f0 2b 01 f9 f0 2b 41 f9 
  00000510  f0 2f 01 f9 f0 2f 41 f9  11 02 c0 39 f1 33 01 f9 
  00000520  f1 23 41 f9 f0 83 c9 39  30 02 00 39 f0 03 00 91 
  00000530  10 42 10 91 f0 3b 01 f9  f0 07 40 f9 f0 3f 01 f9 
  00000540  f0 3f 41 f9 31 01 80 d2  10 02 11 8b f0 43 01 f9 
  00000550  f0 43 41 f9 f0 47 01 f9  f0 47 41 f9 11 02 c0 39 
  00000560  f1 4b 01 f9 f1 3b 41 f9  f0 43 ca 39 30 02 00 39 
  00000570  f0 03 00 91 10 62 10 91  f0 53 01 f9 f0 07 40 f9 
  00000580  f0 57 01 f9 f0 57 41 f9  51 01 80 d2 10 02 11 8b 
  00000590  f0 5b 01 f9 f0 5b 41 f9  f0 5f 01 f9 f0 5f 41 f9 
  000005a0  11 02 c0 39 f1 63 01 f9  f1 53 41 f9 f0 03 cb 39 
  000005b0  30 02 00 39 f1 03 40 f9  eb 03 11 aa 10 00 00 90 
  000005c0  10 02 00 91 ea 03 0b aa  50 01 00 f9 50 01 80 d2 
  000005d0  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 ea 03 0b aa 
  000005e0  4a 21 00 91 50 01 00 f9  e8 fe ff 17 f1 03 40 f9 
  000005f0  eb 03 11 aa e5 fe ff 17  ff 03 08 d1 fd 7b 1f a9 
  00000600  fd 03 00 91 e0 bf 00 f9  e1 9f 00 f9 f0 03 00 91 
  00000610  10 82 06 91 f0 03 00 f9  f0 03 00 91 10 c2 06 91 
  00000620  f0 07 00 f9 f1 07 40 f9  f0 9f 40 f9 30 02 00 f9 
  00000630  f0 03 00 91 10 e2 06 91  f0 0f 00 f9 f0 07 40 f9 
  00000640  11 02 40 f9 f1 13 00 f9  f0 13 40 f9 1f 02 00 f1 
  00000650  f0 17 9f 9a f0 17 00 f9  f1 0f 40 f9 f0 a3 40 39 
  00000660  30 02 00 39 f0 0f 40 f9  11 02 40 39 f1 1f 00 f9 
  00000670  f0 e3 40 39 1f 06 00 f1  f0 17 9f 9a f0 23 00 f9 
  00000680  f0 23 40 f9 1f 02 00 f1  41 00 00 54 0f 00 00 14 
  00000690  f1 03 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  000006a0  ea 03 0b aa 50 01 00 f9  90 00 80 d2 10 00 a0 f2 
  000006b0  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  000006c0  50 01 00 f9 02 00 00 14  18 00 00 14 f1 03 40 f9 
  000006d0  e9 03 11 aa 30 01 40 f9  f0 c3 00 f9 e9 03 11 aa 
  000006e0  29 21 00 91 30 01 40 f9  f0 c7 00 f9 f0 03 00 91 
  000006f0  10 02 06 91 f0 2b 00 f9  f1 bf 40 f9 f0 c3 40 f9 
  00000700  e9 03 11 aa 30 01 00 f9  f0 c7 40 f9 e9 03 11 aa 
  00000710  29 21 00 91 30 01 00 f9  bf 03 00 91 fd 7b 5f a9 
  00000720  ff 03 08 91 c0 03 5f d6  f0 03 00 91 10 02 07 91 
  00000730  f0 2f 00 f9 f0 07 40 f9  11 02 40 f9 f1 33 00 f9 
  00000740  f1 2f 40 f9 f0 33 40 f9  30 02 00 f9 f0 03 00 91 
  00000750  10 22 07 91 f0 3b 00 f9  f0 2f 40 f9 11 02 40 f9 
  00000760  f1 3f 00 f9 f0 3f 40 f9  1f 02 00 f1 f0 a7 9f 9a 
  00000770  f0 43 00 f9 f1 3b 40 f9  f0 03 42 39 30 02 00 39 
  00000780  f0 3b 40 f9 11 02 40 39  f1 4b 00 f9 f0 43 42 39 
  00000790  1f 06 00 f1 f0 17 9f 9a  f0 4f 00 f9 f0 4f 40 f9 
  000007a0  1f 02 00 f1 61 00 00 54  01 00 00 14 0f 00 00 14 
  000007b0  f1 03 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  000007c0  ea 03 0b aa 50 01 00 f9  10 01 80 d2 10 00 a0 f2 
  000007d0  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  000007e0  50 01 00 f9 ba ff ff 17  f0 03 00 91 10 42 07 91 
  000007f0  f0 57 00 f9 f0 07 40 f9  11 02 40 f9 f1 5b 00 f9 
  00000800  f1 57 40 f9 f0 5b 40 f9  30 02 00 f9 f0 03 00 91 
  00000810  10 62 07 91 f0 63 00 f9  f0 57 40 f9 11 02 40 f9 
  00000820  f1 67 00 f9 f0 67 40 f9  51 00 80 d2 09 0e d1 9a 
  00000830  30 c1 11 9b f0 6b 00 f9  f1 63 40 f9 f0 6b 40 f9 
  00000840  30 02 00 f9 f0 03 00 91  10 82 07 91 f0 73 00 f9 
  00000850  f0 63 40 f9 11 02 40 f9  f1 77 00 f9 f0 77 40 f9 
  00000860  1f 02 00 f1 f0 17 9f 9a  f0 7b 00 f9 f1 73 40 f9 
  00000870  f0 c3 43 39 30 02 00 39  f0 73 40 f9 11 02 40 39 
  00000880  f1 83 00 f9 f0 03 44 39  1f 06 00 f1 f0 17 9f 9a 
  00000890  f0 87 00 f9 f0 87 40 f9  1f 02 00 f1 61 00 00 54 
  000008a0  01 00 00 14 0f 00 00 14  f1 03 40 f9 eb 03 11 aa 
  000008b0  10 00 00 90 10 02 00 91  ea 03 0b aa 50 01 00 f9 
  000008c0  90 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  000008d0  ea 03 0b aa 4a 21 00 91  50 01 00 f9 7c ff ff 17 
  000008e0  f1 03 40 f9 eb 03 11 aa  10 00 00 90 10 02 00 91 
  000008f0  ea 03 0b aa 50 01 00 f9  70 00 80 d2 10 00 a0 f2 
  00000900  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 21 00 91 
  00000910  50 01 00 f9 6e ff ff 17  f1 03 40 f9 e9 03 11 aa 
  00000920  30 01 40 f9 f0 cb 00 f9  e9 03 11 aa 29 21 00 91 
  00000930  30 01 40 f9 f0 cf 00 f9  f0 03 00 91 10 42 06 91 
  00000940  f0 93 00 f9 f1 bf 40 f9  f0 cb 40 f9 e9 03 11 aa 
  00000950  30 01 00 f9 f0 cf 40 f9  e9 03 11 aa 29 21 00 91 
  00000960  30 01 00 f9 bf 03 00 91  fd 7b 5f a9 ff 03 08 91 
  00000970  c0 03 5f d6 ff 83 05 d1  fd 7b 15 a9 fd 03 00 91 
  00000980  e9 03 00 aa 30 01 40 f9  f0 73 00 f9 e9 03 00 aa 
  00000990  29 21 00 91 30 01 40 f9  f0 77 00 f9 e1 7b 00 f9 
  000009a0  f0 03 00 91 10 82 04 91  f0 03 00 f9 f0 03 00 91 
  000009b0  10 a2 04 91 f0 07 00 f9  f1 07 40 f9 f0 73 40 f9 
  000009c0  e9 03 11 aa 30 01 00 f9  f0 77 40 f9 e9 03 11 aa 
  000009d0  29 21 00 91 30 01 00 f9  f0 03 00 91 10 e2 04 91 
  000009e0  f0 0f 00 f9 f0 07 40 f9  f0 13 00 f9 f0 13 40 f9 
  000009f0  11 02 40 f9 f1 17 00 f9  f0 17 40 f9 1f 02 00 f1 
  00000a00  f0 17 9f 9a f0 1b 00 f9  f1 0f 40 f9 f0 c3 40 39 
  00000a10  30 02 00 39 f0 0f 40 f9  11 02 40 39 f1 23 00 f9 
  00000a20  f0 03 41 39 1f 06 00 f1  f0 17 9f 9a f0 27 00 f9 
  00000a30  f0 27 40 f9 1f 02 00 f1  41 00 00 54 19 00 00 14 
  00000a40  f0 03 00 91 10 02 05 91  f0 2b 00 f9 f0 07 40 f9 
  00000a50  f0 2f 00 f9 f0 2f 40 f9  11 01 80 d2 10 02 11 8b 
  00000a60  f0 33 00 f9 f0 33 40 f9  f0 37 00 f9 f0 37 40 f9 
  00000a70  11 02 40 f9 f1 3b 00 f9  f1 2b 40 f9 f0 3b 40 f9 
  00000a80  30 02 00 f9 f0 2b 40 f9  11 02 40 f9 f1 43 00 f9 
  00000a90  f1 03 40 f9 f0 43 40 f9  30 02 00 f9 1b 00 00 14 
  00000aa0  f0 03 00 91 10 22 05 91  f0 4b 00 f9 f0 07 40 f9 
  00000ab0  f0 4f 00 f9 f0 4f 40 f9  11 02 40 f9 f1 53 00 f9 
  00000ac0  f0 53 40 f9 1f 06 00 f1  f0 17 9f 9a f0 57 00 f9 
  00000ad0  f1 4b 40 f9 f0 a3 42 39  30 02 00 39 f0 4b 40 f9 
  00000ae0  11 02 40 39 f1 5f 00 f9  f0 e3 42 39 1f 06 00 f1 
  00000af0  f0 17 9f 9a f0 63 00 f9  f0 63 40 f9 1f 02 00 f1 
  00000b00  41 01 00 54 0d 00 00 14  f0 03 40 f9 11 02 40 f9 
  00000b10  f1 67 00 f9 e0 67 40 f9  bf 03 00 91 fd 7b 55 a9 
  00000b20  ff 83 05 91 c0 03 5f d6  f1 03 40 f9 f0 7b 40 f9 
  00000b30  30 02 00 f9 f5 ff ff 17  f4 ff ff 17 ff c3 13 d1 
  00000b40  f0 03 00 91 10 82 13 91  1d 7a 00 a9 fd 03 00 91 
  00000b50  00 00 00 90 00 00 00 91  00 e0 00 91 00 00 00 94 
  00000b60  00 00 00 90 00 00 00 91  00 80 01 91 00 00 00 94 
  00000b70  00 00 00 90 00 00 00 91  00 c0 02 91 00 00 00 94 
  00000b80  00 00 00 90 00 00 00 91  00 80 03 91 00 00 00 94 
  00000b90  00 00 00 90 00 00 00 91  00 20 04 91 00 00 00 94 
  00000ba0  f0 03 00 91 10 62 10 91  f0 1f 00 f9 f1 1f 40 f9 
  00000bb0  eb 03 11 aa 10 00 80 d2  10 00 a0 f2 10 00 c0 f2 
  00000bc0  10 00 e0 f2 ea 03 0b aa  50 01 00 f9 10 00 80 d2 
  00000bd0  ea 03 0b aa 4a 21 00 91  50 01 00 39 10 00 80 d2 
  00000be0  ea 03 0b aa 4a 25 00 91  50 01 00 39 10 00 80 d2 
  00000bf0  ea 03 0b aa 4a 29 00 91  50 01 00 39 f0 03 00 91 
  00000c00  10 a2 10 91 f0 27 00 f9  f1 1f 40 f9 e9 03 11 aa 
  00000c10  30 01 40 f9 f0 c7 01 f9  e9 03 11 aa 29 21 00 91 
  00000c20  30 01 40 f9 f0 cb 01 f9  f0 03 00 91 10 22 0e 91 
  00000c30  f0 2b 00 f9 f1 27 40 f9  f0 c7 41 f9 e9 03 11 aa 
  00000c40  30 01 00 f9 f0 cb 41 f9  e9 03 11 aa 29 21 00 91 
  00000c50  30 01 00 f9 f0 03 00 91  10 e2 10 91 f0 33 00 f9 
  00000c60  f1 33 40 f9 eb 03 11 aa  50 00 80 d2 10 00 a0 f2 
  00000c70  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 50 01 00 f9 
  00000c80  10 10 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00000c90  ea 03 0b aa 4a 21 00 91  50 01 00 39 10 08 80 d2 
  00000ca0  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 ea 03 0b aa 
  00000cb0  4a 25 00 91 50 01 00 39  10 04 80 d2 10 00 a0 f2 
  00000cc0  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 4a 29 00 91 
  00000cd0  50 01 00 39 f0 03 00 91  10 22 11 91 f0 3b 00 f9 
  00000ce0  f1 3b 40 f9 f0 27 40 f9  30 02 00 f9 f0 3b 40 f9 
  00000cf0  11 02 40 f9 f1 43 00 f9  e0 03 00 91 00 60 0e 91 
  00000d00  e1 43 40 f9 bf fc ff 97  f0 03 00 91 10 62 0e 91 
  00000d10  f0 47 00 f9 f0 03 00 91  10 42 11 91 f0 4b 00 f9 
  00000d20  f1 4b 40 f9 f0 cf 41 f9  e9 03 11 aa 30 01 00 f9 
  00000d30  f0 d3 41 f9 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  00000d40  01 00 00 14 f0 4b 40 f9  f0 53 00 f9 f0 53 40 f9 
  00000d50  11 02 40 f9 f1 57 00 f9  00 00 00 90 00 00 00 91 
  00000d60  00 40 04 91 e1 57 40 f9  f0 57 40 f9 f0 03 00 f9 
  00000d70  00 00 00 94 f0 03 00 91  10 82 11 91 f0 5f 00 f9 
  00000d80  f1 5f 40 f9 f0 33 40 f9  30 02 00 f9 f0 5f 40 f9 
  00000d90  11 02 40 f9 f1 67 00 f9  e0 03 00 91 00 a0 0e 91 
  00000da0  e1 67 40 f9 97 fc ff 97  f0 03 00 91 10 a2 0e 91 
  00000db0  f0 6b 00 f9 f0 03 00 91  10 a2 11 91 f0 6f 00 f9 
  00000dc0  f1 6f 40 f9 f0 d7 41 f9  e9 03 11 aa 30 01 00 f9 
  00000dd0  f0 db 41 f9 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  00000de0  01 00 00 14 f0 6f 40 f9  f0 77 00 f9 f0 77 40 f9 
  00000df0  11 02 40 f9 f1 7b 00 f9  00 00 00 90 00 00 00 91 
  00000e00  00 a0 04 91 e1 7b 40 f9  f0 7b 40 f9 f0 03 00 f9 
  00000e10  00 00 00 94 f0 03 00 91  10 e2 11 91 f0 83 00 f9 
  00000e20  10 00 80 d2 10 16 00 d1  f0 87 00 f9 f1 83 40 f9 
  00000e30  f0 87 40 f9 30 02 00 f9  f0 83 40 f9 11 02 40 f9 
  00000e40  f1 8f 00 f9 e0 03 00 91  00 e0 0e 91 e1 8f 40 f9 
  00000e50  ea fd ff 97 f0 03 00 91  10 e2 0e 91 f0 93 00 f9 
  00000e60  f0 03 00 91 10 02 12 91  f0 97 00 f9 f1 97 40 f9 
  00000e70  f0 df 41 f9 e9 03 11 aa  30 01 00 f9 f0 e3 41 f9 
  00000e80  e9 03 11 aa 29 21 00 91  30 01 00 f9 01 00 00 14 
  00000e90  f0 97 40 f9 f0 9f 00 f9  f0 9f 40 f9 11 02 40 f9 
  00000ea0  f1 a3 00 f9 00 00 00 90  00 00 00 91 00 00 05 91 
  00000eb0  e1 a3 40 f9 f0 a3 40 f9  f0 03 00 f9 00 00 00 94 
  00000ec0  e0 03 00 91 00 20 0f 91  01 00 80 d2 cb fd ff 97 
  00000ed0  f0 03 00 91 10 22 0f 91  f0 ab 00 f9 f0 03 00 91 
  00000ee0  10 42 12 91 f0 af 00 f9  f1 af 40 f9 f0 e7 41 f9 
  00000ef0  e9 03 11 aa 30 01 00 f9  f0 eb 41 f9 e9 03 11 aa 
  00000f00  29 21 00 91 30 01 00 f9  01 00 00 14 f0 af 40 f9 
  00000f10  f0 b7 00 f9 f0 b7 40 f9  11 02 40 f9 f1 bb 00 f9 
  00000f20  00 00 00 90 00 00 00 91  00 60 05 91 e1 bb 40 f9 
  00000f30  f0 bb 40 f9 f0 03 00 f9  00 00 00 94 e0 03 00 91 
  00000f40  00 60 0f 91 81 00 80 d2  ac fd ff 97 f0 03 00 91 
  00000f50  10 62 0f 91 f0 c3 00 f9  f0 03 00 91 10 82 12 91 
  00000f60  f0 c7 00 f9 f1 c7 40 f9  f0 ef 41 f9 e9 03 11 aa 
  00000f70  30 01 00 f9 f0 f3 41 f9  e9 03 11 aa 29 21 00 91 
  00000f80  30 01 00 f9 01 00 00 14  f0 c7 40 f9 f0 cf 00 f9 
  00000f90  f0 cf 40 f9 11 02 40 f9  f1 d3 00 f9 00 00 00 90 
  00000fa0  00 00 00 91 00 c0 05 91  e1 d3 40 f9 f0 d3 40 f9 
  00000fb0  f0 03 00 f9 00 00 00 94  e0 03 00 91 00 a0 0f 91 
  00000fc0  e1 00 80 d2 8d fd ff 97  f0 03 00 91 10 a2 0f 91 
  00000fd0  f0 db 00 f9 f0 03 00 91  10 c2 12 91 f0 df 00 f9 
  00000fe0  f1 df 40 f9 f0 f7 41 f9  e9 03 11 aa 30 01 00 f9 
  00000ff0  f0 fb 41 f9 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  00001000  01 00 00 14 f0 df 40 f9  f0 e7 00 f9 f0 e7 40 f9 
  00001010  11 02 40 f9 f1 eb 00 f9  00 00 00 90 00 00 00 91 
  00001020  00 20 06 91 e1 eb 40 f9  f0 eb 40 f9 f0 03 00 f9 
  00001030  00 00 00 94 f0 03 00 91  10 02 13 91 f0 f3 00 f9 
  00001040  f1 f3 40 f9 eb 03 11 aa  10 00 80 d2 10 00 a0 f2 
  00001050  10 00 c0 f2 10 00 e0 f2  ea 03 0b aa 50 01 00 f9 
  00001060  50 05 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  00001070  ea 03 0b aa 4a 21 00 91  50 01 00 f9 f1 f3 40 f9 
  00001080  e9 03 11 aa 30 01 40 f9  f0 ff 01 f9 e9 03 11 aa 
  00001090  29 21 00 91 30 01 40 f9  f0 03 02 f9 f0 03 00 91 
  000010a0  10 e2 0f 91 f0 fb 00 f9  e0 fb 40 f9 01 00 80 d2 
  000010b0  31 fe ff 97 e0 ff 00 f9  01 00 00 14 00 00 00 90 
  000010c0  00 00 00 91 00 80 06 91  e1 ff 40 f9 f0 ff 40 f9 
  000010d0  f0 03 00 f9 00 00 00 94  f0 03 00 91 10 42 13 91 
  000010e0  f0 07 01 f9 f1 07 41 f9  eb 03 11 aa 30 00 80 d2 
  000010f0  10 00 a0 f2 10 00 c0 f2  10 00 e0 f2 ea 03 0b aa 
  00001100  50 01 00 f9 10 00 80 d2  ea 03 0b aa 4a 21 00 91 
  00001110  50 01 00 f9 f1 07 41 f9  e9 03 11 aa 30 01 40 f9 
  00001120  f0 07 02 f9 e9 03 11 aa  29 21 00 91 30 01 40 f9 
  00001130  f0 0b 02 f9 f0 03 00 91  10 22 10 91 f0 0f 01 f9 
  00001140  e0 0f 41 f9 61 0c 80 d2  0b fe ff 97 e0 13 01 f9 
  00001150  01 00 00 14 00 00 00 90  00 00 00 91 00 00 07 91 
  00001160  e1 13 41 f9 f0 13 41 f9  f0 03 00 f9 00 00 00 94 
  00001170  00 00 00 90 00 00 00 91  00 80 07 91 01 e0 9f d2 
  00001180  10 e0 9f d2 f0 03 00 f9  00 00 00 94 bf 03 00 91 
  00001190  f0 03 00 91 10 82 13 91  1d 7a 40 a9 ff c3 13 91 
  000011a0  00 00 80 d2 c0 03 5f d6  ff c3 03 d1 fd 7b 0e a9 
  000011b0  fd 03 00 91 f0 03 00 91  10 02 03 91 f0 03 00 f9 
  000011c0  f0 03 00 91 10 22 03 91  f0 07 00 f9 f1 07 40 f9 
  000011d0  30 00 80 d2 30 02 00 f9  f0 03 00 91 10 42 03 91 
  000011e0  f0 0f 00 f9 f0 07 40 f9  11 02 40 f9 f1 13 00 f9 
  000011f0  f0 13 40 f9 1f 02 00 f1  f0 17 9f 9a f0 17 00 f9 
  00001200  f1 0f 40 f9 f0 a3 40 39  30 02 00 39 f0 0f 40 f9 
  00001210  11 02 40 39 f1 1f 00 f9  f0 e3 40 39 1f 06 00 f1 
  00001220  f0 17 9f 9a f0 23 00 f9  f0 23 40 f9 1f 02 00 f1 
  00001230  41 00 00 54 08 00 00 14  f1 03 40 f9 10 00 80 d2 
  00001240  f0 1f a0 f2 10 00 c0 f2  10 00 e0 f2 30 02 00 f9 
  00001250  19 00 00 14 f0 03 00 91  10 62 03 91 f0 2b 00 f9 
  00001260  f0 07 40 f9 11 02 40 f9  f1 2f 00 f9 f0 2f 40 f9 
  00001270  1f 06 00 f1 f0 17 9f 9a  f0 33 00 f9 f1 2b 40 f9 
  00001280  f0 83 41 39 30 02 00 39  f0 2b 40 f9 11 02 40 39 
  00001290  f1 3b 00 f9 f0 c3 41 39  1f 06 00 f1 f0 17 9f 9a 
  000012a0  f0 3f 00 f9 f0 3f 40 f9  1f 02 00 f1 41 01 00 54 
  000012b0  0d 00 00 14 f0 03 40 f9  11 02 40 f9 f1 43 00 f9 
  000012c0  e0 43 40 f9 bf 03 00 91  fd 7b 4e a9 ff c3 03 91 
  000012d0  c0 03 5f d6 f1 03 40 f9  10 e0 9f d2 30 02 00 f9 
  000012e0  f5 ff ff 17 01 00 00 14  f1 03 40 f9 10 00 80 d2 
  000012f0  30 02 00 f9 f0 ff ff 17  f0 03 40 f9 11 02 40 f9 
  00001300  f1 4f 00 f9 e0 4f 40 f9  bf 03 00 91 fd 7b 4e a9 
  00001310  ff c3 03 91 c0 03 5f d6 

.rodata (488 bytes):
  00000000  72 65 64 00 67 72 65 65  6e 00 72 65 64 20 72 67 
  00000010  62 00 63 75 73 74 6f 6d  20 72 67 62 00 7a 65 72 
  00000020  6f 00 6e 65 67 61 74 69  76 65 00 65 76 65 6e 00 
  00000030  6f 64 64 00 00 00 00 00  f0 9f 93 98 20 54 75 74 
  00000040  6f 72 69 61 6c 3a 20 31  32 5f 70 61 74 74 65 72 
  00000050  6e 5f 6d 61 74 63 68 69  6e 67 2e 66 70 0a 00 00 
  00000060  f0 9f a7 ad 20 46 6f 63  75 73 3a 20 50 61 74 74 
  00000070  65 72 6e 20 6d 61 74 63  68 69 6e 67 3a 20 6d 61 
  00000080  74 63 68 20 65 78 70 72  65 73 73 69 6f 6e 73 20 
  00000090  77 69 74 68 20 67 75 61  72 64 73 20 61 6e 64 20 
  000000a0  64 65 73 74 72 75 63 74  75 72 69 6e 67 0a 00 00 
  000000b0  f0 9f a7 aa 20 57 68 61  74 20 74 6f 20 6c 6f 6f 
  000000c0  6b 20 66 6f 72 3a 20 6c  61 62 65 6c 65 64 20 6f 
  000000d0  75 74 70 75 74 73 20 62  65 6c 6f 77 0a 00 00 00 
  000000e0  e2 9c 85 20 45 78 70 65  63 74 61 74 69 6f 6e 3a 
  000000f0  20 6f 75 74 70 75 74 73  20 6d 61 74 63 68 20 6c 
  00000100  61 62 65 6c 73 0a 00 00  0a 00 00 00 00 00 00 00 
  00000110  64 65 73 63 72 69 62 65  28 72 65 64 29 20 3d 20 
  00000120  25 73 0a 00 00 00 00 00  64 65 73 63 72 69 62 65 
  00000130  28 72 67 62 29 20 3d 20  25 73 0a 00 00 00 00 00 
  00000140  63 6c 61 73 73 69 66 79  28 2d 35 29 20 3d 20 25 
  00000150  73 0a 00 00 00 00 00 00  63 6c 61 73 73 69 66 79 
  00000160  28 30 29 20 3d 20 25 73  0a 00 00 00 00 00 00 00 
  00000170  63 6c 61 73 73 69 66 79  28 34 29 20 3d 20 25 73 
  00000180  0a 00 00 00 00 00 00 00  63 6c 61 73 73 69 66 79 
  00000190  28 37 29 20 3d 20 25 73  0a 00 00 00 00 00 00 00 
  000001a0  75 6e 77 72 61 70 5f 6f  72 28 53 6f 6d 65 28 34 
  000001b0  32 29 2c 20 30 29 20 3d  20 25 6c 6c 64 0a 00 00 
  000001c0  75 6e 77 72 61 70 5f 6f  72 28 4e 6f 6e 65 2c 20 
  000001d0  39 39 29 20 3d 20 25 6c  6c 64 0a 00 00 00 00 00 
  000001e0  30 78 25 30 36 58 0a 00 
