fp-native dump: format=MachO arch=Aarch64 entry=0x4c4

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global FACTORIAL_CONST ty=I64 constant=true initializer=Some(Bytes([120, 0, 0, 0, 0, 0, 0, 0]))
fn find_first_divisor
  bb0 bb0
    alloca Virtual { id: 2, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 3, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 2
    br
  bb1 bb1
    br
  bb2 bb2
    alloca Virtual { id: 5, bank: General, size_bits: 64 }, 8
    load Virtual { id: 6, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 7, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 8, bank: General, size_bits: 64 }, Virtual { id: 6, bank: General, size_bits: 64 }, Virtual { id: 7, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 8, bank: General, size_bits: 64 }
    alloca Virtual { id: 10, bank: General, size_bits: 64 }, 1
    load Virtual { id: 11, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    gt Virtual { id: 12, bank: General, size_bits: 8 }, Virtual { id: 11, bank: General, size_bits: 64 }, symbol(local.1)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 10, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 12, bank: General, size_bits: 8 }
    load Virtual { id: 14, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 10, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 15, bank: General, size_bits: 8 }, Virtual { id: 14, bank: General, size_bits: 8 }, 1
    condbr
  bb4 bb4
    alloca Virtual { id: 16, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 16, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    load Virtual { id: 18, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 16, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 18, bank: General, size_bits: 64 }
    br
  bb5 bb5
    br
  bb3 bb3
    load Virtual { id: 20, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb6 bb6
    alloca Virtual { id: 21, bank: General, size_bits: 64 }, 8
    load Virtual { id: 22, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    rem Virtual { id: 23, bank: General, size_bits: 64 }, symbol(local.1), Virtual { id: 22, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 23, bank: General, size_bits: 64 }
    alloca Virtual { id: 25, bank: General, size_bits: 64 }, 1
    load Virtual { id: 26, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 27, bank: General, size_bits: 8 }, Virtual { id: 26, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 25, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 27, bank: General, size_bits: 8 }
    load Virtual { id: 29, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 25, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 30, bank: General, size_bits: 8 }, Virtual { id: 29, bank: General, size_bits: 8 }, 1
    condbr
  bb8 bb8
    alloca Virtual { id: 31, bank: General, size_bits: 64 }, 8
    load Virtual { id: 32, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 31, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 32, bank: General, size_bits: 64 }
    load Virtual { id: 34, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 31, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 34, bank: General, size_bits: 64 }
    br
  bb9 bb9
    br
  bb10 bb10
    load Virtual { id: 36, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 37, bank: General, size_bits: 64 }, Virtual { id: 36, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 37, bank: General, size_bits: 64 }
    br
  bb7 bb7
    load Virtual { id: 39, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb11 bb11
    load Virtual { id: 40, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn sum_even_numbers
  bb0 bb0
    alloca Virtual { id: 41, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 42, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 43, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 42, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    br
  bb1 bb1
    alloca Virtual { id: 46, bank: General, size_bits: 64 }, 1
    load Virtual { id: 47, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 48, bank: General, size_bits: 8 }, Virtual { id: 47, bank: General, size_bits: 64 }, symbol(local.1)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 46, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 48, bank: General, size_bits: 8 }
    load Virtual { id: 50, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 46, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 51, bank: General, size_bits: 8 }, Virtual { id: 50, bank: General, size_bits: 8 }, 1
    condbr
  bb2 bb2
    load Virtual { id: 52, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 53, bank: General, size_bits: 64 }, Virtual { id: 52, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 53, bank: General, size_bits: 64 }
    alloca Virtual { id: 55, bank: General, size_bits: 64 }, 8
    load Virtual { id: 56, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    rem Virtual { id: 57, bank: General, size_bits: 64 }, Virtual { id: 56, bank: General, size_bits: 64 }, 2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 55, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 57, bank: General, size_bits: 64 }
    alloca Virtual { id: 59, bank: General, size_bits: 64 }, 1
    load Virtual { id: 60, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 55, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ne Virtual { id: 61, bank: General, size_bits: 8 }, Virtual { id: 60, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 59, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 61, bank: General, size_bits: 8 }
    load Virtual { id: 63, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 59, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 64, bank: General, size_bits: 8 }, Virtual { id: 63, bank: General, size_bits: 8 }, 1
    condbr
  bb3 bb3
    load Virtual { id: 65, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 42, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 43, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 65, bank: General, size_bits: 64 }
    load Virtual { id: 67, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 43, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb4 bb4
    br
  bb5 bb5
    br
  bb6 bb6
    load Virtual { id: 68, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 42, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 69, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 41, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 70, bank: General, size_bits: 64 }, Virtual { id: 68, bank: General, size_bits: 64 }, Virtual { id: 69, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 42, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 70, bank: General, size_bits: 64 }
    br
  bb7 bb7
    load Virtual { id: 72, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 43, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn main
  bb0 bb0
    alloca Virtual { id: 73, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 74, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 75, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 76, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 77, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 78, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 79, bank: General, size_bits: 64 }, 8
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    call symbol(factorial)(5) cc=C tail=false
    br
  bb1 bb1
    intrinsic.call symbol(intrinsic.println), Virtual { id: 87, bank: General, size_bits: 64 }
    call symbol(factorial)(7) cc=C tail=false
    br
  bb2 bb2
    intrinsic.call symbol(intrinsic.println), Virtual { id: 89, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 77, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 78, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    br
  bb3 bb3
    alloca Virtual { id: 94, bank: General, size_bits: 64 }, 1
    load Virtual { id: 95, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 78, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 96, bank: General, size_bits: 8 }, Virtual { id: 95, bank: General, size_bits: 64 }, 10
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 94, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 96, bank: General, size_bits: 8 }
    load Virtual { id: 98, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 94, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 99, bank: General, size_bits: 8 }, Virtual { id: 98, bank: General, size_bits: 8 }, 1
    condbr
  bb4 bb4
    alloca Virtual { id: 100, bank: General, size_bits: 64 }, 8
    load Virtual { id: 101, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 78, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 102, bank: General, size_bits: 64 }, Virtual { id: 101, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 100, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 102, bank: General, size_bits: 64 }
    load Virtual { id: 104, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 77, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 105, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 100, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 106, bank: General, size_bits: 64 }, Virtual { id: 104, bank: General, size_bits: 64 }, Virtual { id: 105, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 77, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 106, bank: General, size_bits: 64 }
    load Virtual { id: 108, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 78, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 109, bank: General, size_bits: 64 }, Virtual { id: 108, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 78, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 109, bank: General, size_bits: 64 }
    br
  bb5 bb5
    load Virtual { id: 111, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 77, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 111, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 73, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 75, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 5
    br
  bb6 bb6
    alloca Virtual { id: 115, bank: General, size_bits: 64 }, 1
    load Virtual { id: 116, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 75, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 117, bank: General, size_bits: 8 }, Virtual { id: 116, bank: General, size_bits: 64 }, 15
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 115, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 117, bank: General, size_bits: 8 }
    load Virtual { id: 119, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 115, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 120, bank: General, size_bits: 8 }, Virtual { id: 119, bank: General, size_bits: 8 }, 1
    condbr
  bb7 bb7
    alloca Virtual { id: 121, bank: General, size_bits: 64 }, 8
    load Virtual { id: 122, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 75, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 123, bank: General, size_bits: 64 }, Virtual { id: 122, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 121, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 123, bank: General, size_bits: 64 }
    load Virtual { id: 125, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 73, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 126, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 121, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 127, bank: General, size_bits: 64 }, Virtual { id: 125, bank: General, size_bits: 64 }, Virtual { id: 126, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 73, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 127, bank: General, size_bits: 64 }
    load Virtual { id: 129, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 75, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 130, bank: General, size_bits: 64 }, Virtual { id: 129, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 75, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 130, bank: General, size_bits: 64 }
    br
  bb8 bb8
    load Virtual { id: 132, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 73, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 132, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    call symbol(find_first_divisor)(24) cc=C tail=false
    br
  bb9 bb9
    intrinsic.call symbol(intrinsic.println), Virtual { id: 135, bank: General, size_bits: 64 }
    call symbol(find_first_divisor)(17) cc=C tail=false
    br
  bb10 bb10
    intrinsic.call symbol(intrinsic.println), Virtual { id: 137, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    call symbol(sum_even_numbers)(10) cc=C tail=false
    br
  bb11 bb11
    intrinsic.call symbol(intrinsic.println), Virtual { id: 140, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 74, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 79, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    br
  bb12 bb12
    alloca Virtual { id: 145, bank: General, size_bits: 64 }, 1
    load Virtual { id: 146, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 79, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 147, bank: General, size_bits: 8 }, Virtual { id: 146, bank: General, size_bits: 64 }, 4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 145, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 147, bank: General, size_bits: 8 }
    load Virtual { id: 149, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 145, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 150, bank: General, size_bits: 8 }, Virtual { id: 149, bank: General, size_bits: 8 }, 1
    condbr
  bb13 bb13
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 76, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    br
  bb14 bb14
    load Virtual { id: 152, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 74, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 152, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println), 120
    intrinsic.call symbol(intrinsic.println)
    ret
  bb15 bb15
    alloca Virtual { id: 157, bank: General, size_bits: 64 }, 1
    load Virtual { id: 158, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 76, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 159, bank: General, size_bits: 8 }, Virtual { id: 158, bank: General, size_bits: 64 }, 4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 157, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 159, bank: General, size_bits: 8 }
    load Virtual { id: 161, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 157, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 162, bank: General, size_bits: 8 }, Virtual { id: 161, bank: General, size_bits: 8 }, 1
    condbr
  bb16 bb16
    load Virtual { id: 163, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 74, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 164, bank: General, size_bits: 64 }, Virtual { id: 163, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 74, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 164, bank: General, size_bits: 64 }
    alloca Virtual { id: 166, bank: General, size_bits: 64 }, 1
    load Virtual { id: 167, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 79, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 168, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 76, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 169, bank: General, size_bits: 8 }, Virtual { id: 167, bank: General, size_bits: 64 }, Virtual { id: 168, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 166, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 169, bank: General, size_bits: 8 }
    load Virtual { id: 171, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 166, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 172, bank: General, size_bits: 8 }, Virtual { id: 171, bank: General, size_bits: 8 }, 1
    condbr
  bb17 bb17
    load Virtual { id: 173, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 79, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 174, bank: General, size_bits: 64 }, Virtual { id: 173, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 79, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 174, bank: General, size_bits: 64 }
    br
  bb18 bb18
    load Virtual { id: 176, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 79, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.print), Virtual { id: 176, bank: General, size_bits: 64 }
    br
  bb19 bb19
    br
  bb20 bb20
    load Virtual { id: 178, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 76, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 179, bank: General, size_bits: 64 }, Virtual { id: 178, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 76, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 179, bank: General, size_bits: 64 }
    br
fn factorial
  bb0 bb0
    alloca Virtual { id: 181, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 182, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 183, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 182, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 183, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    br
  bb1 bb1
    alloca Virtual { id: 186, bank: General, size_bits: 64 }, 1
    load Virtual { id: 187, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 183, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    le Virtual { id: 188, bank: General, size_bits: 8 }, Virtual { id: 187, bank: General, size_bits: 64 }, symbol(local.1)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 186, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 188, bank: General, size_bits: 8 }
    load Virtual { id: 190, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 186, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 191, bank: General, size_bits: 8 }, Virtual { id: 190, bank: General, size_bits: 8 }, 1
    condbr
  bb2 bb2
    load Virtual { id: 192, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 182, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 193, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 183, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 194, bank: General, size_bits: 64 }, Virtual { id: 192, bank: General, size_bits: 64 }, Virtual { id: 193, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 182, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 194, bank: General, size_bits: 64 }
    load Virtual { id: 196, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 183, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 197, bank: General, size_bits: 64 }, Virtual { id: 196, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 183, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 197, bank: General, size_bits: 64 }
    br
  bb3 bb3
    load Virtual { id: 199, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 182, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 181, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 199, bank: General, size_bits: 64 }
    load Virtual { id: 201, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 181, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret


Symbols:
  find_first_divisor               0x00000000
  sum_even_numbers                 0x000002a0
  main                             0x000004c4
  factorial                        0x00000b94

Text relocations:
  offset=0x00000530 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000053c kind=CallRel32 symbol=printf addend=0
  offset=0x00000540 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000054c kind=CallRel32 symbol=printf addend=0
  offset=0x00000550 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000055c kind=CallRel32 symbol=printf addend=0
  offset=0x00000560 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000056c kind=CallRel32 symbol=printf addend=0
  offset=0x00000570 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000057c kind=CallRel32 symbol=printf addend=0
  offset=0x00000580 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000058c kind=CallRel32 symbol=printf addend=0
  offset=0x00000590 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000059c kind=CallRel32 symbol=printf addend=0
  offset=0x000005b0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000005c8 kind=CallRel32 symbol=printf addend=0
  offset=0x000005dc kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000005f4 kind=CallRel32 symbol=printf addend=0
  offset=0x000005f8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000604 kind=CallRel32 symbol=printf addend=0
  offset=0x00000718 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000730 kind=CallRel32 symbol=printf addend=0
  offset=0x00000844 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000085c kind=CallRel32 symbol=printf addend=0
  offset=0x00000860 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000086c kind=CallRel32 symbol=printf addend=0
  offset=0x00000880 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000898 kind=CallRel32 symbol=printf addend=0
  offset=0x000008ac kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000008c4 kind=CallRel32 symbol=printf addend=0
  offset=0x000008c8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000008d4 kind=CallRel32 symbol=printf addend=0
  offset=0x000008e8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000900 kind=CallRel32 symbol=printf addend=0
  offset=0x00000904 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000910 kind=CallRel32 symbol=printf addend=0
  offset=0x000009ac kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000009c4 kind=CallRel32 symbol=printf addend=0
  offset=0x000009c8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000009d4 kind=CallRel32 symbol=printf addend=0
  offset=0x000009d8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000009f0 kind=CallRel32 symbol=printf addend=0
  offset=0x000009f4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000a00 kind=CallRel32 symbol=printf addend=0
  offset=0x00000b48 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000b60 kind=CallRel32 symbol=printf addend=0

.text (3312 bytes):
  00000000  ff 83 19 d1 f0 03 00 91  10 42 19 91 1d 7a 00 a9 
  00000010  fd 03 00 91 e0 3f 02 f9  1f 20 03 d5 f0 03 00 91 
  00000020  10 e2 12 91 f0 03 00 f9  f0 03 00 91 10 e2 13 91 
  00000030  f0 07 00 f9 f1 03 40 f9  50 00 80 d2 30 02 00 f9 
  00000040  01 00 00 14 01 00 00 14  f0 03 00 91 10 e2 14 91 
  00000050  f0 0f 00 f9 f0 03 40 f9  11 02 40 f9 f1 13 00 f9 
  00000060  f0 03 40 f9 11 02 40 f9  f1 17 00 f9 f0 13 40 f9 
  00000070  f1 17 40 f9 10 7e 11 9b  f0 1b 00 f9 f1 0f 40 f9 
  00000080  f0 1b 40 f9 30 02 00 f9  f0 03 00 91 10 e2 15 91 
  00000090  f0 23 00 f9 f0 0f 40 f9  11 02 40 f9 f1 27 00 f9 
  000000a0  f0 27 40 f9 f1 3f 42 f9  1f 02 11 eb f0 d7 9f 9a 
  000000b0  f0 2b 00 f9 f1 23 40 f9  f0 43 41 39 30 02 00 39 
  000000c0  f0 23 40 f9 11 02 40 39  f1 33 00 f9 f0 83 41 39 
  000000d0  1f 06 00 f1 f0 17 9f 9a  f0 37 00 f9 f0 37 40 f9 
  000000e0  1f 02 00 f1 41 00 00 54  0e 00 00 14 f0 03 00 91 
  000000f0  10 02 16 91 f0 3b 00 f9  f1 3b 40 f9 f0 3f 42 f9 
  00000100  30 02 00 f9 f0 3b 40 f9  11 02 40 f9 f1 43 00 f9 
  00000110  f1 07 40 f9 f0 43 40 f9  30 02 00 f9 02 00 00 14 
  00000120  0b 00 00 14 f0 07 40 f9  11 02 40 f9 f1 4b 00 f9 
  00000130  e0 4b 40 f9 bf 03 00 91  f0 03 00 91 10 42 19 91 
  00000140  1d 7a 40 a9 ff 83 19 91  c0 03 5f d6 f0 03 00 91 
  00000150  10 02 17 91 f0 4f 00 f9  f0 03 40 f9 11 02 40 f9 
  00000160  f1 53 00 f9 f0 3f 42 f9  f1 53 40 f9 09 0e d1 9a 
  00000170  30 c1 11 9b f0 57 00 f9  f1 4f 40 f9 f0 57 40 f9 
  00000180  30 02 00 f9 f0 03 00 91  10 02 18 91 f0 5f 00 f9 
  00000190  f0 4f 40 f9 11 02 40 f9  f1 63 00 f9 f0 63 40 f9 
  000001a0  1f 02 00 f1 f0 17 9f 9a  f0 67 00 f9 f1 5f 40 f9 
  000001b0  f0 23 43 39 30 02 00 39  f0 5f 40 f9 11 02 40 39 
  000001c0  f1 6f 00 f9 f0 63 43 39  1f 06 00 f1 f0 17 9f 9a 
  000001d0  f0 73 00 f9 f0 73 40 f9  1f 02 00 f1 41 00 00 54 
  000001e0  11 00 00 14 f0 03 00 91  10 22 18 91 f0 77 00 f9 
  000001f0  f0 03 40 f9 11 02 40 f9  f1 7b 00 f9 f1 77 40 f9 
  00000200  f0 7b 40 f9 30 02 00 f9  f0 77 40 f9 11 02 40 f9 
  00000210  f1 83 00 f9 f1 07 40 f9  f0 83 40 f9 30 02 00 f9 
  00000220  c1 ff ff 17 01 00 00 14  f0 03 40 f9 11 02 40 f9 
  00000230  f1 8b 00 f9 f0 8b 40 f9  10 06 00 91 f0 8f 00 f9 
  00000240  f1 03 40 f9 f0 8f 40 f9  30 02 00 f9 7e ff ff 17 
  00000250  f0 07 40 f9 11 02 40 f9  f1 97 00 f9 e0 97 40 f9 
  00000260  bf 03 00 91 f0 03 00 91  10 42 19 91 1d 7a 40 a9 
  00000270  ff 83 19 91 c0 03 5f d6  f0 07 40 f9 11 02 40 f9 
  00000280  f1 9b 00 f9 e0 9b 40 f9  bf 03 00 91 f0 03 00 91 
  00000290  10 42 19 91 1d 7a 40 a9  ff 83 19 91 c0 03 5f d6 
  000002a0  ff 03 17 d1 f0 03 00 91  10 c2 16 91 1d 7a 00 a9 
  000002b0  fd 03 00 91 e0 37 02 f9  1f 20 03 d5 f0 03 00 91 
  000002c0  10 62 12 91 f0 77 00 f9  f0 03 00 91 10 62 13 91 
  000002d0  f0 7b 00 f9 f0 03 00 91  10 62 14 91 f0 7f 00 f9 
  000002e0  f1 7b 40 f9 10 00 80 d2  30 02 00 f9 f1 77 40 f9 
  000002f0  10 00 80 d2 30 02 00 f9  01 00 00 14 f0 03 00 91 
  00000300  10 62 15 91 f0 8b 00 f9  f0 77 40 f9 11 02 40 f9 
  00000310  f1 8f 00 f9 f0 8f 40 f9  f1 37 42 f9 1f 02 11 eb 
  00000320  f0 a7 9f 9a f0 93 00 f9  f1 8b 40 f9 f0 83 44 39 
  00000330  30 02 00 39 f0 8b 40 f9  11 02 40 39 f1 9b 00 f9 
  00000340  f0 c3 44 39 1f 06 00 f1  f0 17 9f 9a f0 9f 00 f9 
  00000350  f0 9f 40 f9 1f 02 00 f1  41 00 00 54 30 00 00 14 
  00000360  f0 77 40 f9 11 02 40 f9  f1 a3 00 f9 f0 a3 40 f9 
  00000370  10 06 00 91 f0 a7 00 f9  f1 77 40 f9 f0 a7 40 f9 
  00000380  30 02 00 f9 f0 03 00 91  10 82 15 91 f0 af 00 f9 
  00000390  f0 77 40 f9 11 02 40 f9  f1 b3 00 f9 f0 b3 40 f9 
  000003a0  51 00 80 d2 09 0e d1 9a  30 c1 11 9b f0 b7 00 f9 
  000003b0  f1 af 40 f9 f0 b7 40 f9  30 02 00 f9 f0 03 00 91 
  000003c0  10 82 16 91 f0 bf 00 f9  f0 af 40 f9 11 02 40 f9 
  000003d0  f1 c3 00 f9 f0 c3 40 f9  1f 02 00 f1 f0 07 9f 9a 
  000003e0  f0 c7 00 f9 f1 bf 40 f9  f0 23 46 39 30 02 00 39 
  000003f0  f0 bf 40 f9 11 02 40 39  f1 cf 00 f9 f0 63 46 39 
  00000400  1f 06 00 f1 f0 17 9f 9a  f0 d3 00 f9 f0 d3 40 f9 
  00000410  1f 02 00 f1 41 02 00 54  12 00 00 14 f0 7b 40 f9 
  00000420  11 02 40 f9 f1 d7 00 f9  f1 7f 40 f9 f0 d7 40 f9 
  00000430  30 02 00 f9 f0 7f 40 f9  11 02 40 f9 f1 df 00 f9 
  00000440  e0 df 40 f9 bf 03 00 91  f0 03 00 91 10 c2 16 91 
  00000450  1d 7a 40 a9 ff 03 17 91  c0 03 5f d6 a8 ff ff 17 
  00000460  01 00 00 14 f0 7b 40 f9  11 02 40 f9 f1 e3 00 f9 
  00000470  f0 77 40 f9 11 02 40 f9  f1 e7 00 f9 f0 e3 40 f9 
  00000480  f1 e7 40 f9 10 02 11 8b  f0 eb 00 f9 f1 7b 40 f9 
  00000490  f0 eb 40 f9 30 02 00 f9  99 ff ff 17 f0 7f 40 f9 
  000004a0  11 02 40 f9 f1 f3 00 f9  e0 f3 40 f9 bf 03 00 91 
  000004b0  f0 03 00 91 10 c2 16 91  1d 7a 40 a9 ff 03 17 91 
  000004c0  c0 03 5f d6 ff c3 25 d1  f0 03 00 91 10 82 25 91 
  000004d0  1d 7a 00 a9 fd 03 00 91  1f 20 03 d5 f0 03 00 91 
  000004e0  10 e2 1b 91 f0 df 00 f9  f0 03 00 91 10 e2 1c 91 
  000004f0  f0 e3 00 f9 f0 03 00 91  10 e2 1d 91 f0 e7 00 f9 
  00000500  f0 03 00 91 10 e2 1e 91  f0 eb 00 f9 f0 03 00 91 
  00000510  10 e2 1f 91 f0 ef 00 f9  f0 03 00 91 10 e2 20 91 
  00000520  f0 f3 00 f9 f0 03 00 91  10 e2 21 91 f0 f7 00 f9 
  00000530  00 00 00 90 00 00 00 91  00 20 00 91 00 00 00 94 
  00000540  00 00 00 90 00 00 00 91  00 a0 00 91 00 00 00 94 
  00000550  00 00 00 90 00 00 00 91  00 80 01 91 00 00 00 94 
  00000560  00 00 00 90 00 00 00 91  00 40 02 91 00 00 00 94 
  00000570  00 00 00 90 00 00 00 91  00 e0 02 91 00 00 00 94 
  00000580  00 00 00 90 00 00 00 91  00 00 03 91 00 00 00 94 
  00000590  00 00 00 90 00 00 00 91  00 80 03 91 00 00 00 94 
  000005a0  a0 00 80 d2 7c 01 00 94  e0 17 01 f9 01 00 00 14 
  000005b0  00 00 00 90 00 00 00 91  00 00 04 91 e1 17 41 f9 
  000005c0  f0 17 41 f9 f0 03 00 f9  00 00 00 94 e0 00 80 d2 
  000005d0  71 01 00 94 e0 1f 01 f9  01 00 00 14 00 00 00 90 
  000005e0  00 00 00 91 00 40 04 91  e1 1f 41 f9 f0 1f 41 f9 
  000005f0  f0 03 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  00000600  00 80 04 91 00 00 00 94  f1 ef 40 f9 10 00 80 d2 
  00000610  30 02 00 f9 f1 f3 40 f9  30 00 80 d2 30 02 00 f9 
  00000620  01 00 00 14 f0 03 00 91  10 e2 22 91 f0 33 01 f9 
  00000630  f0 f3 40 f9 11 02 40 f9  f1 37 01 f9 f0 37 41 f9 
  00000640  1f 2a 00 f1 f0 a7 9f 9a  f0 3b 01 f9 f1 33 41 f9 
  00000650  f0 c3 49 39 30 02 00 39  f0 33 41 f9 11 02 40 39 
  00000660  f1 43 01 f9 f0 03 4a 39  1f 06 00 f1 f0 17 9f 9a 
  00000670  f0 47 01 f9 f0 47 41 f9  1f 02 00 f1 41 00 00 54 
  00000680  23 00 00 14 f0 03 00 91  10 02 23 91 f0 4b 01 f9 
  00000690  f0 f3 40 f9 11 02 40 f9  f1 4f 01 f9 f0 4f 41 f9 
  000006a0  f0 53 01 f9 f1 4b 41 f9  f0 53 41 f9 30 02 00 f9 
  000006b0  f0 ef 40 f9 11 02 40 f9  f1 5b 01 f9 f0 4b 41 f9 
  000006c0  11 02 40 f9 f1 5f 01 f9  f0 5b 41 f9 f1 5f 41 f9 
  000006d0  10 02 11 8b f0 63 01 f9  f1 ef 40 f9 f0 63 41 f9 
  000006e0  30 02 00 f9 f0 f3 40 f9  11 02 40 f9 f1 6b 01 f9 
  000006f0  f0 6b 41 f9 10 06 00 91  f0 6f 01 f9 f1 f3 40 f9 
  00000700  f0 6f 41 f9 30 02 00 f9  c7 ff ff 17 f0 ef 40 f9 
  00000710  11 02 40 f9 f1 77 01 f9  00 00 00 90 00 00 00 91 
  00000720  00 00 05 91 e1 77 41 f9  f0 77 41 f9 f0 03 00 f9 
  00000730  00 00 00 94 f1 df 40 f9  10 00 80 d2 30 02 00 f9 
  00000740  f1 e7 40 f9 b0 00 80 d2  30 02 00 f9 01 00 00 14 
  00000750  f0 03 00 91 10 02 24 91  f0 87 01 f9 f0 e7 40 f9 
  00000760  11 02 40 f9 f1 8b 01 f9  f0 8b 41 f9 1f 3e 00 f1 
  00000770  f0 a7 9f 9a f0 8f 01 f9  f1 87 41 f9 f0 63 4c 39 
  00000780  30 02 00 39 f0 87 41 f9  11 02 40 39 f1 97 01 f9 
  00000790  f0 a3 4c 39 1f 06 00 f1  f0 17 9f 9a f0 9b 01 f9 
  000007a0  f0 9b 41 f9 1f 02 00 f1  41 00 00 54 23 00 00 14 
  000007b0  f0 03 00 91 10 22 24 91  f0 9f 01 f9 f0 e7 40 f9 
  000007c0  11 02 40 f9 f1 a3 01 f9  f0 a3 41 f9 f0 a7 01 f9 
  000007d0  f1 9f 41 f9 f0 a7 41 f9  30 02 00 f9 f0 df 40 f9 
  000007e0  11 02 40 f9 f1 af 01 f9  f0 9f 41 f9 11 02 40 f9 
  000007f0  f1 b3 01 f9 f0 af 41 f9  f1 b3 41 f9 10 02 11 8b 
  00000800  f0 b7 01 f9 f1 df 40 f9  f0 b7 41 f9 30 02 00 f9 
  00000810  f0 e7 40 f9 11 02 40 f9  f1 bf 01 f9 f0 bf 41 f9 
  00000820  10 06 00 91 f0 c3 01 f9  f1 e7 40 f9 f0 c3 41 f9 
  00000830  30 02 00 f9 c7 ff ff 17  f0 df 40 f9 11 02 40 f9 
  00000840  f1 cb 01 f9 00 00 00 90  00 00 00 91 00 60 05 91 
  00000850  e1 cb 41 f9 f0 cb 41 f9  f0 03 00 f9 00 00 00 94 
  00000860  00 00 00 90 00 00 00 91  00 c0 05 91 00 00 00 94 
  00000870  00 03 80 d2 e3 fd ff 97  e0 d7 01 f9 01 00 00 14 
  00000880  00 00 00 90 00 00 00 91  00 60 06 91 e1 d7 41 f9 
  00000890  f0 d7 41 f9 f0 03 00 f9  00 00 00 94 20 02 80 d2 
  000008a0  d8 fd ff 97 e0 df 01 f9  01 00 00 14 00 00 00 90 
  000008b0  00 00 00 91 00 e0 06 91  e1 df 41 f9 f0 df 41 f9 
  000008c0  f0 03 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  000008d0  00 60 07 91 00 00 00 94  40 01 80 d2 71 fe ff 97 
  000008e0  e0 eb 01 f9 01 00 00 14  00 00 00 90 00 00 00 91 
  000008f0  00 e0 07 91 e1 eb 41 f9  f0 eb 41 f9 f0 03 00 f9 
  00000900  00 00 00 94 00 00 00 90  00 00 00 91 00 80 08 91 
  00000910  00 00 00 94 f1 e3 40 f9  10 00 80 d2 30 02 00 f9 
  00000920  f1 f7 40 f9 30 00 80 d2  30 02 00 f9 01 00 00 14 
  00000930  f0 03 00 91 10 22 25 91  f0 ff 01 f9 f0 f7 40 f9 
  00000940  11 02 40 f9 f1 03 02 f9  f0 03 42 f9 1f 12 00 f1 
  00000950  f0 a7 9f 9a f0 07 02 f9  f1 ff 41 f9 f0 23 50 39 
  00000960  30 02 00 39 f0 ff 41 f9  11 02 40 39 f1 0f 02 f9 
  00000970  f0 63 50 39 1f 06 00 f1  f0 17 9f 9a f0 13 02 f9 
  00000980  f0 13 42 f9 1f 02 00 f1  41 00 00 54 05 00 00 14 
  00000990  f1 eb 40 f9 30 00 80 d2  30 02 00 f9 21 00 00 14 
  000009a0  f0 e3 40 f9 11 02 40 f9  f1 1b 02 f9 00 00 00 90 
  000009b0  00 00 00 91 00 e0 08 91  e1 1b 42 f9 f0 1b 42 f9 
  000009c0  f0 03 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  000009d0  00 40 09 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000009e0  00 c0 09 91 01 0f 80 d2  10 0f 80 d2 f0 03 00 f9 
  000009f0  00 00 00 94 00 00 00 90  00 00 00 91 00 20 0a 91 
  00000a00  00 00 00 94 bf 03 00 91  f0 03 00 91 10 82 25 91 
  00000a10  1d 7a 40 a9 ff c3 25 91  00 00 80 d2 c0 03 5f d6 
  00000a20  f0 03 00 91 10 42 25 91  f0 2f 02 f9 f0 eb 40 f9 
  00000a30  11 02 40 f9 f1 33 02 f9  f0 33 42 f9 1f 12 00 f1 
  00000a40  f0 a7 9f 9a f0 37 02 f9  f1 2f 42 f9 f0 a3 51 39 
  00000a50  30 02 00 39 f0 2f 42 f9  11 02 40 39 f1 3f 02 f9 
  00000a60  f0 e3 51 39 1f 06 00 f1  f0 17 9f 9a f0 43 02 f9 
  00000a70  f0 43 42 f9 1f 02 00 f1  41 00 00 54 26 00 00 14 
  00000a80  f0 e3 40 f9 11 02 40 f9  f1 47 02 f9 f0 47 42 f9 
  00000a90  10 06 00 91 f0 4b 02 f9  f1 e3 40 f9 f0 4b 42 f9 
  00000aa0  30 02 00 f9 f0 03 00 91  10 62 25 91 f0 53 02 f9 
  00000ab0  f0 f7 40 f9 11 02 40 f9  f1 57 02 f9 f0 eb 40 f9 
  00000ac0  11 02 40 f9 f1 5b 02 f9  f0 57 42 f9 f1 5b 42 f9 
  00000ad0  1f 02 11 eb f0 17 9f 9a  f0 5f 02 f9 f1 53 42 f9 
  00000ae0  f0 e3 52 39 30 02 00 39  f0 53 42 f9 11 02 40 39 
  00000af0  f1 67 02 f9 f0 23 53 39  1f 06 00 f1 f0 17 9f 9a 
  00000b00  f0 6b 02 f9 f0 6b 42 f9  1f 02 00 f1 81 01 00 54 
  00000b10  16 00 00 14 f0 f7 40 f9  11 02 40 f9 f1 6f 02 f9 
  00000b20  f0 6f 42 f9 10 06 00 91  f0 73 02 f9 f1 f7 40 f9 
  00000b30  f0 73 42 f9 30 02 00 f9  7e ff ff 17 f0 f7 40 f9 
  00000b40  11 02 40 f9 f1 7b 02 f9  00 00 00 90 00 00 00 91 
  00000b50  00 c0 0a 91 e1 7b 42 f9  f0 7b 42 f9 f0 03 00 f9 
  00000b60  00 00 00 94 02 00 00 14  01 00 00 14 f0 eb 40 f9 
  00000b70  11 02 40 f9 f1 83 02 f9  f0 83 42 f9 10 06 00 91 
  00000b80  f0 87 02 f9 f1 eb 40 f9  f0 87 42 f9 30 02 00 f9 
  00000b90  a4 ff ff 17 ff 43 15 d1  f0 03 00 91 10 02 15 91 
  00000ba0  1d 7a 00 a9 fd 03 00 91  e0 2f 02 f9 1f 20 03 d5 
  00000bb0  f0 03 00 91 10 e2 11 91  f0 d7 01 f9 f0 03 00 91 
  00000bc0  10 e2 12 91 f0 db 01 f9  f0 03 00 91 10 e2 13 91 
  00000bd0  f0 df 01 f9 f1 db 41 f9  30 00 80 d2 30 02 00 f9 
  00000be0  f1 df 41 f9 30 00 80 d2  30 02 00 f9 01 00 00 14 
  00000bf0  f0 03 00 91 10 e2 14 91  f0 eb 01 f9 f0 df 41 f9 
  00000c00  11 02 40 f9 f1 ef 01 f9  f0 ef 41 f9 f1 2f 42 f9 
  00000c10  1f 02 11 eb f0 c7 9f 9a  f0 f3 01 f9 f1 eb 41 f9 
  00000c20  f0 83 4f 39 30 02 00 39  f0 eb 41 f9 11 02 40 39 
  00000c30  f1 fb 01 f9 f0 c3 4f 39  1f 06 00 f1 f0 17 9f 9a 
  00000c40  f0 ff 01 f9 f0 ff 41 f9  1f 02 00 f1 41 00 00 54 
  00000c50  18 00 00 14 f0 db 41 f9  11 02 40 f9 f1 03 02 f9 
  00000c60  f0 df 41 f9 11 02 40 f9  f1 07 02 f9 f0 03 42 f9 
  00000c70  f1 07 42 f9 10 7e 11 9b  f0 0b 02 f9 f1 db 41 f9 
  00000c80  f0 0b 42 f9 30 02 00 f9  f0 df 41 f9 11 02 40 f9 
  00000c90  f1 13 02 f9 f0 13 42 f9  10 06 00 91 f0 17 02 f9 
  00000ca0  f1 df 41 f9 f0 17 42 f9  30 02 00 f9 d1 ff ff 17 
  00000cb0  f0 db 41 f9 11 02 40 f9  f1 1f 02 f9 f1 d7 41 f9 
  00000cc0  f0 1f 42 f9 30 02 00 f9  f0 d7 41 f9 11 02 40 f9 
  00000cd0  f1 27 02 f9 e0 27 42 f9  bf 03 00 91 f0 03 00 91 
  00000ce0  10 02 15 91 1d 7a 40 a9  ff 43 15 91 c0 03 5f d6 

.rodata (696 bytes):
  00000000  78 00 00 00 00 00 00 00  f0 9f 93 98 20 54 75 74 
  00000010  6f 72 69 61 6c 3a 20 31  33 5f 6c 6f 6f 70 73 2e 
  00000020  66 70 0a 00 00 00 00 00  f0 9f a7 ad 20 46 6f 63 
  00000030  75 73 3a 20 4c 6f 6f 70  20 63 6f 6e 73 74 72 75 
  00000040  63 74 73 3a 20 77 68 69  6c 65 2c 20 66 6f 72 2c 
  00000050  20 61 6e 64 20 6c 6f 6f  70 2e 0a 00 00 00 00 00 
  00000060  f0 9f a7 aa 20 57 68 61  74 20 74 6f 20 6c 6f 6f 
  00000070  6b 20 66 6f 72 3a 20 6c  61 62 65 6c 65 64 20 6f 
  00000080  75 74 70 75 74 73 20 62  65 6c 6f 77 0a 00 00 00 
  00000090  e2 9c 85 20 45 78 70 65  63 74 61 74 69 6f 6e 3a 
  000000a0  20 6f 75 74 70 75 74 73  20 6d 61 74 63 68 20 6c 
  000000b0  61 62 65 6c 73 0a 00 00  0a 00 00 00 00 00 00 00 
  000000c0  3d 3d 3d 20 4c 6f 6f 70  20 43 6f 6e 73 74 72 75 
  000000d0  63 74 73 20 3d 3d 3d 0a  0a 00 00 00 00 00 00 00 
  000000e0  31 2e 20 57 68 69 6c 65  20 6c 6f 6f 70 20 2d 20 
  000000f0  66 61 63 74 6f 72 69 61  6c 3a 0a 00 00 00 00 00 
  00000100  20 20 35 21 20 3d 20 25  6c 6c 64 0a 00 00 00 00 
  00000110  20 20 37 21 20 3d 20 25  6c 6c 64 0a 00 00 00 00 
  00000120  0a 32 2e 20 46 6f 72 20  6c 6f 6f 70 20 2d 20 73 
  00000130  75 6d 20 72 61 6e 67 65  3a 0a 00 00 00 00 00 00 
  00000140  20 20 73 75 6d 28 31 2e  2e 31 30 29 20 3d 20 25 
  00000150  6c 6c 64 0a 00 00 00 00  20 20 73 75 6d 28 35 2e 
  00000160  2e 31 35 29 20 3d 20 25  6c 6c 64 0a 00 00 00 00 
  00000170  0a 33 2e 20 4c 6f 6f 70  20 77 69 74 68 20 62 72 
  00000180  65 61 6b 20 65 78 70 72  65 73 73 69 6f 6e 3a 0a 
  00000190  00 00 00 00 00 00 00 00  20 20 46 69 72 73 74 20 
  000001a0  64 69 76 69 73 6f 72 20  6f 66 20 32 34 3a 20 25 
  000001b0  6c 6c 64 0a 00 00 00 00  20 20 46 69 72 73 74 20 
  000001c0  64 69 76 69 73 6f 72 20  6f 66 20 31 37 3a 20 25 
  000001d0  6c 6c 64 0a 00 00 00 00  0a 34 2e 20 4c 6f 6f 70 
  000001e0  20 77 69 74 68 20 63 6f  6e 74 69 6e 75 65 3a 0a 
  000001f0  00 00 00 00 00 00 00 00  20 20 53 75 6d 20 6f 66 
  00000200  20 65 76 65 6e 20 6e 75  6d 62 65 72 73 20 3c 20 
  00000210  31 30 3a 20 25 6c 6c 64  0a 00 00 00 00 00 00 00 
  00000220  0a 35 2e 20 4e 65 73 74  65 64 20 6c 6f 6f 70 73 
  00000230  3a 0a 00 00 00 00 00 00  0a 20 20 49 74 65 72 61 
  00000240  74 69 6f 6e 73 3a 20 25  6c 6c 64 0a 00 00 00 00 
  00000250  0a 36 2e 20 43 6f 6d 70  69 6c 65 2d 74 69 6d 65 
  00000260  20 63 6f 6e 73 74 61 6e  74 3a 0a 00 00 00 00 00 
  00000270  20 20 63 6f 6e 73 74 20  35 21 20 3d 20 25 6c 6c 
  00000280  64 0a 00 00 00 00 00 00  0a e2 9c 93 20 4c 6f 6f 
  00000290  70 20 63 6f 6e 73 74 72  75 63 74 73 20 64 65 6d 
  000002a0  6f 6e 73 74 72 61 74 65  64 21 0a 00 00 00 00 00 
  000002b0  5b 25 6c 6c 75 5d 20 00 
