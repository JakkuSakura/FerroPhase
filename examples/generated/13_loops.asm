fp-native dump: format=MachO arch=Aarch64 entry=0x674

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
fn factorial
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 1, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 2, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    br
  bb1 bb1
    alloca Virtual { id: 5, bank: General, size_bits: 64 }, 1
    load Virtual { id: 6, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    le Virtual { id: 7, bank: General, size_bits: 8 }, Virtual { id: 6, bank: General, size_bits: 64 }, symbol(local.1)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 7, bank: General, size_bits: 64 }
    load Virtual { id: 9, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 10, bank: General, size_bits: 8 }, Virtual { id: 9, bank: General, size_bits: 64 }, 1
    condbr
  bb2 bb2
    load Virtual { id: 11, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 12, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 13, bank: General, size_bits: 64 }, Virtual { id: 11, bank: General, size_bits: 64 }, Virtual { id: 12, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 13, bank: General, size_bits: 64 }
    load Virtual { id: 15, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 16, bank: General, size_bits: 64 }, Virtual { id: 15, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 16, bank: General, size_bits: 64 }
    br
  bb3 bb3
    load Virtual { id: 18, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 2, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 18, bank: General, size_bits: 64 }
    load Virtual { id: 20, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn const_factorial
  bb0 bb0
    alloca Virtual { id: 21, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 22, bank: General, size_bits: 64 }, 1
    le Virtual { id: 23, bank: General, size_bits: 8 }, symbol(local.1), 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 22, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 23, bank: General, size_bits: 64 }
    load Virtual { id: 25, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 22, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 26, bank: General, size_bits: 8 }, Virtual { id: 25, bank: General, size_bits: 64 }, 1
    condbr
  bb1 bb1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    br
  bb2 bb2
    alloca Virtual { id: 28, bank: General, size_bits: 64 }, 1
    sub Virtual { id: 29, bank: General, size_bits: 64 }, symbol(local.1), 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 28, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 29, bank: General, size_bits: 64 }
    load Virtual { id: 31, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 28, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(const_factorial)(v31) cc=C tail=false
    br
  bb3 bb3
    load Virtual { id: 33, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb4 bb4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    mul Virtual { id: 35, bank: General, size_bits: 64 }, symbol(local.1), Virtual { id: 32, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 35, bank: General, size_bits: 64 }
    br
fn sum_range
  bb0 bb0
    alloca Virtual { id: 37, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 38, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 39, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 37, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 38, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    br
  bb1 bb1
    alloca Virtual { id: 42, bank: General, size_bits: 64 }, 1
    load Virtual { id: 43, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 38, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 44, bank: General, size_bits: 8 }, Virtual { id: 43, bank: General, size_bits: 64 }, symbol(local.2)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 42, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 44, bank: General, size_bits: 64 }
    load Virtual { id: 46, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 42, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 47, bank: General, size_bits: 8 }, Virtual { id: 46, bank: General, size_bits: 64 }, 1
    condbr
  bb2 bb2
    load Virtual { id: 48, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 37, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 49, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 38, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 50, bank: General, size_bits: 64 }, Virtual { id: 48, bank: General, size_bits: 64 }, Virtual { id: 49, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 37, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 50, bank: General, size_bits: 64 }
    load Virtual { id: 52, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 38, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 53, bank: General, size_bits: 64 }, Virtual { id: 52, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 38, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 53, bank: General, size_bits: 64 }
    br
  bb3 bb3
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 39, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    load Virtual { id: 56, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 39, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn find_first_divisor
  bb0 bb0
    alloca Virtual { id: 57, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 58, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 58, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 2
    br
  bb1 bb1
    br
  bb2 bb2
    alloca Virtual { id: 60, bank: General, size_bits: 64 }, 1
    load Virtual { id: 61, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 58, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 62, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 58, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 63, bank: General, size_bits: 64 }, Virtual { id: 61, bank: General, size_bits: 64 }, Virtual { id: 62, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 60, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 63, bank: General, size_bits: 64 }
    alloca Virtual { id: 65, bank: General, size_bits: 64 }, 1
    load Virtual { id: 66, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 60, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    gt Virtual { id: 67, bank: General, size_bits: 8 }, Virtual { id: 66, bank: General, size_bits: 64 }, symbol(local.1)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 67, bank: General, size_bits: 64 }
    load Virtual { id: 69, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 70, bank: General, size_bits: 8 }, Virtual { id: 69, bank: General, size_bits: 64 }, 1
    condbr
  bb4 bb4
    alloca Virtual { id: 71, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 71, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    load Virtual { id: 73, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 71, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 57, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 73, bank: General, size_bits: 64 }
    br
  bb5 bb5
    br
  bb3 bb3
    load Virtual { id: 75, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 57, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb6 bb6
    br
  bb7 bb7
    br
fn sum_even_numbers
  bb0 bb0
    alloca Virtual { id: 76, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 77, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 78, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 78, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 77, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    br
  bb1 bb1
    alloca Virtual { id: 81, bank: General, size_bits: 64 }, 1
    load Virtual { id: 82, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 77, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 83, bank: General, size_bits: 8 }, Virtual { id: 82, bank: General, size_bits: 64 }, symbol(local.1)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 81, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 83, bank: General, size_bits: 64 }
    load Virtual { id: 85, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 81, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 86, bank: General, size_bits: 8 }, Virtual { id: 85, bank: General, size_bits: 64 }, 1
    condbr
  bb2 bb2
    load Virtual { id: 87, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 77, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 88, bank: General, size_bits: 64 }, Virtual { id: 87, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 77, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 88, bank: General, size_bits: 64 }
    alloca Virtual { id: 90, bank: General, size_bits: 64 }, 1
    load Virtual { id: 91, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 77, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    rem Virtual { id: 92, bank: General, size_bits: 64 }, Virtual { id: 91, bank: General, size_bits: 64 }, 2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 90, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 92, bank: General, size_bits: 64 }
    alloca Virtual { id: 94, bank: General, size_bits: 64 }, 1
    load Virtual { id: 95, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 90, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ne Virtual { id: 96, bank: General, size_bits: 8 }, Virtual { id: 95, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 94, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 96, bank: General, size_bits: 64 }
    load Virtual { id: 98, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 94, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 99, bank: General, size_bits: 8 }, Virtual { id: 98, bank: General, size_bits: 64 }, 1
    condbr
  bb3 bb3
    load Virtual { id: 100, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 76, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
  bb4 bb4
    br
  bb5 bb5
    br
  bb6 bb6
    br
  bb7 bb7
    br
fn main
  bb0 bb0
    alloca Virtual { id: 101, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 102, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 103, bank: General, size_bits: 64 }, 1
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
    intrinsic.call symbol(intrinsic.println), Virtual { id: 111, bank: General, size_bits: 64 }
    call symbol(factorial)(7) cc=C tail=false
    br
  bb2 bb2
    intrinsic.call symbol(intrinsic.println), Virtual { id: 113, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    call symbol(sum_range)(1, 10) cc=C tail=false
    br
  bb3 bb3
    intrinsic.call symbol(intrinsic.println), Virtual { id: 116, bank: General, size_bits: 64 }
    call symbol(sum_range)(5, 15) cc=C tail=false
    br
  bb4 bb4
    intrinsic.call symbol(intrinsic.println), Virtual { id: 118, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    call symbol(find_first_divisor)(24) cc=C tail=false
    br
  bb5 bb5
    intrinsic.call symbol(intrinsic.println), Virtual { id: 121, bank: General, size_bits: 64 }
    call symbol(find_first_divisor)(17) cc=C tail=false
    br
  bb6 bb6
    intrinsic.call symbol(intrinsic.println), Virtual { id: 123, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    call symbol(sum_even_numbers)(10) cc=C tail=false
    br
  bb7 bb7
    intrinsic.call symbol(intrinsic.println), Virtual { id: 126, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 102, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 103, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    br
  bb8 bb8
    alloca Virtual { id: 131, bank: General, size_bits: 64 }, 1
    load Virtual { id: 132, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 103, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 133, bank: General, size_bits: 8 }, Virtual { id: 132, bank: General, size_bits: 64 }, 4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 131, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 133, bank: General, size_bits: 64 }
    load Virtual { id: 135, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 131, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 136, bank: General, size_bits: 8 }, Virtual { id: 135, bank: General, size_bits: 64 }, 1
    condbr
  bb9 bb9
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 101, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    br
  bb10 bb10
    load Virtual { id: 138, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 102, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 138, bank: General, size_bits: 64 }
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println), 0
    intrinsic.call symbol(intrinsic.println)
    ret
  bb11 bb11
    alloca Virtual { id: 143, bank: General, size_bits: 64 }, 1
    load Virtual { id: 144, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 101, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 145, bank: General, size_bits: 8 }, Virtual { id: 144, bank: General, size_bits: 64 }, 4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 143, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 145, bank: General, size_bits: 64 }
    load Virtual { id: 147, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 143, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 148, bank: General, size_bits: 8 }, Virtual { id: 147, bank: General, size_bits: 64 }, 1
    condbr
  bb12 bb12
    load Virtual { id: 149, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 102, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 150, bank: General, size_bits: 64 }, Virtual { id: 149, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 102, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 150, bank: General, size_bits: 64 }
    alloca Virtual { id: 152, bank: General, size_bits: 64 }, 1
    load Virtual { id: 153, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 103, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 154, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 101, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 155, bank: General, size_bits: 8 }, Virtual { id: 153, bank: General, size_bits: 64 }, Virtual { id: 154, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 152, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 155, bank: General, size_bits: 64 }
    load Virtual { id: 157, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 152, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 158, bank: General, size_bits: 8 }, Virtual { id: 157, bank: General, size_bits: 64 }, 1
    condbr
  bb13 bb13
    load Virtual { id: 159, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 103, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 160, bank: General, size_bits: 64 }, Virtual { id: 159, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 103, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 160, bank: General, size_bits: 64 }
    br
  bb14 bb14
    load Virtual { id: 162, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 103, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.print), Virtual { id: 162, bank: General, size_bits: 64 }
    br
  bb15 bb15
    br
  bb16 bb16
    load Virtual { id: 164, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 101, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 165, bank: General, size_bits: 64 }, Virtual { id: 164, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 101, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 165, bank: General, size_bits: 64 }
    br
fn __fp_comptime_const_FACTORIAL_CONST_8596463050749636282
  bb0 bb0
    alloca Virtual { id: 167, bank: General, size_bits: 64 }, 1
    call symbol(const_factorial)(5) cc=C tail=false
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 167, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 168, bank: General, size_bits: 64 }
    br
  bb1 bb1
    load Virtual { id: 170, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 167, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret


Symbols:
  factorial                        0x00000000
  const_factorial                  0x00000148
  sum_range                        0x00000254
  find_first_divisor               0x00000394
  sum_even_numbers                 0x000004d4
  main                             0x00000674
  __fp_comptime_const_FACTORIAL_CONST_8596463050749636282 0x00000b14

Text relocations:
  offset=0x000006ac kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000006b4 kind=CallRel32 symbol=printf addend=0
  offset=0x000006b8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000006c4 kind=CallRel32 symbol=printf addend=0
  offset=0x000006c8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000006d4 kind=CallRel32 symbol=printf addend=0
  offset=0x000006d8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000006e4 kind=CallRel32 symbol=printf addend=0
  offset=0x000006e8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000006f4 kind=CallRel32 symbol=printf addend=0
  offset=0x000006f8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000704 kind=CallRel32 symbol=printf addend=0
  offset=0x00000708 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000714 kind=CallRel32 symbol=printf addend=0
  offset=0x00000728 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000740 kind=CallRel32 symbol=printf addend=0
  offset=0x00000754 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000076c kind=CallRel32 symbol=printf addend=0
  offset=0x00000770 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000077c kind=CallRel32 symbol=printf addend=0
  offset=0x00000794 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000007ac kind=CallRel32 symbol=printf addend=0
  offset=0x000007c4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000007dc kind=CallRel32 symbol=printf addend=0
  offset=0x000007e0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000007ec kind=CallRel32 symbol=printf addend=0
  offset=0x00000800 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000818 kind=CallRel32 symbol=printf addend=0
  offset=0x0000082c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000844 kind=CallRel32 symbol=printf addend=0
  offset=0x00000848 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000854 kind=CallRel32 symbol=printf addend=0
  offset=0x00000868 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000880 kind=CallRel32 symbol=printf addend=0
  offset=0x00000884 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000890 kind=CallRel32 symbol=printf addend=0
  offset=0x0000092c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000944 kind=CallRel32 symbol=printf addend=0
  offset=0x00000948 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000954 kind=CallRel32 symbol=printf addend=0
  offset=0x00000958 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000970 kind=CallRel32 symbol=printf addend=0
  offset=0x00000974 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000980 kind=CallRel32 symbol=printf addend=0
  offset=0x00000ac8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000ae0 kind=CallRel32 symbol=printf addend=0

.text (2920 bytes):
  00000000  ff 03 04 d1 fd 7b 0f a9  fd 03 00 91 e0 5b 00 f9 
  00000010  f0 03 00 91 10 42 03 91  f0 03 00 f9 f0 03 00 91 
  00000020  10 62 03 91 f0 07 00 f9  f0 03 00 91 10 82 03 91 
  00000030  f0 0b 00 f9 f1 0b 40 f9  30 00 80 d2 30 02 00 f9 
  00000040  f1 03 40 f9 30 00 80 d2  30 02 00 f9 01 00 00 14 
  00000050  f0 03 00 91 10 a2 03 91  f0 17 00 f9 f0 03 40 f9 
  00000060  11 02 40 f9 f1 1b 00 f9  f0 1b 40 f9 f1 5b 40 f9 
  00000070  1f 02 11 eb f0 c7 9f 9a  f0 1f 00 f9 f1 17 40 f9 
  00000080  f0 e3 40 39 30 02 00 39  f0 17 40 f9 11 02 40 39 
  00000090  f1 27 00 f9 f0 23 41 39  1f 06 00 f1 f0 17 9f 9a 
  000000a0  f0 2b 00 f9 f0 2b 40 f9  1f 02 00 f1 41 00 00 54 
  000000b0  18 00 00 14 f0 0b 40 f9  11 02 40 f9 f1 2f 00 f9 
  000000c0  f0 03 40 f9 11 02 40 f9  f1 33 00 f9 f0 2f 40 f9 
  000000d0  f1 33 40 f9 10 7e 11 9b  f0 37 00 f9 f1 0b 40 f9 
  000000e0  f0 37 40 f9 30 02 00 f9  f0 03 40 f9 11 02 40 f9 
  000000f0  f1 3f 00 f9 f0 3f 40 f9  10 06 00 91 f0 43 00 f9 
  00000100  f1 03 40 f9 f0 43 40 f9  30 02 00 f9 d1 ff ff 17 
  00000110  f0 0b 40 f9 11 02 40 f9  f1 4b 00 f9 f1 07 40 f9 
  00000120  f0 4b 40 f9 30 02 00 f9  f0 07 40 f9 11 02 40 f9 
  00000130  f1 53 00 f9 e0 53 40 f9  bf 03 00 91 fd 7b 4f a9 
  00000140  ff 03 04 91 c0 03 5f d6  ff 43 03 d1 fd 7b 0c a9 
  00000150  fd 03 00 91 e0 47 00 f9  f0 03 00 91 10 a2 02 91 
  00000160  f0 03 00 f9 f0 03 00 91  10 c2 02 91 f0 07 00 f9 
  00000170  f0 47 40 f9 1f 06 00 f1  f0 c7 9f 9a f0 0b 00 f9 
  00000180  f1 07 40 f9 f0 43 40 39  30 02 00 39 f0 07 40 f9 
  00000190  11 02 40 39 f1 13 00 f9  f0 83 40 39 1f 06 00 f1 
  000001a0  f0 17 9f 9a f0 17 00 f9  f0 17 40 f9 1f 02 00 f1 
  000001b0  41 00 00 54 05 00 00 14  f1 03 40 f9 30 00 80 d2 
  000001c0  30 02 00 f9 11 00 00 14  f0 03 00 91 10 e2 02 91 
  000001d0  f0 1f 00 f9 f0 47 40 f9  10 06 00 d1 f0 23 00 f9 
  000001e0  f1 1f 40 f9 f0 23 40 f9  30 02 00 f9 f0 1f 40 f9 
  000001f0  11 02 40 f9 f1 2b 00 f9  e0 2b 40 f9 d3 ff ff 97 
  00000200  e0 2f 00 f9 09 00 00 14  f0 03 40 f9 11 02 40 f9 
  00000210  f1 33 00 f9 e0 33 40 f9  bf 03 00 91 fd 7b 4c a9 
  00000220  ff 43 03 91 c0 03 5f d6  f1 03 40 f9 30 00 80 d2 
  00000230  30 02 00 f9 f0 47 40 f9  f1 2f 40 f9 10 7e 11 9b 
  00000240  f0 3b 00 f9 f1 03 40 f9  f0 3b 40 f9 30 02 00 f9 
  00000250  ee ff ff 17 ff 03 04 d1  fd 7b 0f a9 fd 03 00 91 
  00000260  e0 57 00 f9 e1 5b 00 f9  f0 03 00 91 10 42 03 91 
  00000270  f0 03 00 f9 f0 03 00 91  10 62 03 91 f0 07 00 f9 
  00000280  f0 03 00 91 10 82 03 91  f0 0b 00 f9 f1 03 40 f9 
  00000290  10 00 80 d2 30 02 00 f9  f1 07 40 f9 f0 57 40 f9 
  000002a0  30 02 00 f9 01 00 00 14  f0 03 00 91 10 a2 03 91 
  000002b0  f0 17 00 f9 f0 07 40 f9  11 02 40 f9 f1 1b 00 f9 
  000002c0  f0 1b 40 f9 f1 5b 40 f9  1f 02 11 eb f0 a7 9f 9a 
  000002d0  f0 1f 00 f9 f1 17 40 f9  f0 e3 40 39 30 02 00 39 
  000002e0  f0 17 40 f9 11 02 40 39  f1 27 00 f9 f0 23 41 39 
  000002f0  1f 06 00 f1 f0 17 9f 9a  f0 2b 00 f9 f0 2b 40 f9 
  00000300  1f 02 00 f1 41 00 00 54  18 00 00 14 f0 03 40 f9 
  00000310  11 02 40 f9 f1 2f 00 f9  f0 07 40 f9 11 02 40 f9 
  00000320  f1 33 00 f9 f0 2f 40 f9  f1 33 40 f9 10 02 11 8b 
  00000330  f0 37 00 f9 f1 03 40 f9  f0 37 40 f9 30 02 00 f9 
  00000340  f0 07 40 f9 11 02 40 f9  f1 3f 00 f9 f0 3f 40 f9 
  00000350  10 06 00 91 f0 43 00 f9  f1 07 40 f9 f0 43 40 f9 
  00000360  30 02 00 f9 d1 ff ff 17  f1 0b 40 f9 10 00 80 d2 
  00000370  30 02 00 f9 f0 0b 40 f9  11 02 40 f9 f1 4f 00 f9 
  00000380  e0 4f 40 f9 bf 03 00 91  fd 7b 4f a9 ff 03 04 91 
  00000390  c0 03 5f d6 ff 03 04 d1  fd 7b 0f a9 fd 03 00 91 
  000003a0  e0 53 00 f9 f0 03 00 91  10 22 03 91 f0 03 00 f9 
  000003b0  f0 03 00 91 10 42 03 91  f0 07 00 f9 f1 07 40 f9 
  000003c0  50 00 80 d2 30 02 00 f9  01 00 00 14 01 00 00 14 
  000003d0  f0 03 00 91 10 62 03 91  f0 0f 00 f9 f0 07 40 f9 
  000003e0  11 02 40 f9 f1 13 00 f9  f0 07 40 f9 11 02 40 f9 
  000003f0  f1 17 00 f9 f0 13 40 f9  f1 17 40 f9 10 7e 11 9b 
  00000400  f0 1b 00 f9 f1 0f 40 f9  f0 1b 40 f9 30 02 00 f9 
  00000410  f0 03 00 91 10 82 03 91  f0 23 00 f9 f0 0f 40 f9 
  00000420  11 02 40 f9 f1 27 00 f9  f0 27 40 f9 f1 53 40 f9 
  00000430  1f 02 11 eb f0 d7 9f 9a  f0 2b 00 f9 f1 23 40 f9 
  00000440  f0 43 41 39 30 02 00 39  f0 23 40 f9 11 02 40 39 
  00000450  f1 33 00 f9 f0 83 41 39  1f 06 00 f1 f0 17 9f 9a 
  00000460  f0 37 00 f9 f0 37 40 f9  1f 02 00 f1 41 00 00 54 
  00000470  0e 00 00 14 f0 03 00 91  10 a2 03 91 f0 3b 00 f9 
  00000480  f1 3b 40 f9 f0 53 40 f9  30 02 00 f9 f0 3b 40 f9 
  00000490  11 02 40 f9 f1 43 00 f9  f1 03 40 f9 f0 43 40 f9 
  000004a0  30 02 00 f9 02 00 00 14  09 00 00 14 f0 03 40 f9 
  000004b0  11 02 40 f9 f1 4b 00 f9  e0 4b 40 f9 bf 03 00 91 
  000004c0  fd 7b 4f a9 ff 03 04 91  c0 03 5f d6 c0 ff ff 17 
  000004d0  ff ff ff 17 ff 03 05 d1  fd 7b 13 a9 fd 03 00 91 
  000004e0  e0 6b 00 f9 f0 03 00 91  10 02 04 91 f0 03 00 f9 
  000004f0  f0 03 00 91 10 22 04 91  f0 07 00 f9 f0 03 00 91 
  00000500  10 42 04 91 f0 0b 00 f9  f1 0b 40 f9 10 00 80 d2 
  00000510  30 02 00 f9 f1 07 40 f9  10 00 80 d2 30 02 00 f9 
  00000520  01 00 00 14 f0 03 00 91  10 62 04 91 f0 17 00 f9 
  00000530  f0 07 40 f9 11 02 40 f9  f1 1b 00 f9 f0 1b 40 f9 
  00000540  f1 6b 40 f9 1f 02 11 eb  f0 a7 9f 9a f0 1f 00 f9 
  00000550  f1 17 40 f9 f0 e3 40 39  30 02 00 39 f0 17 40 f9 
  00000560  11 02 40 39 f1 27 00 f9  f0 23 41 39 1f 06 00 f1 
  00000570  f0 17 9f 9a f0 2b 00 f9  f0 2b 40 f9 1f 02 00 f1 
  00000580  41 00 00 54 30 00 00 14  f0 07 40 f9 11 02 40 f9 
  00000590  f1 2f 00 f9 f0 2f 40 f9  10 06 00 91 f0 33 00 f9 
  000005a0  f1 07 40 f9 f0 33 40 f9  30 02 00 f9 f0 03 00 91 
  000005b0  10 82 04 91 f0 3b 00 f9  f0 07 40 f9 11 02 40 f9 
  000005c0  f1 3f 00 f9 f0 3f 40 f9  51 00 80 d2 09 0e d1 9a 
  000005d0  30 c1 11 9b f0 43 00 f9  f1 3b 40 f9 f0 43 40 f9 
  000005e0  30 02 00 f9 f0 03 00 91  10 a2 04 91 f0 4b 00 f9 
  000005f0  f0 3b 40 f9 11 02 40 f9  f1 4f 00 f9 f0 4f 40 f9 
  00000600  1f 02 00 f1 f0 07 9f 9a  f0 53 00 f9 f1 4b 40 f9 
  00000610  f0 83 42 39 30 02 00 39  f0 4b 40 f9 11 02 40 39 
  00000620  f1 5b 00 f9 f0 c3 42 39  1f 06 00 f1 f0 17 9f 9a 
  00000630  f0 5f 00 f9 f0 5f 40 f9  1f 02 00 f1 41 01 00 54 
  00000640  0a 00 00 14 f0 03 40 f9  11 02 40 f9 f1 63 00 f9 
  00000650  e0 63 40 f9 bf 03 00 91  fd 7b 53 a9 ff 03 05 91 
  00000660  c0 03 5f d6 b0 ff ff 17  01 00 00 14 ae ff ff 17 
  00000670  ff ff ff 17 ff 43 0e d1  f0 03 00 91 10 02 0e 91 
  00000680  1d 7a 00 a9 fd 03 00 91  f0 03 00 91 10 22 0d 91 
  00000690  f0 0b 00 f9 f0 03 00 91  10 42 0d 91 f0 0f 00 f9 
  000006a0  f0 03 00 91 10 62 0d 91  f0 13 00 f9 00 00 00 90 
  000006b0  00 00 00 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000006c0  00 80 00 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000006d0  00 60 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000006e0  00 20 02 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000006f0  00 c0 02 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000700  00 e0 02 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000710  00 60 03 91 00 00 00 94  a0 00 80 d2 39 fe ff 97 
  00000720  e0 33 00 f9 01 00 00 14  00 00 00 90 00 00 00 91 
  00000730  00 e0 03 91 e1 33 40 f9  f0 33 40 f9 f0 03 00 f9 
  00000740  00 00 00 94 e0 00 80 d2  2e fe ff 97 e0 3b 00 f9 
  00000750  01 00 00 14 00 00 00 90  00 00 00 91 00 20 04 91 
  00000760  e1 3b 40 f9 f0 3b 40 f9  f0 03 00 f9 00 00 00 94 
  00000770  00 00 00 90 00 00 00 91  00 60 04 91 00 00 00 94 
  00000780  20 00 80 d2 41 01 80 d2  b3 fe ff 97 e0 47 00 f9 
  00000790  01 00 00 14 00 00 00 90  00 00 00 91 00 e0 04 91 
  000007a0  e1 47 40 f9 f0 47 40 f9  f0 03 00 f9 00 00 00 94 
  000007b0  a0 00 80 d2 e1 01 80 d2  a7 fe ff 97 e0 4f 00 f9 
  000007c0  01 00 00 14 00 00 00 90  00 00 00 91 00 40 05 91 
  000007d0  e1 4f 40 f9 f0 4f 40 f9  f0 03 00 f9 00 00 00 94 
  000007e0  00 00 00 90 00 00 00 91  00 a0 05 91 00 00 00 94 
  000007f0  00 03 80 d2 e8 fe ff 97  e0 5b 00 f9 01 00 00 14 
  00000800  00 00 00 90 00 00 00 91  00 40 06 91 e1 5b 40 f9 
  00000810  f0 5b 40 f9 f0 03 00 f9  00 00 00 94 20 02 80 d2 
  00000820  dd fe ff 97 e0 63 00 f9  01 00 00 14 00 00 00 90 
  00000830  00 00 00 91 00 c0 06 91  e1 63 40 f9 f0 63 40 f9 
  00000840  f0 03 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  00000850  00 40 07 91 00 00 00 94  40 01 80 d2 1e ff ff 97 
  00000860  e0 6f 00 f9 01 00 00 14  00 00 00 90 00 00 00 91 
  00000870  00 c0 07 91 e1 6f 40 f9  f0 6f 40 f9 f0 03 00 f9 
  00000880  00 00 00 94 00 00 00 90  00 00 00 91 00 60 08 91 
  00000890  00 00 00 94 f1 0f 40 f9  10 00 80 d2 30 02 00 f9 
  000008a0  f1 13 40 f9 30 00 80 d2  30 02 00 f9 01 00 00 14 
  000008b0  f0 03 00 91 10 82 0d 91  f0 83 00 f9 f0 13 40 f9 
  000008c0  11 02 40 f9 f1 87 00 f9  f0 87 40 f9 1f 12 00 f1 
  000008d0  f0 a7 9f 9a f0 8b 00 f9  f1 83 40 f9 f0 43 44 39 
  000008e0  30 02 00 39 f0 83 40 f9  11 02 40 39 f1 93 00 f9 
  000008f0  f0 83 44 39 1f 06 00 f1  f0 17 9f 9a f0 97 00 f9 
  00000900  f0 97 40 f9 1f 02 00 f1  41 00 00 54 05 00 00 14 
  00000910  f1 0b 40 f9 30 00 80 d2  30 02 00 f9 21 00 00 14 
  00000920  f0 0f 40 f9 11 02 40 f9  f1 9f 00 f9 00 00 00 90 
  00000930  00 00 00 91 00 c0 08 91  e1 9f 40 f9 f0 9f 40 f9 
  00000940  f0 03 00 f9 00 00 00 94  00 00 00 90 00 00 00 91 
  00000950  00 20 09 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000960  00 a0 09 91 01 00 80 d2  10 00 80 d2 f0 03 00 f9 
  00000970  00 00 00 94 00 00 00 90  00 00 00 91 00 20 0a 91 
  00000980  00 00 00 94 bf 03 00 91  f0 03 00 91 10 02 0e 91 
  00000990  1d 7a 40 a9 ff 43 0e 91  00 00 80 d2 c0 03 5f d6 
  000009a0  f0 03 00 91 10 a2 0d 91  f0 b3 00 f9 f0 0b 40 f9 
  000009b0  11 02 40 f9 f1 b7 00 f9  f0 b7 40 f9 1f 12 00 f1 
  000009c0  f0 a7 9f 9a f0 bb 00 f9  f1 b3 40 f9 f0 c3 45 39 
  000009d0  30 02 00 39 f0 b3 40 f9  11 02 40 39 f1 c3 00 f9 
  000009e0  f0 03 46 39 1f 06 00 f1  f0 17 9f 9a f0 c7 00 f9 
  000009f0  f0 c7 40 f9 1f 02 00 f1  41 00 00 54 26 00 00 14 
  00000a00  f0 0f 40 f9 11 02 40 f9  f1 cb 00 f9 f0 cb 40 f9 
  00000a10  10 06 00 91 f0 cf 00 f9  f1 0f 40 f9 f0 cf 40 f9 
  00000a20  30 02 00 f9 f0 03 00 91  10 c2 0d 91 f0 d7 00 f9 
  00000a30  f0 13 40 f9 11 02 40 f9  f1 db 00 f9 f0 0b 40 f9 
  00000a40  11 02 40 f9 f1 df 00 f9  f0 db 40 f9 f1 df 40 f9 
  00000a50  1f 02 11 eb f0 17 9f 9a  f0 e3 00 f9 f1 d7 40 f9 
  00000a60  f0 03 47 39 30 02 00 39  f0 d7 40 f9 11 02 40 39 
  00000a70  f1 eb 00 f9 f0 43 47 39  1f 06 00 f1 f0 17 9f 9a 
  00000a80  f0 ef 00 f9 f0 ef 40 f9  1f 02 00 f1 81 01 00 54 
  00000a90  16 00 00 14 f0 13 40 f9  11 02 40 f9 f1 f3 00 f9 
  00000aa0  f0 f3 40 f9 10 06 00 91  f0 f7 00 f9 f1 13 40 f9 
  00000ab0  f0 f7 40 f9 30 02 00 f9  7e ff ff 17 f0 13 40 f9 
  00000ac0  11 02 40 f9 f1 ff 00 f9  00 00 00 90 00 00 00 91 
  00000ad0  00 c0 0a 91 e1 ff 40 f9  f0 ff 40 f9 f0 03 00 f9 
  00000ae0  00 00 00 94 02 00 00 14  01 00 00 14 f0 0b 40 f9 
  00000af0  11 02 40 f9 f1 07 01 f9  f0 07 41 f9 10 06 00 91 
  00000b00  f0 0b 01 f9 f1 0b 40 f9  f0 0b 41 f9 30 02 00 f9 
  00000b10  a4 ff ff 17 ff 03 01 d1  fd 7b 03 a9 fd 03 00 91 
  00000b20  f0 03 00 91 10 a2 00 91  f0 03 00 f9 a0 00 80 d2 
  00000b30  86 fd ff 97 e0 07 00 f9  f1 03 40 f9 f0 07 40 f9 
  00000b40  30 02 00 f9 01 00 00 14  f0 03 40 f9 11 02 40 f9 
  00000b50  f1 0f 00 f9 e0 0f 40 f9  bf 03 00 91 fd 7b 43 a9 
  00000b60  ff 03 01 91 c0 03 5f d6 

.rodata (696 bytes):
  00000000  f0 9f 93 98 20 54 75 74  6f 72 69 61 6c 3a 20 31 
  00000010  33 5f 6c 6f 6f 70 73 2e  66 70 0a 00 00 00 00 00 
  00000020  f0 9f a7 ad 20 46 6f 63  75 73 3a 20 4c 6f 6f 70 
  00000030  20 63 6f 6e 73 74 72 75  63 74 73 3a 20 77 68 69 
  00000040  6c 65 2c 20 66 6f 72 2c  20 61 6e 64 20 6c 6f 6f 
  00000050  70 2e 0a 00 00 00 00 00  f0 9f a7 aa 20 57 68 61 
  00000060  74 20 74 6f 20 6c 6f 6f  6b 20 66 6f 72 3a 20 6c 
  00000070  61 62 65 6c 65 64 20 6f  75 74 70 75 74 73 20 62 
  00000080  65 6c 6f 77 0a 00 00 00  e2 9c 85 20 45 78 70 65 
  00000090  63 74 61 74 69 6f 6e 3a  20 6f 75 74 70 75 74 73 
  000000a0  20 6d 61 74 63 68 20 6c  61 62 65 6c 73 0a 00 00 
  000000b0  0a 00 00 00 00 00 00 00  3d 3d 3d 20 4c 6f 6f 70 
  000000c0  20 43 6f 6e 73 74 72 75  63 74 73 20 3d 3d 3d 0a 
  000000d0  0a 00 00 00 00 00 00 00  31 2e 20 57 68 69 6c 65 
  000000e0  20 6c 6f 6f 70 20 2d 20  66 61 63 74 6f 72 69 61 
  000000f0  6c 3a 0a 00 00 00 00 00  20 20 35 21 20 3d 20 25 
  00000100  6c 6c 64 0a 00 00 00 00  20 20 37 21 20 3d 20 25 
  00000110  6c 6c 64 0a 00 00 00 00  0a 32 2e 20 46 6f 72 20 
  00000120  6c 6f 6f 70 20 2d 20 73  75 6d 20 72 61 6e 67 65 
  00000130  3a 0a 00 00 00 00 00 00  20 20 73 75 6d 28 31 2e 
  00000140  2e 31 30 29 20 3d 20 25  6c 6c 64 0a 00 00 00 00 
  00000150  20 20 73 75 6d 28 35 2e  2e 31 35 29 20 3d 20 25 
  00000160  6c 6c 64 0a 00 00 00 00  0a 33 2e 20 4c 6f 6f 70 
  00000170  20 77 69 74 68 20 62 72  65 61 6b 20 65 78 70 72 
  00000180  65 73 73 69 6f 6e 3a 0a  00 00 00 00 00 00 00 00 
  00000190  20 20 46 69 72 73 74 20  64 69 76 69 73 6f 72 20 
  000001a0  6f 66 20 32 34 3a 20 25  6c 6c 64 0a 00 00 00 00 
  000001b0  20 20 46 69 72 73 74 20  64 69 76 69 73 6f 72 20 
  000001c0  6f 66 20 31 37 3a 20 25  6c 6c 64 0a 00 00 00 00 
  000001d0  0a 34 2e 20 4c 6f 6f 70  20 77 69 74 68 20 63 6f 
  000001e0  6e 74 69 6e 75 65 3a 0a  00 00 00 00 00 00 00 00 
  000001f0  20 20 53 75 6d 20 6f 66  20 65 76 65 6e 20 6e 75 
  00000200  6d 62 65 72 73 20 3c 20  31 30 3a 20 25 6c 6c 64 
  00000210  0a 00 00 00 00 00 00 00  0a 35 2e 20 4e 65 73 74 
  00000220  65 64 20 6c 6f 6f 70 73  3a 0a 00 00 00 00 00 00 
  00000230  0a 20 20 49 74 65 72 61  74 69 6f 6e 73 3a 20 25 
  00000240  6c 6c 64 0a 00 00 00 00  0a 36 2e 20 43 6f 6d 70 
  00000250  69 6c 65 2d 74 69 6d 65  20 72 65 63 75 72 73 69 
  00000260  6f 6e 3a 0a 00 00 00 00  20 20 63 6f 6e 73 74 5f 
  00000270  66 61 63 74 6f 72 69 61  6c 28 35 29 20 3d 20 25 
  00000280  6c 6c 64 0a 00 00 00 00  0a e2 9c 93 20 4c 6f 6f 
  00000290  70 20 63 6f 6e 73 74 72  75 63 74 73 20 64 65 6d 
  000002a0  6f 6e 73 74 72 61 74 65  64 21 0a 00 00 00 00 00 
  000002b0  5b 25 6c 6c 64 5d 20 00 
