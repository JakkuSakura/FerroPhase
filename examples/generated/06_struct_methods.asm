fp-native dump: format=MachO arch=Aarch64 entry=0x6e8

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
fn Point__new
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 1
    insertvalue Virtual { id: 1, bank: General, size_bits: 64 }, 0, symbol(local.1), 0
    insertvalue Virtual { id: 2, bank: General, size_bits: 64 }, Virtual { id: 1, bank: General, size_bits: 64 }, symbol(local.2), 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 2, bank: General, size_bits: 64 }
    load Virtual { id: 4, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn Point__translate
  bb0 bb0
    alloca Virtual { id: 5, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    load Virtual { id: 7, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 8, bank: General, size_bits: 64 }, Virtual { id: 7, bank: General, size_bits: 64 }
    load Virtual { id: 9, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 10, bank: General, size_bits: 64 }, Virtual { id: 9, bank: General, size_bits: 64 }
    load Virtual { id: 11, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 10, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 12, bank: General, size_bits: 64 }, Virtual { id: 11, bank: General, size_bits: 64 }, symbol(local.2)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 8, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 12, bank: General, size_bits: 64 }
    load Virtual { id: 14, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 15, bank: General, size_bits: 64 }, Virtual { id: 14, bank: General, size_bits: 64 }
    gep Virtual { id: 16, bank: General, size_bits: 64 }, Virtual { id: 15, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 17, bank: General, size_bits: 64 }, Virtual { id: 16, bank: General, size_bits: 64 }
    load Virtual { id: 18, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 5, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 19, bank: General, size_bits: 64 }, Virtual { id: 18, bank: General, size_bits: 64 }
    gep Virtual { id: 20, bank: General, size_bits: 64 }, Virtual { id: 19, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 21, bank: General, size_bits: 64 }, Virtual { id: 20, bank: General, size_bits: 64 }
    load Virtual { id: 22, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 23, bank: General, size_bits: 64 }, Virtual { id: 22, bank: General, size_bits: 64 }, symbol(local.3)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 17, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 23, bank: General, size_bits: 64 }
    ret
fn Point__distance2
  bb0 bb0
    alloca Virtual { id: 25, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 26, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 27, bank: General, size_bits: 64 }, symbol(local.1)
    load Virtual { id: 28, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 27, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 29, bank: General, size_bits: 64 }, symbol(local.2)
    load Virtual { id: 30, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 29, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    sub Virtual { id: 31, bank: General, size_bits: 64 }, Virtual { id: 28, bank: General, size_bits: 64 }, Virtual { id: 30, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 26, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 31, bank: General, size_bits: 64 }
    alloca Virtual { id: 33, bank: General, size_bits: 64 }, 1
    load Virtual { id: 34, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 26, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 33, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 34, bank: General, size_bits: 64 }
    alloca Virtual { id: 36, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 37, bank: General, size_bits: 64 }, symbol(local.1)
    gep Virtual { id: 38, bank: General, size_bits: 64 }, Virtual { id: 37, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 39, bank: General, size_bits: 64 }, Virtual { id: 38, bank: General, size_bits: 64 }
    load Virtual { id: 40, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 39, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 41, bank: General, size_bits: 64 }, symbol(local.2)
    gep Virtual { id: 42, bank: General, size_bits: 64 }, Virtual { id: 41, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 43, bank: General, size_bits: 64 }, Virtual { id: 42, bank: General, size_bits: 64 }
    load Virtual { id: 44, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 43, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    sub Virtual { id: 45, bank: General, size_bits: 64 }, Virtual { id: 40, bank: General, size_bits: 64 }, Virtual { id: 44, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 45, bank: General, size_bits: 64 }
    alloca Virtual { id: 47, bank: General, size_bits: 64 }, 1
    load Virtual { id: 48, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 47, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 48, bank: General, size_bits: 64 }
    alloca Virtual { id: 50, bank: General, size_bits: 64 }, 1
    load Virtual { id: 51, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 33, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 52, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 33, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 53, bank: General, size_bits: 64 }, Virtual { id: 51, bank: General, size_bits: 64 }, Virtual { id: 52, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 50, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 53, bank: General, size_bits: 64 }
    alloca Virtual { id: 55, bank: General, size_bits: 64 }, 1
    load Virtual { id: 56, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 47, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 57, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 47, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 58, bank: General, size_bits: 64 }, Virtual { id: 56, bank: General, size_bits: 64 }, Virtual { id: 57, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 55, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 58, bank: General, size_bits: 64 }
    load Virtual { id: 60, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 50, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 61, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 55, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 62, bank: General, size_bits: 64 }, Virtual { id: 60, bank: General, size_bits: 64 }, Virtual { id: 61, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 25, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 62, bank: General, size_bits: 64 }
    load Virtual { id: 64, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 25, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn Rectangle__new
  bb0 bb0
    alloca Virtual { id: 65, bank: General, size_bits: 64 }, 1
    insertvalue Virtual { id: 66, bank: General, size_bits: 64 }, 0, symbol(local.1), 0
    insertvalue Virtual { id: 67, bank: General, size_bits: 64 }, Virtual { id: 66, bank: General, size_bits: 64 }, symbol(local.2), 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 67, bank: General, size_bits: 64 }
    load Virtual { id: 69, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn Rectangle__area
  bb0 bb0
    alloca Virtual { id: 70, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 71, bank: General, size_bits: 64 }, symbol(local.1)
    load Virtual { id: 72, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 71, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 73, bank: General, size_bits: 64 }, symbol(local.1)
    gep Virtual { id: 74, bank: General, size_bits: 64 }, Virtual { id: 73, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 75, bank: General, size_bits: 64 }, Virtual { id: 74, bank: General, size_bits: 64 }
    load Virtual { id: 76, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 75, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 77, bank: General, size_bits: 64 }, Virtual { id: 72, bank: General, size_bits: 64 }, Virtual { id: 76, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 70, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 77, bank: General, size_bits: 64 }
    load Virtual { id: 79, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 70, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn Rectangle__perimeter
  bb0 bb0
    alloca Virtual { id: 80, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 81, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 82, bank: General, size_bits: 64 }, symbol(local.1)
    load Virtual { id: 83, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 82, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 84, bank: General, size_bits: 64 }, symbol(local.1)
    gep Virtual { id: 85, bank: General, size_bits: 64 }, Virtual { id: 84, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 86, bank: General, size_bits: 64 }, Virtual { id: 85, bank: General, size_bits: 64 }
    load Virtual { id: 87, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 86, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 88, bank: General, size_bits: 64 }, Virtual { id: 83, bank: General, size_bits: 64 }, Virtual { id: 87, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 81, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 88, bank: General, size_bits: 64 }
    load Virtual { id: 90, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 81, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 91, bank: General, size_bits: 64 }, 2, Virtual { id: 90, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 80, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 91, bank: General, size_bits: 64 }
    load Virtual { id: 93, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 80, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn Rectangle__is_square
  bb0 bb0
    alloca Virtual { id: 94, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 95, bank: General, size_bits: 64 }, symbol(local.1)
    load Virtual { id: 96, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 95, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 97, bank: General, size_bits: 64 }, symbol(local.1)
    gep Virtual { id: 98, bank: General, size_bits: 64 }, Virtual { id: 97, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 99, bank: General, size_bits: 64 }, Virtual { id: 98, bank: General, size_bits: 64 }
    load Virtual { id: 100, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 99, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 101, bank: General, size_bits: 8 }, Virtual { id: 96, bank: General, size_bits: 64 }, Virtual { id: 100, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 94, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 101, bank: General, size_bits: 8 }
    load Virtual { id: 103, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 94, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    call symbol(Point__new)(10, 20) cc=C tail=false
    alloca Virtual { id: 111, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 111, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 110, bank: General, size_bits: 64 }
    br
  bb1 bb1
    call symbol(Point__new)(5, 15) cc=C tail=false
    alloca Virtual { id: 114, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 114, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 113, bank: General, size_bits: 64 }
    br
  bb2 bb2
    bitcast Virtual { id: 116, bank: General, size_bits: 64 }, Virtual { id: 111, bank: General, size_bits: 64 }
    load Virtual { id: 117, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 116, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 118, bank: General, size_bits: 64 }, Virtual { id: 111, bank: General, size_bits: 64 }
    gep Virtual { id: 119, bank: General, size_bits: 64 }, Virtual { id: 118, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 120, bank: General, size_bits: 64 }, Virtual { id: 119, bank: General, size_bits: 64 }
    load Virtual { id: 121, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 120, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 117, bank: General, size_bits: 64 }, Virtual { id: 121, bank: General, size_bits: 64 }
    bitcast Virtual { id: 123, bank: General, size_bits: 64 }, Virtual { id: 114, bank: General, size_bits: 64 }
    load Virtual { id: 124, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 123, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 125, bank: General, size_bits: 64 }, Virtual { id: 114, bank: General, size_bits: 64 }
    gep Virtual { id: 126, bank: General, size_bits: 64 }, Virtual { id: 125, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 127, bank: General, size_bits: 64 }, Virtual { id: 126, bank: General, size_bits: 64 }
    load Virtual { id: 128, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 127, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 124, bank: General, size_bits: 64 }, Virtual { id: 128, bank: General, size_bits: 64 }
    alloca Virtual { id: 130, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 130, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 111, bank: General, size_bits: 64 }
    alloca Virtual { id: 132, bank: General, size_bits: 64 }, 1
    sub Virtual { id: 133, bank: General, size_bits: 64 }, 0, 4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 132, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 133, bank: General, size_bits: 64 }
    load Virtual { id: 135, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 130, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 136, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 132, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(Point__translate)(v135, 3, v136) cc=C tail=false
    br
  bb3 bb3
    bitcast Virtual { id: 138, bank: General, size_bits: 64 }, Virtual { id: 111, bank: General, size_bits: 64 }
    load Virtual { id: 139, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 138, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 140, bank: General, size_bits: 64 }, Virtual { id: 111, bank: General, size_bits: 64 }
    gep Virtual { id: 141, bank: General, size_bits: 64 }, Virtual { id: 140, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 142, bank: General, size_bits: 64 }, Virtual { id: 141, bank: General, size_bits: 64 }
    load Virtual { id: 143, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 142, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 139, bank: General, size_bits: 64 }, Virtual { id: 143, bank: General, size_bits: 64 }
    alloca Virtual { id: 145, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 145, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 111, bank: General, size_bits: 64 }
    alloca Virtual { id: 147, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 147, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 114, bank: General, size_bits: 64 }
    load Virtual { id: 149, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 145, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 150, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 147, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(Point__distance2)(v149, v150) cc=C tail=false
    br
  bb4 bb4
    intrinsic.call symbol(intrinsic.println), Virtual { id: 151, bank: General, size_bits: 64 }
    call symbol(Rectangle__new)(10, 5) cc=C tail=false
    alloca Virtual { id: 154, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 154, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 153, bank: General, size_bits: 64 }
    br
  bb5 bb5
    bitcast Virtual { id: 156, bank: General, size_bits: 64 }, Virtual { id: 154, bank: General, size_bits: 64 }
    load Virtual { id: 157, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 156, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 158, bank: General, size_bits: 64 }, Virtual { id: 154, bank: General, size_bits: 64 }
    gep Virtual { id: 159, bank: General, size_bits: 64 }, Virtual { id: 158, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 160, bank: General, size_bits: 64 }, Virtual { id: 159, bank: General, size_bits: 64 }
    load Virtual { id: 161, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 160, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 157, bank: General, size_bits: 64 }, Virtual { id: 161, bank: General, size_bits: 64 }
    alloca Virtual { id: 163, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 163, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 154, bank: General, size_bits: 64 }
    load Virtual { id: 165, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 163, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(Rectangle__area)(v165) cc=C tail=false
    br
  bb6 bb6
    intrinsic.call symbol(intrinsic.println), Virtual { id: 166, bank: General, size_bits: 64 }
    alloca Virtual { id: 168, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 168, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 154, bank: General, size_bits: 64 }
    load Virtual { id: 170, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 168, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(Rectangle__perimeter)(v170) cc=C tail=false
    br
  bb7 bb7
    intrinsic.call symbol(intrinsic.println), Virtual { id: 171, bank: General, size_bits: 64 }
    alloca Virtual { id: 173, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 173, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 154, bank: General, size_bits: 64 }
    load Virtual { id: 175, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 173, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(Rectangle__is_square)(v175) cc=C tail=false
    br
  bb8 bb8
    intrinsic.call symbol(intrinsic.println), Virtual { id: 176, bank: General, size_bits: 8 }
    ret


Symbols:
  Point__new                       0x00000000
  Point__translate                 0x000000e4
  Point__distance2                 0x000001f8
  Rectangle__new                   0x00000404
  Rectangle__area                  0x000004e8
  Rectangle__perimeter             0x00000580
  Rectangle__is_square             0x0000064c
  main                             0x000006e8

Text relocations:
  offset=0x000006fc kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000704 kind=CallRel32 symbol=printf addend=0
  offset=0x00000708 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000714 kind=CallRel32 symbol=printf addend=0
  offset=0x00000718 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000724 kind=CallRel32 symbol=printf addend=0
  offset=0x00000728 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000734 kind=CallRel32 symbol=printf addend=0
  offset=0x00000738 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000744 kind=CallRel32 symbol=printf addend=0
  offset=0x00000748 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000754 kind=CallRel32 symbol=printf addend=0
  offset=0x00000838 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000085c kind=CallRel32 symbol=printf addend=0
  offset=0x000008a0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000008c4 kind=CallRel32 symbol=printf addend=0
  offset=0x00000970 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000994 kind=CallRel32 symbol=printf addend=0
  offset=0x000009f4 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000a0c kind=CallRel32 symbol=printf addend=0
  offset=0x00000aa0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000ac4 kind=CallRel32 symbol=printf addend=0
  offset=0x00000afc kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000b14 kind=CallRel32 symbol=printf addend=0
  offset=0x00000b4c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000b64 kind=CallRel32 symbol=printf addend=0
  offset=0x00000b9c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000bb4 kind=CallRel32 symbol=printf addend=0

.text (3028 bytes):
  00000000  ff 83 02 d1 fd 7b 09 a9  fd 03 00 91 e0 27 00 f9 
  00000010  e1 1f 00 f9 e2 23 00 f9  f0 03 00 91 10 02 02 91 
  00000020  f0 03 00 f9 10 00 80 d2  f0 2b 00 f9 f0 2f 00 f9 
  00000030  f0 1f 40 f9 f0 2b 00 f9  f0 03 00 91 10 42 01 91 
  00000040  f0 07 00 f9 f0 2b 40 f9  f0 33 00 f9 f0 2f 40 f9 
  00000050  f0 37 00 f9 f0 23 40 f9  f0 37 00 f9 f0 03 00 91 
  00000060  10 82 01 91 f0 0b 00 f9  f1 03 40 f9 f0 33 40 f9 
  00000070  e9 03 11 aa 30 01 00 f9  f0 37 40 f9 e9 03 11 aa 
  00000080  29 21 00 91 30 01 00 f9  f1 03 40 f9 e9 03 11 aa 
  00000090  30 01 40 f9 f0 3b 00 f9  e9 03 11 aa 29 21 00 91 
  000000a0  30 01 40 f9 f0 3f 00 f9  f0 03 00 91 10 c2 01 91 
  000000b0  f0 13 00 f9 f1 27 40 f9  f0 3b 40 f9 e9 03 11 aa 
  000000c0  30 01 00 f9 f0 3f 40 f9  e9 03 11 aa 29 21 00 91 
  000000d0  30 01 00 f9 bf 03 00 91  fd 7b 49 a9 ff 83 02 91 
  000000e0  c0 03 5f d6 ff 83 03 d1  fd 7b 0d a9 fd 03 00 91 
  000000f0  e0 57 00 f9 e1 5b 00 f9  e2 5f 00 f9 f0 03 00 91 
  00000100  10 02 03 91 f0 03 00 f9  f1 03 40 f9 f0 57 40 f9 
  00000110  30 02 00 f9 f0 03 40 f9  11 02 40 f9 f1 0b 00 f9 
  00000120  f0 0b 40 f9 f0 0f 00 f9  f0 03 40 f9 11 02 40 f9 
  00000130  f1 13 00 f9 f0 13 40 f9  f0 17 00 f9 f0 17 40 f9 
  00000140  11 02 40 f9 f1 1b 00 f9  f0 1b 40 f9 f1 5b 40 f9 
  00000150  10 02 11 8b f0 1f 00 f9  f1 0f 40 f9 f0 1f 40 f9 
  00000160  30 02 00 f9 f0 03 40 f9  11 02 40 f9 f1 27 00 f9 
  00000170  f0 27 40 f9 f0 2b 00 f9  f0 2b 40 f9 11 01 80 d2 
  00000180  10 02 11 8b f0 2f 00 f9  f0 2f 40 f9 f0 33 00 f9 
  00000190  f0 03 40 f9 11 02 40 f9  f1 37 00 f9 f0 37 40 f9 
  000001a0  f0 3b 00 f9 f0 3b 40 f9  11 01 80 d2 10 02 11 8b 
  000001b0  f0 3f 00 f9 f0 3f 40 f9  f0 43 00 f9 f0 43 40 f9 
  000001c0  11 02 40 f9 f1 47 00 f9  f0 47 40 f9 f1 5f 40 f9 
  000001d0  10 02 11 8b f0 4b 00 f9  f1 33 40 f9 f0 4b 40 f9 
  000001e0  30 02 00 f9 bf 03 00 91  fd 7b 4d a9 ff 83 03 91 
  000001f0  00 00 80 d2 c0 03 5f d6  ff 43 07 d1 fd 7b 1c a9 
  00000200  fd 03 00 91 e0 a7 00 f9  e1 ab 00 f9 f0 03 00 91 
  00000210  10 22 06 91 f0 03 00 f9  f0 03 00 91 10 42 06 91 
  00000220  f0 07 00 f9 f0 a7 40 f9  f0 0b 00 f9 f0 0b 40 f9 
  00000230  11 02 40 f9 f1 0f 00 f9  f0 ab 40 f9 f0 13 00 f9 
  00000240  f0 13 40 f9 11 02 40 f9  f1 17 00 f9 f0 0f 40 f9 
  00000250  f1 17 40 f9 10 02 11 cb  f0 1b 00 f9 f1 07 40 f9 
  00000260  f0 1b 40 f9 30 02 00 f9  f0 03 00 91 10 62 06 91 
  00000270  f0 23 00 f9 f0 07 40 f9  11 02 40 f9 f1 27 00 f9 
  00000280  f1 23 40 f9 f0 27 40 f9  30 02 00 f9 f0 03 00 91 
  00000290  10 82 06 91 f0 2f 00 f9  f0 a7 40 f9 f0 33 00 f9 
  000002a0  f0 33 40 f9 11 01 80 d2  10 02 11 8b f0 37 00 f9 
  000002b0  f0 37 40 f9 f0 3b 00 f9  f0 3b 40 f9 11 02 40 f9 
  000002c0  f1 3f 00 f9 f0 ab 40 f9  f0 43 00 f9 f0 43 40 f9 
  000002d0  11 01 80 d2 10 02 11 8b  f0 47 00 f9 f0 47 40 f9 
  000002e0  f0 4b 00 f9 f0 4b 40 f9  11 02 40 f9 f1 4f 00 f9 
  000002f0  f0 3f 40 f9 f1 4f 40 f9  10 02 11 cb f0 53 00 f9 
  00000300  f1 2f 40 f9 f0 53 40 f9  30 02 00 f9 f0 03 00 91 
  00000310  10 a2 06 91 f0 5b 00 f9  f0 2f 40 f9 11 02 40 f9 
  00000320  f1 5f 00 f9 f1 5b 40 f9  f0 5f 40 f9 30 02 00 f9 
  00000330  f0 03 00 91 10 c2 06 91  f0 67 00 f9 f0 23 40 f9 
  00000340  11 02 40 f9 f1 6b 00 f9  f0 23 40 f9 11 02 40 f9 
  00000350  f1 6f 00 f9 f0 6b 40 f9  f1 6f 40 f9 10 7e 11 9b 
  00000360  f0 73 00 f9 f1 67 40 f9  f0 73 40 f9 30 02 00 f9 
  00000370  f0 03 00 91 10 e2 06 91  f0 7b 00 f9 f0 5b 40 f9 
  00000380  11 02 40 f9 f1 7f 00 f9  f0 5b 40 f9 11 02 40 f9 
  00000390  f1 83 00 f9 f0 7f 40 f9  f1 83 40 f9 10 7e 11 9b 
  000003a0  f0 87 00 f9 f1 7b 40 f9  f0 87 40 f9 30 02 00 f9 
  000003b0  f0 67 40 f9 11 02 40 f9  f1 8f 00 f9 f0 7b 40 f9 
  000003c0  11 02 40 f9 f1 93 00 f9  f0 8f 40 f9 f1 93 40 f9 
  000003d0  10 02 11 8b f0 97 00 f9  f1 03 40 f9 f0 97 40 f9 
  000003e0  30 02 00 f9 f0 03 40 f9  11 02 40 f9 f1 9f 00 f9 
  000003f0  e0 9f 40 f9 bf 03 00 91  fd 7b 5c a9 ff 43 07 91 
  00000400  c0 03 5f d6 ff 83 02 d1  fd 7b 09 a9 fd 03 00 91 
  00000410  e0 27 00 f9 e1 1f 00 f9  e2 23 00 f9 f0 03 00 91 
  00000420  10 02 02 91 f0 03 00 f9  10 00 80 d2 f0 2b 00 f9 
  00000430  f0 2f 00 f9 f0 1f 40 f9  f0 2b 00 f9 f0 03 00 91 
  00000440  10 42 01 91 f0 07 00 f9  f0 2b 40 f9 f0 33 00 f9 
  00000450  f0 2f 40 f9 f0 37 00 f9  f0 23 40 f9 f0 37 00 f9 
  00000460  f0 03 00 91 10 82 01 91  f0 0b 00 f9 f1 03 40 f9 
  00000470  f0 33 40 f9 e9 03 11 aa  30 01 00 f9 f0 37 40 f9 
  00000480  e9 03 11 aa 29 21 00 91  30 01 00 f9 f1 03 40 f9 
  00000490  e9 03 11 aa 30 01 40 f9  f0 3b 00 f9 e9 03 11 aa 
  000004a0  29 21 00 91 30 01 40 f9  f0 3f 00 f9 f0 03 00 91 
  000004b0  10 c2 01 91 f0 13 00 f9  f1 27 40 f9 f0 3b 40 f9 
  000004c0  e9 03 11 aa 30 01 00 f9  f0 3f 40 f9 e9 03 11 aa 
  000004d0  29 21 00 91 30 01 00 f9  bf 03 00 91 fd 7b 49 a9 
  000004e0  ff 83 02 91 c0 03 5f d6  ff 03 02 d1 fd 7b 07 a9 
  000004f0  fd 03 00 91 e0 2f 00 f9  f0 03 00 91 10 82 01 91 
  00000500  f0 03 00 f9 f0 2f 40 f9  f0 07 00 f9 f0 07 40 f9 
  00000510  11 02 40 f9 f1 0b 00 f9  f0 2f 40 f9 f0 0f 00 f9 
  00000520  f0 0f 40 f9 11 01 80 d2  10 02 11 8b f0 13 00 f9 
  00000530  f0 13 40 f9 f0 17 00 f9  f0 17 40 f9 11 02 40 f9 
  00000540  f1 1b 00 f9 f0 0b 40 f9  f1 1b 40 f9 10 7e 11 9b 
  00000550  f0 1f 00 f9 f1 03 40 f9  f0 1f 40 f9 30 02 00 f9 
  00000560  f0 03 40 f9 11 02 40 f9  f1 27 00 f9 e0 27 40 f9 
  00000570  bf 03 00 91 fd 7b 47 a9  ff 03 02 91 c0 03 5f d6 
  00000580  ff c3 02 d1 fd 7b 0a a9  fd 03 00 91 e0 3f 00 f9 
  00000590  f0 03 00 91 10 22 02 91  f0 03 00 f9 f0 03 00 91 
  000005a0  10 42 02 91 f0 07 00 f9  f0 3f 40 f9 f0 0b 00 f9 
  000005b0  f0 0b 40 f9 11 02 40 f9  f1 0f 00 f9 f0 3f 40 f9 
  000005c0  f0 13 00 f9 f0 13 40 f9  11 01 80 d2 10 02 11 8b 
  000005d0  f0 17 00 f9 f0 17 40 f9  f0 1b 00 f9 f0 1b 40 f9 
  000005e0  11 02 40 f9 f1 1f 00 f9  f0 0f 40 f9 f1 1f 40 f9 
  000005f0  10 02 11 8b f0 23 00 f9  f1 07 40 f9 f0 23 40 f9 
  00000600  30 02 00 f9 f0 07 40 f9  11 02 40 f9 f1 2b 00 f9 
  00000610  50 00 80 d2 f1 2b 40 f9  10 7e 11 9b f0 2f 00 f9 
  00000620  f1 03 40 f9 f0 2f 40 f9  30 02 00 f9 f0 03 40 f9 
  00000630  11 02 40 f9 f1 37 00 f9  e0 37 40 f9 bf 03 00 91 
  00000640  fd 7b 4a a9 ff c3 02 91  c0 03 5f d6 ff 03 02 d1 
  00000650  fd 7b 07 a9 fd 03 00 91  e0 2f 00 f9 f0 03 00 91 
  00000660  10 82 01 91 f0 03 00 f9  f0 2f 40 f9 f0 07 00 f9 
  00000670  f0 07 40 f9 11 02 40 f9  f1 0b 00 f9 f0 2f 40 f9 
  00000680  f0 0f 00 f9 f0 0f 40 f9  11 01 80 d2 10 02 11 8b 
  00000690  f0 13 00 f9 f0 13 40 f9  f0 17 00 f9 f0 17 40 f9 
  000006a0  11 02 40 f9 f1 1b 00 f9  f0 0b 40 f9 f1 1b 40 f9 
  000006b0  1f 02 11 eb f0 17 9f 9a  f0 1f 00 f9 f1 03 40 f9 
  000006c0  f0 e3 40 39 30 02 00 39  f0 03 40 f9 11 02 40 39 
  000006d0  f1 27 00 f9 e0 23 41 39  bf 03 00 91 fd 7b 47 a9 
  000006e0  ff 03 02 91 c0 03 5f d6  ff 43 10 d1 f0 03 00 91 
  000006f0  10 02 10 91 1d 7a 00 a9  fd 03 00 91 00 00 00 90 
  00000700  00 00 00 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000710  00 a0 00 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000720  00 60 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000730  00 20 02 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000740  00 c0 02 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000750  00 e0 02 91 00 00 00 94  e0 03 00 91 00 a0 0d 91 
  00000760  41 01 80 d2 82 02 80 d2  26 fe ff 97 f0 03 00 91 
  00000770  10 a2 0d 91 f0 23 00 f9  f0 03 00 91 10 62 0e 91 
  00000780  f0 27 00 f9 f1 27 40 f9  f0 b7 41 f9 e9 03 11 aa 
  00000790  30 01 00 f9 f0 bb 41 f9  e9 03 11 aa 29 21 00 91 
  000007a0  30 01 00 f9 01 00 00 14  e0 03 00 91 00 e0 0d 91 
  000007b0  a1 00 80 d2 e2 01 80 d2  12 fe ff 97 f0 03 00 91 
  000007c0  10 e2 0d 91 f0 2f 00 f9  f0 03 00 91 10 a2 0e 91 
  000007d0  f0 33 00 f9 f1 33 40 f9  f0 bf 41 f9 e9 03 11 aa 
  000007e0  30 01 00 f9 f0 c3 41 f9  e9 03 11 aa 29 21 00 91 
  000007f0  30 01 00 f9 01 00 00 14  f0 27 40 f9 f0 3b 00 f9 
  00000800  f0 3b 40 f9 11 02 40 f9  f1 3f 00 f9 f0 27 40 f9 
  00000810  f0 43 00 f9 f0 43 40 f9  11 01 80 d2 10 02 11 8b 
  00000820  f0 47 00 f9 f0 47 40 f9  f0 4b 00 f9 f0 4b 40 f9 
  00000830  11 02 40 f9 f1 4f 00 f9  00 00 00 90 00 00 00 91 
  00000840  00 60 03 91 e1 3f 40 f9  f0 3f 40 f9 f0 03 00 f9 
  00000850  e2 4f 40 f9 f0 4f 40 f9  f0 07 00 f9 00 00 00 94 
  00000860  f0 33 40 f9 f0 57 00 f9  f0 57 40 f9 11 02 40 f9 
  00000870  f1 5b 00 f9 f0 33 40 f9  f0 5f 00 f9 f0 5f 40 f9 
  00000880  11 01 80 d2 10 02 11 8b  f0 63 00 f9 f0 63 40 f9 
  00000890  f0 67 00 f9 f0 67 40 f9  11 02 40 f9 f1 6b 00 f9 
  000008a0  00 00 00 90 00 00 00 91  00 c0 03 91 e1 5b 40 f9 
  000008b0  f0 5b 40 f9 f0 03 00 f9  e2 6b 40 f9 f0 6b 40 f9 
  000008c0  f0 07 00 f9 00 00 00 94  f0 03 00 91 10 e2 0e 91 
  000008d0  f0 73 00 f9 f1 73 40 f9  f0 27 40 f9 30 02 00 f9 
  000008e0  f0 03 00 91 10 02 0f 91  f0 7b 00 f9 10 00 80 d2 
  000008f0  10 12 00 d1 f0 7f 00 f9  f1 7b 40 f9 f0 7f 40 f9 
  00000900  30 02 00 f9 f0 73 40 f9  11 02 40 f9 f1 87 00 f9 
  00000910  f0 7b 40 f9 11 02 40 f9  f1 8b 00 f9 e0 87 40 f9 
  00000920  61 00 80 d2 e2 8b 40 f9  ef fd ff 97 01 00 00 14 
  00000930  f0 27 40 f9 f0 93 00 f9  f0 93 40 f9 11 02 40 f9 
  00000940  f1 97 00 f9 f0 27 40 f9  f0 9b 00 f9 f0 9b 40 f9 
  00000950  11 01 80 d2 10 02 11 8b  f0 9f 00 f9 f0 9f 40 f9 
  00000960  f0 a3 00 f9 f0 a3 40 f9  11 02 40 f9 f1 a7 00 f9 
  00000970  00 00 00 90 00 00 00 91  00 20 04 91 e1 97 40 f9 
  00000980  f0 97 40 f9 f0 03 00 f9  e2 a7 40 f9 f0 a7 40 f9 
  00000990  f0 07 00 f9 00 00 00 94  f0 03 00 91 10 22 0f 91 
  000009a0  f0 af 00 f9 f1 af 40 f9  f0 27 40 f9 30 02 00 f9 
  000009b0  f0 03 00 91 10 42 0f 91  f0 b7 00 f9 f1 b7 40 f9 
  000009c0  f0 33 40 f9 30 02 00 f9  f0 af 40 f9 11 02 40 f9 
  000009d0  f1 bf 00 f9 f0 b7 40 f9  11 02 40 f9 f1 c3 00 f9 
  000009e0  e0 bf 40 f9 e1 c3 40 f9  04 fe ff 97 e0 c7 00 f9 
  000009f0  01 00 00 14 00 00 00 90  00 00 00 91 00 c0 04 91 
  00000a00  e1 c7 40 f9 f0 c7 40 f9  f0 03 00 f9 00 00 00 94 
  00000a10  e0 03 00 91 00 20 0e 91  41 01 80 d2 a2 00 80 d2 
  00000a20  79 fe ff 97 f0 03 00 91  10 22 0e 91 f0 cf 00 f9 
  00000a30  f0 03 00 91 10 62 0f 91  f0 d3 00 f9 f1 d3 40 f9 
  00000a40  f0 c7 41 f9 e9 03 11 aa  30 01 00 f9 f0 cb 41 f9 
  00000a50  e9 03 11 aa 29 21 00 91  30 01 00 f9 01 00 00 14 
  00000a60  f0 d3 40 f9 f0 db 00 f9  f0 db 40 f9 11 02 40 f9 
  00000a70  f1 df 00 f9 f0 d3 40 f9  f0 e3 00 f9 f0 e3 40 f9 
  00000a80  11 01 80 d2 10 02 11 8b  f0 e7 00 f9 f0 e7 40 f9 
  00000a90  f0 eb 00 f9 f0 eb 40 f9  11 02 40 f9 f1 ef 00 f9 
  00000aa0  00 00 00 90 00 00 00 91  00 40 05 91 e1 df 40 f9 
  00000ab0  f0 df 40 f9 f0 03 00 f9  e2 ef 40 f9 f0 ef 40 f9 
  00000ac0  f0 07 00 f9 00 00 00 94  f0 03 00 91 10 a2 0f 91 
  00000ad0  f0 f7 00 f9 f1 f7 40 f9  f0 d3 40 f9 30 02 00 f9 
  00000ae0  f0 f7 40 f9 11 02 40 f9  f1 ff 00 f9 e0 ff 40 f9 
  00000af0  7e fe ff 97 e0 03 01 f9  01 00 00 14 00 00 00 90 
  00000b00  00 00 00 91 00 a0 05 91  e1 03 41 f9 f0 03 41 f9 
  00000b10  f0 03 00 f9 00 00 00 94  f0 03 00 91 10 c2 0f 91 
  00000b20  f0 0b 01 f9 f1 0b 41 f9  f0 d3 40 f9 30 02 00 f9 
  00000b30  f0 0b 41 f9 11 02 40 f9  f1 13 01 f9 e0 13 41 f9 
  00000b40  90 fe ff 97 e0 17 01 f9  01 00 00 14 00 00 00 90 
  00000b50  00 00 00 91 00 e0 05 91  e1 17 41 f9 f0 17 41 f9 
  00000b60  f0 03 00 f9 00 00 00 94  f0 03 00 91 10 e2 0f 91 
  00000b70  f0 1f 01 f9 f1 1f 41 f9  f0 d3 40 f9 30 02 00 f9 
  00000b80  f0 1f 41 f9 11 02 40 f9  f1 27 01 f9 e0 27 41 f9 
  00000b90  af fe ff 97 e0 2b 01 f9  01 00 00 14 00 00 00 90 
  00000ba0  00 00 00 91 00 40 06 91  e1 43 49 39 f0 43 49 39 
  00000bb0  f0 03 00 f9 00 00 00 94  bf 03 00 91 f0 03 00 91 
  00000bc0  10 02 10 91 1d 7a 40 a9  ff 43 10 91 00 00 80 d2 
  00000bd0  c0 03 5f d6 

.rodata (418 bytes):
  00000000  f0 9f 93 98 20 54 75 74  6f 72 69 61 6c 3a 20 30 
  00000010  36 5f 73 74 72 75 63 74  5f 6d 65 74 68 6f 64 73 
  00000020  2e 66 70 0a 00 00 00 00  f0 9f a7 ad 20 46 6f 63 
  00000030  75 73 3a 20 53 74 72 75  63 74 20 6d 65 74 68 6f 
  00000040  64 73 20 61 6e 64 20 66  69 65 6c 64 20 61 63 63 
  00000050  65 73 73 0a 00 00 00 00  f0 9f a7 aa 20 57 68 61 
  00000060  74 20 74 6f 20 6c 6f 6f  6b 20 66 6f 72 3a 20 6c 
  00000070  61 62 65 6c 65 64 20 6f  75 74 70 75 74 73 20 62 
  00000080  65 6c 6f 77 0a 00 00 00  e2 9c 85 20 45 78 70 65 
  00000090  63 74 61 74 69 6f 6e 3a  20 6f 75 74 70 75 74 73 
  000000a0  20 6d 61 74 63 68 20 6c  61 62 65 6c 73 0a 00 00 
  000000b0  0a 00 00 00 00 00 00 00  3d 3d 3d 20 53 74 72 75 
  000000c0  63 74 20 4f 70 65 72 61  74 69 6f 6e 73 20 3d 3d 
  000000d0  3d 0a 00 00 00 00 00 00  70 31 20 3d 20 28 25 6c 
  000000e0  6c 64 2c 20 25 6c 6c 64  29 0a 00 00 00 00 00 00 
  000000f0  70 32 20 3d 20 28 25 6c  6c 64 2c 20 25 6c 6c 64 
  00000100  29 0a 00 00 00 00 00 00  70 31 20 61 66 74 65 72 
  00000110  20 74 72 61 6e 73 6c 61  74 65 20 3d 20 28 25 6c 
  00000120  6c 64 2c 20 25 6c 6c 64  29 0a 00 00 00 00 00 00 
  00000130  44 69 73 74 61 6e 63 65  c2 b2 28 70 31 2c 20 70 
  00000140  32 29 20 3d 20 25 6c 6c  64 0a 00 00 00 00 00 00 
  00000150  52 65 63 74 61 6e 67 6c  65 3a 20 25 6c 6c 64 c3 
  00000160  97 25 6c 6c 64 0a 00 00  20 20 61 72 65 61 20 3d 
  00000170  20 25 6c 6c 64 0a 00 00  20 20 70 65 72 69 6d 65 
  00000180  74 65 72 20 3d 20 25 6c  6c 64 0a 00 00 00 00 00 
  00000190  20 20 69 73 5f 73 71 75  61 72 65 20 3d 20 25 64 
  000001a0  0a 00 
