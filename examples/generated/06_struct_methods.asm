fp-native dump: format=MachO arch=Aarch64 entry=0x774

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
fn Point__new
  bb0 bb0
    alloca Virtual { id: 3, bank: General, size_bits: 64 }, 16
    insertvalue Virtual { id: 4, bank: General, size_bits: 64 }, 0, symbol(local.1), 0
    insertvalue Virtual { id: 5, bank: General, size_bits: 64 }, Virtual { id: 4, bank: General, size_bits: 64 }, symbol(local.2), 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 5, bank: General, size_bits: 64 }
    load Virtual { id: 7, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 3, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn Point__translate
  bb0 bb0
    alloca Virtual { id: 8, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 8, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    load Virtual { id: 10, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 8, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 11, bank: General, size_bits: 64 }, Virtual { id: 10, bank: General, size_bits: 64 }
    load Virtual { id: 12, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 8, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 13, bank: General, size_bits: 64 }, Virtual { id: 12, bank: General, size_bits: 64 }
    load Virtual { id: 14, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 13, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 15, bank: General, size_bits: 64 }, Virtual { id: 14, bank: General, size_bits: 64 }, symbol(local.2)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 11, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 15, bank: General, size_bits: 64 }
    load Virtual { id: 17, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 8, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 18, bank: General, size_bits: 64 }, Virtual { id: 17, bank: General, size_bits: 64 }
    gep Virtual { id: 19, bank: General, size_bits: 64 }, Virtual { id: 18, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 20, bank: General, size_bits: 64 }, Virtual { id: 19, bank: General, size_bits: 64 }
    load Virtual { id: 21, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 8, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 22, bank: General, size_bits: 64 }, Virtual { id: 21, bank: General, size_bits: 64 }
    gep Virtual { id: 23, bank: General, size_bits: 64 }, Virtual { id: 22, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 24, bank: General, size_bits: 64 }, Virtual { id: 23, bank: General, size_bits: 64 }
    load Virtual { id: 25, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 24, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 26, bank: General, size_bits: 64 }, Virtual { id: 25, bank: General, size_bits: 64 }, symbol(local.3)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 20, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 26, bank: General, size_bits: 64 }
    ret
fn Point__distance2
  bb0 bb0
    alloca Virtual { id: 28, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 29, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 30, bank: General, size_bits: 64 }, symbol(local.1)
    load Virtual { id: 31, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 30, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 32, bank: General, size_bits: 64 }, symbol(local.2)
    load Virtual { id: 33, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 32, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    sub Virtual { id: 34, bank: General, size_bits: 64 }, Virtual { id: 31, bank: General, size_bits: 64 }, Virtual { id: 33, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 29, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 34, bank: General, size_bits: 64 }
    alloca Virtual { id: 36, bank: General, size_bits: 64 }, 8
    load Virtual { id: 37, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 29, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 37, bank: General, size_bits: 64 }
    alloca Virtual { id: 39, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 40, bank: General, size_bits: 64 }, symbol(local.1)
    gep Virtual { id: 41, bank: General, size_bits: 64 }, Virtual { id: 40, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 42, bank: General, size_bits: 64 }, Virtual { id: 41, bank: General, size_bits: 64 }
    load Virtual { id: 43, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 42, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 44, bank: General, size_bits: 64 }, symbol(local.2)
    gep Virtual { id: 45, bank: General, size_bits: 64 }, Virtual { id: 44, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 46, bank: General, size_bits: 64 }, Virtual { id: 45, bank: General, size_bits: 64 }
    load Virtual { id: 47, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 46, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    sub Virtual { id: 48, bank: General, size_bits: 64 }, Virtual { id: 43, bank: General, size_bits: 64 }, Virtual { id: 47, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 39, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 48, bank: General, size_bits: 64 }
    alloca Virtual { id: 50, bank: General, size_bits: 64 }, 8
    load Virtual { id: 51, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 39, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 50, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 51, bank: General, size_bits: 64 }
    alloca Virtual { id: 53, bank: General, size_bits: 64 }, 8
    load Virtual { id: 54, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 55, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 56, bank: General, size_bits: 64 }, Virtual { id: 54, bank: General, size_bits: 64 }, Virtual { id: 55, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 53, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 56, bank: General, size_bits: 64 }
    alloca Virtual { id: 58, bank: General, size_bits: 64 }, 8
    load Virtual { id: 59, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 50, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 60, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 50, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 61, bank: General, size_bits: 64 }, Virtual { id: 59, bank: General, size_bits: 64 }, Virtual { id: 60, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 58, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 61, bank: General, size_bits: 64 }
    load Virtual { id: 63, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 53, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 64, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 58, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 65, bank: General, size_bits: 64 }, Virtual { id: 63, bank: General, size_bits: 64 }, Virtual { id: 64, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 28, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 65, bank: General, size_bits: 64 }
    load Virtual { id: 67, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 28, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn Rectangle__new
  bb0 bb0
    alloca Virtual { id: 68, bank: General, size_bits: 64 }, 16
    insertvalue Virtual { id: 69, bank: General, size_bits: 64 }, 0, symbol(local.1), 0
    insertvalue Virtual { id: 70, bank: General, size_bits: 64 }, Virtual { id: 69, bank: General, size_bits: 64 }, symbol(local.2), 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 68, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 70, bank: General, size_bits: 64 }
    load Virtual { id: 72, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 68, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn Rectangle__area
  bb0 bb0
    alloca Virtual { id: 73, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 74, bank: General, size_bits: 64 }, symbol(local.1)
    load Virtual { id: 75, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 74, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 76, bank: General, size_bits: 64 }, symbol(local.1)
    gep Virtual { id: 77, bank: General, size_bits: 64 }, Virtual { id: 76, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 78, bank: General, size_bits: 64 }, Virtual { id: 77, bank: General, size_bits: 64 }
    load Virtual { id: 79, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 78, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 80, bank: General, size_bits: 64 }, Virtual { id: 75, bank: General, size_bits: 64 }, Virtual { id: 79, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 73, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 80, bank: General, size_bits: 64 }
    load Virtual { id: 82, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 73, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn Rectangle__perimeter
  bb0 bb0
    alloca Virtual { id: 83, bank: General, size_bits: 64 }, 8
    alloca Virtual { id: 84, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 85, bank: General, size_bits: 64 }, symbol(local.1)
    load Virtual { id: 86, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 85, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 87, bank: General, size_bits: 64 }, symbol(local.1)
    gep Virtual { id: 88, bank: General, size_bits: 64 }, Virtual { id: 87, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 89, bank: General, size_bits: 64 }, Virtual { id: 88, bank: General, size_bits: 64 }
    load Virtual { id: 90, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 89, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    add Virtual { id: 91, bank: General, size_bits: 64 }, Virtual { id: 86, bank: General, size_bits: 64 }, Virtual { id: 90, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 84, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 91, bank: General, size_bits: 64 }
    load Virtual { id: 93, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 84, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    mul Virtual { id: 94, bank: General, size_bits: 64 }, 2, Virtual { id: 93, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 83, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 94, bank: General, size_bits: 64 }
    load Virtual { id: 96, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 83, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn Rectangle__is_square
  bb0 bb0
    alloca Virtual { id: 97, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 98, bank: General, size_bits: 64 }, symbol(local.1)
    load Virtual { id: 99, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 98, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 100, bank: General, size_bits: 64 }, symbol(local.1)
    gep Virtual { id: 101, bank: General, size_bits: 64 }, Virtual { id: 100, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 102, bank: General, size_bits: 64 }, Virtual { id: 101, bank: General, size_bits: 64 }
    load Virtual { id: 103, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 102, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 104, bank: General, size_bits: 8 }, Virtual { id: 99, bank: General, size_bits: 64 }, Virtual { id: 103, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 97, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 104, bank: General, size_bits: 8 }
    load Virtual { id: 106, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 97, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
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
    alloca Virtual { id: 114, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 114, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 113, bank: General, size_bits: 64 }
    br
  bb1 bb1
    call symbol(Point__new)(5, 15) cc=C tail=false
    alloca Virtual { id: 117, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 117, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 116, bank: General, size_bits: 64 }
    br
  bb2 bb2
    bitcast Virtual { id: 119, bank: General, size_bits: 64 }, Virtual { id: 114, bank: General, size_bits: 64 }
    load Virtual { id: 120, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 119, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 121, bank: General, size_bits: 64 }, Virtual { id: 114, bank: General, size_bits: 64 }
    gep Virtual { id: 122, bank: General, size_bits: 64 }, Virtual { id: 121, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 123, bank: General, size_bits: 64 }, Virtual { id: 122, bank: General, size_bits: 64 }
    load Virtual { id: 124, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 123, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 120, bank: General, size_bits: 64 }, Virtual { id: 124, bank: General, size_bits: 64 }
    bitcast Virtual { id: 126, bank: General, size_bits: 64 }, Virtual { id: 117, bank: General, size_bits: 64 }
    load Virtual { id: 127, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 126, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 128, bank: General, size_bits: 64 }, Virtual { id: 117, bank: General, size_bits: 64 }
    gep Virtual { id: 129, bank: General, size_bits: 64 }, Virtual { id: 128, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 130, bank: General, size_bits: 64 }, Virtual { id: 129, bank: General, size_bits: 64 }
    load Virtual { id: 131, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 130, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 127, bank: General, size_bits: 64 }, Virtual { id: 131, bank: General, size_bits: 64 }
    alloca Virtual { id: 133, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 133, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 114, bank: General, size_bits: 64 }
    alloca Virtual { id: 135, bank: General, size_bits: 64 }, 8
    sub Virtual { id: 136, bank: General, size_bits: 64 }, 0, 4
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 135, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 136, bank: General, size_bits: 64 }
    load Virtual { id: 138, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 133, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 139, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 135, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(Point__translate)(v138, 3, v139) cc=C tail=false
    br
  bb3 bb3
    bitcast Virtual { id: 141, bank: General, size_bits: 64 }, Virtual { id: 114, bank: General, size_bits: 64 }
    load Virtual { id: 142, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 141, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 143, bank: General, size_bits: 64 }, Virtual { id: 114, bank: General, size_bits: 64 }
    gep Virtual { id: 144, bank: General, size_bits: 64 }, Virtual { id: 143, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 145, bank: General, size_bits: 64 }, Virtual { id: 144, bank: General, size_bits: 64 }
    load Virtual { id: 146, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 145, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 142, bank: General, size_bits: 64 }, Virtual { id: 146, bank: General, size_bits: 64 }
    alloca Virtual { id: 148, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 148, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 114, bank: General, size_bits: 64 }
    alloca Virtual { id: 150, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 150, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 117, bank: General, size_bits: 64 }
    load Virtual { id: 152, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 148, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 153, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 150, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(Point__distance2)(v152, v153) cc=C tail=false
    br
  bb4 bb4
    intrinsic.call symbol(intrinsic.println), Virtual { id: 154, bank: General, size_bits: 64 }
    call symbol(Rectangle__new)(10, 5) cc=C tail=false
    alloca Virtual { id: 157, bank: General, size_bits: 64 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 157, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 156, bank: General, size_bits: 64 }
    br
  bb5 bb5
    bitcast Virtual { id: 159, bank: General, size_bits: 64 }, Virtual { id: 157, bank: General, size_bits: 64 }
    load Virtual { id: 160, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 159, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 161, bank: General, size_bits: 64 }, Virtual { id: 157, bank: General, size_bits: 64 }
    gep Virtual { id: 162, bank: General, size_bits: 64 }, Virtual { id: 161, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 163, bank: General, size_bits: 64 }, Virtual { id: 162, bank: General, size_bits: 64 }
    load Virtual { id: 164, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 163, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 160, bank: General, size_bits: 64 }, Virtual { id: 164, bank: General, size_bits: 64 }
    alloca Virtual { id: 166, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 166, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 157, bank: General, size_bits: 64 }
    load Virtual { id: 168, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 166, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(Rectangle__area)(v168) cc=C tail=false
    br
  bb6 bb6
    intrinsic.call symbol(intrinsic.println), Virtual { id: 169, bank: General, size_bits: 64 }
    alloca Virtual { id: 171, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 171, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 157, bank: General, size_bits: 64 }
    load Virtual { id: 173, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 171, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(Rectangle__perimeter)(v173) cc=C tail=false
    br
  bb7 bb7
    intrinsic.call symbol(intrinsic.println), Virtual { id: 174, bank: General, size_bits: 64 }
    alloca Virtual { id: 176, bank: General, size_bits: 64 }, 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 176, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 157, bank: General, size_bits: 64 }
    load Virtual { id: 178, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 176, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(Rectangle__is_square)(v178) cc=C tail=false
    br
  bb8 bb8
    intrinsic.call symbol(intrinsic.println), Virtual { id: 179, bank: General, size_bits: 8 }
    ret


Symbols:
  Point__new                       0x00000000
  Point__translate                 0x000000f8
  Point__distance2                 0x00000220
  Rectangle__new                   0x00000440
  Rectangle__area                  0x00000538
  Rectangle__perimeter             0x000005e4
  Rectangle__is_square             0x000006c4
  main                             0x00000774

Text relocations:
  offset=0x0000078c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000794 kind=CallRel32 symbol=printf addend=0
  offset=0x00000798 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000007a4 kind=CallRel32 symbol=printf addend=0
  offset=0x000007a8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000007b4 kind=CallRel32 symbol=printf addend=0
  offset=0x000007b8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000007c4 kind=CallRel32 symbol=printf addend=0
  offset=0x000007c8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000007d4 kind=CallRel32 symbol=printf addend=0
  offset=0x000007d8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000007e4 kind=CallRel32 symbol=printf addend=0
  offset=0x000008c8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000008ec kind=CallRel32 symbol=printf addend=0
  offset=0x00000930 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000954 kind=CallRel32 symbol=printf addend=0
  offset=0x00000a00 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000a24 kind=CallRel32 symbol=printf addend=0
  offset=0x00000a84 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000a9c kind=CallRel32 symbol=printf addend=0
  offset=0x00000b30 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000b54 kind=CallRel32 symbol=printf addend=0
  offset=0x00000b8c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000ba4 kind=CallRel32 symbol=printf addend=0
  offset=0x00000bdc kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000bf4 kind=CallRel32 symbol=printf addend=0
  offset=0x00000c2c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000c44 kind=CallRel32 symbol=printf addend=0

.text (3172 bytes):
  00000000  ff 03 17 d1 f0 03 00 91  10 c2 16 91 1d 7a 00 a9 
  00000010  fd 03 00 91 e0 3b 02 f9  e1 33 02 f9 e2 37 02 f9 
  00000020  1f 20 03 d5 f0 03 00 91  10 a2 12 91 f0 03 00 f9 
  00000030  10 00 80 d2 f0 3f 02 f9  f0 43 02 f9 f0 33 42 f9 
  00000040  f0 3f 02 f9 f0 03 00 91  10 e2 11 91 f0 07 00 f9 
  00000050  f0 3f 42 f9 f0 47 02 f9  f0 43 42 f9 f0 4b 02 f9 
  00000060  f0 37 42 f9 f0 4b 02 f9  f0 03 00 91 10 22 12 91 
  00000070  f0 0b 00 f9 f1 03 40 f9  f0 47 42 f9 e9 03 11 aa 
  00000080  30 01 00 f9 f0 4b 42 f9  e9 03 11 aa 29 21 00 91 
  00000090  30 01 00 f9 f1 03 40 f9  e9 03 11 aa 30 01 40 f9 
  000000a0  f0 4f 02 f9 e9 03 11 aa  29 21 00 91 30 01 40 f9 
  000000b0  f0 53 02 f9 f0 03 00 91  10 62 12 91 f0 13 00 f9 
  000000c0  f1 3b 42 f9 f0 4f 42 f9  e9 03 11 aa 30 01 00 f9 
  000000d0  f0 53 42 f9 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  000000e0  bf 03 00 91 f0 03 00 91  10 c2 16 91 1d 7a 40 a9 
  000000f0  ff 03 17 91 c0 03 5f d6  ff 43 13 d1 f0 03 00 91 
  00000100  10 02 13 91 1d 7a 00 a9  fd 03 00 91 e0 37 02 f9 
  00000110  e1 3b 02 f9 e2 3f 02 f9  1f 20 03 d5 f0 03 00 91 
  00000120  10 02 12 91 f0 13 00 f9  f1 13 40 f9 f0 37 42 f9 
  00000130  30 02 00 f9 f0 13 40 f9  11 02 40 f9 f1 1b 00 f9 
  00000140  f0 1b 40 f9 f0 1f 00 f9  f0 13 40 f9 11 02 40 f9 
  00000150  f1 23 00 f9 f0 23 40 f9  f0 27 00 f9 f0 27 40 f9 
  00000160  11 02 40 f9 f1 2b 00 f9  f0 2b 40 f9 f1 3b 42 f9 
  00000170  10 02 11 8b f0 2f 00 f9  f1 1f 40 f9 f0 2f 40 f9 
  00000180  30 02 00 f9 f0 13 40 f9  11 02 40 f9 f1 37 00 f9 
  00000190  f0 37 40 f9 f0 3b 00 f9  f0 3b 40 f9 11 01 80 d2 
  000001a0  10 02 11 8b f0 3f 00 f9  f0 3f 40 f9 f0 43 00 f9 
  000001b0  f0 13 40 f9 11 02 40 f9  f1 47 00 f9 f0 47 40 f9 
  000001c0  f0 4b 00 f9 f0 4b 40 f9  11 01 80 d2 10 02 11 8b 
  000001d0  f0 4f 00 f9 f0 4f 40 f9  f0 53 00 f9 f0 53 40 f9 
  000001e0  11 02 40 f9 f1 57 00 f9  f0 57 40 f9 f1 3f 42 f9 
  000001f0  10 02 11 8b f0 5b 00 f9  f1 43 40 f9 f0 5b 40 f9 
  00000200  30 02 00 f9 bf 03 00 91  f0 03 00 91 10 02 13 91 
  00000210  1d 7a 40 a9 ff 43 13 91  00 00 80 d2 c0 03 5f d6 
  00000220  ff 83 1a d1 f0 03 00 91  10 42 1a 91 1d 7a 00 a9 
  00000230  fd 03 00 91 e0 47 02 f9  e1 4b 02 f9 1f 20 03 d5 
  00000240  f0 03 00 91 10 22 13 91  f0 57 00 f9 f0 03 00 91 
  00000250  10 22 14 91 f0 5b 00 f9  f0 47 42 f9 f0 5f 00 f9 
  00000260  f0 5f 40 f9 11 02 40 f9  f1 63 00 f9 f0 4b 42 f9 
  00000270  f0 67 00 f9 f0 67 40 f9  11 02 40 f9 f1 6b 00 f9 
  00000280  f0 63 40 f9 f1 6b 40 f9  10 02 11 cb f0 6f 00 f9 
  00000290  f1 5b 40 f9 f0 6f 40 f9  30 02 00 f9 f0 03 00 91 
  000002a0  10 22 15 91 f0 77 00 f9  f0 5b 40 f9 11 02 40 f9 
  000002b0  f1 7b 00 f9 f1 77 40 f9  f0 7b 40 f9 30 02 00 f9 
  000002c0  f0 03 00 91 10 22 16 91  f0 83 00 f9 f0 47 42 f9 
  000002d0  f0 87 00 f9 f0 87 40 f9  11 01 80 d2 10 02 11 8b 
  000002e0  f0 8b 00 f9 f0 8b 40 f9  f0 8f 00 f9 f0 8f 40 f9 
  000002f0  11 02 40 f9 f1 93 00 f9  f0 4b 42 f9 f0 97 00 f9 
  00000300  f0 97 40 f9 11 01 80 d2  10 02 11 8b f0 9b 00 f9 
  00000310  f0 9b 40 f9 f0 9f 00 f9  f0 9f 40 f9 11 02 40 f9 
  00000320  f1 a3 00 f9 f0 93 40 f9  f1 a3 40 f9 10 02 11 cb 
  00000330  f0 a7 00 f9 f1 83 40 f9  f0 a7 40 f9 30 02 00 f9 
  00000340  f0 03 00 91 10 22 17 91  f0 af 00 f9 f0 83 40 f9 
  00000350  11 02 40 f9 f1 b3 00 f9  f1 af 40 f9 f0 b3 40 f9 
  00000360  30 02 00 f9 f0 03 00 91  10 22 18 91 f0 bb 00 f9 
  00000370  f0 77 40 f9 11 02 40 f9  f1 bf 00 f9 f0 77 40 f9 
  00000380  11 02 40 f9 f1 c3 00 f9  f0 bf 40 f9 f1 c3 40 f9 
  00000390  10 7e 11 9b f0 c7 00 f9  f1 bb 40 f9 f0 c7 40 f9 
  000003a0  30 02 00 f9 f0 03 00 91  10 22 19 91 f0 cf 00 f9 
  000003b0  f0 af 40 f9 11 02 40 f9  f1 d3 00 f9 f0 af 40 f9 
  000003c0  11 02 40 f9 f1 d7 00 f9  f0 d3 40 f9 f1 d7 40 f9 
  000003d0  10 7e 11 9b f0 db 00 f9  f1 cf 40 f9 f0 db 40 f9 
  000003e0  30 02 00 f9 f0 bb 40 f9  11 02 40 f9 f1 e3 00 f9 
  000003f0  f0 cf 40 f9 11 02 40 f9  f1 e7 00 f9 f0 e3 40 f9 
  00000400  f1 e7 40 f9 10 02 11 8b  f0 eb 00 f9 f1 57 40 f9 
  00000410  f0 eb 40 f9 30 02 00 f9  f0 57 40 f9 11 02 40 f9 
  00000420  f1 f3 00 f9 e0 f3 40 f9  bf 03 00 91 f0 03 00 91 
  00000430  10 42 1a 91 1d 7a 40 a9  ff 83 1a 91 c0 03 5f d6 
  00000440  ff 03 17 d1 f0 03 00 91  10 c2 16 91 1d 7a 00 a9 
  00000450  fd 03 00 91 e0 3b 02 f9  e1 33 02 f9 e2 37 02 f9 
  00000460  1f 20 03 d5 f0 03 00 91  10 a2 12 91 f0 db 00 f9 
  00000470  10 00 80 d2 f0 3f 02 f9  f0 43 02 f9 f0 33 42 f9 
  00000480  f0 3f 02 f9 f0 03 00 91  10 e2 11 91 f0 df 00 f9 
  00000490  f0 3f 42 f9 f0 47 02 f9  f0 43 42 f9 f0 4b 02 f9 
  000004a0  f0 37 42 f9 f0 4b 02 f9  f0 03 00 91 10 22 12 91 
  000004b0  f0 e3 00 f9 f1 db 40 f9  f0 47 42 f9 e9 03 11 aa 
  000004c0  30 01 00 f9 f0 4b 42 f9  e9 03 11 aa 29 21 00 91 
  000004d0  30 01 00 f9 f1 db 40 f9  e9 03 11 aa 30 01 40 f9 
  000004e0  f0 4f 02 f9 e9 03 11 aa  29 21 00 91 30 01 40 f9 
  000004f0  f0 53 02 f9 f0 03 00 91  10 62 12 91 f0 eb 00 f9 
  00000500  f1 3b 42 f9 f0 4f 42 f9  e9 03 11 aa 30 01 00 f9 
  00000510  f0 53 42 f9 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  00000520  bf 03 00 91 f0 03 00 91  10 c2 16 91 1d 7a 40 a9 
  00000530  ff 03 17 91 c0 03 5f d6  ff c3 12 d1 f0 03 00 91 
  00000540  10 82 12 91 1d 7a 00 a9  fd 03 00 91 e0 2f 02 f9 
  00000550  1f 20 03 d5 f0 03 00 91  10 82 11 91 f0 eb 00 f9 
  00000560  f0 2f 42 f9 f0 ef 00 f9  f0 ef 40 f9 11 02 40 f9 
  00000570  f1 f3 00 f9 f0 2f 42 f9  f0 f7 00 f9 f0 f7 40 f9 
  00000580  11 01 80 d2 10 02 11 8b  f0 fb 00 f9 f0 fb 40 f9 
  00000590  f0 ff 00 f9 f0 ff 40 f9  11 02 40 f9 f1 03 01 f9 
  000005a0  f0 f3 40 f9 f1 03 41 f9  10 7e 11 9b f0 07 01 f9 
  000005b0  f1 eb 40 f9 f0 07 41 f9  30 02 00 f9 f0 eb 40 f9 
  000005c0  11 02 40 f9 f1 0f 01 f9  e0 0f 41 f9 bf 03 00 91 
  000005d0  f0 03 00 91 10 82 12 91  1d 7a 40 a9 ff c3 12 91 
  000005e0  c0 03 5f d6 ff 03 14 d1  f0 03 00 91 10 c2 13 91 
  000005f0  1d 7a 00 a9 fd 03 00 91  e0 33 02 f9 1f 20 03 d5 
  00000600  f0 03 00 91 10 c2 11 91  f0 0f 01 f9 f0 03 00 91 
  00000610  10 c2 12 91 f0 13 01 f9  f0 33 42 f9 f0 17 01 f9 
  00000620  f0 17 41 f9 11 02 40 f9  f1 1b 01 f9 f0 33 42 f9 
  00000630  f0 1f 01 f9 f0 1f 41 f9  11 01 80 d2 10 02 11 8b 
  00000640  f0 23 01 f9 f0 23 41 f9  f0 27 01 f9 f0 27 41 f9 
  00000650  11 02 40 f9 f1 2b 01 f9  f0 1b 41 f9 f1 2b 41 f9 
  00000660  10 02 11 8b f0 2f 01 f9  f1 13 41 f9 f0 2f 41 f9 
  00000670  30 02 00 f9 f0 13 41 f9  11 02 40 f9 f1 37 01 f9 
  00000680  50 00 80 d2 f1 37 41 f9  10 7e 11 9b f0 3b 01 f9 
  00000690  f1 0f 41 f9 f0 3b 41 f9  30 02 00 f9 f0 0f 41 f9 
  000006a0  11 02 40 f9 f1 43 01 f9  e0 43 41 f9 bf 03 00 91 
  000006b0  f0 03 00 91 10 c2 13 91  1d 7a 40 a9 ff 03 14 91 
  000006c0  c0 03 5f d6 ff 03 12 d1  f0 03 00 91 10 c2 11 91 
  000006d0  1d 7a 00 a9 fd 03 00 91  e0 2f 02 f9 1f 20 03 d5 
  000006e0  f0 03 00 91 10 82 11 91  f0 3f 01 f9 f0 2f 42 f9 
  000006f0  f0 43 01 f9 f0 43 41 f9  11 02 40 f9 f1 47 01 f9 
  00000700  f0 2f 42 f9 f0 4b 01 f9  f0 4b 41 f9 11 01 80 d2 
  00000710  10 02 11 8b f0 4f 01 f9  f0 4f 41 f9 f0 53 01 f9 
  00000720  f0 53 41 f9 11 02 40 f9  f1 57 01 f9 f0 47 41 f9 
  00000730  f1 57 41 f9 1f 02 11 eb  f0 17 9f 9a f0 5b 01 f9 
  00000740  f1 3f 41 f9 f0 c3 4a 39  30 02 00 39 f0 3f 41 f9 
  00000750  11 02 40 39 f1 63 01 f9  e0 03 4b 39 bf 03 00 91 
  00000760  f0 03 00 91 10 c2 11 91  1d 7a 40 a9 ff 03 12 91 
  00000770  c0 03 5f d6 ff c3 2c d1  f0 03 00 91 10 82 2c 91 
  00000780  1d 7a 00 a9 fd 03 00 91  1f 20 03 d5 00 00 00 90 
  00000790  00 00 00 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000007a0  00 a0 00 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000007b0  00 60 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000007c0  00 20 02 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000007d0  00 c0 02 91 00 00 00 94  00 00 00 90 00 00 00 91 
  000007e0  00 e0 02 91 00 00 00 94  e0 03 00 91 00 a0 18 91 
  000007f0  41 01 80 d2 82 02 80 d2  02 fe ff 97 f0 03 00 91 
  00000800  10 a2 18 91 f0 83 01 f9  f0 03 00 91 10 62 19 91 
  00000810  f0 87 01 f9 f1 87 41 f9  f0 17 43 f9 e9 03 11 aa 
  00000820  30 01 00 f9 f0 1b 43 f9  e9 03 11 aa 29 21 00 91 
  00000830  30 01 00 f9 01 00 00 14  e0 03 00 91 00 e0 18 91 
  00000840  a1 00 80 d2 e2 01 80 d2  ee fd ff 97 f0 03 00 91 
  00000850  10 e2 18 91 f0 8f 01 f9  f0 03 00 91 10 62 1d 91 
  00000860  f0 93 01 f9 f1 93 41 f9  f0 1f 43 f9 e9 03 11 aa 
  00000870  30 01 00 f9 f0 23 43 f9  e9 03 11 aa 29 21 00 91 
  00000880  30 01 00 f9 01 00 00 14  f0 87 41 f9 f0 9b 01 f9 
  00000890  f0 9b 41 f9 11 02 40 f9  f1 9f 01 f9 f0 87 41 f9 
  000008a0  f0 a3 01 f9 f0 a3 41 f9  11 01 80 d2 10 02 11 8b 
  000008b0  f0 a7 01 f9 f0 a7 41 f9  f0 ab 01 f9 f0 ab 41 f9 
  000008c0  11 02 40 f9 f1 af 01 f9  00 00 00 90 00 00 00 91 
  000008d0  00 60 03 91 e1 9f 41 f9  f0 9f 41 f9 f0 03 00 f9 
  000008e0  e2 af 41 f9 f0 af 41 f9  f0 07 00 f9 00 00 00 94 
  000008f0  f0 93 41 f9 f0 b7 01 f9  f0 b7 41 f9 11 02 40 f9 
  00000900  f1 bb 01 f9 f0 93 41 f9  f0 bf 01 f9 f0 bf 41 f9 
  00000910  11 01 80 d2 10 02 11 8b  f0 c3 01 f9 f0 c3 41 f9 
  00000920  f0 c7 01 f9 f0 c7 41 f9  11 02 40 f9 f1 cb 01 f9 
  00000930  00 00 00 90 00 00 00 91  00 c0 03 91 e1 bb 41 f9 
  00000940  f0 bb 41 f9 f0 03 00 f9  e2 cb 41 f9 f0 cb 41 f9 
  00000950  f0 07 00 f9 00 00 00 94  f0 03 00 91 10 62 21 91 
  00000960  f0 d3 01 f9 f1 d3 41 f9  f0 87 41 f9 30 02 00 f9 
  00000970  f0 03 00 91 10 62 22 91  f0 db 01 f9 10 00 80 d2 
  00000980  10 12 00 d1 f0 df 01 f9  f1 db 41 f9 f0 df 41 f9 
  00000990  30 02 00 f9 f0 d3 41 f9  11 02 40 f9 f1 e7 01 f9 
  000009a0  f0 db 41 f9 11 02 40 f9  f1 eb 01 f9 e0 e7 41 f9 
  000009b0  61 00 80 d2 e2 eb 41 f9  d0 fd ff 97 01 00 00 14 
  000009c0  f0 87 41 f9 f0 f3 01 f9  f0 f3 41 f9 11 02 40 f9 
  000009d0  f1 f7 01 f9 f0 87 41 f9  f0 fb 01 f9 f0 fb 41 f9 
  000009e0  11 01 80 d2 10 02 11 8b  f0 ff 01 f9 f0 ff 41 f9 
  000009f0  f0 03 02 f9 f0 03 42 f9  11 02 40 f9 f1 07 02 f9 
  00000a00  00 00 00 90 00 00 00 91  00 20 04 91 e1 f7 41 f9 
  00000a10  f0 f7 41 f9 f0 03 00 f9  e2 07 42 f9 f0 07 42 f9 
  00000a20  f0 07 00 f9 00 00 00 94  f0 03 00 91 10 62 23 91 
  00000a30  f0 0f 02 f9 f1 0f 42 f9  f0 87 41 f9 30 02 00 f9 
  00000a40  f0 03 00 91 10 62 24 91  f0 17 02 f9 f1 17 42 f9 
  00000a50  f0 93 41 f9 30 02 00 f9  f0 0f 42 f9 11 02 40 f9 
  00000a60  f1 1f 02 f9 f0 17 42 f9  11 02 40 f9 f1 23 02 f9 
  00000a70  e0 1f 42 f9 e1 23 42 f9  ea fd ff 97 e0 27 02 f9 
  00000a80  01 00 00 14 00 00 00 90  00 00 00 91 00 c0 04 91 
  00000a90  e1 27 42 f9 f0 27 42 f9  f0 03 00 f9 00 00 00 94 
  00000aa0  e0 03 00 91 00 20 19 91  41 01 80 d2 a2 00 80 d2 
  00000ab0  64 fe ff 97 f0 03 00 91  10 22 19 91 f0 2f 02 f9 
  00000ac0  f0 03 00 91 10 62 25 91  f0 33 02 f9 f1 33 42 f9 
  00000ad0  f0 27 43 f9 e9 03 11 aa  30 01 00 f9 f0 2b 43 f9 
  00000ae0  e9 03 11 aa 29 21 00 91  30 01 00 f9 01 00 00 14 
  00000af0  f0 33 42 f9 f0 3b 02 f9  f0 3b 42 f9 11 02 40 f9 
  00000b00  f1 3f 02 f9 f0 33 42 f9  f0 43 02 f9 f0 43 42 f9 
  00000b10  11 01 80 d2 10 02 11 8b  f0 47 02 f9 f0 47 42 f9 
  00000b20  f0 4b 02 f9 f0 4b 42 f9  11 02 40 f9 f1 4f 02 f9 
  00000b30  00 00 00 90 00 00 00 91  00 40 05 91 e1 3f 42 f9 
  00000b40  f0 3f 42 f9 f0 03 00 f9  e2 4f 42 f9 f0 4f 42 f9 
  00000b50  f0 07 00 f9 00 00 00 94  f0 03 00 91 10 62 29 91 
  00000b60  f0 57 02 f9 f1 57 42 f9  f0 33 42 f9 30 02 00 f9 
  00000b70  f0 57 42 f9 11 02 40 f9  f1 5f 02 f9 e0 5f 42 f9 
  00000b80  6e fe ff 97 e0 63 02 f9  01 00 00 14 00 00 00 90 
  00000b90  00 00 00 91 00 a0 05 91  e1 63 42 f9 f0 63 42 f9 
  00000ba0  f0 03 00 f9 00 00 00 94  f0 03 00 91 10 62 2a 91 
  00000bb0  f0 6b 02 f9 f1 6b 42 f9  f0 33 42 f9 30 02 00 f9 
  00000bc0  f0 6b 42 f9 11 02 40 f9  f1 73 02 f9 e0 73 42 f9 
  00000bd0  85 fe ff 97 e0 77 02 f9  01 00 00 14 00 00 00 90 
  00000be0  00 00 00 91 00 e0 05 91  e1 77 42 f9 f0 77 42 f9 
  00000bf0  f0 03 00 f9 00 00 00 94  f0 03 00 91 10 62 2b 91 
  00000c00  f0 7f 02 f9 f1 7f 42 f9  f0 33 42 f9 30 02 00 f9 
  00000c10  f0 7f 42 f9 11 02 40 f9  f1 87 02 f9 e0 87 42 f9 
  00000c20  a9 fe ff 97 e0 8b 02 f9  01 00 00 14 00 00 00 90 
  00000c30  00 00 00 91 00 40 06 91  e1 43 54 39 f0 43 54 39 
  00000c40  f0 03 00 f9 00 00 00 94  bf 03 00 91 f0 03 00 91 
  00000c50  10 82 2c 91 1d 7a 40 a9  ff c3 2c 91 00 00 80 d2 
  00000c60  c0 03 5f d6 

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
