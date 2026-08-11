fp-native dump: format=Elf arch=X86_64 entry=0xaba

AsmIR:
asmir target=X86_64 format=Elf endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
fn examples__06_struct_methods__Point__new
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 1
    insertvalue Virtual { id: 1, bank: General, size_bits: 128 }, 0, symbol(local.1), 0
    insertvalue Virtual { id: 2, bank: General, size_bits: 128 }, Virtual { id: 1, bank: General, size_bits: 64 }, symbol(local.2), 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 2, bank: General, size_bits: 64 }
    load Virtual { id: 4, bank: General, size_bits: 128 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn examples__06_struct_methods__Point__translate
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
fn examples__06_struct_methods__Point__distance2
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
fn examples__06_struct_methods__Rectangle__new
  bb0 bb0
    alloca Virtual { id: 65, bank: General, size_bits: 64 }, 1
    insertvalue Virtual { id: 66, bank: General, size_bits: 128 }, 0, symbol(local.1), 0
    insertvalue Virtual { id: 67, bank: General, size_bits: 128 }, Virtual { id: 66, bank: General, size_bits: 64 }, symbol(local.2), 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 67, bank: General, size_bits: 64 }
    load Virtual { id: 69, bank: General, size_bits: 128 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn examples__06_struct_methods__Rectangle__area
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
fn examples__06_struct_methods__Rectangle__perimeter
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
fn examples__06_struct_methods__Rectangle__is_square
  bb0 bb0
    alloca Virtual { id: 94, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 95, bank: General, size_bits: 64 }, symbol(local.1)
    load Virtual { id: 96, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 95, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 97, bank: General, size_bits: 64 }, symbol(local.1)
    gep Virtual { id: 98, bank: General, size_bits: 64 }, Virtual { id: 97, bank: General, size_bits: 64 }, 8
    bitcast Virtual { id: 99, bank: General, size_bits: 64 }, Virtual { id: 98, bank: General, size_bits: 64 }
    load Virtual { id: 100, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 99, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 101, bank: General, size_bits: 8 }, Virtual { id: 96, bank: General, size_bits: 64 }, Virtual { id: 100, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 94, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 101, bank: General, size_bits: 64 }
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
    call symbol(examples__06_struct_methods__Point__new)(10, 20) cc=C tail=false
    alloca Virtual { id: 111, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 111, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 110, bank: General, size_bits: 64 }
    br
  bb1 bb1
    call symbol(examples__06_struct_methods__Point__new)(5, 15) cc=C tail=false
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
    call symbol(examples__06_struct_methods__Point__translate)(v135, 3, v136) cc=C tail=false
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
    call symbol(examples__06_struct_methods__Point__distance2)(v149, v150) cc=C tail=false
    br
  bb4 bb4
    intrinsic.call symbol(intrinsic.println), Virtual { id: 151, bank: General, size_bits: 64 }
    call symbol(examples__06_struct_methods__Rectangle__new)(10, 5) cc=C tail=false
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
    call symbol(examples__06_struct_methods__Rectangle__area)(v165) cc=C tail=false
    br
  bb6 bb6
    intrinsic.call symbol(intrinsic.println), Virtual { id: 166, bank: General, size_bits: 64 }
    alloca Virtual { id: 168, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 168, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 154, bank: General, size_bits: 64 }
    load Virtual { id: 170, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 168, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(examples__06_struct_methods__Rectangle__perimeter)(v170) cc=C tail=false
    br
  bb7 bb7
    intrinsic.call symbol(intrinsic.println), Virtual { id: 171, bank: General, size_bits: 64 }
    alloca Virtual { id: 173, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 173, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 154, bank: General, size_bits: 64 }
    load Virtual { id: 175, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 173, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(examples__06_struct_methods__Rectangle__is_square)(v175) cc=C tail=false
    br
  bb8 bb8
    intrinsic.call symbol(intrinsic.println), Virtual { id: 176, bank: General, size_bits: 64 }
    ret


Symbols:
  examples__06_struct_methods__Point__new 0x00000000
  examples__06_struct_methods__Point__translate 0x0000015e
  examples__06_struct_methods__Point__distance2 0x00000315
  examples__06_struct_methods__Rectangle__new 0x00000659
  examples__06_struct_methods__Rectangle__area 0x000007b7
  examples__06_struct_methods__Rectangle__perimeter 0x00000898
  examples__06_struct_methods__Rectangle__is_square 0x000009cf
  main                             0x00000aba

Text relocations:
  offset=0x00000ac7 kind=Abs64 symbol=.rodata addend=0
  offset=0x00000ad2 kind=CallRel32 symbol=printf addend=0
  offset=0x00000ad8 kind=Abs64 symbol=.rodata addend=40
  offset=0x00000ae3 kind=CallRel32 symbol=printf addend=0
  offset=0x00000ae9 kind=Abs64 symbol=.rodata addend=88
  offset=0x00000af4 kind=CallRel32 symbol=printf addend=0
  offset=0x00000afa kind=Abs64 symbol=.rodata addend=136
  offset=0x00000b05 kind=CallRel32 symbol=printf addend=0
  offset=0x00000b0b kind=Abs64 symbol=.rodata addend=176
  offset=0x00000b16 kind=CallRel32 symbol=printf addend=0
  offset=0x00000b1c kind=Abs64 symbol=.rodata addend=184
  offset=0x00000b27 kind=CallRel32 symbol=printf addend=0
  offset=0x00000ca2 kind=Abs64 symbol=.rodata addend=216
  offset=0x00000cbb kind=CallRel32 symbol=printf addend=0
  offset=0x00000d30 kind=Abs64 symbol=.rodata addend=240
  offset=0x00000d49 kind=CallRel32 symbol=printf addend=0
  offset=0x00000e70 kind=Abs64 symbol=.rodata addend=264
  offset=0x00000e89 kind=CallRel32 symbol=printf addend=0
  offset=0x00000f26 kind=Abs64 symbol=.rodata addend=304
  offset=0x00000f38 kind=CallRel32 symbol=printf addend=0
  offset=0x00001030 kind=Abs64 symbol=.rodata addend=336
  offset=0x00001049 kind=CallRel32 symbol=printf addend=0
  offset=0x000010a4 kind=Abs64 symbol=.rodata addend=360
  offset=0x000010b6 kind=CallRel32 symbol=printf addend=0
  offset=0x00001111 kind=Abs64 symbol=.rodata addend=376
  offset=0x00001123 kind=CallRel32 symbol=printf addend=0
  offset=0x0000117e kind=Abs64 symbol=.rodata addend=400
  offset=0x00001191 kind=CallRel32 symbol=printf addend=0

.text (4516 bytes):
  00000000  55 48 89 e5 48 81 ec 98  00 00 00 48 89 bd b0 ff 
  00000010  ff ff 48 89 b5 c0 ff ff  ff 48 89 95 b8 ff ff ff 
  00000020  49 89 ea 49 81 c2 80 ff  ff ff 4c 89 95 f8 ff ff 
  00000030  ff 49 ba 00 00 00 00 00  00 00 00 4c 89 95 a0 ff 
  00000040  ff ff 4c 89 95 a8 ff ff  ff 4c 8b 95 c0 ff ff ff 
  00000050  4c 89 95 a0 ff ff ff 49  89 ea 49 81 c2 a0 ff ff 
  00000060  ff 4c 89 95 f0 ff ff ff  4c 8b 95 a0 ff ff ff 4c 
  00000070  89 95 90 ff ff ff 4c 8b  95 a8 ff ff ff 4c 89 95 
  00000080  98 ff ff ff 4c 8b 95 b8  ff ff ff 4c 89 95 98 ff 
  00000090  ff ff 49 89 ea 49 81 c2  90 ff ff ff 4c 89 95 e8 
  000000a0  ff ff ff 4c 8b 9d f8 ff  ff ff 4c 8b 95 90 ff ff 
  000000b0  ff 4d 89 db 49 81 c3 00  00 00 00 4d 89 93 00 00 
  000000c0  00 00 4c 8b 95 98 ff ff  ff 4d 89 db 49 81 c3 08 
  000000d0  00 00 00 4d 89 93 00 00  00 00 4c 8b 9d f8 ff ff 
  000000e0  ff 4d 89 db 49 81 c3 00  00 00 00 4d 8b 93 00 00 
  000000f0  00 00 4c 89 95 80 ff ff  ff 4d 89 db 49 81 c3 08 
  00000100  00 00 00 4d 8b 93 00 00  00 00 4c 89 95 88 ff ff 
  00000110  ff 49 89 ea 49 81 c2 80  ff ff ff 4c 89 95 d8 ff 
  00000120  ff ff 4c 8b 9d b0 ff ff  ff 4c 8b 95 80 ff ff ff 
  00000130  4d 89 db 49 81 c3 00 00  00 00 4d 89 93 00 00 00 
  00000140  00 4c 8b 95 88 ff ff ff  4d 89 db 49 81 c3 08 00 
  00000150  00 00 4d 89 93 00 00 00  00 48 89 ec 5d c3 55 48 
  00000160  89 e5 48 81 ec c8 00 00  00 48 89 bd 50 ff ff ff 
  00000170  48 89 b5 48 ff ff ff 48  89 95 40 ff ff ff 49 89 
  00000180  ea 49 81 c2 40 ff ff ff  4c 89 95 f8 ff ff ff 4c 
  00000190  8b 95 50 ff ff ff 4c 8b  9d f8 ff ff ff 4d 89 93 
  000001a0  00 00 00 00 4c 8b 9d f8  ff ff ff 4d 8b 93 00 00 
  000001b0  00 00 4c 89 95 e8 ff ff  ff 4c 8b 95 e8 ff ff ff 
  000001c0  4c 89 95 e0 ff ff ff 4c  8b 9d f8 ff ff ff 4d 8b 
  000001d0  93 00 00 00 00 4c 89 95  d8 ff ff ff 4c 8b 95 d8 
  000001e0  ff ff ff 4c 89 95 d0 ff  ff ff 4c 8b 9d d0 ff ff 
  000001f0  ff 4d 8b 93 00 00 00 00  4c 89 95 c8 ff ff ff 4c 
  00000200  8b 95 c8 ff ff ff 4c 8b  9d 48 ff ff ff 4d 01 da 
  00000210  4c 89 95 c0 ff ff ff 4c  8b 95 c0 ff ff ff 4c 8b 
  00000220  9d e0 ff ff ff 4d 89 93  00 00 00 00 4c 8b 9d f8 
  00000230  ff ff ff 4d 8b 93 00 00  00 00 4c 89 95 b0 ff ff 
  00000240  ff 4c 8b 95 b0 ff ff ff  4c 89 95 a8 ff ff ff 4c 
  00000250  8b 9d a8 ff ff ff 49 ba  08 00 00 00 00 00 00 00 
  00000260  4d 01 d3 4c 89 9d a0 ff  ff ff 4c 8b 95 a0 ff ff 
  00000270  ff 4c 89 95 98 ff ff ff  4c 8b 9d f8 ff ff ff 4d 
  00000280  8b 93 00 00 00 00 4c 89  95 90 ff ff ff 4c 8b 95 
  00000290  90 ff ff ff 4c 89 95 88  ff ff ff 4c 8b 9d 88 ff 
  000002a0  ff ff 49 ba 08 00 00 00  00 00 00 00 4d 01 d3 4c 
  000002b0  89 9d 80 ff ff ff 4c 8b  95 80 ff ff ff 4c 89 95 
  000002c0  78 ff ff ff 4c 8b 9d 78  ff ff ff 4d 8b 93 00 00 
  000002d0  00 00 4c 89 95 70 ff ff  ff 4c 8b 95 70 ff ff ff 
  000002e0  4c 8b 9d 40 ff ff ff 4d  01 da 4c 89 95 68 ff ff 
  000002f0  ff 4c 8b 95 68 ff ff ff  4c 8b 9d 98 ff ff ff 4d 
  00000300  89 93 00 00 00 00 48 89  ec 5d 48 b8 00 00 00 00 
  00000310  00 00 00 00 c3 55 48 89  e5 48 81 ec c8 01 00 00 
  00000320  48 89 bd b0 fe ff ff 48  89 b5 a8 fe ff ff 49 89 
  00000330  ea 49 81 c2 78 fe ff ff  4c 89 95 f8 ff ff ff 49 
  00000340  89 ea 49 81 c2 70 fe ff  ff 4c 89 95 f0 ff ff ff 
  00000350  4c 8b 95 b0 fe ff ff 4c  89 95 e8 ff ff ff 4c 8b 
  00000360  9d e8 ff ff ff 4d 8b 93  00 00 00 00 4c 89 95 e0 
  00000370  ff ff ff 4c 8b 95 a8 fe  ff ff 4c 89 95 d8 ff ff 
  00000380  ff 4c 8b 9d d8 ff ff ff  4d 8b 93 00 00 00 00 4c 
  00000390  89 95 d0 ff ff ff 4c 8b  95 e0 ff ff ff 4c 8b 9d 
  000003a0  d0 ff ff ff 4d 29 da 4c  89 95 c8 ff ff ff 4c 8b 
  000003b0  95 c8 ff ff ff 4c 8b 9d  f0 ff ff ff 4d 89 93 00 
  000003c0  00 00 00 49 89 ea 49 81  c2 68 fe ff ff 4c 89 95 
  000003d0  b8 ff ff ff 4c 8b 9d f0  ff ff ff 4d 8b 93 00 00 
  000003e0  00 00 4c 89 95 b0 ff ff  ff 4c 8b 95 b0 ff ff ff 
  000003f0  4c 8b 9d b8 ff ff ff 4d  89 93 00 00 00 00 49 89 
  00000400  ea 49 81 c2 60 fe ff ff  4c 89 95 a0 ff ff ff 4c 
  00000410  8b 95 b0 fe ff ff 4c 89  95 98 ff ff ff 4c 8b 9d 
  00000420  98 ff ff ff 49 ba 08 00  00 00 00 00 00 00 4d 01 
  00000430  d3 4c 89 9d 90 ff ff ff  4c 8b 95 90 ff ff ff 4c 
  00000440  89 95 88 ff ff ff 4c 8b  9d 88 ff ff ff 4d 8b 93 
  00000450  00 00 00 00 4c 89 95 80  ff ff ff 4c 8b 95 a8 fe 
  00000460  ff ff 4c 89 95 78 ff ff  ff 4c 8b 9d 78 ff ff ff 
  00000470  49 ba 08 00 00 00 00 00  00 00 4d 01 d3 4c 89 9d 
  00000480  70 ff ff ff 4c 8b 95 70  ff ff ff 4c 89 95 68 ff 
  00000490  ff ff 4c 8b 9d 68 ff ff  ff 4d 8b 93 00 00 00 00 
  000004a0  4c 89 95 60 ff ff ff 4c  8b 95 80 ff ff ff 4c 8b 
  000004b0  9d 60 ff ff ff 4d 29 da  4c 89 95 58 ff ff ff 4c 
  000004c0  8b 95 58 ff ff ff 4c 8b  9d a0 ff ff ff 4d 89 93 
  000004d0  00 00 00 00 49 89 ea 49  81 c2 58 fe ff ff 4c 89 
  000004e0  95 48 ff ff ff 4c 8b 9d  a0 ff ff ff 4d 8b 93 00 
  000004f0  00 00 00 4c 89 95 40 ff  ff ff 4c 8b 95 40 ff ff 
  00000500  ff 4c 8b 9d 48 ff ff ff  4d 89 93 00 00 00 00 49 
  00000510  89 ea 49 81 c2 50 fe ff  ff 4c 89 95 30 ff ff ff 
  00000520  4c 8b 9d b8 ff ff ff 4d  8b 93 00 00 00 00 4c 89 
  00000530  95 28 ff ff ff 4c 8b 9d  b8 ff ff ff 4d 8b 93 00 
  00000540  00 00 00 4c 89 95 20 ff  ff ff 4c 8b 95 28 ff ff 
  00000550  ff 4c 8b 9d 20 ff ff ff  4d 0f af d3 4c 89 95 18 
  00000560  ff ff ff 4c 8b 95 18 ff  ff ff 4c 8b 9d 30 ff ff 
  00000570  ff 4d 89 93 00 00 00 00  49 89 ea 49 81 c2 48 fe 
  00000580  ff ff 4c 89 95 08 ff ff  ff 4c 8b 9d 48 ff ff ff 
  00000590  4d 8b 93 00 00 00 00 4c  89 95 00 ff ff ff 4c 8b 
  000005a0  9d 48 ff ff ff 4d 8b 93  00 00 00 00 4c 89 95 f8 
  000005b0  fe ff ff 4c 8b 95 00 ff  ff ff 4c 8b 9d f8 fe ff 
  000005c0  ff 4d 0f af d3 4c 89 95  f0 fe ff ff 4c 8b 95 f0 
  000005d0  fe ff ff 4c 8b 9d 08 ff  ff ff 4d 89 93 00 00 00 
  000005e0  00 4c 8b 9d 30 ff ff ff  4d 8b 93 00 00 00 00 4c 
  000005f0  89 95 e0 fe ff ff 4c 8b  9d 08 ff ff ff 4d 8b 93 
  00000600  00 00 00 00 4c 89 95 d8  fe ff ff 4c 8b 95 e0 fe 
  00000610  ff ff 4c 8b 9d d8 fe ff  ff 4d 01 da 4c 89 95 d0 
  00000620  fe ff ff 4c 8b 95 d0 fe  ff ff 4c 8b 9d f8 ff ff 
  00000630  ff 4d 89 93 00 00 00 00  4c 8b 9d f8 ff ff ff 4d 
  00000640  8b 93 00 00 00 00 4c 89  95 c0 fe ff ff 48 8b 85 
  00000650  c0 fe ff ff 48 89 ec 5d  c3 55 48 89 e5 48 81 ec 
  00000660  98 00 00 00 48 89 bd b0  ff ff ff 48 89 b5 c0 ff 
  00000670  ff ff 48 89 95 b8 ff ff  ff 49 89 ea 49 81 c2 80 
  00000680  ff ff ff 4c 89 95 f8 ff  ff ff 49 ba 00 00 00 00 
  00000690  00 00 00 00 4c 89 95 a0  ff ff ff 4c 89 95 a8 ff 
  000006a0  ff ff 4c 8b 95 c0 ff ff  ff 4c 89 95 a0 ff ff ff 
  000006b0  49 89 ea 49 81 c2 a0 ff  ff ff 4c 89 95 f0 ff ff 
  000006c0  ff 4c 8b 95 a0 ff ff ff  4c 89 95 90 ff ff ff 4c 
  000006d0  8b 95 a8 ff ff ff 4c 89  95 98 ff ff ff 4c 8b 95 
  000006e0  b8 ff ff ff 4c 89 95 98  ff ff ff 49 89 ea 49 81 
  000006f0  c2 90 ff ff ff 4c 89 95  e8 ff ff ff 4c 8b 9d f8 
  00000700  ff ff ff 4c 8b 95 90 ff  ff ff 4d 89 db 49 81 c3 
  00000710  00 00 00 00 4d 89 93 00  00 00 00 4c 8b 95 98 ff 
  00000720  ff ff 4d 89 db 49 81 c3  08 00 00 00 4d 89 93 00 
  00000730  00 00 00 4c 8b 9d f8 ff  ff ff 4d 89 db 49 81 c3 
  00000740  00 00 00 00 4d 8b 93 00  00 00 00 4c 89 95 80 ff 
  00000750  ff ff 4d 89 db 49 81 c3  08 00 00 00 4d 8b 93 00 
  00000760  00 00 00 4c 89 95 88 ff  ff ff 49 89 ea 49 81 c2 
  00000770  80 ff ff ff 4c 89 95 d8  ff ff ff 4c 8b 9d b0 ff 
  00000780  ff ff 4c 8b 95 80 ff ff  ff 4d 89 db 49 81 c3 00 
  00000790  00 00 00 4d 89 93 00 00  00 00 4c 8b 95 88 ff ff 
  000007a0  ff 4d 89 db 49 81 c3 08  00 00 00 4d 89 93 00 00 
  000007b0  00 00 48 89 ec 5d c3 55  48 89 e5 48 81 ec 68 00 
  000007c0  00 00 48 89 bd a0 ff ff  ff 49 89 ea 49 81 c2 a0 
  000007d0  ff ff ff 4c 89 95 f8 ff  ff ff 4c 8b 95 a0 ff ff 
  000007e0  ff 4c 89 95 f0 ff ff ff  4c 8b 9d f0 ff ff ff 4d 
  000007f0  8b 93 00 00 00 00 4c 89  95 e8 ff ff ff 4c 8b 95 
  00000800  a0 ff ff ff 4c 89 95 e0  ff ff ff 4c 8b 9d e0 ff 
  00000810  ff ff 49 ba 08 00 00 00  00 00 00 00 4d 01 d3 4c 
  00000820  89 9d d8 ff ff ff 4c 8b  95 d8 ff ff ff 4c 89 95 
  00000830  d0 ff ff ff 4c 8b 9d d0  ff ff ff 4d 8b 93 00 00 
  00000840  00 00 4c 89 95 c8 ff ff  ff 4c 8b 95 e8 ff ff ff 
  00000850  4c 8b 9d c8 ff ff ff 4d  0f af d3 4c 89 95 c0 ff 
  00000860  ff ff 4c 8b 95 c0 ff ff  ff 4c 8b 9d f8 ff ff ff 
  00000870  4d 89 93 00 00 00 00 4c  8b 9d f8 ff ff ff 4d 8b 
  00000880  93 00 00 00 00 4c 89 95  b0 ff ff ff 48 8b 85 b0 
  00000890  ff ff ff 48 89 ec 5d c3  55 48 89 e5 48 81 ec 98 
  000008a0  00 00 00 48 89 bd 80 ff  ff ff 49 89 ea 49 81 c2 
  000008b0  78 ff ff ff 4c 89 95 f8  ff ff ff 49 89 ea 49 81 
  000008c0  c2 70 ff ff ff 4c 89 95  f0 ff ff ff 4c 8b 95 80 
  000008d0  ff ff ff 4c 89 95 e8 ff  ff ff 4c 8b 9d e8 ff ff 
  000008e0  ff 4d 8b 93 00 00 00 00  4c 89 95 e0 ff ff ff 4c 
  000008f0  8b 95 80 ff ff ff 4c 89  95 d8 ff ff ff 4c 8b 9d 
  00000900  d8 ff ff ff 49 ba 08 00  00 00 00 00 00 00 4d 01 
  00000910  d3 4c 89 9d d0 ff ff ff  4c 8b 95 d0 ff ff ff 4c 
  00000920  89 95 c8 ff ff ff 4c 8b  9d c8 ff ff ff 4d 8b 93 
  00000930  00 00 00 00 4c 89 95 c0  ff ff ff 4c 8b 95 e0 ff 
  00000940  ff ff 4c 8b 9d c0 ff ff  ff 4d 01 da 4c 89 95 b8 
  00000950  ff ff ff 4c 8b 95 b8 ff  ff ff 4c 8b 9d f0 ff ff 
  00000960  ff 4d 89 93 00 00 00 00  4c 8b 9d f0 ff ff ff 4d 
  00000970  8b 93 00 00 00 00 4c 89  95 a8 ff ff ff 49 ba 02 
  00000980  00 00 00 00 00 00 00 4c  8b 9d a8 ff ff ff 4d 0f 
  00000990  af d3 4c 89 95 a0 ff ff  ff 4c 8b 95 a0 ff ff ff 
  000009a0  4c 8b 9d f8 ff ff ff 4d  89 93 00 00 00 00 4c 8b 
  000009b0  9d f8 ff ff ff 4d 8b 93  00 00 00 00 4c 89 95 90 
  000009c0  ff ff ff 48 8b 85 90 ff  ff ff 48 89 ec 5d c3 55 
  000009d0  48 89 e5 48 81 ec 68 00  00 00 48 89 bd a0 ff ff 
  000009e0  ff 49 89 ea 49 81 c2 a0  ff ff ff 4c 89 95 f8 ff 
  000009f0  ff ff 4c 8b 95 a0 ff ff  ff 4c 89 95 f0 ff ff ff 
  00000a00  4c 8b 9d f0 ff ff ff 4d  8b 93 00 00 00 00 4c 89 
  00000a10  95 e8 ff ff ff 4c 8b 95  a0 ff ff ff 4c 89 95 e0 
  00000a20  ff ff ff 4c 8b 9d e0 ff  ff ff 49 ba 08 00 00 00 
  00000a30  00 00 00 00 4d 01 d3 4c  89 9d d8 ff ff ff 4c 8b 
  00000a40  95 d8 ff ff ff 4c 89 95  d0 ff ff ff 4c 8b 9d d0 
  00000a50  ff ff ff 4d 8b 93 00 00  00 00 4c 89 95 c8 ff ff 
  00000a60  ff 4c 8b 95 e8 ff ff ff  4c 8b 9d c8 ff ff ff 4d 
  00000a70  39 da 41 0f 94 c3 4d 0f  b6 d3 4c 89 95 c0 ff ff 
  00000a80  ff 4c 0f b6 95 c0 ff ff  ff 4c 8b 9d f8 ff ff ff 
  00000a90  45 88 93 00 00 00 00 4c  8b 9d f8 ff ff ff 4d 0f 
  00000aa0  b6 93 00 00 00 00 4c 89  95 b0 ff ff ff 48 0f b6 
  00000ab0  85 b0 ff ff ff 48 89 ec  5d c3 55 48 89 e5 48 81 
  00000ac0  ec f8 03 00 00 48 bf 00  00 00 00 00 00 00 00 b0 
  00000ad0  00 e8 00 00 00 00 48 bf  00 00 00 00 00 00 00 00 
  00000ae0  b0 00 e8 00 00 00 00 48  bf 00 00 00 00 00 00 00 
  00000af0  00 b0 00 e8 00 00 00 00  48 bf 00 00 00 00 00 00 
  00000b00  00 00 b0 00 e8 00 00 00  00 48 bf 00 00 00 00 00 
  00000b10  00 00 00 b0 00 e8 00 00  00 00 48 bf 00 00 00 00 
  00000b20  00 00 00 00 b0 00 e8 00  00 00 00 48 89 ef 48 81 
  00000b30  c7 98 fc ff ff 48 be 0a  00 00 00 00 00 00 00 48 
  00000b40  ba 14 00 00 00 00 00 00  00 b0 00 e8 b0 f4 ff ff 
  00000b50  49 89 ea 49 81 c2 98 fc  ff ff 4c 89 95 c8 ff ff 
  00000b60  ff 49 89 ea 49 81 c2 78  fc ff ff 4c 89 95 c0 ff 
  00000b70  ff ff 4c 8b 9d c0 ff ff  ff 4c 8b 95 98 fc ff ff 
  00000b80  4d 89 db 49 81 c3 00 00  00 00 4d 89 93 00 00 00 
  00000b90  00 4c 8b 95 a0 fc ff ff  4d 89 db 49 81 c3 08 00 
  00000ba0  00 00 4d 89 93 00 00 00  00 e9 00 00 00 00 48 89 
  00000bb0  ef 48 81 c7 88 fc ff ff  48 be 05 00 00 00 00 00 
  00000bc0  00 00 48 ba 0f 00 00 00  00 00 00 00 b0 00 e8 2d 
  00000bd0  f4 ff ff 49 89 ea 49 81  c2 88 fc ff ff 4c 89 95 
  00000be0  b0 ff ff ff 49 89 ea 49  81 c2 68 fc ff ff 4c 89 
  00000bf0  95 a8 ff ff ff 4c 8b 9d  a8 ff ff ff 4c 8b 95 88 
  00000c00  fc ff ff 4d 89 db 49 81  c3 00 00 00 00 4d 89 93 
  00000c10  00 00 00 00 4c 8b 95 90  fc ff ff 4d 89 db 49 81 
  00000c20  c3 08 00 00 00 4d 89 93  00 00 00 00 e9 00 00 00 
  00000c30  00 4c 8b 95 c0 ff ff ff  4c 89 95 98 ff ff ff 4c 
  00000c40  8b 9d 98 ff ff ff 4d 8b  93 00 00 00 00 4c 89 95 
  00000c50  90 ff ff ff 4c 8b 95 c0  ff ff ff 4c 89 95 88 ff 
  00000c60  ff ff 4c 8b 9d 88 ff ff  ff 49 ba 08 00 00 00 00 
  00000c70  00 00 00 4d 01 d3 4c 89  9d 80 ff ff ff 4c 8b 95 
  00000c80  80 ff ff ff 4c 89 95 78  ff ff ff 4c 8b 9d 78 ff 
  00000c90  ff ff 4d 8b 93 00 00 00  00 4c 89 95 70 ff ff ff 
  00000ca0  48 bf 00 00 00 00 00 00  00 00 48 8b b5 90 ff ff 
  00000cb0  ff 48 8b 95 70 ff ff ff  b0 00 e8 00 00 00 00 4c 
  00000cc0  8b 95 a8 ff ff ff 4c 89  95 60 ff ff ff 4c 8b 9d 
  00000cd0  60 ff ff ff 4d 8b 93 00  00 00 00 4c 89 95 58 ff 
  00000ce0  ff ff 4c 8b 95 a8 ff ff  ff 4c 89 95 50 ff ff ff 
  00000cf0  4c 8b 9d 50 ff ff ff 49  ba 08 00 00 00 00 00 00 
  00000d00  00 4d 01 d3 4c 89 9d 48  ff ff ff 4c 8b 95 48 ff 
  00000d10  ff ff 4c 89 95 40 ff ff  ff 4c 8b 9d 40 ff ff ff 
  00000d20  4d 8b 93 00 00 00 00 4c  89 95 38 ff ff ff 48 bf 
  00000d30  00 00 00 00 00 00 00 00  48 8b b5 58 ff ff ff 48 
  00000d40  8b 95 38 ff ff ff b0 00  e8 00 00 00 00 49 89 ea 
  00000d50  49 81 c2 58 fc ff ff 4c  89 95 28 ff ff ff 4c 8b 
  00000d60  95 c0 ff ff ff 4c 8b 9d  28 ff ff ff 4d 89 93 00 
  00000d70  00 00 00 49 89 ea 49 81  c2 50 fc ff ff 4c 89 95 
  00000d80  18 ff ff ff 49 ba 00 00  00 00 00 00 00 00 49 81 
  00000d90  ea 04 00 00 00 4c 89 95  10 ff ff ff 4c 8b 95 10 
  00000da0  ff ff ff 4c 8b 9d 18 ff  ff ff 4d 89 93 00 00 00 
  00000db0  00 4c 8b 9d 28 ff ff ff  4d 8b 93 00 00 00 00 4c 
  00000dc0  89 95 00 ff ff ff 4c 8b  9d 18 ff ff ff 4d 8b 93 
  00000dd0  00 00 00 00 4c 89 95 f8  fe ff ff 48 8b bd 00 ff 
  00000de0  ff ff 48 be 03 00 00 00  00 00 00 00 48 8b 95 f8 
  00000df0  fe ff ff b0 00 e8 64 f3  ff ff e9 00 00 00 00 4c 
  00000e00  8b 95 c0 ff ff ff 4c 89  95 e8 fe ff ff 4c 8b 9d 
  00000e10  e8 fe ff ff 4d 8b 93 00  00 00 00 4c 89 95 e0 fe 
  00000e20  ff ff 4c 8b 95 c0 ff ff  ff 4c 89 95 d8 fe ff ff 
  00000e30  4c 8b 9d d8 fe ff ff 49  ba 08 00 00 00 00 00 00 
  00000e40  00 4d 01 d3 4c 89 9d d0  fe ff ff 4c 8b 95 d0 fe 
  00000e50  ff ff 4c 89 95 c8 fe ff  ff 4c 8b 9d c8 fe ff ff 
  00000e60  4d 8b 93 00 00 00 00 4c  89 95 c0 fe ff ff 48 bf 
  00000e70  00 00 00 00 00 00 00 00  48 8b b5 e0 fe ff ff 48 
  00000e80  8b 95 c0 fe ff ff b0 00  e8 00 00 00 00 49 89 ea 
  00000e90  49 81 c2 48 fc ff ff 4c  89 95 b0 fe ff ff 4c 8b 
  00000ea0  95 c0 ff ff ff 4c 8b 9d  b0 fe ff ff 4d 89 93 00 
  00000eb0  00 00 00 49 89 ea 49 81  c2 40 fc ff ff 4c 89 95 
  00000ec0  a0 fe ff ff 4c 8b 95 a8  ff ff ff 4c 8b 9d a0 fe 
  00000ed0  ff ff 4d 89 93 00 00 00  00 4c 8b 9d b0 fe ff ff 
  00000ee0  4d 8b 93 00 00 00 00 4c  89 95 90 fe ff ff 4c 8b 
  00000ef0  9d a0 fe ff ff 4d 8b 93  00 00 00 00 4c 89 95 88 
  00000f00  fe ff ff 48 8b bd 90 fe  ff ff 48 8b b5 88 fe ff 
  00000f10  ff b0 00 e8 fd f3 ff ff  48 89 85 80 fe ff ff e9 
  00000f20  00 00 00 00 48 bf 00 00  00 00 00 00 00 00 48 8b 
  00000f30  b5 80 fe ff ff b0 00 e8  00 00 00 00 48 89 ef 48 
  00000f40  81 c7 78 fc ff ff 48 be  0a 00 00 00 00 00 00 00 
  00000f50  48 ba 05 00 00 00 00 00  00 00 b0 00 e8 f8 f6 ff 
  00000f60  ff 49 89 ea 49 81 c2 78  fc ff ff 4c 89 95 70 fe 
  00000f70  ff ff 49 89 ea 49 81 c2  38 fc ff ff 4c 89 95 68 
  00000f80  fe ff ff 4c 8b 9d 68 fe  ff ff 4c 8b 95 78 fc ff 
  00000f90  ff 4d 89 db 49 81 c3 00  00 00 00 4d 89 93 00 00 
  00000fa0  00 00 4c 8b 95 80 fc ff  ff 4d 89 db 49 81 c3 08 
  00000fb0  00 00 00 4d 89 93 00 00  00 00 e9 00 00 00 00 4c 
  00000fc0  8b 95 68 fe ff ff 4c 89  95 58 fe ff ff 4c 8b 9d 
  00000fd0  58 fe ff ff 4d 8b 93 00  00 00 00 4c 89 95 50 fe 
  00000fe0  ff ff 4c 8b 95 68 fe ff  ff 4c 89 95 48 fe ff ff 
  00000ff0  4c 8b 9d 48 fe ff ff 49  ba 08 00 00 00 00 00 00 
  00001000  00 4d 01 d3 4c 89 9d 40  fe ff ff 4c 8b 95 40 fe 
  00001010  ff ff 4c 89 95 38 fe ff  ff 4c 8b 9d 38 fe ff ff 
  00001020  4d 8b 93 00 00 00 00 4c  89 95 30 fe ff ff 48 bf 
  00001030  00 00 00 00 00 00 00 00  48 8b b5 50 fe ff ff 48 
  00001040  8b 95 30 fe ff ff b0 00  e8 00 00 00 00 49 89 ea 
  00001050  49 81 c2 28 fc ff ff 4c  89 95 20 fe ff ff 4c 8b 
  00001060  95 68 fe ff ff 4c 8b 9d  20 fe ff ff 4d 89 93 00 
  00001070  00 00 00 4c 8b 9d 20 fe  ff ff 4d 8b 93 00 00 00 
  00001080  00 4c 89 95 10 fe ff ff  48 8b bd 10 fe ff ff b0 
  00001090  00 e8 21 f7 ff ff 48 89  85 08 fe ff ff e9 00 00 
  000010a0  00 00 48 bf 00 00 00 00  00 00 00 00 48 8b b5 08 
  000010b0  fe ff ff b0 00 e8 00 00  00 00 49 89 ea 49 81 c2 
  000010c0  20 fc ff ff 4c 89 95 f8  fd ff ff 4c 8b 95 68 fe 
  000010d0  ff ff 4c 8b 9d f8 fd ff  ff 4d 89 93 00 00 00 00 
  000010e0  4c 8b 9d f8 fd ff ff 4d  8b 93 00 00 00 00 4c 89 
  000010f0  95 e8 fd ff ff 48 8b bd  e8 fd ff ff b0 00 e8 95 
  00001100  f7 ff ff 48 89 85 e0 fd  ff ff e9 00 00 00 00 48 
  00001110  bf 00 00 00 00 00 00 00  00 48 8b b5 e0 fd ff ff 
  00001120  b0 00 e8 00 00 00 00 49  89 ea 49 81 c2 18 fc ff 
  00001130  ff 4c 89 95 d0 fd ff ff  4c 8b 95 68 fe ff ff 4c 
  00001140  8b 9d d0 fd ff ff 4d 89  93 00 00 00 00 4c 8b 9d 
  00001150  d0 fd ff ff 4d 8b 93 00  00 00 00 4c 89 95 c0 fd 
  00001160  ff ff 48 8b bd c0 fd ff  ff b0 00 e8 5f f8 ff ff 
  00001170  48 89 85 b8 fd ff ff e9  00 00 00 00 48 bf 00 00 
  00001180  00 00 00 00 00 00 48 0f  b6 b5 b8 fd ff ff b0 00 
  00001190  e8 00 00 00 00 48 89 ec  5d 48 b8 00 00 00 00 00 
  000011a0  00 00 00 c3 

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
