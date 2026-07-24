fp-native dump: format=MachO arch=Aarch64 entry=0x314

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
fn socket
fn setsockopt
fn bind
fn listen
fn accept
fn write
fn strlen
fn close
fn make_addr
  bb0 bb0
    alloca Virtual { id: 6, bank: General, size_bits: 64 }, 1
    sextortrunc Virtual { id: 7, bank: General, size_bits: 8 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 6, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 7, bank: General, size_bits: 64 }
    alloca Virtual { id: 9, bank: General, size_bits: 64 }, 1
    sextortrunc Virtual { id: 10, bank: General, size_bits: 8 }, 2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 9, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 10, bank: General, size_bits: 64 }
    load Virtual { id: 12, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 6, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 13, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 9, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    insertvalue Virtual { id: 14, bank: General, size_bits: 128 }, 0, Virtual { id: 12, bank: General, size_bits: 64 }, 0
    insertvalue Virtual { id: 15, bank: General, size_bits: 128 }, Virtual { id: 14, bank: General, size_bits: 64 }, Virtual { id: 13, bank: General, size_bits: 64 }, 1
    insertvalue Virtual { id: 16, bank: General, size_bits: 128 }, Virtual { id: 15, bank: General, size_bits: 64 }, symbol(local.1), 2
    insertvalue Virtual { id: 17, bank: General, size_bits: 128 }, Virtual { id: 16, bank: General, size_bits: 64 }, symbol(local.2), 3
    insertvalue Virtual { id: 18, bank: General, size_bits: 128 }, Virtual { id: 17, bank: General, size_bits: 64 }, 0, 4
    insertvalue Virtual { id: 19, bank: General, size_bits: 128 }, Virtual { id: 18, bank: General, size_bits: 64 }, 0, 5
    insertvalue Virtual { id: 20, bank: General, size_bits: 128 }, Virtual { id: 19, bank: General, size_bits: 64 }, 0, 6
    insertvalue Virtual { id: 21, bank: General, size_bits: 128 }, Virtual { id: 20, bank: General, size_bits: 64 }, 0, 7
    insertvalue Virtual { id: 22, bank: General, size_bits: 128 }, Virtual { id: 21, bank: General, size_bits: 64 }, 0, 8
    insertvalue Virtual { id: 23, bank: General, size_bits: 128 }, Virtual { id: 22, bank: General, size_bits: 64 }, 0, 9
    insertvalue Virtual { id: 24, bank: General, size_bits: 128 }, Virtual { id: 23, bank: General, size_bits: 64 }, 0, 10
    insertvalue Virtual { id: 25, bank: General, size_bits: 128 }, Virtual { id: 24, bank: General, size_bits: 64 }, 0, 11
    insertvalue Virtual { id: 26, bank: General, size_bits: 128 }, Virtual { id: 25, bank: General, size_bits: 64 }, 0, 12
    insertvalue Virtual { id: 27, bank: General, size_bits: 128 }, Virtual { id: 26, bank: General, size_bits: 64 }, 0, 13
    insertvalue Virtual { id: 28, bank: General, size_bits: 128 }, Virtual { id: 27, bank: General, size_bits: 64 }, 0, 14
    insertvalue Virtual { id: 29, bank: General, size_bits: 128 }, Virtual { id: 28, bank: General, size_bits: 64 }, 0, 15
    ret
fn main
  bb0 bb0
    call symbol(socket)(2, 1, 0) cc=C tail=false
    br
  bb1 bb1
    alloca Virtual { id: 31, bank: General, size_bits: 64 }, 1
    lt Virtual { id: 32, bank: General, size_bits: 8 }, Virtual { id: 30, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 31, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 32, bank: General, size_bits: 64 }
    load Virtual { id: 34, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 31, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 35, bank: General, size_bits: 8 }, Virtual { id: 34, bank: General, size_bits: 64 }, 1
    condbr
  bb2 bb2
    ret
  bb3 bb3
    br
  bb4 bb4
    alloca Virtual { id: 36, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 36, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    alloca Virtual { id: 38, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 38, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 36, bank: General, size_bits: 64 }
    alloca Virtual { id: 40, bank: General, size_bits: 64 }, 1
    load Virtual { id: 41, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 38, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 42, bank: General, size_bits: 64 }, Virtual { id: 41, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 40, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 42, bank: General, size_bits: 64 }
    alloca Virtual { id: 44, bank: General, size_bits: 64 }, 1
    load Virtual { id: 45, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 40, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 44, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 45, bank: General, size_bits: 64 }
    alloca Virtual { id: 47, bank: General, size_bits: 64 }, 1
    load Virtual { id: 48, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 44, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 49, bank: General, size_bits: 64 }, Virtual { id: 48, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 47, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 49, bank: General, size_bits: 64 }
    load Virtual { id: 51, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 47, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(setsockopt)(v30, 1, 2, v51, 4) cc=C tail=false
    br
  bb6 bb6
    call symbol(make_addr)(31, 145) cc=C tail=false
    alloca Virtual { id: 54, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 54, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 53, bank: General, size_bits: 64 }
    br
  bb7 bb7
    alloca Virtual { id: 56, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 56, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 54, bank: General, size_bits: 64 }
    alloca Virtual { id: 58, bank: General, size_bits: 64 }, 1
    load Virtual { id: 59, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 56, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 60, bank: General, size_bits: 64 }, Virtual { id: 59, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 58, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 60, bank: General, size_bits: 64 }
    alloca Virtual { id: 62, bank: General, size_bits: 64 }, 1
    load Virtual { id: 63, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 58, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 62, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 63, bank: General, size_bits: 64 }
    alloca Virtual { id: 65, bank: General, size_bits: 64 }, 1
    load Virtual { id: 66, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 62, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 67, bank: General, size_bits: 64 }, Virtual { id: 66, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 67, bank: General, size_bits: 64 }
    load Virtual { id: 69, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 65, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(bind)(v30, v69, 16) cc=C tail=false
    br
  bb8 bb8
    alloca Virtual { id: 71, bank: General, size_bits: 64 }, 1
    ne Virtual { id: 72, bank: General, size_bits: 8 }, Virtual { id: 70, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 71, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 72, bank: General, size_bits: 64 }
    load Virtual { id: 74, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 71, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 75, bank: General, size_bits: 8 }, Virtual { id: 74, bank: General, size_bits: 64 }, 1
    condbr
  bb9 bb9
    call symbol(close)(v30) cc=C tail=false
    br
  bb10 bb10
    br
  bb12 bb12
    ret
  bb11 bb11
    call symbol(listen)(v30, 128) cc=C tail=false
    br
  bb14 bb14
    alloca Virtual { id: 78, bank: General, size_bits: 64 }, 1
    ne Virtual { id: 79, bank: General, size_bits: 8 }, Virtual { id: 77, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 78, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 79, bank: General, size_bits: 64 }
    load Virtual { id: 81, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 78, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 82, bank: General, size_bits: 8 }, Virtual { id: 81, bank: General, size_bits: 64 }, 1
    condbr
  bb15 bb15
    call symbol(close)(v30) cc=C tail=false
    br
  bb16 bb16
    br
  bb18 bb18
    ret
  bb17 bb17
    intrinsic.call symbol(intrinsic.println)
    br
  bb20 bb20
    br
  bb21 bb21
    call symbol(make_addr)(0, 0) cc=C tail=false
    alloca Virtual { id: 86, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 86, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 85, bank: General, size_bits: 64 }
    br
  bb23 bb23
    alloca Virtual { id: 88, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 88, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 16
    alloca Virtual { id: 90, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 90, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 86, bank: General, size_bits: 64 }
    alloca Virtual { id: 92, bank: General, size_bits: 64 }, 1
    load Virtual { id: 93, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 90, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 94, bank: General, size_bits: 64 }, Virtual { id: 93, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 94, bank: General, size_bits: 64 }
    alloca Virtual { id: 96, bank: General, size_bits: 64 }, 1
    load Virtual { id: 97, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 96, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 97, bank: General, size_bits: 64 }
    alloca Virtual { id: 99, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 99, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 88, bank: General, size_bits: 64 }
    alloca Virtual { id: 101, bank: General, size_bits: 64 }, 1
    load Virtual { id: 102, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 99, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 103, bank: General, size_bits: 64 }, Virtual { id: 102, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 101, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 103, bank: General, size_bits: 64 }
    alloca Virtual { id: 105, bank: General, size_bits: 64 }, 1
    load Virtual { id: 106, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 101, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 105, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 106, bank: General, size_bits: 64 }
    alloca Virtual { id: 108, bank: General, size_bits: 64 }, 1
    load Virtual { id: 109, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 96, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 110, bank: General, size_bits: 64 }, Virtual { id: 109, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 108, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 110, bank: General, size_bits: 64 }
    load Virtual { id: 112, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 108, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 113, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 105, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(accept)(v30, v112, v113) cc=C tail=false
    br
  bb24 bb24
    alloca Virtual { id: 115, bank: General, size_bits: 64 }, 1
    lt Virtual { id: 116, bank: General, size_bits: 8 }, Virtual { id: 114, bank: General, size_bits: 64 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 115, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 116, bank: General, size_bits: 64 }
    load Virtual { id: 118, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 115, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 119, bank: General, size_bits: 8 }, Virtual { id: 118, bank: General, size_bits: 64 }, 1
    condbr
  bb25 bb25
    br
  bb26 bb26
    br
  bb27 bb27
    alloca Virtual { id: 120, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 120, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_0)
    alloca Virtual { id: 122, bank: General, size_bits: 64 }, 1
    load Virtual { id: 123, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 120, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 122, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 123, bank: General, size_bits: 64 }
    load Virtual { id: 125, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 122, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(strlen)(v125) cc=C tail=false
    br
  bb29 bb29
    alloca Virtual { id: 127, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 127, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_0)
    alloca Virtual { id: 129, bank: General, size_bits: 64 }, 1
    load Virtual { id: 130, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 127, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 129, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 130, bank: General, size_bits: 64 }
    load Virtual { id: 132, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 129, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(write)(v114, v132, v126) cc=C tail=false
    br
  bb30 bb30
    call symbol(close)(v114) cc=C tail=false
    br
  bb31 bb31
    br
  bb5 bb5
    ret
  bb13 bb13
    ret
  bb19 bb19
    ret
  bb22 bb22
    ret
  bb28 bb28
    ret


Symbols:
  make_addr                        0x00000000
  main                             0x00000314

Text relocations:
  offset=0x00000334 kind=CallRel32 symbol=socket addend=0
  offset=0x00000480 kind=CallRel32 symbol=setsockopt addend=0
  offset=0x00000588 kind=CallRel32 symbol=bind addend=0
  offset=0x000005ec kind=CallRel32 symbol=close addend=0
  offset=0x00000620 kind=CallRel32 symbol=listen addend=0
  offset=0x00000684 kind=CallRel32 symbol=close addend=0
  offset=0x000006b0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000006bc kind=CallRel32 symbol=printf addend=0
  offset=0x00000850 kind=CallRel32 symbol=accept addend=0
  offset=0x000008c8 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000908 kind=CallRel32 symbol=strlen addend=0
  offset=0x00000924 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x0000096c kind=CallRel32 symbol=write addend=0
  offset=0x0000097c kind=CallRel32 symbol=close addend=0

Section relocations:
  section=Data offset=0x00000000 kind=Abs64 symbol=__const_data_0 addend=0

.text (2584 bytes):
  00000000  ff 83 08 d1 f0 03 00 91  10 42 08 91 1d 7a 00 a9 
  00000010  fd 03 00 91 e0 7b 00 f9  e1 43 03 39 e2 63 03 39 
  00000020  f0 03 00 91 10 e2 07 91  f0 03 00 f9 10 02 80 d2 
  00000030  f1 1f 80 d2 11 00 a0 f2  11 00 c0 f2 11 00 e0 f2 
  00000040  10 02 11 8a f0 07 00 f9  f1 03 40 f9 f0 23 c0 39 
  00000050  30 02 00 39 f0 03 00 91  10 02 08 91 f0 0f 00 f9 
  00000060  50 00 80 d2 f1 1f 80 d2  11 00 a0 f2 11 00 c0 f2 
  00000070  11 00 e0 f2 10 02 11 8a  f0 13 00 f9 f1 0f 40 f9 
  00000080  f0 83 c0 39 30 02 00 39  f0 03 40 f9 11 02 c0 39 
  00000090  f1 1b 00 f9 f0 0f 40 f9  11 02 c0 39 f1 1f 00 f9 
  000000a0  10 00 80 d2 f0 7f 00 f9  f0 83 00 f9 f0 c3 c0 39 
  000000b0  f0 e3 03 39 f0 03 00 91  10 e2 03 91 f0 23 00 f9 
  000000c0  f0 7f 40 f9 f0 87 00 f9  f0 83 40 f9 f0 8b 00 f9 
  000000d0  f0 e3 c0 39 f0 27 04 39  f0 03 00 91 10 22 04 91 
  000000e0  f0 27 00 f9 f0 87 40 f9  f0 8f 00 f9 f0 8b 40 f9 
  000000f0  f0 93 00 f9 f0 43 c3 39  f0 6b 04 39 f0 03 00 91 
  00000100  10 62 04 91 f0 2b 00 f9  f0 8f 40 f9 f0 97 00 f9 
  00000110  f0 93 40 f9 f0 9b 00 f9  f0 63 c3 39 f0 af 04 39 
  00000120  f0 03 00 91 10 a2 04 91  f0 2f 00 f9 f0 97 40 f9 
  00000130  f0 9f 00 f9 f0 9b 40 f9  f0 a3 00 f9 10 00 80 d2 
  00000140  f0 f3 04 39 f0 03 00 91  10 e2 04 91 f0 33 00 f9 
  00000150  f0 9f 40 f9 f0 a7 00 f9  f0 a3 40 f9 f0 ab 00 f9 
  00000160  10 00 80 d2 f0 37 05 39  f0 03 00 91 10 22 05 91 
  00000170  f0 37 00 f9 f0 a7 40 f9  f0 af 00 f9 f0 ab 40 f9 
  00000180  f0 b3 00 f9 10 00 80 d2  f0 7b 05 39 f0 03 00 91 
  00000190  10 62 05 91 f0 3b 00 f9  f0 af 40 f9 f0 b7 00 f9 
  000001a0  f0 b3 40 f9 f0 bb 00 f9  10 00 80 d2 f0 bf 05 39 
  000001b0  f0 03 00 91 10 a2 05 91  f0 3f 00 f9 f0 b7 40 f9 
  000001c0  f0 bf 00 f9 f0 bb 40 f9  f0 c3 00 f9 10 00 80 d2 
  000001d0  f0 03 06 39 f0 03 00 91  10 e2 05 91 f0 43 00 f9 
  000001e0  f0 bf 40 f9 f0 c7 00 f9  f0 c3 40 f9 f0 cb 00 f9 
  000001f0  10 00 80 d2 f0 47 06 39  f0 03 00 91 10 22 06 91 
  00000200  f0 47 00 f9 f0 c7 40 f9  f0 cf 00 f9 f0 cb 40 f9 
  00000210  f0 d3 00 f9 10 00 80 d2  f0 8b 06 39 f0 03 00 91 
  00000220  10 62 06 91 f0 4b 00 f9  f0 cf 40 f9 f0 d7 00 f9 
  00000230  f0 d3 40 f9 f0 db 00 f9  10 00 80 d2 f0 cf 06 39 
  00000240  f0 03 00 91 10 a2 06 91  f0 4f 00 f9 f0 d7 40 f9 
  00000250  f0 df 00 f9 f0 db 40 f9  f0 e3 00 f9 10 00 80 d2 
  00000260  f0 13 07 39 f0 03 00 91  10 e2 06 91 f0 53 00 f9 
  00000270  f0 df 40 f9 f0 e7 00 f9  f0 e3 40 f9 f0 eb 00 f9 
  00000280  10 00 80 d2 f0 57 07 39  f0 03 00 91 10 22 07 91 
  00000290  f0 57 00 f9 f0 e7 40 f9  f0 ef 00 f9 f0 eb 40 f9 
  000002a0  f0 f3 00 f9 10 00 80 d2  f0 9b 07 39 f0 03 00 91 
  000002b0  10 62 07 91 f0 5b 00 f9  f0 ef 40 f9 f0 f7 00 f9 
  000002c0  f0 f3 40 f9 f0 fb 00 f9  10 00 80 d2 f0 df 07 39 
  000002d0  f0 03 00 91 10 a2 07 91  f0 5f 00 f9 f1 7b 40 f9 
  000002e0  f0 f7 40 f9 e9 03 11 aa  30 01 00 f9 f0 fb 40 f9 
  000002f0  e9 03 11 aa 29 21 00 91  30 01 00 f9 bf 03 00 91 
  00000300  f0 03 00 91 10 42 08 91  1d 7a 40 a9 ff 83 08 91 
  00000310  c0 03 5f d6 ff c3 16 d1  f0 03 00 91 10 82 16 91 
  00000320  1d 7a 00 a9 fd 03 00 91  40 00 80 d2 21 00 80 d2 
  00000330  02 00 80 d2 00 00 00 94  e0 03 00 f9 01 00 00 14 
  00000340  f0 03 00 91 10 e2 12 91  f0 07 00 f9 f0 03 80 b9 
  00000350  1f 02 00 f1 f0 a7 9f 9a  f0 0b 00 f9 f1 07 40 f9 
  00000360  f0 43 40 39 30 02 00 39  f0 07 40 f9 11 02 40 39 
  00000370  f1 13 00 f9 f0 83 40 39  1f 06 00 f1 f0 17 9f 9a 
  00000380  f0 17 00 f9 f0 17 40 f9  1f 02 00 f1 41 00 00 54 
  00000390  08 00 00 14 bf 03 00 91  f0 03 00 91 10 82 16 91 
  000003a0  1d 7a 40 a9 ff c3 16 91  00 00 80 d2 c0 03 5f d6 
  000003b0  01 00 00 14 f0 03 00 91  10 02 13 91 f0 1b 00 f9 
  000003c0  f1 1b 40 f9 30 00 80 d2  30 02 00 b9 f0 03 00 91 
  000003d0  10 22 13 91 f0 23 00 f9  f1 23 40 f9 f0 1b 40 f9 
  000003e0  30 02 00 f9 f0 03 00 91  10 42 13 91 f0 2b 00 f9 
  000003f0  f0 23 40 f9 11 02 40 f9  f1 2f 00 f9 f0 2f 40 f9 
  00000400  f0 33 00 f9 f1 2b 40 f9  f0 33 40 f9 30 02 00 f9 
  00000410  f0 03 00 91 10 62 13 91  f0 3b 00 f9 f0 2b 40 f9 
  00000420  11 02 40 f9 f1 3f 00 f9  f1 3b 40 f9 f0 3f 40 f9 
  00000430  30 02 00 f9 f0 03 00 91  10 82 13 91 f0 47 00 f9 
  00000440  f0 3b 40 f9 11 02 40 f9  f1 4b 00 f9 f0 4b 40 f9 
  00000450  f0 4f 00 f9 f1 47 40 f9  f0 4f 40 f9 30 02 00 f9 
  00000460  f0 47 40 f9 11 02 40 f9  f1 57 00 f9 e0 03 80 b9 
  00000470  21 00 80 d2 42 00 80 d2  e3 57 40 f9 84 00 80 d2 
  00000480  00 00 00 94 e0 5b 00 f9  01 00 00 14 e0 03 00 91 
  00000490  00 60 12 91 e1 03 80 d2  22 12 80 d2 d9 fe ff 97 
  000004a0  f0 03 00 91 10 62 12 91  f0 5f 00 f9 f0 03 00 91 
  000004b0  10 a2 13 91 f0 63 00 f9  f1 63 40 f9 f0 4f 42 f9 
  000004c0  e9 03 11 aa 30 01 00 f9  f0 53 42 f9 e9 03 11 aa 
  000004d0  29 21 00 91 30 01 00 f9  01 00 00 14 f0 03 00 91 
  000004e0  10 e2 13 91 f0 6b 00 f9  f1 6b 40 f9 f0 63 40 f9 
  000004f0  30 02 00 f9 f0 03 00 91  10 02 14 91 f0 73 00 f9 
  00000500  f0 6b 40 f9 11 02 40 f9  f1 77 00 f9 f0 77 40 f9 
  00000510  f0 7b 00 f9 f1 73 40 f9  f0 7b 40 f9 30 02 00 f9 
  00000520  f0 03 00 91 10 22 14 91  f0 83 00 f9 f0 73 40 f9 
  00000530  11 02 40 f9 f1 87 00 f9  f1 83 40 f9 f0 87 40 f9 
  00000540  30 02 00 f9 f0 03 00 91  10 42 14 91 f0 8f 00 f9 
  00000550  f0 83 40 f9 11 02 40 f9  f1 93 00 f9 f0 93 40 f9 
  00000560  f0 97 00 f9 f1 8f 40 f9  f0 97 40 f9 30 02 00 f9 
  00000570  f0 8f 40 f9 11 02 40 f9  f1 9f 00 f9 e0 03 80 b9 
  00000580  e1 9f 40 f9 02 02 80 d2  00 00 00 94 e0 a3 00 f9 
  00000590  01 00 00 14 f0 03 00 91  10 62 14 91 f0 a7 00 f9 
  000005a0  f0 43 81 b9 1f 02 00 f1  f0 07 9f 9a f0 ab 00 f9 
  000005b0  f1 a7 40 f9 f0 43 45 39  30 02 00 39 f0 a7 40 f9 
  000005c0  11 02 40 39 f1 b3 00 f9  f0 83 45 39 1f 06 00 f1 
  000005d0  f0 17 9f 9a f0 b7 00 f9  f0 b7 40 f9 1f 02 00 f1 
  000005e0  41 00 00 54 05 00 00 14  e0 03 80 b9 00 00 00 94 
  000005f0  e0 bb 00 f9 02 00 00 14  08 00 00 14 bf 03 00 91 
  00000600  f0 03 00 91 10 82 16 91  1d 7a 40 a9 ff c3 16 91 
  00000610  00 00 80 d2 c0 03 5f d6  e0 03 80 b9 01 10 80 d2 
  00000620  00 00 00 94 e0 bf 00 f9  01 00 00 14 f0 03 00 91 
  00000630  10 82 14 91 f0 c3 00 f9  f0 7b 81 b9 1f 02 00 f1 
  00000640  f0 07 9f 9a f0 c7 00 f9  f1 c3 40 f9 f0 23 46 39 
  00000650  30 02 00 39 f0 c3 40 f9  11 02 40 39 f1 cf 00 f9 
  00000660  f0 63 46 39 1f 06 00 f1  f0 17 9f 9a f0 d3 00 f9 
  00000670  f0 d3 40 f9 1f 02 00 f1  41 00 00 54 05 00 00 14 
  00000680  e0 03 80 b9 00 00 00 94  e0 d7 00 f9 02 00 00 14 
  00000690  08 00 00 14 bf 03 00 91  f0 03 00 91 10 82 16 91 
  000006a0  1d 7a 40 a9 ff c3 16 91  00 00 80 d2 c0 03 5f d6 
  000006b0  00 00 00 90 00 00 00 91  00 60 01 91 00 00 00 94 
  000006c0  01 00 00 14 01 00 00 14  e0 03 00 91 00 a0 12 91 
  000006d0  01 00 80 d2 02 00 80 d2  4a fe ff 97 f0 03 00 91 
  000006e0  10 a2 12 91 f0 df 00 f9  f0 03 00 91 10 a2 14 91 
  000006f0  f0 e3 00 f9 f1 e3 40 f9  f0 57 42 f9 e9 03 11 aa 
  00000700  30 01 00 f9 f0 5b 42 f9  e9 03 11 aa 29 21 00 91 
  00000710  30 01 00 f9 01 00 00 14  f0 03 00 91 10 e2 14 91 
  00000720  f0 eb 00 f9 f1 eb 40 f9  10 02 80 d2 30 02 00 b9 
  00000730  f0 03 00 91 10 02 15 91  f0 f3 00 f9 f1 f3 40 f9 
  00000740  f0 e3 40 f9 30 02 00 f9  f0 03 00 91 10 22 15 91 
  00000750  f0 fb 00 f9 f0 f3 40 f9  11 02 40 f9 f1 ff 00 f9 
  00000760  f0 ff 40 f9 f0 03 01 f9  f1 fb 40 f9 f0 03 41 f9 
  00000770  30 02 00 f9 f0 03 00 91  10 42 15 91 f0 0b 01 f9 
  00000780  f0 fb 40 f9 11 02 40 f9  f1 0f 01 f9 f1 0b 41 f9 
  00000790  f0 0f 41 f9 30 02 00 f9  f0 03 00 91 10 62 15 91 
  000007a0  f0 17 01 f9 f1 17 41 f9  f0 eb 40 f9 30 02 00 f9 
  000007b0  f0 03 00 91 10 82 15 91  f0 1f 01 f9 f0 17 41 f9 
  000007c0  11 02 40 f9 f1 23 01 f9  f0 23 41 f9 f0 27 01 f9 
  000007d0  f1 1f 41 f9 f0 27 41 f9  30 02 00 f9 f0 03 00 91 
  000007e0  10 a2 15 91 f0 2f 01 f9  f0 1f 41 f9 11 02 40 f9 
  000007f0  f1 33 01 f9 f1 2f 41 f9  f0 33 41 f9 30 02 00 f9 
  00000800  f0 03 00 91 10 c2 15 91  f0 3b 01 f9 f0 0b 41 f9 
  00000810  11 02 40 f9 f1 3f 01 f9  f0 3f 41 f9 f0 43 01 f9 
  00000820  f1 3b 41 f9 f0 43 41 f9  30 02 00 f9 f0 3b 41 f9 
  00000830  11 02 40 f9 f1 4b 01 f9  f0 2f 41 f9 11 02 40 f9 
  00000840  f1 4f 01 f9 e0 03 80 b9  e1 4b 41 f9 e2 4f 41 f9 
  00000850  00 00 00 94 e0 53 01 f9  01 00 00 14 f0 03 00 91 
  00000860  10 e2 15 91 f0 57 01 f9  f0 a3 82 b9 1f 02 00 f1 
  00000870  f0 a7 9f 9a f0 5b 01 f9  f1 57 41 f9 f0 c3 4a 39 
  00000880  30 02 00 39 f0 57 41 f9  11 02 40 39 f1 63 01 f9 
  00000890  f0 03 4b 39 1f 06 00 f1  f0 17 9f 9a f0 67 01 f9 
  000008a0  f0 67 41 f9 1f 02 00 f1  41 00 00 54 02 00 00 14 
  000008b0  85 ff ff 17 01 00 00 14  f0 03 00 91 10 02 16 91 
  000008c0  f0 6b 01 f9 f1 6b 41 f9  10 00 00 90 10 02 00 91 
  000008d0  30 02 00 f9 f0 03 00 91  10 22 16 91 f0 73 01 f9 
  000008e0  f0 6b 41 f9 11 02 40 f9  f1 77 01 f9 f1 73 41 f9 
  000008f0  f0 77 41 f9 30 02 00 f9  f0 73 41 f9 11 02 40 f9 
  00000900  f1 7f 01 f9 e0 7f 41 f9  00 00 00 94 e0 83 01 f9 
  00000910  01 00 00 14 f0 03 00 91  10 42 16 91 f0 87 01 f9 
  00000920  f1 87 41 f9 10 00 00 90  10 02 00 91 30 02 00 f9 
  00000930  f0 03 00 91 10 62 16 91  f0 8f 01 f9 f0 87 41 f9 
  00000940  11 02 40 f9 f1 93 01 f9  f1 8f 41 f9 f0 93 41 f9 
  00000950  30 02 00 f9 f0 8f 41 f9  11 02 40 f9 f1 9b 01 f9 
  00000960  e0 a3 82 b9 e1 9b 41 f9  e2 83 41 f9 00 00 00 94 
  00000970  e0 9f 01 f9 01 00 00 14  e0 a3 82 b9 00 00 00 94 
  00000980  e0 a3 01 f9 01 00 00 14  4f ff ff 17 bf 03 00 91 
  00000990  f0 03 00 91 10 82 16 91  1d 7a 40 a9 ff c3 16 91 
  000009a0  00 00 80 d2 c0 03 5f d6  bf 03 00 91 f0 03 00 91 
  000009b0  10 82 16 91 1d 7a 40 a9  ff c3 16 91 00 00 80 d2 
  000009c0  c0 03 5f d6 bf 03 00 91  f0 03 00 91 10 82 16 91 
  000009d0  1d 7a 40 a9 ff c3 16 91  00 00 80 d2 c0 03 5f d6 
  000009e0  bf 03 00 91 f0 03 00 91  10 82 16 91 1d 7a 40 a9 
  000009f0  ff c3 16 91 00 00 80 d2  c0 03 5f d6 bf 03 00 91 
  00000a00  f0 03 00 91 10 82 16 91  1d 7a 40 a9 ff c3 16 91 
  00000a10  00 00 80 d2 c0 03 5f d6 

.rodata (117 bytes):
  00000000  48 54 54 50 2f 31 2e 31  20 32 30 30 20 4f 4b 0d 
  00000010  0a 43 6f 6e 74 65 6e 74  2d 4c 65 6e 67 74 68 3a 
  00000020  20 33 0d 0a 43 6f 6e 6e  65 63 74 69 6f 6e 3a 20 
  00000030  63 6c 6f 73 65 0d 0a 0d  0a 4f 4b 0a 00 00 00 00 
  00000040  02 00 00 00 01 00 00 00  01 00 00 00 02 00 00 00 
  00000050  10 00 00 00 00 00 00 00  6c 69 73 74 65 6e 69 6e 
  00000060  67 20 6f 6e 20 31 32 37  2e 30 2e 30 2e 31 3a 38 
  00000070  30 38 31 0a 00 
