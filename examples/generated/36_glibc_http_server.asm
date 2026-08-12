fp-native dump: format=MachO arch=Aarch64 entry=0x484

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global ::AF_INET ty=I32 constant=true initializer=Some(Bytes([2, 0, 0, 0]))
global ::SOCK_STREAM ty=I32 constant=true initializer=Some(Bytes([1, 0, 0, 0]))
global ::SOL_SOCKET ty=I32 constant=true initializer=Some(Bytes([1, 0, 0, 0]))
global ::SO_REUSEADDR ty=I32 constant=true initializer=Some(Bytes([2, 0, 0, 0]))
global ::SOCKADDR_LEN ty=I32 constant=true initializer=Some(Bytes([16, 0, 0, 0]))
global __const_data_0 ty=Array(I8, 112) constant=true initializer=Some(Bytes([72, 84, 84, 80, 47, 49, 46, 49, 32, 50, 48, 48, 32, 79, 75, 13, 10, 67, 111, 110, 116, 101, 110, 116, 45, 76, 101, 110, 103, 116, 104, 58, 32, 49, 50, 13, 10, 67, 111, 110, 116, 101, 110, 116, 45, 84, 121, 112, 101, 58, 32, 116, 101, 120, 116, 47, 112, 108, 97, 105, 110, 59, 32, 99, 104, 97, 114, 115, 101, 116, 61, 117, 116, 102, 45, 56, 13, 10, 67, 111, 110, 110, 101, 99, 116, 105, 111, 110, 58, 32, 99, 108, 111, 115, 101, 13, 10, 13, 10, 72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 10, 0]))
global ::RESPONSE ty=Ptr(I8) constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_1 ty=Array(I8, 7) constant=true initializer=Some(Bytes([115, 111, 99, 107, 101, 116, 0]))
global ::SOCKET_ERR ty=Ptr(I8) constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_2 ty=Array(I8, 5) constant=true initializer=Some(Bytes([98, 105, 110, 100, 0]))
global ::BIND_ERR ty=Ptr(I8) constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_3 ty=Array(I8, 7) constant=true initializer=Some(Bytes([108, 105, 115, 116, 101, 110, 0]))
global ::LISTEN_ERR ty=Ptr(I8) constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0]))
global __const_data_4 ty=Array(I8, 7) constant=true initializer=Some(Bytes([97, 99, 99, 101, 112, 116, 0]))
global ::ACCEPT_ERR ty=Ptr(I8) constant=true initializer=Some(Bytes([0, 0, 0, 0, 0, 0, 0, 0]))
fn socket
fn setsockopt
fn bind
fn listen
fn accept
fn write
fn strlen
fn perror
fn close
fn examples__36_glibc_http_server__make_addr
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 1, bank: General, size_bits: 64 }, 1
    sextortrunc Virtual { id: 2, bank: General, size_bits: 8 }, 16
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 2, bank: General, size_bits: 8 }
    alloca Virtual { id: 4, bank: General, size_bits: 64 }, 1
    sextortrunc Virtual { id: 5, bank: General, size_bits: 8 }, 2
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 4, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 5, bank: General, size_bits: 8 }
    alloca Virtual { id: 7, bank: General, size_bits: 64 }, 1
    shr Virtual { id: 8, bank: General, size_bits: 16 }, symbol(local.1), 8
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 7, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 8, bank: General, size_bits: 16 }
    alloca Virtual { id: 10, bank: General, size_bits: 64 }, 1
    load Virtual { id: 11, bank: General, size_bits: 16 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 7, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(2), address_space: None, pre_indexed: false, post_indexed: false })
    and Virtual { id: 12, bank: General, size_bits: 16 }, Virtual { id: 11, bank: General, size_bits: 16 }, 255
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 10, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 12, bank: General, size_bits: 16 }
    alloca Virtual { id: 14, bank: General, size_bits: 64 }, 1
    load Virtual { id: 15, bank: General, size_bits: 16 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 10, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(2), address_space: None, pre_indexed: false, post_indexed: false })
    sextortrunc Virtual { id: 16, bank: General, size_bits: 8 }, Virtual { id: 15, bank: General, size_bits: 16 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 14, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 16, bank: General, size_bits: 8 }
    alloca Virtual { id: 18, bank: General, size_bits: 64 }, 1
    and Virtual { id: 19, bank: General, size_bits: 16 }, symbol(local.1), 255
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 18, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 19, bank: General, size_bits: 16 }
    alloca Virtual { id: 21, bank: General, size_bits: 64 }, 1
    load Virtual { id: 22, bank: General, size_bits: 16 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 18, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(2), address_space: None, pre_indexed: false, post_indexed: false })
    sextortrunc Virtual { id: 23, bank: General, size_bits: 8 }, Virtual { id: 22, bank: General, size_bits: 16 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 23, bank: General, size_bits: 8 }
    load Virtual { id: 25, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 1, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 26, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 4, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 27, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 14, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 28, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 21, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    insertvalue Virtual { id: 29, bank: General, size_bits: 64 }, 0, Virtual { id: 25, bank: General, size_bits: 8 }, 0
    insertvalue Virtual { id: 30, bank: General, size_bits: 64 }, Virtual { id: 29, bank: General, size_bits: 64 }, Virtual { id: 26, bank: General, size_bits: 8 }, 1
    insertvalue Virtual { id: 31, bank: General, size_bits: 64 }, Virtual { id: 30, bank: General, size_bits: 64 }, Virtual { id: 27, bank: General, size_bits: 8 }, 2
    insertvalue Virtual { id: 32, bank: General, size_bits: 64 }, Virtual { id: 31, bank: General, size_bits: 64 }, Virtual { id: 28, bank: General, size_bits: 8 }, 3
    insertvalue Virtual { id: 33, bank: General, size_bits: 64 }, Virtual { id: 32, bank: General, size_bits: 64 }, 0, 4
    insertvalue Virtual { id: 34, bank: General, size_bits: 64 }, Virtual { id: 33, bank: General, size_bits: 64 }, 0, 5
    insertvalue Virtual { id: 35, bank: General, size_bits: 64 }, Virtual { id: 34, bank: General, size_bits: 64 }, 0, 6
    insertvalue Virtual { id: 36, bank: General, size_bits: 64 }, Virtual { id: 35, bank: General, size_bits: 64 }, 0, 7
    insertvalue Virtual { id: 37, bank: General, size_bits: 64 }, Virtual { id: 36, bank: General, size_bits: 64 }, 0, 8
    insertvalue Virtual { id: 38, bank: General, size_bits: 64 }, Virtual { id: 37, bank: General, size_bits: 64 }, 0, 9
    insertvalue Virtual { id: 39, bank: General, size_bits: 64 }, Virtual { id: 38, bank: General, size_bits: 64 }, 0, 10
    insertvalue Virtual { id: 40, bank: General, size_bits: 64 }, Virtual { id: 39, bank: General, size_bits: 64 }, 0, 11
    insertvalue Virtual { id: 41, bank: General, size_bits: 64 }, Virtual { id: 40, bank: General, size_bits: 64 }, 0, 12
    insertvalue Virtual { id: 42, bank: General, size_bits: 64 }, Virtual { id: 41, bank: General, size_bits: 64 }, 0, 13
    insertvalue Virtual { id: 43, bank: General, size_bits: 64 }, Virtual { id: 42, bank: General, size_bits: 64 }, 0, 14
    insertvalue Virtual { id: 44, bank: General, size_bits: 64 }, Virtual { id: 43, bank: General, size_bits: 64 }, 0, 15
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 44, bank: General, size_bits: 64 }
    load Virtual { id: 46, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn main
  bb0 bb0
    call symbol(socket)(2, 1, 0) cc=C tail=false
    br
  bb1 bb1
    alloca Virtual { id: 48, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 49, bank: General, size_bits: 32 }, Virtual { id: 47, bank: General, size_bits: 32 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 48, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 49, bank: General, size_bits: 32 }
    alloca Virtual { id: 51, bank: General, size_bits: 64 }, 1
    load Virtual { id: 52, bank: General, size_bits: 32 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 48, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(4), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 51, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 52, bank: General, size_bits: 32 }
    load Virtual { id: 54, bank: General, size_bits: 32 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 51, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(4), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 54, bank: General, size_bits: 32 }
    alloca Virtual { id: 56, bank: General, size_bits: 64 }, 1
    load Virtual { id: 57, bank: General, size_bits: 32 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 51, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(4), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 58, bank: General, size_bits: 8 }, Virtual { id: 57, bank: General, size_bits: 32 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 56, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 58, bank: General, size_bits: 8 }
    load Virtual { id: 60, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 56, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 61, bank: General, size_bits: 8 }, Virtual { id: 60, bank: General, size_bits: 8 }, 1
    condbr
  bb2 bb2
    alloca Virtual { id: 62, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 62, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_1)
    alloca Virtual { id: 64, bank: General, size_bits: 64 }, 1
    load Virtual { id: 65, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 62, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 64, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 65, bank: General, size_bits: 64 }
    load Virtual { id: 67, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 64, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(perror)(v67) cc=C tail=false
    br
  bb3 bb3
    br
  bb5 bb5
    ret
  bb4 bb4
    alloca Virtual { id: 69, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 69, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 1
    alloca Virtual { id: 71, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 71, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 69, bank: General, size_bits: 64 }
    alloca Virtual { id: 73, bank: General, size_bits: 64 }, 1
    load Virtual { id: 74, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 71, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 75, bank: General, size_bits: 64 }, Virtual { id: 74, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 73, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 75, bank: General, size_bits: 64 }
    alloca Virtual { id: 77, bank: General, size_bits: 64 }, 1
    load Virtual { id: 78, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 73, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 77, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 78, bank: General, size_bits: 64 }
    alloca Virtual { id: 80, bank: General, size_bits: 64 }, 1
    load Virtual { id: 81, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 77, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 82, bank: General, size_bits: 64 }, Virtual { id: 81, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 80, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 82, bank: General, size_bits: 64 }
    load Virtual { id: 84, bank: General, size_bits: 32 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 51, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(4), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 85, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 80, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(setsockopt)(v84, 1, 2, v85, 4) cc=C tail=false
    br
  bb7 bb7
    call symbol(examples__36_glibc_http_server__make_addr)(8080) cc=C tail=false
    alloca Virtual { id: 88, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 88, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 87, bank: General, size_bits: 64 }
    br
  bb8 bb8
    alloca Virtual { id: 90, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 90, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 88, bank: General, size_bits: 64 }
    alloca Virtual { id: 92, bank: General, size_bits: 64 }, 1
    load Virtual { id: 93, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 90, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 94, bank: General, size_bits: 64 }, Virtual { id: 93, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 94, bank: General, size_bits: 64 }
    alloca Virtual { id: 96, bank: General, size_bits: 64 }, 1
    load Virtual { id: 97, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 92, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 96, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 97, bank: General, size_bits: 64 }
    alloca Virtual { id: 99, bank: General, size_bits: 64 }, 1
    load Virtual { id: 100, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 96, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 101, bank: General, size_bits: 64 }, Virtual { id: 100, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 99, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 101, bank: General, size_bits: 64 }
    load Virtual { id: 103, bank: General, size_bits: 32 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 51, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(4), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 104, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 99, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(bind)(v103, v104, 16) cc=C tail=false
    br
  bb9 bb9
    intrinsic.call symbol(intrinsic.println), Virtual { id: 105, bank: General, size_bits: 32 }
    alloca Virtual { id: 107, bank: General, size_bits: 64 }, 1
    ne Virtual { id: 108, bank: General, size_bits: 8 }, Virtual { id: 105, bank: General, size_bits: 32 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 107, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 108, bank: General, size_bits: 8 }
    load Virtual { id: 110, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 107, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 111, bank: General, size_bits: 8 }, Virtual { id: 110, bank: General, size_bits: 8 }, 1
    condbr
  bb10 bb10
    alloca Virtual { id: 112, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 112, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_2)
    alloca Virtual { id: 114, bank: General, size_bits: 64 }, 1
    load Virtual { id: 115, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 112, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 114, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 115, bank: General, size_bits: 64 }
    load Virtual { id: 117, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 114, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(perror)(v117) cc=C tail=false
    br
  bb11 bb11
    br
  bb13 bb13
    load Virtual { id: 119, bank: General, size_bits: 32 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 51, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(4), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(close)(v119) cc=C tail=false
    br
  bb12 bb12
    load Virtual { id: 121, bank: General, size_bits: 32 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 51, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(4), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(listen)(v121, 16) cc=C tail=false
    br
  bb14 bb14
    ret
  bb16 bb16
    intrinsic.call symbol(intrinsic.println), Virtual { id: 122, bank: General, size_bits: 32 }
    alloca Virtual { id: 124, bank: General, size_bits: 64 }, 1
    ne Virtual { id: 125, bank: General, size_bits: 8 }, Virtual { id: 122, bank: General, size_bits: 32 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 124, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 125, bank: General, size_bits: 8 }
    load Virtual { id: 127, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 124, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 128, bank: General, size_bits: 8 }, Virtual { id: 127, bank: General, size_bits: 8 }, 1
    condbr
  bb17 bb17
    alloca Virtual { id: 129, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 129, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_3)
    alloca Virtual { id: 131, bank: General, size_bits: 64 }, 1
    load Virtual { id: 132, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 129, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 131, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 132, bank: General, size_bits: 64 }
    load Virtual { id: 134, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 131, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(perror)(v134) cc=C tail=false
    br
  bb18 bb18
    br
  bb20 bb20
    br
  bb19 bb19
    intrinsic.call symbol(intrinsic.println)
    br
  bb21 bb21
    br
  bb22 bb22
    call symbol(examples__36_glibc_http_server__make_addr)(0) cc=C tail=false
    alloca Virtual { id: 138, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 138, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 137, bank: General, size_bits: 64 }
    br
  bb24 bb24
    alloca Virtual { id: 140, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 140, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), 16
    alloca Virtual { id: 142, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 142, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 138, bank: General, size_bits: 64 }
    alloca Virtual { id: 144, bank: General, size_bits: 64 }, 1
    load Virtual { id: 145, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 142, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 146, bank: General, size_bits: 64 }, Virtual { id: 145, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 144, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 146, bank: General, size_bits: 64 }
    alloca Virtual { id: 148, bank: General, size_bits: 64 }, 1
    load Virtual { id: 149, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 144, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 148, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 149, bank: General, size_bits: 64 }
    alloca Virtual { id: 151, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 151, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 140, bank: General, size_bits: 64 }
    alloca Virtual { id: 153, bank: General, size_bits: 64 }, 1
    load Virtual { id: 154, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 151, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 155, bank: General, size_bits: 64 }, Virtual { id: 154, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 153, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 155, bank: General, size_bits: 64 }
    alloca Virtual { id: 157, bank: General, size_bits: 64 }, 1
    load Virtual { id: 158, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 153, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 157, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 158, bank: General, size_bits: 64 }
    alloca Virtual { id: 160, bank: General, size_bits: 64 }, 1
    load Virtual { id: 161, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 148, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    bitcast Virtual { id: 162, bank: General, size_bits: 64 }, Virtual { id: 161, bank: General, size_bits: 64 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 160, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 162, bank: General, size_bits: 64 }
    load Virtual { id: 164, bank: General, size_bits: 32 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 51, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(4), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 165, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 160, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 166, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 157, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(accept)(v164, v165, v166) cc=C tail=false
    br
  bb25 bb25
    alloca Virtual { id: 168, bank: General, size_bits: 64 }, 1
    bitcast Virtual { id: 169, bank: General, size_bits: 32 }, Virtual { id: 167, bank: General, size_bits: 32 }
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 168, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 169, bank: General, size_bits: 32 }
    alloca Virtual { id: 171, bank: General, size_bits: 64 }, 1
    load Virtual { id: 172, bank: General, size_bits: 32 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 168, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(4), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 171, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 172, bank: General, size_bits: 32 }
    alloca Virtual { id: 174, bank: General, size_bits: 64 }, 1
    load Virtual { id: 175, bank: General, size_bits: 32 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 171, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(4), address_space: None, pre_indexed: false, post_indexed: false })
    lt Virtual { id: 176, bank: General, size_bits: 8 }, Virtual { id: 175, bank: General, size_bits: 32 }, 0
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 174, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 176, bank: General, size_bits: 8 }
    load Virtual { id: 178, bank: General, size_bits: 8 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 174, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(1), address_space: None, pre_indexed: false, post_indexed: false })
    eq Virtual { id: 179, bank: General, size_bits: 8 }, Virtual { id: 178, bank: General, size_bits: 8 }, 1
    condbr
  bb26 bb26
    alloca Virtual { id: 180, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 180, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_4)
    alloca Virtual { id: 182, bank: General, size_bits: 64 }, 1
    load Virtual { id: 183, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 180, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 182, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 183, bank: General, size_bits: 64 }
    load Virtual { id: 185, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 182, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(perror)(v185) cc=C tail=false
    br
  bb27 bb27
    br
  bb29 bb29
    br
  bb28 bb28
    alloca Virtual { id: 187, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 187, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_0)
    alloca Virtual { id: 189, bank: General, size_bits: 64 }, 1
    load Virtual { id: 190, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 187, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 189, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 190, bank: General, size_bits: 64 }
    load Virtual { id: 192, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 189, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(strlen)(v192) cc=C tail=false
    br
  bb31 bb31
    alloca Virtual { id: 194, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 194, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(__const_data_0)
    alloca Virtual { id: 196, bank: General, size_bits: 64 }, 1
    load Virtual { id: 197, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 194, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 196, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 197, bank: General, size_bits: 64 }
    load Virtual { id: 199, bank: General, size_bits: 32 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 171, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(4), address_space: None, pre_indexed: false, post_indexed: false })
    load Virtual { id: 200, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 196, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(write)(v199, v200, v193) cc=C tail=false
    br
  bb32 bb32
    load Virtual { id: 202, bank: General, size_bits: 32 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 171, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(4), address_space: None, pre_indexed: false, post_indexed: false })
    call symbol(close)(v202) cc=C tail=false
    br
  bb33 bb33
    br
  bb6 bb6
    ret
  bb15 bb15
    ret
  bb23 bb23
    ret
  bb30 bb30
    ret


Symbols:
  examples__36_glibc_http_server__make_addr 0x00000000
  main                             0x00000484

Text relocations:
  offset=0x000004a4 kind=CallRel32 symbol=socket addend=0
  offset=0x00000500 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000518 kind=CallRel32 symbol=printf addend=0
  offset=0x0000058c kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x000005cc kind=CallRel32 symbol=perror addend=0
  offset=0x000006cc kind=CallRel32 symbol=setsockopt addend=0
  offset=0x000007dc kind=CallRel32 symbol=bind addend=0
  offset=0x000007e8 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000800 kind=CallRel32 symbol=printf addend=0
  offset=0x00000868 kind=Aarch64AdrpAdd symbol=__const_data_2 addend=0
  offset=0x000008a8 kind=CallRel32 symbol=perror addend=0
  offset=0x000008c4 kind=CallRel32 symbol=close addend=0
  offset=0x000008e4 kind=CallRel32 symbol=listen addend=0
  offset=0x0000090c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000924 kind=CallRel32 symbol=printf addend=0
  offset=0x0000098c kind=Aarch64AdrpAdd symbol=__const_data_3 addend=0
  offset=0x000009cc kind=CallRel32 symbol=perror addend=0
  offset=0x000009dc kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000009e8 kind=CallRel32 symbol=printf addend=0
  offset=0x00000b84 kind=CallRel32 symbol=accept addend=0
  offset=0x00000c44 kind=Aarch64AdrpAdd symbol=__const_data_4 addend=0
  offset=0x00000c84 kind=CallRel32 symbol=perror addend=0
  offset=0x00000ca4 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000ce4 kind=CallRel32 symbol=strlen addend=0
  offset=0x00000d00 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x00000d54 kind=CallRel32 symbol=write addend=0
  offset=0x00000d70 kind=CallRel32 symbol=close addend=0

Section relocations:
  section=Data offset=0x00000000 kind=Abs64 symbol=__const_data_0 addend=0
  section=Data offset=0x00000008 kind=Abs64 symbol=__const_data_1 addend=0
  section=Data offset=0x00000010 kind=Abs64 symbol=__const_data_2 addend=0
  section=Data offset=0x00000018 kind=Abs64 symbol=__const_data_3 addend=0
  section=Data offset=0x00000020 kind=Abs64 symbol=__const_data_4 addend=0

.text (3568 bytes):
  00000000  ff 03 0d d1 f0 03 00 91  10 c2 0c 91 1d 7a 00 a9 
  00000010  fd 03 00 91 e0 e7 00 f9  e1 13 03 79 f0 03 00 91 
  00000020  10 82 0b 91 f0 03 00 f9  f0 03 00 91 10 c2 0b 91 
  00000030  f0 07 00 f9 10 02 80 d2  f1 1f 80 d2 11 00 a0 f2 
  00000040  11 00 c0 f2 11 00 e0 f2  10 02 11 8a f0 0b 00 f9 
  00000050  f1 07 40 f9 f0 43 c0 39  30 02 00 39 f0 03 00 91 
  00000060  10 e2 0b 91 f0 13 00 f9  50 00 80 d2 f1 1f 80 d2 
  00000070  11 00 a0 f2 11 00 c0 f2  11 00 e0 f2 10 02 11 8a 
  00000080  f0 17 00 f9 f1 13 40 f9  f0 a3 c0 39 30 02 00 39 
  00000090  f0 03 00 91 10 02 0c 91  f0 1f 00 f9 f0 13 c3 79 
  000000a0  11 01 80 d2 10 26 d1 1a  f0 23 00 f9 f1 1f 40 f9 
  000000b0  f0 83 c0 79 30 02 00 79  f0 03 00 91 10 22 0c 91 
  000000c0  f0 2b 00 f9 f0 1f 40 f9  11 02 c0 79 f1 2f 00 f9 
  000000d0  f0 b3 c0 79 f1 1f 80 d2  10 02 11 8a f0 33 00 f9 
  000000e0  f1 2b 40 f9 f0 c3 c0 79  30 02 00 79 f0 03 00 91 
  000000f0  10 42 0c 91 f0 3b 00 f9  f0 2b 40 f9 11 02 c0 79 
  00000100  f1 3f 00 f9 f0 f3 c0 79  f1 1f 80 d2 11 00 a0 f2 
  00000110  11 00 c0 f2 11 00 e0 f2  10 02 11 8a f0 43 00 f9 
  00000120  f1 3b 40 f9 f0 03 c2 39  30 02 00 39 f0 03 00 91 
  00000130  10 62 0c 91 f0 4b 00 f9  f0 13 c3 79 f1 1f 80 d2 
  00000140  10 02 11 8a f0 4f 00 f9  f1 4b 40 f9 f0 33 c1 79 
  00000150  30 02 00 79 f0 03 00 91  10 82 0c 91 f0 57 00 f9 
  00000160  f0 4b 40 f9 11 02 c0 79  f1 5b 00 f9 f0 63 c1 79 
  00000170  f1 1f 80 d2 11 00 a0 f2  11 00 c0 f2 11 00 e0 f2 
  00000180  10 02 11 8a f0 5f 00 f9  f1 57 40 f9 f0 e3 c2 39 
  00000190  30 02 00 39 f0 07 40 f9  11 02 c0 39 f1 67 00 f9 
  000001a0  f0 13 40 f9 11 02 c0 39  f1 6b 00 f9 f0 3b 40 f9 
  000001b0  11 02 c0 39 f1 6f 00 f9  f0 57 40 f9 11 02 c0 39 
  000001c0  f1 73 00 f9 10 00 80 d2  f0 eb 00 f9 f0 ef 00 f9 
  000001d0  f0 23 c3 39 f0 43 07 39  f0 03 00 91 10 42 07 91 
  000001e0  f0 77 00 f9 f0 eb 40 f9  f0 f3 00 f9 f0 ef 40 f9 
  000001f0  f0 f7 00 f9 f0 43 c3 39  f0 87 07 39 f0 03 00 91 
  00000200  10 82 07 91 f0 7b 00 f9  f0 f3 40 f9 f0 fb 00 f9 
  00000210  f0 f7 40 f9 f0 ff 00 f9  f0 63 c3 39 f0 cb 07 39 
  00000220  f0 03 00 91 10 c2 07 91  f0 7f 00 f9 f0 fb 40 f9 
  00000230  f0 03 01 f9 f0 ff 40 f9  f0 07 01 f9 f0 83 c3 39 
  00000240  f0 0f 08 39 f0 03 00 91  10 02 08 91 f0 83 00 f9 
  00000250  f0 03 41 f9 f0 0b 01 f9  f0 07 41 f9 f0 0f 01 f9 
  00000260  10 00 80 d2 f0 53 08 39  f0 03 00 91 10 42 08 91 
  00000270  f0 87 00 f9 f0 0b 41 f9  f0 13 01 f9 f0 0f 41 f9 
  00000280  f0 17 01 f9 10 00 80 d2  f0 97 08 39 f0 03 00 91 
  00000290  10 82 08 91 f0 8b 00 f9  f0 13 41 f9 f0 1b 01 f9 
  000002a0  f0 17 41 f9 f0 1f 01 f9  10 00 80 d2 f0 db 08 39 
  000002b0  f0 03 00 91 10 c2 08 91  f0 8f 00 f9 f0 1b 41 f9 
  000002c0  f0 23 01 f9 f0 1f 41 f9  f0 27 01 f9 10 00 80 d2 
  000002d0  f0 1f 09 39 f0 03 00 91  10 02 09 91 f0 93 00 f9 
  000002e0  f0 23 41 f9 f0 2b 01 f9  f0 27 41 f9 f0 2f 01 f9 
  000002f0  10 00 80 d2 f0 63 09 39  f0 03 00 91 10 42 09 91 
  00000300  f0 97 00 f9 f0 2b 41 f9  f0 33 01 f9 f0 2f 41 f9 
  00000310  f0 37 01 f9 10 00 80 d2  f0 a7 09 39 f0 03 00 91 
  00000320  10 82 09 91 f0 9b 00 f9  f0 33 41 f9 f0 3b 01 f9 
  00000330  f0 37 41 f9 f0 3f 01 f9  10 00 80 d2 f0 eb 09 39 
  00000340  f0 03 00 91 10 c2 09 91  f0 9f 00 f9 f0 3b 41 f9 
  00000350  f0 43 01 f9 f0 3f 41 f9  f0 47 01 f9 10 00 80 d2 
  00000360  f0 2f 0a 39 f0 03 00 91  10 02 0a 91 f0 a3 00 f9 
  00000370  f0 43 41 f9 f0 4b 01 f9  f0 47 41 f9 f0 4f 01 f9 
  00000380  10 00 80 d2 f0 73 0a 39  f0 03 00 91 10 42 0a 91 
  00000390  f0 a7 00 f9 f0 4b 41 f9  f0 53 01 f9 f0 4f 41 f9 
  000003a0  f0 57 01 f9 10 00 80 d2  f0 b7 0a 39 f0 03 00 91 
  000003b0  10 82 0a 91 f0 ab 00 f9  f0 53 41 f9 f0 5b 01 f9 
  000003c0  f0 57 41 f9 f0 5f 01 f9  10 00 80 d2 f0 fb 0a 39 
  000003d0  f0 03 00 91 10 c2 0a 91  f0 af 00 f9 f0 5b 41 f9 
  000003e0  f0 63 01 f9 f0 5f 41 f9  f0 67 01 f9 10 00 80 d2 
  000003f0  f0 3f 0b 39 f0 03 00 91  10 02 0b 91 f0 b3 00 f9 
  00000400  f1 03 40 f9 f0 63 41 f9  e9 03 11 aa 30 01 00 f9 
  00000410  f0 67 41 f9 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  00000420  f1 03 40 f9 e9 03 11 aa  30 01 40 f9 f0 6b 01 f9 
  00000430  e9 03 11 aa 29 21 00 91  30 01 40 f9 f0 6f 01 f9 
  00000440  f0 03 00 91 10 42 0b 91  f0 bb 00 f9 f1 e7 40 f9 
  00000450  f0 6b 41 f9 e9 03 11 aa  30 01 00 f9 f0 6f 41 f9 
  00000460  e9 03 11 aa 29 21 00 91  30 01 00 f9 bf 03 00 91 
  00000470  f0 03 00 91 10 c2 0c 91  1d 7a 40 a9 ff 03 0d 91 
  00000480  c0 03 5f d6 ff 43 21 d1  f0 03 00 91 10 02 21 91 
  00000490  1d 7a 00 a9 fd 03 00 91  40 00 80 d2 21 00 80 d2 
  000004a0  02 00 80 d2 00 00 00 94  e0 0b 00 f9 01 00 00 14 
  000004b0  f0 03 00 91 10 e2 1b 91  f0 0f 00 f9 f0 13 80 b9 
  000004c0  f0 13 00 f9 f1 0f 40 f9  f0 23 80 b9 30 02 00 b9 
  000004d0  f0 03 00 91 10 02 1c 91  f0 1b 00 f9 f0 0f 40 f9 
  000004e0  11 02 80 b9 f1 1f 00 f9  f1 1b 40 f9 f0 3b 80 b9 
  000004f0  30 02 00 b9 f0 1b 40 f9  11 02 80 b9 f1 27 00 f9 
  00000500  00 00 00 90 00 00 00 91  00 80 02 91 e1 4b 80 b9 
  00000510  f0 4b 80 b9 f0 03 00 f9  00 00 00 94 f0 03 00 91 
  00000520  10 22 1c 91 f0 2f 00 f9  f0 1b 40 f9 11 02 80 b9 
  00000530  f1 33 00 f9 f0 63 80 b9  1f 02 00 f1 f0 a7 9f 9a 
  00000540  f0 37 00 f9 f1 2f 40 f9  f0 a3 41 39 30 02 00 39 
  00000550  f0 2f 40 f9 11 02 40 39  f1 3f 00 f9 f0 e3 41 39 
  00000560  1f 06 00 f1 f0 17 9f 9a  f0 43 00 f9 f0 43 40 f9 
  00000570  1f 02 00 f1 41 00 00 54  17 00 00 14 f0 03 00 91 
  00000580  10 42 1c 91 f0 47 00 f9  f1 47 40 f9 10 00 00 90 
  00000590  10 02 00 91 30 02 00 f9  f0 03 00 91 10 62 1c 91 
  000005a0  f0 4f 00 f9 f0 47 40 f9  11 02 40 f9 f1 53 00 f9 
  000005b0  f1 4f 40 f9 f0 53 40 f9  30 02 00 f9 f0 4f 40 f9 
  000005c0  11 02 40 f9 f1 5b 00 f9  e0 5b 40 f9 00 00 00 94 
  000005d0  02 00 00 14 08 00 00 14  bf 03 00 91 f0 03 00 91 
  000005e0  10 02 21 91 1d 7a 40 a9  ff 43 21 91 00 00 80 d2 
  000005f0  c0 03 5f d6 f0 03 00 91  10 82 1c 91 f0 63 00 f9 
  00000600  f1 63 40 f9 30 00 80 d2  30 02 00 b9 f0 03 00 91 
  00000610  10 a2 1c 91 f0 6b 00 f9  f1 6b 40 f9 f0 63 40 f9 
  00000620  30 02 00 f9 f0 03 00 91  10 c2 1c 91 f0 73 00 f9 
  00000630  f0 6b 40 f9 11 02 40 f9  f1 77 00 f9 f0 77 40 f9 
  00000640  f0 7b 00 f9 f1 73 40 f9  f0 7b 40 f9 30 02 00 f9 
  00000650  f0 03 00 91 10 e2 1c 91  f0 83 00 f9 f0 73 40 f9 
  00000660  11 02 40 f9 f1 87 00 f9  f1 83 40 f9 f0 87 40 f9 
  00000670  30 02 00 f9 f0 03 00 91  10 02 1d 91 f0 8f 00 f9 
  00000680  f0 83 40 f9 11 02 40 f9  f1 93 00 f9 f0 93 40 f9 
  00000690  f0 97 00 f9 f1 8f 40 f9  f0 97 40 f9 30 02 00 f9 
  000006a0  f0 1b 40 f9 11 02 80 b9  f1 9f 00 f9 f0 8f 40 f9 
  000006b0  11 02 40 f9 f1 a3 00 f9  e0 3b 81 b9 21 00 80 d2 
  000006c0  42 00 80 d2 e3 a3 40 f9  84 00 80 d2 00 00 00 94 
  000006d0  e0 a7 00 f9 01 00 00 14  e0 03 00 91 00 60 1b 91 
  000006e0  01 f2 83 d2 47 fe ff 97  f0 03 00 91 10 62 1b 91 
  000006f0  f0 ab 00 f9 f0 03 00 91  10 22 1d 91 f0 af 00 f9 
  00000700  f1 af 40 f9 f0 6f 43 f9  e9 03 11 aa 30 01 00 f9 
  00000710  f0 73 43 f9 e9 03 11 aa  29 21 00 91 30 01 00 f9 
  00000720  01 00 00 14 f0 03 00 91  10 62 1d 91 f0 b7 00 f9 
  00000730  f1 b7 40 f9 f0 af 40 f9  30 02 00 f9 f0 03 00 91 
  00000740  10 82 1d 91 f0 bf 00 f9  f0 b7 40 f9 11 02 40 f9 
  00000750  f1 c3 00 f9 f0 c3 40 f9  f0 c7 00 f9 f1 bf 40 f9 
  00000760  f0 c7 40 f9 30 02 00 f9  f0 03 00 91 10 a2 1d 91 
  00000770  f0 cf 00 f9 f0 bf 40 f9  11 02 40 f9 f1 d3 00 f9 
  00000780  f1 cf 40 f9 f0 d3 40 f9  30 02 00 f9 f0 03 00 91 
  00000790  10 c2 1d 91 f0 db 00 f9  f0 cf 40 f9 11 02 40 f9 
  000007a0  f1 df 00 f9 f0 df 40 f9  f0 e3 00 f9 f1 db 40 f9 
  000007b0  f0 e3 40 f9 30 02 00 f9  f0 1b 40 f9 11 02 80 b9 
  000007c0  f1 eb 00 f9 f0 db 40 f9  11 02 40 f9 f1 ef 00 f9 
  000007d0  e0 d3 81 b9 e1 ef 40 f9  02 02 80 d2 00 00 00 94 
  000007e0  e0 f3 00 f9 01 00 00 14  00 00 00 90 00 00 00 91 
  000007f0  00 c0 02 91 e1 e3 81 b9  f0 e3 81 b9 f0 03 00 f9 
  00000800  00 00 00 94 f0 03 00 91  10 e2 1d 91 f0 fb 00 f9 
  00000810  f0 e3 81 b9 1f 02 00 f1  f0 07 9f 9a f0 ff 00 f9 
  00000820  f1 fb 40 f9 f0 e3 47 39  30 02 00 39 f0 fb 40 f9 
  00000830  11 02 40 39 f1 07 01 f9  f0 23 48 39 1f 06 00 f1 
  00000840  f0 17 9f 9a f0 0b 01 f9  f0 0b 41 f9 1f 02 00 f1 
  00000850  41 00 00 54 17 00 00 14  f0 03 00 91 10 02 1e 91 
  00000860  f0 0f 01 f9 f1 0f 41 f9  10 00 00 90 10 02 00 91 
  00000870  30 02 00 f9 f0 03 00 91  10 22 1e 91 f0 17 01 f9 
  00000880  f0 0f 41 f9 11 02 40 f9  f1 1b 01 f9 f1 17 41 f9 
  00000890  f0 1b 41 f9 30 02 00 f9  f0 17 41 f9 11 02 40 f9 
  000008a0  f1 23 01 f9 e0 23 41 f9  00 00 00 94 02 00 00 14 
  000008b0  08 00 00 14 f0 1b 40 f9  11 02 80 b9 f1 2b 01 f9 
  000008c0  e0 53 82 b9 00 00 00 94  e0 2f 01 f9 09 00 00 14 
  000008d0  f0 1b 40 f9 11 02 80 b9  f1 33 01 f9 e0 63 82 b9 
  000008e0  01 02 80 d2 00 00 00 94  e0 37 01 f9 08 00 00 14 
  000008f0  bf 03 00 91 f0 03 00 91  10 02 21 91 1d 7a 40 a9 
  00000900  ff 43 21 91 00 00 80 d2  c0 03 5f d6 00 00 00 90 
  00000910  00 00 00 91 00 00 03 91  e1 6b 82 b9 f0 6b 82 b9 
  00000920  f0 03 00 f9 00 00 00 94  f0 03 00 91 10 42 1e 91 
  00000930  f0 3f 01 f9 f0 6b 82 b9  1f 02 00 f1 f0 07 9f 9a 
  00000940  f0 43 01 f9 f1 3f 41 f9  f0 03 4a 39 30 02 00 39 
  00000950  f0 3f 41 f9 11 02 40 39  f1 4b 01 f9 f0 43 4a 39 
  00000960  1f 06 00 f1 f0 17 9f 9a  f0 4f 01 f9 f0 4f 41 f9 
  00000970  1f 02 00 f1 41 00 00 54  17 00 00 14 f0 03 00 91 
  00000980  10 62 1e 91 f0 53 01 f9  f1 53 41 f9 10 00 00 90 
  00000990  10 02 00 91 30 02 00 f9  f0 03 00 91 10 82 1e 91 
  000009a0  f0 5b 01 f9 f0 53 41 f9  11 02 40 f9 f1 5f 01 f9 
  000009b0  f1 5b 41 f9 f0 5f 41 f9  30 02 00 f9 f0 5b 41 f9 
  000009c0  11 02 40 f9 f1 67 01 f9  e0 67 41 f9 00 00 00 94 
  000009d0  02 00 00 14 02 00 00 14  01 00 00 14 00 00 00 90 
  000009e0  00 00 00 91 00 40 03 91  00 00 00 94 01 00 00 14 
  000009f0  01 00 00 14 e0 03 00 91  00 a0 1b 91 01 00 80 d2 
  00000a00  80 fd ff 97 f0 03 00 91  10 a2 1b 91 f0 73 01 f9 
  00000a10  f0 03 00 91 10 a2 1e 91  f0 77 01 f9 f1 77 41 f9 
  00000a20  f0 77 43 f9 e9 03 11 aa  30 01 00 f9 f0 7b 43 f9 
  00000a30  e9 03 11 aa 29 21 00 91  30 01 00 f9 01 00 00 14 
  00000a40  f0 03 00 91 10 e2 1e 91  f0 7f 01 f9 f1 7f 41 f9 
  00000a50  10 02 80 d2 30 02 00 b9  f0 03 00 91 10 02 1f 91 
  00000a60  f0 87 01 f9 f1 87 41 f9  f0 77 41 f9 30 02 00 f9 
  00000a70  f0 03 00 91 10 22 1f 91  f0 8f 01 f9 f0 87 41 f9 
  00000a80  11 02 40 f9 f1 93 01 f9  f0 93 41 f9 f0 97 01 f9 
  00000a90  f1 8f 41 f9 f0 97 41 f9  30 02 00 f9 f0 03 00 91 
  00000aa0  10 42 1f 91 f0 9f 01 f9  f0 8f 41 f9 11 02 40 f9 
  00000ab0  f1 a3 01 f9 f1 9f 41 f9  f0 a3 41 f9 30 02 00 f9 
  00000ac0  f0 03 00 91 10 62 1f 91  f0 ab 01 f9 f1 ab 41 f9 
  00000ad0  f0 7f 41 f9 30 02 00 f9  f0 03 00 91 10 82 1f 91 
  00000ae0  f0 b3 01 f9 f0 ab 41 f9  11 02 40 f9 f1 b7 01 f9 
  00000af0  f0 b7 41 f9 f0 bb 01 f9  f1 b3 41 f9 f0 bb 41 f9 
  00000b00  30 02 00 f9 f0 03 00 91  10 a2 1f 91 f0 c3 01 f9 
  00000b10  f0 b3 41 f9 11 02 40 f9  f1 c7 01 f9 f1 c3 41 f9 
  00000b20  f0 c7 41 f9 30 02 00 f9  f0 03 00 91 10 c2 1f 91 
  00000b30  f0 cf 01 f9 f0 9f 41 f9  11 02 40 f9 f1 d3 01 f9 
  00000b40  f0 d3 41 f9 f0 d7 01 f9  f1 cf 41 f9 f0 d7 41 f9 
  00000b50  30 02 00 f9 f0 1b 40 f9  11 02 80 b9 f1 df 01 f9 
  00000b60  f0 cf 41 f9 11 02 40 f9  f1 e3 01 f9 f0 c3 41 f9 
  00000b70  11 02 40 f9 f1 e7 01 f9  e0 bb 83 b9 e1 e3 41 f9 
  00000b80  e2 e7 41 f9 00 00 00 94  e0 eb 01 f9 01 00 00 14 
  00000b90  f0 03 00 91 10 e2 1f 91  f0 ef 01 f9 f0 d3 83 b9 
  00000ba0  f0 f3 01 f9 f1 ef 41 f9  f0 e3 83 b9 30 02 00 b9 
  00000bb0  f0 03 00 91 10 02 20 91  f0 fb 01 f9 f0 ef 41 f9 
  00000bc0  11 02 80 b9 f1 ff 01 f9  f1 fb 41 f9 f0 fb 83 b9 
  00000bd0  30 02 00 b9 f0 03 00 91  10 22 20 91 f0 07 02 f9 
  00000be0  f0 fb 41 f9 11 02 80 b9  f1 0b 02 f9 f0 13 84 b9 
  00000bf0  1f 02 00 f1 f0 a7 9f 9a  f0 0f 02 f9 f1 07 42 f9 
  00000c00  f0 63 50 39 30 02 00 39  f0 07 42 f9 11 02 40 39 
  00000c10  f1 17 02 f9 f0 a3 50 39  1f 06 00 f1 f0 17 9f 9a 
  00000c20  f0 1b 02 f9 f0 1b 42 f9  1f 02 00 f1 41 00 00 54 
  00000c30  17 00 00 14 f0 03 00 91  10 42 20 91 f0 1f 02 f9 
  00000c40  f1 1f 42 f9 10 00 00 90  10 02 00 91 30 02 00 f9 
  00000c50  f0 03 00 91 10 62 20 91  f0 27 02 f9 f0 1f 42 f9 
  00000c60  11 02 40 f9 f1 2b 02 f9  f1 27 42 f9 f0 2b 42 f9 
  00000c70  30 02 00 f9 f0 27 42 f9  11 02 40 f9 f1 33 02 f9 
  00000c80  e0 33 42 f9 00 00 00 94  02 00 00 14 02 00 00 14 
  00000c90  58 ff ff 17 f0 03 00 91  10 82 20 91 f0 3b 02 f9 
  00000ca0  f1 3b 42 f9 10 00 00 90  10 02 00 91 30 02 00 f9 
  00000cb0  f0 03 00 91 10 a2 20 91  f0 43 02 f9 f0 3b 42 f9 
  00000cc0  11 02 40 f9 f1 47 02 f9  f1 43 42 f9 f0 47 42 f9 
  00000cd0  30 02 00 f9 f0 43 42 f9  11 02 40 f9 f1 4f 02 f9 
  00000ce0  e0 4f 42 f9 00 00 00 94  e0 53 02 f9 01 00 00 14 
  00000cf0  f0 03 00 91 10 c2 20 91  f0 57 02 f9 f1 57 42 f9 
  00000d00  10 00 00 90 10 02 00 91  30 02 00 f9 f0 03 00 91 
  00000d10  10 e2 20 91 f0 5f 02 f9  f0 57 42 f9 11 02 40 f9 
  00000d20  f1 63 02 f9 f1 5f 42 f9  f0 63 42 f9 30 02 00 f9 
  00000d30  f0 fb 41 f9 11 02 80 b9  f1 6b 02 f9 f0 5f 42 f9 
  00000d40  11 02 40 f9 f1 6f 02 f9  e0 d3 84 b9 e1 6f 42 f9 
  00000d50  e2 53 42 f9 00 00 00 94  e0 73 02 f9 01 00 00 14 
  00000d60  f0 fb 41 f9 11 02 80 b9  f1 77 02 f9 e0 eb 84 b9 
  00000d70  00 00 00 94 e0 7b 02 f9  01 00 00 14 1d ff ff 17 
  00000d80  bf 03 00 91 f0 03 00 91  10 02 21 91 1d 7a 40 a9 
  00000d90  ff 43 21 91 00 00 80 d2  c0 03 5f d6 bf 03 00 91 
  00000da0  f0 03 00 91 10 02 21 91  1d 7a 40 a9 ff 43 21 91 
  00000db0  00 00 80 d2 c0 03 5f d6  bf 03 00 91 f0 03 00 91 
  00000dc0  10 02 21 91 1d 7a 40 a9  ff 43 21 91 00 00 80 d2 
  00000dd0  c0 03 5f d6 bf 03 00 91  f0 03 00 91 10 02 21 91 
  00000de0  1d 7a 40 a9 ff 43 21 91  00 00 80 d2 c0 03 5f d6 

.rodata (235 bytes):
  00000000  02 00 00 00 01 00 00 00  01 00 00 00 02 00 00 00 
  00000010  10 00 00 00 48 54 54 50  2f 31 2e 31 20 32 30 30 
  00000020  20 4f 4b 0d 0a 43 6f 6e  74 65 6e 74 2d 4c 65 6e 
  00000030  67 74 68 3a 20 31 32 0d  0a 43 6f 6e 74 65 6e 74 
  00000040  2d 54 79 70 65 3a 20 74  65 78 74 2f 70 6c 61 69 
  00000050  6e 3b 20 63 68 61 72 73  65 74 3d 75 74 66 2d 38 
  00000060  0d 0a 43 6f 6e 6e 65 63  74 69 6f 6e 3a 20 63 6c 
  00000070  6f 73 65 0d 0a 0d 0a 48  65 6c 6c 6f 20 77 6f 72 
  00000080  6c 64 0a 00 73 6f 63 6b  65 74 00 62 69 6e 64 00 
  00000090  6c 69 73 74 65 6e 00 61  63 63 65 70 74 00 00 00 
  000000a0  73 6f 63 6b 65 74 20 66  64 3a 20 25 64 0a 00 00 
  000000b0  62 69 6e 64 20 72 63 3a  20 25 64 0a 00 00 00 00 
  000000c0  6c 69 73 74 65 6e 20 72  63 3a 20 25 64 0a 00 00 
  000000d0  6c 69 73 74 65 6e 69 6e  67 20 6f 6e 20 30 2e 30 
  000000e0  2e 30 2e 30 3a 38 30 38  30 0a 00 
