fp-native dump: format=MachO arch=Aarch64 entry=0x0

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println), 42
    ret


Symbols:
  main                             0x00000000

Relocations:
  offset=0x0000000c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000014 kind=CallRel32 symbol=printf addend=0
  offset=0x00000018 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000024 kind=CallRel32 symbol=printf addend=0
  offset=0x00000028 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000034 kind=CallRel32 symbol=printf addend=0
  offset=0x00000038 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000050 kind=CallRel32 symbol=printf addend=0

.text (104 bytes):
  00000000  ff c3 01 d1 fd 7b 06 a9  fd 03 00 91 00 00 00 90 
  00000010  00 00 00 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000020  00 60 00 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000030  00 40 01 91 00 00 00 94  00 00 00 90 00 00 00 91 
  00000040  00 c0 01 91 41 05 80 d2  50 05 80 d2 f0 03 00 f9 
  00000050  00 00 00 94 bf 03 00 91  fd 7b 46 a9 ff c3 01 91 
  00000060  00 00 80 d2 c0 03 5f d6 

.rodata (127 bytes):
  00000000  43 72 6f 73 73 2d 63 6f  6d 70 69 6c 65 20 64 65 
  00000010  6d 6f 3a 0a 00 00 00 00  2d 20 74 61 72 67 65 74 
  00000020  20 74 72 69 70 6c 65 3a  20 73 65 74 20 76 69 61 
  00000030  20 66 70 20 63 6f 6d 70  69 6c 65 20 2d 2d 74 61 
  00000040  72 67 65 74 20 3c 74 72  69 70 6c 65 3e 0a 00 00 
  00000050  2d 20 6f 75 74 70 75 74  3a 20 2e 6c 6c 20 28 4c 
  00000060  4c 56 4d 20 49 52 29 0a  00 00 00 00 00 00 00 00 
  00000070  2d 20 76 61 6c 75 65 3a  20 25 6c 6c 64 0a 00 
