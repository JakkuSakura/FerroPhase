; ModuleID = ''
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx12.0.0"

@.str.0 = constant [36 x i8] [i8 67, i8 111, i8 109, i8 112, i8 117, i8 116, i8 101, i8 100, i8 58, i8 32, i8 102, i8 97, i8 99, i8 116, i8 111, i8 114, i8 105, i8 97, i8 108, i8 61, i8 37, i8 100, i8 44, i8 32, i8 105, i8 115, i8 95, i8 112, i8 111, i8 119, i8 50, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
@.str.1 = constant [61 x i8] [i8 67, i8 111, i8 110, i8 102, i8 105, i8 103, i8 58, i8 32, i8 98, i8 117, i8 102, i8 102, i8 101, i8 114, i8 61, i8 37, i8 100, i8 75, i8 66, i8 44, i8 32, i8 99, i8 111, i8 110, i8 110, i8 101, i8 99, i8 116, i8 105, i8 111, i8 110, i8 115, i8 61, i8 37, i8 100, i8 44, i8 32, i8 116, i8 105, i8 109, i8 101, i8 111, i8 117, i8 116, i8 61, i8 37, i8 100, i8 109, i8 115, i8 44, i8 32, i8 100, i8 101, i8 98, i8 117, i8 103, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
declare void @func_0()
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  call i32 (ptr, ...) @printf(ptr @.str.0, i32 120, i32 1)
  call i32 (ptr, ...) @printf(ptr @.str.1, i32 0, i32 0, i32 0, i32 0)
  ret i32 0
}

