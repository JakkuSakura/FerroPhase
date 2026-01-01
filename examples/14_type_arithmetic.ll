; ModuleID = '14_type_arithmetic'
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.14_type_arithmetic.0 = constant [4 x i8] [i8 37, i8 115, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  ret i32 0
}

define void @print(ptr %arg0) {
bb0:
  %call_0 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.0, ptr %arg0)
  br label %bb1
bb1:
  call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.0, ptr %arg0)
  br label %bb2
bb2:
  ret void
}

