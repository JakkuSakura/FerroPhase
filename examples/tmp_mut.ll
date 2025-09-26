; ModuleID = ''
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx12.0.0"

@.str.0 = constant [13 x i8] [i8 105, i8 110, i8 105, i8 116, i8 105, i8 97, i8 108, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.1 = constant [21 x i8] [i8 97, i8 102, i8 116, i8 101, i8 114, i8 32, i8 102, i8 105, i8 114, i8 115, i8 116, i8 32, i8 97, i8 100, i8 100, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.2 = constant [23 x i8] [i8 97, i8 102, i8 116, i8 101, i8 114, i8 32, i8 115, i8 101, i8 99, i8 111, i8 110, i8 100, i8 32, i8 115, i8 116, i8 101, i8 112, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.3 = constant [11 x i8] [i8 102, i8 105, i8 110, i8 97, i8 108, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
declare void @func_0()
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  call i32 @printf(ptr getelementptr inbounds ([13 x i8], ptr @.str.0, i32 0, i32 0), i64 1)
  br label %bb1
bb1:
  call i32 @printf(ptr getelementptr inbounds ([21 x i8], ptr @.str.1, i32 0, i32 0), i64 1)
  br label %bb2
bb2:
  call i32 @printf(ptr getelementptr inbounds ([23 x i8], ptr @.str.2, i32 0, i32 0), i64 1)
  br label %bb3
bb3:
  call i32 @printf(ptr getelementptr inbounds ([11 x i8], ptr @.str.3, i32 0, i32 0), i64 1)
  br label %bb4
bb4:
  ret i32 0
}

