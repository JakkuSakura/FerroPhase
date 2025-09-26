; ModuleID = ''
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx12.0.0"

@.str.0 = constant [6 x i8] c"Hello\00", align 1
@.str.1 = constant [20 x i8] c"World with newlines\00", align 1
@.str.2 = constant [10 x i8] [i8 77, i8 117, i8 108, i8 116, i8 105, i8 112, i8 108, i8 101, i8 10, i8 0], align 1
@.str.3 = constant [10 x i8] c"arguments\00", align 1
@.str.4 = constant [5 x i8] c"test\00", align 1
@.str.5 = constant [17 x i8] c"This is println!\00", align 1
@.str.6 = constant [27 x i8] c"This is print with newline\00", align 1
declare void @func_0()
declare i32 @puts(ptr)
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  call i32 @puts(ptr getelementptr inbounds ([6 x i8], ptr @.str.0, i32 0, i32 0))
  br label %bb1
bb1:
  call i32 @puts(ptr getelementptr inbounds ([20 x i8], ptr @.str.1, i32 0, i32 0))
  br label %bb2
bb2:
  call i32 @printf(ptr getelementptr inbounds ([10 x i8], ptr @.str.2, i32 0, i32 0), ptr getelementptr inbounds ([10 x i8], ptr @.str.3, i32 0, i32 0), ptr getelementptr inbounds ([5 x i8], ptr @.str.4, i32 0, i32 0))
  br label %bb3
bb3:
  call i32 @puts(ptr getelementptr inbounds ([17 x i8], ptr @.str.5, i32 0, i32 0))
  br label %bb4
bb4:
  call i32 @puts(ptr getelementptr inbounds ([27 x i8], ptr @.str.6, i32 0, i32 0))
  br label %bb5
bb5:
  ret i32 0
}

