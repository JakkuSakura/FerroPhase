; ModuleID = ''
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx12.0.0"

@.str.0 = constant [6 x i8] c"Hello\00", align 1
@.str.1 = constant [2 x i8] c" \00", align 1
@.str.2 = constant [6 x i8] c"World\00", align 1
@.str.3 = constant [1 x i8] c"\00", align 1
@.str.4 = constant [8 x i8] c"Testing\00", align 1
@.str.5 = constant [9 x i8] c"multiple\00", align 1
@.str.6 = constant [10 x i8] c"arguments\00", align 1
@.str.7 = constant [7 x i8] c"Count:\00", align 1
@.str.8 = constant [6 x i8] c"items\00", align 1
@.str.9 = constant [24 x i8] c"Explicit namespace call\00", align 1
@.str.10 = constant [19 x i8] c"This has a newline\00", align 1
@.str.11 = constant [14 x i8] c"This does not\00", align 1
@.str.12 = constant [26 x i8] c" - continued on same line\00", align 1
declare void @func_0()
declare i32 @printf(ptr, ...)
declare i32 @puts(ptr)
define i32 @main() {
bb0:
  call i32 @printf(ptr getelementptr inbounds ([6 x i8], ptr @.str.0, i32 0, i32 0))
  br label %bb1
bb1:
  call i32 @printf(ptr getelementptr inbounds ([2 x i8], ptr @.str.1, i32 0, i32 0))
  br label %bb2
bb2:
  call i32 @printf(ptr getelementptr inbounds ([6 x i8], ptr @.str.2, i32 0, i32 0))
  br label %bb3
bb3:
  call i32 @puts(ptr getelementptr inbounds ([1 x i8], ptr @.str.3, i32 0, i32 0))
  br label %bb4
bb4:
  call i32 @printf(ptr getelementptr inbounds ([8 x i8], ptr @.str.4, i32 0, i32 0), ptr getelementptr inbounds ([9 x i8], ptr @.str.5, i32 0, i32 0), ptr getelementptr inbounds ([10 x i8], ptr @.str.6, i32 0, i32 0))
  br label %bb5
bb5:
  call i32 @puts(ptr getelementptr inbounds ([1 x i8], ptr @.str.3, i32 0, i32 0))
  br label %bb6
bb6:
  call i32 @printf(ptr getelementptr inbounds ([7 x i8], ptr @.str.7, i32 0, i32 0), i64 42, ptr getelementptr inbounds ([6 x i8], ptr @.str.8, i32 0, i32 0))
  br label %bb7
bb7:
  call i32 @puts(ptr getelementptr inbounds ([1 x i8], ptr @.str.3, i32 0, i32 0))
  br label %bb8
bb8:
  call i32 @printf(ptr getelementptr inbounds ([24 x i8], ptr @.str.9, i32 0, i32 0))
  br label %bb9
bb9:
  call i32 @puts(ptr getelementptr inbounds ([1 x i8], ptr @.str.3, i32 0, i32 0))
  br label %bb10
bb10:
  call i32 @puts(ptr getelementptr inbounds ([19 x i8], ptr @.str.10, i32 0, i32 0))
  br label %bb11
bb11:
  call i32 @printf(ptr getelementptr inbounds ([14 x i8], ptr @.str.11, i32 0, i32 0))
  br label %bb12
bb12:
  call i32 @printf(ptr getelementptr inbounds ([26 x i8], ptr @.str.12, i32 0, i32 0))
  br label %bb13
bb13:
  call i32 @puts(ptr getelementptr inbounds ([1 x i8], ptr @.str.3, i32 0, i32 0))
  br label %bb14
bb14:
  ret i32 0
}

