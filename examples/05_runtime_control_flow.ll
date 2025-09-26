; ModuleID = ''
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx12.0.0"

@.str.0 = constant [38 x i8] c"=== Runtime Control Flow Showcase ===\00", align 1
@.str.1 = constant [27 x i8] c"Basic if expressions work:\00", align 1
@.str.2 = constant [24 x i8] c"Value is greater than 5\00", align 1
@.str.3 = constant [19 x i8] c"Value is 5 or less\00", align 1
@.str.4 = constant [39 x i8] [i8 10, i8 68, i8 101, i8 109, i8 111, i8 110, i8 115, i8 116, i8 114, i8 97, i8 116, i8 105, i8 110, i8 103, i8 32, i8 99, i8 111, i8 110, i8 116, i8 114, i8 111, i8 108, i8 32, i8 102, i8 108, i8 111, i8 119, i8 32, i8 115, i8 116, i8 114, i8 117, i8 99, i8 116, i8 117, i8 114, i8 101, i8 58, i8 0], align 1
@.str.5 = constant [40 x i8] c"While loop structure would execute here\00", align 1
@.str.6 = constant [43 x i8] c"Infinite loop structure would execute here\00", align 1
@.str.7 = constant [47 x i8] c"Control flow structures compiled successfully!\00", align 1
declare void @func_0()
declare i32 @puts(ptr)
define i32 @main() {
bb0:
  call i32 @puts(ptr getelementptr inbounds ([38 x i8], ptr @.str.0, i32 0, i32 0))
  br label %bb1
bb1:
  call i32 @puts(ptr getelementptr inbounds ([27 x i8], ptr @.str.1, i32 0, i32 0))
  br label %bb2
bb2:
  ret i32 0
bb3:
  call i32 @puts(ptr getelementptr inbounds ([24 x i8], ptr @.str.2, i32 0, i32 0))
  br label %bb6
bb4:
  call i32 @puts(ptr getelementptr inbounds ([19 x i8], ptr @.str.3, i32 0, i32 0))
  br label %bb7
bb5:
  call i32 @puts(ptr getelementptr inbounds ([39 x i8], ptr @.str.4, i32 0, i32 0))
  br label %bb8
bb6:
  br label %bb5
bb7:
  br label %bb5
bb8:
  ret i32 0
bb9:
  call i32 @puts(ptr getelementptr inbounds ([40 x i8], ptr @.str.5, i32 0, i32 0))
  br label %bb12
bb10:
  br label %bb11
bb11:
  ret i32 0
bb12:
  br label %bb11
bb13:
  call i32 @puts(ptr getelementptr inbounds ([43 x i8], ptr @.str.6, i32 0, i32 0))
  br label %bb16
bb14:
  br label %bb15
bb15:
  call i32 @puts(ptr getelementptr inbounds ([47 x i8], ptr @.str.7, i32 0, i32 0))
  br label %bb17
bb16:
  br label %bb15
bb17:
  ret i32 0
}

