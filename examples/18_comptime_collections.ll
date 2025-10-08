; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@global_0 = constant i64 6, align 8
@global_1 = constant i64 16, align 8
@global_2 = constant i64 4, align 8
@.str.0 = constant [34 x i8] [i8 61, i8 61, i8 61, i8 32, i8 67, i8 111, i8 109, i8 112, i8 105, i8 108, i8 101, i8 45, i8 116, i8 105, i8 109, i8 101, i8 32, i8 67, i8 111, i8 108, i8 108, i8 101, i8 99, i8 116, i8 105, i8 111, i8 110, i8 115, i8 32, i8 61, i8 61, i8 61, i8 10, i8 0], align 1
@.str.1 = constant [15 x i8] [i8 86, i8 101, i8 99, i8 32, i8 108, i8 105, i8 116, i8 101, i8 114, i8 97, i8 108, i8 115, i8 58, i8 10, i8 0], align 1
@.str.2 = constant [23 x i8] [i8 32, i8 32, i8 112, i8 114, i8 105, i8 109, i8 101, i8 115, i8 58, i8 32, i8 37, i8 100, i8 32, i8 101, i8 108, i8 101, i8 109, i8 101, i8 110, i8 116, i8 115, i8 10, i8 0], align 1
@.str.3 = constant [28 x i8] [i8 32, i8 32, i8 122, i8 101, i8 114, i8 111, i8 32, i8 98, i8 117, i8 102, i8 102, i8 101, i8 114, i8 58, i8 32, i8 37, i8 100, i8 32, i8 101, i8 108, i8 101, i8 109, i8 101, i8 110, i8 116, i8 115, i8 10, i8 0], align 1
@.str.4 = constant [37 x i8] [i8 10, i8 72, i8 97, i8 115, i8 104, i8 77, i8 97, i8 112, i8 32, i8 108, i8 105, i8 116, i8 101, i8 114, i8 97, i8 108, i8 32, i8 118, i8 105, i8 97, i8 32, i8 72, i8 97, i8 115, i8 104, i8 77, i8 97, i8 112, i8 58, i8 58, i8 102, i8 114, i8 111, i8 109, i8 58, i8 10, i8 0], align 1
@.str.5 = constant [37 x i8] [i8 32, i8 32, i8 116, i8 114, i8 97, i8 99, i8 107, i8 101, i8 100, i8 32, i8 72, i8 84, i8 84, i8 80, i8 32, i8 115, i8 116, i8 97, i8 116, i8 117, i8 115, i8 101, i8 115, i8 58, i8 32, i8 37, i8 100, i8 32, i8 101, i8 110, i8 116, i8 114, i8 105, i8 101, i8 115, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.0)
  br label %bb1
bb1:
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.1)
  br label %bb2
bb2:
  %alloca_5 = alloca i64, align 8
  store i64 6, ptr %alloca_5
  %load_7 = load i64, ptr %alloca_5
  %call_8 = call i32 (ptr, ...) @printf(ptr @.str.2, i64 %load_7)
  br label %bb3
bb3:
  %alloca_9 = alloca i64, align 8
  store i64 16, ptr %alloca_9
  %load_11 = load i64, ptr %alloca_9
  %call_12 = call i32 (ptr, ...) @printf(ptr @.str.3, i64 %load_11)
  br label %bb4
bb4:
  %call_13 = call i32 (ptr, ...) @printf(ptr @.str.4)
  br label %bb5
bb5:
  %alloca_14 = alloca i64, align 8
  store i64 4, ptr %alloca_14
  %load_16 = load i64, ptr %alloca_14
  %call_17 = call i32 (ptr, ...) @printf(ptr @.str.5, i64 %load_16)
  br label %bb6
bb6:
  ret i32 0
}

