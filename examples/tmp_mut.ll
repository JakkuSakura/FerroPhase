; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [15 x i8] [i8 105, i8 110, i8 105, i8 116, i8 105, i8 97, i8 108, i8 58, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.1 = constant [23 x i8] [i8 97, i8 102, i8 116, i8 101, i8 114, i8 32, i8 102, i8 105, i8 114, i8 115, i8 116, i8 32, i8 97, i8 100, i8 100, i8 58, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.2 = constant [25 x i8] [i8 97, i8 102, i8 116, i8 101, i8 114, i8 32, i8 115, i8 101, i8 99, i8 111, i8 110, i8 100, i8 32, i8 115, i8 116, i8 101, i8 112, i8 58, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.3 = constant [13 x i8] [i8 102, i8 105, i8 110, i8 97, i8 108, i8 58, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
declare void @func_0()
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %alloca_0 = alloca i64, align 8
  store i64 1, ptr %alloca_0
  %load_2 = load i64, ptr %alloca_0
  call i32 (ptr, ...) @printf(ptr @.str.0, i64 %load_2)
  br label %bb1
bb1:
  %load_4 = load i64, ptr %alloca_0
  %add_5 = add i64 %load_4, 41
  store i64 %add_5, ptr %alloca_0
  %load_7 = load i64, ptr %alloca_0
  call i32 (ptr, ...) @printf(ptr @.str.1, i64 %load_7)
  br label %bb2
bb2:
  %load_9 = load i64, ptr %alloca_0
  %sub_10 = sub i64 %load_9, 2
  store i64 %sub_10, ptr %alloca_0
  %load_12 = load i64, ptr %alloca_0
  call i32 (ptr, ...) @printf(ptr @.str.2, i64 %load_12)
  br label %bb3
bb3:
  %load_14 = load i64, ptr %alloca_0
  %mul_15 = mul i64 %load_14, 3
  store i64 %mul_15, ptr %alloca_0
  %load_17 = load i64, ptr %alloca_0
  call i32 (ptr, ...) @printf(ptr @.str.3, i64 %load_17)
  br label %bb4
bb4:
  ret i32 0
}

