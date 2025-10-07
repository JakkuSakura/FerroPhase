; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [4 x i8] [i8 37, i8 100, i8 10, i8 0], align 1
@.str.1 = constant [11 x i8] [i8 99, i8 111, i8 110, i8 115, i8 116, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i64 @add(i64 %arg0, i64 %arg1) {
bb0:
  %alloca_0 = alloca i64, align 8
  %add_1 = add i64 %arg0, %arg1
  store i64 %add_1, ptr %alloca_0
  %load_3 = load i64, ptr %alloca_0
  ret i64 %load_3
}

define i64 @double(i64 %arg0) {
bb0:
  %alloca_4 = alloca i64, align 8
  %mul_5 = mul i64 %arg0, 2
  store i64 %mul_5, ptr %alloca_4
  %load_7 = load i64, ptr %alloca_4
  ret i64 %load_7
}

define i64 @compose(i64 %arg0) {
bb0:
  %alloca_8 = alloca i64, align 8
  %call_9 = call i64 (i64, i64) @add(i64 %arg0, i64 1)
  br label %bb1
bb1:
  %call_10 = call i64 (i64) @double(i64 %call_9)
  store i64 %call_10, ptr %alloca_8
  br label %bb2
bb2:
  %load_12 = load i64, ptr %alloca_8
  ret i64 %load_12
}

define i32 @main() {
bb0:
  %call_13 = call i64 (i64, i64) @add(i64 2, i64 3)
  br label %bb1
bb1:
  %call_14 = call i32 (ptr, ...) @printf(ptr @.str.0, i64 %call_13)
  br label %bb2
bb2:
  %call_15 = call i64 (i64) @double(i64 5)
  br label %bb3
bb3:
  %call_16 = call i32 (ptr, ...) @printf(ptr @.str.0, i64 %call_15)
  br label %bb4
bb4:
  %call_17 = call i64 (i64) @compose(i64 10)
  br label %bb5
bb5:
  %call_18 = call i32 (ptr, ...) @printf(ptr @.str.0, i64 %call_17)
  br label %bb6
bb6:
  %alloca_19 = alloca i64, align 8
  store i64 30, ptr %alloca_19
  %load_21 = load i64, ptr %alloca_19
  %call_22 = call i32 (ptr, ...) @printf(ptr @.str.1, i64 %load_21)
  br label %bb7
bb7:
  ret i32 0
}

