; ModuleID = '11_specialization_basics'
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.11_specialization_basics.0 = constant [4 x i8] [i8 37, i8 100, i8 10, i8 0], align 1
@.str.11_specialization_basics.1 = constant [11 x i8] [i8 99, i8 111, i8 110, i8 115, i8 116, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i64 @add(i64 %arg0, i64 %arg1) {
bb0:
  %alloca_0 = alloca i64, align 8
  %alloca_1 = alloca i64, align 8
  %add_2 = add i64 %arg0, %arg1
  store i64 %add_2, ptr %alloca_1
  %add_4 = add i64 %arg0, %arg1
  store i64 %add_4, ptr %alloca_0
  %load_6 = load i64, ptr %alloca_0
  ret i64 %load_6
}

define i64 @compose(i64 %arg0) {
bb0:
  %alloca_14 = alloca i64, align 8
  %call_15 = call i64 (i64, i64) @add(i64 %arg0, i64 1)
  br label %bb1
bb1:
  %call_16 = call i64 (i64) @double(i64 %call_15)
  br label %bb2
bb2:
  %call_17 = call i64 (i64, i64) @add(i64 %arg0, i64 1)
  br label %bb3
bb3:
  %call_18 = call i64 (i64) @double(i64 %call_17)
  store i64 %call_18, ptr %alloca_14
  br label %bb4
bb4:
  %load_20 = load i64, ptr %alloca_14
  ret i64 %load_20
}

define i64 @double(i64 %arg0) {
bb0:
  %alloca_7 = alloca i64, align 8
  %alloca_8 = alloca i64, align 8
  %mul_9 = mul i64 %arg0, 2
  store i64 %mul_9, ptr %alloca_8
  %mul_11 = mul i64 %arg0, 2
  store i64 %mul_11, ptr %alloca_7
  %load_13 = load i64, ptr %alloca_7
  ret i64 %load_13
}

define i32 @main() {
bb0:
  %call_21 = call i64 (i64, i64) @add(i64 2, i64 3)
  br label %bb1
bb1:
  %call_22 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.0, i64 %call_21)
  br label %bb2
bb2:
  %call_23 = call i64 (i64) @double(i64 5)
  br label %bb3
bb3:
  %call_24 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.0, i64 %call_23)
  br label %bb4
bb4:
  %call_25 = call i64 (i64) @compose(i64 10)
  br label %bb5
bb5:
  %call_26 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.0, i64 %call_25)
  br label %bb6
bb6:
  %alloca_27 = alloca i64, align 8
  store i64 30, ptr %alloca_27
  %load_29 = load i64, ptr %alloca_27
  %call_30 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.1, i64 %load_29)
  br label %bb7
bb7:
  ret i32 0
}

