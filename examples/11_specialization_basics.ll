; ModuleID = '11_specialization_basics'
source_filename = "11_specialization_basics"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.11_specialization_basics.0 = private unnamed_addr constant [6 x i8] c"%lld\0A\00", align 1
@.str.11_specialization_basics.1 = private unnamed_addr constant [13 x i8] c"const: %lld\0A\00", align 1

define internal i64 @add(i64 %0, i64 %1) {
bb0:
  %alloca_0 = alloca i64, align 8
  %alloca_count_0 = alloca i64, align 8
  %alloca_1 = alloca i64, align 8
  %alloca_count_1 = alloca i64, align 8
  %iop_2 = add i64 %0, %1
  store i64 %iop_2, ptr %alloca_count_1, align 8
  %iop_4 = add i64 %0, %1
  store i64 %iop_4, ptr %alloca_count_0, align 8
  %load_6 = load i64, ptr %alloca_count_0, align 8
  ret i64 %load_6
}

define internal i64 @double(i64 %0) {
bb0:
  %alloca_7 = alloca i64, align 8
  %alloca_count_7 = alloca i64, align 8
  %alloca_8 = alloca i64, align 8
  %alloca_count_8 = alloca i64, align 8
  %iop_9 = mul i64 %0, 2
  store i64 %iop_9, ptr %alloca_count_8, align 8
  %iop_11 = mul i64 %0, 2
  store i64 %iop_11, ptr %alloca_count_7, align 8
  %load_13 = load i64, ptr %alloca_count_7, align 8
  ret i64 %load_13
}

define internal i64 @compose(i64 %0) {
bb0:
  %alloca_14 = alloca i64, align 8
  %alloca_count_14 = alloca i64, align 8
  %call_15 = call i64 @add(i64 %0, i64 1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_16 = call i64 @double(i64 %call_15)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_17 = call i64 @add(i64 %0, i64 1)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_18 = call i64 @double(i64 %call_17)
  store i64 %call_18, ptr %alloca_count_14, align 8
  br label %bb4

bb4:                                              ; preds = %bb3
  %load_20 = load i64, ptr %alloca_count_14, align 8
  ret i64 %load_20
}

define internal void @main() {
bb0:
  %call_21 = call i64 @add(i64 2, i64 3)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_22 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.0, i64 %call_21)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_23 = call i64 @double(i64 5)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_24 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.0, i64 %call_23)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_25 = call i64 @compose(i64 10)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_26 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.0, i64 %call_25)
  br label %bb6

bb6:                                              ; preds = %bb5
  %alloca_27 = alloca i64, align 8
  %alloca_count_27 = alloca i64, align 8
  store i64 30, ptr %alloca_count_27, align 8
  %load_29 = load i64, ptr %alloca_count_27, align 8
  %call_30 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.1, i64 %load_29)
  br label %bb7

bb7:                                              ; preds = %bb6
  ret void
}

declare i32 @printf(ptr, ...)
