; ModuleID = '11_specialization_basics'
source_filename = "11_specialization_basics"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.11_specialization_basics.0 = private unnamed_addr constant [26 x i8] c"specialized result: %lld\0A\00", align 1
@.str.11_specialization_basics.1 = private unnamed_addr constant [24 x i8] c"specialized result: %f\0A\00", align 1

define i32 @main() {
bb0:
  %call_0 = call i64 @pipeline__mono_a7af9f593fdc4675_2(i64 10, i64 20)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_1 = call double @pipeline__mono_d7ad91e83a08a980_2(double 1.500000e+00, double 2.500000e+00)
  br label %bb2

bb2:                                              ; preds = %bb1
  ret i32 0
}

define internal i64 @pipeline__mono_a7af9f593fdc4675_2(i64 %0, i64 %1) {
bb0:
  %alloca_10 = alloca i64, align 8
  %alloca_count_10 = alloca i64, align 8
  %call_11 = call i64 @add__mono_a7af9f593fdc4675_0(i64 %0, i64 %1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_12 = call i64 @double__mono_a7af9f593fdc4675_1(i64 %call_11)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_13 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.0, i64 %call_12)
  br label %bb3

bb3:                                              ; preds = %bb2
  store i64 %call_12, ptr %alloca_count_10, align 8
  %load_15 = load i64, ptr %alloca_count_10, align 8
  ret i64 %load_15
}

define internal double @pipeline__mono_d7ad91e83a08a980_2(double %0, double %1) {
bb0:
  %alloca_24 = alloca double, align 8
  %alloca_count_24 = alloca double, align 8
  %call_25 = call double @add__mono_d7ad91e83a08a980_0(double %0, double %1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_26 = call double @double__mono_d7ad91e83a08a980_1(double %call_25)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_27 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.1, double %call_26)
  br label %bb3

bb3:                                              ; preds = %bb2
  store double %call_26, ptr %alloca_count_24, align 8
  %load_29 = load double, ptr %alloca_count_24, align 8
  ret double %load_29
}

define internal i64 @add__mono_a7af9f593fdc4675_0(i64 %0, i64 %1) {
bb0:
  %alloca_2 = alloca i64, align 8
  %alloca_count_2 = alloca i64, align 8
  %iop_3 = add i64 %0, %1
  store i64 %iop_3, ptr %alloca_count_2, align 8
  %load_5 = load i64, ptr %alloca_count_2, align 8
  ret i64 %load_5
}

define internal i64 @double__mono_a7af9f593fdc4675_1(i64 %0) {
bb0:
  %alloca_6 = alloca i64, align 8
  %alloca_count_6 = alloca i64, align 8
  %iop_7 = add i64 %0, %0
  store i64 %iop_7, ptr %alloca_count_6, align 8
  %load_9 = load i64, ptr %alloca_count_6, align 8
  ret i64 %load_9
}

declare i32 @printf(ptr, ...)

define internal double @add__mono_d7ad91e83a08a980_0(double %0, double %1) {
bb0:
  %alloca_16 = alloca double, align 8
  %alloca_count_16 = alloca double, align 8
  %fop_17 = fadd double %0, %1
  store double %fop_17, ptr %alloca_count_16, align 8
  %load_19 = load double, ptr %alloca_count_16, align 8
  ret double %load_19
}

define internal double @double__mono_d7ad91e83a08a980_1(double %0) {
bb0:
  %alloca_20 = alloca double, align 8
  %alloca_count_20 = alloca double, align 8
  %fop_21 = fadd double %0, %0
  store double %fop_21, ptr %alloca_count_20, align 8
  %load_23 = load double, ptr %alloca_count_20, align 8
  ret double %load_23
}
