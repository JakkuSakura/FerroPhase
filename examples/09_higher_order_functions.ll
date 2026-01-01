; ModuleID = '09_higher_order_functions'
source_filename = "09_higher_order_functions"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.09_higher_order_functions.0 = private unnamed_addr constant [21 x i8] c"Generic operations:\0A\00", align 1
@.str.09_higher_order_functions.1 = private unnamed_addr constant [15 x i8] c"\0AConditional:\0A\00", align 1
@.str.09_higher_order_functions.2 = private unnamed_addr constant [6 x i8] c"%lld\0A\00", align 1
@.str.09_higher_order_functions.3 = private unnamed_addr constant [19 x i8] c"\0AClosure factory:\0A\00", align 1
@.str.09_higher_order_functions.4 = private unnamed_addr constant [16 x i8] c"add_10(5) = %s\0A\00", align 1
@.str.09_higher_order_functions.5 = private unnamed_addr constant [10 x i8] c"<unknown>\00", align 1
@.str.09_higher_order_functions.6 = private unnamed_addr constant [18 x i8] c"double(7) = %lld\0A\00", align 1

define internal i64 @__closure0_call({ i64 } %0, i64 %1) {
bb0:
  %alloca_1 = alloca { i64 }, align 8
  %alloca_count_1 = alloca { i64 }, align 8
  %alloca_0 = alloca i64, align 8
  %alloca_count_0 = alloca i64, align 8
  store { i64 } %0, ptr %alloca_count_1, align 8
  %load_4 = load i64, ptr %alloca_count_1, align 8
  %iop_5 = add i64 %1, %load_4
  store i64 %iop_5, ptr %alloca_count_0, align 8
  %load_7 = load i64, ptr %alloca_count_0, align 8
  ret i64 %load_7
}

define internal i64 @__closure1_call({ i8 } %0, i64 %1) {
bb0:
  %alloca_8 = alloca i64, align 8
  %alloca_count_8 = alloca i64, align 8
  %iop_9 = mul i64 %1, 2
  store i64 %iop_9, ptr %alloca_count_8, align 8
  %load_11 = load i64, ptr %alloca_count_8, align 8
  ret i64 %load_11
}

define internal i64 @apply_if(i1 %0, i64 %1, i64 %2, ptr %3) {
bb0:
  %alloca_14 = alloca i64, align 8
  %alloca_count_14 = alloca i64, align 8
  %alloca_12 = alloca i64, align 8
  %alloca_count_12 = alloca i64, align 8
  br i1 %0, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  %call_13 = call i64 %3(i64 %1, i64 %2)
  br label %bb4

bb2:                                              ; preds = %bb0
  store i64 0, ptr %alloca_count_14, align 8
  br label %bb3

bb4:                                              ; preds = %bb1
  %call_16 = call i64 %3(i64 %1, i64 %2)
  store i64 %call_16, ptr %alloca_count_14, align 8
  br label %bb5

bb3:                                              ; preds = %bb5, %bb2
  br i1 %0, label %bb6, label %bb7

bb5:                                              ; preds = %bb4
  br label %bb3

bb6:                                              ; preds = %bb3
  %call_18 = call i64 %3(i64 %1, i64 %2)
  br label %bb9

bb7:                                              ; preds = %bb3
  store i64 0, ptr %alloca_count_12, align 8
  br label %bb8

bb9:                                              ; preds = %bb6
  %call_20 = call i64 %3(i64 %1, i64 %2)
  store i64 %call_20, ptr %alloca_count_12, align 8
  br label %bb10

bb8:                                              ; preds = %bb10, %bb7
  %load_22 = load i64, ptr %alloca_count_12, align 8
  ret i64 %load_22

bb10:                                             ; preds = %bb9
  br label %bb8
}

define internal ptr @make_adder(i64 %0) {
bb0:
  %alloca_23 = alloca ptr, align 8
  %alloca_count_23 = alloca ptr, align 8
  store i64 %0, ptr %alloca_count_23, align 8
  %load_25 = load ptr, ptr %alloca_count_23, align 8
  ret ptr %load_25
}

define i32 @main() {
bb0:
  %alloca_36 = alloca { i8 }, align 8
  %alloca_count_36 = alloca { i8 }, align 8
  %call_26 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  call void @apply(double 1.000000e+01, double 2.000000e+01, ptr @add__spec0)
  br label %bb2

bb2:                                              ; preds = %bb1
  call void @apply(double 1.500000e+00, double 2.500000e+00, ptr @add__spec0)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_29 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.1)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_30 = call i64 @apply_if(i1 true, i64 5, i64 3, ptr @add__spec0)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_31 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.2, i64 %call_30)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_32 = call i64 @apply_if(i1 false, i64 5, i64 3, ptr @add__spec0)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_33 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.2, i64 %call_32)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_34 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.3)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_35 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.4, ptr @.str.09_higher_order_functions.5)
  br label %bb10

bb10:                                             ; preds = %bb9
  store { i8 } zeroinitializer, ptr %alloca_count_36, align 1
  %load_38 = load { i8 }, ptr %alloca_count_36, align 1
  %call_39 = call i64 @__closure1_call({ i8 } %load_38, i64 7)
  br label %bb11

bb11:                                             ; preds = %bb10
  %call_40 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.6, i64 %call_39)
  br label %bb12

bb12:                                             ; preds = %bb11
  ret i32 0
}

declare i32 @printf(ptr, ...)

define internal void @apply(double %0, double %1, ptr %2) {
bb0:
  ret void
}

define internal i64 @add__spec0(i64 %0, i64 %1) {
bb0:
  %alloca_42 = alloca i64, align 8
  %alloca_count_42 = alloca i64, align 8
  %alloca_41 = alloca i64, align 8
  %alloca_count_41 = alloca i64, align 8
  %iop_43 = add i64 %0, %1
  store i64 %iop_43, ptr %alloca_count_42, align 8
  %iop_45 = add i64 %0, %1
  store i64 %iop_45, ptr %alloca_count_41, align 8
  %load_47 = load i64, ptr %alloca_count_41, align 8
  ret i64 %load_47
}
