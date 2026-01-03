; ModuleID = '09_higher_order_functions'
source_filename = "09_higher_order_functions"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.09_higher_order_functions.0 = private unnamed_addr constant [21 x i8] c"Generic operations:\0A\00", align 1
@.str.09_higher_order_functions.1 = private unnamed_addr constant [15 x i8] c"\0AConditional:\0A\00", align 1
@.str.09_higher_order_functions.2 = private unnamed_addr constant [6 x i8] c"%lld\0A\00", align 1
@.str.09_higher_order_functions.3 = private unnamed_addr constant [19 x i8] c"\0AClosure factory:\0A\00", align 1
@.str.09_higher_order_functions.4 = private unnamed_addr constant [18 x i8] c"add_10(5) = %lld\0A\00", align 1
@.str.09_higher_order_functions.5 = private unnamed_addr constant [18 x i8] c"double(7) = %lld\0A\00", align 1
@.str.09_higher_order_functions.6 = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1

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
  %alloca_12 = alloca i64, align 8
  %alloca_count_12 = alloca i64, align 8
  br i1 %0, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  %call_13 = call i64 %3(i64 %1, i64 %2)
  store i64 %call_13, ptr %alloca_count_12, align 8
  br label %bb4

bb2:                                              ; preds = %bb0
  store i64 0, ptr %alloca_count_12, align 8
  br label %bb3

bb4:                                              ; preds = %bb1
  br label %bb3

bb3:                                              ; preds = %bb4, %bb2
  %load_16 = load i64, ptr %alloca_count_12, align 8
  ret i64 %load_16
}

define internal { i64 } @make_adder(i64 %0) {
bb0:
  %alloca_17 = alloca { i64 }, align 8
  %alloca_count_17 = alloca { i64 }, align 8
  %insertvalue_18 = insertvalue { i64 } undef, i64 %0, 0
  store { i64 } %insertvalue_18, ptr %alloca_count_17, align 8
  %load_20 = load { i64 }, ptr %alloca_count_17, align 8
  ret { i64 } %load_20
}

define i32 @main() {
bb0:
  %alloca_33 = alloca { i8 }, align 8
  %alloca_count_33 = alloca { i8 }, align 8
  %call_21 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  call void @apply__mono_a7af9f593fdc4675_4(i64 10, i64 20, ptr @add__mono_692e805f044e08ba_5)
  br label %bb2

bb2:                                              ; preds = %bb1
  call void @apply__mono_d7ad91e83a08a980_4(double 1.500000e+00, double 2.500000e+00, ptr @add__mono_692e805f044e08ba_5)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_24 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.1)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_25 = call i64 @apply_if(i1 true, i64 5, i64 3, ptr @add__spec0)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_26 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.2, i64 %call_25)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_27 = call i64 @apply_if(i1 false, i64 5, i64 3, ptr @add__spec0)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_28 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.2, i64 %call_27)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_29 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.3)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_30 = call { i64 } @make_adder(i64 10)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_31 = call i64 @__closure0_call({ i64 } %call_30, i64 5)
  br label %bb11

bb11:                                             ; preds = %bb10
  %call_32 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.4, i64 %call_31)
  br label %bb12

bb12:                                             ; preds = %bb11
  store { i8 } zeroinitializer, ptr %alloca_count_33, align 1
  %load_35 = load { i8 }, ptr %alloca_count_33, align 1
  %call_36 = call i64 @__closure1_call({ i8 } %load_35, i64 7)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_37 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.5, i64 %call_36)
  br label %bb14

bb14:                                             ; preds = %bb13
  ret i32 0
}

declare i32 @printf(ptr, ...)

define internal void @apply__mono_a7af9f593fdc4675_4(i64 %0, i64 %1, ptr %2) {
bb0:
  %call_46 = call i64 %2(i64 %0, i64 %1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_47 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.2, i64 %call_46)
  br label %bb2

bb2:                                              ; preds = %bb1
  ret void
}

define internal ptr @add__mono_692e805f044e08ba_5(ptr %0, ptr %1) {
bb0:
  %alloca_42 = alloca ptr, align 8
  %alloca_count_42 = alloca ptr, align 8
  %ptrtoint = ptrtoint ptr %0 to i64
  %ptrtoint1 = ptrtoint ptr %1 to i64
  %iop_43 = add i64 %ptrtoint, %ptrtoint1
  store i64 %iop_43, ptr %alloca_count_42, align 8
  %load_45 = load ptr, ptr %alloca_count_42, align 8
  ret ptr %load_45
}

define internal void @apply__mono_d7ad91e83a08a980_4(double %0, double %1, ptr %2) {
bb0:
  %call_48 = call double %2(double %0, double %1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_49 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.6, double %call_48)
  br label %bb2

bb2:                                              ; preds = %bb1
  ret void
}

define internal i64 @add__spec0(i64 %0, i64 %1) {
bb0:
  %alloca_38 = alloca i64, align 8
  %alloca_count_38 = alloca i64, align 8
  %iop_39 = add i64 %0, %1
  store i64 %iop_39, ptr %alloca_count_38, align 8
  %load_41 = load i64, ptr %alloca_count_38, align 8
  ret i64 %load_41
}
