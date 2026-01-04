; ModuleID = '09_higher_order_functions'
source_filename = "09_higher_order_functions"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.09_higher_order_functions.0 = private unnamed_addr constant [45 x i8] c"\F0\9F\93\98 Tutorial: 09_higher_order_functions.fp\0A\00", align 1
@.str.09_higher_order_functions.1 = private unnamed_addr constant [81 x i8] c"\F0\9F\A7\AD Focus: Higher-order functions: passing functions as arguments and closures\0A\00", align 1
@.str.09_higher_order_functions.2 = private unnamed_addr constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00", align 1
@.str.09_higher_order_functions.3 = private unnamed_addr constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00", align 1
@.str.09_higher_order_functions.4 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@.str.09_higher_order_functions.5 = private unnamed_addr constant [21 x i8] c"Generic operations:\0A\00", align 1
@.str.09_higher_order_functions.6 = private unnamed_addr constant [15 x i8] c"\0AConditional:\0A\00", align 1
@.str.09_higher_order_functions.7 = private unnamed_addr constant [29 x i8] c"apply_if(true, 5, 3) = %lld\0A\00", align 1
@.str.09_higher_order_functions.8 = private unnamed_addr constant [30 x i8] c"apply_if(false, 5, 3) = %lld\0A\00", align 1
@.str.09_higher_order_functions.9 = private unnamed_addr constant [19 x i8] c"\0AClosure factory:\0A\00", align 1
@.str.09_higher_order_functions.10 = private unnamed_addr constant [18 x i8] c"add_10(5) = %lld\0A\00", align 1
@.str.09_higher_order_functions.11 = private unnamed_addr constant [18 x i8] c"double(7) = %lld\0A\00", align 1
@.str.09_higher_order_functions.12 = private unnamed_addr constant [26 x i8] c"apply(%lld, %lld) = %lld\0A\00", align 1
@.str.09_higher_order_functions.13 = private unnamed_addr constant [20 x i8] c"apply(%f, %f) = %f\0A\00", align 1

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
  %alloca_38 = alloca { i8 }, align 8
  %alloca_count_38 = alloca { i8 }, align 8
  %call_21 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_22 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_23 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.2)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_24 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.3)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_25 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_26 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.5)
  br label %bb6

bb6:                                              ; preds = %bb5
  call void @apply__mono_a7af9f593fdc4675_4(i64 10, i64 20, ptr @add__mono_a7af9f593fdc4675_5)
  br label %bb7

bb7:                                              ; preds = %bb6
  call void @apply__mono_d7ad91e83a08a980_4(double 1.500000e+00, double 2.500000e+00, ptr @add__mono_d7ad91e83a08a980_5)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_29 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.6)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_30 = call i64 @apply_if(i1 true, i64 5, i64 3, ptr @add__spec0)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_31 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.7, i64 %call_30)
  br label %bb11

bb11:                                             ; preds = %bb10
  %call_32 = call i64 @apply_if(i1 false, i64 5, i64 3, ptr @add__spec0)
  br label %bb12

bb12:                                             ; preds = %bb11
  %call_33 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.8, i64 %call_32)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_34 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.9)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_35 = call { i64 } @make_adder(i64 10)
  br label %bb15

bb15:                                             ; preds = %bb14
  %call_36 = call i64 @__closure0_call({ i64 } %call_35, i64 5)
  br label %bb16

bb16:                                             ; preds = %bb15
  %call_37 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.10, i64 %call_36)
  br label %bb17

bb17:                                             ; preds = %bb16
  store { i8 } zeroinitializer, ptr %alloca_count_38, align 1
  %load_40 = load { i8 }, ptr %alloca_count_38, align 1
  %call_41 = call i64 @__closure1_call({ i8 } %load_40, i64 7)
  br label %bb18

bb18:                                             ; preds = %bb17
  %call_42 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.11, i64 %call_41)
  br label %bb19

bb19:                                             ; preds = %bb18
  ret i32 0
}

declare i32 @printf(ptr, ...)

define internal void @apply__mono_a7af9f593fdc4675_4(i64 %0, i64 %1, ptr %2) {
bb0:
  %call_47 = call i64 %2(i64 %0, i64 %1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_48 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.12, i64 %0, i64 %1, i64 %call_47)
  ret void
}

define internal i64 @add__mono_a7af9f593fdc4675_5(i64 %0, i64 %1) {
bb0:
  %alloca_49 = alloca i64, align 8
  %alloca_count_49 = alloca i64, align 8
  %iop_50 = add i64 %0, %1
  store i64 %iop_50, ptr %alloca_count_49, align 8
  %load_52 = load i64, ptr %alloca_count_49, align 8
  ret i64 %load_52
}

define internal void @apply__mono_d7ad91e83a08a980_4(double %0, double %1, ptr %2) {
bb0:
  %call_53 = call double %2(double %0, double %1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_54 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.13, double %0, double %1, double %call_53)
  ret void
}

define internal double @add__mono_d7ad91e83a08a980_5(double %0, double %1) {
bb0:
  %alloca_55 = alloca double, align 8
  %alloca_count_55 = alloca double, align 8
  %fop_56 = fadd double %0, %1
  store double %fop_56, ptr %alloca_count_55, align 8
  %load_58 = load double, ptr %alloca_count_55, align 8
  ret double %load_58
}

define internal i64 @add__spec0(i64 %0, i64 %1) {
bb0:
  %alloca_43 = alloca i64, align 8
  %alloca_count_43 = alloca i64, align 8
  %iop_44 = add i64 %0, %1
  store i64 %iop_44, ptr %alloca_count_43, align 8
  %load_46 = load i64, ptr %alloca_count_43, align 8
  ret i64 %load_46
}
