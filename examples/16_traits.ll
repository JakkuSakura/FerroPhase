; ModuleID = '16_traits'
source_filename = "16_traits"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_0 = internal constant i1 false, align 1
@.str.16_traits.0 = private unnamed_addr constant [11 x i8] c"area=%.2f\0A\00", align 1
@.str.16_traits.1 = private unnamed_addr constant [29 x i8] c"\F0\9F\93\98 Tutorial: 16_traits.fp\0A\00", align 1
@.str.16_traits.2 = private unnamed_addr constant [67 x i8] c"\F0\9F\A7\AD Focus: Traits: defining shared behavior with default methods\0A\00", align 1
@.str.16_traits.3 = private unnamed_addr constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00", align 1
@.str.16_traits.4 = private unnamed_addr constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00", align 1
@.str.16_traits.5 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@.str.16_traits.6 = private unnamed_addr constant [6 x i8] c"%.2f\0A\00", align 1

define internal double @Circle__area(ptr %0) {
bb0:
  %alloca_2 = alloca double, align 8
  %alloca_count_2 = alloca double, align 8
  %alloca_1 = alloca double, align 8
  %alloca_count_1 = alloca double, align 8
  %load_4 = load double, ptr %0, align 8
  %fop_5 = fmul double 3.141590e+00, %load_4
  store double %fop_5, ptr %alloca_count_2, align 8
  %load_7 = load double, ptr %alloca_count_2, align 8
  %load_9 = load double, ptr %0, align 8
  %fop_10 = fmul double %load_7, %load_9
  store double %fop_10, ptr %alloca_count_1, align 8
  %load_12 = load double, ptr %alloca_count_1, align 8
  ret double %load_12
}

define internal void @Circle__describe(ptr %0) {
bb0:
  %call_13 = call double @Circle__area(ptr %0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_14 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.0, double %call_13)
  ret void
}

declare i32 @printf(ptr, ...)

define internal double @Rectangle__area(ptr %0) {
bb0:
  %alloca_15 = alloca double, align 8
  %alloca_count_15 = alloca double, align 8
  %load_17 = load double, ptr %0, align 8
  %gep_19 = getelementptr inbounds i8, ptr %0, i64 8
  %load_21 = load double, ptr %gep_19, align 8
  %fop_22 = fmul double %load_17, %load_21
  store double %fop_22, ptr %alloca_count_15, align 8
  %load_24 = load double, ptr %alloca_count_15, align 8
  ret double %load_24
}

define internal void @Rectangle__describe(ptr %0) {
bb0:
  %call_25 = call double @Rectangle__area(ptr %0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_26 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.0, double %call_25)
  ret void
}

define i32 @main() {
bb0:
  %alloca_48 = alloca ptr, align 8
  %alloca_count_48 = alloca ptr, align 8
  %alloca_44 = alloca ptr, align 8
  %alloca_count_44 = alloca ptr, align 8
  %alloca_40 = alloca ptr, align 8
  %alloca_count_40 = alloca ptr, align 8
  %alloca_36 = alloca ptr, align 8
  %alloca_count_36 = alloca ptr, align 8
  %alloca_34 = alloca { double, double }, align 8
  %alloca_count_34 = alloca { double, double }, align 8
  %alloca_32 = alloca { double }, align 8
  %alloca_count_32 = alloca { double }, align 8
  %call_27 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_28 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.2)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_29 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.3)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_30 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.4)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_31 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.5)
  br label %bb5

bb5:                                              ; preds = %bb4
  store { double } { double 5.000000e+00 }, ptr %alloca_count_32, align 8
  store { double, double } { double 4.000000e+00, double 6.000000e+00 }, ptr %alloca_count_34, align 8
  store ptr %alloca_count_32, ptr %alloca_count_36, align 8
  %load_38 = load ptr, ptr %alloca_count_36, align 8
  call void @Circle__describe(ptr %load_38)
  br label %bb6

bb6:                                              ; preds = %bb5
  store ptr %alloca_count_34, ptr %alloca_count_40, align 8
  %load_42 = load ptr, ptr %alloca_count_40, align 8
  call void @Rectangle__describe(ptr %load_42)
  br label %bb7

bb7:                                              ; preds = %bb6
  store ptr %alloca_count_32, ptr %alloca_count_44, align 8
  %load_46 = load ptr, ptr %alloca_count_44, align 8
  call void @print_area__mono_85382269fdc78777_5(ptr %load_46)
  br label %bb8

bb8:                                              ; preds = %bb7
  store ptr %alloca_count_34, ptr %alloca_count_48, align 8
  %load_50 = load ptr, ptr %alloca_count_48, align 8
  call void @print_area__mono_93ff406d3b2fff38_5(ptr %load_50)
  br label %bb9

bb9:                                              ; preds = %bb8
  ret i32 0
}

define internal void @print_area__mono_85382269fdc78777_5(ptr %0) {
bb0:
  %call_52 = call double @Circle__area(ptr %0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_53 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.6, double %call_52)
  br label %bb2

bb2:                                              ; preds = %bb1
  ret void
}

define internal void @print_area__mono_93ff406d3b2fff38_5(ptr %0) {
bb0:
  %call_54 = call double @Rectangle__area(ptr %0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_55 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.6, double %call_54)
  br label %bb2

bb2:                                              ; preds = %bb1
  ret void
}
