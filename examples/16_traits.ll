; ModuleID = '16_traits'
source_filename = "16_traits"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_0 = internal constant i1 false, align 1
@.str.16_traits.0 = private unnamed_addr constant [5 x i8] c"%.2\0A\00", align 1
@.str.16_traits.1 = private unnamed_addr constant [3 x i8] c"()\00", align 1

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

define internal double @Rectangle__area(ptr %0) {
bb0:
  %alloca_13 = alloca double, align 8
  %alloca_count_13 = alloca double, align 8
  %load_15 = load double, ptr %0, align 8
  %gep_17 = getelementptr inbounds i8, ptr %0, i64 8
  %load_19 = load double, ptr %gep_17, align 8
  %fop_20 = fmul double %load_15, %load_19
  store double %fop_20, ptr %alloca_count_13, align 8
  %load_22 = load double, ptr %alloca_count_13, align 8
  ret double %load_22
}

define i32 @main() {
bb0:
  %alloca_37 = alloca ptr, align 8
  %alloca_count_37 = alloca ptr, align 8
  %alloca_33 = alloca ptr, align 8
  %alloca_count_33 = alloca ptr, align 8
  %alloca_25 = alloca ptr, align 8
  %alloca_count_25 = alloca ptr, align 8
  %alloca_23 = alloca ptr, align 8
  %alloca_count_23 = alloca ptr, align 8
  store double 5.000000e+00, ptr %alloca_count_23, align 8
  store { double, double } { double 4.000000e+00, double 6.000000e+00 }, ptr %alloca_count_25, align 8
  %load_29 = load ptr, ptr %alloca_count_23, align 8
  call void @opaque__describe(ptr %load_29)
  br label %bb1

bb1:                                              ; preds = %bb0
  %load_31 = load ptr, ptr %alloca_count_25, align 8
  call void @opaque__describe(ptr %load_31)
  br label %bb2

bb2:                                              ; preds = %bb1
  store ptr %alloca_count_23, ptr %alloca_count_33, align 8
  %load_35 = load ptr, ptr %alloca_count_33, align 8
  call void @print_area__mono_692e805f044e08ba_5(ptr %load_35)
  br label %bb3

bb3:                                              ; preds = %bb2
  store ptr %alloca_count_25, ptr %alloca_count_37, align 8
  %load_39 = load ptr, ptr %alloca_count_37, align 8
  call void @print_area__mono_692e805f044e08ba_5(ptr %load_39)
  br label %bb4

bb4:                                              ; preds = %bb3
  ret i32 0
}

define internal void @opaque__describe(ptr %0) {
bb0:
  ret void
}

define internal void @print_area__mono_692e805f044e08ba_5(ptr %0) {
bb0:
  call void @T__area(ptr %0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_42 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.0, ptr @.str.16_traits.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  ret void
}

define internal void @T__area(ptr %0) {
bb0:
  ret void
}

declare i32 @printf(ptr, ...)
