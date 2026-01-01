; ModuleID = '11_specialization_basics'
source_filename = "11_specialization_basics"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

define i32 @main() {
bb0:
  %call_0 = call ptr @pipeline(double 1.000000e+01, double 2.000000e+01)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_1 = call ptr @pipeline(double 1.500000e+00, double 2.500000e+00)
  br label %bb2

bb2:                                              ; preds = %bb1
  ret i32 0
}

define internal ptr @pipeline(double %0, double %1) {
bb0:
  %alloca_2 = alloca ptr, align 8
  %alloca_count_2 = alloca ptr, align 8
  store i64 0, ptr %alloca_count_2, align 8
  %load_4 = load ptr, ptr %alloca_count_2, align 8
  ret ptr %load_4
}
