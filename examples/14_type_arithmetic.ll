; ModuleID = '14_type_arithmetic'
source_filename = "14_type_arithmetic"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.14_type_arithmetic.0 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

define internal void @print(ptr %0) {
bb0:
  %call_0 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.0, ptr %0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.0, ptr %0)
  br label %bb2

bb2:                                              ; preds = %bb1
  ret void
}

declare i32 @printf(ptr, ...)

define internal void @main() {
bb0:
  ret void
}
