; ModuleID = '16_traits'
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@global_0 = constant i1 0, align 1
declare double @area(ptr)
define i32 @main() {
bb0:
  %alloca_43 = alloca ptr, align 8
  store { double } { double 5.000000e0 }, ptr %alloca_43
  %alloca_45 = alloca ptr, align 8
  store { double, double } { double 4.000000e0, double 6.000000e0 }, ptr %alloca_45
  call void (ptr) @opaque__describe(ptr %alloca_43)
  br label %bb1
bb1:
  call void (ptr) @opaque__describe(ptr %alloca_45)
  br label %bb2
bb2:
  call void (ptr) @print_area(ptr %alloca_43)
  br label %bb3
bb3:
  call void (ptr) @print_area(ptr %alloca_45)
  br label %bb4
bb4:
  ret i32 0
}

define void @opaque__describe(ptr %arg0) {
bb0:
  ret void
}

define void @print_area(ptr %arg0) {
bb0:
  ret void
}

