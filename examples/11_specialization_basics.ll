; ModuleID = '11_specialization_basics'
source_filename = "11_specialization_basics"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.11_specialization_basics.0 = private unnamed_addr constant [24 x i8] c"specialized result: %s\0A\00", align 1
@.str.11_specialization_basics.1 = private unnamed_addr constant [10 x i8] c"<unknown>\00", align 1

define internal ptr @add(ptr %0, ptr %1) {
bb0:
  %alloca_1 = alloca ptr, align 8
  %alloca_count_1 = alloca ptr, align 8
  %alloca_0 = alloca ptr, align 8
  %alloca_count_0 = alloca ptr, align 8
  %ptrtoint = ptrtoint ptr %0 to i64
  %ptrtoint1 = ptrtoint ptr %1 to i64
  %iop_2 = add i64 %ptrtoint, %ptrtoint1
  store i64 %iop_2, ptr %alloca_count_1, align 8
  %ptrtoint2 = ptrtoint ptr %0 to i64
  %ptrtoint3 = ptrtoint ptr %1 to i64
  %iop_4 = add i64 %ptrtoint2, %ptrtoint3
  store i64 %iop_4, ptr %alloca_count_0, align 8
  %load_6 = load ptr, ptr %alloca_count_0, align 8
  ret ptr %load_6
}

define internal ptr @double(ptr %0) {
bb0:
  %alloca_8 = alloca ptr, align 8
  %alloca_count_8 = alloca ptr, align 8
  %alloca_7 = alloca ptr, align 8
  %alloca_count_7 = alloca ptr, align 8
  %ptrtoint = ptrtoint ptr %0 to i64
  %ptrtoint1 = ptrtoint ptr %0 to i64
  %iop_9 = add i64 %ptrtoint, %ptrtoint1
  store i64 %iop_9, ptr %alloca_count_8, align 8
  %ptrtoint2 = ptrtoint ptr %0 to i64
  %ptrtoint3 = ptrtoint ptr %0 to i64
  %iop_11 = add i64 %ptrtoint2, %ptrtoint3
  store i64 %iop_11, ptr %alloca_count_7, align 8
  %load_13 = load ptr, ptr %alloca_count_7, align 8
  ret ptr %load_13
}

define internal ptr @pipeline(ptr %0, ptr %1) {
bb0:
  %alloca_14 = alloca ptr, align 8
  %alloca_count_14 = alloca ptr, align 8
  %call_15 = call ptr @add(ptr %0, ptr %1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_16 = call ptr @double(ptr %call_15)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_17 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.0, ptr @.str.11_specialization_basics.1)
  br label %bb3

bb3:                                              ; preds = %bb2
  store ptr %call_16, ptr %alloca_count_14, align 8
  %load_19 = load ptr, ptr %alloca_count_14, align 8
  ret ptr %load_19
}

declare i32 @printf(ptr, ...)

define i32 @main() {
bb0:
  %call_22 = call ptr @pipeline(ptr inttoptr (i64 10 to ptr), ptr inttoptr (i64 20 to ptr))
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_27 = call ptr @pipeline(ptr inttoptr (i64 1 to ptr), ptr inttoptr (i64 2 to ptr))
  br label %bb2

bb2:                                              ; preds = %bb1
  ret i32 0
}
