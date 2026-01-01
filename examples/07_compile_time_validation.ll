; ModuleID = '07_compile_time_validation'
source_filename = "07_compile_time_validation"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.07_compile_time_validation.0 = private unnamed_addr constant [26 x i8] c"sizeof=%llu, fields=%llu\0A\00", align 1
@.str.07_compile_time_validation.1 = private unnamed_addr constant [20 x i8] c"has_a=%d, has_x=%d\0A\00", align 1
@.str.07_compile_time_validation.2 = private unnamed_addr constant [24 x i8] c"size_ok=%d, aligned=%d\0A\00", align 1
@.str.07_compile_time_validation.3 = private unnamed_addr constant [10 x i8] c"optimized\00", align 1
@.str.07_compile_time_validation.4 = private unnamed_addr constant [10 x i8] c"mode: %s\0A\00", align 1

define internal void @main() {
bb0:
  %alloca_0 = alloca i64, align 8
  %alloca_count_0 = alloca i64, align 8
  store i64 24, ptr %alloca_count_0, align 8
  %alloca_2 = alloca i64, align 8
  %alloca_count_2 = alloca i64, align 8
  store i64 3, ptr %alloca_count_2, align 8
  %load_4 = load i64, ptr %alloca_count_0, align 8
  %load_5 = load i64, ptr %alloca_count_2, align 8
  %call_6 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.0, i64 %load_4, i64 %load_5)
  br label %bb1

bb1:                                              ; preds = %bb0
  %alloca_7 = alloca i1, align 1
  %alloca_count_7 = alloca i1, align 1
  store i1 true, ptr %alloca_count_7, align 1
  %alloca_9 = alloca i1, align 1
  %alloca_count_9 = alloca i1, align 1
  store i1 false, ptr %alloca_count_9, align 1
  %load_11 = load i1, ptr %alloca_count_7, align 1
  %zext = zext i1 %load_11 to i32
  %load_13 = load i1, ptr %alloca_count_9, align 1
  %zext1 = zext i1 %load_13 to i32
  %call_15 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.1, i32 %zext, i32 %zext1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %alloca_16 = alloca i1, align 1
  %alloca_count_16 = alloca i1, align 1
  store i1 true, ptr %alloca_count_16, align 1
  %alloca_18 = alloca i1, align 1
  %alloca_count_18 = alloca i1, align 1
  store i1 true, ptr %alloca_count_18, align 1
  %load_20 = load i1, ptr %alloca_count_16, align 1
  %zext2 = zext i1 %load_20 to i32
  %load_22 = load i1, ptr %alloca_count_18, align 1
  %zext3 = zext i1 %load_22 to i32
  %call_24 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.2, i32 %zext2, i32 %zext3)
  br label %bb3

bb3:                                              ; preds = %bb2
  %alloca_25 = alloca ptr, align 8
  %alloca_count_25 = alloca ptr, align 8
  store ptr @.str.07_compile_time_validation.3, ptr %alloca_count_25, align 8
  %load_27 = load ptr, ptr %alloca_count_25, align 8
  %call_28 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.4, ptr %load_27)
  br label %bb4

bb4:                                              ; preds = %bb3
  ret void
}

declare i32 @printf(ptr, ...)
