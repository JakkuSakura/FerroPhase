; ModuleID = '02_string_processing'
source_filename = "02_string_processing"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.02_string_processing.0 = private unnamed_addr constant [11 x i8] c"FerroPhase\00", align 1
@.str.02_string_processing.1 = private unnamed_addr constant [20 x i8] c"name='%s' len=%llu\0A\00", align 1
@.str.02_string_processing.2 = private unnamed_addr constant [6 x i8] c"0.1.0\00", align 1
@.str.02_string_processing.3 = private unnamed_addr constant [23 x i8] c"version='%s' len=%llu\0A\00", align 1
@.str.02_string_processing.4 = private unnamed_addr constant [19 x i8] c"empty=%d, long=%d\0A\00", align 1
@.str.02_string_processing.5 = private unnamed_addr constant [18 x i8] c"FerroPhase v0.1.0\00", align 1
@.str.02_string_processing.6 = private unnamed_addr constant [13 x i8] c"banner='%s'\0A\00", align 1
@.str.02_string_processing.7 = private unnamed_addr constant [18 x i8] c"buffer_size=%llu\0A\00", align 1

define internal void @main() {
bb0:
  %alloca_0 = alloca ptr, align 8
  %alloca_count_0 = alloca ptr, align 8
  store ptr @.str.02_string_processing.0, ptr %alloca_count_0, align 8
  %alloca_2 = alloca i64, align 8
  %alloca_count_2 = alloca i64, align 8
  store i64 10, ptr %alloca_count_2, align 8
  %load_4 = load ptr, ptr %alloca_count_0, align 8
  %load_5 = load i64, ptr %alloca_count_2, align 8
  %call_6 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.1, ptr %load_4, i64 %load_5)
  br label %bb1

bb1:                                              ; preds = %bb0
  %alloca_7 = alloca ptr, align 8
  %alloca_count_7 = alloca ptr, align 8
  store ptr @.str.02_string_processing.2, ptr %alloca_count_7, align 8
  %alloca_9 = alloca i64, align 8
  %alloca_count_9 = alloca i64, align 8
  store i64 5, ptr %alloca_count_9, align 8
  %load_11 = load ptr, ptr %alloca_count_7, align 8
  %load_12 = load i64, ptr %alloca_count_9, align 8
  %call_13 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.3, ptr %load_11, i64 %load_12)
  br label %bb2

bb2:                                              ; preds = %bb1
  %alloca_14 = alloca i1, align 1
  %alloca_count_14 = alloca i1, align 1
  store i1 false, ptr %alloca_count_14, align 1
  %alloca_16 = alloca i1, align 1
  %alloca_count_16 = alloca i1, align 1
  store i1 true, ptr %alloca_count_16, align 1
  %load_18 = load i1, ptr %alloca_count_14, align 1
  %zext = zext i1 %load_18 to i32
  %load_20 = load i1, ptr %alloca_count_16, align 1
  %zext1 = zext i1 %load_20 to i32
  %call_22 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.4, i32 %zext, i32 %zext1)
  br label %bb3

bb3:                                              ; preds = %bb2
  %alloca_23 = alloca ptr, align 8
  %alloca_count_23 = alloca ptr, align 8
  store ptr @.str.02_string_processing.5, ptr %alloca_count_23, align 8
  %load_25 = load ptr, ptr %alloca_count_23, align 8
  %call_26 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.6, ptr %load_25)
  br label %bb4

bb4:                                              ; preds = %bb3
  %alloca_27 = alloca i64, align 8
  %alloca_count_27 = alloca i64, align 8
  store i64 256, ptr %alloca_count_27, align 8
  %load_29 = load i64, ptr %alloca_count_27, align 8
  %call_30 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.7, i64 %load_29)
  br label %bb5

bb5:                                              ; preds = %bb4
  ret void
}

declare i32 @printf(ptr, ...)
