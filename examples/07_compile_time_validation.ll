; ModuleID = '07_compile_time_validation'
source_filename = "07_compile_time_validation"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.07_compile_time_validation.0 = private unnamed_addr constant [32 x i8] c"data: sizeof=%llu, fields=%llu\0A\00", align 1
@.str.07_compile_time_validation.1 = private unnamed_addr constant [26 x i8] c"data: has_a=%d, has_x=%d\0A\00", align 1
@.str.07_compile_time_validation.2 = private unnamed_addr constant [50 x i8] c"header: sizeof=%llu, fields=%llu, has_version=%d\0A\00", align 1
@.str.07_compile_time_validation.3 = private unnamed_addr constant [12 x i8] c"struct Data\00", align 1
@.str.07_compile_time_validation.4 = private unnamed_addr constant [5 x i8] c"i64\0A\00", align 1
@.str.07_compile_time_validation.5 = private unnamed_addr constant [4 x i8] c"u8\0A\00", align 1
@.str.07_compile_time_validation.6 = private unnamed_addr constant [38 x i8] c"types: data='%s' a='%s' version='%s'\0A\00", align 1
@.str.07_compile_time_validation.7 = private unnamed_addr constant [24 x i8] c"data has to_string: %d\0A\00", align 1
@.str.07_compile_time_validation.8 = private unnamed_addr constant [64 x i8] c"layout: data_ok=%d, header_ok=%d, total_ok=%d, total_size=%llu\0A\00", align 1

define i32 @main() {
bb0:
  %alloca_44 = alloca i64, align 8
  %alloca_count_44 = alloca i64, align 8
  %alloca_42 = alloca i1, align 1
  %alloca_count_42 = alloca i1, align 1
  %alloca_40 = alloca i1, align 1
  %alloca_count_40 = alloca i1, align 1
  %alloca_38 = alloca i1, align 1
  %alloca_count_38 = alloca i1, align 1
  %alloca_34 = alloca i1, align 1
  %alloca_count_34 = alloca i1, align 1
  %alloca_28 = alloca ptr, align 8
  %alloca_count_28 = alloca ptr, align 8
  %alloca_26 = alloca ptr, align 8
  %alloca_count_26 = alloca ptr, align 8
  %alloca_24 = alloca ptr, align 8
  %alloca_count_24 = alloca ptr, align 8
  %alloca_18 = alloca i1, align 1
  %alloca_count_18 = alloca i1, align 1
  %alloca_16 = alloca i64, align 8
  %alloca_count_16 = alloca i64, align 8
  %alloca_14 = alloca i64, align 8
  %alloca_count_14 = alloca i64, align 8
  %alloca_9 = alloca i1, align 1
  %alloca_count_9 = alloca i1, align 1
  %alloca_7 = alloca i1, align 1
  %alloca_count_7 = alloca i1, align 1
  %alloca_2 = alloca i64, align 8
  %alloca_count_2 = alloca i64, align 8
  %alloca_0 = alloca i64, align 8
  %alloca_count_0 = alloca i64, align 8
  store i64 24, ptr %alloca_count_0, align 8
  store i64 3, ptr %alloca_count_2, align 8
  %load_4 = load i64, ptr %alloca_count_0, align 8
  %load_5 = load i64, ptr %alloca_count_2, align 8
  %call_6 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.0, i64 %load_4, i64 %load_5)
  store i1 true, ptr %alloca_count_7, align 1
  store i1 false, ptr %alloca_count_9, align 1
  %load_11 = load i1, ptr %alloca_count_7, align 1
  %load_12 = load i1, ptr %alloca_count_9, align 1
  %call_13 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.1, i1 %load_11, i1 %load_12)
  store i64 32, ptr %alloca_count_14, align 8
  store i64 4, ptr %alloca_count_16, align 8
  store i1 true, ptr %alloca_count_18, align 1
  %load_20 = load i64, ptr %alloca_count_14, align 8
  %load_21 = load i64, ptr %alloca_count_16, align 8
  %load_22 = load i1, ptr %alloca_count_18, align 1
  %call_23 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.2, i64 %load_20, i64 %load_21, i1 %load_22)
  store ptr @.str.07_compile_time_validation.3, ptr %alloca_count_24, align 8
  store ptr @.str.07_compile_time_validation.4, ptr %alloca_count_26, align 8
  store ptr @.str.07_compile_time_validation.5, ptr %alloca_count_28, align 8
  %load_30 = load ptr, ptr %alloca_count_24, align 8
  %load_31 = load ptr, ptr %alloca_count_26, align 8
  %load_32 = load ptr, ptr %alloca_count_28, align 8
  %call_33 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.6, ptr %load_30, ptr %load_31, ptr %load_32)
  store i1 true, ptr %alloca_count_34, align 1
  %load_36 = load i1, ptr %alloca_count_34, align 1
  %call_37 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.7, i1 %load_36)
  store i1 true, ptr %alloca_count_38, align 1
  store i1 true, ptr %alloca_count_40, align 1
  store i1 true, ptr %alloca_count_42, align 1
  store i64 56, ptr %alloca_count_44, align 8
  %load_46 = load i1, ptr %alloca_count_38, align 1
  %load_47 = load i1, ptr %alloca_count_40, align 1
  %load_48 = load i1, ptr %alloca_count_42, align 1
  %load_49 = load i64, ptr %alloca_count_44, align 8
  %call_50 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.8, i1 %load_46, i1 %load_47, i1 %load_48, i64 %load_49)
  ret i32 0
}

declare i32 @printf(ptr, ...)
