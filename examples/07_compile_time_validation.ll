; ModuleID = '07_compile_time_validation'
source_filename = "07_compile_time_validation"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.07_compile_time_validation.0 = constant [46 x i8] c"\F0\9F\93\98 Tutorial: 07_compile_time_validation.fp\0A\00"
@.str.07_compile_time_validation.1 = constant [79 x i8] c"\F0\9F\A7\AD Focus: Compile-time validation using const expressions and introspection\0A\00"
@.str.07_compile_time_validation.2 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.07_compile_time_validation.3 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.07_compile_time_validation.4 = constant [2 x i8] c"\0A\00"
@.str.07_compile_time_validation.5 = constant [32 x i8] c"data: sizeof=%llu, fields=%llu\0A\00"
@.str.07_compile_time_validation.6 = constant [26 x i8] c"data: has_a=%d, has_x=%d\0A\00"
@.str.07_compile_time_validation.7 = constant [50 x i8] c"header: sizeof=%llu, fields=%llu, has_version=%d\0A\00"
@.str.07_compile_time_validation.8 = constant [12 x i8] c"struct Data\00"
@.str.07_compile_time_validation.9 = constant [5 x i8] c"i64\0A\00"
@.str.07_compile_time_validation.10 = constant [4 x i8] c"u8\0A\00"
@.str.07_compile_time_validation.11 = constant [38 x i8] c"types: data='%s' a='%s' version='%s'\0A\00"
@.str.07_compile_time_validation.12 = constant [24 x i8] c"data has to_string: %d\0A\00"
@.str.07_compile_time_validation.13 = constant [64 x i8] c"layout: data_ok=%d, header_ok=%d, total_ok=%d, total_size=%llu\0A\00"

define i32 @main() {
bb0:
  %alloca_53 = alloca i64, align 8
  %alloca_count_53 = alloca i64, align 8
  %alloca_51 = alloca i1, align 1
  %alloca_count_51 = alloca i1, align 1
  %alloca_49 = alloca i1, align 1
  %alloca_count_49 = alloca i1, align 1
  %alloca_47 = alloca i1, align 1
  %alloca_count_47 = alloca i1, align 1
  %alloca_42 = alloca i1, align 1
  %alloca_count_42 = alloca i1, align 1
  %alloca_36 = alloca ptr, align 8
  %alloca_count_36 = alloca ptr, align 8
  %alloca_34 = alloca ptr, align 8
  %alloca_count_34 = alloca ptr, align 8
  %alloca_32 = alloca ptr, align 8
  %alloca_count_32 = alloca ptr, align 8
  %alloca_25 = alloca i1, align 1
  %alloca_count_25 = alloca i1, align 1
  %alloca_23 = alloca i64, align 8
  %alloca_count_23 = alloca i64, align 8
  %alloca_21 = alloca i64, align 8
  %alloca_count_21 = alloca i64, align 8
  %alloca_14 = alloca i1, align 1
  %alloca_count_14 = alloca i1, align 1
  %alloca_12 = alloca i1, align 1
  %alloca_count_12 = alloca i1, align 1
  %alloca_7 = alloca i64, align 8
  %alloca_count_7 = alloca i64, align 8
  %alloca_5 = alloca i64, align 8
  %alloca_count_5 = alloca i64, align 8
  %call_0 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.2)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.3)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  store i64 24, ptr %alloca_count_5, align 8
  store i64 3, ptr %alloca_count_7, align 8
  %load_9 = load i64, ptr %alloca_count_5, align 8
  %load_10 = load i64, ptr %alloca_count_7, align 8
  %call_11 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.5, i64 %load_9, i64 %load_10)
  br label %bb6

bb6:                                              ; preds = %bb5
  store i1 true, ptr %alloca_count_12, align 1
  store i1 false, ptr %alloca_count_14, align 1
  %load_16 = load i1, ptr %alloca_count_12, align 1
  %zext = zext i1 %load_16 to i32
  %load_18 = load i1, ptr %alloca_count_14, align 1
  %zext1 = zext i1 %load_18 to i32
  %call_20 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.6, i32 %zext, i32 %zext1)
  br label %bb7

bb7:                                              ; preds = %bb6
  store i64 32, ptr %alloca_count_21, align 8
  store i64 4, ptr %alloca_count_23, align 8
  store i1 true, ptr %alloca_count_25, align 1
  %load_27 = load i64, ptr %alloca_count_21, align 8
  %load_28 = load i64, ptr %alloca_count_23, align 8
  %load_29 = load i1, ptr %alloca_count_25, align 1
  %zext2 = zext i1 %load_29 to i32
  %call_31 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.7, i64 %load_27, i64 %load_28, i32 %zext2)
  br label %bb8

bb8:                                              ; preds = %bb7
  store ptr @.str.07_compile_time_validation.8, ptr %alloca_count_32, align 8
  store ptr @.str.07_compile_time_validation.9, ptr %alloca_count_34, align 8
  store ptr @.str.07_compile_time_validation.10, ptr %alloca_count_36, align 8
  %load_38 = load ptr, ptr %alloca_count_32, align 8
  %load_39 = load ptr, ptr %alloca_count_34, align 8
  %load_40 = load ptr, ptr %alloca_count_36, align 8
  %call_41 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.11, ptr %load_38, ptr %load_39, ptr %load_40)
  br label %bb9

bb9:                                              ; preds = %bb8
  store i1 true, ptr %alloca_count_42, align 1
  %load_44 = load i1, ptr %alloca_count_42, align 1
  %zext3 = zext i1 %load_44 to i32
  %call_46 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.12, i32 %zext3)
  br label %bb10

bb10:                                             ; preds = %bb9
  store i1 true, ptr %alloca_count_47, align 1
  store i1 true, ptr %alloca_count_49, align 1
  store i1 true, ptr %alloca_count_51, align 1
  store i64 56, ptr %alloca_count_53, align 8
  %load_55 = load i1, ptr %alloca_count_47, align 1
  %zext4 = zext i1 %load_55 to i32
  %load_57 = load i1, ptr %alloca_count_49, align 1
  %zext5 = zext i1 %load_57 to i32
  %load_59 = load i1, ptr %alloca_count_51, align 1
  %zext6 = zext i1 %load_59 to i32
  %load_61 = load i64, ptr %alloca_count_53, align 8
  %call_62 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.13, i32 %zext4, i32 %zext5, i32 %zext6, i64 %load_61)
  br label %bb11

bb11:                                             ; preds = %bb10
  ret i32 0
}

declare i32 @printf(ptr, ...)
