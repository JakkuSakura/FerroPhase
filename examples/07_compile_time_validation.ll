; ModuleID = '07_compile_time_validation'
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.07_compile_time_validation.0 = constant [26 x i8] [i8 115, i8 105, i8 122, i8 101, i8 111, i8 102, i8 61, i8 37, i8 108, i8 108, i8 117, i8 44, i8 32, i8 102, i8 105, i8 101, i8 108, i8 100, i8 115, i8 61, i8 37, i8 108, i8 108, i8 117, i8 10, i8 0], align 1
@.str.07_compile_time_validation.1 = constant [20 x i8] [i8 104, i8 97, i8 115, i8 95, i8 97, i8 61, i8 37, i8 100, i8 44, i8 32, i8 104, i8 97, i8 115, i8 95, i8 120, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
@.str.07_compile_time_validation.2 = constant [24 x i8] [i8 115, i8 105, i8 122, i8 101, i8 95, i8 111, i8 107, i8 61, i8 37, i8 100, i8 44, i8 32, i8 97, i8 108, i8 105, i8 103, i8 110, i8 101, i8 100, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
@.str.07_compile_time_validation.3 = constant [10 x i8] c"optimized\00", align 1
@.str.07_compile_time_validation.4 = constant [10 x i8] [i8 109, i8 111, i8 100, i8 101, i8 58, i8 32, i8 37, i8 115, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %alloca_0 = alloca i64, align 8
  store i64 24, ptr %alloca_0
  %alloca_2 = alloca i64, align 8
  store i64 3, ptr %alloca_2
  %load_4 = load i64, ptr %alloca_0
  %load_5 = load i64, ptr %alloca_2
  %call_6 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.0, i64 %load_4, i64 %load_5)
  br label %bb1
bb1:
  %alloca_7 = alloca i1, align 1
  store i1 1, ptr %alloca_7
  %alloca_9 = alloca i1, align 1
  store i1 0, ptr %alloca_9
  %load_11 = load i1, ptr %alloca_7
  %zext_12 = zext i1 %load_11 to i32
  %load_13 = load i1, ptr %alloca_9
  %zext_14 = zext i1 %load_13 to i32
  %call_15 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.1, i32 %zext_12, i32 %zext_14)
  br label %bb2
bb2:
  %alloca_16 = alloca i1, align 1
  store i1 1, ptr %alloca_16
  %alloca_18 = alloca i1, align 1
  store i1 1, ptr %alloca_18
  %load_20 = load i1, ptr %alloca_16
  %zext_21 = zext i1 %load_20 to i32
  %load_22 = load i1, ptr %alloca_18
  %zext_23 = zext i1 %load_22 to i32
  %call_24 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.2, i32 %zext_21, i32 %zext_23)
  br label %bb3
bb3:
  %alloca_25 = alloca ptr, align 8
  store ptr @.str.07_compile_time_validation.3, ptr %alloca_25
  %load_27 = load ptr, ptr %alloca_25
  %call_28 = call i32 (ptr, ...) @printf(ptr @.str.07_compile_time_validation.4, ptr %load_27)
  br label %bb4
bb4:
  ret i32 0
}

