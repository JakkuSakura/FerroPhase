; ModuleID = '02_string_processing'
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.02_string_processing.0 = constant [11 x i8] c"FerroPhase\00", align 1
@.str.02_string_processing.1 = constant [18 x i8] [i8 110, i8 97, i8 109, i8 101, i8 61, i8 39, i8 37, i8 115, i8 39, i8 32, i8 108, i8 101, i8 110, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
@.str.02_string_processing.2 = constant [6 x i8] c"0.1.0\00", align 1
@.str.02_string_processing.3 = constant [21 x i8] [i8 118, i8 101, i8 114, i8 115, i8 105, i8 111, i8 110, i8 61, i8 39, i8 37, i8 115, i8 39, i8 32, i8 108, i8 101, i8 110, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
@.str.02_string_processing.4 = constant [19 x i8] [i8 101, i8 109, i8 112, i8 116, i8 121, i8 61, i8 37, i8 100, i8 44, i8 32, i8 108, i8 111, i8 110, i8 103, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
@.str.02_string_processing.5 = constant [18 x i8] c"FerroPhase v0.1.0\00", align 1
@.str.02_string_processing.6 = constant [13 x i8] [i8 98, i8 97, i8 110, i8 110, i8 101, i8 114, i8 61, i8 39, i8 37, i8 115, i8 39, i8 10, i8 0], align 1
@.str.02_string_processing.7 = constant [16 x i8] [i8 98, i8 117, i8 102, i8 102, i8 101, i8 114, i8 95, i8 115, i8 105, i8 122, i8 101, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %alloca_0 = alloca ptr, align 8
  store ptr @.str.02_string_processing.0, ptr %alloca_0
  %alloca_2 = alloca i64, align 8
  store i64 10, ptr %alloca_2
  %load_4 = load ptr, ptr %alloca_0
  %load_5 = load i64, ptr %alloca_2
  %call_6 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.1, ptr %load_4, i64 %load_5)
  br label %bb1
bb1:
  %alloca_7 = alloca ptr, align 8
  store ptr @.str.02_string_processing.2, ptr %alloca_7
  %alloca_9 = alloca i64, align 8
  store i64 5, ptr %alloca_9
  %load_11 = load ptr, ptr %alloca_7
  %load_12 = load i64, ptr %alloca_9
  %call_13 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.3, ptr %load_11, i64 %load_12)
  br label %bb2
bb2:
  %alloca_14 = alloca i1, align 1
  store i1 0, ptr %alloca_14
  %alloca_16 = alloca i1, align 1
  store i1 1, ptr %alloca_16
  %load_18 = load i1, ptr %alloca_14
  %zext_19 = zext i1 %load_18 to i32
  %load_20 = load i1, ptr %alloca_16
  %zext_21 = zext i1 %load_20 to i32
  %call_22 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.4, i32 %zext_19, i32 %zext_21)
  br label %bb3
bb3:
  %alloca_23 = alloca ptr, align 8
  store ptr @.str.02_string_processing.5, ptr %alloca_23
  %load_25 = load ptr, ptr %alloca_23
  %call_26 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.6, ptr %load_25)
  br label %bb4
bb4:
  %alloca_27 = alloca i64, align 8
  store i64 256, ptr %alloca_27
  %load_29 = load i64, ptr %alloca_27
  %call_30 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.7, i64 %load_29)
  br label %bb5
bb5:
  ret i32 0
}

