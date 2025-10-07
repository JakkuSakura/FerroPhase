; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [5 x i8] c"warm\00", align 1
@.str.1 = constant [13 x i8] [i8 37, i8 100, i8 194, i8 176, i8 67, i8 32, i8 105, i8 115, i8 32, i8 37, i8 115, i8 10, i8 0], align 1
@.str.2 = constant [8 x i8] c"outdoor\00", align 1
@.str.3 = constant [15 x i8] [i8 83, i8 117, i8 103, i8 103, i8 101, i8 115, i8 116, i8 101, i8 100, i8 58, i8 32, i8 37, i8 115, i8 10, i8 0], align 1
@.str.4 = constant [2 x i8] c"B\00", align 1
@.str.5 = constant [21 x i8] [i8 83, i8 99, i8 111, i8 114, i8 101, i8 32, i8 37, i8 100, i8 32, i8 61, i8 32, i8 103, i8 114, i8 97, i8 100, i8 101, i8 32, i8 37, i8 115, i8 10, i8 0], align 1
@.str.6 = constant [5 x i8] c"high\00", align 1
@.str.7 = constant [16 x i8] [i8 86, i8 97, i8 108, i8 117, i8 101, i8 32, i8 37, i8 100, i8 32, i8 105, i8 115, i8 32, i8 37, i8 115, i8 10, i8 0], align 1
@.str.8 = constant [7 x i8] c"medium\00", align 1
@.str.9 = constant [4 x i8] c"low\00", align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %alloca_0 = alloca ptr, align 8
  %alloca_1 = alloca i64, align 8
  store i64 25, ptr %alloca_1
  %alloca_3 = alloca ptr, align 8
  store ptr @.str.0, ptr %alloca_3
  %load_5 = load i64, ptr %alloca_1
  %load_6 = load ptr, ptr %alloca_3
  %call_7 = call i32 (ptr, ...) @printf(ptr @.str.1, i64 %load_5, ptr %load_6)
  br label %bb1
bb1:
  %alloca_8 = alloca ptr, align 8
  store ptr @.str.2, ptr %alloca_8
  %load_10 = load ptr, ptr %alloca_8
  %call_11 = call i32 (ptr, ...) @printf(ptr @.str.3, ptr %load_10)
  br label %bb2
bb2:
  %alloca_12 = alloca i64, align 8
  store i64 85, ptr %alloca_12
  %alloca_14 = alloca ptr, align 8
  store ptr @.str.4, ptr %alloca_14
  %load_16 = load i64, ptr %alloca_12
  %load_17 = load ptr, ptr %alloca_14
  %call_18 = call i32 (ptr, ...) @printf(ptr @.str.5, i64 %load_16, ptr %load_17)
  br label %bb3
bb3:
  %alloca_19 = alloca i64, align 8
  store i64 42, ptr %alloca_19
  %alloca_21 = alloca i1, align 1
  %load_22 = load i64, ptr %alloca_19
  %icmp_23 = icmp sgt i64 %load_22, 50
  store i1 %icmp_23, ptr %alloca_21
  %load_25 = load i1, ptr %alloca_21
  br i1 %load_25, label %bb4, label %bb5
bb4:
  store ptr @.str.6, ptr %alloca_0
  br label %bb6
bb5:
  %alloca_27 = alloca i1, align 1
  %load_28 = load i64, ptr %alloca_19
  %icmp_29 = icmp sgt i64 %load_28, 25
  store i1 %icmp_29, ptr %alloca_27
  %load_31 = load i1, ptr %alloca_27
  br i1 %load_31, label %bb7, label %bb8
bb6:
  %alloca_32 = alloca ptr, align 8
  %load_33 = load ptr, ptr %alloca_0
  store ptr %load_33, ptr %alloca_32
  %load_35 = load i64, ptr %alloca_19
  %load_36 = load ptr, ptr %alloca_32
  %call_37 = call i32 (ptr, ...) @printf(ptr @.str.7, i64 %load_35, ptr %load_36)
  br label %bb10
bb7:
  store ptr @.str.8, ptr %alloca_0
  br label %bb9
bb8:
  store ptr @.str.9, ptr %alloca_0
  br label %bb9
bb10:
  ret i32 0
bb9:
  br label %bb6
}

