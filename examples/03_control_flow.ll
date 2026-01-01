; ModuleID = '03_control_flow'
source_filename = "03_control_flow"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.03_control_flow.0 = private unnamed_addr constant [5 x i8] c"warm\00", align 1
@.str.03_control_flow.1 = private unnamed_addr constant [15 x i8] c"%lld\C2\B0C is %s\0A\00", align 1
@.str.03_control_flow.2 = private unnamed_addr constant [8 x i8] c"outdoor\00", align 1
@.str.03_control_flow.3 = private unnamed_addr constant [15 x i8] c"Suggested: %s\0A\00", align 1
@.str.03_control_flow.4 = private unnamed_addr constant [2 x i8] c"B\00", align 1
@.str.03_control_flow.5 = private unnamed_addr constant [23 x i8] c"Score %lld = grade %s\0A\00", align 1
@.str.03_control_flow.6 = private unnamed_addr constant [5 x i8] c"high\00", align 1
@.str.03_control_flow.7 = private unnamed_addr constant [18 x i8] c"Value %lld is %s\0A\00", align 1
@.str.03_control_flow.8 = private unnamed_addr constant [7 x i8] c"medium\00", align 1
@.str.03_control_flow.9 = private unnamed_addr constant [4 x i8] c"low\00", align 1

define i32 @main() {
bb0:
  %alloca_32 = alloca ptr, align 8
  %alloca_count_32 = alloca ptr, align 8
  %alloca_27 = alloca i1, align 1
  %alloca_count_27 = alloca i1, align 1
  %alloca_21 = alloca i1, align 1
  %alloca_count_21 = alloca i1, align 1
  %alloca_19 = alloca i64, align 8
  %alloca_count_19 = alloca i64, align 8
  %alloca_14 = alloca ptr, align 8
  %alloca_count_14 = alloca ptr, align 8
  %alloca_12 = alloca i64, align 8
  %alloca_count_12 = alloca i64, align 8
  %alloca_8 = alloca ptr, align 8
  %alloca_count_8 = alloca ptr, align 8
  %alloca_3 = alloca ptr, align 8
  %alloca_count_3 = alloca ptr, align 8
  %alloca_1 = alloca i64, align 8
  %alloca_count_1 = alloca i64, align 8
  %alloca_0 = alloca ptr, align 8
  %alloca_count_0 = alloca ptr, align 8
  store i64 25, ptr %alloca_count_1, align 8
  store ptr @.str.03_control_flow.0, ptr %alloca_count_3, align 8
  %load_5 = load i64, ptr %alloca_count_1, align 8
  %load_6 = load ptr, ptr %alloca_count_3, align 8
  %call_7 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.1, i64 %load_5, ptr %load_6)
  br label %bb1

bb1:                                              ; preds = %bb0
  store ptr @.str.03_control_flow.2, ptr %alloca_count_8, align 8
  %load_10 = load ptr, ptr %alloca_count_8, align 8
  %call_11 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.3, ptr %load_10)
  br label %bb2

bb2:                                              ; preds = %bb1
  store i64 85, ptr %alloca_count_12, align 8
  store ptr @.str.03_control_flow.4, ptr %alloca_count_14, align 8
  %load_16 = load i64, ptr %alloca_count_12, align 8
  %load_17 = load ptr, ptr %alloca_count_14, align 8
  %call_18 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.5, i64 %load_16, ptr %load_17)
  br label %bb3

bb3:                                              ; preds = %bb2
  store i64 42, ptr %alloca_count_19, align 8
  %load_22 = load i64, ptr %alloca_count_19, align 8
  %icmp_23 = icmp sgt i64 %load_22, 50
  store i1 %icmp_23, ptr %alloca_count_21, align 1
  %load_25 = load i1, ptr %alloca_count_21, align 1
  br i1 %load_25, label %bb4, label %bb5

bb4:                                              ; preds = %bb3
  store ptr @.str.03_control_flow.6, ptr %alloca_count_0, align 8
  br label %bb6

bb5:                                              ; preds = %bb3
  %load_28 = load i64, ptr %alloca_count_19, align 8
  %icmp_29 = icmp sgt i64 %load_28, 25
  store i1 %icmp_29, ptr %alloca_count_27, align 1
  %load_31 = load i1, ptr %alloca_count_27, align 1
  br i1 %load_31, label %bb7, label %bb8

bb6:                                              ; preds = %bb9, %bb4
  %load_33 = load ptr, ptr %alloca_count_0, align 8
  store ptr %load_33, ptr %alloca_count_32, align 8
  %load_35 = load i64, ptr %alloca_count_19, align 8
  %load_36 = load ptr, ptr %alloca_count_32, align 8
  %call_37 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.7, i64 %load_35, ptr %load_36)
  br label %bb10

bb7:                                              ; preds = %bb5
  store ptr @.str.03_control_flow.8, ptr %alloca_count_0, align 8
  br label %bb9

bb8:                                              ; preds = %bb5
  store ptr @.str.03_control_flow.9, ptr %alloca_count_0, align 8
  br label %bb9

bb10:                                             ; preds = %bb6
  ret i32 0

bb9:                                              ; preds = %bb8, %bb7
  br label %bb6
}

declare i32 @printf(ptr, ...)
