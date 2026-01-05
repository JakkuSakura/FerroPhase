; ModuleID = '03_control_flow'
source_filename = "03_control_flow"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.03_control_flow.0 = constant [35 x i8] c"\F0\9F\93\98 Tutorial: 03_control_flow.fp\0A\00"
@.str.03_control_flow.1 = constant [81 x i8] c"\F0\9F\A7\AD Focus: Control flow: if/else expressions with const and runtime evaluation\0A\00"
@.str.03_control_flow.2 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.03_control_flow.3 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.03_control_flow.4 = constant [2 x i8] c"\0A\00"
@.str.03_control_flow.5 = constant [5 x i8] c"warm\00"
@.str.03_control_flow.6 = constant [15 x i8] c"%lld\C2\B0C is %s\0A\00"
@.str.03_control_flow.7 = constant [8 x i8] c"outdoor\00"
@.str.03_control_flow.8 = constant [15 x i8] c"Suggested: %s\0A\00"
@.str.03_control_flow.9 = constant [2 x i8] c"B\00"
@.str.03_control_flow.10 = constant [23 x i8] c"Score %lld = grade %s\0A\00"
@.str.03_control_flow.11 = constant [5 x i8] c"high\00"
@.str.03_control_flow.12 = constant [18 x i8] c"Value %lld is %s\0A\00"
@.str.03_control_flow.13 = constant [7 x i8] c"medium\00"
@.str.03_control_flow.14 = constant [4 x i8] c"low\00"

define i32 @main() {
bb0:
  %alloca_37 = alloca ptr, align 8
  %alloca_count_37 = alloca ptr, align 8
  %alloca_32 = alloca i1, align 1
  %alloca_count_32 = alloca i1, align 1
  %alloca_26 = alloca i1, align 1
  %alloca_count_26 = alloca i1, align 1
  %alloca_24 = alloca i64, align 8
  %alloca_count_24 = alloca i64, align 8
  %alloca_19 = alloca ptr, align 8
  %alloca_count_19 = alloca ptr, align 8
  %alloca_17 = alloca i64, align 8
  %alloca_count_17 = alloca i64, align 8
  %alloca_13 = alloca ptr, align 8
  %alloca_count_13 = alloca ptr, align 8
  %alloca_8 = alloca ptr, align 8
  %alloca_count_8 = alloca ptr, align 8
  %alloca_6 = alloca i64, align 8
  %alloca_count_6 = alloca i64, align 8
  %alloca_0 = alloca ptr, align 8
  %alloca_count_0 = alloca ptr, align 8
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.2)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.3)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_5 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  store i64 25, ptr %alloca_count_6, align 8
  store ptr @.str.03_control_flow.5, ptr %alloca_count_8, align 8
  %load_10 = load i64, ptr %alloca_count_6, align 8
  %load_11 = load ptr, ptr %alloca_count_8, align 8
  %call_12 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.6, i64 %load_10, ptr %load_11)
  br label %bb6

bb6:                                              ; preds = %bb5
  store ptr @.str.03_control_flow.7, ptr %alloca_count_13, align 8
  %load_15 = load ptr, ptr %alloca_count_13, align 8
  %call_16 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.8, ptr %load_15)
  br label %bb7

bb7:                                              ; preds = %bb6
  store i64 85, ptr %alloca_count_17, align 8
  store ptr @.str.03_control_flow.9, ptr %alloca_count_19, align 8
  %load_21 = load i64, ptr %alloca_count_17, align 8
  %load_22 = load ptr, ptr %alloca_count_19, align 8
  %call_23 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.10, i64 %load_21, ptr %load_22)
  br label %bb8

bb8:                                              ; preds = %bb7
  store i64 42, ptr %alloca_count_24, align 8
  %load_27 = load i64, ptr %alloca_count_24, align 8
  %icmp_28 = icmp sgt i64 %load_27, 50
  store i1 %icmp_28, ptr %alloca_count_26, align 1
  %load_30 = load i1, ptr %alloca_count_26, align 1
  br i1 %load_30, label %bb9, label %bb10

bb9:                                              ; preds = %bb8
  store ptr @.str.03_control_flow.11, ptr %alloca_count_0, align 8
  br label %bb11

bb10:                                             ; preds = %bb8
  %load_33 = load i64, ptr %alloca_count_24, align 8
  %icmp_34 = icmp sgt i64 %load_33, 25
  store i1 %icmp_34, ptr %alloca_count_32, align 1
  %load_36 = load i1, ptr %alloca_count_32, align 1
  br i1 %load_36, label %bb12, label %bb13

bb11:                                             ; preds = %bb14, %bb9
  %load_38 = load ptr, ptr %alloca_count_0, align 8
  store ptr %load_38, ptr %alloca_count_37, align 8
  %load_40 = load i64, ptr %alloca_count_24, align 8
  %load_41 = load ptr, ptr %alloca_count_37, align 8
  %call_42 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.12, i64 %load_40, ptr %load_41)
  br label %bb15

bb12:                                             ; preds = %bb10
  store ptr @.str.03_control_flow.13, ptr %alloca_count_0, align 8
  br label %bb14

bb13:                                             ; preds = %bb10
  store ptr @.str.03_control_flow.14, ptr %alloca_count_0, align 8
  br label %bb14

bb15:                                             ; preds = %bb11
  ret i32 0

bb14:                                             ; preds = %bb13, %bb12
  br label %bb11
}

declare i32 @printf(ptr, ...)
