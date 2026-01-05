; ModuleID = '05_struct_generation'
source_filename = "05_struct_generation"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.05_struct_generation.0 = constant [40 x i8] c"\F0\9F\93\98 Tutorial: 05_struct_generation.fp\0A\00"
@.str.05_struct_generation.1 = constant [62 x i8] c"\F0\9F\A7\AD Focus: Struct generation with compile-time conditionals\0A\00"
@.str.05_struct_generation.2 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.05_struct_generation.3 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.05_struct_generation.4 = constant [2 x i8] c"\0A\00"
@.str.05_struct_generation.5 = constant [5 x i8] c"core\00"
@.str.05_struct_generation.6 = constant [23 x i8] c"base: id=%lld name=%s\0A\00"
@.str.05_struct_generation.7 = constant [8 x i8] c"primary\00"
@.str.05_struct_generation.8 = constant [7 x i8] c"strict\00"
@.str.05_struct_generation.9 = constant [33 x i8] c"config: id=%lld name=%s mode=%s\0A\00"
@.str.05_struct_generation.10 = constant [7 x i8] c"shadow\00"
@.str.05_struct_generation.11 = constant [8 x i8] c"relaxed\00"
@.str.05_struct_generation.12 = constant [39 x i8] c"config clone: id=%lld name=%s mode=%s\0A\00"
@.str.05_struct_generation.13 = constant [21 x i8] c"config fields: %lld\0A\00"
@.str.05_struct_generation.14 = constant [21 x i8] c"config has mode: %d\0A\00"
@.str.05_struct_generation.15 = constant [28 x i8] c"config has max_retries: %d\0A\00"
@.str.05_struct_generation.16 = constant [19 x i8] c"config size: %lld\0A\00"
@.str.05_struct_generation.17 = constant [17 x i8] c"config type: %s\0A\00"
@.str.05_struct_generation.18 = constant [14 x i8] c"struct Config\00"

define i32 @main() {
bb0:
  %alloca_27 = alloca { i64, ptr, ptr }, align 8
  %alloca_count_27 = alloca { i64, ptr, ptr }, align 8
  %alloca_14 = alloca { i64, ptr, ptr }, align 8
  %alloca_count_14 = alloca { i64, ptr, ptr }, align 8
  %alloca_5 = alloca { i64, ptr }, align 8
  %alloca_count_5 = alloca { i64, ptr }, align 8
  %call_0 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.2)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.3)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  store { i64, ptr } { i64 1, ptr @.str.05_struct_generation.5 }, ptr %alloca_count_5, align 8
  %load_8 = load i64, ptr %alloca_count_5, align 8
  %gep_10 = getelementptr inbounds i8, ptr %alloca_count_5, i64 8
  %load_12 = load ptr, ptr %gep_10, align 8
  %call_13 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.6, i64 %load_8, ptr %load_12)
  br label %bb6

bb6:                                              ; preds = %bb5
  store { i64, ptr, ptr } { i64 2, ptr @.str.05_struct_generation.7, ptr @.str.05_struct_generation.8 }, ptr %alloca_count_14, align 8
  %load_17 = load i64, ptr %alloca_count_14, align 8
  %gep_19 = getelementptr inbounds i8, ptr %alloca_count_14, i64 8
  %load_21 = load ptr, ptr %gep_19, align 8
  %gep_23 = getelementptr inbounds i8, ptr %alloca_count_14, i64 16
  %load_25 = load ptr, ptr %gep_23, align 8
  %call_26 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.9, i64 %load_17, ptr %load_21, ptr %load_25)
  br label %bb7

bb7:                                              ; preds = %bb6
  store { i64, ptr, ptr } { i64 3, ptr @.str.05_struct_generation.10, ptr @.str.05_struct_generation.11 }, ptr %alloca_count_27, align 8
  %load_30 = load i64, ptr %alloca_count_27, align 8
  %gep_32 = getelementptr inbounds i8, ptr %alloca_count_27, i64 8
  %load_34 = load ptr, ptr %gep_32, align 8
  %gep_36 = getelementptr inbounds i8, ptr %alloca_count_27, i64 16
  %load_38 = load ptr, ptr %gep_36, align 8
  %call_39 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.12, i64 %load_30, ptr %load_34, ptr %load_38)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_40 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.13, i64 3)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_42 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.14, i32 1)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_44 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.15, i32 0)
  br label %bb11

bb11:                                             ; preds = %bb10
  %call_45 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.16, i64 24)
  br label %bb12

bb12:                                             ; preds = %bb11
  %call_46 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.17, ptr @.str.05_struct_generation.18)
  br label %bb13

bb13:                                             ; preds = %bb12
  ret i32 0
}

declare i32 @printf(ptr, ...)
