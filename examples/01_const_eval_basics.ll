; ModuleID = '01_const_eval_basics'
source_filename = "01_const_eval_basics"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.01_const_eval_basics.0 = constant [40 x i8] c"\F0\9F\93\98 Tutorial: 01_const_eval_basics.fp\0A\00"
@.str.01_const_eval_basics.1 = constant [82 x i8] c"\F0\9F\A7\AD Focus: Basic const evaluation with compile-time arithmetic and const blocks\0A\00"
@.str.01_const_eval_basics.2 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.01_const_eval_basics.3 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.01_const_eval_basics.4 = constant [2 x i8] c"\0A\00"
@.str.01_const_eval_basics.5 = constant [45 x i8] c"Buffer: %lldKB, factorial(5)=%lld, large=%d\0A\00"
@.str.01_const_eval_basics.6 = constant [41 x i8] c"Config: %lldKB buffer, %lld connections\0A\00"
@.str.01_const_eval_basics.7 = constant [6 x i8] c"large\00"
@.str.01_const_eval_basics.8 = constant [51 x i8] c"Const blocks: size=%lld, strategy=%s, memory=%lld\0A\00"

define i32 @main() {
bb0:
  %alloca_54 = alloca i64, align 8
  %alloca_count_54 = alloca i64, align 8
  %alloca_50 = alloca i64, align 8
  %alloca_count_50 = alloca i64, align 8
  %alloca_45 = alloca i64, align 8
  %alloca_count_45 = alloca i64, align 8
  %alloca_43 = alloca i64, align 8
  %alloca_count_43 = alloca i64, align 8
  %alloca_41 = alloca i64, align 8
  %alloca_count_41 = alloca i64, align 8
  %alloca_39 = alloca ptr, align 8
  %alloca_count_39 = alloca ptr, align 8
  %alloca_37 = alloca i64, align 8
  %alloca_count_37 = alloca i64, align 8
  %alloca_35 = alloca i64, align 8
  %alloca_count_35 = alloca i64, align 8
  %alloca_27 = alloca { i64, i64 }, align 8
  %alloca_count_27 = alloca { i64, i64 }, align 8
  %alloca_22 = alloca i64, align 8
  %alloca_count_22 = alloca i64, align 8
  %alloca_20 = alloca { i64, i64 }, align 8
  %alloca_count_20 = alloca { i64, i64 }, align 8
  %alloca_13 = alloca i1, align 1
  %alloca_count_13 = alloca i1, align 1
  %alloca_11 = alloca i64, align 8
  %alloca_count_11 = alloca i64, align 8
  %alloca_7 = alloca i64, align 8
  %alloca_count_7 = alloca i64, align 8
  %alloca_5 = alloca i64, align 8
  %alloca_count_5 = alloca i64, align 8
  %call_0 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.2)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.3)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  store i64 4096, ptr %alloca_count_5, align 8
  %load_8 = load i64, ptr %alloca_count_5, align 8
  %iop_9 = udiv i64 %load_8, 1024
  store i64 %iop_9, ptr %alloca_count_7, align 8
  store i64 120, ptr %alloca_count_11, align 8
  store i1 true, ptr %alloca_count_13, align 1
  %load_15 = load i64, ptr %alloca_count_7, align 8
  %load_16 = load i64, ptr %alloca_count_11, align 8
  %load_17 = load i1, ptr %alloca_count_13, align 1
  %zext = zext i1 %load_17 to i32
  %call_19 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.5, i64 %load_15, i64 %load_16, i32 %zext)
  br label %bb6

bb6:                                              ; preds = %bb5
  store { i64, i64 } { i64 4096, i64 150 }, ptr %alloca_count_20, align 8
  %load_24 = load i64, ptr %alloca_count_20, align 8
  %iop_25 = udiv i64 %load_24, 1024
  store i64 %iop_25, ptr %alloca_count_22, align 8
  store { i64, i64 } { i64 4096, i64 150 }, ptr %alloca_count_27, align 8
  %load_29 = load i64, ptr %alloca_count_22, align 8
  %gep_31 = getelementptr inbounds i8, ptr %alloca_count_27, i64 8
  %load_33 = load i64, ptr %gep_31, align 8
  %call_34 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.6, i64 %load_29, i64 %load_33)
  br label %bb7

bb7:                                              ; preds = %bb6
  store i64 3, ptr %alloca_count_35, align 8
  store i64 8192, ptr %alloca_count_37, align 8
  store ptr @.str.01_const_eval_basics.7, ptr %alloca_count_39, align 8
  store i64 4096, ptr %alloca_count_41, align 8
  store i64 150, ptr %alloca_count_43, align 8
  %load_46 = load i64, ptr %alloca_count_41, align 8
  %load_47 = load i64, ptr %alloca_count_43, align 8
  %iop_48 = mul i64 %load_46, %load_47
  store i64 %iop_48, ptr %alloca_count_45, align 8
  %load_51 = load i64, ptr %alloca_count_35, align 8
  %iop_52 = mul i64 %load_51, 0
  store i64 %iop_52, ptr %alloca_count_50, align 8
  %load_55 = load i64, ptr %alloca_count_50, align 8
  store i64 %load_55, ptr %alloca_count_54, align 8
  %load_57 = load i64, ptr %alloca_count_37, align 8
  %load_58 = load ptr, ptr %alloca_count_39, align 8
  %load_59 = load i64, ptr %alloca_count_54, align 8
  %call_60 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.8, i64 %load_57, ptr %load_58, i64 %load_59)
  br label %bb8

bb8:                                              ; preds = %bb7
  ret i32 0
}

declare i32 @printf(ptr, ...)
