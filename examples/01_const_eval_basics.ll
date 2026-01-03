; ModuleID = '01_const_eval_basics'
source_filename = "01_const_eval_basics"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.01_const_eval_basics.0 = private unnamed_addr constant [45 x i8] c"Buffer: %lldKB, factorial(5)=%lld, large=%d\0A\00", align 1
@.str.01_const_eval_basics.1 = private unnamed_addr constant [41 x i8] c"Config: %lldKB buffer, %lld connections\0A\00", align 1
@.str.01_const_eval_basics.2 = private unnamed_addr constant [6 x i8] c"large\00", align 1
@.str.01_const_eval_basics.3 = private unnamed_addr constant [51 x i8] c"Const blocks: size=%lld, strategy=%s, memory=%lld\0A\00", align 1

define i32 @main() {
bb0:
  %alloca_49 = alloca i64, align 8
  %alloca_count_49 = alloca i64, align 8
  %alloca_45 = alloca i64, align 8
  %alloca_count_45 = alloca i64, align 8
  %alloca_40 = alloca i64, align 8
  %alloca_count_40 = alloca i64, align 8
  %alloca_38 = alloca i64, align 8
  %alloca_count_38 = alloca i64, align 8
  %alloca_36 = alloca i64, align 8
  %alloca_count_36 = alloca i64, align 8
  %alloca_34 = alloca ptr, align 8
  %alloca_count_34 = alloca ptr, align 8
  %alloca_32 = alloca i64, align 8
  %alloca_count_32 = alloca i64, align 8
  %alloca_30 = alloca i64, align 8
  %alloca_count_30 = alloca i64, align 8
  %alloca_22 = alloca { i64, i64 }, align 8
  %alloca_count_22 = alloca { i64, i64 }, align 8
  %alloca_17 = alloca i64, align 8
  %alloca_count_17 = alloca i64, align 8
  %alloca_15 = alloca { i64, i64 }, align 8
  %alloca_count_15 = alloca { i64, i64 }, align 8
  %alloca_8 = alloca i1, align 1
  %alloca_count_8 = alloca i1, align 1
  %alloca_6 = alloca i64, align 8
  %alloca_count_6 = alloca i64, align 8
  %alloca_2 = alloca i64, align 8
  %alloca_count_2 = alloca i64, align 8
  %alloca_0 = alloca i64, align 8
  %alloca_count_0 = alloca i64, align 8
  store i64 4096, ptr %alloca_count_0, align 8
  %load_3 = load i64, ptr %alloca_count_0, align 8
  %iop_4 = udiv i64 %load_3, 1024
  store i64 %iop_4, ptr %alloca_count_2, align 8
  store i64 120, ptr %alloca_count_6, align 8
  store i1 true, ptr %alloca_count_8, align 1
  %load_10 = load i64, ptr %alloca_count_2, align 8
  %load_11 = load i64, ptr %alloca_count_6, align 8
  %load_12 = load i1, ptr %alloca_count_8, align 1
  %zext = zext i1 %load_12 to i32
  %call_14 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.0, i64 %load_10, i64 %load_11, i32 %zext)
  br label %bb1

bb1:                                              ; preds = %bb0
  store { i64, i64 } { i64 4096, i64 150 }, ptr %alloca_count_15, align 8
  %load_19 = load i64, ptr %alloca_count_15, align 8
  %iop_20 = udiv i64 %load_19, 1024
  store i64 %iop_20, ptr %alloca_count_17, align 8
  store { i64, i64 } { i64 4096, i64 150 }, ptr %alloca_count_22, align 8
  %load_24 = load i64, ptr %alloca_count_17, align 8
  %gep_26 = getelementptr inbounds i8, ptr %alloca_count_22, i64 8
  %load_28 = load i64, ptr %gep_26, align 8
  %call_29 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.1, i64 %load_24, i64 %load_28)
  br label %bb2

bb2:                                              ; preds = %bb1
  store i64 3, ptr %alloca_count_30, align 8
  store i64 8192, ptr %alloca_count_32, align 8
  store ptr @.str.01_const_eval_basics.2, ptr %alloca_count_34, align 8
  store i64 4096, ptr %alloca_count_36, align 8
  store i64 150, ptr %alloca_count_38, align 8
  %load_41 = load i64, ptr %alloca_count_36, align 8
  %load_42 = load i64, ptr %alloca_count_38, align 8
  %iop_43 = mul i64 %load_41, %load_42
  store i64 %iop_43, ptr %alloca_count_40, align 8
  %load_46 = load i64, ptr %alloca_count_30, align 8
  %iop_47 = mul i64 %load_46, 0
  store i64 %iop_47, ptr %alloca_count_45, align 8
  %load_50 = load i64, ptr %alloca_count_45, align 8
  store i64 %load_50, ptr %alloca_count_49, align 8
  %load_52 = load i64, ptr %alloca_count_32, align 8
  %load_53 = load ptr, ptr %alloca_count_34, align 8
  %load_54 = load i64, ptr %alloca_count_49, align 8
  %call_55 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.3, i64 %load_52, ptr %load_53, i64 %load_54)
  br label %bb3

bb3:                                              ; preds = %bb2
  ret i32 0
}

declare i32 @printf(ptr, ...)
