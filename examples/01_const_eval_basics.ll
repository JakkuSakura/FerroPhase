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
  %alloca_48 = alloca i64, align 8
  %alloca_count_48 = alloca i64, align 8
  %alloca_44 = alloca i64, align 8
  %alloca_count_44 = alloca i64, align 8
  %alloca_39 = alloca i64, align 8
  %alloca_count_39 = alloca i64, align 8
  %alloca_37 = alloca i64, align 8
  %alloca_count_37 = alloca i64, align 8
  %alloca_35 = alloca i64, align 8
  %alloca_count_35 = alloca i64, align 8
  %alloca_33 = alloca ptr, align 8
  %alloca_count_33 = alloca ptr, align 8
  %alloca_31 = alloca i64, align 8
  %alloca_count_31 = alloca i64, align 8
  %alloca_29 = alloca i64, align 8
  %alloca_count_29 = alloca i64, align 8
  %alloca_21 = alloca { i64, i64 }, align 8
  %alloca_count_21 = alloca { i64, i64 }, align 8
  %alloca_16 = alloca i64, align 8
  %alloca_count_16 = alloca i64, align 8
  %alloca_14 = alloca { i64, i64 }, align 8
  %alloca_count_14 = alloca { i64, i64 }, align 8
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
  %call_13 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.0, i64 %load_10, i64 %load_11, i1 %load_12)
  store { i64, i64 } { i64 4096, i64 150 }, ptr %alloca_count_14, align 8
  %load_18 = load i64, ptr %alloca_count_14, align 8
  %iop_19 = udiv i64 %load_18, 1024
  store i64 %iop_19, ptr %alloca_count_16, align 8
  store { i64, i64 } { i64 4096, i64 150 }, ptr %alloca_count_21, align 8
  %load_23 = load i64, ptr %alloca_count_16, align 8
  %gep_25 = getelementptr inbounds i8, ptr %alloca_count_21, i64 8
  %load_27 = load i64, ptr %gep_25, align 8
  %call_28 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.1, i64 %load_23, i64 %load_27)
  store i64 3, ptr %alloca_count_29, align 8
  store i64 8192, ptr %alloca_count_31, align 8
  store ptr @.str.01_const_eval_basics.2, ptr %alloca_count_33, align 8
  store i64 4096, ptr %alloca_count_35, align 8
  store i64 150, ptr %alloca_count_37, align 8
  %load_40 = load i64, ptr %alloca_count_35, align 8
  %load_41 = load i64, ptr %alloca_count_37, align 8
  %iop_42 = mul i64 %load_40, %load_41
  store i64 %iop_42, ptr %alloca_count_39, align 8
  %load_45 = load i64, ptr %alloca_count_29, align 8
  %iop_46 = mul i64 %load_45, 0
  store i64 %iop_46, ptr %alloca_count_44, align 8
  %load_49 = load i64, ptr %alloca_count_44, align 8
  store i64 %load_49, ptr %alloca_count_48, align 8
  %load_51 = load i64, ptr %alloca_count_31, align 8
  %load_52 = load ptr, ptr %alloca_count_33, align 8
  %load_53 = load i64, ptr %alloca_count_48, align 8
  %call_54 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.3, i64 %load_51, ptr %load_52, i64 %load_53)
  ret i32 0
}

declare i32 @printf(ptr, ...)
