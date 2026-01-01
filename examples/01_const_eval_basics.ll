; ModuleID = '01_const_eval_basics'
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.01_const_eval_basics.0 = constant [45 x i8] [i8 66, i8 117, i8 102, i8 102, i8 101, i8 114, i8 58, i8 32, i8 37, i8 108, i8 108, i8 100, i8 75, i8 66, i8 44, i8 32, i8 102, i8 97, i8 99, i8 116, i8 111, i8 114, i8 105, i8 97, i8 108, i8 40, i8 53, i8 41, i8 61, i8 37, i8 108, i8 108, i8 100, i8 44, i8 32, i8 108, i8 97, i8 114, i8 103, i8 101, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
@.str.01_const_eval_basics.1 = constant [41 x i8] [i8 67, i8 111, i8 110, i8 102, i8 105, i8 103, i8 58, i8 32, i8 37, i8 108, i8 108, i8 100, i8 75, i8 66, i8 32, i8 98, i8 117, i8 102, i8 102, i8 101, i8 114, i8 44, i8 32, i8 37, i8 108, i8 108, i8 100, i8 32, i8 99, i8 111, i8 110, i8 110, i8 101, i8 99, i8 116, i8 105, i8 111, i8 110, i8 115, i8 10, i8 0], align 1
@.str.01_const_eval_basics.2 = constant [6 x i8] c"large\00", align 1
@.str.01_const_eval_basics.3 = constant [51 x i8] [i8 67, i8 111, i8 110, i8 115, i8 116, i8 32, i8 98, i8 108, i8 111, i8 99, i8 107, i8 115, i8 58, i8 32, i8 115, i8 105, i8 122, i8 101, i8 61, i8 37, i8 108, i8 108, i8 100, i8 44, i8 32, i8 115, i8 116, i8 114, i8 97, i8 116, i8 101, i8 103, i8 121, i8 61, i8 37, i8 115, i8 44, i8 32, i8 109, i8 101, i8 109, i8 111, i8 114, i8 121, i8 61, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %alloca_0 = alloca i64, align 8
  store i64 4096, ptr %alloca_0
  %alloca_2 = alloca i64, align 8
  %load_3 = load i64, ptr %alloca_0
  %div_4 = udiv i64 %load_3, 1024
  store i64 %div_4, ptr %alloca_2
  %alloca_6 = alloca i64, align 8
  store i64 120, ptr %alloca_6
  %alloca_8 = alloca i1, align 1
  store i1 1, ptr %alloca_8
  %load_10 = load i64, ptr %alloca_2
  %load_11 = load i64, ptr %alloca_6
  %load_12 = load i1, ptr %alloca_8
  %zext_13 = zext i1 %load_12 to i32
  %call_14 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.0, i64 %load_10, i64 %load_11, i32 %zext_13)
  br label %bb1
bb1:
  %alloca_15 = alloca { i64, i64 }, align 8
  store { i64, i64 } { i64 4096, i64 150 }, ptr %alloca_15
  %alloca_17 = alloca i64, align 8
  %bitcast_18 = bitcast ptr %alloca_15 to ptr
  %load_19 = load i64, ptr %bitcast_18
  %div_20 = udiv i64 %load_19, 1024
  store i64 %div_20, ptr %alloca_17
  %alloca_22 = alloca { i64, i64 }, align 8
  store { i64, i64 } { i64 4096, i64 150 }, ptr %alloca_22
  %load_24 = load i64, ptr %alloca_17
  %bitcast_25 = bitcast ptr %alloca_22 to ptr
  %gep_26 = getelementptr inbounds i8, ptr %bitcast_25, i64 8
  %bitcast_27 = bitcast ptr %gep_26 to ptr
  %load_28 = load i64, ptr %bitcast_27
  %call_29 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.1, i64 %load_24, i64 %load_28)
  br label %bb2
bb2:
  %alloca_30 = alloca i64, align 8
  store i64 3, ptr %alloca_30
  %alloca_32 = alloca i64, align 8
  store i64 8192, ptr %alloca_32
  %alloca_34 = alloca ptr, align 8
  store ptr @.str.01_const_eval_basics.2, ptr %alloca_34
  %alloca_36 = alloca i64, align 8
  store i64 4096, ptr %alloca_36
  %alloca_38 = alloca i64, align 8
  store i64 150, ptr %alloca_38
  %alloca_40 = alloca i64, align 8
  %load_41 = load i64, ptr %alloca_36
  %load_42 = load i64, ptr %alloca_38
  %mul_43 = mul i64 %load_41, %load_42
  store i64 %mul_43, ptr %alloca_40
  %alloca_45 = alloca i64, align 8
  store i64 4096, ptr %alloca_45
  %alloca_47 = alloca i64, align 8
  store i64 150, ptr %alloca_47
  %alloca_49 = alloca i64, align 8
  %load_50 = load i64, ptr %alloca_45
  %load_51 = load i64, ptr %alloca_47
  %mul_52 = mul i64 %load_50, %load_51
  store i64 %mul_52, ptr %alloca_49
  %alloca_54 = alloca i64, align 8
  %load_55 = load i64, ptr %alloca_30
  %mul_56 = mul i64 %load_55, 0
  store i64 %mul_56, ptr %alloca_54
  %alloca_58 = alloca i64, align 8
  %load_59 = load i64, ptr %alloca_54
  store i64 %load_59, ptr %alloca_58
  %load_61 = load i64, ptr %alloca_32
  %load_62 = load ptr, ptr %alloca_34
  %load_63 = load i64, ptr %alloca_58
  %call_64 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.3, i64 %load_61, ptr %load_62, i64 %load_63)
  br label %bb3
bb3:
  ret i32 0
}

