; ModuleID = '01_const_eval_basics'
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.01_const_eval_basics.0 = constant [41 x i8] [i8 66, i8 117, i8 102, i8 102, i8 101, i8 114, i8 58, i8 32, i8 37, i8 100, i8 75, i8 66, i8 44, i8 32, i8 102, i8 97, i8 99, i8 116, i8 111, i8 114, i8 105, i8 97, i8 108, i8 40, i8 53, i8 41, i8 61, i8 37, i8 100, i8 44, i8 32, i8 108, i8 97, i8 114, i8 103, i8 101, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
@.str.01_const_eval_basics.1 = constant [37 x i8] [i8 67, i8 111, i8 110, i8 102, i8 105, i8 103, i8 58, i8 32, i8 37, i8 100, i8 75, i8 66, i8 32, i8 98, i8 117, i8 102, i8 102, i8 101, i8 114, i8 44, i8 32, i8 37, i8 100, i8 32, i8 99, i8 111, i8 110, i8 110, i8 101, i8 99, i8 116, i8 105, i8 111, i8 110, i8 115, i8 10, i8 0], align 1
@.str.01_const_eval_basics.2 = constant [6 x i8] c"large\00", align 1
@.str.01_const_eval_basics.3 = constant [6 x i8] c"small\00", align 1
@.str.01_const_eval_basics.4 = constant [47 x i8] [i8 67, i8 111, i8 110, i8 115, i8 116, i8 32, i8 98, i8 108, i8 111, i8 99, i8 107, i8 115, i8 58, i8 32, i8 115, i8 105, i8 122, i8 101, i8 61, i8 37, i8 100, i8 44, i8 32, i8 115, i8 116, i8 114, i8 97, i8 116, i8 101, i8 103, i8 121, i8 61, i8 37, i8 115, i8 44, i8 32, i8 109, i8 101, i8 109, i8 111, i8 114, i8 121, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %alloca_0 = alloca ptr, align 8
  %alloca_1 = alloca ptr, align 8
  %alloca_2 = alloca i64, align 8
  store i64 4096, ptr %alloca_2
  %alloca_4 = alloca i64, align 8
  %load_5 = load i64, ptr %alloca_2
  %div_6 = udiv i64 %load_5, 1024
  store i64 %div_6, ptr %alloca_4
  %alloca_8 = alloca i64, align 8
  store i64 120, ptr %alloca_8
  %alloca_10 = alloca i1, align 1
  store i1 1, ptr %alloca_10
  %load_12 = load i64, ptr %alloca_4
  %load_13 = load i64, ptr %alloca_8
  %load_14 = load i1, ptr %alloca_10
  %zext_15 = zext i1 %load_14 to i32
  %call_16 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.0, i64 %load_12, i64 %load_13, i32 %zext_15)
  br label %bb1
bb1:
  %alloca_17 = alloca { i64, i64 }, align 8
  store { i64, i64 } { i64 4096, i64 150 }, ptr %alloca_17
  %alloca_19 = alloca i64, align 8
  %bitcast_20 = bitcast ptr %alloca_17 to ptr
  %load_21 = load i64, ptr %bitcast_20
  %div_22 = udiv i64 %load_21, 1024
  store i64 %div_22, ptr %alloca_19
  %alloca_24 = alloca { i64, i64 }, align 8
  store { i64, i64 } { i64 4096, i64 150 }, ptr %alloca_24
  %load_26 = load i64, ptr %alloca_19
  %bitcast_27 = bitcast ptr %alloca_24 to ptr
  %gep_28 = getelementptr inbounds i8, ptr %bitcast_27, i64 8
  %bitcast_29 = bitcast ptr %gep_28 to ptr
  %load_30 = load i64, ptr %bitcast_29
  %call_31 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.1, i64 %load_26, i64 %load_30)
  br label %bb2
bb2:
  %alloca_32 = alloca i64, align 8
  store i64 3, ptr %alloca_32
  %alloca_34 = alloca i64, align 8
  %mul_35 = mul i64 4096, 2
  store i64 %mul_35, ptr %alloca_34
  %alloca_37 = alloca i64, align 8
  %mul_38 = mul i64 4096, 2
  store i64 %mul_38, ptr %alloca_37
  %load_40 = load i64, ptr %alloca_37
  %alloca_41 = alloca i1, align 1
  store i1 1, ptr %alloca_41
  %load_44 = load i1, ptr %alloca_41
  br i1 %load_44, label %bb3, label %bb4
bb3:
  store ptr @.str.01_const_eval_basics.2, ptr %alloca_0
  br label %bb5
bb4:
  store ptr @.str.01_const_eval_basics.3, ptr %alloca_0
  br label %bb5
bb5:
  %alloca_47 = alloca i1, align 1
  store i1 1, ptr %alloca_47
  %load_50 = load i1, ptr %alloca_47
  br i1 %load_50, label %bb6, label %bb7
bb6:
  store ptr @.str.01_const_eval_basics.2, ptr %alloca_1
  br label %bb8
bb7:
  store ptr @.str.01_const_eval_basics.3, ptr %alloca_1
  br label %bb8
bb8:
  %load_53 = load ptr, ptr %alloca_1
  %alloca_54 = alloca i64, align 8
  store i64 4096, ptr %alloca_54
  %alloca_56 = alloca i64, align 8
  store i64 150, ptr %alloca_56
  %alloca_58 = alloca i64, align 8
  %load_59 = load i64, ptr %alloca_54
  %load_60 = load i64, ptr %alloca_56
  %mul_61 = mul i64 %load_59, %load_60
  store i64 %mul_61, ptr %alloca_58
  %alloca_63 = alloca i64, align 8
  store i64 4096, ptr %alloca_63
  %alloca_65 = alloca i64, align 8
  store i64 150, ptr %alloca_65
  %alloca_67 = alloca i64, align 8
  %load_68 = load i64, ptr %alloca_63
  %load_69 = load i64, ptr %alloca_65
  %mul_70 = mul i64 %load_68, %load_69
  store i64 %mul_70, ptr %alloca_67
  %alloca_72 = alloca i64, align 8
  %load_73 = load i64, ptr %alloca_32
  %mul_74 = mul i64 %load_73, 0
  store i64 %mul_74, ptr %alloca_72
  %alloca_76 = alloca i64, align 8
  %load_77 = load i64, ptr %alloca_72
  store i64 %load_77, ptr %alloca_76
  %load_79 = load i64, ptr %alloca_76
  %call_80 = call i32 (ptr, ...) @printf(ptr @.str.01_const_eval_basics.4, i64 %load_40, ptr %load_53, i64 %load_79)
  br label %bb9
bb9:
  ret i32 0
}

