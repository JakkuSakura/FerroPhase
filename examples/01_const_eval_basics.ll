; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [41 x i8] [i8 66, i8 117, i8 102, i8 102, i8 101, i8 114, i8 58, i8 32, i8 37, i8 100, i8 75, i8 66, i8 44, i8 32, i8 102, i8 97, i8 99, i8 116, i8 111, i8 114, i8 105, i8 97, i8 108, i8 40, i8 53, i8 41, i8 61, i8 37, i8 100, i8 44, i8 32, i8 108, i8 97, i8 114, i8 103, i8 101, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
@.str.1 = constant [37 x i8] [i8 67, i8 111, i8 110, i8 102, i8 105, i8 103, i8 58, i8 32, i8 37, i8 100, i8 75, i8 66, i8 32, i8 98, i8 117, i8 102, i8 102, i8 101, i8 114, i8 44, i8 32, i8 37, i8 100, i8 32, i8 99, i8 111, i8 110, i8 110, i8 101, i8 99, i8 116, i8 105, i8 111, i8 110, i8 115, i8 10, i8 0], align 1
@.str.2 = constant [6 x i8] c"large\00", align 1
@.str.3 = constant [6 x i8] c"small\00", align 1
@.str.4 = constant [47 x i8] [i8 67, i8 111, i8 110, i8 115, i8 116, i8 32, i8 98, i8 108, i8 111, i8 99, i8 107, i8 115, i8 58, i8 32, i8 115, i8 105, i8 122, i8 101, i8 61, i8 37, i8 100, i8 44, i8 32, i8 115, i8 116, i8 114, i8 97, i8 116, i8 101, i8 103, i8 121, i8 61, i8 37, i8 115, i8 44, i8 32, i8 109, i8 101, i8 109, i8 111, i8 114, i8 121, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %alloca_0 = alloca ptr, align 8
  %alloca_1 = alloca i64, align 8
  store i64 4096, ptr %alloca_1
  %alloca_3 = alloca i64, align 8
  %load_4 = load i64, ptr %alloca_1
  %div_5 = udiv i64 %load_4, 1024
  store i64 %div_5, ptr %alloca_3
  %alloca_7 = alloca i64, align 8
  store i64 120, ptr %alloca_7
  %alloca_9 = alloca i1, align 1
  store i1 1, ptr %alloca_9
  %load_11 = load i64, ptr %alloca_3
  %load_12 = load i64, ptr %alloca_7
  %load_13 = load i1, ptr %alloca_9
  %zext_14 = zext i1 %load_13 to i32
  %call_15 = call i32 (ptr, ...) @printf(ptr @.str.0, i64 %load_11, i64 %load_12, i32 %zext_14)
  br label %bb1
bb1:
  %alloca_16 = alloca { i64, i64 }, align 8
  store { i64, i64 } { i64 4096, i64 150 }, ptr %alloca_16
  %alloca_18 = alloca i64, align 8
  %bitcast_19 = bitcast ptr %alloca_16 to ptr
  %load_20 = load i64, ptr %bitcast_19
  %div_21 = udiv i64 %load_20, 1024
  store i64 %div_21, ptr %alloca_18
  %alloca_23 = alloca { i64, i64 }, align 8
  store { i64, i64 } { i64 4096, i64 150 }, ptr %alloca_23
  %load_25 = load i64, ptr %alloca_18
  %bitcast_26 = bitcast ptr %alloca_23 to ptr
  %gep_27 = getelementptr inbounds i8, ptr %bitcast_26, i64 8
  %bitcast_28 = bitcast ptr %gep_27 to ptr
  %load_29 = load i64, ptr %bitcast_28
  %call_30 = call i32 (ptr, ...) @printf(ptr @.str.1, i64 %load_25, i64 %load_29)
  br label %bb2
bb2:
  %alloca_31 = alloca i64, align 8
  store i64 3, ptr %alloca_31
  %alloca_33 = alloca i64, align 8
  store i64 4096, ptr %alloca_33
  %alloca_35 = alloca i64, align 8
  %load_36 = load i64, ptr %alloca_33
  %mul_37 = mul i64 %load_36, 2
  store i64 %mul_37, ptr %alloca_35
  %load_39 = load i64, ptr %alloca_35
  %alloca_40 = alloca i64, align 8
  store i64 4096, ptr %alloca_40
  %alloca_42 = alloca i1, align 1
  %load_43 = load i64, ptr %alloca_40
  %icmp_44 = icmp sgt i64 %load_43, 2048
  store i1 %icmp_44, ptr %alloca_42
  %load_46 = load i1, ptr %alloca_42
  br i1 %load_46, label %bb3, label %bb4
bb3:
  store ptr @.str.2, ptr %alloca_0
  br label %bb5
bb4:
  store ptr @.str.3, ptr %alloca_0
  br label %bb5
bb5:
  %load_49 = load ptr, ptr %alloca_0
  %alloca_50 = alloca i64, align 8
  store i64 4096, ptr %alloca_50
  %alloca_52 = alloca i64, align 8
  store i64 150, ptr %alloca_52
  %alloca_54 = alloca i64, align 8
  %load_55 = load i64, ptr %alloca_50
  %load_56 = load i64, ptr %alloca_52
  %mul_57 = mul i64 %load_55, %load_56
  store i64 %mul_57, ptr %alloca_54
  %alloca_59 = alloca i64, align 8
  %load_60 = load i64, ptr %alloca_31
  %load_61 = load i64, ptr %alloca_54
  %mul_62 = mul i64 %load_60, %load_61
  store i64 %mul_62, ptr %alloca_59
  %load_64 = load i64, ptr %alloca_59
  %call_65 = call i32 (ptr, ...) @printf(ptr @.str.4, i64 %load_39, ptr %load_49, i64 %load_64)
  br label %bb6
bb6:
  ret i32 0
}

