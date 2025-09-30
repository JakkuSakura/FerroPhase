; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [11 x i8] c"FerroPhase\00", align 1
@.str.1 = constant [6 x i8] c"0.1.0\00", align 1
@.str.2 = constant [20 x i8] [i8 110, i8 97, i8 109, i8 101, i8 61, i8 39, i8 37, i8 115, i8 39, i8 32, i8 108, i8 101, i8 110, i8 61, i8 37, i8 108, i8 108, i8 117, i8 10, i8 0], align 1
@.str.3 = constant [23 x i8] [i8 118, i8 101, i8 114, i8 115, i8 105, i8 111, i8 110, i8 61, i8 39, i8 37, i8 115, i8 39, i8 32, i8 108, i8 101, i8 110, i8 61, i8 37, i8 108, i8 108, i8 117, i8 10, i8 0], align 1
@.str.4 = constant [19 x i8] [i8 101, i8 109, i8 112, i8 116, i8 121, i8 61, i8 37, i8 100, i8 44, i8 32, i8 108, i8 111, i8 110, i8 103, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
@.str.5 = constant [18 x i8] c"FerroPhase v0.1.0\00", align 1
@.str.6 = constant [13 x i8] [i8 98, i8 97, i8 110, i8 110, i8 101, i8 114, i8 61, i8 39, i8 37, i8 115, i8 39, i8 10, i8 0], align 1
@.str.7 = constant [18 x i8] [i8 98, i8 117, i8 102, i8 102, i8 101, i8 114, i8 95, i8 115, i8 105, i8 122, i8 101, i8 61, i8 37, i8 108, i8 108, i8 117, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %alloca_0 = alloca i64, align 8
  %alloca_1 = alloca i32, align 4
  %alloca_2 = alloca ptr, align 8
  store ptr @.str.0, ptr %alloca_2
  %alloca_4 = alloca ptr, align 8
  %load_5 = load ptr, ptr %alloca_2
  store ptr %load_5, ptr %alloca_4
  %alloca_7 = alloca ptr, align 8
  store ptr @.str.1, ptr %alloca_7
  %alloca_9 = alloca ptr, align 8
  %load_10 = load ptr, ptr %alloca_7
  store ptr %load_10, ptr %alloca_9
  %alloca_12 = alloca i64, align 8
  store i64 10, ptr %alloca_12
  %alloca_14 = alloca i64, align 8
  %load_15 = load i64, ptr %alloca_12
  store i64 %load_15, ptr %alloca_14
  %alloca_17 = alloca i64, align 8
  store i64 5, ptr %alloca_17
  %alloca_19 = alloca i64, align 8
  %load_20 = load i64, ptr %alloca_17
  store i64 %load_20, ptr %alloca_19
  %alloca_22 = alloca ptr, align 8
  %load_23 = load ptr, ptr %alloca_4
  %load_24 = load i64, ptr %alloca_14
  call i32 (ptr, ...) @printf(ptr @.str.2, ptr %load_23, i64 %load_24)
  br label %bb1
bb1:
  %alloca_26 = alloca ptr, align 8
  %load_27 = load ptr, ptr %alloca_9
  %load_28 = load i64, ptr %alloca_19
  call i32 (ptr, ...) @printf(ptr @.str.3, ptr %load_27, i64 %load_28)
  br label %bb2
bb2:
  %alloca_30 = alloca i64, align 8
  store i64 0, ptr %alloca_30
  %alloca_32 = alloca i1, align 1
  %load_33 = load i64, ptr %alloca_14
  %load_34 = load i64, ptr %alloca_30
  %icmp_35 = icmp eq i64 %load_33, %load_34
  store i1 %icmp_35, ptr %alloca_32
  %alloca_37 = alloca i1, align 1
  %load_38 = load i1, ptr %alloca_32
  store i1 %load_38, ptr %alloca_37
  %alloca_40 = alloca i64, align 8
  store i64 5, ptr %alloca_40
  %alloca_42 = alloca i1, align 1
  %load_43 = load i64, ptr %alloca_14
  %load_44 = load i64, ptr %alloca_40
  %icmp_45 = icmp sgt i64 %load_43, %load_44
  store i1 %icmp_45, ptr %alloca_42
  %alloca_47 = alloca i1, align 1
  %load_48 = load i1, ptr %alloca_42
  store i1 %load_48, ptr %alloca_47
  %alloca_50 = alloca ptr, align 8
  %load_51 = load i1, ptr %alloca_37
  %load_52 = load i1, ptr %alloca_47
  call i32 (ptr, ...) @printf(ptr @.str.4, i1 %load_51, i1 %load_52)
  br label %bb3
bb3:
  %alloca_54 = alloca ptr, align 8
  store ptr @.str.5, ptr %alloca_54
  %alloca_56 = alloca ptr, align 8
  %load_57 = load ptr, ptr %alloca_54
  store ptr %load_57, ptr %alloca_56
  %alloca_59 = alloca ptr, align 8
  %load_60 = load ptr, ptr %alloca_56
  call i32 (ptr, ...) @printf(ptr @.str.6, ptr %load_60)
  br label %bb4
bb4:
  %alloca_62 = alloca i64, align 8
  store i64 8, ptr %alloca_62
  %alloca_64 = alloca i1, align 1
  %load_65 = load i64, ptr %alloca_14
  %load_66 = load i64, ptr %alloca_62
  %icmp_67 = icmp sgt i64 %load_65, %load_66
  store i1 %icmp_67, ptr %alloca_64
  %load_69 = load i1, ptr %alloca_64
  br i1 %load_69, label %bb5, label %bb6
bb5:
  %alloca_70 = alloca i64, align 8
  store i64 256, ptr %alloca_70
  %load_72 = load i64, ptr %alloca_70
  store i64 %load_72, ptr %alloca_0
  br label %bb7
bb6:
  %alloca_74 = alloca i64, align 8
  store i64 128, ptr %alloca_74
  %load_76 = load i64, ptr %alloca_74
  store i64 %load_76, ptr %alloca_0
  br label %bb7
bb7:
  %alloca_78 = alloca i64, align 8
  %load_79 = load i64, ptr %alloca_0
  store i64 %load_79, ptr %alloca_78
  %alloca_81 = alloca ptr, align 8
  %load_82 = load i64, ptr %alloca_78
  call i32 (ptr, ...) @printf(ptr @.str.7, i64 %load_82)
  br label %bb8
bb8:
  store i32 0, ptr %alloca_1
  ret i32 0
}

