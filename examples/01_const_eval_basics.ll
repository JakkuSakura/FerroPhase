; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [45 x i8] [i8 66, i8 117, i8 102, i8 102, i8 101, i8 114, i8 58, i8 32, i8 37, i8 108, i8 108, i8 100, i8 75, i8 66, i8 44, i8 32, i8 102, i8 97, i8 99, i8 116, i8 111, i8 114, i8 105, i8 97, i8 108, i8 40, i8 53, i8 41, i8 61, i8 37, i8 108, i8 108, i8 100, i8 44, i8 32, i8 108, i8 97, i8 114, i8 103, i8 101, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
@.str.1 = constant [41 x i8] [i8 67, i8 111, i8 110, i8 102, i8 105, i8 103, i8 58, i8 32, i8 37, i8 108, i8 108, i8 100, i8 75, i8 66, i8 32, i8 98, i8 117, i8 102, i8 102, i8 101, i8 114, i8 44, i8 32, i8 37, i8 108, i8 108, i8 100, i8 32, i8 99, i8 111, i8 110, i8 110, i8 101, i8 99, i8 116, i8 105, i8 111, i8 110, i8 115, i8 10, i8 0], align 1
@.str.2 = constant [6 x i8] c"large\00", align 1
@.str.3 = constant [6 x i8] c"small\00", align 1
@.str.4 = constant [51 x i8] [i8 67, i8 111, i8 110, i8 115, i8 116, i8 32, i8 98, i8 108, i8 111, i8 99, i8 107, i8 115, i8 58, i8 32, i8 115, i8 105, i8 122, i8 101, i8 61, i8 37, i8 108, i8 108, i8 100, i8 44, i8 32, i8 115, i8 116, i8 114, i8 97, i8 116, i8 101, i8 103, i8 121, i8 61, i8 37, i8 112, i8 44, i8 32, i8 109, i8 101, i8 109, i8 111, i8 114, i8 121, i8 61, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %alloca_0 = alloca ptr, align 8
  %alloca_1 = alloca i32, align 4
  %alloca_2 = alloca i64, align 8
  store i64 4096, ptr %alloca_2
  %alloca_4 = alloca i64, align 8
  %load_5 = load i64, ptr %alloca_2
  store i64 %load_5, ptr %alloca_4
  %alloca_7 = alloca i64, align 8
  store i64 150, ptr %alloca_7
  %alloca_9 = alloca i64, align 8
  %load_10 = load i64, ptr %alloca_7
  store i64 %load_10, ptr %alloca_9
  %alloca_12 = alloca i64, align 8
  store i64 120, ptr %alloca_12
  %alloca_14 = alloca i64, align 8
  %load_15 = load i64, ptr %alloca_12
  store i64 %load_15, ptr %alloca_14
  %alloca_17 = alloca i64, align 8
  store i64 2048, ptr %alloca_17
  %alloca_19 = alloca i1, align 1
  %load_20 = load i64, ptr %alloca_4
  %load_21 = load i64, ptr %alloca_17
  %icmp_22 = icmp sgt i64 %load_20, %load_21
  store i1 %icmp_22, ptr %alloca_19
  %alloca_24 = alloca i1, align 1
  %load_25 = load i1, ptr %alloca_19
  store i1 %load_25, ptr %alloca_24
  %alloca_27 = alloca i64, align 8
  store i64 1024, ptr %alloca_27
  %alloca_29 = alloca i64, align 8
  %load_30 = load i64, ptr %alloca_4
  %load_31 = load i64, ptr %alloca_27
  %div_32 = udiv i64 %load_30, %load_31
  store i64 %div_32, ptr %alloca_29
  %load_34 = load i64, ptr %alloca_29
  %load_35 = load i64, ptr %alloca_14
  %load_36 = load i1, ptr %alloca_24
  call i32 (ptr, ...) @printf(ptr @.str.0, i64 %load_34, i64 %load_35, i1 %load_36)
  br label %bb1
bb1:
  %alloca_38 = alloca i64, align 8
  store i64 0, ptr %alloca_38
  %alloca_40 = alloca i64, align 8
  %load_41 = load i64, ptr %alloca_38
  store i64 %load_41, ptr %alloca_40
  %alloca_43 = alloca i64, align 8
  store i64 1024, ptr %alloca_43
  %alloca_45 = alloca i64, align 8
  %load_46 = load i64, ptr %alloca_4
  %load_47 = load i64, ptr %alloca_43
  %div_48 = udiv i64 %load_46, %load_47
  store i64 %div_48, ptr %alloca_45
  %load_50 = load i64, ptr %alloca_45
  %load_51 = load i64, ptr %alloca_9
  call i32 (ptr, ...) @printf(ptr @.str.1, i64 %load_50, i64 %load_51)
  br label %bb2
bb2:
  %alloca_53 = alloca i64, align 8
  store i64 3, ptr %alloca_53
  %alloca_55 = alloca i64, align 8
  %load_56 = load i64, ptr %alloca_53
  store i64 %load_56, ptr %alloca_55
  %alloca_58 = alloca i64, align 8
  store i64 2, ptr %alloca_58
  %alloca_60 = alloca i64, align 8
  %load_61 = load i64, ptr %alloca_4
  %load_62 = load i64, ptr %alloca_58
  %mul_63 = mul i64 %load_61, %load_62
  store i64 %mul_63, ptr %alloca_60
  %alloca_65 = alloca i64, align 8
  %load_66 = load i64, ptr %alloca_60
  store i64 %load_66, ptr %alloca_65
  %alloca_68 = alloca i64, align 8
  store i64 2048, ptr %alloca_68
  %alloca_70 = alloca i1, align 1
  %load_71 = load i64, ptr %alloca_4
  %load_72 = load i64, ptr %alloca_68
  %icmp_73 = icmp sgt i64 %load_71, %load_72
  store i1 %icmp_73, ptr %alloca_70
  %load_75 = load i1, ptr %alloca_70
  br i1 %load_75, label %bb3, label %bb4
bb3:
  %alloca_76 = alloca ptr, align 8
  store ptr @.str.2, ptr %alloca_76
  %load_78 = load ptr, ptr %alloca_76
  store ptr %load_78, ptr %alloca_0
  br label %bb5
bb4:
  %alloca_80 = alloca ptr, align 8
  store ptr @.str.3, ptr %alloca_80
  %load_82 = load ptr, ptr %alloca_80
  store ptr %load_82, ptr %alloca_0
  br label %bb5
bb5:
  %alloca_84 = alloca ptr, align 8
  %load_85 = load ptr, ptr %alloca_0
  store ptr %load_85, ptr %alloca_84
  %alloca_87 = alloca i64, align 8
  %load_88 = load i64, ptr %alloca_4
  %load_89 = load i64, ptr %alloca_9
  %mul_90 = mul i64 %load_88, %load_89
  store i64 %mul_90, ptr %alloca_87
  %alloca_92 = alloca i64, align 8
  %load_93 = load i64, ptr %alloca_55
  %load_94 = load i64, ptr %alloca_87
  %mul_95 = mul i64 %load_93, %load_94
  store i64 %mul_95, ptr %alloca_92
  %alloca_97 = alloca i64, align 8
  %load_98 = load i64, ptr %alloca_92
  store i64 %load_98, ptr %alloca_97
  %load_100 = load i64, ptr %alloca_65
  %load_101 = load ptr, ptr %alloca_84
  %load_102 = load i64, ptr %alloca_97
  call i32 (ptr, ...) @printf(ptr @.str.4, i64 %load_100, ptr %load_101, i64 %load_102)
  br label %bb6
bb6:
  store i32 0, ptr %alloca_1
  ret i32 0
}

