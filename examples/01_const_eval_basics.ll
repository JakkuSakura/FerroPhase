; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [45 x i8] [i8 66, i8 117, i8 102, i8 102, i8 101, i8 114, i8 58, i8 32, i8 37, i8 108, i8 108, i8 100, i8 75, i8 66, i8 44, i8 32, i8 102, i8 97, i8 99, i8 116, i8 111, i8 114, i8 105, i8 97, i8 108, i8 40, i8 53, i8 41, i8 61, i8 37, i8 108, i8 108, i8 100, i8 44, i8 32, i8 108, i8 97, i8 114, i8 103, i8 101, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
@.str.1 = constant [41 x i8] [i8 67, i8 111, i8 110, i8 102, i8 105, i8 103, i8 58, i8 32, i8 37, i8 108, i8 108, i8 100, i8 75, i8 66, i8 32, i8 98, i8 117, i8 102, i8 102, i8 101, i8 114, i8 44, i8 32, i8 37, i8 108, i8 108, i8 100, i8 32, i8 99, i8 111, i8 110, i8 110, i8 101, i8 99, i8 116, i8 105, i8 111, i8 110, i8 115, i8 10, i8 0], align 1
@.str.2 = constant [6 x i8] c"large\00", align 1
@.str.3 = constant [6 x i8] c"small\00", align 1
@.str.4 = constant [51 x i8] [i8 67, i8 111, i8 110, i8 115, i8 116, i8 32, i8 98, i8 108, i8 111, i8 99, i8 107, i8 115, i8 58, i8 32, i8 115, i8 105, i8 122, i8 101, i8 61, i8 37, i8 108, i8 108, i8 100, i8 44, i8 32, i8 115, i8 116, i8 114, i8 97, i8 116, i8 101, i8 103, i8 121, i8 61, i8 37, i8 115, i8 44, i8 32, i8 109, i8 101, i8 109, i8 111, i8 114, i8 121, i8 61, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %alloca_0 = alloca i32, align 4
  %alloca_1 = alloca ptr, align 8
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
  %alloca_27 = alloca ptr, align 8
  %alloca_28 = alloca i64, align 8
  store i64 1024, ptr %alloca_28
  %alloca_30 = alloca i64, align 8
  %load_31 = load i64, ptr %alloca_4
  %load_32 = load i64, ptr %alloca_28
  %div_33 = udiv i64 %load_31, %load_32
  store i64 %div_33, ptr %alloca_30
  %load_35 = load i64, ptr %alloca_30
  %load_36 = load i64, ptr %alloca_14
  %load_37 = load i1, ptr %alloca_24
  call i32 (ptr, ...) @printf(ptr @.str.0, i64 %load_35, i64 %load_36, i1 %load_37)
  br label %bb1
bb1:
  %alloca_39 = alloca i64, align 8
  store i64 0, ptr %alloca_39
  %alloca_41 = alloca i64, align 8
  %load_42 = load i64, ptr %alloca_39
  store i64 %load_42, ptr %alloca_41
  %alloca_44 = alloca ptr, align 8
  %alloca_45 = alloca i64, align 8
  store i64 1024, ptr %alloca_45
  %alloca_47 = alloca i64, align 8
  %load_48 = load i64, ptr %alloca_4
  %load_49 = load i64, ptr %alloca_45
  %div_50 = udiv i64 %load_48, %load_49
  store i64 %div_50, ptr %alloca_47
  %load_52 = load i64, ptr %alloca_47
  %load_53 = load i64, ptr %alloca_9
  call i32 (ptr, ...) @printf(ptr @.str.1, i64 %load_52, i64 %load_53)
  br label %bb2
bb2:
  %alloca_55 = alloca i64, align 8
  store i64 3, ptr %alloca_55
  %alloca_57 = alloca i64, align 8
  %load_58 = load i64, ptr %alloca_55
  store i64 %load_58, ptr %alloca_57
  %alloca_60 = alloca i64, align 8
  store i64 2, ptr %alloca_60
  %alloca_62 = alloca i64, align 8
  %load_63 = load i64, ptr %alloca_4
  %load_64 = load i64, ptr %alloca_60
  %mul_65 = mul i64 %load_63, %load_64
  store i64 %mul_65, ptr %alloca_62
  %alloca_67 = alloca i64, align 8
  %load_68 = load i64, ptr %alloca_62
  store i64 %load_68, ptr %alloca_67
  %alloca_70 = alloca i64, align 8
  store i64 2048, ptr %alloca_70
  %alloca_72 = alloca i1, align 1
  %load_73 = load i64, ptr %alloca_4
  %load_74 = load i64, ptr %alloca_70
  %icmp_75 = icmp sgt i64 %load_73, %load_74
  store i1 %icmp_75, ptr %alloca_72
  %load_77 = load i1, ptr %alloca_72
  br i1 %load_77, label %bb3, label %bb4
bb3:
  %alloca_78 = alloca ptr, align 8
  store ptr @.str.2, ptr %alloca_78
  %load_80 = load ptr, ptr %alloca_78
  store ptr %load_80, ptr %alloca_1
  br label %bb5
bb4:
  %alloca_82 = alloca ptr, align 8
  store ptr @.str.3, ptr %alloca_82
  %load_84 = load ptr, ptr %alloca_82
  store ptr %load_84, ptr %alloca_1
  br label %bb5
bb5:
  %alloca_86 = alloca ptr, align 8
  %load_87 = load ptr, ptr %alloca_1
  store ptr %load_87, ptr %alloca_86
  %alloca_89 = alloca i64, align 8
  %load_90 = load i64, ptr %alloca_4
  %load_91 = load i64, ptr %alloca_9
  %mul_92 = mul i64 %load_90, %load_91
  store i64 %mul_92, ptr %alloca_89
  %alloca_94 = alloca i64, align 8
  %load_95 = load i64, ptr %alloca_57
  %load_96 = load i64, ptr %alloca_89
  %mul_97 = mul i64 %load_95, %load_96
  store i64 %mul_97, ptr %alloca_94
  %alloca_99 = alloca i64, align 8
  %load_100 = load i64, ptr %alloca_94
  store i64 %load_100, ptr %alloca_99
  %alloca_102 = alloca ptr, align 8
  %load_103 = load i64, ptr %alloca_67
  %load_104 = load ptr, ptr %alloca_86
  %load_105 = load i64, ptr %alloca_99
  call i32 (ptr, ...) @printf(ptr @.str.4, i64 %load_103, ptr %load_104, i64 %load_105)
  br label %bb6
bb6:
  store i32 0, ptr %alloca_0
  ret i32 0
}

