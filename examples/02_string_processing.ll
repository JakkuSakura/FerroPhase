; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [17 x i8] [i8 65, i8 112, i8 112, i8 32, i8 108, i8 101, i8 110, i8 103, i8 116, i8 104, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.1 = constant [21 x i8] [i8 86, i8 101, i8 114, i8 115, i8 105, i8 111, i8 110, i8 32, i8 108, i8 101, i8 110, i8 103, i8 116, i8 104, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.2 = constant [20 x i8] [i8 66, i8 97, i8 110, i8 110, i8 101, i8 114, i8 32, i8 108, i8 101, i8 110, i8 103, i8 116, i8 104, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.3 = constant [19 x i8] [i8 83, i8 101, i8 112, i8 97, i8 114, i8 97, i8 116, i8 111, i8 114, i8 32, i8 102, i8 108, i8 97, i8 103, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.4 = constant [17 x i8] [i8 66, i8 101, i8 116, i8 97, i8 32, i8 99, i8 104, i8 97, i8 110, i8 110, i8 101, i8 108, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.5 = constant [19 x i8] [i8 77, i8 101, i8 116, i8 97, i8 100, i8 97, i8 116, i8 97, i8 32, i8 118, i8 97, i8 108, i8 105, i8 100, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.6 = constant [18 x i8] [i8 66, i8 117, i8 102, i8 102, i8 101, i8 114, i8 32, i8 115, i8 105, i8 122, i8 101, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.7 = constant [18 x i8] [i8 72, i8 97, i8 115, i8 104, i8 32, i8 109, i8 97, i8 114, i8 107, i8 101, i8 114, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.8 = constant [20 x i8] [i8 83, i8 117, i8 109, i8 109, i8 97, i8 114, i8 121, i8 32, i8 115, i8 99, i8 111, i8 114, i8 101, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %alloca_0 = alloca i32, align 4
  %alloca_1 = alloca i64, align 8
  %alloca_2 = alloca i64, align 8
  store i64 10, ptr %alloca_2
  %alloca_4 = alloca i64, align 8
  %load_5 = load i64, ptr %alloca_2
  store i64 %load_5, ptr %alloca_4
  %alloca_7 = alloca i64, align 8
  store i64 5, ptr %alloca_7
  %alloca_9 = alloca i64, align 8
  %load_10 = load i64, ptr %alloca_7
  store i64 %load_10, ptr %alloca_9
  %alloca_12 = alloca i64, align 8
  store i64 3, ptr %alloca_12
  %alloca_14 = alloca i64, align 8
  %load_15 = load i64, ptr %alloca_12
  store i64 %load_15, ptr %alloca_14
  %alloca_17 = alloca i64, align 8
  %load_18 = load i64, ptr %alloca_4
  %load_19 = load i64, ptr %alloca_9
  %add_20 = add i64 %load_18, %load_19
  store i64 %add_20, ptr %alloca_17
  %alloca_22 = alloca i64, align 8
  %load_23 = load i64, ptr %alloca_17
  %load_24 = load i64, ptr %alloca_14
  %add_25 = add i64 %load_23, %load_24
  store i64 %add_25, ptr %alloca_22
  %alloca_27 = alloca i64, align 8
  %load_28 = load i64, ptr %alloca_22
  store i64 %load_28, ptr %alloca_27
  %alloca_30 = alloca i1, align 1
  store i1 1, ptr %alloca_30
  %alloca_32 = alloca i1, align 1
  %load_33 = load i1, ptr %alloca_30
  store i1 %load_33, ptr %alloca_32
  %alloca_35 = alloca i1, align 1
  store i1 0, ptr %alloca_35
  %alloca_37 = alloca i1, align 1
  %load_38 = load i1, ptr %alloca_35
  store i1 %load_38, ptr %alloca_37
  %alloca_40 = alloca i64, align 8
  store i64 15, ptr %alloca_40
  %alloca_42 = alloca i1, align 1
  %load_43 = load i64, ptr %alloca_27
  %load_44 = load i64, ptr %alloca_40
  %icmp_45 = icmp sge i64 %load_43, %load_44
  store i1 %icmp_45, ptr %alloca_42
  %alloca_47 = alloca i1, align 1
  %load_48 = load i1, ptr %alloca_42
  store i1 %load_48, ptr %alloca_47
  %alloca_50 = alloca i64, align 8
  store i64 20, ptr %alloca_50
  %alloca_52 = alloca i1, align 1
  %load_53 = load i64, ptr %alloca_27
  %load_54 = load i64, ptr %alloca_50
  %icmp_55 = icmp sgt i64 %load_53, %load_54
  store i1 %icmp_55, ptr %alloca_52
  %load_57 = load i1, ptr %alloca_52
  br i1 %load_57, label %bb1, label %bb2
bb1:
  %alloca_58 = alloca i64, align 8
  store i64 1024, ptr %alloca_58
  %load_60 = load i64, ptr %alloca_58
  store i64 %load_60, ptr %alloca_1
  br label %bb3
bb2:
  %alloca_62 = alloca i64, align 8
  store i64 512, ptr %alloca_62
  %load_64 = load i64, ptr %alloca_62
  store i64 %load_64, ptr %alloca_1
  br label %bb3
bb3:
  %alloca_66 = alloca i64, align 8
  %load_67 = load i64, ptr %alloca_1
  store i64 %load_67, ptr %alloca_66
  %alloca_69 = alloca i64, align 8
  store i64 31, ptr %alloca_69
  %alloca_71 = alloca i64, align 8
  %load_72 = load i64, ptr %alloca_69
  store i64 %load_72, ptr %alloca_71
  %alloca_74 = alloca i64, align 8
  %load_75 = load i64, ptr %alloca_4
  %load_76 = load i64, ptr %alloca_71
  %mul_77 = mul i64 %load_75, %load_76
  store i64 %mul_77, ptr %alloca_74
  %alloca_79 = alloca i64, align 8
  %load_80 = load i64, ptr %alloca_74
  %load_81 = load i64, ptr %alloca_9
  %add_82 = add i64 %load_80, %load_81
  store i64 %add_82, ptr %alloca_79
  %alloca_84 = alloca i64, align 8
  %load_85 = load i64, ptr %alloca_79
  store i64 %load_85, ptr %alloca_84
  %alloca_87 = alloca i64, align 8
  %load_88 = load i64, ptr %alloca_4
  %load_89 = load i64, ptr %alloca_9
  %add_90 = add i64 %load_88, %load_89
  store i64 %add_90, ptr %alloca_87
  %alloca_92 = alloca i64, align 8
  %load_93 = load i64, ptr %alloca_87
  %load_94 = load i64, ptr %alloca_27
  %add_95 = add i64 %load_93, %load_94
  store i64 %add_95, ptr %alloca_92
  %alloca_97 = alloca i64, align 8
  %load_98 = load i64, ptr %alloca_92
  store i64 %load_98, ptr %alloca_97
  %load_100 = load i64, ptr %alloca_4
  call i32 (ptr, ...) @printf(ptr @.str.0, i64 %load_100)
  br label %bb4
bb4:
  %load_102 = load i64, ptr %alloca_9
  call i32 (ptr, ...) @printf(ptr @.str.1, i64 %load_102)
  br label %bb5
bb5:
  %load_104 = load i64, ptr %alloca_27
  call i32 (ptr, ...) @printf(ptr @.str.2, i64 %load_104)
  br label %bb6
bb6:
  %load_106 = load i1, ptr %alloca_32
  call i32 (ptr, ...) @printf(ptr @.str.3, i1 %load_106)
  br label %bb7
bb7:
  %load_108 = load i1, ptr %alloca_37
  call i32 (ptr, ...) @printf(ptr @.str.4, i1 %load_108)
  br label %bb8
bb8:
  %load_110 = load i1, ptr %alloca_47
  call i32 (ptr, ...) @printf(ptr @.str.5, i1 %load_110)
  br label %bb9
bb9:
  %load_112 = load i64, ptr %alloca_66
  call i32 (ptr, ...) @printf(ptr @.str.6, i64 %load_112)
  br label %bb10
bb10:
  %load_114 = load i64, ptr %alloca_84
  call i32 (ptr, ...) @printf(ptr @.str.7, i64 %load_114)
  br label %bb11
bb11:
  %load_116 = load i64, ptr %alloca_97
  call i32 (ptr, ...) @printf(ptr @.str.8, i64 %load_116)
  br label %bb12
bb12:
  store i32 0, ptr %alloca_0
  ret i32 0
}

