; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [30 x i8] [i8 61, i8 61, i8 61, i8 32, i8 83, i8 116, i8 114, i8 117, i8 99, i8 116, i8 32, i8 73, i8 110, i8 116, i8 114, i8 111, i8 115, i8 112, i8 101, i8 99, i8 116, i8 105, i8 111, i8 110, i8 32, i8 61, i8 61, i8 61, i8 10, i8 0], align 1
@.str.1 = constant [22 x i8] [i8 80, i8 111, i8 105, i8 110, i8 116, i8 32, i8 115, i8 105, i8 122, i8 101, i8 58, i8 32, i8 37, i8 100, i8 32, i8 98, i8 121, i8 116, i8 101, i8 115, i8 10, i8 0], align 1
@.str.2 = constant [22 x i8] [i8 67, i8 111, i8 108, i8 111, i8 114, i8 32, i8 115, i8 105, i8 122, i8 101, i8 58, i8 32, i8 37, i8 100, i8 32, i8 98, i8 121, i8 116, i8 101, i8 115, i8 10, i8 0], align 1
@.str.3 = constant [18 x i8] [i8 80, i8 111, i8 105, i8 110, i8 116, i8 32, i8 102, i8 105, i8 101, i8 108, i8 100, i8 115, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.4 = constant [18 x i8] [i8 67, i8 111, i8 108, i8 111, i8 114, i8 32, i8 102, i8 105, i8 101, i8 108, i8 100, i8 115, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.5 = constant [17 x i8] [i8 80, i8 111, i8 105, i8 110, i8 116, i8 32, i8 104, i8 97, i8 115, i8 32, i8 120, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.6 = constant [17 x i8] [i8 80, i8 111, i8 105, i8 110, i8 116, i8 32, i8 104, i8 97, i8 115, i8 32, i8 122, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.7 = constant [19 x i8] [i8 80, i8 111, i8 105, i8 110, i8 116, i8 32, i8 109, i8 101, i8 116, i8 104, i8 111, i8 100, i8 115, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.8 = constant [19 x i8] [i8 67, i8 111, i8 108, i8 111, i8 114, i8 32, i8 109, i8 101, i8 116, i8 104, i8 111, i8 100, i8 115, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.9 = constant [31 x i8] [i8 10, i8 226, i8 156, i8 147, i8 32, i8 73, i8 110, i8 116, i8 114, i8 111, i8 115, i8 112, i8 101, i8 99, i8 116, i8 105, i8 111, i8 110, i8 32, i8 99, i8 111, i8 109, i8 112, i8 108, i8 101, i8 116, i8 101, i8 100, i8 33, i8 10, i8 0], align 1
@.str.10 = constant [29 x i8] [i8 10, i8 61, i8 61, i8 61, i8 32, i8 84, i8 114, i8 97, i8 110, i8 115, i8 112, i8 105, i8 108, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 68, i8 101, i8 109, i8 111, i8 32, i8 61, i8 61, i8 61, i8 10, i8 0], align 1
@.str.11 = constant [29 x i8] [i8 84, i8 114, i8 97, i8 110, i8 115, i8 112, i8 105, i8 108, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 116, i8 97, i8 114, i8 103, i8 101, i8 116, i8 32, i8 115, i8 105, i8 122, i8 101, i8 115, i8 58, i8 10, i8 0], align 1
@.str.12 = constant [29 x i8] [i8 32, i8 32, i8 80, i8 111, i8 105, i8 110, i8 116, i8 58, i8 32, i8 37, i8 108, i8 108, i8 117, i8 32, i8 98, i8 121, i8 116, i8 101, i8 115, i8 32, i8 40, i8 99, i8 111, i8 110, i8 115, i8 116, i8 41, i8 10, i8 0], align 1
@.str.13 = constant [29 x i8] [i8 32, i8 32, i8 67, i8 111, i8 108, i8 111, i8 114, i8 58, i8 32, i8 37, i8 108, i8 108, i8 117, i8 32, i8 98, i8 121, i8 116, i8 101, i8 115, i8 32, i8 40, i8 99, i8 111, i8 110, i8 115, i8 116, i8 41, i8 10, i8 0], align 1
@.str.14 = constant [24 x i8] [i8 32, i8 32, i8 67, i8 111, i8 109, i8 98, i8 105, i8 110, i8 101, i8 100, i8 58, i8 32, i8 37, i8 108, i8 108, i8 117, i8 32, i8 98, i8 121, i8 116, i8 101, i8 115, i8 10, i8 0], align 1
@.str.15 = constant [20 x i8] [i8 82, i8 117, i8 110, i8 116, i8 105, i8 109, i8 101, i8 32, i8 105, i8 110, i8 115, i8 116, i8 97, i8 110, i8 99, i8 101, i8 115, i8 58, i8 10, i8 0], align 1
@.str.16 = constant [20 x i8] [i8 32, i8 32, i8 79, i8 114, i8 105, i8 103, i8 105, i8 110, i8 58, i8 32, i8 40, i8 37, i8 100, i8 44, i8 32, i8 37, i8 100, i8 41, i8 10, i8 0], align 1
@.str.17 = constant [24 x i8] [i8 32, i8 32, i8 82, i8 101, i8 100, i8 58, i8 32, i8 114, i8 103, i8 98, i8 40, i8 37, i8 100, i8 44, i8 32, i8 37, i8 100, i8 44, i8 32, i8 37, i8 100, i8 41, i8 10, i8 0], align 1
@.str.18 = constant [54 x i8] [i8 10, i8 226, i8 156, i8 147, i8 32, i8 73, i8 110, i8 116, i8 114, i8 111, i8 115, i8 112, i8 101, i8 99, i8 116, i8 105, i8 111, i8 110, i8 32, i8 101, i8 110, i8 97, i8 98, i8 108, i8 101, i8 115, i8 32, i8 101, i8 120, i8 116, i8 101, i8 114, i8 110, i8 97, i8 108, i8 32, i8 99, i8 111, i8 100, i8 101, i8 32, i8 103, i8 101, i8 110, i8 101, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 33, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %alloca_0 = alloca i32, align 4
  %alloca_1 = alloca i64, align 8
  store i1 0, ptr %alloca_1
  %alloca_3 = alloca i64, align 8
  %load_4 = load i64, ptr %alloca_1
  store i64 %load_4, ptr %alloca_3
  %alloca_6 = alloca i64, align 8
  store i1 0, ptr %alloca_6
  %alloca_8 = alloca i64, align 8
  %load_9 = load i64, ptr %alloca_6
  store i64 %load_9, ptr %alloca_8
  %alloca_11 = alloca i64, align 8
  store i1 0, ptr %alloca_11
  %alloca_13 = alloca i64, align 8
  %load_14 = load i64, ptr %alloca_11
  store i64 %load_14, ptr %alloca_13
  %alloca_16 = alloca i64, align 8
  store i1 0, ptr %alloca_16
  %alloca_18 = alloca i64, align 8
  %load_19 = load i64, ptr %alloca_16
  store i64 %load_19, ptr %alloca_18
  %alloca_21 = alloca i1, align 1
  store i1 0, ptr %alloca_21
  %alloca_23 = alloca i1, align 1
  %load_24 = load i1, ptr %alloca_21
  store i1 %load_24, ptr %alloca_23
  %alloca_26 = alloca i1, align 1
  store i1 0, ptr %alloca_26
  %alloca_28 = alloca i1, align 1
  %load_29 = load i1, ptr %alloca_26
  store i1 %load_29, ptr %alloca_28
  %alloca_31 = alloca i64, align 8
  store i1 0, ptr %alloca_31
  %alloca_33 = alloca i64, align 8
  %load_34 = load i64, ptr %alloca_31
  store i64 %load_34, ptr %alloca_33
  %alloca_36 = alloca i64, align 8
  store i1 0, ptr %alloca_36
  %alloca_38 = alloca i64, align 8
  %load_39 = load i64, ptr %alloca_36
  store i64 %load_39, ptr %alloca_38
  %alloca_41 = alloca ptr, align 8
  call i32 (ptr, ...) @printf(ptr @.str.0)
  br label %bb1
bb1:
  %alloca_43 = alloca ptr, align 8
  call i32 (ptr, ...) @printf(ptr @.str.1, i1 0)
  br label %bb2
bb2:
  %alloca_45 = alloca ptr, align 8
  call i32 (ptr, ...) @printf(ptr @.str.2, i1 0)
  br label %bb3
bb3:
  %alloca_47 = alloca ptr, align 8
  call i32 (ptr, ...) @printf(ptr @.str.3, i1 0)
  br label %bb4
bb4:
  %alloca_49 = alloca ptr, align 8
  call i32 (ptr, ...) @printf(ptr @.str.4, i1 0)
  br label %bb5
bb5:
  %alloca_51 = alloca ptr, align 8
  call i32 (ptr, ...) @printf(ptr @.str.5, i1 0)
  br label %bb6
bb6:
  %alloca_53 = alloca ptr, align 8
  call i32 (ptr, ...) @printf(ptr @.str.6, i1 0)
  br label %bb7
bb7:
  %alloca_55 = alloca ptr, align 8
  call i32 (ptr, ...) @printf(ptr @.str.7, i1 0)
  br label %bb8
bb8:
  %alloca_57 = alloca ptr, align 8
  call i32 (ptr, ...) @printf(ptr @.str.8, i1 0)
  br label %bb9
bb9:
  %alloca_59 = alloca ptr, align 8
  call i32 (ptr, ...) @printf(ptr @.str.9)
  br label %bb10
bb10:
  %alloca_61 = alloca ptr, align 8
  call i32 (ptr, ...) @printf(ptr @.str.10)
  br label %bb11
bb11:
  %alloca_63 = alloca i64, align 8
  store i1 0, ptr %alloca_63
  %alloca_65 = alloca i64, align 8
  %load_66 = load i64, ptr %alloca_63
  store i64 %load_66, ptr %alloca_65
  %alloca_68 = alloca i64, align 8
  store i1 0, ptr %alloca_68
  %alloca_70 = alloca i64, align 8
  %load_71 = load i64, ptr %alloca_68
  store i64 %load_71, ptr %alloca_70
  %alloca_73 = alloca i64, align 8
  %load_74 = load i64, ptr %alloca_65
  %load_75 = load i64, ptr %alloca_70
  %add_76 = add i64 %load_74, %load_75
  store i64 %add_76, ptr %alloca_73
  %alloca_78 = alloca i64, align 8
  %load_79 = load i64, ptr %alloca_73
  store i64 %load_79, ptr %alloca_78
  %alloca_81 = alloca i64, align 8
  store i64 0, ptr %alloca_81
  %alloca_83 = alloca i64, align 8
  %load_84 = load i64, ptr %alloca_81
  store i64 %load_84, ptr %alloca_83
  %alloca_86 = alloca i64, align 8
  store i64 0, ptr %alloca_86
  %alloca_88 = alloca i64, align 8
  %load_89 = load i64, ptr %alloca_86
  store i64 %load_89, ptr %alloca_88
  %alloca_91 = alloca ptr, align 8
  call i32 (ptr, ...) @printf(ptr @.str.11)
  br label %bb12
bb12:
  %alloca_93 = alloca ptr, align 8
  %load_94 = load i64, ptr %alloca_65
  call i32 (ptr, ...) @printf(ptr @.str.12, i64 %load_94)
  br label %bb13
bb13:
  %alloca_96 = alloca ptr, align 8
  %load_97 = load i64, ptr %alloca_70
  call i32 (ptr, ...) @printf(ptr @.str.13, i64 %load_97)
  br label %bb14
bb14:
  %alloca_99 = alloca ptr, align 8
  %load_100 = load i64, ptr %alloca_78
  call i32 (ptr, ...) @printf(ptr @.str.14, i64 %load_100)
  br label %bb15
bb15:
  %alloca_102 = alloca ptr, align 8
  call i32 (ptr, ...) @printf(ptr @.str.15)
  br label %bb16
bb16:
  %alloca_104 = alloca ptr, align 8
  %alloca_105 = alloca i32, align 4
  %load_106 = load i64, ptr %alloca_83
  store i64 %load_106, ptr %alloca_105
  %alloca_108 = alloca i32, align 4
  %load_109 = load i64, ptr %alloca_83
  store i64 %load_109, ptr %alloca_108
  %load_111 = load i32, ptr %alloca_105
  %load_112 = load i32, ptr %alloca_108
  call i32 (ptr, ...) @printf(ptr @.str.16, i32 %load_111, i32 %load_112)
  br label %bb17
bb17:
  %alloca_114 = alloca ptr, align 8
  %alloca_115 = alloca i32, align 4
  %load_116 = load i64, ptr %alloca_88
  store i64 %load_116, ptr %alloca_115
  %alloca_118 = alloca i32, align 4
  %load_119 = load i64, ptr %alloca_88
  store i64 %load_119, ptr %alloca_118
  %alloca_121 = alloca i32, align 4
  %load_122 = load i64, ptr %alloca_88
  store i64 %load_122, ptr %alloca_121
  %load_124 = load i32, ptr %alloca_115
  %load_125 = load i32, ptr %alloca_118
  %load_126 = load i32, ptr %alloca_121
  call i32 (ptr, ...) @printf(ptr @.str.17, i32 %load_124, i32 %load_125, i32 %load_126)
  br label %bb18
bb18:
  %alloca_128 = alloca ptr, align 8
  call i32 (ptr, ...) @printf(ptr @.str.18)
  br label %bb19
bb19:
  store i32 0, ptr %alloca_0
  ret i32 0
}

