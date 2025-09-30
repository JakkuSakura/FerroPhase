; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [4 x i8] c"hot\00", align 1
@.str.1 = constant [15 x i8] [i8 37, i8 108, i8 108, i8 100, i8 194, i8 176, i8 67, i8 32, i8 105, i8 115, i8 32, i8 37, i8 115, i8 10, i8 0], align 1
@.str.2 = constant [5 x i8] c"warm\00", align 1
@.str.3 = constant [5 x i8] c"cold\00", align 1
@.str.4 = constant [8 x i8] c"outdoor\00", align 1
@.str.5 = constant [7 x i8] c"indoor\00", align 1
@.str.6 = constant [15 x i8] [i8 83, i8 117, i8 103, i8 103, i8 101, i8 115, i8 116, i8 101, i8 100, i8 58, i8 32, i8 37, i8 115, i8 10, i8 0], align 1
@.str.7 = constant [2 x i8] c"A\00", align 1
@.str.8 = constant [23 x i8] [i8 83, i8 99, i8 111, i8 114, i8 101, i8 32, i8 37, i8 108, i8 108, i8 100, i8 32, i8 61, i8 32, i8 103, i8 114, i8 97, i8 100, i8 101, i8 32, i8 37, i8 115, i8 10, i8 0], align 1
@.str.9 = constant [2 x i8] c"B\00", align 1
@.str.10 = constant [2 x i8] c"C\00", align 1
@.str.11 = constant [2 x i8] c"F\00", align 1
@.str.12 = constant [5 x i8] c"high\00", align 1
@.str.13 = constant [18 x i8] [i8 86, i8 97, i8 108, i8 117, i8 101, i8 32, i8 37, i8 108, i8 108, i8 100, i8 32, i8 105, i8 115, i8 32, i8 37, i8 115, i8 10, i8 0], align 1
@.str.14 = constant [7 x i8] c"medium\00", align 1
@.str.15 = constant [4 x i8] c"low\00", align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %alloca_0 = alloca ptr, align 8
  %alloca_1 = alloca ptr, align 8
  %alloca_2 = alloca i32, align 4
  %alloca_3 = alloca ptr, align 8
  %alloca_4 = alloca ptr, align 8
  %alloca_5 = alloca ptr, align 8
  %alloca_6 = alloca ptr, align 8
  %alloca_7 = alloca ptr, align 8
  %alloca_8 = alloca ptr, align 8
  %alloca_9 = alloca i64, align 8
  store i64 25, ptr %alloca_9
  %alloca_11 = alloca i64, align 8
  %load_12 = load i64, ptr %alloca_9
  store i64 %load_12, ptr %alloca_11
  %alloca_14 = alloca i64, align 8
  store i64 30, ptr %alloca_14
  %alloca_16 = alloca i1, align 1
  %load_17 = load i64, ptr %alloca_11
  %load_18 = load i64, ptr %alloca_14
  %icmp_19 = icmp sgt i64 %load_17, %load_18
  store i1 %icmp_19, ptr %alloca_16
  %load_21 = load i1, ptr %alloca_16
  br i1 %load_21, label %bb1, label %bb2
bb1:
  %alloca_22 = alloca ptr, align 8
  store ptr @.str.0, ptr %alloca_22
  %load_24 = load ptr, ptr %alloca_22
  store ptr %load_24, ptr %alloca_7
  br label %bb3
bb2:
  %alloca_26 = alloca i64, align 8
  store i64 20, ptr %alloca_26
  %alloca_28 = alloca i1, align 1
  %load_29 = load i64, ptr %alloca_11
  %load_30 = load i64, ptr %alloca_26
  %icmp_31 = icmp sgt i64 %load_29, %load_30
  store i1 %icmp_31, ptr %alloca_28
  %load_33 = load i1, ptr %alloca_28
  br i1 %load_33, label %bb4, label %bb5
bb3:
  %alloca_34 = alloca ptr, align 8
  %load_35 = load ptr, ptr %alloca_7
  store ptr %load_35, ptr %alloca_34
  %alloca_37 = alloca ptr, align 8
  %load_38 = load i64, ptr %alloca_11
  %load_39 = load ptr, ptr %alloca_34
  call i32 (ptr, ...) @printf(ptr @.str.1, i64 %load_38, ptr %load_39)
  br label %bb7
bb4:
  %alloca_41 = alloca ptr, align 8
  store ptr @.str.2, ptr %alloca_41
  %load_43 = load ptr, ptr %alloca_41
  store ptr %load_43, ptr %alloca_4
  br label %bb6
bb5:
  %alloca_45 = alloca ptr, align 8
  store ptr @.str.3, ptr %alloca_45
  %load_47 = load ptr, ptr %alloca_45
  store ptr %load_47, ptr %alloca_4
  br label %bb6
bb7:
  %alloca_49 = alloca i1, align 1
  store i1 1, ptr %alloca_49
  %alloca_51 = alloca i1, align 1
  %load_52 = load i1, ptr %alloca_49
  store i1 %load_52, ptr %alloca_51
  %alloca_54 = alloca i64, align 8
  store i64 20, ptr %alloca_54
  %alloca_56 = alloca i1, align 1
  %load_57 = load i64, ptr %alloca_11
  %load_58 = load i64, ptr %alloca_54
  %icmp_59 = icmp sgt i64 %load_57, %load_58
  store i1 %icmp_59, ptr %alloca_56
  %alloca_61 = alloca i1, align 1
  %load_62 = load i1, ptr %alloca_56
  store i1 %load_62, ptr %alloca_61
  %alloca_64 = alloca i1, align 1
  %load_65 = load i1, ptr %alloca_51
  %load_66 = load i1, ptr %alloca_61
  %and_67 = and i1 %load_65, %load_66
  store i1 %and_67, ptr %alloca_64
  %load_69 = load i1, ptr %alloca_64
  br i1 %load_69, label %bb8, label %bb9
bb6:
  %load_70 = load ptr, ptr %alloca_4
  store ptr %load_70, ptr %alloca_7
  br label %bb3
bb8:
  %alloca_72 = alloca ptr, align 8
  store ptr @.str.4, ptr %alloca_72
  %load_74 = load ptr, ptr %alloca_72
  store ptr %load_74, ptr %alloca_6
  br label %bb10
bb9:
  %alloca_76 = alloca ptr, align 8
  store ptr @.str.5, ptr %alloca_76
  %load_78 = load ptr, ptr %alloca_76
  store ptr %load_78, ptr %alloca_6
  br label %bb10
bb10:
  %alloca_80 = alloca ptr, align 8
  %load_81 = load ptr, ptr %alloca_6
  store ptr %load_81, ptr %alloca_80
  %alloca_83 = alloca ptr, align 8
  %load_84 = load ptr, ptr %alloca_80
  call i32 (ptr, ...) @printf(ptr @.str.6, ptr %load_84)
  br label %bb11
bb11:
  %alloca_86 = alloca i64, align 8
  store i64 85, ptr %alloca_86
  %alloca_88 = alloca i64, align 8
  %load_89 = load i64, ptr %alloca_86
  store i64 %load_89, ptr %alloca_88
  %alloca_91 = alloca i64, align 8
  store i64 90, ptr %alloca_91
  %alloca_93 = alloca i1, align 1
  %load_94 = load i64, ptr %alloca_88
  %load_95 = load i64, ptr %alloca_91
  %icmp_96 = icmp sge i64 %load_94, %load_95
  store i1 %icmp_96, ptr %alloca_93
  %load_98 = load i1, ptr %alloca_93
  br i1 %load_98, label %bb12, label %bb13
bb12:
  %alloca_99 = alloca ptr, align 8
  store ptr @.str.7, ptr %alloca_99
  %load_101 = load ptr, ptr %alloca_99
  store ptr %load_101, ptr %alloca_3
  br label %bb14
bb13:
  %alloca_103 = alloca i64, align 8
  store i64 80, ptr %alloca_103
  %alloca_105 = alloca i1, align 1
  %load_106 = load i64, ptr %alloca_88
  %load_107 = load i64, ptr %alloca_103
  %icmp_108 = icmp sge i64 %load_106, %load_107
  store i1 %icmp_108, ptr %alloca_105
  %load_110 = load i1, ptr %alloca_105
  br i1 %load_110, label %bb15, label %bb16
bb14:
  %alloca_111 = alloca ptr, align 8
  %load_112 = load ptr, ptr %alloca_3
  store ptr %load_112, ptr %alloca_111
  %alloca_114 = alloca ptr, align 8
  %load_115 = load i64, ptr %alloca_88
  %load_116 = load ptr, ptr %alloca_111
  call i32 (ptr, ...) @printf(ptr @.str.8, i64 %load_115, ptr %load_116)
  br label %bb21
bb15:
  %alloca_118 = alloca ptr, align 8
  store ptr @.str.9, ptr %alloca_118
  %load_120 = load ptr, ptr %alloca_118
  store ptr %load_120, ptr %alloca_0
  br label %bb17
bb16:
  %alloca_122 = alloca i64, align 8
  store i64 70, ptr %alloca_122
  %alloca_124 = alloca i1, align 1
  %load_125 = load i64, ptr %alloca_88
  %load_126 = load i64, ptr %alloca_122
  %icmp_127 = icmp sge i64 %load_125, %load_126
  store i1 %icmp_127, ptr %alloca_124
  %load_129 = load i1, ptr %alloca_124
  br i1 %load_129, label %bb18, label %bb19
bb21:
  %alloca_130 = alloca i64, align 8
  store i64 42, ptr %alloca_130
  %alloca_132 = alloca i64, align 8
  %load_133 = load i64, ptr %alloca_130
  store i64 %load_133, ptr %alloca_132
  %alloca_135 = alloca i64, align 8
  store i64 50, ptr %alloca_135
  %alloca_137 = alloca i1, align 1
  %load_138 = load i64, ptr %alloca_132
  %load_139 = load i64, ptr %alloca_135
  %icmp_140 = icmp sgt i64 %load_138, %load_139
  store i1 %icmp_140, ptr %alloca_137
  %load_142 = load i1, ptr %alloca_137
  br i1 %load_142, label %bb22, label %bb23
bb17:
  %load_143 = load ptr, ptr %alloca_0
  store ptr %load_143, ptr %alloca_3
  br label %bb14
bb18:
  %alloca_145 = alloca ptr, align 8
  store ptr @.str.10, ptr %alloca_145
  %load_147 = load ptr, ptr %alloca_145
  store ptr %load_147, ptr %alloca_5
  br label %bb20
bb19:
  %alloca_149 = alloca ptr, align 8
  store ptr @.str.11, ptr %alloca_149
  %load_151 = load ptr, ptr %alloca_149
  store ptr %load_151, ptr %alloca_5
  br label %bb20
bb22:
  %alloca_153 = alloca ptr, align 8
  store ptr @.str.12, ptr %alloca_153
  %load_155 = load ptr, ptr %alloca_153
  store ptr %load_155, ptr %alloca_8
  br label %bb24
bb23:
  %alloca_157 = alloca i64, align 8
  store i64 25, ptr %alloca_157
  %alloca_159 = alloca i1, align 1
  %load_160 = load i64, ptr %alloca_132
  %load_161 = load i64, ptr %alloca_157
  %icmp_162 = icmp sgt i64 %load_160, %load_161
  store i1 %icmp_162, ptr %alloca_159
  %load_164 = load i1, ptr %alloca_159
  br i1 %load_164, label %bb25, label %bb26
bb20:
  %load_165 = load ptr, ptr %alloca_5
  store ptr %load_165, ptr %alloca_0
  br label %bb17
bb24:
  %alloca_167 = alloca ptr, align 8
  %load_168 = load ptr, ptr %alloca_8
  store ptr %load_168, ptr %alloca_167
  %alloca_170 = alloca ptr, align 8
  %load_171 = load i64, ptr %alloca_132
  %load_172 = load ptr, ptr %alloca_167
  call i32 (ptr, ...) @printf(ptr @.str.13, i64 %load_171, ptr %load_172)
  br label %bb28
bb25:
  %alloca_174 = alloca ptr, align 8
  store ptr @.str.14, ptr %alloca_174
  %load_176 = load ptr, ptr %alloca_174
  store ptr %load_176, ptr %alloca_1
  br label %bb27
bb26:
  %alloca_178 = alloca ptr, align 8
  store ptr @.str.15, ptr %alloca_178
  %load_180 = load ptr, ptr %alloca_178
  store ptr %load_180, ptr %alloca_1
  br label %bb27
bb28:
  store i32 0, ptr %alloca_2
  ret i32 0
bb27:
  %load_183 = load ptr, ptr %alloca_1
  store ptr %load_183, ptr %alloca_8
  br label %bb24
}

