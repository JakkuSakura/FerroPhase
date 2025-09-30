; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [4 x i8] c"hot\00", align 1
@.str.1 = constant [15 x i8] [i8 37, i8 108, i8 108, i8 100, i8 194, i8 176, i8 67, i8 32, i8 105, i8 115, i8 32, i8 37, i8 112, i8 10, i8 0], align 1
@.str.2 = constant [5 x i8] c"warm\00", align 1
@.str.3 = constant [5 x i8] c"cold\00", align 1
@.str.4 = constant [8 x i8] c"outdoor\00", align 1
@.str.5 = constant [7 x i8] c"indoor\00", align 1
@.str.6 = constant [15 x i8] [i8 83, i8 117, i8 103, i8 103, i8 101, i8 115, i8 116, i8 101, i8 100, i8 58, i8 32, i8 37, i8 112, i8 10, i8 0], align 1
@.str.7 = constant [2 x i8] c"A\00", align 1
@.str.8 = constant [23 x i8] [i8 83, i8 99, i8 111, i8 114, i8 101, i8 32, i8 37, i8 108, i8 108, i8 100, i8 32, i8 61, i8 32, i8 103, i8 114, i8 97, i8 100, i8 101, i8 32, i8 37, i8 112, i8 10, i8 0], align 1
@.str.9 = constant [2 x i8] c"B\00", align 1
@.str.10 = constant [2 x i8] c"C\00", align 1
@.str.11 = constant [2 x i8] c"F\00", align 1
@.str.12 = constant [5 x i8] c"high\00", align 1
@.str.13 = constant [18 x i8] [i8 86, i8 97, i8 108, i8 117, i8 101, i8 32, i8 37, i8 108, i8 108, i8 100, i8 32, i8 105, i8 115, i8 32, i8 37, i8 112, i8 10, i8 0], align 1
@.str.14 = constant [7 x i8] c"medium\00", align 1
@.str.15 = constant [4 x i8] c"low\00", align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %alloca_0 = alloca ptr, align 8
  %alloca_1 = alloca ptr, align 8
  %alloca_2 = alloca ptr, align 8
  %alloca_3 = alloca ptr, align 8
  %alloca_4 = alloca ptr, align 8
  %alloca_5 = alloca ptr, align 8
  %alloca_6 = alloca ptr, align 8
  %alloca_7 = alloca ptr, align 8
  %alloca_8 = alloca i32, align 4
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
  store ptr %load_24, ptr %alloca_3
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
  %load_35 = load ptr, ptr %alloca_3
  store ptr %load_35, ptr %alloca_34
  %load_37 = load i64, ptr %alloca_11
  %load_38 = load ptr, ptr %alloca_34
  call i32 (ptr, ...) @printf(ptr @.str.1, i64 %load_37, ptr %load_38)
  br label %bb7
bb4:
  %alloca_40 = alloca ptr, align 8
  store ptr @.str.2, ptr %alloca_40
  %load_42 = load ptr, ptr %alloca_40
  store ptr %load_42, ptr %alloca_2
  br label %bb6
bb5:
  %alloca_44 = alloca ptr, align 8
  store ptr @.str.3, ptr %alloca_44
  %load_46 = load ptr, ptr %alloca_44
  store ptr %load_46, ptr %alloca_2
  br label %bb6
bb7:
  %alloca_48 = alloca i1, align 1
  store i1 1, ptr %alloca_48
  %alloca_50 = alloca i1, align 1
  %load_51 = load i1, ptr %alloca_48
  store i1 %load_51, ptr %alloca_50
  %alloca_53 = alloca i64, align 8
  store i64 20, ptr %alloca_53
  %alloca_55 = alloca i1, align 1
  %load_56 = load i64, ptr %alloca_11
  %load_57 = load i64, ptr %alloca_53
  %icmp_58 = icmp sgt i64 %load_56, %load_57
  store i1 %icmp_58, ptr %alloca_55
  %alloca_60 = alloca i1, align 1
  %load_61 = load i1, ptr %alloca_55
  store i1 %load_61, ptr %alloca_60
  %alloca_63 = alloca i1, align 1
  %load_64 = load i1, ptr %alloca_50
  %load_65 = load i1, ptr %alloca_60
  %and_66 = and i1 %load_64, %load_65
  store i1 %and_66, ptr %alloca_63
  %load_68 = load i1, ptr %alloca_63
  br i1 %load_68, label %bb8, label %bb9
bb6:
  %load_69 = load ptr, ptr %alloca_2
  store ptr %load_69, ptr %alloca_3
  br label %bb3
bb8:
  %alloca_71 = alloca ptr, align 8
  store ptr @.str.4, ptr %alloca_71
  %load_73 = load ptr, ptr %alloca_71
  store ptr %load_73, ptr %alloca_6
  br label %bb10
bb9:
  %alloca_75 = alloca ptr, align 8
  store ptr @.str.5, ptr %alloca_75
  %load_77 = load ptr, ptr %alloca_75
  store ptr %load_77, ptr %alloca_6
  br label %bb10
bb10:
  %alloca_79 = alloca ptr, align 8
  %load_80 = load ptr, ptr %alloca_6
  store ptr %load_80, ptr %alloca_79
  %load_82 = load ptr, ptr %alloca_79
  call i32 (ptr, ...) @printf(ptr @.str.6, ptr %load_82)
  br label %bb11
bb11:
  %alloca_84 = alloca i64, align 8
  store i64 85, ptr %alloca_84
  %alloca_86 = alloca i64, align 8
  %load_87 = load i64, ptr %alloca_84
  store i64 %load_87, ptr %alloca_86
  %alloca_89 = alloca i64, align 8
  store i64 90, ptr %alloca_89
  %alloca_91 = alloca i1, align 1
  %load_92 = load i64, ptr %alloca_86
  %load_93 = load i64, ptr %alloca_89
  %icmp_94 = icmp sge i64 %load_92, %load_93
  store i1 %icmp_94, ptr %alloca_91
  %load_96 = load i1, ptr %alloca_91
  br i1 %load_96, label %bb12, label %bb13
bb12:
  %alloca_97 = alloca ptr, align 8
  store ptr @.str.7, ptr %alloca_97
  %load_99 = load ptr, ptr %alloca_97
  store ptr %load_99, ptr %alloca_4
  br label %bb14
bb13:
  %alloca_101 = alloca i64, align 8
  store i64 80, ptr %alloca_101
  %alloca_103 = alloca i1, align 1
  %load_104 = load i64, ptr %alloca_86
  %load_105 = load i64, ptr %alloca_101
  %icmp_106 = icmp sge i64 %load_104, %load_105
  store i1 %icmp_106, ptr %alloca_103
  %load_108 = load i1, ptr %alloca_103
  br i1 %load_108, label %bb15, label %bb16
bb14:
  %alloca_109 = alloca ptr, align 8
  %load_110 = load ptr, ptr %alloca_4
  store ptr %load_110, ptr %alloca_109
  %load_112 = load i64, ptr %alloca_86
  %load_113 = load ptr, ptr %alloca_109
  call i32 (ptr, ...) @printf(ptr @.str.8, i64 %load_112, ptr %load_113)
  br label %bb21
bb15:
  %alloca_115 = alloca ptr, align 8
  store ptr @.str.9, ptr %alloca_115
  %load_117 = load ptr, ptr %alloca_115
  store ptr %load_117, ptr %alloca_1
  br label %bb17
bb16:
  %alloca_119 = alloca i64, align 8
  store i64 70, ptr %alloca_119
  %alloca_121 = alloca i1, align 1
  %load_122 = load i64, ptr %alloca_86
  %load_123 = load i64, ptr %alloca_119
  %icmp_124 = icmp sge i64 %load_122, %load_123
  store i1 %icmp_124, ptr %alloca_121
  %load_126 = load i1, ptr %alloca_121
  br i1 %load_126, label %bb18, label %bb19
bb21:
  %alloca_127 = alloca i64, align 8
  store i64 42, ptr %alloca_127
  %alloca_129 = alloca i64, align 8
  %load_130 = load i64, ptr %alloca_127
  store i64 %load_130, ptr %alloca_129
  %alloca_132 = alloca i64, align 8
  store i64 50, ptr %alloca_132
  %alloca_134 = alloca i1, align 1
  %load_135 = load i64, ptr %alloca_129
  %load_136 = load i64, ptr %alloca_132
  %icmp_137 = icmp sgt i64 %load_135, %load_136
  store i1 %icmp_137, ptr %alloca_134
  %load_139 = load i1, ptr %alloca_134
  br i1 %load_139, label %bb22, label %bb23
bb17:
  %load_140 = load ptr, ptr %alloca_1
  store ptr %load_140, ptr %alloca_4
  br label %bb14
bb18:
  %alloca_142 = alloca ptr, align 8
  store ptr @.str.10, ptr %alloca_142
  %load_144 = load ptr, ptr %alloca_142
  store ptr %load_144, ptr %alloca_0
  br label %bb20
bb19:
  %alloca_146 = alloca ptr, align 8
  store ptr @.str.11, ptr %alloca_146
  %load_148 = load ptr, ptr %alloca_146
  store ptr %load_148, ptr %alloca_0
  br label %bb20
bb22:
  %alloca_150 = alloca ptr, align 8
  store ptr @.str.12, ptr %alloca_150
  %load_152 = load ptr, ptr %alloca_150
  store ptr %load_152, ptr %alloca_7
  br label %bb24
bb23:
  %alloca_154 = alloca i64, align 8
  store i64 25, ptr %alloca_154
  %alloca_156 = alloca i1, align 1
  %load_157 = load i64, ptr %alloca_129
  %load_158 = load i64, ptr %alloca_154
  %icmp_159 = icmp sgt i64 %load_157, %load_158
  store i1 %icmp_159, ptr %alloca_156
  %load_161 = load i1, ptr %alloca_156
  br i1 %load_161, label %bb25, label %bb26
bb20:
  %load_162 = load ptr, ptr %alloca_0
  store ptr %load_162, ptr %alloca_1
  br label %bb17
bb24:
  %alloca_164 = alloca ptr, align 8
  %load_165 = load ptr, ptr %alloca_7
  store ptr %load_165, ptr %alloca_164
  %load_167 = load i64, ptr %alloca_129
  %load_168 = load ptr, ptr %alloca_164
  call i32 (ptr, ...) @printf(ptr @.str.13, i64 %load_167, ptr %load_168)
  br label %bb28
bb25:
  %alloca_170 = alloca ptr, align 8
  store ptr @.str.14, ptr %alloca_170
  %load_172 = load ptr, ptr %alloca_170
  store ptr %load_172, ptr %alloca_5
  br label %bb27
bb26:
  %alloca_174 = alloca ptr, align 8
  store ptr @.str.15, ptr %alloca_174
  %load_176 = load ptr, ptr %alloca_174
  store ptr %load_176, ptr %alloca_5
  br label %bb27
bb28:
  store i32 0, ptr %alloca_8
  ret i32 0
bb27:
  %load_179 = load ptr, ptr %alloca_5
  store ptr %load_179, ptr %alloca_7
  br label %bb24
}

