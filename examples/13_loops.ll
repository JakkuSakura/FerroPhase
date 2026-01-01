; ModuleID = '13_loops'
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.13_loops.0 = constant [26 x i8] [i8 61, i8 61, i8 61, i8 32, i8 76, i8 111, i8 111, i8 112, i8 32, i8 67, i8 111, i8 110, i8 115, i8 116, i8 114, i8 117, i8 99, i8 116, i8 115, i8 32, i8 61, i8 61, i8 61, i8 10, i8 10, i8 0], align 1
@.str.13_loops.1 = constant [28 x i8] [i8 49, i8 46, i8 32, i8 87, i8 104, i8 105, i8 108, i8 101, i8 32, i8 108, i8 111, i8 111, i8 112, i8 32, i8 45, i8 32, i8 102, i8 97, i8 99, i8 116, i8 111, i8 114, i8 105, i8 97, i8 108, i8 58, i8 10, i8 0], align 1
@.str.13_loops.10 = constant [25 x i8] [i8 10, i8 52, i8 46, i8 32, i8 76, i8 111, i8 111, i8 112, i8 32, i8 119, i8 105, i8 116, i8 104, i8 32, i8 99, i8 111, i8 110, i8 116, i8 105, i8 110, i8 117, i8 101, i8 58, i8 10, i8 0], align 1
@.str.13_loops.11 = constant [34 x i8] [i8 32, i8 32, i8 83, i8 117, i8 109, i8 32, i8 111, i8 102, i8 32, i8 101, i8 118, i8 101, i8 110, i8 32, i8 110, i8 117, i8 109, i8 98, i8 101, i8 114, i8 115, i8 32, i8 60, i8 32, i8 49, i8 48, i8 58, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.13_loops.12 = constant [19 x i8] [i8 10, i8 53, i8 46, i8 32, i8 78, i8 101, i8 115, i8 116, i8 101, i8 100, i8 32, i8 108, i8 111, i8 111, i8 112, i8 115, i8 58, i8 10, i8 0], align 1
@.str.13_loops.13 = constant [21 x i8] [i8 10, i8 32, i8 32, i8 73, i8 116, i8 101, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 115, i8 58, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.13_loops.14 = constant [41 x i8] [i8 10, i8 54, i8 46, i8 32, i8 67, i8 111, i8 109, i8 112, i8 105, i8 108, i8 101, i8 45, i8 116, i8 105, i8 109, i8 101, i8 32, i8 105, i8 116, i8 101, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 40, i8 115, i8 105, i8 109, i8 117, i8 108, i8 97, i8 116, i8 101, i8 100, i8 41, i8 58, i8 10, i8 0], align 1
@.str.13_loops.15 = constant [19 x i8] [i8 32, i8 32, i8 99, i8 111, i8 110, i8 115, i8 116, i8 32, i8 53, i8 33, i8 32, i8 61, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.13_loops.16 = constant [36 x i8] [i8 10, i8 226, i8 156, i8 147, i8 32, i8 76, i8 111, i8 111, i8 112, i8 32, i8 99, i8 111, i8 110, i8 115, i8 116, i8 114, i8 117, i8 99, i8 116, i8 115, i8 32, i8 100, i8 101, i8 109, i8 111, i8 110, i8 115, i8 116, i8 114, i8 97, i8 116, i8 101, i8 100, i8 33, i8 10, i8 0], align 1
@.str.13_loops.2 = constant [13 x i8] [i8 32, i8 32, i8 53, i8 33, i8 32, i8 61, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.13_loops.3 = constant [13 x i8] [i8 32, i8 32, i8 55, i8 33, i8 32, i8 61, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.13_loops.4 = constant [27 x i8] [i8 10, i8 50, i8 46, i8 32, i8 70, i8 111, i8 114, i8 32, i8 108, i8 111, i8 111, i8 112, i8 32, i8 45, i8 32, i8 115, i8 117, i8 109, i8 32, i8 114, i8 97, i8 110, i8 103, i8 101, i8 58, i8 10, i8 0], align 1
@.str.13_loops.5 = constant [21 x i8] [i8 32, i8 32, i8 115, i8 117, i8 109, i8 40, i8 49, i8 46, i8 46, i8 49, i8 48, i8 41, i8 32, i8 61, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.13_loops.6 = constant [21 x i8] [i8 32, i8 32, i8 115, i8 117, i8 109, i8 40, i8 53, i8 46, i8 46, i8 49, i8 53, i8 41, i8 32, i8 61, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.13_loops.7 = constant [33 x i8] [i8 10, i8 51, i8 46, i8 32, i8 76, i8 111, i8 111, i8 112, i8 32, i8 119, i8 105, i8 116, i8 104, i8 32, i8 98, i8 114, i8 101, i8 97, i8 107, i8 32, i8 101, i8 120, i8 112, i8 114, i8 101, i8 115, i8 115, i8 105, i8 111, i8 110, i8 58, i8 10, i8 0], align 1
@.str.13_loops.8 = constant [29 x i8] [i8 32, i8 32, i8 70, i8 105, i8 114, i8 115, i8 116, i8 32, i8 100, i8 105, i8 118, i8 105, i8 115, i8 111, i8 114, i8 32, i8 111, i8 102, i8 32, i8 50, i8 52, i8 58, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.13_loops.9 = constant [29 x i8] [i8 32, i8 32, i8 70, i8 105, i8 114, i8 115, i8 116, i8 32, i8 100, i8 105, i8 118, i8 105, i8 115, i8 111, i8 114, i8 32, i8 111, i8 102, i8 32, i8 49, i8 55, i8 58, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i64 @factorial(i64 %arg0) {
bb0:
  %alloca_0 = alloca i64, align 8
  %alloca_1 = alloca i64, align 8
  %alloca_2 = alloca i64, align 8
  store i64 1, ptr %alloca_0
  store i64 1, ptr %alloca_1
  br label %bb1
bb1:
  %alloca_5 = alloca i1, align 1
  %load_6 = load i64, ptr %alloca_1
  %icmp_7 = icmp sle i64 %load_6, %arg0
  store i1 %icmp_7, ptr %alloca_5
  %load_9 = load i1, ptr %alloca_5
  br i1 %load_9, label %bb2, label %bb3
bb2:
  %load_10 = load i64, ptr %alloca_0
  %load_11 = load i64, ptr %alloca_1
  %mul_12 = mul i64 %load_10, %load_11
  store i64 %mul_12, ptr %alloca_0
  %load_14 = load i64, ptr %alloca_1
  %add_15 = add i64 %load_14, 1
  store i64 %add_15, ptr %alloca_1
  br label %bb1
bb3:
  %load_17 = load i64, ptr %alloca_0
  store i64 %load_17, ptr %alloca_2
  %load_19 = load i64, ptr %alloca_2
  ret i64 %load_19
}

define i64 @find_first_divisor(i64 %arg0) {
bb0:
  %alloca_40 = alloca i64, align 8
  %alloca_41 = alloca i64, align 8
  %alloca_42 = alloca i64, align 8
  store i64 2, ptr %alloca_42
  br label %bb1
bb1:
  br label %bb2
bb2:
  %alloca_44 = alloca i64, align 8
  %load_45 = load i64, ptr %alloca_42
  %load_46 = load i64, ptr %alloca_42
  %mul_47 = mul i64 %load_45, %load_46
  store i64 %mul_47, ptr %alloca_44
  %alloca_49 = alloca i1, align 1
  %load_50 = load i64, ptr %alloca_44
  %icmp_51 = icmp sgt i64 %load_50, %arg0
  store i1 %icmp_51, ptr %alloca_49
  %load_53 = load i1, ptr %alloca_49
  br i1 %load_53, label %bb4, label %bb5
bb4:
  store i64 %arg0, ptr %alloca_41
  br label %bb3
bb5:
  br label %bb6
bb3:
  br label %bb12
bb6:
  %alloca_55 = alloca i64, align 8
  %load_56 = load i64, ptr %alloca_42
  %srem_57 = srem i64 %arg0, %load_56
  store i64 %srem_57, ptr %alloca_55
  %alloca_59 = alloca i1, align 1
  %load_60 = load i64, ptr %alloca_55
  %icmp_61 = icmp eq i64 %load_60, 0
  store i1 %icmp_61, ptr %alloca_59
  %load_63 = load i1, ptr %alloca_59
  br i1 %load_63, label %bb8, label %bb9
bb12:
  br label %bb13
bb8:
  %load_64 = load i64, ptr %alloca_42
  store i64 %load_64, ptr %alloca_41
  br label %bb3
bb9:
  br label %bb10
bb13:
  %alloca_66 = alloca i64, align 8
  %load_67 = load i64, ptr %alloca_42
  %load_68 = load i64, ptr %alloca_42
  %mul_69 = mul i64 %load_67, %load_68
  store i64 %mul_69, ptr %alloca_66
  %alloca_71 = alloca i1, align 1
  %load_72 = load i64, ptr %alloca_66
  %icmp_73 = icmp sgt i64 %load_72, %arg0
  store i1 %icmp_73, ptr %alloca_71
  %load_75 = load i1, ptr %alloca_71
  br i1 %load_75, label %bb15, label %bb16
bb10:
  %load_76 = load i64, ptr %alloca_42
  %add_77 = add i64 %load_76, 1
  store i64 %add_77, ptr %alloca_42
  br label %bb1
bb15:
  store i64 %arg0, ptr %alloca_40
  br label %bb14
bb16:
  br label %bb17
bb14:
  %load_80 = load i64, ptr %alloca_40
  ret i64 %load_80
bb17:
  %alloca_81 = alloca i64, align 8
  %load_82 = load i64, ptr %alloca_42
  %srem_83 = srem i64 %arg0, %load_82
  store i64 %srem_83, ptr %alloca_81
  %alloca_85 = alloca i1, align 1
  %load_86 = load i64, ptr %alloca_81
  %icmp_87 = icmp eq i64 %load_86, 0
  store i1 %icmp_87, ptr %alloca_85
  %load_89 = load i1, ptr %alloca_85
  br i1 %load_89, label %bb19, label %bb20
bb19:
  %load_90 = load i64, ptr %alloca_42
  store i64 %load_90, ptr %alloca_40
  br label %bb14
bb20:
  br label %bb21
bb21:
  %load_92 = load i64, ptr %alloca_42
  %add_93 = add i64 %load_92, 1
  store i64 %add_93, ptr %alloca_42
  br label %bb12
bb7:
  br label %bb6
bb11:
  br label %bb10
bb18:
  br label %bb17
bb22:
  br label %bb21
}

define i32 @main() {
bb0:
  %alloca_124 = alloca i64, align 8
  %alloca_125 = alloca i64, align 8
  %alloca_126 = alloca i64, align 8
  %alloca_127 = alloca i64, align 8
  %call_128 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.0)
  br label %bb1
bb1:
  %call_129 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.1)
  br label %bb2
bb2:
  %call_130 = call i64 (i64) @factorial(i64 5)
  br label %bb3
bb3:
  %call_131 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.2, i64 %call_130)
  br label %bb4
bb4:
  %call_132 = call i64 (i64) @factorial(i64 7)
  br label %bb5
bb5:
  %call_133 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.3, i64 %call_132)
  br label %bb6
bb6:
  %call_134 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.4)
  br label %bb7
bb7:
  %call_135 = call i64 (i64, i64) @sum_range(i64 1, i64 10)
  br label %bb8
bb8:
  %call_136 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.5, i64 %call_135)
  br label %bb9
bb9:
  %call_137 = call i64 (i64, i64) @sum_range(i64 5, i64 15)
  br label %bb10
bb10:
  %call_138 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.6, i64 %call_137)
  br label %bb11
bb11:
  %call_139 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.7)
  br label %bb12
bb12:
  %call_140 = call i64 (i64) @find_first_divisor(i64 24)
  br label %bb13
bb13:
  %call_141 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.8, i64 %call_140)
  br label %bb14
bb14:
  %call_142 = call i64 (i64) @find_first_divisor(i64 17)
  br label %bb15
bb15:
  %call_143 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.9, i64 %call_142)
  br label %bb16
bb16:
  %call_144 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.10)
  br label %bb17
bb17:
  %call_145 = call i64 (i64) @sum_even_numbers(i64 10)
  br label %bb18
bb18:
  %call_146 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.11, i64 %call_145)
  br label %bb19
bb19:
  %call_147 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.12)
  br label %bb20
bb20:
  store i64 0, ptr %alloca_124
  store i64 1, ptr %alloca_126
  br label %bb21
bb21:
  %alloca_150 = alloca i1, align 1
  %load_151 = load i64, ptr %alloca_126
  %icmp_152 = icmp slt i64 %load_151, 4
  store i1 %icmp_152, ptr %alloca_150
  %load_154 = load i1, ptr %alloca_150
  br i1 %load_154, label %bb22, label %bb23
bb22:
  store i64 1, ptr %alloca_127
  br label %bb24
bb23:
  %load_156 = load i64, ptr %alloca_124
  %call_157 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.13, i64 %load_156)
  br label %bb42
bb24:
  %alloca_158 = alloca i1, align 1
  %load_159 = load i64, ptr %alloca_127
  %icmp_160 = icmp slt i64 %load_159, 4
  store i1 %icmp_160, ptr %alloca_158
  %load_162 = load i1, ptr %alloca_158
  br i1 %load_162, label %bb25, label %bb26
bb42:
  %call_163 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.14)
  br label %bb43
bb25:
  %load_164 = load i64, ptr %alloca_124
  %add_165 = add i64 %load_164, 1
  store i64 %add_165, ptr %alloca_124
  %alloca_167 = alloca i1, align 1
  %load_168 = load i64, ptr %alloca_126
  %load_169 = load i64, ptr %alloca_127
  %icmp_170 = icmp eq i64 %load_168, %load_169
  store i1 %icmp_170, ptr %alloca_167
  %load_172 = load i1, ptr %alloca_167
  br i1 %load_172, label %bb27, label %bb28
bb26:
  store i64 1, ptr %alloca_125
  br label %bb33
bb43:
  %alloca_174 = alloca i64, align 8
  store i64 120, ptr %alloca_174
  %load_176 = load i64, ptr %alloca_174
  %call_177 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.15, i64 %load_176)
  br label %bb44
bb27:
  br label %bb29
bb28:
  br label %bb29
bb33:
  %alloca_178 = alloca i1, align 1
  %load_179 = load i64, ptr %alloca_125
  %icmp_180 = icmp slt i64 %load_179, 4
  store i1 %icmp_180, ptr %alloca_178
  %load_182 = load i1, ptr %alloca_178
  br i1 %load_182, label %bb34, label %bb35
bb44:
  %call_183 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.16)
  br label %bb45
bb29:
  %alloca_184 = alloca i1, align 1
  %load_185 = load i64, ptr %alloca_126
  %load_186 = load i64, ptr %alloca_127
  %icmp_187 = icmp eq i64 %load_185, %load_186
  store i1 %icmp_187, ptr %alloca_184
  %load_189 = load i1, ptr %alloca_184
  br i1 %load_189, label %bb30, label %bb31
bb34:
  %load_190 = load i64, ptr %alloca_124
  %add_191 = add i64 %load_190, 1
  store i64 %add_191, ptr %alloca_124
  %alloca_193 = alloca i1, align 1
  %load_194 = load i64, ptr %alloca_126
  %load_195 = load i64, ptr %alloca_125
  %icmp_196 = icmp eq i64 %load_194, %load_195
  store i1 %icmp_196, ptr %alloca_193
  %load_198 = load i1, ptr %alloca_193
  br i1 %load_198, label %bb36, label %bb37
bb35:
  %load_199 = load i64, ptr %alloca_126
  %add_200 = add i64 %load_199, 1
  store i64 %add_200, ptr %alloca_126
  br label %bb21
bb45:
  ret i32 0
bb30:
  br label %bb32
bb31:
  br label %bb32
bb36:
  br label %bb38
bb37:
  br label %bb38
bb32:
  %load_202 = load i64, ptr %alloca_127
  %add_203 = add i64 %load_202, 1
  store i64 %add_203, ptr %alloca_127
  br label %bb24
bb38:
  %alloca_205 = alloca i1, align 1
  %load_206 = load i64, ptr %alloca_126
  %load_207 = load i64, ptr %alloca_125
  %icmp_208 = icmp eq i64 %load_206, %load_207
  store i1 %icmp_208, ptr %alloca_205
  %load_210 = load i1, ptr %alloca_205
  br i1 %load_210, label %bb39, label %bb40
bb39:
  br label %bb41
bb40:
  br label %bb41
bb41:
  %load_211 = load i64, ptr %alloca_125
  %add_212 = add i64 %load_211, 1
  store i64 %add_212, ptr %alloca_125
  br label %bb33
}

define i64 @sum_even_numbers(i64 %arg0) {
bb0:
  %alloca_95 = alloca i64, align 8
  %alloca_96 = alloca i64, align 8
  %alloca_97 = alloca i64, align 8
  store i64 0, ptr %alloca_96
  store i64 0, ptr %alloca_95
  br label %bb1
bb1:
  %alloca_100 = alloca i1, align 1
  %load_101 = load i64, ptr %alloca_95
  %icmp_102 = icmp slt i64 %load_101, %arg0
  store i1 %icmp_102, ptr %alloca_100
  %load_104 = load i1, ptr %alloca_100
  br i1 %load_104, label %bb2, label %bb3
bb2:
  %load_105 = load i64, ptr %alloca_95
  %add_106 = add i64 %load_105, 1
  store i64 %add_106, ptr %alloca_95
  %alloca_108 = alloca i64, align 8
  %load_109 = load i64, ptr %alloca_95
  %srem_110 = srem i64 %load_109, 2
  store i64 %srem_110, ptr %alloca_108
  %alloca_112 = alloca i1, align 1
  %load_113 = load i64, ptr %alloca_108
  %icmp_114 = icmp ne i64 %load_113, 0
  store i1 %icmp_114, ptr %alloca_112
  %load_116 = load i1, ptr %alloca_112
  br i1 %load_116, label %bb4, label %bb5
bb3:
  %load_117 = load i64, ptr %alloca_96
  store i64 %load_117, ptr %alloca_97
  %load_119 = load i64, ptr %alloca_97
  ret i64 %load_119
bb4:
  br label %bb1
bb5:
  br label %bb6
bb6:
  %load_120 = load i64, ptr %alloca_96
  %load_121 = load i64, ptr %alloca_95
  %add_122 = add i64 %load_120, %load_121
  store i64 %add_122, ptr %alloca_96
  br label %bb1
bb7:
  br label %bb6
}

define i64 @sum_range(i64 %arg0, i64 %arg1) {
bb0:
  %alloca_20 = alloca i64, align 8
  %alloca_21 = alloca i64, align 8
  %alloca_22 = alloca i64, align 8
  store i64 0, ptr %alloca_21
  store i64 %arg0, ptr %alloca_22
  br label %bb1
bb1:
  %alloca_25 = alloca i1, align 1
  %load_26 = load i64, ptr %alloca_22
  %icmp_27 = icmp slt i64 %load_26, %arg1
  store i1 %icmp_27, ptr %alloca_25
  %load_29 = load i1, ptr %alloca_25
  br i1 %load_29, label %bb2, label %bb3
bb2:
  %load_30 = load i64, ptr %alloca_21
  %load_31 = load i64, ptr %alloca_22
  %add_32 = add i64 %load_30, %load_31
  store i64 %add_32, ptr %alloca_21
  %load_34 = load i64, ptr %alloca_22
  %add_35 = add i64 %load_34, 1
  store i64 %add_35, ptr %alloca_22
  br label %bb1
bb3:
  %load_37 = load i64, ptr %alloca_21
  store i64 %load_37, ptr %alloca_20
  %load_39 = load i64, ptr %alloca_20
  ret i64 %load_39
}

