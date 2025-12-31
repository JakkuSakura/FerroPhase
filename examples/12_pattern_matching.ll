; ModuleID = '12_pattern_matching'
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.12_pattern_matching.0 = constant [4 x i8] c"red\00", align 1
@.str.12_pattern_matching.1 = constant [6 x i8] c"green\00", align 1
@.str.12_pattern_matching.10 = constant [8 x i8] [i8 48, i8 120, i8 37, i8 48, i8 54, i8 88, i8 10, i8 0], align 1
@.str.12_pattern_matching.2 = constant [8 x i8] c"red rgb\00", align 1
@.str.12_pattern_matching.3 = constant [11 x i8] c"custom rgb\00", align 1
@.str.12_pattern_matching.4 = constant [5 x i8] c"zero\00", align 1
@.str.12_pattern_matching.5 = constant [9 x i8] c"negative\00", align 1
@.str.12_pattern_matching.6 = constant [5 x i8] c"even\00", align 1
@.str.12_pattern_matching.7 = constant [4 x i8] c"odd\00", align 1
@.str.12_pattern_matching.8 = constant [4 x i8] [i8 37, i8 115, i8 10, i8 0], align 1
@.str.12_pattern_matching.9 = constant [4 x i8] [i8 37, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define ptr @Color__Rgb(i64 %arg0, i64 %arg1, i64 %arg2) {
bb0:
  %alloca_217 = alloca ptr, align 8
  store i64 0, ptr %alloca_217
  %load_219 = load ptr, ptr %alloca_217
  ret ptr %load_219
}

define i64 @Option__Some(i64 %arg0) {
bb0:
  %alloca_220 = alloca i64, align 8
  store i64 0, ptr %alloca_220
  %load_222 = load i64, ptr %alloca_220
  ret i64 %load_222
}

define ptr @classify(i64 %arg0) {
bb0:
  %alloca_76 = alloca ptr, align 8
  %alloca_77 = alloca ptr, align 8
  %alloca_78 = alloca i64, align 8
  store i64 %arg0, ptr %alloca_78
  %alloca_80 = alloca i1, align 1
  %load_81 = load i64, ptr %alloca_78
  %icmp_82 = icmp eq i64 %load_81, 0
  store i1 %icmp_82, ptr %alloca_80
  %load_84 = load i1, ptr %alloca_80
  br i1 %load_84, label %bb2, label %bb3
bb2:
  store ptr @.str.12_pattern_matching.4, ptr %alloca_76
  br label %bb1
bb3:
  br label %bb4
bb1:
  %alloca_86 = alloca i64, align 8
  store i64 %arg0, ptr %alloca_86
  %alloca_88 = alloca i1, align 1
  %load_89 = load i64, ptr %alloca_86
  %icmp_90 = icmp eq i64 %load_89, 0
  store i1 %icmp_90, ptr %alloca_88
  %load_92 = load i1, ptr %alloca_88
  br i1 %load_92, label %bb13, label %bb14
bb4:
  %alloca_93 = alloca i64, align 8
  %load_94 = load i64, ptr %alloca_78
  store i64 %load_94, ptr %alloca_93
  %alloca_96 = alloca i1, align 1
  %load_97 = load i64, ptr %alloca_93
  %icmp_98 = icmp slt i64 %load_97, 0
  store i1 %icmp_98, ptr %alloca_96
  %load_100 = load i1, ptr %alloca_96
  br i1 %load_100, label %bb6, label %bb5
bb13:
  store ptr @.str.12_pattern_matching.4, ptr %alloca_77
  br label %bb12
bb14:
  br label %bb15
bb5:
  br label %bb7
bb6:
  store ptr @.str.12_pattern_matching.5, ptr %alloca_76
  br label %bb1
bb12:
  %load_103 = load ptr, ptr %alloca_77
  ret ptr %load_103
bb15:
  %alloca_104 = alloca i64, align 8
  %load_105 = load i64, ptr %alloca_86
  store i64 %load_105, ptr %alloca_104
  %alloca_107 = alloca i1, align 1
  %load_108 = load i64, ptr %alloca_104
  %icmp_109 = icmp slt i64 %load_108, 0
  store i1 %icmp_109, ptr %alloca_107
  %load_111 = load i1, ptr %alloca_107
  br i1 %load_111, label %bb17, label %bb16
bb7:
  %alloca_112 = alloca i64, align 8
  %load_113 = load i64, ptr %alloca_78
  store i64 %load_113, ptr %alloca_112
  %alloca_115 = alloca i64, align 8
  %load_116 = load i64, ptr %alloca_112
  %srem_117 = srem i64 %load_116, 2
  store i64 %srem_117, ptr %alloca_115
  %alloca_119 = alloca i1, align 1
  %load_120 = load i64, ptr %alloca_115
  %icmp_121 = icmp eq i64 %load_120, 0
  store i1 %icmp_121, ptr %alloca_119
  %load_123 = load i1, ptr %alloca_119
  br i1 %load_123, label %bb9, label %bb8
bb16:
  br label %bb18
bb17:
  store ptr @.str.12_pattern_matching.5, ptr %alloca_77
  br label %bb12
bb8:
  br label %bb10
bb9:
  store ptr @.str.12_pattern_matching.6, ptr %alloca_76
  br label %bb1
bb18:
  %alloca_126 = alloca i64, align 8
  %load_127 = load i64, ptr %alloca_86
  store i64 %load_127, ptr %alloca_126
  %alloca_129 = alloca i64, align 8
  %load_130 = load i64, ptr %alloca_126
  %srem_131 = srem i64 %load_130, 2
  store i64 %srem_131, ptr %alloca_129
  %alloca_133 = alloca i1, align 1
  %load_134 = load i64, ptr %alloca_129
  %icmp_135 = icmp eq i64 %load_134, 0
  store i1 %icmp_135, ptr %alloca_133
  %load_137 = load i1, ptr %alloca_133
  br i1 %load_137, label %bb20, label %bb19
bb10:
  store ptr @.str.12_pattern_matching.7, ptr %alloca_76
  br label %bb1
bb19:
  br label %bb21
bb20:
  store ptr @.str.12_pattern_matching.6, ptr %alloca_77
  br label %bb12
bb21:
  store ptr @.str.12_pattern_matching.7, ptr %alloca_77
  br label %bb12
bb11:
  %load_141 = load ptr, ptr %alloca_77
  ret ptr %load_141
bb22:
  store i1 0, ptr %alloca_77
  %load_143 = load ptr, ptr %alloca_77
  ret ptr %load_143
}

define ptr @describe(ptr %arg0) {
bb0:
  %alloca_0 = alloca ptr, align 8
  %alloca_1 = alloca ptr, align 8
  %alloca_2 = alloca ptr, align 8
  store ptr %arg0, ptr %alloca_2
  %alloca_4 = alloca i1, align 1
  %load_5 = load ptr, ptr %alloca_2
  %icmp_6 = icmp eq ptr %load_5, 0
  store i1 %icmp_6, ptr %alloca_4
  %load_8 = load i1, ptr %alloca_4
  br i1 %load_8, label %bb2, label %bb3
bb2:
  store ptr @.str.12_pattern_matching.0, ptr %alloca_1
  br label %bb1
bb3:
  %alloca_10 = alloca i1, align 1
  %load_11 = load ptr, ptr %alloca_2
  %icmp_12 = icmp eq ptr %load_11, 1
  store i1 %icmp_12, ptr %alloca_10
  %load_14 = load i1, ptr %alloca_10
  br i1 %load_14, label %bb4, label %bb5
bb1:
  %alloca_15 = alloca ptr, align 8
  store ptr %arg0, ptr %alloca_15
  %alloca_17 = alloca i1, align 1
  %load_18 = load ptr, ptr %alloca_15
  %icmp_19 = icmp eq ptr %load_18, 0
  store i1 %icmp_19, ptr %alloca_17
  %load_21 = load i1, ptr %alloca_17
  br i1 %load_21, label %bb11, label %bb12
bb4:
  store ptr @.str.12_pattern_matching.1, ptr %alloca_1
  br label %bb1
bb5:
  %alloca_23 = alloca i1, align 1
  %load_24 = load ptr, ptr %alloca_2
  %icmp_25 = icmp eq ptr %load_24, 2
  store i1 %icmp_25, ptr %alloca_23
  %load_27 = load i1, ptr %alloca_23
  br i1 %load_27, label %bb6, label %bb7
bb11:
  store ptr @.str.12_pattern_matching.0, ptr %alloca_0
  br label %bb10
bb12:
  %alloca_29 = alloca i1, align 1
  %load_30 = load ptr, ptr %alloca_15
  %icmp_31 = icmp eq ptr %load_30, 1
  store i1 %icmp_31, ptr %alloca_29
  %load_33 = load i1, ptr %alloca_29
  br i1 %load_33, label %bb13, label %bb14
bb6:
  store ptr @.str.12_pattern_matching.2, ptr %alloca_1
  br label %bb1
bb7:
  %alloca_35 = alloca i1, align 1
  %load_36 = load ptr, ptr %alloca_2
  %icmp_37 = icmp eq ptr %load_36, 2
  store i1 %icmp_37, ptr %alloca_35
  %load_39 = load i1, ptr %alloca_35
  br i1 %load_39, label %bb8, label %bb9
bb10:
  %load_40 = load ptr, ptr %alloca_0
  ret ptr %load_40
bb13:
  store ptr @.str.12_pattern_matching.1, ptr %alloca_0
  br label %bb10
bb14:
  %alloca_42 = alloca i1, align 1
  %load_43 = load ptr, ptr %alloca_15
  %icmp_44 = icmp eq ptr %load_43, 2
  store i1 %icmp_44, ptr %alloca_42
  %load_46 = load i1, ptr %alloca_42
  br i1 %load_46, label %bb15, label %bb16
bb8:
  %alloca_47 = alloca ptr, align 8
  %load_48 = load ptr, ptr %alloca_2
  store ptr %load_48, ptr %alloca_47
  %alloca_50 = alloca ptr, align 8
  %load_51 = load ptr, ptr %alloca_2
  store ptr %load_51, ptr %alloca_50
  %alloca_53 = alloca ptr, align 8
  %load_54 = load ptr, ptr %alloca_2
  store ptr %load_54, ptr %alloca_53
  store ptr @.str.12_pattern_matching.3, ptr %alloca_1
  br label %bb1
bb9:
  store {  } {  }, ptr %alloca_1
  br label %bb1
bb15:
  store ptr @.str.12_pattern_matching.2, ptr %alloca_0
  br label %bb10
bb16:
  %alloca_59 = alloca i1, align 1
  %load_60 = load ptr, ptr %alloca_15
  %icmp_61 = icmp eq ptr %load_60, 2
  store i1 %icmp_61, ptr %alloca_59
  %load_63 = load i1, ptr %alloca_59
  br i1 %load_63, label %bb17, label %bb18
bb17:
  %alloca_64 = alloca ptr, align 8
  %load_65 = load ptr, ptr %alloca_15
  store ptr %load_65, ptr %alloca_64
  %alloca_67 = alloca ptr, align 8
  %load_68 = load ptr, ptr %alloca_15
  store ptr %load_68, ptr %alloca_67
  %alloca_70 = alloca ptr, align 8
  %load_71 = load ptr, ptr %alloca_15
  store ptr %load_71, ptr %alloca_70
  store ptr @.str.12_pattern_matching.3, ptr %alloca_0
  br label %bb10
bb18:
  store {  } {  }, ptr %alloca_0
  store i1 0, ptr %alloca_0
  br label %bb10
}

define i32 @main() {
bb0:
  %alloca_185 = alloca i64, align 8
  store i64 0, ptr %alloca_185
  %call_187 = call ptr (i64, i64, i64) @Color__Rgb(i64 128, i64 64, i64 32)
  br label %bb1
bb1:
  %alloca_188 = alloca ptr, align 8
  store ptr %alloca_185, ptr %alloca_188
  %call_190 = call ptr (ptr) @describe(ptr %alloca_188)
  br label %bb2
bb2:
  %call_191 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_190)
  br label %bb3
bb3:
  %alloca_192 = alloca ptr, align 8
  store ptr %call_187, ptr %alloca_192
  %call_194 = call ptr (ptr) @describe(ptr %alloca_192)
  br label %bb4
bb4:
  %call_195 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_194)
  br label %bb5
bb5:
  %alloca_196 = alloca i64, align 8
  %sub_197 = sub i64 0, 5
  store i64 %sub_197, ptr %alloca_196
  %load_199 = load i64, ptr %alloca_196
  %call_200 = call ptr (i64) @classify(i64 %load_199)
  br label %bb6
bb6:
  %call_201 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_200)
  br label %bb7
bb7:
  %call_202 = call ptr (i64) @classify(i64 0)
  br label %bb8
bb8:
  %call_203 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_202)
  br label %bb9
bb9:
  %call_204 = call ptr (i64) @classify(i64 4)
  br label %bb10
bb10:
  %call_205 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_204)
  br label %bb11
bb11:
  %call_206 = call ptr (i64) @classify(i64 7)
  br label %bb12
bb12:
  %call_207 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_206)
  br label %bb13
bb13:
  %call_208 = call i64 (i64) @Option__Some(i64 42)
  br label %bb14
bb14:
  %call_209 = call i64 (i64, i64) @unwrap_or(i64 %call_208, i64 0)
  br label %bb15
bb15:
  %call_210 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.9, i64 %call_209)
  br label %bb16
bb16:
  %call_211 = call i64 (i64, i64) @unwrap_or(i64 1, i64 99)
  br label %bb17
bb17:
  %call_212 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.9, i64 %call_211)
  br label %bb18
bb18:
  %alloca_213 = alloca i64, align 8
  store i64 0, ptr %alloca_213
  %load_215 = load i64, ptr %alloca_213
  %call_216 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.10, i64 %load_215)
  br label %bb19
bb19:
  ret i32 0
}

define i64 @unwrap_or(i64 %arg0, i64 %arg1) {
bb0:
  %alloca_144 = alloca i64, align 8
  %alloca_145 = alloca i64, align 8
  %alloca_146 = alloca i64, align 8
  store i64 %arg0, ptr %alloca_146
  %alloca_148 = alloca i1, align 1
  %load_149 = load i64, ptr %alloca_146
  %icmp_150 = icmp eq i64 %load_149, 0
  store i1 %icmp_150, ptr %alloca_148
  %load_152 = load i1, ptr %alloca_148
  br i1 %load_152, label %bb2, label %bb3
bb2:
  %alloca_153 = alloca i64, align 8
  %load_154 = load i64, ptr %alloca_146
  store i64 %load_154, ptr %alloca_153
  %load_156 = load i64, ptr %alloca_153
  store i64 %load_156, ptr %alloca_144
  br label %bb1
bb3:
  %alloca_158 = alloca i1, align 1
  %load_159 = load i64, ptr %alloca_146
  %icmp_160 = icmp eq i64 %load_159, 1
  store i1 %icmp_160, ptr %alloca_158
  %load_162 = load i1, ptr %alloca_158
  br i1 %load_162, label %bb4, label %bb5
bb1:
  %alloca_163 = alloca i64, align 8
  store i64 %arg0, ptr %alloca_163
  %alloca_165 = alloca i1, align 1
  %load_166 = load i64, ptr %alloca_163
  %icmp_167 = icmp eq i64 %load_166, 0
  store i1 %icmp_167, ptr %alloca_165
  %load_169 = load i1, ptr %alloca_165
  br i1 %load_169, label %bb7, label %bb8
bb4:
  store i64 %arg1, ptr %alloca_144
  br label %bb1
bb5:
  store {  } {  }, ptr %alloca_144
  br label %bb1
bb7:
  %alloca_172 = alloca i64, align 8
  %load_173 = load i64, ptr %alloca_163
  store i64 %load_173, ptr %alloca_172
  %load_175 = load i64, ptr %alloca_172
  store i64 %load_175, ptr %alloca_145
  br label %bb6
bb8:
  %alloca_177 = alloca i1, align 1
  %load_178 = load i64, ptr %alloca_163
  %icmp_179 = icmp eq i64 %load_178, 1
  store i1 %icmp_179, ptr %alloca_177
  %load_181 = load i1, ptr %alloca_177
  br i1 %load_181, label %bb9, label %bb10
bb6:
  %load_182 = load i64, ptr %alloca_145
  ret i64 %load_182
bb9:
  store i64 %arg1, ptr %alloca_145
  br label %bb6
bb10:
  store {  } {  }, ptr %alloca_145
  br label %bb6
}

