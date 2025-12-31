; ModuleID = '06_struct_methods'
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.06_struct_methods.0 = constant [27 x i8] [i8 61, i8 61, i8 61, i8 32, i8 83, i8 116, i8 114, i8 117, i8 99, i8 116, i8 32, i8 79, i8 112, i8 101, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 115, i8 32, i8 61, i8 61, i8 61, i8 10, i8 0], align 1
@.str.06_struct_methods.1 = constant [15 x i8] [i8 112, i8 49, i8 32, i8 61, i8 32, i8 40, i8 37, i8 100, i8 44, i8 32, i8 37, i8 100, i8 41, i8 10, i8 0], align 1
@.str.06_struct_methods.2 = constant [15 x i8] [i8 112, i8 50, i8 32, i8 61, i8 32, i8 40, i8 37, i8 100, i8 44, i8 32, i8 37, i8 100, i8 41, i8 10, i8 0], align 1
@.str.06_struct_methods.3 = constant [31 x i8] [i8 112, i8 49, i8 32, i8 97, i8 102, i8 116, i8 101, i8 114, i8 32, i8 116, i8 114, i8 97, i8 110, i8 115, i8 108, i8 97, i8 116, i8 101, i8 32, i8 61, i8 32, i8 40, i8 37, i8 100, i8 44, i8 32, i8 37, i8 100, i8 41, i8 10, i8 0], align 1
@.str.06_struct_methods.4 = constant [25 x i8] [i8 68, i8 105, i8 115, i8 116, i8 97, i8 110, i8 99, i8 101, i8 194, i8 178, i8 40, i8 112, i8 49, i8 44, i8 32, i8 112, i8 50, i8 41, i8 32, i8 61, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.06_struct_methods.5 = constant [19 x i8] [i8 82, i8 101, i8 99, i8 116, i8 97, i8 110, i8 103, i8 108, i8 101, i8 58, i8 32, i8 37, i8 100, i8 195, i8 151, i8 37, i8 100, i8 10, i8 0], align 1
@.str.06_struct_methods.6 = constant [13 x i8] [i8 32, i8 32, i8 97, i8 114, i8 101, i8 97, i8 32, i8 61, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.06_struct_methods.7 = constant [18 x i8] [i8 32, i8 32, i8 112, i8 101, i8 114, i8 105, i8 109, i8 101, i8 116, i8 101, i8 114, i8 32, i8 61, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.06_struct_methods.8 = constant [18 x i8] [i8 32, i8 32, i8 105, i8 115, i8 95, i8 115, i8 113, i8 117, i8 97, i8 114, i8 101, i8 32, i8 61, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i64 @area(ptr %arg0) {
bb0:
  %alloca_83 = alloca i64, align 8
  %alloca_84 = alloca i64, align 8
  %bitcast_85 = bitcast ptr %arg0 to ptr
  %load_86 = load i64, ptr %bitcast_85
  %bitcast_87 = bitcast ptr %arg0 to ptr
  %gep_88 = getelementptr inbounds i8, ptr %bitcast_87, i64 8
  %bitcast_89 = bitcast ptr %gep_88 to ptr
  %load_90 = load i64, ptr %bitcast_89
  %mul_91 = mul i64 %load_86, %load_90
  store i64 %mul_91, ptr %alloca_84
  %bitcast_93 = bitcast ptr %arg0 to ptr
  %load_94 = load i64, ptr %bitcast_93
  %bitcast_95 = bitcast ptr %arg0 to ptr
  %gep_96 = getelementptr inbounds i8, ptr %bitcast_95, i64 8
  %bitcast_97 = bitcast ptr %gep_96 to ptr
  %load_98 = load i64, ptr %bitcast_97
  %mul_99 = mul i64 %load_94, %load_98
  store i64 %mul_99, ptr %alloca_83
  %load_101 = load i64, ptr %alloca_83
  ret i64 %load_101
}

define i64 @distance2(ptr %arg0, ptr %arg1) {
bb0:
  %alloca_21 = alloca i64, align 8
  %alloca_22 = alloca i64, align 8
  %bitcast_23 = bitcast ptr %arg0 to ptr
  %load_24 = load i64, ptr %bitcast_23
  %bitcast_25 = bitcast ptr %arg1 to ptr
  %load_26 = load i64, ptr %bitcast_25
  %sub_27 = sub i64 %load_24, %load_26
  store i64 %sub_27, ptr %alloca_22
  %alloca_29 = alloca i64, align 8
  %load_30 = load i64, ptr %alloca_22
  store i64 %load_30, ptr %alloca_29
  %alloca_32 = alloca i64, align 8
  %bitcast_33 = bitcast ptr %arg0 to ptr
  %gep_34 = getelementptr inbounds i8, ptr %bitcast_33, i64 8
  %bitcast_35 = bitcast ptr %gep_34 to ptr
  %load_36 = load i64, ptr %bitcast_35
  %bitcast_37 = bitcast ptr %arg1 to ptr
  %gep_38 = getelementptr inbounds i8, ptr %bitcast_37, i64 8
  %bitcast_39 = bitcast ptr %gep_38 to ptr
  %load_40 = load i64, ptr %bitcast_39
  %sub_41 = sub i64 %load_36, %load_40
  store i64 %sub_41, ptr %alloca_32
  %alloca_43 = alloca i64, align 8
  %load_44 = load i64, ptr %alloca_32
  store i64 %load_44, ptr %alloca_43
  %alloca_46 = alloca i64, align 8
  %load_47 = load i64, ptr %alloca_29
  %load_48 = load i64, ptr %alloca_29
  %mul_49 = mul i64 %load_47, %load_48
  store i64 %mul_49, ptr %alloca_46
  %alloca_51 = alloca i64, align 8
  %load_52 = load i64, ptr %alloca_43
  %load_53 = load i64, ptr %alloca_43
  %mul_54 = mul i64 %load_52, %load_53
  store i64 %mul_54, ptr %alloca_51
  %alloca_56 = alloca i64, align 8
  %load_57 = load i64, ptr %alloca_46
  %load_58 = load i64, ptr %alloca_51
  %add_59 = add i64 %load_57, %load_58
  store i64 %add_59, ptr %alloca_56
  %alloca_61 = alloca i64, align 8
  %load_62 = load i64, ptr %alloca_29
  %load_63 = load i64, ptr %alloca_29
  %mul_64 = mul i64 %load_62, %load_63
  store i64 %mul_64, ptr %alloca_61
  %alloca_66 = alloca i64, align 8
  %load_67 = load i64, ptr %alloca_43
  %load_68 = load i64, ptr %alloca_43
  %mul_69 = mul i64 %load_67, %load_68
  store i64 %mul_69, ptr %alloca_66
  %load_71 = load i64, ptr %alloca_61
  %load_72 = load i64, ptr %alloca_66
  %add_73 = add i64 %load_71, %load_72
  store i64 %add_73, ptr %alloca_21
  %load_75 = load i64, ptr %alloca_21
  ret i64 %load_75
}

define i1 @is_square(ptr %arg0) {
bb0:
  %alloca_129 = alloca i1, align 1
  %alloca_130 = alloca i1, align 1
  %bitcast_131 = bitcast ptr %arg0 to ptr
  %load_132 = load i64, ptr %bitcast_131
  %bitcast_133 = bitcast ptr %arg0 to ptr
  %gep_134 = getelementptr inbounds i8, ptr %bitcast_133, i64 8
  %bitcast_135 = bitcast ptr %gep_134 to ptr
  %load_136 = load i64, ptr %bitcast_135
  %icmp_137 = icmp eq i64 %load_132, %load_136
  store i1 %icmp_137, ptr %alloca_130
  %bitcast_139 = bitcast ptr %arg0 to ptr
  %load_140 = load i64, ptr %bitcast_139
  %bitcast_141 = bitcast ptr %arg0 to ptr
  %gep_142 = getelementptr inbounds i8, ptr %bitcast_141, i64 8
  %bitcast_143 = bitcast ptr %gep_142 to ptr
  %load_144 = load i64, ptr %bitcast_143
  %icmp_145 = icmp eq i64 %load_140, %load_144
  store i1 %icmp_145, ptr %alloca_129
  %load_147 = load i1, ptr %alloca_129
  ret i1 %load_147
}

define i32 @main() {
bb0:
  %call_148 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.0)
  br label %bb1
bb1:
  %call_149 = call { i64, i64 } (i64, i64) @new__1(i64 10, i64 20)
  br label %bb2
bb2:
  %call_150 = call { i64, i64 } (i64, i64) @new__1(i64 5, i64 15)
  br label %bb3
bb3:
  %alloca_151 = alloca { i64, i64 }, align 8
  store { i64, i64 } %call_149, ptr %alloca_151
  %bitcast_153 = bitcast ptr %alloca_151 to ptr
  %load_154 = load i64, ptr %bitcast_153
  %alloca_155 = alloca { i64, i64 }, align 8
  store { i64, i64 } %call_149, ptr %alloca_155
  %bitcast_157 = bitcast ptr %alloca_155 to ptr
  %gep_158 = getelementptr inbounds i8, ptr %bitcast_157, i64 8
  %bitcast_159 = bitcast ptr %gep_158 to ptr
  %load_160 = load i64, ptr %bitcast_159
  %call_161 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.1, i64 %load_154, i64 %load_160)
  br label %bb4
bb4:
  %alloca_162 = alloca { i64, i64 }, align 8
  store { i64, i64 } %call_150, ptr %alloca_162
  %bitcast_164 = bitcast ptr %alloca_162 to ptr
  %load_165 = load i64, ptr %bitcast_164
  %alloca_166 = alloca { i64, i64 }, align 8
  store { i64, i64 } %call_150, ptr %alloca_166
  %bitcast_168 = bitcast ptr %alloca_166 to ptr
  %gep_169 = getelementptr inbounds i8, ptr %bitcast_168, i64 8
  %bitcast_170 = bitcast ptr %gep_169 to ptr
  %load_171 = load i64, ptr %bitcast_170
  %call_172 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.2, i64 %load_165, i64 %load_171)
  br label %bb5
bb5:
  %alloca_173 = alloca ptr, align 8
  store { i64, i64 } %call_149, ptr %alloca_173
  %alloca_175 = alloca i64, align 8
  %sub_176 = sub i64 0, 4
  store i64 %sub_176, ptr %alloca_175
  %load_178 = load i64, ptr %alloca_175
  call void (ptr, i64, i64) @translate(ptr %alloca_173, i64 3, i64 %load_178)
  br label %bb6
bb6:
  %alloca_180 = alloca { i64, i64 }, align 8
  store { i64, i64 } %call_149, ptr %alloca_180
  %bitcast_182 = bitcast ptr %alloca_180 to ptr
  %load_183 = load i64, ptr %bitcast_182
  %alloca_184 = alloca { i64, i64 }, align 8
  store { i64, i64 } %call_149, ptr %alloca_184
  %bitcast_186 = bitcast ptr %alloca_184 to ptr
  %gep_187 = getelementptr inbounds i8, ptr %bitcast_186, i64 8
  %bitcast_188 = bitcast ptr %gep_187 to ptr
  %load_189 = load i64, ptr %bitcast_188
  %call_190 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.3, i64 %load_183, i64 %load_189)
  br label %bb7
bb7:
  %alloca_191 = alloca ptr, align 8
  store { i64, i64 } %call_149, ptr %alloca_191
  %alloca_193 = alloca ptr, align 8
  store { i64, i64 } %call_150, ptr %alloca_193
  %call_195 = call i64 (ptr, ptr) @distance2(ptr %alloca_191, ptr %alloca_193)
  br label %bb8
bb8:
  %call_196 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.4, i64 %call_195)
  br label %bb9
bb9:
  %call_197 = call { i64, i64 } (i64, i64) @new__1(i64 10, i64 5)
  br label %bb10
bb10:
  %alloca_198 = alloca { i64, i64 }, align 8
  store { i64, i64 } %call_197, ptr %alloca_198
  %bitcast_200 = bitcast ptr %alloca_198 to ptr
  %load_201 = load i64, ptr %bitcast_200
  %alloca_202 = alloca { i64, i64 }, align 8
  store { i64, i64 } %call_197, ptr %alloca_202
  %bitcast_204 = bitcast ptr %alloca_202 to ptr
  %gep_205 = getelementptr inbounds i8, ptr %bitcast_204, i64 8
  %bitcast_206 = bitcast ptr %gep_205 to ptr
  %load_207 = load i64, ptr %bitcast_206
  %call_208 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.5, i64 %load_201, i64 %load_207)
  br label %bb11
bb11:
  %alloca_209 = alloca ptr, align 8
  store { i64, i64 } %call_197, ptr %alloca_209
  %call_211 = call i64 (ptr) @area(ptr %alloca_209)
  br label %bb12
bb12:
  %call_212 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.6, i64 %call_211)
  br label %bb13
bb13:
  %alloca_213 = alloca ptr, align 8
  store { i64, i64 } %call_197, ptr %alloca_213
  %call_215 = call i64 (ptr) @perimeter(ptr %alloca_213)
  br label %bb14
bb14:
  %call_216 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.7, i64 %call_215)
  br label %bb15
bb15:
  %alloca_217 = alloca ptr, align 8
  store { i64, i64 } %call_197, ptr %alloca_217
  %call_219 = call i1 (ptr) @is_square(ptr %alloca_217)
  br label %bb16
bb16:
  %zext_220 = zext i1 %call_219 to i32
  %call_221 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.8, i32 %zext_220)
  br label %bb17
bb17:
  ret i32 0
}

define { i64, i64 } @new(i64 %arg0, i64 %arg1) {
bb0:
  %alloca_0 = alloca { i64, i64 }, align 8
  %insertvalue_1 = insertvalue { i64, i64 } undef, i64 %arg0, 0
  %insertvalue_2 = insertvalue { i64, i64 } %insertvalue_1, i64 %arg1, 1
  %insertvalue_3 = insertvalue { i64, i64 } undef, i64 %arg0, 0
  %insertvalue_4 = insertvalue { i64, i64 } %insertvalue_3, i64 %arg1, 1
  store { i64, i64 } %insertvalue_4, ptr %alloca_0
  %load_6 = load { i64, i64 }, ptr %alloca_0
  ret { i64, i64 } %load_6
}

define { i64, i64 } @new__1(i64 %arg0, i64 %arg1) {
bb0:
  %alloca_76 = alloca { i64, i64 }, align 8
  %insertvalue_77 = insertvalue { i64, i64 } undef, i64 %arg0, 0
  %insertvalue_78 = insertvalue { i64, i64 } %insertvalue_77, i64 %arg1, 1
  %insertvalue_79 = insertvalue { i64, i64 } undef, i64 %arg0, 0
  %insertvalue_80 = insertvalue { i64, i64 } %insertvalue_79, i64 %arg1, 1
  store { i64, i64 } %insertvalue_80, ptr %alloca_76
  %load_82 = load { i64, i64 }, ptr %alloca_76
  ret { i64, i64 } %load_82
}

define i64 @perimeter(ptr %arg0) {
bb0:
  %alloca_102 = alloca i64, align 8
  %alloca_103 = alloca i64, align 8
  %bitcast_104 = bitcast ptr %arg0 to ptr
  %load_105 = load i64, ptr %bitcast_104
  %bitcast_106 = bitcast ptr %arg0 to ptr
  %gep_107 = getelementptr inbounds i8, ptr %bitcast_106, i64 8
  %bitcast_108 = bitcast ptr %gep_107 to ptr
  %load_109 = load i64, ptr %bitcast_108
  %add_110 = add i64 %load_105, %load_109
  store i64 %add_110, ptr %alloca_103
  %alloca_112 = alloca i64, align 8
  %load_113 = load i64, ptr %alloca_103
  %mul_114 = mul i64 2, %load_113
  store i64 %mul_114, ptr %alloca_112
  %alloca_116 = alloca i64, align 8
  %bitcast_117 = bitcast ptr %arg0 to ptr
  %load_118 = load i64, ptr %bitcast_117
  %bitcast_119 = bitcast ptr %arg0 to ptr
  %gep_120 = getelementptr inbounds i8, ptr %bitcast_119, i64 8
  %bitcast_121 = bitcast ptr %gep_120 to ptr
  %load_122 = load i64, ptr %bitcast_121
  %add_123 = add i64 %load_118, %load_122
  store i64 %add_123, ptr %alloca_116
  %load_125 = load i64, ptr %alloca_116
  %mul_126 = mul i64 2, %load_125
  store i64 %mul_126, ptr %alloca_102
  %load_128 = load i64, ptr %alloca_102
  ret i64 %load_128
}

define void @translate(ptr %arg0, i64 %arg1, i64 %arg2) {
bb0:
  %bitcast_7 = bitcast ptr %arg0 to ptr
  %bitcast_8 = bitcast ptr %arg0 to ptr
  %load_9 = load i64, ptr %bitcast_8
  %add_10 = add i64 %load_9, %arg1
  store i64 %add_10, ptr %bitcast_7
  %bitcast_12 = bitcast ptr %arg0 to ptr
  %gep_13 = getelementptr inbounds i8, ptr %bitcast_12, i64 8
  %bitcast_14 = bitcast ptr %gep_13 to ptr
  %bitcast_15 = bitcast ptr %arg0 to ptr
  %gep_16 = getelementptr inbounds i8, ptr %bitcast_15, i64 8
  %bitcast_17 = bitcast ptr %gep_16 to ptr
  %load_18 = load i64, ptr %bitcast_17
  %add_19 = add i64 %load_18, %arg2
  store i64 %add_19, ptr %bitcast_14
  ret void
}

