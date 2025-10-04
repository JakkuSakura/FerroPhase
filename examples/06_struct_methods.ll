; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [27 x i8] [i8 61, i8 61, i8 61, i8 32, i8 83, i8 116, i8 114, i8 117, i8 99, i8 116, i8 32, i8 79, i8 112, i8 101, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 115, i8 32, i8 61, i8 61, i8 61, i8 10, i8 0], align 1
@.str.1 = constant [15 x i8] [i8 112, i8 49, i8 32, i8 61, i8 32, i8 40, i8 37, i8 100, i8 44, i8 32, i8 37, i8 100, i8 41, i8 10, i8 0], align 1
@.str.2 = constant [15 x i8] [i8 112, i8 50, i8 32, i8 61, i8 32, i8 40, i8 37, i8 100, i8 44, i8 32, i8 37, i8 100, i8 41, i8 10, i8 0], align 1
@.str.3 = constant [31 x i8] [i8 112, i8 49, i8 32, i8 97, i8 102, i8 116, i8 101, i8 114, i8 32, i8 116, i8 114, i8 97, i8 110, i8 115, i8 108, i8 97, i8 116, i8 101, i8 32, i8 61, i8 32, i8 40, i8 37, i8 100, i8 44, i8 32, i8 37, i8 100, i8 41, i8 10, i8 0], align 1
@.str.4 = constant [25 x i8] [i8 68, i8 105, i8 115, i8 116, i8 97, i8 110, i8 99, i8 101, i8 194, i8 178, i8 40, i8 112, i8 49, i8 44, i8 32, i8 112, i8 50, i8 41, i8 32, i8 61, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.5 = constant [19 x i8] [i8 82, i8 101, i8 99, i8 116, i8 97, i8 110, i8 103, i8 108, i8 101, i8 58, i8 32, i8 37, i8 100, i8 195, i8 151, i8 37, i8 100, i8 10, i8 0], align 1
@.str.6 = constant [13 x i8] [i8 32, i8 32, i8 97, i8 114, i8 101, i8 97, i8 32, i8 61, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.7 = constant [18 x i8] [i8 32, i8 32, i8 112, i8 101, i8 114, i8 105, i8 109, i8 101, i8 116, i8 101, i8 114, i8 32, i8 61, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.8 = constant [18 x i8] [i8 32, i8 32, i8 105, i8 115, i8 95, i8 115, i8 113, i8 117, i8 97, i8 114, i8 101, i8 32, i8 61, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define { i64, i64 } @new(i64 %arg0, i64 %arg1) {
bb0:
  %alloca_0 = alloca { i64, i64 }, align 8
  %insertvalue_1 = insertvalue { i64, i64 } undef, i64 %arg0, 0
  %insertvalue_2 = insertvalue { i64, i64 } %insertvalue_1, i64 %arg1, 1
  store { i64, i64 } %insertvalue_2, ptr %alloca_0
  ret { i64, i64 } %insertvalue_2
}

define void @translate(ptr %arg0, i64 %arg1, i64 %arg2) {
bb0:
  %bitcast_4 = bitcast ptr %arg0 to ptr
  %bitcast_5 = bitcast ptr %arg0 to ptr
  %load_6 = load i64, ptr %bitcast_5
  %add_7 = add i64 %load_6, %arg1
  store i64 %add_7, ptr %bitcast_4
  %bitcast_9 = bitcast ptr %arg0 to ptr
  %gep_10 = getelementptr inbounds i8, ptr %bitcast_9, i64 8
  %bitcast_11 = bitcast ptr %gep_10 to ptr
  %bitcast_12 = bitcast ptr %arg0 to ptr
  %gep_13 = getelementptr inbounds i8, ptr %bitcast_12, i64 8
  %bitcast_14 = bitcast ptr %gep_13 to ptr
  %load_15 = load i64, ptr %bitcast_14
  %add_16 = add i64 %load_15, %arg2
  store i64 %add_16, ptr %bitcast_11
  ret void
}

define i64 @distance2(ptr %arg0, ptr %arg1) {
bb0:
  %alloca_18 = alloca i64, align 8
  %alloca_19 = alloca i64, align 8
  %bitcast_20 = bitcast ptr %arg0 to ptr
  %load_21 = load i64, ptr %bitcast_20
  %bitcast_22 = bitcast ptr %arg1 to ptr
  %load_23 = load i64, ptr %bitcast_22
  %sub_24 = sub i64 %load_21, %load_23
  store i64 %sub_24, ptr %alloca_19
  %alloca_26 = alloca i64, align 8
  %load_27 = load i64, ptr %alloca_19
  store i64 %load_27, ptr %alloca_26
  %alloca_29 = alloca i64, align 8
  %bitcast_30 = bitcast ptr %arg0 to ptr
  %gep_31 = getelementptr inbounds i8, ptr %bitcast_30, i64 8
  %bitcast_32 = bitcast ptr %gep_31 to ptr
  %load_33 = load i64, ptr %bitcast_32
  %bitcast_34 = bitcast ptr %arg1 to ptr
  %gep_35 = getelementptr inbounds i8, ptr %bitcast_34, i64 8
  %bitcast_36 = bitcast ptr %gep_35 to ptr
  %load_37 = load i64, ptr %bitcast_36
  %sub_38 = sub i64 %load_33, %load_37
  store i64 %sub_38, ptr %alloca_29
  %alloca_40 = alloca i64, align 8
  %load_41 = load i64, ptr %alloca_29
  store i64 %load_41, ptr %alloca_40
  %alloca_43 = alloca i64, align 8
  %load_44 = load i64, ptr %alloca_26
  %load_45 = load i64, ptr %alloca_26
  %mul_46 = mul i64 %load_44, %load_45
  store i64 %mul_46, ptr %alloca_43
  %alloca_48 = alloca i64, align 8
  %load_49 = load i64, ptr %alloca_40
  %load_50 = load i64, ptr %alloca_40
  %mul_51 = mul i64 %load_49, %load_50
  store i64 %mul_51, ptr %alloca_48
  %alloca_53 = alloca i64, align 8
  %load_54 = load i64, ptr %alloca_43
  %load_55 = load i64, ptr %alloca_48
  %add_56 = add i64 %load_54, %load_55
  store i64 %add_56, ptr %alloca_53
  %alloca_58 = alloca i64, align 8
  %load_59 = load i64, ptr %alloca_26
  %load_60 = load i64, ptr %alloca_26
  %mul_61 = mul i64 %load_59, %load_60
  store i64 %mul_61, ptr %alloca_58
  %alloca_63 = alloca i64, align 8
  %load_64 = load i64, ptr %alloca_40
  %load_65 = load i64, ptr %alloca_40
  %mul_66 = mul i64 %load_64, %load_65
  store i64 %mul_66, ptr %alloca_63
  %load_68 = load i64, ptr %alloca_58
  %load_69 = load i64, ptr %alloca_63
  %add_70 = add i64 %load_68, %load_69
  store i64 %add_70, ptr %alloca_18
  ret i64 %add_70
}

define { i64, i64 } @new__1(i64 %arg0, i64 %arg1) {
bb0:
  %alloca_72 = alloca { i64, i64 }, align 8
  %insertvalue_73 = insertvalue { i64, i64 } undef, i64 %arg0, 0
  %insertvalue_74 = insertvalue { i64, i64 } %insertvalue_73, i64 %arg1, 1
  store { i64, i64 } %insertvalue_74, ptr %alloca_72
  ret { i64, i64 } %insertvalue_74
}

define i64 @area(ptr %arg0) {
bb0:
  %alloca_76 = alloca i64, align 8
  %bitcast_77 = bitcast ptr %arg0 to ptr
  %load_78 = load i64, ptr %bitcast_77
  %bitcast_79 = bitcast ptr %arg0 to ptr
  %gep_80 = getelementptr inbounds i8, ptr %bitcast_79, i64 8
  %bitcast_81 = bitcast ptr %gep_80 to ptr
  %load_82 = load i64, ptr %bitcast_81
  %mul_83 = mul i64 %load_78, %load_82
  store i64 %mul_83, ptr %alloca_76
  ret i64 %mul_83
}

define i64 @perimeter(ptr %arg0) {
bb0:
  %alloca_85 = alloca i64, align 8
  %alloca_86 = alloca i64, align 8
  %bitcast_87 = bitcast ptr %arg0 to ptr
  %load_88 = load i64, ptr %bitcast_87
  %bitcast_89 = bitcast ptr %arg0 to ptr
  %gep_90 = getelementptr inbounds i8, ptr %bitcast_89, i64 8
  %bitcast_91 = bitcast ptr %gep_90 to ptr
  %load_92 = load i64, ptr %bitcast_91
  %add_93 = add i64 %load_88, %load_92
  store i64 %add_93, ptr %alloca_86
  %load_95 = load i64, ptr %alloca_86
  %mul_96 = mul i64 2, %load_95
  store i64 %mul_96, ptr %alloca_85
  ret i64 %mul_96
}

define i1 @is_square(ptr %arg0) {
bb0:
  %alloca_98 = alloca i1, align 1
  %bitcast_99 = bitcast ptr %arg0 to ptr
  %load_100 = load i64, ptr %bitcast_99
  %bitcast_101 = bitcast ptr %arg0 to ptr
  %gep_102 = getelementptr inbounds i8, ptr %bitcast_101, i64 8
  %bitcast_103 = bitcast ptr %gep_102 to ptr
  %load_104 = load i64, ptr %bitcast_103
  %icmp_105 = icmp eq i64 %load_100, %load_104
  store i1 %icmp_105, ptr %alloca_98
  ret i1 %icmp_105
}

define i32 @main() {
bb0:
  %call_107 = call i32 (ptr, ...) @printf(ptr @.str.0)
  br label %bb1
bb1:
  %call_108 = call { i64, i64 } (i64, i64) @new__1(i64 10, i64 20)
  br label %bb2
bb2:
  %call_109 = call { i64, i64 } (i64, i64) @new__1(i64 5, i64 15)
  br label %bb3
bb3:
  %alloca_110 = alloca { i64, i64 }, align 8
  store { i64, i64 } %call_108, ptr %alloca_110
  %bitcast_112 = bitcast ptr %alloca_110 to ptr
  %load_113 = load i64, ptr %bitcast_112
  %alloca_114 = alloca { i64, i64 }, align 8
  store { i64, i64 } %call_108, ptr %alloca_114
  %bitcast_116 = bitcast ptr %alloca_114 to ptr
  %gep_117 = getelementptr inbounds i8, ptr %bitcast_116, i64 8
  %bitcast_118 = bitcast ptr %gep_117 to ptr
  %load_119 = load i64, ptr %bitcast_118
  %call_120 = call i32 (ptr, ...) @printf(ptr @.str.1, i64 %load_113, i64 %load_119)
  br label %bb4
bb4:
  %alloca_121 = alloca { i64, i64 }, align 8
  store { i64, i64 } %call_109, ptr %alloca_121
  %bitcast_123 = bitcast ptr %alloca_121 to ptr
  %load_124 = load i64, ptr %bitcast_123
  %alloca_125 = alloca { i64, i64 }, align 8
  store { i64, i64 } %call_109, ptr %alloca_125
  %bitcast_127 = bitcast ptr %alloca_125 to ptr
  %gep_128 = getelementptr inbounds i8, ptr %bitcast_127, i64 8
  %bitcast_129 = bitcast ptr %gep_128 to ptr
  %load_130 = load i64, ptr %bitcast_129
  %call_131 = call i32 (ptr, ...) @printf(ptr @.str.2, i64 %load_124, i64 %load_130)
  br label %bb5
bb5:
  %alloca_132 = alloca ptr, align 8
  store { i64, i64 } %call_108, ptr %alloca_132
  %alloca_134 = alloca i64, align 8
  %sub_135 = sub i64 0, 4
  store i64 %sub_135, ptr %alloca_134
  %load_137 = load i64, ptr %alloca_134
  call void (ptr, i64, i64) @translate(ptr %alloca_132, i64 3, i64 %load_137)
  br label %bb6
bb6:
  %alloca_139 = alloca { i64, i64 }, align 8
  store { i64, i64 } %call_108, ptr %alloca_139
  %bitcast_141 = bitcast ptr %alloca_139 to ptr
  %load_142 = load i64, ptr %bitcast_141
  %alloca_143 = alloca { i64, i64 }, align 8
  store { i64, i64 } %call_108, ptr %alloca_143
  %bitcast_145 = bitcast ptr %alloca_143 to ptr
  %gep_146 = getelementptr inbounds i8, ptr %bitcast_145, i64 8
  %bitcast_147 = bitcast ptr %gep_146 to ptr
  %load_148 = load i64, ptr %bitcast_147
  %call_149 = call i32 (ptr, ...) @printf(ptr @.str.3, i64 %load_142, i64 %load_148)
  br label %bb7
bb7:
  %alloca_150 = alloca ptr, align 8
  store { i64, i64 } %call_108, ptr %alloca_150
  %alloca_152 = alloca ptr, align 8
  store { i64, i64 } %call_109, ptr %alloca_152
  %call_154 = call i64 (ptr, ptr) @distance2(ptr %alloca_150, ptr %alloca_152)
  br label %bb8
bb8:
  %call_155 = call i32 (ptr, ...) @printf(ptr @.str.4, i64 %call_154)
  br label %bb9
bb9:
  %call_156 = call { i64, i64 } (i64, i64) @new__1(i64 10, i64 5)
  br label %bb10
bb10:
  %alloca_157 = alloca { i64, i64 }, align 8
  store { i64, i64 } %call_156, ptr %alloca_157
  %bitcast_159 = bitcast ptr %alloca_157 to ptr
  %load_160 = load i64, ptr %bitcast_159
  %alloca_161 = alloca { i64, i64 }, align 8
  store { i64, i64 } %call_156, ptr %alloca_161
  %bitcast_163 = bitcast ptr %alloca_161 to ptr
  %gep_164 = getelementptr inbounds i8, ptr %bitcast_163, i64 8
  %bitcast_165 = bitcast ptr %gep_164 to ptr
  %load_166 = load i64, ptr %bitcast_165
  %call_167 = call i32 (ptr, ...) @printf(ptr @.str.5, i64 %load_160, i64 %load_166)
  br label %bb11
bb11:
  %alloca_168 = alloca ptr, align 8
  store { i64, i64 } %call_156, ptr %alloca_168
  %call_170 = call i64 (ptr) @area(ptr %alloca_168)
  br label %bb12
bb12:
  %call_171 = call i32 (ptr, ...) @printf(ptr @.str.6, i64 %call_170)
  br label %bb13
bb13:
  %alloca_172 = alloca ptr, align 8
  store { i64, i64 } %call_156, ptr %alloca_172
  %call_174 = call i64 (ptr) @perimeter(ptr %alloca_172)
  br label %bb14
bb14:
  %call_175 = call i32 (ptr, ...) @printf(ptr @.str.7, i64 %call_174)
  br label %bb15
bb15:
  %alloca_176 = alloca ptr, align 8
  store { i64, i64 } %call_156, ptr %alloca_176
  %call_178 = call i1 (ptr) @is_square(ptr %alloca_176)
  br label %bb16
bb16:
  %zext_179 = zext i1 %call_178 to i32
  %call_180 = call i32 (ptr, ...) @printf(ptr @.str.8, i32 %zext_179)
  br label %bb17
bb17:
  ret i32 0
}

