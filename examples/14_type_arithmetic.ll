; ModuleID = '14_type_arithmetic'
source_filename = "14_type_arithmetic"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.14_type_arithmetic.0 = constant [38 x i8] c"\F0\9F\93\98 Tutorial: 14_type_arithmetic.fp\0A\00"
@.str.14_type_arithmetic.1 = constant [84 x i8] c"\F0\9F\A7\AD Focus: Type arithmetic examples: combining and relating types with operators.\0A\00"
@.str.14_type_arithmetic.2 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.14_type_arithmetic.3 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.14_type_arithmetic.4 = constant [2 x i8] c"\0A\00"
@.str.14_type_arithmetic.5 = constant [33 x i8] c"Foo a=%lld common=%lld foo=%lld\0A\00"
@.str.14_type_arithmetic.6 = constant [26 x i8] c"Bar common=%lld bar=%lld\0A\00"
@.str.14_type_arithmetic.7 = constant [49 x i8] c"FooPlusBar a=%lld common=%lld foo=%lld bar=%lld\0A\00"
@.str.14_type_arithmetic.8 = constant [29 x i8] c"FooMinusBar a=%lld foo=%lld\0A\00"
@.str.14_type_arithmetic.9 = constant [23 x i8] c"FooAndBar common=%lld\0A\00"
@.str.14_type_arithmetic.10 = constant [34 x i8] c"InlineRecord tag=%lld value=%lld\0A\00"
@.str.14_type_arithmetic.11 = constant [24 x i8] c"FooOrBar left sum=%lld\0A\00"
@.str.14_type_arithmetic.12 = constant [25 x i8] c"FooOrBar right sum=%lld\0A\00"
@.str.14_type_arithmetic.13 = constant [53 x i8] c"Int4[0]=%lld Int4[1]=%lld Int4[2]=%lld Int4[3]=%lld\0A\00"
@.str.14_type_arithmetic.14 = constant [6 x i8] c"hello\00"
@.str.14_type_arithmetic.15 = constant [6 x i8] c"green\00"
@.str.14_type_arithmetic.16 = constant [17 x i8] c"LiteralInt %lld\0A\00"
@.str.14_type_arithmetic.17 = constant [16 x i8] c"LiteralBool %d\0A\00"
@.str.14_type_arithmetic.18 = constant [15 x i8] c"LiteralStr %s\0A\00"
@.str.14_type_arithmetic.19 = constant [16 x i8] c"LiteralUnit %s\0A\00"
@.str.14_type_arithmetic.20 = constant [3 x i8] c"()\00"
@.str.14_type_arithmetic.21 = constant [16 x i8] c"LiteralNull %s\0A\00"
@.str.14_type_arithmetic.22 = constant [19 x i8] c"LiteralStrEnum %s\0A\00"
@.str.14_type_arithmetic.23 = constant [19 x i8] c"FooMaybe sum=%lld\0A\00"

define internal i64 @describe_union({ i64, i64, i64, i64 } %0) {
bb0:
  %alloca_49 = alloca i64, align 8
  %alloca_count_49 = alloca i64, align 8
  %alloca_43 = alloca i64, align 8
  %alloca_count_43 = alloca i64, align 8
  %alloca_36 = alloca i1, align 1
  %alloca_count_36 = alloca i1, align 1
  %alloca_27 = alloca i64, align 8
  %alloca_count_27 = alloca i64, align 8
  %alloca_21 = alloca i64, align 8
  %alloca_count_21 = alloca i64, align 8
  %alloca_15 = alloca i64, align 8
  %alloca_count_15 = alloca i64, align 8
  %alloca_9 = alloca i64, align 8
  %alloca_count_9 = alloca i64, align 8
  %alloca_3 = alloca i1, align 1
  %alloca_count_3 = alloca i1, align 1
  %alloca_1 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_1 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_0 = alloca i64, align 8
  %alloca_count_0 = alloca i64, align 8
  store { i64, i64, i64, i64 } %0, ptr %alloca_count_1, align 8
  %load_5 = load i64, ptr %alloca_count_1, align 8
  %icmp_6 = icmp eq i64 %load_5, 0
  store i1 %icmp_6, ptr %alloca_count_3, align 1
  %load_8 = load i1, ptr %alloca_count_3, align 1
  br i1 %load_8, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  %gep_11 = getelementptr inbounds i8, ptr %alloca_count_1, i64 8
  %load_13 = load i64, ptr %gep_11, align 8
  store i64 %load_13, ptr %alloca_count_9, align 8
  %gep_17 = getelementptr inbounds i8, ptr %alloca_count_1, i64 16
  %load_19 = load i64, ptr %gep_17, align 8
  store i64 %load_19, ptr %alloca_count_15, align 8
  %gep_23 = getelementptr inbounds i8, ptr %alloca_count_1, i64 24
  %load_25 = load i64, ptr %gep_23, align 8
  store i64 %load_25, ptr %alloca_count_21, align 8
  %load_28 = load i64, ptr %alloca_count_9, align 8
  %load_29 = load i64, ptr %alloca_count_15, align 8
  %iop_30 = add i64 %load_28, %load_29
  store i64 %iop_30, ptr %alloca_count_27, align 8
  %load_32 = load i64, ptr %alloca_count_27, align 8
  %load_33 = load i64, ptr %alloca_count_21, align 8
  %iop_34 = add i64 %load_32, %load_33
  store i64 %iop_34, ptr %alloca_count_0, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  %load_38 = load i64, ptr %alloca_count_1, align 8
  %icmp_39 = icmp eq i64 %load_38, 1
  store i1 %icmp_39, ptr %alloca_count_36, align 1
  %load_41 = load i1, ptr %alloca_count_36, align 1
  br i1 %load_41, label %bb4, label %bb5

bb1:                                              ; preds = %bb5, %bb4, %bb2
  %load_42 = load i64, ptr %alloca_count_0, align 8
  ret i64 %load_42

bb4:                                              ; preds = %bb3
  %gep_45 = getelementptr inbounds i8, ptr %alloca_count_1, i64 8
  %load_47 = load i64, ptr %gep_45, align 8
  store i64 %load_47, ptr %alloca_count_43, align 8
  %gep_51 = getelementptr inbounds i8, ptr %alloca_count_1, i64 16
  %load_53 = load i64, ptr %gep_51, align 8
  store i64 %load_53, ptr %alloca_count_49, align 8
  %load_55 = load i64, ptr %alloca_count_43, align 8
  %load_56 = load i64, ptr %alloca_count_49, align 8
  %iop_57 = add i64 %load_55, %load_56
  store i64 %iop_57, ptr %alloca_count_0, align 8
  br label %bb1

bb5:                                              ; preds = %bb3
  br label %bb1
}

define internal i64 @describe_optional({ i64, i64, i64, i64 } %0) {
bb0:
  %alloca_96 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_96 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_86 = alloca i64, align 8
  %alloca_count_86 = alloca i64, align 8
  %alloca_80 = alloca i64, align 8
  %alloca_count_80 = alloca i64, align 8
  %alloca_74 = alloca i64, align 8
  %alloca_count_74 = alloca i64, align 8
  %alloca_68 = alloca i64, align 8
  %alloca_count_68 = alloca i64, align 8
  %alloca_62 = alloca i1, align 1
  %alloca_count_62 = alloca i1, align 1
  %alloca_60 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_60 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_59 = alloca i64, align 8
  %alloca_count_59 = alloca i64, align 8
  store { i64, i64, i64, i64 } %0, ptr %alloca_count_60, align 8
  %load_64 = load i64, ptr %alloca_count_60, align 8
  %icmp_65 = icmp eq i64 %load_64, 0
  store i1 %icmp_65, ptr %alloca_count_62, align 1
  %load_67 = load i1, ptr %alloca_count_62, align 1
  br i1 %load_67, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  %gep_70 = getelementptr inbounds i8, ptr %alloca_count_60, i64 8
  %load_72 = load i64, ptr %gep_70, align 8
  store i64 %load_72, ptr %alloca_count_68, align 8
  %gep_76 = getelementptr inbounds i8, ptr %alloca_count_60, i64 16
  %load_78 = load i64, ptr %gep_76, align 8
  store i64 %load_78, ptr %alloca_count_74, align 8
  %gep_82 = getelementptr inbounds i8, ptr %alloca_count_60, i64 24
  %load_84 = load i64, ptr %gep_82, align 8
  store i64 %load_84, ptr %alloca_count_80, align 8
  %load_87 = load i64, ptr %alloca_count_68, align 8
  %load_88 = load i64, ptr %alloca_count_74, align 8
  %iop_89 = add i64 %load_87, %load_88
  store i64 %iop_89, ptr %alloca_count_86, align 8
  %load_91 = load i64, ptr %alloca_count_86, align 8
  %load_92 = load i64, ptr %alloca_count_80, align 8
  %iop_93 = add i64 %load_91, %load_92
  store i64 %iop_93, ptr %alloca_count_59, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  br label %bb4

bb1:                                              ; preds = %bb4, %bb2
  %load_95 = load i64, ptr %alloca_count_59, align 8
  ret i64 %load_95

bb4:                                              ; preds = %bb3
  %load_97 = load { i64, i64, i64, i64 }, ptr %alloca_count_60, align 8
  store { i64, i64, i64, i64 } %load_97, ptr %alloca_count_96, align 8
  store i64 0, ptr %alloca_count_59, align 8
  br label %bb1

bb5:                                              ; No predecessors!
  %load_100 = load i64, ptr %alloca_count_59, align 8
  ret i64 %load_100
}

define i32 @main() {
bb0:
  %alloca_273 = alloca ptr, align 8
  %alloca_count_273 = alloca ptr, align 8
  %alloca_268 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_268 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_255 = alloca ptr, align 8
  %alloca_count_255 = alloca ptr, align 8
  %alloca_253 = alloca ptr, align 8
  %alloca_count_253 = alloca ptr, align 8
  %alloca_251 = alloca ptr, align 8
  %alloca_count_251 = alloca ptr, align 8
  %alloca_249 = alloca i1, align 1
  %alloca_count_249 = alloca i1, align 1
  %alloca_247 = alloca i64, align 8
  %alloca_count_247 = alloca i64, align 8
  %alloca_220 = alloca i64, align 8
  %alloca_count_220 = alloca i64, align 8
  %alloca_218 = alloca i64, align 8
  %alloca_count_218 = alloca i64, align 8
  %alloca_216 = alloca i64, align 8
  %alloca_count_216 = alloca i64, align 8
  %alloca_214 = alloca i64, align 8
  %alloca_count_214 = alloca i64, align 8
  %alloca_211 = alloca [4 x i64], align 8
  %alloca_count_211 = alloca [4 x i64], align 8
  %alloca_209 = alloca [4 x i64], align 8
  %alloca_count_209 = alloca [4 x i64], align 8
  %alloca_202 = alloca ptr, align 8
  %alloca_count_202 = alloca ptr, align 8
  %alloca_190 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_190 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_188 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_188 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_179 = alloca { i64, i64 }, align 8
  %alloca_count_179 = alloca { i64, i64 }, align 8
  %alloca_174 = alloca { i64 }, align 8
  %alloca_count_174 = alloca { i64 }, align 8
  %alloca_165 = alloca { i64, i64 }, align 8
  %alloca_count_165 = alloca { i64, i64 }, align 8
  %alloca_147 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_147 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_131 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_131 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_128 = alloca { i64, i64, i64 }, align 8
  %alloca_count_128 = alloca { i64, i64, i64 }, align 8
  %alloca_119 = alloca { i64, i64 }, align 8
  %alloca_count_119 = alloca { i64, i64 }, align 8
  %alloca_106 = alloca { i64, i64, i64 }, align 8
  %alloca_count_106 = alloca { i64, i64, i64 }, align 8
  %call_101 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_102 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_103 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.2)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_104 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.3)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_105 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  store { i64, i64, i64 } { i64 1, i64 2, i64 3 }, ptr %alloca_count_106, align 8
  %load_109 = load i64, ptr %alloca_count_106, align 8
  %gep_111 = getelementptr inbounds i8, ptr %alloca_count_106, i64 8
  %load_113 = load i64, ptr %gep_111, align 8
  %gep_115 = getelementptr inbounds i8, ptr %alloca_count_106, i64 16
  %load_117 = load i64, ptr %gep_115, align 8
  %call_118 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.5, i64 %load_109, i64 %load_113, i64 %load_117)
  br label %bb6

bb6:                                              ; preds = %bb5
  store { i64, i64 } { i64 4, i64 5 }, ptr %alloca_count_119, align 8
  %load_122 = load i64, ptr %alloca_count_119, align 8
  %gep_124 = getelementptr inbounds i8, ptr %alloca_count_119, i64 8
  %load_126 = load i64, ptr %gep_124, align 8
  %call_127 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.6, i64 %load_122, i64 %load_126)
  br label %bb7

bb7:                                              ; preds = %bb6
  %load_129 = load { i64, i64, i64 }, ptr %alloca_count_106, align 8
  store { i64, i64, i64 } %load_129, ptr %alloca_count_128, align 8
  %load_133 = load i64, ptr %alloca_count_128, align 8
  %gep_135 = getelementptr inbounds i8, ptr %alloca_count_128, i64 8
  %load_137 = load i64, ptr %gep_135, align 8
  %gep_139 = getelementptr inbounds i8, ptr %alloca_count_128, i64 16
  %load_141 = load i64, ptr %gep_139, align 8
  %insertvalue_142 = insertvalue { i64, i64, i64, i64 } undef, i64 %load_133, 0
  %insertvalue_143 = insertvalue { i64, i64, i64, i64 } %insertvalue_142, i64 %load_137, 1
  %insertvalue_144 = insertvalue { i64, i64, i64, i64 } %insertvalue_143, i64 %load_141, 2
  %insertvalue_145 = insertvalue { i64, i64, i64, i64 } %insertvalue_144, i64 6, 3
  store { i64, i64, i64, i64 } %insertvalue_145, ptr %alloca_count_131, align 8
  %load_148 = load { i64, i64, i64, i64 }, ptr %alloca_count_131, align 8
  store { i64, i64, i64, i64 } %load_148, ptr %alloca_count_147, align 8
  %load_151 = load i64, ptr %alloca_count_147, align 8
  %gep_153 = getelementptr inbounds i8, ptr %alloca_count_147, i64 8
  %load_155 = load i64, ptr %gep_153, align 8
  %gep_157 = getelementptr inbounds i8, ptr %alloca_count_147, i64 16
  %load_159 = load i64, ptr %gep_157, align 8
  %gep_161 = getelementptr inbounds i8, ptr %alloca_count_147, i64 24
  %load_163 = load i64, ptr %gep_161, align 8
  %call_164 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.7, i64 %load_151, i64 %load_155, i64 %load_159, i64 %load_163)
  br label %bb8

bb8:                                              ; preds = %bb7
  store { i64, i64 } { i64 10, i64 20 }, ptr %alloca_count_165, align 8
  %load_168 = load i64, ptr %alloca_count_165, align 8
  %gep_170 = getelementptr inbounds i8, ptr %alloca_count_165, i64 8
  %load_172 = load i64, ptr %gep_170, align 8
  %call_173 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.8, i64 %load_168, i64 %load_172)
  br label %bb9

bb9:                                              ; preds = %bb8
  store { i64 } { i64 99 }, ptr %alloca_count_174, align 8
  %load_177 = load i64, ptr %alloca_count_174, align 8
  %call_178 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.9, i64 %load_177)
  br label %bb10

bb10:                                             ; preds = %bb9
  store { i64, i64 } { i64 7, i64 42 }, ptr %alloca_count_179, align 8
  %load_182 = load i64, ptr %alloca_count_179, align 8
  %gep_184 = getelementptr inbounds i8, ptr %alloca_count_179, i64 8
  %load_186 = load i64, ptr %gep_184, align 8
  %call_187 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.10, i64 %load_182, i64 %load_186)
  br label %bb11

bb11:                                             ; preds = %bb10
  store { i64, i64, i64, i64 } { i64 0, i64 2, i64 3, i64 4 }, ptr %alloca_count_188, align 8
  %load_192 = load i64, ptr %alloca_count_119, align 8
  %gep_194 = getelementptr inbounds i8, ptr %alloca_count_119, i64 8
  %load_196 = load i64, ptr %gep_194, align 8
  %insertvalue_198 = insertvalue { i64, i64, i64, i64 } { i64 1, i64 undef, i64 undef, i64 undef }, i64 %load_192, 1
  %insertvalue_199 = insertvalue { i64, i64, i64, i64 } %insertvalue_198, i64 %load_196, 2
  %insertvalue_200 = insertvalue { i64, i64, i64, i64 } %insertvalue_199, i64 0, 3
  store { i64, i64, i64, i64 } %insertvalue_200, ptr %alloca_count_190, align 8
  %load_203 = load { i64, i64, i64, i64 }, ptr %alloca_count_188, align 8
  %call_204 = call i64 @describe_union({ i64, i64, i64, i64 } %load_203)
  br label %bb12

bb12:                                             ; preds = %bb11
  %call_205 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.11, i64 %call_204)
  %load_206 = load { i64, i64, i64, i64 }, ptr %alloca_count_190, align 8
  %call_207 = call i64 @describe_union({ i64, i64, i64, i64 } %load_206)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_208 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.12, i64 %call_207)
  store [4 x i64] [i64 1, i64 2, i64 3, i64 4], ptr %alloca_count_209, align 8
  %load_212 = load [4 x i64], ptr %alloca_count_209, align 8
  store [4 x i64] %load_212, ptr %alloca_count_211, align 8
  store i64 0, ptr %alloca_count_214, align 8
  store i64 1, ptr %alloca_count_216, align 8
  store i64 2, ptr %alloca_count_218, align 8
  store i64 3, ptr %alloca_count_220, align 8
  %load_222 = load i64, ptr %alloca_count_214, align 8
  %iop_223 = mul i64 %load_222, 8
  %gep_225 = getelementptr inbounds i8, ptr %alloca_count_211, i64 %iop_223
  %load_227 = load i64, ptr %gep_225, align 8
  %load_228 = load i64, ptr %alloca_count_216, align 8
  %iop_229 = mul i64 %load_228, 8
  %gep_231 = getelementptr inbounds i8, ptr %alloca_count_211, i64 %iop_229
  %load_233 = load i64, ptr %gep_231, align 8
  %load_234 = load i64, ptr %alloca_count_218, align 8
  %iop_235 = mul i64 %load_234, 8
  %gep_237 = getelementptr inbounds i8, ptr %alloca_count_211, i64 %iop_235
  %load_239 = load i64, ptr %gep_237, align 8
  %load_240 = load i64, ptr %alloca_count_220, align 8
  %iop_241 = mul i64 %load_240, 8
  %gep_243 = getelementptr inbounds i8, ptr %alloca_count_211, i64 %iop_241
  %load_245 = load i64, ptr %gep_243, align 8
  %call_246 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.13, i64 %load_227, i64 %load_233, i64 %load_239, i64 %load_245)
  store i64 42, ptr %alloca_count_247, align 8
  store i1 true, ptr %alloca_count_249, align 1
  store ptr @.str.14_type_arithmetic.14, ptr %alloca_count_251, align 8
  store ptr null, ptr %alloca_count_253, align 8
  store ptr @.str.14_type_arithmetic.15, ptr %alloca_count_255, align 8
  %load_257 = load i64, ptr %alloca_count_247, align 8
  %call_258 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.16, i64 %load_257)
  %load_259 = load i1, ptr %alloca_count_249, align 1
  %call_260 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.17, i1 %load_259)
  %load_261 = load ptr, ptr %alloca_count_251, align 8
  %call_262 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.18, ptr %load_261)
  %call_263 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.19, ptr @.str.14_type_arithmetic.20)
  %load_264 = load ptr, ptr %alloca_count_253, align 8
  %call_265 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.21, ptr %load_264)
  %load_266 = load ptr, ptr %alloca_count_255, align 8
  %call_267 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.22, ptr %load_266)
  store { i64, i64, i64, i64 } { i64 0, i64 5, i64 6, i64 7 }, ptr %alloca_count_268, align 8
  %load_270 = load { i64, i64, i64, i64 }, ptr %alloca_count_268, align 8
  %call_271 = call i64 @describe_optional({ i64, i64, i64, i64 } %load_270)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_272 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.23, i64 %call_271)
  ret i32 0
}

declare i32 @printf(ptr, ...)
