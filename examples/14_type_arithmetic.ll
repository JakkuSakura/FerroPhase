; ModuleID = '14_type_arithmetic'
source_filename = "14_type_arithmetic"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.14_type_arithmetic.0 = private unnamed_addr constant [33 x i8] c"Foo a=%lld common=%lld foo=%lld\0A\00", align 1
@.str.14_type_arithmetic.1 = private unnamed_addr constant [26 x i8] c"Bar common=%lld bar=%lld\0A\00", align 1
@.str.14_type_arithmetic.2 = private unnamed_addr constant [49 x i8] c"FooPlusBar a=%lld common=%lld foo=%lld bar=%lld\0A\00", align 1
@.str.14_type_arithmetic.3 = private unnamed_addr constant [29 x i8] c"FooMinusBar a=%lld foo=%lld\0A\00", align 1
@.str.14_type_arithmetic.4 = private unnamed_addr constant [23 x i8] c"FooAndBar common=%lld\0A\00", align 1
@.str.14_type_arithmetic.5 = private unnamed_addr constant [34 x i8] c"InlineRecord tag=%lld value=%lld\0A\00", align 1
@.str.14_type_arithmetic.6 = private unnamed_addr constant [24 x i8] c"FooOrBar left sum=%lld\0A\00", align 1
@.str.14_type_arithmetic.7 = private unnamed_addr constant [25 x i8] c"FooOrBar right sum=%lld\0A\00", align 1
@.str.14_type_arithmetic.8 = private unnamed_addr constant [53 x i8] c"Int4[0]=%lld Int4[1]=%lld Int4[2]=%lld Int4[3]=%lld\0A\00", align 1
@.str.14_type_arithmetic.9 = private unnamed_addr constant [6 x i8] c"hello\00", align 1
@.str.14_type_arithmetic.10 = private unnamed_addr constant [6 x i8] c"green\00", align 1
@.str.14_type_arithmetic.11 = private unnamed_addr constant [17 x i8] c"LiteralInt %lld\0A\00", align 1
@.str.14_type_arithmetic.12 = private unnamed_addr constant [16 x i8] c"LiteralBool %d\0A\00", align 1
@.str.14_type_arithmetic.13 = private unnamed_addr constant [15 x i8] c"LiteralStr %s\0A\00", align 1
@.str.14_type_arithmetic.14 = private unnamed_addr constant [15 x i8] c"LiteralUnit %s\00", align 1
@.str.14_type_arithmetic.15 = private unnamed_addr constant [3 x i8] c"()\00", align 1
@.str.14_type_arithmetic.16 = private unnamed_addr constant [15 x i8] c"LiteralNull %s\00", align 1
@.str.14_type_arithmetic.17 = private unnamed_addr constant [5 x i8] c"null\00", align 1
@.str.14_type_arithmetic.18 = private unnamed_addr constant [19 x i8] c"LiteralStrEnum %s\0A\00", align 1
@.str.14_type_arithmetic.19 = private unnamed_addr constant [19 x i8] c"FooMaybe sum=%lld\0A\00", align 1

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
  %alloca_271 = alloca ptr, align 8
  %alloca_count_271 = alloca ptr, align 8
  %alloca_266 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_266 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_253 = alloca ptr, align 8
  %alloca_count_253 = alloca ptr, align 8
  %alloca_251 = alloca ptr, align 8
  %alloca_count_251 = alloca ptr, align 8
  %alloca_249 = alloca ptr, align 8
  %alloca_count_249 = alloca ptr, align 8
  %alloca_247 = alloca i1, align 1
  %alloca_count_247 = alloca i1, align 1
  %alloca_245 = alloca i64, align 8
  %alloca_count_245 = alloca i64, align 8
  %alloca_218 = alloca i64, align 8
  %alloca_count_218 = alloca i64, align 8
  %alloca_216 = alloca i64, align 8
  %alloca_count_216 = alloca i64, align 8
  %alloca_214 = alloca i64, align 8
  %alloca_count_214 = alloca i64, align 8
  %alloca_212 = alloca i64, align 8
  %alloca_count_212 = alloca i64, align 8
  %alloca_209 = alloca [4 x i64], align 8
  %alloca_count_209 = alloca [4 x i64], align 8
  %alloca_207 = alloca [4 x i64], align 8
  %alloca_count_207 = alloca [4 x i64], align 8
  %alloca_200 = alloca ptr, align 8
  %alloca_count_200 = alloca ptr, align 8
  %alloca_197 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_197 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_185 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_185 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_183 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_183 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_174 = alloca { i64, i64 }, align 8
  %alloca_count_174 = alloca { i64, i64 }, align 8
  %alloca_169 = alloca { i64 }, align 8
  %alloca_count_169 = alloca { i64 }, align 8
  %alloca_160 = alloca { i64, i64 }, align 8
  %alloca_count_160 = alloca { i64, i64 }, align 8
  %alloca_142 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_142 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_126 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_126 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_123 = alloca { i64, i64, i64 }, align 8
  %alloca_count_123 = alloca { i64, i64, i64 }, align 8
  %alloca_114 = alloca { i64, i64 }, align 8
  %alloca_count_114 = alloca { i64, i64 }, align 8
  %alloca_101 = alloca { i64, i64, i64 }, align 8
  %alloca_count_101 = alloca { i64, i64, i64 }, align 8
  store { i64, i64, i64 } { i64 1, i64 2, i64 3 }, ptr %alloca_count_101, align 8
  %load_104 = load i64, ptr %alloca_count_101, align 8
  %gep_106 = getelementptr inbounds i8, ptr %alloca_count_101, i64 8
  %load_108 = load i64, ptr %gep_106, align 8
  %gep_110 = getelementptr inbounds i8, ptr %alloca_count_101, i64 16
  %load_112 = load i64, ptr %gep_110, align 8
  %call_113 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.0, i64 %load_104, i64 %load_108, i64 %load_112)
  br label %bb1

bb1:                                              ; preds = %bb0
  store { i64, i64 } { i64 4, i64 5 }, ptr %alloca_count_114, align 8
  %load_117 = load i64, ptr %alloca_count_114, align 8
  %gep_119 = getelementptr inbounds i8, ptr %alloca_count_114, i64 8
  %load_121 = load i64, ptr %gep_119, align 8
  %call_122 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.1, i64 %load_117, i64 %load_121)
  br label %bb2

bb2:                                              ; preds = %bb1
  %load_124 = load { i64, i64, i64 }, ptr %alloca_count_101, align 8
  store { i64, i64, i64 } %load_124, ptr %alloca_count_123, align 8
  %load_128 = load i64, ptr %alloca_count_123, align 8
  %gep_130 = getelementptr inbounds i8, ptr %alloca_count_123, i64 8
  %load_132 = load i64, ptr %gep_130, align 8
  %gep_134 = getelementptr inbounds i8, ptr %alloca_count_123, i64 16
  %load_136 = load i64, ptr %gep_134, align 8
  %insertvalue_137 = insertvalue { i64, i64, i64, i64 } undef, i64 %load_128, 0
  %insertvalue_138 = insertvalue { i64, i64, i64, i64 } %insertvalue_137, i64 %load_132, 1
  %insertvalue_139 = insertvalue { i64, i64, i64, i64 } %insertvalue_138, i64 %load_136, 2
  %insertvalue_140 = insertvalue { i64, i64, i64, i64 } %insertvalue_139, i64 6, 3
  store { i64, i64, i64, i64 } %insertvalue_140, ptr %alloca_count_126, align 8
  %load_143 = load { i64, i64, i64, i64 }, ptr %alloca_count_126, align 8
  store { i64, i64, i64, i64 } %load_143, ptr %alloca_count_142, align 8
  %load_146 = load i64, ptr %alloca_count_142, align 8
  %gep_148 = getelementptr inbounds i8, ptr %alloca_count_142, i64 8
  %load_150 = load i64, ptr %gep_148, align 8
  %gep_152 = getelementptr inbounds i8, ptr %alloca_count_142, i64 16
  %load_154 = load i64, ptr %gep_152, align 8
  %gep_156 = getelementptr inbounds i8, ptr %alloca_count_142, i64 24
  %load_158 = load i64, ptr %gep_156, align 8
  %call_159 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.2, i64 %load_146, i64 %load_150, i64 %load_154, i64 %load_158)
  br label %bb3

bb3:                                              ; preds = %bb2
  store { i64, i64 } { i64 10, i64 20 }, ptr %alloca_count_160, align 8
  %load_163 = load i64, ptr %alloca_count_160, align 8
  %gep_165 = getelementptr inbounds i8, ptr %alloca_count_160, i64 8
  %load_167 = load i64, ptr %gep_165, align 8
  %call_168 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.3, i64 %load_163, i64 %load_167)
  br label %bb4

bb4:                                              ; preds = %bb3
  store { i64 } { i64 99 }, ptr %alloca_count_169, align 8
  %load_172 = load i64, ptr %alloca_count_169, align 8
  %call_173 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.4, i64 %load_172)
  br label %bb5

bb5:                                              ; preds = %bb4
  store { i64, i64 } { i64 7, i64 42 }, ptr %alloca_count_174, align 8
  %load_177 = load i64, ptr %alloca_count_174, align 8
  %gep_179 = getelementptr inbounds i8, ptr %alloca_count_174, i64 8
  %load_181 = load i64, ptr %gep_179, align 8
  %call_182 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.5, i64 %load_177, i64 %load_181)
  br label %bb6

bb6:                                              ; preds = %bb5
  store { i64, i64, i64, i64 } { i64 0, i64 2, i64 3, i64 4 }, ptr %alloca_count_183, align 8
  %load_187 = load i64, ptr %alloca_count_114, align 8
  %gep_189 = getelementptr inbounds i8, ptr %alloca_count_114, i64 8
  %load_191 = load i64, ptr %gep_189, align 8
  %insertvalue_193 = insertvalue { i64, i64, i64, i64 } { i64 1, i64 undef, i64 undef, i64 undef }, i64 %load_187, 1
  %insertvalue_194 = insertvalue { i64, i64, i64, i64 } %insertvalue_193, i64 %load_191, 2
  %insertvalue_195 = insertvalue { i64, i64, i64, i64 } %insertvalue_194, i64 0, 3
  store { i64, i64, i64, i64 } %insertvalue_195, ptr %alloca_count_185, align 8
  %load_198 = load { i64, i64, i64, i64 }, ptr %alloca_count_185, align 8
  store { i64, i64, i64, i64 } %load_198, ptr %alloca_count_197, align 8
  %load_201 = load { i64, i64, i64, i64 }, ptr %alloca_count_183, align 8
  %call_202 = call i64 @describe_union({ i64, i64, i64, i64 } %load_201)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_203 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.6, i64 %call_202)
  br label %bb8

bb8:                                              ; preds = %bb7
  %load_204 = load { i64, i64, i64, i64 }, ptr %alloca_count_197, align 8
  %call_205 = call i64 @describe_union({ i64, i64, i64, i64 } %load_204)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_206 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.7, i64 %call_205)
  br label %bb10

bb10:                                             ; preds = %bb9
  store [4 x i64] [i64 1, i64 2, i64 3, i64 4], ptr %alloca_count_207, align 8
  %load_210 = load [4 x i64], ptr %alloca_count_207, align 8
  store [4 x i64] %load_210, ptr %alloca_count_209, align 8
  store i64 0, ptr %alloca_count_212, align 8
  store i64 1, ptr %alloca_count_214, align 8
  store i64 2, ptr %alloca_count_216, align 8
  store i64 3, ptr %alloca_count_218, align 8
  %load_220 = load i64, ptr %alloca_count_212, align 8
  %iop_221 = mul i64 %load_220, 8
  %gep_223 = getelementptr inbounds i8, ptr %alloca_count_209, i64 %iop_221
  %load_225 = load i64, ptr %gep_223, align 8
  %load_226 = load i64, ptr %alloca_count_214, align 8
  %iop_227 = mul i64 %load_226, 8
  %gep_229 = getelementptr inbounds i8, ptr %alloca_count_209, i64 %iop_227
  %load_231 = load i64, ptr %gep_229, align 8
  %load_232 = load i64, ptr %alloca_count_216, align 8
  %iop_233 = mul i64 %load_232, 8
  %gep_235 = getelementptr inbounds i8, ptr %alloca_count_209, i64 %iop_233
  %load_237 = load i64, ptr %gep_235, align 8
  %load_238 = load i64, ptr %alloca_count_218, align 8
  %iop_239 = mul i64 %load_238, 8
  %gep_241 = getelementptr inbounds i8, ptr %alloca_count_209, i64 %iop_239
  %load_243 = load i64, ptr %gep_241, align 8
  %call_244 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.8, i64 %load_225, i64 %load_231, i64 %load_237, i64 %load_243)
  br label %bb11

bb11:                                             ; preds = %bb10
  store i64 42, ptr %alloca_count_245, align 8
  store i1 true, ptr %alloca_count_247, align 1
  store ptr @.str.14_type_arithmetic.9, ptr %alloca_count_249, align 8
  store ptr null, ptr %alloca_count_251, align 8
  store ptr @.str.14_type_arithmetic.10, ptr %alloca_count_253, align 8
  %load_255 = load i64, ptr %alloca_count_245, align 8
  %call_256 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.11, i64 %load_255)
  br label %bb12

bb12:                                             ; preds = %bb11
  %load_257 = load i1, ptr %alloca_count_247, align 1
  %zext = zext i1 %load_257 to i32
  %call_259 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.12, i32 %zext)
  br label %bb13

bb13:                                             ; preds = %bb12
  %load_260 = load ptr, ptr %alloca_count_249, align 8
  %call_261 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.13, ptr %load_260)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_262 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.14, ptr @.str.14_type_arithmetic.15)
  br label %bb15

bb15:                                             ; preds = %bb14
  %call_263 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.16, ptr @.str.14_type_arithmetic.17)
  br label %bb16

bb16:                                             ; preds = %bb15
  %load_264 = load ptr, ptr %alloca_count_253, align 8
  %call_265 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.18, ptr %load_264)
  br label %bb17

bb17:                                             ; preds = %bb16
  store { i64, i64, i64, i64 } { i64 0, i64 5, i64 6, i64 7 }, ptr %alloca_count_266, align 8
  %load_268 = load { i64, i64, i64, i64 }, ptr %alloca_count_266, align 8
  %call_269 = call i64 @describe_optional({ i64, i64, i64, i64 } %load_268)
  br label %bb18

bb18:                                             ; preds = %bb17
  %call_270 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.19, i64 %call_269)
  br label %bb19

bb19:                                             ; preds = %bb18
  ret i32 0
}

declare i32 @printf(ptr, ...)
