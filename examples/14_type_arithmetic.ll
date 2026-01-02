; ModuleID = '14_type_arithmetic'
source_filename = "14_type_arithmetic"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.14_type_arithmetic.0 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str.14_type_arithmetic.1 = private unnamed_addr constant [10 x i8] c"<unknown>\00", align 1
@.str.14_type_arithmetic.2 = private unnamed_addr constant [33 x i8] c"Foo a=%lld common=%lld foo=%lld\0A\00", align 1
@.str.14_type_arithmetic.3 = private unnamed_addr constant [26 x i8] c"Bar common=%lld bar=%lld\0A\00", align 1
@.str.14_type_arithmetic.4 = private unnamed_addr constant [49 x i8] c"FooPlusBar a=%lld common=%lld foo=%lld bar=%lld\0A\00", align 1
@.str.14_type_arithmetic.5 = private unnamed_addr constant [29 x i8] c"FooMinusBar a=%lld foo=%lld\0A\00", align 1
@.str.14_type_arithmetic.6 = private unnamed_addr constant [23 x i8] c"FooAndBar common=%lld\0A\00", align 1
@.str.14_type_arithmetic.7 = private unnamed_addr constant [34 x i8] c"InlineRecord tag=%lld value=%lld\0A\00", align 1
@.str.14_type_arithmetic.8 = private unnamed_addr constant [22 x i8] c"FooOrBar left sum=%s\0A\00", align 1
@.str.14_type_arithmetic.9 = private unnamed_addr constant [23 x i8] c"FooOrBar right sum=%s\0A\00", align 1
@.str.14_type_arithmetic.10 = private unnamed_addr constant [45 x i8] c"Int4[0]=%s Int4[1]=%s Int4[2]=%s Int4[3]=%s\0A\00", align 1
@.str.14_type_arithmetic.11 = private unnamed_addr constant [6 x i8] c"hello\00", align 1
@.str.14_type_arithmetic.12 = private unnamed_addr constant [6 x i8] c"green\00", align 1
@.str.14_type_arithmetic.13 = private unnamed_addr constant [15 x i8] c"LiteralInt %s\0A\00", align 1
@.str.14_type_arithmetic.14 = private unnamed_addr constant [16 x i8] c"LiteralBool %s\0A\00", align 1
@.str.14_type_arithmetic.15 = private unnamed_addr constant [15 x i8] c"LiteralStr %s\0A\00", align 1
@.str.14_type_arithmetic.16 = private unnamed_addr constant [15 x i8] c"LiteralUnit %s\00", align 1
@.str.14_type_arithmetic.17 = private unnamed_addr constant [15 x i8] c"LiteralNull %s\00", align 1
@.str.14_type_arithmetic.18 = private unnamed_addr constant [19 x i8] c"LiteralStrEnum %s\0A\00", align 1
@.str.14_type_arithmetic.19 = private unnamed_addr constant [17 x i8] c"FooMaybe sum=%s\0A\00", align 1

define internal i64 @describe_union({ i64, i64, i64, i64 } %0) {
bb0:
  %alloca_107 = alloca i64, align 8
  %alloca_count_107 = alloca i64, align 8
  %alloca_101 = alloca i64, align 8
  %alloca_count_101 = alloca i64, align 8
  %alloca_94 = alloca i1, align 1
  %alloca_count_94 = alloca i1, align 1
  %alloca_85 = alloca i64, align 8
  %alloca_count_85 = alloca i64, align 8
  %alloca_79 = alloca i64, align 8
  %alloca_count_79 = alloca i64, align 8
  %alloca_73 = alloca i64, align 8
  %alloca_count_73 = alloca i64, align 8
  %alloca_67 = alloca i64, align 8
  %alloca_count_67 = alloca i64, align 8
  %alloca_57 = alloca i64, align 8
  %alloca_count_57 = alloca i64, align 8
  %alloca_51 = alloca i64, align 8
  %alloca_count_51 = alloca i64, align 8
  %alloca_45 = alloca i1, align 1
  %alloca_count_45 = alloca i1, align 1
  %alloca_43 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_43 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_37 = alloca i1, align 1
  %alloca_count_37 = alloca i1, align 1
  %alloca_28 = alloca i64, align 8
  %alloca_count_28 = alloca i64, align 8
  %alloca_22 = alloca i64, align 8
  %alloca_count_22 = alloca i64, align 8
  %alloca_16 = alloca i64, align 8
  %alloca_count_16 = alloca i64, align 8
  %alloca_10 = alloca i64, align 8
  %alloca_count_10 = alloca i64, align 8
  %alloca_4 = alloca i1, align 1
  %alloca_count_4 = alloca i1, align 1
  %alloca_2 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_2 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_1 = alloca i64, align 8
  %alloca_count_1 = alloca i64, align 8
  %alloca_0 = alloca i64, align 8
  %alloca_count_0 = alloca i64, align 8
  store { i64, i64, i64, i64 } %0, ptr %alloca_count_2, align 8
  %load_6 = load i64, ptr %alloca_count_2, align 8
  %icmp_7 = icmp eq i64 %load_6, 0
  store i1 %icmp_7, ptr %alloca_count_4, align 1
  %load_9 = load i1, ptr %alloca_count_4, align 1
  br i1 %load_9, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  %gep_12 = getelementptr inbounds i8, ptr %alloca_count_2, i64 8
  %load_14 = load i64, ptr %gep_12, align 8
  store i64 %load_14, ptr %alloca_count_10, align 8
  %gep_18 = getelementptr inbounds i8, ptr %alloca_count_2, i64 16
  %load_20 = load i64, ptr %gep_18, align 8
  store i64 %load_20, ptr %alloca_count_16, align 8
  %gep_24 = getelementptr inbounds i8, ptr %alloca_count_2, i64 24
  %load_26 = load i64, ptr %gep_24, align 8
  store i64 %load_26, ptr %alloca_count_22, align 8
  %load_29 = load i64, ptr %alloca_count_10, align 8
  %load_30 = load i64, ptr %alloca_count_16, align 8
  %iop_31 = add i64 %load_29, %load_30
  store i64 %iop_31, ptr %alloca_count_28, align 8
  %load_33 = load i64, ptr %alloca_count_28, align 8
  %load_34 = load i64, ptr %alloca_count_22, align 8
  %iop_35 = add i64 %load_33, %load_34
  store i64 %iop_35, ptr %alloca_count_0, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  %load_39 = load i64, ptr %alloca_count_2, align 8
  %icmp_40 = icmp eq i64 %load_39, 1
  store i1 %icmp_40, ptr %alloca_count_37, align 1
  %load_42 = load i1, ptr %alloca_count_37, align 1
  br i1 %load_42, label %bb4, label %bb5

bb1:                                              ; preds = %bb5, %bb4, %bb2
  store { i64, i64, i64, i64 } %0, ptr %alloca_count_43, align 8
  %load_47 = load i64, ptr %alloca_count_43, align 8
  %icmp_48 = icmp eq i64 %load_47, 0
  store i1 %icmp_48, ptr %alloca_count_45, align 1
  %load_50 = load i1, ptr %alloca_count_45, align 1
  br i1 %load_50, label %bb7, label %bb8

bb4:                                              ; preds = %bb3
  %gep_53 = getelementptr inbounds i8, ptr %alloca_count_2, i64 8
  %load_55 = load i64, ptr %gep_53, align 8
  store i64 %load_55, ptr %alloca_count_51, align 8
  %gep_59 = getelementptr inbounds i8, ptr %alloca_count_2, i64 16
  %load_61 = load i64, ptr %gep_59, align 8
  store i64 %load_61, ptr %alloca_count_57, align 8
  %load_63 = load i64, ptr %alloca_count_51, align 8
  %load_64 = load i64, ptr %alloca_count_57, align 8
  %iop_65 = add i64 %load_63, %load_64
  store i64 %iop_65, ptr %alloca_count_0, align 8
  br label %bb1

bb5:                                              ; preds = %bb3
  br label %bb1

bb7:                                              ; preds = %bb1
  %gep_69 = getelementptr inbounds i8, ptr %alloca_count_43, i64 8
  %load_71 = load i64, ptr %gep_69, align 8
  store i64 %load_71, ptr %alloca_count_67, align 8
  %gep_75 = getelementptr inbounds i8, ptr %alloca_count_43, i64 16
  %load_77 = load i64, ptr %gep_75, align 8
  store i64 %load_77, ptr %alloca_count_73, align 8
  %gep_81 = getelementptr inbounds i8, ptr %alloca_count_43, i64 24
  %load_83 = load i64, ptr %gep_81, align 8
  store i64 %load_83, ptr %alloca_count_79, align 8
  %load_86 = load i64, ptr %alloca_count_67, align 8
  %load_87 = load i64, ptr %alloca_count_73, align 8
  %iop_88 = add i64 %load_86, %load_87
  store i64 %iop_88, ptr %alloca_count_85, align 8
  %load_90 = load i64, ptr %alloca_count_85, align 8
  %load_91 = load i64, ptr %alloca_count_79, align 8
  %iop_92 = add i64 %load_90, %load_91
  store i64 %iop_92, ptr %alloca_count_1, align 8
  br label %bb6

bb8:                                              ; preds = %bb1
  %load_96 = load i64, ptr %alloca_count_43, align 8
  %icmp_97 = icmp eq i64 %load_96, 1
  store i1 %icmp_97, ptr %alloca_count_94, align 1
  %load_99 = load i1, ptr %alloca_count_94, align 1
  br i1 %load_99, label %bb9, label %bb10

bb6:                                              ; preds = %bb10, %bb9, %bb7
  %load_100 = load i64, ptr %alloca_count_1, align 8
  ret i64 %load_100

bb9:                                              ; preds = %bb8
  %gep_103 = getelementptr inbounds i8, ptr %alloca_count_43, i64 8
  %load_105 = load i64, ptr %gep_103, align 8
  store i64 %load_105, ptr %alloca_count_101, align 8
  %gep_109 = getelementptr inbounds i8, ptr %alloca_count_43, i64 16
  %load_111 = load i64, ptr %gep_109, align 8
  store i64 %load_111, ptr %alloca_count_107, align 8
  %load_113 = load i64, ptr %alloca_count_101, align 8
  %load_114 = load i64, ptr %alloca_count_107, align 8
  %iop_115 = add i64 %load_113, %load_114
  store i64 %iop_115, ptr %alloca_count_1, align 8
  br label %bb6

bb10:                                             ; preds = %bb8
  br label %bb6
}

define internal void @print_display(ptr %0) {
bb0:
  %call_117 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.0, ptr @.str.14_type_arithmetic.1)
  br label %bb1

bb1:                                              ; preds = %bb0
  ret void
}

declare i32 @printf(ptr, ...)

define internal i64 @describe_optional({ i64, i64, i64, i64 } %0) {
bb0:
  %alloca_195 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_195 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_185 = alloca i64, align 8
  %alloca_count_185 = alloca i64, align 8
  %alloca_179 = alloca i64, align 8
  %alloca_count_179 = alloca i64, align 8
  %alloca_173 = alloca i64, align 8
  %alloca_count_173 = alloca i64, align 8
  %alloca_167 = alloca i64, align 8
  %alloca_count_167 = alloca i64, align 8
  %alloca_163 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_163 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_157 = alloca i1, align 1
  %alloca_count_157 = alloca i1, align 1
  %alloca_155 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_155 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_146 = alloca i64, align 8
  %alloca_count_146 = alloca i64, align 8
  %alloca_140 = alloca i64, align 8
  %alloca_count_140 = alloca i64, align 8
  %alloca_134 = alloca i64, align 8
  %alloca_count_134 = alloca i64, align 8
  %alloca_128 = alloca i64, align 8
  %alloca_count_128 = alloca i64, align 8
  %alloca_122 = alloca i1, align 1
  %alloca_count_122 = alloca i1, align 1
  %alloca_120 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_120 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_119 = alloca i64, align 8
  %alloca_count_119 = alloca i64, align 8
  %alloca_118 = alloca i64, align 8
  %alloca_count_118 = alloca i64, align 8
  store { i64, i64, i64, i64 } %0, ptr %alloca_count_120, align 8
  %load_124 = load i64, ptr %alloca_count_120, align 8
  %icmp_125 = icmp eq i64 %load_124, 0
  store i1 %icmp_125, ptr %alloca_count_122, align 1
  %load_127 = load i1, ptr %alloca_count_122, align 1
  br i1 %load_127, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  %gep_130 = getelementptr inbounds i8, ptr %alloca_count_120, i64 8
  %load_132 = load i64, ptr %gep_130, align 8
  store i64 %load_132, ptr %alloca_count_128, align 8
  %gep_136 = getelementptr inbounds i8, ptr %alloca_count_120, i64 16
  %load_138 = load i64, ptr %gep_136, align 8
  store i64 %load_138, ptr %alloca_count_134, align 8
  %gep_142 = getelementptr inbounds i8, ptr %alloca_count_120, i64 24
  %load_144 = load i64, ptr %gep_142, align 8
  store i64 %load_144, ptr %alloca_count_140, align 8
  %load_147 = load i64, ptr %alloca_count_128, align 8
  %load_148 = load i64, ptr %alloca_count_134, align 8
  %iop_149 = add i64 %load_147, %load_148
  store i64 %iop_149, ptr %alloca_count_146, align 8
  %load_151 = load i64, ptr %alloca_count_146, align 8
  %load_152 = load i64, ptr %alloca_count_140, align 8
  %iop_153 = add i64 %load_151, %load_152
  store i64 %iop_153, ptr %alloca_count_118, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  br label %bb4

bb1:                                              ; preds = %bb4, %bb2
  store { i64, i64, i64, i64 } %0, ptr %alloca_count_155, align 8
  %load_159 = load i64, ptr %alloca_count_155, align 8
  %icmp_160 = icmp eq i64 %load_159, 0
  store i1 %icmp_160, ptr %alloca_count_157, align 1
  %load_162 = load i1, ptr %alloca_count_157, align 1
  br i1 %load_162, label %bb7, label %bb8

bb4:                                              ; preds = %bb3
  %load_164 = load { i64, i64, i64, i64 }, ptr %alloca_count_120, align 8
  store { i64, i64, i64, i64 } %load_164, ptr %alloca_count_163, align 8
  store i64 0, ptr %alloca_count_118, align 8
  br label %bb1

bb7:                                              ; preds = %bb1
  %gep_169 = getelementptr inbounds i8, ptr %alloca_count_155, i64 8
  %load_171 = load i64, ptr %gep_169, align 8
  store i64 %load_171, ptr %alloca_count_167, align 8
  %gep_175 = getelementptr inbounds i8, ptr %alloca_count_155, i64 16
  %load_177 = load i64, ptr %gep_175, align 8
  store i64 %load_177, ptr %alloca_count_173, align 8
  %gep_181 = getelementptr inbounds i8, ptr %alloca_count_155, i64 24
  %load_183 = load i64, ptr %gep_181, align 8
  store i64 %load_183, ptr %alloca_count_179, align 8
  %load_186 = load i64, ptr %alloca_count_167, align 8
  %load_187 = load i64, ptr %alloca_count_173, align 8
  %iop_188 = add i64 %load_186, %load_187
  store i64 %iop_188, ptr %alloca_count_185, align 8
  %load_190 = load i64, ptr %alloca_count_185, align 8
  %load_191 = load i64, ptr %alloca_count_179, align 8
  %iop_192 = add i64 %load_190, %load_191
  store i64 %iop_192, ptr %alloca_count_119, align 8
  br label %bb6

bb8:                                              ; preds = %bb1
  br label %bb9

bb6:                                              ; preds = %bb9, %bb7
  %load_194 = load i64, ptr %alloca_count_119, align 8
  ret i64 %load_194

bb9:                                              ; preds = %bb8
  %load_196 = load { i64, i64, i64, i64 }, ptr %alloca_count_155, align 8
  store { i64, i64, i64, i64 } %load_196, ptr %alloca_count_195, align 8
  store i64 0, ptr %alloca_count_119, align 8
  br label %bb6

bb5:                                              ; No predecessors!
  %load_199 = load i64, ptr %alloca_count_119, align 8
  ret i64 %load_199

bb10:                                             ; No predecessors!
  %load_200 = load i64, ptr %alloca_count_119, align 8
  ret i64 %load_200
}

define i32 @main() {
bb0:
  %alloca_316 = alloca ptr, align 8
  %alloca_count_316 = alloca ptr, align 8
  %alloca_313 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_313 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_305 = alloca ptr, align 8
  %alloca_count_305 = alloca ptr, align 8
  %alloca_303 = alloca ptr, align 8
  %alloca_count_303 = alloca ptr, align 8
  %alloca_301 = alloca ptr, align 8
  %alloca_count_301 = alloca ptr, align 8
  %alloca_299 = alloca i1, align 1
  %alloca_count_299 = alloca i1, align 1
  %alloca_297 = alloca i64, align 8
  %alloca_count_297 = alloca i64, align 8
  %alloca_293 = alloca [4 x i64], align 8
  %alloca_count_293 = alloca [4 x i64], align 8
  %alloca_291 = alloca [4 x i64], align 8
  %alloca_count_291 = alloca [4 x i64], align 8
  %alloca_288 = alloca ptr, align 8
  %alloca_count_288 = alloca ptr, align 8
  %alloca_285 = alloca { i64, i64 }, align 8
  %alloca_count_285 = alloca { i64, i64 }, align 8
  %alloca_283 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_283 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_274 = alloca { i64, i64 }, align 8
  %alloca_count_274 = alloca { i64, i64 }, align 8
  %alloca_269 = alloca { i64 }, align 8
  %alloca_count_269 = alloca { i64 }, align 8
  %alloca_260 = alloca { i64, i64 }, align 8
  %alloca_count_260 = alloca { i64, i64 }, align 8
  %alloca_242 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_242 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_226 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_226 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_223 = alloca { i64, i64, i64 }, align 8
  %alloca_count_223 = alloca { i64, i64, i64 }, align 8
  %alloca_214 = alloca { i64, i64 }, align 8
  %alloca_count_214 = alloca { i64, i64 }, align 8
  %alloca_201 = alloca { i64, i64, i64 }, align 8
  %alloca_count_201 = alloca { i64, i64, i64 }, align 8
  store { i64, i64, i64 } { i64 1, i64 2, i64 3 }, ptr %alloca_count_201, align 8
  %load_204 = load i64, ptr %alloca_count_201, align 8
  %gep_206 = getelementptr inbounds i8, ptr %alloca_count_201, i64 8
  %load_208 = load i64, ptr %gep_206, align 8
  %gep_210 = getelementptr inbounds i8, ptr %alloca_count_201, i64 16
  %load_212 = load i64, ptr %gep_210, align 8
  %call_213 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.2, i64 %load_204, i64 %load_208, i64 %load_212)
  br label %bb1

bb1:                                              ; preds = %bb0
  store { i64, i64 } { i64 4, i64 5 }, ptr %alloca_count_214, align 8
  %load_217 = load i64, ptr %alloca_count_214, align 8
  %gep_219 = getelementptr inbounds i8, ptr %alloca_count_214, i64 8
  %load_221 = load i64, ptr %gep_219, align 8
  %call_222 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.3, i64 %load_217, i64 %load_221)
  br label %bb2

bb2:                                              ; preds = %bb1
  %load_224 = load { i64, i64, i64 }, ptr %alloca_count_201, align 8
  store { i64, i64, i64 } %load_224, ptr %alloca_count_223, align 8
  %load_228 = load i64, ptr %alloca_count_223, align 8
  %gep_230 = getelementptr inbounds i8, ptr %alloca_count_223, i64 8
  %load_232 = load i64, ptr %gep_230, align 8
  %gep_234 = getelementptr inbounds i8, ptr %alloca_count_223, i64 16
  %load_236 = load i64, ptr %gep_234, align 8
  %insertvalue_237 = insertvalue { i64, i64, i64, i64 } undef, i64 %load_228, 0
  %insertvalue_238 = insertvalue { i64, i64, i64, i64 } %insertvalue_237, i64 %load_232, 1
  %insertvalue_239 = insertvalue { i64, i64, i64, i64 } %insertvalue_238, i64 %load_236, 2
  %insertvalue_240 = insertvalue { i64, i64, i64, i64 } %insertvalue_239, i64 6, 3
  store { i64, i64, i64, i64 } %insertvalue_240, ptr %alloca_count_226, align 8
  %load_243 = load { i64, i64, i64, i64 }, ptr %alloca_count_226, align 8
  store { i64, i64, i64, i64 } %load_243, ptr %alloca_count_242, align 8
  %load_246 = load i64, ptr %alloca_count_242, align 8
  %gep_248 = getelementptr inbounds i8, ptr %alloca_count_242, i64 8
  %load_250 = load i64, ptr %gep_248, align 8
  %gep_252 = getelementptr inbounds i8, ptr %alloca_count_242, i64 16
  %load_254 = load i64, ptr %gep_252, align 8
  %gep_256 = getelementptr inbounds i8, ptr %alloca_count_242, i64 24
  %load_258 = load i64, ptr %gep_256, align 8
  %call_259 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.4, i64 %load_246, i64 %load_250, i64 %load_254, i64 %load_258)
  br label %bb3

bb3:                                              ; preds = %bb2
  store { i64, i64 } { i64 10, i64 20 }, ptr %alloca_count_260, align 8
  %load_263 = load i64, ptr %alloca_count_260, align 8
  %gep_265 = getelementptr inbounds i8, ptr %alloca_count_260, i64 8
  %load_267 = load i64, ptr %gep_265, align 8
  %call_268 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.5, i64 %load_263, i64 %load_267)
  br label %bb4

bb4:                                              ; preds = %bb3
  store { i64 } { i64 99 }, ptr %alloca_count_269, align 8
  %load_272 = load i64, ptr %alloca_count_269, align 8
  %call_273 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.6, i64 %load_272)
  br label %bb5

bb5:                                              ; preds = %bb4
  store { i64, i64 } { i64 7, i64 42 }, ptr %alloca_count_274, align 8
  %load_277 = load i64, ptr %alloca_count_274, align 8
  %gep_279 = getelementptr inbounds i8, ptr %alloca_count_274, i64 8
  %load_281 = load i64, ptr %gep_279, align 8
  %call_282 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.7, i64 %load_277, i64 %load_281)
  br label %bb6

bb6:                                              ; preds = %bb5
  store { i64, i64, i64, i64 } { i64 0, i64 2, i64 3, i64 4 }, ptr %alloca_count_283, align 8
  %load_286 = load { i64, i64 }, ptr %alloca_count_214, align 8
  store { i64, i64 } %load_286, ptr %alloca_count_285, align 8
  %call_289 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.8, ptr @.str.14_type_arithmetic.1)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_290 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.9, ptr @.str.14_type_arithmetic.1)
  br label %bb8

bb8:                                              ; preds = %bb7
  store [4 x i64] [i64 1, i64 2, i64 3, i64 4], ptr %alloca_count_291, align 8
  %load_294 = load [4 x i64], ptr %alloca_count_291, align 8
  store [4 x i64] %load_294, ptr %alloca_count_293, align 8
  %call_296 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.10, ptr @.str.14_type_arithmetic.1, ptr @.str.14_type_arithmetic.1, ptr @.str.14_type_arithmetic.1, ptr @.str.14_type_arithmetic.1)
  br label %bb9

bb9:                                              ; preds = %bb8
  store i64 42, ptr %alloca_count_297, align 8
  store i1 true, ptr %alloca_count_299, align 1
  store ptr @.str.14_type_arithmetic.11, ptr %alloca_count_301, align 8
  store ptr null, ptr %alloca_count_303, align 8
  store ptr @.str.14_type_arithmetic.12, ptr %alloca_count_305, align 8
  %call_307 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.13, ptr @.str.14_type_arithmetic.1)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_308 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.14, ptr @.str.14_type_arithmetic.1)
  br label %bb11

bb11:                                             ; preds = %bb10
  %call_309 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.15, ptr @.str.14_type_arithmetic.1)
  br label %bb12

bb12:                                             ; preds = %bb11
  %call_310 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.16, ptr @.str.14_type_arithmetic.1)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_311 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.17, ptr @.str.14_type_arithmetic.1)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_312 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.18, ptr @.str.14_type_arithmetic.1)
  br label %bb15

bb15:                                             ; preds = %bb14
  store { i64, i64, i64, i64 } { i64 0, i64 5, i64 6, i64 7 }, ptr %alloca_count_313, align 8
  %call_315 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.19, ptr @.str.14_type_arithmetic.1)
  br label %bb16

bb16:                                             ; preds = %bb15
  ret i32 0
}
