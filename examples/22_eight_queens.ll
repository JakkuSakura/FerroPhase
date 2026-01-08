; ModuleID = '22_eight_queens'
source_filename = "22_eight_queens"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_5 = internal constant [0 x { { ptr, i64 }, ptr }] zeroinitializer, align 8
@.str.22_eight_queens.0 = constant [51 x i8] c"test result: %lld passed; %lld failed; %lld total\0A\00"
@.str.22_eight_queens.1 = constant [13 x i8] c"  %s ... ok\0A\00"
@.str.22_eight_queens.2 = constant [17 x i8] c"  %s ... FAILED\0A\00"
@.str.22_eight_queens.3 = constant [17 x i8] c"First solution:\0A\00"
@.str.22_eight_queens.4 = constant [2 x i8] c"\0A\00"
@.str.22_eight_queens.5 = constant [3 x i8] c"Q \00"
@.str.22_eight_queens.6 = constant [3 x i8] c". \00"
@.str.22_eight_queens.7 = constant [35 x i8] c"\F0\9F\93\98 Tutorial: 22_eight_queens.fp\0A\00"
@.str.22_eight_queens.8 = constant [67 x i8] c"\F0\9F\A7\AD Focus: Classic 8-queens solver using recursive backtracking.\0A\00"
@.str.22_eight_queens.9 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.22_eight_queens.10 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.22_eight_queens.11 = constant [23 x i8] c"Total solutions: %lld\0A\00"

define internal void @__closure0_call({ ptr } %0) {
bb0:
  %alloca_0 = alloca ptr, align 8
  %alloca_count_0 = alloca ptr, align 8
  store { ptr } %0, ptr %alloca_count_0, align 8
  %load_3 = load ptr, ptr %alloca_count_0, align 8
  call void @TestCase__run(ptr %load_3)
  br label %bb1

bb1:                                              ; preds = %bb0
  ret void
}

define internal { i64, i64, i64 } @std__test__run_tests() personality ptr @__gxx_personality_v0 {
bb0:
  %alloca_68 = alloca i1, align 1
  %alloca_count_68 = alloca i1, align 1
  %alloca_50 = alloca i64, align 8
  %alloca_count_50 = alloca i64, align 8
  %alloca_45 = alloca i64, align 8
  %alloca_count_45 = alloca i64, align 8
  %alloca_35 = alloca { { ptr, i64 }, ptr }, align 8
  %alloca_count_35 = alloca { { ptr, i64 }, ptr }, align 8
  %alloca_32 = alloca i64, align 8
  %alloca_count_32 = alloca i64, align 8
  %alloca_29 = alloca i64, align 8
  %alloca_count_29 = alloca i64, align 8
  %alloca_23 = alloca i1, align 1
  %alloca_count_23 = alloca i1, align 1
  %alloca_21 = alloca i64, align 8
  %alloca_count_21 = alloca i64, align 8
  %alloca_15 = alloca [0 x { { ptr, i64 }, ptr }], align 8
  %alloca_count_15 = alloca [0 x { { ptr, i64 }, ptr }], align 8
  %alloca_13 = alloca [0 x { { ptr, i64 }, ptr }], align 8
  %alloca_count_13 = alloca [0 x { { ptr, i64 }, ptr }], align 8
  %alloca_11 = alloca [0 x { { ptr, i64 }, ptr }], align 8
  %alloca_count_11 = alloca [0 x { { ptr, i64 }, ptr }], align 8
  %alloca_10 = alloca { i64, i64, i64 }, align 8
  %alloca_count_10 = alloca { i64, i64, i64 }, align 8
  %alloca_9 = alloca i64, align 8
  %alloca_count_9 = alloca i64, align 8
  %alloca_8 = alloca i64, align 8
  %alloca_count_8 = alloca i64, align 8
  %alloca_7 = alloca i1, align 1
  %alloca_count_7 = alloca i1, align 1
  %alloca_6 = alloca i64, align 8
  %alloca_count_6 = alloca i64, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_11, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_13, align 8
  %load_16 = load [0 x { { ptr, i64 }, ptr }], ptr %alloca_count_13, align 8
  store [0 x { { ptr, i64 }, ptr }] %load_16, ptr %alloca_count_15, align 8
  store i64 0, ptr %alloca_count_6, align 8
  store i64 0, ptr %alloca_count_9, align 8
  store i64 0, ptr %alloca_count_8, align 8
  br label %bb1

bb1:                                              ; preds = %bb9, %bb0
  store i64 0, ptr %alloca_count_21, align 8
  %load_24 = load i64, ptr %alloca_count_8, align 8
  %load_25 = load i64, ptr %alloca_count_21, align 8
  %icmp_26 = icmp slt i64 %load_24, %load_25
  store i1 %icmp_26, ptr %alloca_count_23, align 1
  %load_28 = load i1, ptr %alloca_count_23, align 1
  br i1 %load_28, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_30 = load i64, ptr %alloca_count_8, align 8
  store i64 %load_30, ptr %alloca_count_29, align 8
  %load_33 = load i64, ptr %alloca_count_8, align 8
  store i64 %load_33, ptr %alloca_count_32, align 8
  %load_36 = load i64, ptr %alloca_count_32, align 8
  %iop_37 = mul i64 %load_36, 24
  %gep_39 = getelementptr inbounds i8, ptr %alloca_count_15, i64 %iop_37
  %load_41 = load { { ptr, i64 }, ptr }, ptr %gep_39, align 8
  store { { ptr, i64 }, ptr } %load_41, ptr %alloca_count_35, align 8
  %load_43 = load { { ptr, i64 }, ptr }, ptr %alloca_count_35, align 8
  invoke void @__closure0_call({ ptr } undef)
          to label %bb4 unwind label %bb5

bb3:                                              ; preds = %bb1
  %load_46 = load i64, ptr %alloca_count_6, align 8
  %load_47 = load i64, ptr %alloca_count_9, align 8
  %iop_48 = add i64 %load_46, %load_47
  store i64 %iop_48, ptr %alloca_count_45, align 8
  %load_51 = load i64, ptr %alloca_count_45, align 8
  store i64 %load_51, ptr %alloca_count_50, align 8
  %load_53 = load i64, ptr %alloca_count_6, align 8
  %load_54 = load i64, ptr %alloca_count_9, align 8
  %load_55 = load i64, ptr %alloca_count_50, align 8
  %call_56 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.0, i64 %load_53, i64 %load_54, i64 %load_55)
  br label %bb12

bb4:                                              ; preds = %bb2
  store i1 true, ptr %alloca_count_7, align 1
  br label %bb6

bb5:                                              ; preds = %bb2
  %landingpad = landingpad { ptr, i32 }
          catch ptr null
  store i1 false, ptr %alloca_count_7, align 1
  br label %bb6

bb12:                                             ; preds = %bb3
  %load_60 = load i64, ptr %alloca_count_50, align 8
  %load_61 = load i64, ptr %alloca_count_6, align 8
  %load_62 = load i64, ptr %alloca_count_9, align 8
  %insertvalue_63 = insertvalue { i64, i64, i64 } undef, i64 %load_60, 0
  %insertvalue_64 = insertvalue { i64, i64, i64 } %insertvalue_63, i64 %load_61, 1
  %insertvalue_65 = insertvalue { i64, i64, i64 } %insertvalue_64, i64 %load_62, 2
  store { i64, i64, i64 } %insertvalue_65, ptr %alloca_count_10, align 8
  %load_67 = load { i64, i64, i64 }, ptr %alloca_count_10, align 8
  ret { i64, i64, i64 } %load_67

bb6:                                              ; preds = %bb5, %bb4
  %load_69 = load i1, ptr %alloca_count_7, align 1
  store i1 %load_69, ptr %alloca_count_68, align 1
  %load_71 = load i1, ptr %alloca_count_68, align 1
  br i1 %load_71, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_72 = load i64, ptr %alloca_count_6, align 8
  %iop_73 = add i64 %load_72, 1
  store i64 %iop_73, ptr %alloca_count_6, align 8
  %load_76 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_77 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.1, { ptr, i64 } %load_76)
  br label %bb10

bb8:                                              ; preds = %bb6
  %load_78 = load i64, ptr %alloca_count_9, align 8
  %iop_79 = add i64 %load_78, 1
  store i64 %iop_79, ptr %alloca_count_9, align 8
  %load_82 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_83 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.2, { ptr, i64 } %load_82)
  br label %bb11

bb10:                                             ; preds = %bb7
  br label %bb9

bb11:                                             ; preds = %bb8
  br label %bb9

bb9:                                              ; preds = %bb11, %bb10
  %load_84 = load i64, ptr %alloca_count_8, align 8
  %iop_85 = add i64 %load_84, 1
  store i64 %iop_85, ptr %alloca_count_8, align 8
  br label %bb1
}

define internal { i64, i64, i64 } @std__test__run() {
bb0:
  %alloca_87 = alloca { i64, i64, i64 }, align 8
  %alloca_count_87 = alloca { i64, i64, i64 }, align 8
  %call_88 = call { i64, i64, i64 } @std__test__run_tests()
  store { i64, i64, i64 } %call_88, ptr %alloca_count_87, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %load_90 = load { i64, i64, i64 }, ptr %alloca_count_87, align 8
  ret { i64, i64, i64 } %load_90
}

define internal i64 @solve(i64 %0, ptr %1, ptr %2, ptr %3, ptr %4, ptr %5, ptr %6) {
bb0:
  %alloca_303 = alloca i64, align 8
  %alloca_count_303 = alloca i64, align 8
  %alloca_293 = alloca i64, align 8
  %alloca_count_293 = alloca i64, align 8
  %alloca_283 = alloca i64, align 8
  %alloca_count_283 = alloca i64, align 8
  %alloca_273 = alloca i64, align 8
  %alloca_count_273 = alloca i64, align 8
  %alloca_261 = alloca i64, align 8
  %alloca_count_261 = alloca i64, align 8
  %alloca_251 = alloca i64, align 8
  %alloca_count_251 = alloca i64, align 8
  %alloca_241 = alloca i64, align 8
  %alloca_count_241 = alloca i64, align 8
  %alloca_231 = alloca i64, align 8
  %alloca_count_231 = alloca i64, align 8
  %alloca_221 = alloca i64, align 8
  %alloca_count_221 = alloca i64, align 8
  %alloca_212 = alloca i1, align 1
  %alloca_count_212 = alloca i1, align 1
  %alloca_202 = alloca i1, align 1
  %alloca_count_202 = alloca i1, align 1
  %alloca_199 = alloca i64, align 8
  %alloca_count_199 = alloca i64, align 8
  %alloca_194 = alloca i1, align 1
  %alloca_count_194 = alloca i1, align 1
  %alloca_184 = alloca i1, align 1
  %alloca_count_184 = alloca i1, align 1
  %alloca_181 = alloca i64, align 8
  %alloca_count_181 = alloca i64, align 8
  %alloca_171 = alloca i1, align 1
  %alloca_count_171 = alloca i1, align 1
  %alloca_168 = alloca i64, align 8
  %alloca_count_168 = alloca i64, align 8
  %alloca_165 = alloca i64, align 8
  %alloca_count_165 = alloca i64, align 8
  %alloca_161 = alloca i64, align 8
  %alloca_count_161 = alloca i64, align 8
  %alloca_157 = alloca i64, align 8
  %alloca_count_157 = alloca i64, align 8
  %alloca_154 = alloca i64, align 8
  %alloca_count_154 = alloca i64, align 8
  %alloca_150 = alloca i64, align 8
  %alloca_count_150 = alloca i64, align 8
  %alloca_130 = alloca i64, align 8
  %alloca_count_130 = alloca i64, align 8
  %alloca_127 = alloca i64, align 8
  %alloca_count_127 = alloca i64, align 8
  %alloca_122 = alloca i1, align 1
  %alloca_count_122 = alloca i1, align 1
  %alloca_115 = alloca i1, align 1
  %alloca_count_115 = alloca i1, align 1
  %alloca_107 = alloca i1, align 1
  %alloca_count_107 = alloca i1, align 1
  %alloca_103 = alloca i1, align 1
  %alloca_count_103 = alloca i1, align 1
  %alloca_102 = alloca i64, align 8
  %alloca_count_102 = alloca i64, align 8
  %alloca_100 = alloca ptr, align 8
  %alloca_count_100 = alloca ptr, align 8
  %alloca_98 = alloca ptr, align 8
  %alloca_count_98 = alloca ptr, align 8
  %alloca_96 = alloca ptr, align 8
  %alloca_count_96 = alloca ptr, align 8
  %alloca_95 = alloca i64, align 8
  %alloca_count_95 = alloca i64, align 8
  %alloca_94 = alloca i64, align 8
  %alloca_count_94 = alloca i64, align 8
  %alloca_92 = alloca ptr, align 8
  %alloca_count_92 = alloca ptr, align 8
  %alloca_91 = alloca i64, align 8
  %alloca_count_91 = alloca i64, align 8
  store ptr %2, ptr %alloca_count_92, align 8
  store ptr %4, ptr %alloca_count_96, align 8
  store ptr %3, ptr %alloca_count_98, align 8
  store ptr %1, ptr %alloca_count_100, align 8
  %icmp_104 = icmp eq i64 %0, 8
  store i1 %icmp_104, ptr %alloca_count_103, align 1
  %load_106 = load i1, ptr %alloca_count_103, align 1
  br i1 %load_106, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  %load_108 = load i1, ptr %6, align 1
  %zext = zext i1 %load_108 to i64
  %not_109 = xor i64 %zext, -1
  store i64 %not_109, ptr %alloca_count_107, align 1
  %load_111 = load i1, ptr %alloca_count_107, align 1
  br i1 %load_111, label %bb4, label %bb5

bb2:                                              ; preds = %bb0
  br label %bb3

bb4:                                              ; preds = %bb1
  store i64 0, ptr %alloca_count_91, align 8
  br label %bb7

bb5:                                              ; preds = %bb1
  br label %bb6

bb3:                                              ; preds = %bb10, %bb2
  store i64 0, ptr %alloca_count_102, align 8
  store i64 0, ptr %alloca_count_95, align 8
  br label %bb11

bb7:                                              ; preds = %bb8, %bb4
  %load_116 = load i64, ptr %alloca_count_91, align 8
  %icmp_117 = icmp slt i64 %load_116, 8
  store i1 %icmp_117, ptr %alloca_count_115, align 1
  %load_119 = load i1, ptr %alloca_count_115, align 1
  br i1 %load_119, label %bb8, label %bb9

bb6:                                              ; preds = %bb9, %bb5
  store i64 1, ptr %alloca_count_94, align 8
  %load_121 = load i64, ptr %alloca_count_94, align 8
  ret i64 %load_121

bb11:                                             ; preds = %bb16, %bb3
  %load_123 = load i64, ptr %alloca_count_95, align 8
  %icmp_124 = icmp slt i64 %load_123, 8
  store i1 %icmp_124, ptr %alloca_count_122, align 1
  %load_126 = load i1, ptr %alloca_count_122, align 1
  br i1 %load_126, label %bb12, label %bb13

bb8:                                              ; preds = %bb7
  %load_128 = load i64, ptr %alloca_count_91, align 8
  store i64 %load_128, ptr %alloca_count_127, align 8
  %load_131 = load i64, ptr %alloca_count_91, align 8
  store i64 %load_131, ptr %alloca_count_130, align 8
  %load_133 = load i64, ptr %alloca_count_127, align 8
  %iop_134 = mul i64 %load_133, 8
  %gep_136 = getelementptr inbounds i8, ptr %5, i64 %iop_134
  %load_138 = load ptr, ptr %alloca_count_96, align 8
  %load_139 = load i64, ptr %alloca_count_130, align 8
  %iop_140 = mul i64 %load_139, 8
  %gep_142 = getelementptr inbounds i8, ptr %load_138, i64 %iop_140
  %load_144 = load i64, ptr %gep_142, align 8
  store i64 %load_144, ptr %gep_136, align 8
  %load_146 = load i64, ptr %alloca_count_91, align 8
  %iop_147 = add i64 %load_146, 1
  store i64 %iop_147, ptr %alloca_count_91, align 8
  br label %bb7

bb9:                                              ; preds = %bb7
  store i1 true, ptr %6, align 1
  br label %bb6

bb12:                                             ; preds = %bb11
  %load_151 = load i64, ptr %alloca_count_95, align 8
  %iop_152 = add i64 %0, %load_151
  store i64 %iop_152, ptr %alloca_count_150, align 8
  %load_155 = load i64, ptr %alloca_count_150, align 8
  store i64 %load_155, ptr %alloca_count_154, align 8
  %load_158 = load i64, ptr %alloca_count_95, align 8
  %iop_159 = sub i64 %0, %load_158
  store i64 %iop_159, ptr %alloca_count_157, align 8
  %load_162 = load i64, ptr %alloca_count_157, align 8
  %iop_163 = add i64 %load_162, 7
  store i64 %iop_163, ptr %alloca_count_161, align 8
  %load_166 = load i64, ptr %alloca_count_161, align 8
  store i64 %load_166, ptr %alloca_count_165, align 8
  %load_169 = load i64, ptr %alloca_count_95, align 8
  store i64 %load_169, ptr %alloca_count_168, align 8
  %load_172 = load ptr, ptr %alloca_count_100, align 8
  %load_173 = load i64, ptr %alloca_count_168, align 8
  %iop_174 = mul i64 %load_173, 8
  %gep_176 = getelementptr inbounds i8, ptr %load_172, i64 %iop_174
  %load_178 = load i64, ptr %gep_176, align 8
  %icmp_179 = icmp eq i64 %load_178, 0
  store i1 %icmp_179, ptr %alloca_count_171, align 1
  %load_182 = load i64, ptr %alloca_count_154, align 8
  store i64 %load_182, ptr %alloca_count_181, align 8
  %load_185 = load ptr, ptr %alloca_count_92, align 8
  %load_186 = load i64, ptr %alloca_count_181, align 8
  %iop_187 = mul i64 %load_186, 8
  %gep_189 = getelementptr inbounds i8, ptr %load_185, i64 %iop_187
  %load_191 = load i64, ptr %gep_189, align 8
  %icmp_192 = icmp eq i64 %load_191, 0
  store i1 %icmp_192, ptr %alloca_count_184, align 1
  %load_195 = load i1, ptr %alloca_count_171, align 1
  %load_196 = load i1, ptr %alloca_count_184, align 1
  %zext1 = zext i1 %load_195 to i64
  %zext2 = zext i1 %load_196 to i64
  %iop_197 = and i64 %zext1, %zext2
  store i64 %iop_197, ptr %alloca_count_194, align 1
  %load_200 = load i64, ptr %alloca_count_165, align 8
  store i64 %load_200, ptr %alloca_count_199, align 8
  %load_203 = load ptr, ptr %alloca_count_98, align 8
  %load_204 = load i64, ptr %alloca_count_199, align 8
  %iop_205 = mul i64 %load_204, 8
  %gep_207 = getelementptr inbounds i8, ptr %load_203, i64 %iop_205
  %load_209 = load i64, ptr %gep_207, align 8
  %icmp_210 = icmp eq i64 %load_209, 0
  store i1 %icmp_210, ptr %alloca_count_202, align 1
  %load_213 = load i1, ptr %alloca_count_194, align 1
  %load_214 = load i1, ptr %alloca_count_202, align 1
  %zext3 = zext i1 %load_213 to i64
  %zext4 = zext i1 %load_214 to i64
  %iop_215 = and i64 %zext3, %zext4
  store i64 %iop_215, ptr %alloca_count_212, align 1
  %load_217 = load i1, ptr %alloca_count_212, align 1
  br i1 %load_217, label %bb14, label %bb15

bb13:                                             ; preds = %bb11
  %load_218 = load i64, ptr %alloca_count_102, align 8
  store i64 %load_218, ptr %alloca_count_94, align 8
  %load_220 = load i64, ptr %alloca_count_94, align 8
  ret i64 %load_220

bb14:                                             ; preds = %bb12
  %load_222 = load i64, ptr %alloca_count_95, align 8
  store i64 %load_222, ptr %alloca_count_221, align 8
  %load_224 = load ptr, ptr %alloca_count_100, align 8
  %load_225 = load i64, ptr %alloca_count_221, align 8
  %iop_226 = mul i64 %load_225, 8
  %gep_228 = getelementptr inbounds i8, ptr %load_224, i64 %iop_226
  store i64 1, ptr %gep_228, align 8
  %load_232 = load i64, ptr %alloca_count_154, align 8
  store i64 %load_232, ptr %alloca_count_231, align 8
  %load_234 = load ptr, ptr %alloca_count_92, align 8
  %load_235 = load i64, ptr %alloca_count_231, align 8
  %iop_236 = mul i64 %load_235, 8
  %gep_238 = getelementptr inbounds i8, ptr %load_234, i64 %iop_236
  store i64 1, ptr %gep_238, align 8
  %load_242 = load i64, ptr %alloca_count_165, align 8
  store i64 %load_242, ptr %alloca_count_241, align 8
  %load_244 = load ptr, ptr %alloca_count_98, align 8
  %load_245 = load i64, ptr %alloca_count_241, align 8
  %iop_246 = mul i64 %load_245, 8
  %gep_248 = getelementptr inbounds i8, ptr %load_244, i64 %iop_246
  store i64 1, ptr %gep_248, align 8
  store i64 %0, ptr %alloca_count_251, align 8
  %load_253 = load ptr, ptr %alloca_count_96, align 8
  %load_254 = load i64, ptr %alloca_count_251, align 8
  %iop_255 = mul i64 %load_254, 8
  %gep_257 = getelementptr inbounds i8, ptr %load_253, i64 %iop_255
  %load_259 = load i64, ptr %alloca_count_95, align 8
  store i64 %load_259, ptr %gep_257, align 8
  %iop_262 = add i64 %0, 1
  store i64 %iop_262, ptr %alloca_count_261, align 8
  %load_264 = load i64, ptr %alloca_count_261, align 8
  %load_265 = load ptr, ptr %alloca_count_100, align 8
  %load_266 = load ptr, ptr %alloca_count_92, align 8
  %load_267 = load ptr, ptr %alloca_count_98, align 8
  %load_268 = load ptr, ptr %alloca_count_96, align 8
  %call_269 = call i64 @solve(i64 %load_264, ptr %load_265, ptr %load_266, ptr %load_267, ptr %load_268, ptr %5, ptr %6)
  br label %bb17

bb15:                                             ; preds = %bb12
  br label %bb16

bb17:                                             ; preds = %bb14
  %load_270 = load i64, ptr %alloca_count_102, align 8
  %iop_271 = add i64 %load_270, %call_269
  store i64 %iop_271, ptr %alloca_count_102, align 8
  %load_274 = load i64, ptr %alloca_count_95, align 8
  store i64 %load_274, ptr %alloca_count_273, align 8
  %load_276 = load ptr, ptr %alloca_count_100, align 8
  %load_277 = load i64, ptr %alloca_count_273, align 8
  %iop_278 = mul i64 %load_277, 8
  %gep_280 = getelementptr inbounds i8, ptr %load_276, i64 %iop_278
  store i64 0, ptr %gep_280, align 8
  %load_284 = load i64, ptr %alloca_count_154, align 8
  store i64 %load_284, ptr %alloca_count_283, align 8
  %load_286 = load ptr, ptr %alloca_count_92, align 8
  %load_287 = load i64, ptr %alloca_count_283, align 8
  %iop_288 = mul i64 %load_287, 8
  %gep_290 = getelementptr inbounds i8, ptr %load_286, i64 %iop_288
  store i64 0, ptr %gep_290, align 8
  %load_294 = load i64, ptr %alloca_count_165, align 8
  store i64 %load_294, ptr %alloca_count_293, align 8
  %load_296 = load ptr, ptr %alloca_count_98, align 8
  %load_297 = load i64, ptr %alloca_count_293, align 8
  %iop_298 = mul i64 %load_297, 8
  %gep_300 = getelementptr inbounds i8, ptr %load_296, i64 %iop_298
  store i64 0, ptr %gep_300, align 8
  store i64 %0, ptr %alloca_count_303, align 8
  %load_305 = load ptr, ptr %alloca_count_96, align 8
  %load_306 = load i64, ptr %alloca_count_303, align 8
  %iop_307 = mul i64 %load_306, 8
  %gep_309 = getelementptr inbounds i8, ptr %load_305, i64 %iop_307
  store i64 -1, ptr %gep_309, align 8
  br label %bb16

bb16:                                             ; preds = %bb17, %bb15
  %load_313 = load i64, ptr %alloca_count_95, align 8
  %iop_314 = add i64 %load_313, 1
  store i64 %iop_314, ptr %alloca_count_95, align 8
  br label %bb11

bb10:                                             ; No predecessors!
  br label %bb3
}

define internal void @print_board(ptr %0) {
bb0:
  %alloca_334 = alloca i1, align 1
  %alloca_count_334 = alloca i1, align 1
  %alloca_331 = alloca i64, align 8
  %alloca_count_331 = alloca i64, align 8
  %alloca_326 = alloca i1, align 1
  %alloca_count_326 = alloca i1, align 1
  %alloca_320 = alloca i1, align 1
  %alloca_count_320 = alloca i1, align 1
  %alloca_317 = alloca i64, align 8
  %alloca_count_317 = alloca i64, align 8
  %alloca_316 = alloca i64, align 8
  %alloca_count_316 = alloca i64, align 8
  %call_318 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.3)
  br label %bb1

bb1:                                              ; preds = %bb0
  store i64 0, ptr %alloca_count_317, align 8
  br label %bb2

bb2:                                              ; preds = %bb7, %bb1
  %load_321 = load i64, ptr %alloca_count_317, align 8
  %icmp_322 = icmp slt i64 %load_321, 8
  store i1 %icmp_322, ptr %alloca_count_320, align 1
  %load_324 = load i1, ptr %alloca_count_320, align 1
  br i1 %load_324, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  store i64 0, ptr %alloca_count_316, align 8
  br label %bb5

bb4:                                              ; preds = %bb2
  ret void

bb5:                                              ; preds = %bb10, %bb3
  %load_327 = load i64, ptr %alloca_count_316, align 8
  %icmp_328 = icmp slt i64 %load_327, 8
  store i1 %icmp_328, ptr %alloca_count_326, align 1
  %load_330 = load i1, ptr %alloca_count_326, align 1
  br i1 %load_330, label %bb6, label %bb7

bb6:                                              ; preds = %bb5
  %load_332 = load i64, ptr %alloca_count_317, align 8
  store i64 %load_332, ptr %alloca_count_331, align 8
  %load_335 = load i64, ptr %alloca_count_331, align 8
  %iop_336 = mul i64 %load_335, 8
  %gep_338 = getelementptr inbounds i8, ptr %0, i64 %iop_336
  %load_340 = load i64, ptr %gep_338, align 8
  %load_341 = load i64, ptr %alloca_count_316, align 8
  %icmp_342 = icmp eq i64 %load_340, %load_341
  store i1 %icmp_342, ptr %alloca_count_334, align 1
  %load_344 = load i1, ptr %alloca_count_334, align 1
  br i1 %load_344, label %bb8, label %bb9

bb7:                                              ; preds = %bb5
  %call_345 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.4)
  %load_346 = load i64, ptr %alloca_count_317, align 8
  %iop_347 = add i64 %load_346, 1
  store i64 %iop_347, ptr %alloca_count_317, align 8
  br label %bb2

bb8:                                              ; preds = %bb6
  %call_349 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.5)
  br label %bb10

bb9:                                              ; preds = %bb6
  %call_350 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.6)
  br label %bb10

bb10:                                             ; preds = %bb9, %bb8
  %load_351 = load i64, ptr %alloca_count_316, align 8
  %iop_352 = add i64 %load_351, 1
  store i64 %iop_352, ptr %alloca_count_316, align 8
  br label %bb5
}

define i32 @main() {
bb0:
  %alloca_485 = alloca ptr, align 8
  %alloca_count_485 = alloca ptr, align 8
  %alloca_476 = alloca ptr, align 8
  %alloca_count_476 = alloca ptr, align 8
  %alloca_474 = alloca ptr, align 8
  %alloca_count_474 = alloca ptr, align 8
  %alloca_472 = alloca ptr, align 8
  %alloca_count_472 = alloca ptr, align 8
  %alloca_470 = alloca ptr, align 8
  %alloca_count_470 = alloca ptr, align 8
  %alloca_468 = alloca ptr, align 8
  %alloca_count_468 = alloca ptr, align 8
  %alloca_466 = alloca ptr, align 8
  %alloca_count_466 = alloca ptr, align 8
  %alloca_464 = alloca i1, align 1
  %alloca_count_464 = alloca i1, align 1
  %alloca_461 = alloca [8 x i64], align 8
  %alloca_count_461 = alloca [8 x i64], align 8
  %alloca_443 = alloca [8 x i64], align 8
  %alloca_count_443 = alloca [8 x i64], align 8
  %alloca_440 = alloca i64, align 8
  %alloca_count_440 = alloca i64, align 8
  %alloca_437 = alloca i64, align 8
  %alloca_count_437 = alloca i64, align 8
  %alloca_434 = alloca i64, align 8
  %alloca_count_434 = alloca i64, align 8
  %alloca_431 = alloca i64, align 8
  %alloca_count_431 = alloca i64, align 8
  %alloca_428 = alloca i64, align 8
  %alloca_count_428 = alloca i64, align 8
  %alloca_425 = alloca i64, align 8
  %alloca_count_425 = alloca i64, align 8
  %alloca_422 = alloca i64, align 8
  %alloca_count_422 = alloca i64, align 8
  %alloca_419 = alloca i64, align 8
  %alloca_count_419 = alloca i64, align 8
  %alloca_416 = alloca [8 x i64], align 8
  %alloca_count_416 = alloca [8 x i64], align 8
  %alloca_398 = alloca [8 x i64], align 8
  %alloca_count_398 = alloca [8 x i64], align 8
  %alloca_395 = alloca i64, align 8
  %alloca_count_395 = alloca i64, align 8
  %alloca_392 = alloca i64, align 8
  %alloca_count_392 = alloca i64, align 8
  %alloca_389 = alloca i64, align 8
  %alloca_count_389 = alloca i64, align 8
  %alloca_386 = alloca i64, align 8
  %alloca_count_386 = alloca i64, align 8
  %alloca_383 = alloca i64, align 8
  %alloca_count_383 = alloca i64, align 8
  %alloca_380 = alloca i64, align 8
  %alloca_count_380 = alloca i64, align 8
  %alloca_377 = alloca i64, align 8
  %alloca_count_377 = alloca i64, align 8
  %alloca_374 = alloca i64, align 8
  %alloca_count_374 = alloca i64, align 8
  %alloca_371 = alloca [15 x i64], align 8
  %alloca_count_371 = alloca [15 x i64], align 8
  %alloca_369 = alloca [15 x i64], align 8
  %alloca_count_369 = alloca [15 x i64], align 8
  %alloca_366 = alloca [15 x i64], align 8
  %alloca_count_366 = alloca [15 x i64], align 8
  %alloca_364 = alloca [15 x i64], align 8
  %alloca_count_364 = alloca [15 x i64], align 8
  %alloca_361 = alloca [8 x i64], align 8
  %alloca_count_361 = alloca [8 x i64], align 8
  %alloca_359 = alloca [8 x i64], align 8
  %alloca_count_359 = alloca [8 x i64], align 8
  %call_354 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.7)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_355 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.8)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_356 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.9)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_357 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.10)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_358 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  store [8 x i64] zeroinitializer, ptr %alloca_count_359, align 8
  %load_362 = load [8 x i64], ptr %alloca_count_359, align 8
  store [8 x i64] %load_362, ptr %alloca_count_361, align 8
  store [15 x i64] zeroinitializer, ptr %alloca_count_364, align 8
  %load_367 = load [15 x i64], ptr %alloca_count_364, align 8
  store [15 x i64] %load_367, ptr %alloca_count_366, align 8
  store [15 x i64] zeroinitializer, ptr %alloca_count_369, align 8
  %load_372 = load [15 x i64], ptr %alloca_count_369, align 8
  store [15 x i64] %load_372, ptr %alloca_count_371, align 8
  store i64 -1, ptr %alloca_count_374, align 8
  store i64 -1, ptr %alloca_count_377, align 8
  store i64 -1, ptr %alloca_count_380, align 8
  store i64 -1, ptr %alloca_count_383, align 8
  store i64 -1, ptr %alloca_count_386, align 8
  store i64 -1, ptr %alloca_count_389, align 8
  store i64 -1, ptr %alloca_count_392, align 8
  store i64 -1, ptr %alloca_count_395, align 8
  %load_399 = load i64, ptr %alloca_count_374, align 8
  %load_400 = load i64, ptr %alloca_count_377, align 8
  %load_401 = load i64, ptr %alloca_count_380, align 8
  %load_402 = load i64, ptr %alloca_count_383, align 8
  %load_403 = load i64, ptr %alloca_count_386, align 8
  %load_404 = load i64, ptr %alloca_count_389, align 8
  %load_405 = load i64, ptr %alloca_count_392, align 8
  %load_406 = load i64, ptr %alloca_count_395, align 8
  %insertvalue_407 = insertvalue [8 x i64] undef, i64 %load_399, 0
  %insertvalue_408 = insertvalue [8 x i64] %insertvalue_407, i64 %load_400, 1
  %insertvalue_409 = insertvalue [8 x i64] %insertvalue_408, i64 %load_401, 2
  %insertvalue_410 = insertvalue [8 x i64] %insertvalue_409, i64 %load_402, 3
  %insertvalue_411 = insertvalue [8 x i64] %insertvalue_410, i64 %load_403, 4
  %insertvalue_412 = insertvalue [8 x i64] %insertvalue_411, i64 %load_404, 5
  %insertvalue_413 = insertvalue [8 x i64] %insertvalue_412, i64 %load_405, 6
  %insertvalue_414 = insertvalue [8 x i64] %insertvalue_413, i64 %load_406, 7
  store [8 x i64] %insertvalue_414, ptr %alloca_count_398, align 8
  %load_417 = load [8 x i64], ptr %alloca_count_398, align 8
  store [8 x i64] %load_417, ptr %alloca_count_416, align 8
  store i64 -1, ptr %alloca_count_419, align 8
  store i64 -1, ptr %alloca_count_422, align 8
  store i64 -1, ptr %alloca_count_425, align 8
  store i64 -1, ptr %alloca_count_428, align 8
  store i64 -1, ptr %alloca_count_431, align 8
  store i64 -1, ptr %alloca_count_434, align 8
  store i64 -1, ptr %alloca_count_437, align 8
  store i64 -1, ptr %alloca_count_440, align 8
  %load_444 = load i64, ptr %alloca_count_419, align 8
  %load_445 = load i64, ptr %alloca_count_422, align 8
  %load_446 = load i64, ptr %alloca_count_425, align 8
  %load_447 = load i64, ptr %alloca_count_428, align 8
  %load_448 = load i64, ptr %alloca_count_431, align 8
  %load_449 = load i64, ptr %alloca_count_434, align 8
  %load_450 = load i64, ptr %alloca_count_437, align 8
  %load_451 = load i64, ptr %alloca_count_440, align 8
  %insertvalue_452 = insertvalue [8 x i64] undef, i64 %load_444, 0
  %insertvalue_453 = insertvalue [8 x i64] %insertvalue_452, i64 %load_445, 1
  %insertvalue_454 = insertvalue [8 x i64] %insertvalue_453, i64 %load_446, 2
  %insertvalue_455 = insertvalue [8 x i64] %insertvalue_454, i64 %load_447, 3
  %insertvalue_456 = insertvalue [8 x i64] %insertvalue_455, i64 %load_448, 4
  %insertvalue_457 = insertvalue [8 x i64] %insertvalue_456, i64 %load_449, 5
  %insertvalue_458 = insertvalue [8 x i64] %insertvalue_457, i64 %load_450, 6
  %insertvalue_459 = insertvalue [8 x i64] %insertvalue_458, i64 %load_451, 7
  store [8 x i64] %insertvalue_459, ptr %alloca_count_443, align 8
  %load_462 = load [8 x i64], ptr %alloca_count_443, align 8
  store [8 x i64] %load_462, ptr %alloca_count_461, align 8
  store i1 false, ptr %alloca_count_464, align 1
  store ptr %alloca_count_361, ptr %alloca_count_466, align 8
  store ptr %alloca_count_366, ptr %alloca_count_468, align 8
  store ptr %alloca_count_371, ptr %alloca_count_470, align 8
  store ptr %alloca_count_416, ptr %alloca_count_472, align 8
  store ptr %alloca_count_461, ptr %alloca_count_474, align 8
  store ptr %alloca_count_464, ptr %alloca_count_476, align 8
  %load_478 = load ptr, ptr %alloca_count_466, align 8
  %load_479 = load ptr, ptr %alloca_count_468, align 8
  %load_480 = load ptr, ptr %alloca_count_470, align 8
  %load_481 = load ptr, ptr %alloca_count_472, align 8
  %load_482 = load ptr, ptr %alloca_count_474, align 8
  %load_483 = load ptr, ptr %alloca_count_476, align 8
  %call_484 = call i64 @solve(i64 0, ptr %load_478, ptr %load_479, ptr %load_480, ptr %load_481, ptr %load_482, ptr %load_483)
  br label %bb6

bb6:                                              ; preds = %bb5
  store ptr %alloca_count_461, ptr %alloca_count_485, align 8
  %load_487 = load ptr, ptr %alloca_count_485, align 8
  call void @print_board(ptr %load_487)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_489 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.11, i64 %call_484)
  br label %bb8

bb8:                                              ; preds = %bb7
  ret i32 0
}

define internal void @TestCase__run(ptr %0) {
bb0:
  ret void
}

declare i32 @printf(ptr, ...)

declare i32 @__gxx_personality_v0(...)
