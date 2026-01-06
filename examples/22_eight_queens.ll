; ModuleID = '22_eight_queens'
source_filename = "22_eight_queens"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.22_eight_queens.0 = constant [17 x i8] c"First solution:\0A\00"
@.str.22_eight_queens.1 = constant [2 x i8] c"\0A\00"
@.str.22_eight_queens.2 = constant [3 x i8] c"Q \00"
@.str.22_eight_queens.3 = constant [3 x i8] c". \00"
@.str.22_eight_queens.4 = constant [35 x i8] c"\F0\9F\93\98 Tutorial: 22_eight_queens.fp\0A\00"
@.str.22_eight_queens.5 = constant [67 x i8] c"\F0\9F\A7\AD Focus: Classic 8-queens solver using recursive backtracking.\0A\00"
@.str.22_eight_queens.6 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.22_eight_queens.7 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.22_eight_queens.8 = constant [23 x i8] c"Total solutions: %lld\0A\00"

define internal i64 @solve(i64 %0, ptr %1, ptr %2, ptr %3, ptr %4, ptr %5, ptr %6) {
bb0:
  %alloca_212 = alloca i64, align 8
  %alloca_count_212 = alloca i64, align 8
  %alloca_202 = alloca i64, align 8
  %alloca_count_202 = alloca i64, align 8
  %alloca_192 = alloca i64, align 8
  %alloca_count_192 = alloca i64, align 8
  %alloca_182 = alloca i64, align 8
  %alloca_count_182 = alloca i64, align 8
  %alloca_170 = alloca i64, align 8
  %alloca_count_170 = alloca i64, align 8
  %alloca_160 = alloca i64, align 8
  %alloca_count_160 = alloca i64, align 8
  %alloca_150 = alloca i64, align 8
  %alloca_count_150 = alloca i64, align 8
  %alloca_140 = alloca i64, align 8
  %alloca_count_140 = alloca i64, align 8
  %alloca_130 = alloca i64, align 8
  %alloca_count_130 = alloca i64, align 8
  %alloca_121 = alloca i1, align 1
  %alloca_count_121 = alloca i1, align 1
  %alloca_111 = alloca i1, align 1
  %alloca_count_111 = alloca i1, align 1
  %alloca_108 = alloca i64, align 8
  %alloca_count_108 = alloca i64, align 8
  %alloca_103 = alloca i1, align 1
  %alloca_count_103 = alloca i1, align 1
  %alloca_93 = alloca i1, align 1
  %alloca_count_93 = alloca i1, align 1
  %alloca_90 = alloca i64, align 8
  %alloca_count_90 = alloca i64, align 8
  %alloca_80 = alloca i1, align 1
  %alloca_count_80 = alloca i1, align 1
  %alloca_77 = alloca i64, align 8
  %alloca_count_77 = alloca i64, align 8
  %alloca_74 = alloca i64, align 8
  %alloca_count_74 = alloca i64, align 8
  %alloca_70 = alloca i64, align 8
  %alloca_count_70 = alloca i64, align 8
  %alloca_66 = alloca i64, align 8
  %alloca_count_66 = alloca i64, align 8
  %alloca_63 = alloca i64, align 8
  %alloca_count_63 = alloca i64, align 8
  %alloca_59 = alloca i64, align 8
  %alloca_count_59 = alloca i64, align 8
  %alloca_39 = alloca i64, align 8
  %alloca_count_39 = alloca i64, align 8
  %alloca_36 = alloca i64, align 8
  %alloca_count_36 = alloca i64, align 8
  %alloca_31 = alloca i1, align 1
  %alloca_count_31 = alloca i1, align 1
  %alloca_24 = alloca i1, align 1
  %alloca_count_24 = alloca i1, align 1
  %alloca_16 = alloca i1, align 1
  %alloca_count_16 = alloca i1, align 1
  %alloca_12 = alloca i1, align 1
  %alloca_count_12 = alloca i1, align 1
  %alloca_11 = alloca i64, align 8
  %alloca_count_11 = alloca i64, align 8
  %alloca_10 = alloca i64, align 8
  %alloca_count_10 = alloca i64, align 8
  %alloca_8 = alloca ptr, align 8
  %alloca_count_8 = alloca ptr, align 8
  %alloca_7 = alloca i64, align 8
  %alloca_count_7 = alloca i64, align 8
  %alloca_6 = alloca i64, align 8
  %alloca_count_6 = alloca i64, align 8
  %alloca_4 = alloca ptr, align 8
  %alloca_count_4 = alloca ptr, align 8
  %alloca_2 = alloca ptr, align 8
  %alloca_count_2 = alloca ptr, align 8
  %alloca_0 = alloca ptr, align 8
  %alloca_count_0 = alloca ptr, align 8
  store ptr %3, ptr %alloca_count_0, align 8
  store ptr %4, ptr %alloca_count_2, align 8
  store ptr %2, ptr %alloca_count_4, align 8
  store ptr %1, ptr %alloca_count_8, align 8
  %icmp_13 = icmp eq i64 %0, 8
  store i1 %icmp_13, ptr %alloca_count_12, align 1
  %load_15 = load i1, ptr %alloca_count_12, align 1
  br i1 %load_15, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  %load_17 = load i1, ptr %6, align 1
  %zext = zext i1 %load_17 to i64
  %not_18 = xor i64 %zext, -1
  store i64 %not_18, ptr %alloca_count_16, align 1
  %load_20 = load i1, ptr %alloca_count_16, align 1
  br i1 %load_20, label %bb4, label %bb5

bb2:                                              ; preds = %bb0
  br label %bb3

bb4:                                              ; preds = %bb1
  store i64 0, ptr %alloca_count_6, align 8
  br label %bb7

bb5:                                              ; preds = %bb1
  br label %bb6

bb3:                                              ; preds = %bb10, %bb2
  store i64 0, ptr %alloca_count_7, align 8
  store i64 0, ptr %alloca_count_11, align 8
  br label %bb11

bb7:                                              ; preds = %bb8, %bb4
  %load_25 = load i64, ptr %alloca_count_6, align 8
  %icmp_26 = icmp slt i64 %load_25, 8
  store i1 %icmp_26, ptr %alloca_count_24, align 1
  %load_28 = load i1, ptr %alloca_count_24, align 1
  br i1 %load_28, label %bb8, label %bb9

bb6:                                              ; preds = %bb9, %bb5
  store i64 1, ptr %alloca_count_10, align 8
  %load_30 = load i64, ptr %alloca_count_10, align 8
  ret i64 %load_30

bb11:                                             ; preds = %bb16, %bb3
  %load_32 = load i64, ptr %alloca_count_11, align 8
  %icmp_33 = icmp slt i64 %load_32, 8
  store i1 %icmp_33, ptr %alloca_count_31, align 1
  %load_35 = load i1, ptr %alloca_count_31, align 1
  br i1 %load_35, label %bb12, label %bb13

bb8:                                              ; preds = %bb7
  %load_37 = load i64, ptr %alloca_count_6, align 8
  store i64 %load_37, ptr %alloca_count_36, align 8
  %load_40 = load i64, ptr %alloca_count_6, align 8
  store i64 %load_40, ptr %alloca_count_39, align 8
  %load_42 = load i64, ptr %alloca_count_36, align 8
  %iop_43 = mul i64 %load_42, 8
  %gep_45 = getelementptr inbounds i8, ptr %5, i64 %iop_43
  %load_47 = load ptr, ptr %alloca_count_2, align 8
  %load_48 = load i64, ptr %alloca_count_39, align 8
  %iop_49 = mul i64 %load_48, 8
  %gep_51 = getelementptr inbounds i8, ptr %load_47, i64 %iop_49
  %load_53 = load i64, ptr %gep_51, align 8
  store i64 %load_53, ptr %gep_45, align 8
  %load_55 = load i64, ptr %alloca_count_6, align 8
  %iop_56 = add i64 %load_55, 1
  store i64 %iop_56, ptr %alloca_count_6, align 8
  br label %bb7

bb9:                                              ; preds = %bb7
  store i1 true, ptr %6, align 1
  br label %bb6

bb12:                                             ; preds = %bb11
  %load_60 = load i64, ptr %alloca_count_11, align 8
  %iop_61 = add i64 %0, %load_60
  store i64 %iop_61, ptr %alloca_count_59, align 8
  %load_64 = load i64, ptr %alloca_count_59, align 8
  store i64 %load_64, ptr %alloca_count_63, align 8
  %load_67 = load i64, ptr %alloca_count_11, align 8
  %iop_68 = sub i64 %0, %load_67
  store i64 %iop_68, ptr %alloca_count_66, align 8
  %load_71 = load i64, ptr %alloca_count_66, align 8
  %iop_72 = add i64 %load_71, 7
  store i64 %iop_72, ptr %alloca_count_70, align 8
  %load_75 = load i64, ptr %alloca_count_70, align 8
  store i64 %load_75, ptr %alloca_count_74, align 8
  %load_78 = load i64, ptr %alloca_count_11, align 8
  store i64 %load_78, ptr %alloca_count_77, align 8
  %load_81 = load ptr, ptr %alloca_count_8, align 8
  %load_82 = load i64, ptr %alloca_count_77, align 8
  %iop_83 = mul i64 %load_82, 8
  %gep_85 = getelementptr inbounds i8, ptr %load_81, i64 %iop_83
  %load_87 = load i64, ptr %gep_85, align 8
  %icmp_88 = icmp eq i64 %load_87, 0
  store i1 %icmp_88, ptr %alloca_count_80, align 1
  %load_91 = load i64, ptr %alloca_count_63, align 8
  store i64 %load_91, ptr %alloca_count_90, align 8
  %load_94 = load ptr, ptr %alloca_count_4, align 8
  %load_95 = load i64, ptr %alloca_count_90, align 8
  %iop_96 = mul i64 %load_95, 8
  %gep_98 = getelementptr inbounds i8, ptr %load_94, i64 %iop_96
  %load_100 = load i64, ptr %gep_98, align 8
  %icmp_101 = icmp eq i64 %load_100, 0
  store i1 %icmp_101, ptr %alloca_count_93, align 1
  %load_104 = load i1, ptr %alloca_count_80, align 1
  %load_105 = load i1, ptr %alloca_count_93, align 1
  %zext1 = zext i1 %load_104 to i64
  %zext2 = zext i1 %load_105 to i64
  %iop_106 = and i64 %zext1, %zext2
  store i64 %iop_106, ptr %alloca_count_103, align 1
  %load_109 = load i64, ptr %alloca_count_74, align 8
  store i64 %load_109, ptr %alloca_count_108, align 8
  %load_112 = load ptr, ptr %alloca_count_0, align 8
  %load_113 = load i64, ptr %alloca_count_108, align 8
  %iop_114 = mul i64 %load_113, 8
  %gep_116 = getelementptr inbounds i8, ptr %load_112, i64 %iop_114
  %load_118 = load i64, ptr %gep_116, align 8
  %icmp_119 = icmp eq i64 %load_118, 0
  store i1 %icmp_119, ptr %alloca_count_111, align 1
  %load_122 = load i1, ptr %alloca_count_103, align 1
  %load_123 = load i1, ptr %alloca_count_111, align 1
  %zext3 = zext i1 %load_122 to i64
  %zext4 = zext i1 %load_123 to i64
  %iop_124 = and i64 %zext3, %zext4
  store i64 %iop_124, ptr %alloca_count_121, align 1
  %load_126 = load i1, ptr %alloca_count_121, align 1
  br i1 %load_126, label %bb14, label %bb15

bb13:                                             ; preds = %bb11
  %load_127 = load i64, ptr %alloca_count_7, align 8
  store i64 %load_127, ptr %alloca_count_10, align 8
  %load_129 = load i64, ptr %alloca_count_10, align 8
  ret i64 %load_129

bb14:                                             ; preds = %bb12
  %load_131 = load i64, ptr %alloca_count_11, align 8
  store i64 %load_131, ptr %alloca_count_130, align 8
  %load_133 = load ptr, ptr %alloca_count_8, align 8
  %load_134 = load i64, ptr %alloca_count_130, align 8
  %iop_135 = mul i64 %load_134, 8
  %gep_137 = getelementptr inbounds i8, ptr %load_133, i64 %iop_135
  store i64 1, ptr %gep_137, align 8
  %load_141 = load i64, ptr %alloca_count_63, align 8
  store i64 %load_141, ptr %alloca_count_140, align 8
  %load_143 = load ptr, ptr %alloca_count_4, align 8
  %load_144 = load i64, ptr %alloca_count_140, align 8
  %iop_145 = mul i64 %load_144, 8
  %gep_147 = getelementptr inbounds i8, ptr %load_143, i64 %iop_145
  store i64 1, ptr %gep_147, align 8
  %load_151 = load i64, ptr %alloca_count_74, align 8
  store i64 %load_151, ptr %alloca_count_150, align 8
  %load_153 = load ptr, ptr %alloca_count_0, align 8
  %load_154 = load i64, ptr %alloca_count_150, align 8
  %iop_155 = mul i64 %load_154, 8
  %gep_157 = getelementptr inbounds i8, ptr %load_153, i64 %iop_155
  store i64 1, ptr %gep_157, align 8
  store i64 %0, ptr %alloca_count_160, align 8
  %load_162 = load ptr, ptr %alloca_count_2, align 8
  %load_163 = load i64, ptr %alloca_count_160, align 8
  %iop_164 = mul i64 %load_163, 8
  %gep_166 = getelementptr inbounds i8, ptr %load_162, i64 %iop_164
  %load_168 = load i64, ptr %alloca_count_11, align 8
  store i64 %load_168, ptr %gep_166, align 8
  %iop_171 = add i64 %0, 1
  store i64 %iop_171, ptr %alloca_count_170, align 8
  %load_173 = load i64, ptr %alloca_count_170, align 8
  %load_174 = load ptr, ptr %alloca_count_8, align 8
  %load_175 = load ptr, ptr %alloca_count_4, align 8
  %load_176 = load ptr, ptr %alloca_count_0, align 8
  %load_177 = load ptr, ptr %alloca_count_2, align 8
  %call_178 = call i64 @solve(i64 %load_173, ptr %load_174, ptr %load_175, ptr %load_176, ptr %load_177, ptr %5, ptr %6)
  br label %bb17

bb15:                                             ; preds = %bb12
  br label %bb16

bb17:                                             ; preds = %bb14
  %load_179 = load i64, ptr %alloca_count_7, align 8
  %iop_180 = add i64 %load_179, %call_178
  store i64 %iop_180, ptr %alloca_count_7, align 8
  %load_183 = load i64, ptr %alloca_count_11, align 8
  store i64 %load_183, ptr %alloca_count_182, align 8
  %load_185 = load ptr, ptr %alloca_count_8, align 8
  %load_186 = load i64, ptr %alloca_count_182, align 8
  %iop_187 = mul i64 %load_186, 8
  %gep_189 = getelementptr inbounds i8, ptr %load_185, i64 %iop_187
  store i64 0, ptr %gep_189, align 8
  %load_193 = load i64, ptr %alloca_count_63, align 8
  store i64 %load_193, ptr %alloca_count_192, align 8
  %load_195 = load ptr, ptr %alloca_count_4, align 8
  %load_196 = load i64, ptr %alloca_count_192, align 8
  %iop_197 = mul i64 %load_196, 8
  %gep_199 = getelementptr inbounds i8, ptr %load_195, i64 %iop_197
  store i64 0, ptr %gep_199, align 8
  %load_203 = load i64, ptr %alloca_count_74, align 8
  store i64 %load_203, ptr %alloca_count_202, align 8
  %load_205 = load ptr, ptr %alloca_count_0, align 8
  %load_206 = load i64, ptr %alloca_count_202, align 8
  %iop_207 = mul i64 %load_206, 8
  %gep_209 = getelementptr inbounds i8, ptr %load_205, i64 %iop_207
  store i64 0, ptr %gep_209, align 8
  store i64 %0, ptr %alloca_count_212, align 8
  %load_214 = load ptr, ptr %alloca_count_2, align 8
  %load_215 = load i64, ptr %alloca_count_212, align 8
  %iop_216 = mul i64 %load_215, 8
  %gep_218 = getelementptr inbounds i8, ptr %load_214, i64 %iop_216
  store i64 -1, ptr %gep_218, align 8
  br label %bb16

bb16:                                             ; preds = %bb17, %bb15
  %load_222 = load i64, ptr %alloca_count_11, align 8
  %iop_223 = add i64 %load_222, 1
  store i64 %iop_223, ptr %alloca_count_11, align 8
  br label %bb11

bb10:                                             ; No predecessors!
  br label %bb3
}

define internal void @print_board(ptr %0) {
bb0:
  %alloca_243 = alloca i1, align 1
  %alloca_count_243 = alloca i1, align 1
  %alloca_240 = alloca i64, align 8
  %alloca_count_240 = alloca i64, align 8
  %alloca_235 = alloca i1, align 1
  %alloca_count_235 = alloca i1, align 1
  %alloca_229 = alloca i1, align 1
  %alloca_count_229 = alloca i1, align 1
  %alloca_226 = alloca i64, align 8
  %alloca_count_226 = alloca i64, align 8
  %alloca_225 = alloca i64, align 8
  %alloca_count_225 = alloca i64, align 8
  %call_227 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  store i64 0, ptr %alloca_count_225, align 8
  br label %bb2

bb2:                                              ; preds = %bb7, %bb1
  %load_230 = load i64, ptr %alloca_count_225, align 8
  %icmp_231 = icmp slt i64 %load_230, 8
  store i1 %icmp_231, ptr %alloca_count_229, align 1
  %load_233 = load i1, ptr %alloca_count_229, align 1
  br i1 %load_233, label %bb3, label %bb4

bb3:                                              ; preds = %bb2
  store i64 0, ptr %alloca_count_226, align 8
  br label %bb5

bb4:                                              ; preds = %bb2
  ret void

bb5:                                              ; preds = %bb10, %bb3
  %load_236 = load i64, ptr %alloca_count_226, align 8
  %icmp_237 = icmp slt i64 %load_236, 8
  store i1 %icmp_237, ptr %alloca_count_235, align 1
  %load_239 = load i1, ptr %alloca_count_235, align 1
  br i1 %load_239, label %bb6, label %bb7

bb6:                                              ; preds = %bb5
  %load_241 = load i64, ptr %alloca_count_225, align 8
  store i64 %load_241, ptr %alloca_count_240, align 8
  %load_244 = load i64, ptr %alloca_count_240, align 8
  %iop_245 = mul i64 %load_244, 8
  %gep_247 = getelementptr inbounds i8, ptr %0, i64 %iop_245
  %load_249 = load i64, ptr %gep_247, align 8
  %load_250 = load i64, ptr %alloca_count_226, align 8
  %icmp_251 = icmp eq i64 %load_249, %load_250
  store i1 %icmp_251, ptr %alloca_count_243, align 1
  %load_253 = load i1, ptr %alloca_count_243, align 1
  br i1 %load_253, label %bb8, label %bb9

bb7:                                              ; preds = %bb5
  %call_254 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.1)
  %load_255 = load i64, ptr %alloca_count_225, align 8
  %iop_256 = add i64 %load_255, 1
  store i64 %iop_256, ptr %alloca_count_225, align 8
  br label %bb2

bb8:                                              ; preds = %bb6
  %call_258 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.2)
  br label %bb10

bb9:                                              ; preds = %bb6
  %call_259 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.3)
  br label %bb10

bb10:                                             ; preds = %bb9, %bb8
  %load_260 = load i64, ptr %alloca_count_226, align 8
  %iop_261 = add i64 %load_260, 1
  store i64 %iop_261, ptr %alloca_count_226, align 8
  br label %bb5
}

declare i32 @printf(ptr, ...)

define i32 @main() {
bb0:
  %alloca_394 = alloca ptr, align 8
  %alloca_count_394 = alloca ptr, align 8
  %alloca_385 = alloca ptr, align 8
  %alloca_count_385 = alloca ptr, align 8
  %alloca_383 = alloca ptr, align 8
  %alloca_count_383 = alloca ptr, align 8
  %alloca_381 = alloca ptr, align 8
  %alloca_count_381 = alloca ptr, align 8
  %alloca_379 = alloca ptr, align 8
  %alloca_count_379 = alloca ptr, align 8
  %alloca_377 = alloca ptr, align 8
  %alloca_count_377 = alloca ptr, align 8
  %alloca_375 = alloca ptr, align 8
  %alloca_count_375 = alloca ptr, align 8
  %alloca_373 = alloca i1, align 1
  %alloca_count_373 = alloca i1, align 1
  %alloca_370 = alloca [8 x i64], align 8
  %alloca_count_370 = alloca [8 x i64], align 8
  %alloca_352 = alloca [8 x i64], align 8
  %alloca_count_352 = alloca [8 x i64], align 8
  %alloca_349 = alloca i64, align 8
  %alloca_count_349 = alloca i64, align 8
  %alloca_346 = alloca i64, align 8
  %alloca_count_346 = alloca i64, align 8
  %alloca_343 = alloca i64, align 8
  %alloca_count_343 = alloca i64, align 8
  %alloca_340 = alloca i64, align 8
  %alloca_count_340 = alloca i64, align 8
  %alloca_337 = alloca i64, align 8
  %alloca_count_337 = alloca i64, align 8
  %alloca_334 = alloca i64, align 8
  %alloca_count_334 = alloca i64, align 8
  %alloca_331 = alloca i64, align 8
  %alloca_count_331 = alloca i64, align 8
  %alloca_328 = alloca i64, align 8
  %alloca_count_328 = alloca i64, align 8
  %alloca_325 = alloca [8 x i64], align 8
  %alloca_count_325 = alloca [8 x i64], align 8
  %alloca_307 = alloca [8 x i64], align 8
  %alloca_count_307 = alloca [8 x i64], align 8
  %alloca_304 = alloca i64, align 8
  %alloca_count_304 = alloca i64, align 8
  %alloca_301 = alloca i64, align 8
  %alloca_count_301 = alloca i64, align 8
  %alloca_298 = alloca i64, align 8
  %alloca_count_298 = alloca i64, align 8
  %alloca_295 = alloca i64, align 8
  %alloca_count_295 = alloca i64, align 8
  %alloca_292 = alloca i64, align 8
  %alloca_count_292 = alloca i64, align 8
  %alloca_289 = alloca i64, align 8
  %alloca_count_289 = alloca i64, align 8
  %alloca_286 = alloca i64, align 8
  %alloca_count_286 = alloca i64, align 8
  %alloca_283 = alloca i64, align 8
  %alloca_count_283 = alloca i64, align 8
  %alloca_280 = alloca [15 x i64], align 8
  %alloca_count_280 = alloca [15 x i64], align 8
  %alloca_278 = alloca [15 x i64], align 8
  %alloca_count_278 = alloca [15 x i64], align 8
  %alloca_275 = alloca [15 x i64], align 8
  %alloca_count_275 = alloca [15 x i64], align 8
  %alloca_273 = alloca [15 x i64], align 8
  %alloca_count_273 = alloca [15 x i64], align 8
  %alloca_270 = alloca [8 x i64], align 8
  %alloca_count_270 = alloca [8 x i64], align 8
  %alloca_268 = alloca [8 x i64], align 8
  %alloca_count_268 = alloca [8 x i64], align 8
  %call_263 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.4)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_264 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.5)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_265 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.6)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_266 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.7)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_267 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.1)
  br label %bb5

bb5:                                              ; preds = %bb4
  store [8 x i64] zeroinitializer, ptr %alloca_count_268, align 8
  %load_271 = load [8 x i64], ptr %alloca_count_268, align 8
  store [8 x i64] %load_271, ptr %alloca_count_270, align 8
  store [15 x i64] zeroinitializer, ptr %alloca_count_273, align 8
  %load_276 = load [15 x i64], ptr %alloca_count_273, align 8
  store [15 x i64] %load_276, ptr %alloca_count_275, align 8
  store [15 x i64] zeroinitializer, ptr %alloca_count_278, align 8
  %load_281 = load [15 x i64], ptr %alloca_count_278, align 8
  store [15 x i64] %load_281, ptr %alloca_count_280, align 8
  store i64 -1, ptr %alloca_count_283, align 8
  store i64 -1, ptr %alloca_count_286, align 8
  store i64 -1, ptr %alloca_count_289, align 8
  store i64 -1, ptr %alloca_count_292, align 8
  store i64 -1, ptr %alloca_count_295, align 8
  store i64 -1, ptr %alloca_count_298, align 8
  store i64 -1, ptr %alloca_count_301, align 8
  store i64 -1, ptr %alloca_count_304, align 8
  %load_308 = load i64, ptr %alloca_count_283, align 8
  %load_309 = load i64, ptr %alloca_count_286, align 8
  %load_310 = load i64, ptr %alloca_count_289, align 8
  %load_311 = load i64, ptr %alloca_count_292, align 8
  %load_312 = load i64, ptr %alloca_count_295, align 8
  %load_313 = load i64, ptr %alloca_count_298, align 8
  %load_314 = load i64, ptr %alloca_count_301, align 8
  %load_315 = load i64, ptr %alloca_count_304, align 8
  %insertvalue_316 = insertvalue [8 x i64] undef, i64 %load_308, 0
  %insertvalue_317 = insertvalue [8 x i64] %insertvalue_316, i64 %load_309, 1
  %insertvalue_318 = insertvalue [8 x i64] %insertvalue_317, i64 %load_310, 2
  %insertvalue_319 = insertvalue [8 x i64] %insertvalue_318, i64 %load_311, 3
  %insertvalue_320 = insertvalue [8 x i64] %insertvalue_319, i64 %load_312, 4
  %insertvalue_321 = insertvalue [8 x i64] %insertvalue_320, i64 %load_313, 5
  %insertvalue_322 = insertvalue [8 x i64] %insertvalue_321, i64 %load_314, 6
  %insertvalue_323 = insertvalue [8 x i64] %insertvalue_322, i64 %load_315, 7
  store [8 x i64] %insertvalue_323, ptr %alloca_count_307, align 8
  %load_326 = load [8 x i64], ptr %alloca_count_307, align 8
  store [8 x i64] %load_326, ptr %alloca_count_325, align 8
  store i64 -1, ptr %alloca_count_328, align 8
  store i64 -1, ptr %alloca_count_331, align 8
  store i64 -1, ptr %alloca_count_334, align 8
  store i64 -1, ptr %alloca_count_337, align 8
  store i64 -1, ptr %alloca_count_340, align 8
  store i64 -1, ptr %alloca_count_343, align 8
  store i64 -1, ptr %alloca_count_346, align 8
  store i64 -1, ptr %alloca_count_349, align 8
  %load_353 = load i64, ptr %alloca_count_328, align 8
  %load_354 = load i64, ptr %alloca_count_331, align 8
  %load_355 = load i64, ptr %alloca_count_334, align 8
  %load_356 = load i64, ptr %alloca_count_337, align 8
  %load_357 = load i64, ptr %alloca_count_340, align 8
  %load_358 = load i64, ptr %alloca_count_343, align 8
  %load_359 = load i64, ptr %alloca_count_346, align 8
  %load_360 = load i64, ptr %alloca_count_349, align 8
  %insertvalue_361 = insertvalue [8 x i64] undef, i64 %load_353, 0
  %insertvalue_362 = insertvalue [8 x i64] %insertvalue_361, i64 %load_354, 1
  %insertvalue_363 = insertvalue [8 x i64] %insertvalue_362, i64 %load_355, 2
  %insertvalue_364 = insertvalue [8 x i64] %insertvalue_363, i64 %load_356, 3
  %insertvalue_365 = insertvalue [8 x i64] %insertvalue_364, i64 %load_357, 4
  %insertvalue_366 = insertvalue [8 x i64] %insertvalue_365, i64 %load_358, 5
  %insertvalue_367 = insertvalue [8 x i64] %insertvalue_366, i64 %load_359, 6
  %insertvalue_368 = insertvalue [8 x i64] %insertvalue_367, i64 %load_360, 7
  store [8 x i64] %insertvalue_368, ptr %alloca_count_352, align 8
  %load_371 = load [8 x i64], ptr %alloca_count_352, align 8
  store [8 x i64] %load_371, ptr %alloca_count_370, align 8
  store i1 false, ptr %alloca_count_373, align 1
  store ptr %alloca_count_270, ptr %alloca_count_375, align 8
  store ptr %alloca_count_275, ptr %alloca_count_377, align 8
  store ptr %alloca_count_280, ptr %alloca_count_379, align 8
  store ptr %alloca_count_325, ptr %alloca_count_381, align 8
  store ptr %alloca_count_370, ptr %alloca_count_383, align 8
  store ptr %alloca_count_373, ptr %alloca_count_385, align 8
  %load_387 = load ptr, ptr %alloca_count_375, align 8
  %load_388 = load ptr, ptr %alloca_count_377, align 8
  %load_389 = load ptr, ptr %alloca_count_379, align 8
  %load_390 = load ptr, ptr %alloca_count_381, align 8
  %load_391 = load ptr, ptr %alloca_count_383, align 8
  %load_392 = load ptr, ptr %alloca_count_385, align 8
  %call_393 = call i64 @solve(i64 0, ptr %load_387, ptr %load_388, ptr %load_389, ptr %load_390, ptr %load_391, ptr %load_392)
  br label %bb6

bb6:                                              ; preds = %bb5
  store ptr %alloca_count_370, ptr %alloca_count_394, align 8
  %load_396 = load ptr, ptr %alloca_count_394, align 8
  call void @print_board(ptr %load_396)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_398 = call i32 (ptr, ...) @printf(ptr @.str.22_eight_queens.8, i64 %call_393)
  br label %bb8

bb8:                                              ; preds = %bb7
  ret i32 0
}
