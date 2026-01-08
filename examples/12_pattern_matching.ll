; ModuleID = '12_pattern_matching'
source_filename = "12_pattern_matching"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_5 = internal constant [0 x { { ptr, i64 }, ptr }] zeroinitializer, align 8
@.str.12_pattern_matching.0 = constant [51 x i8] c"test result: %lld passed; %lld failed; %lld total\0A\00"
@.str.12_pattern_matching.1 = constant [13 x i8] c"  %s ... ok\0A\00"
@.str.12_pattern_matching.2 = constant [17 x i8] c"  %s ... FAILED\0A\00"
@.str.12_pattern_matching.3 = constant [4 x i8] c"red\00"
@.str.12_pattern_matching.4 = constant [6 x i8] c"green\00"
@.str.12_pattern_matching.5 = constant [8 x i8] c"red rgb\00"
@.str.12_pattern_matching.6 = constant [11 x i8] c"custom rgb\00"
@.str.12_pattern_matching.7 = constant [5 x i8] c"zero\00"
@.str.12_pattern_matching.8 = constant [9 x i8] c"negative\00"
@.str.12_pattern_matching.9 = constant [5 x i8] c"even\00"
@.str.12_pattern_matching.10 = constant [4 x i8] c"odd\00"
@.str.12_pattern_matching.11 = constant [39 x i8] c"\F0\9F\93\98 Tutorial: 12_pattern_matching.fp\0A\00"
@.str.12_pattern_matching.12 = constant [79 x i8] c"\F0\9F\A7\AD Focus: Pattern matching: match expressions with guards and destructuring\0A\00"
@.str.12_pattern_matching.13 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.12_pattern_matching.14 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.12_pattern_matching.15 = constant [2 x i8] c"\0A\00"
@.str.12_pattern_matching.16 = constant [20 x i8] c"describe(red) = %s\0A\00"
@.str.12_pattern_matching.17 = constant [20 x i8] c"describe(rgb) = %s\0A\00"
@.str.12_pattern_matching.18 = constant [19 x i8] c"classify(-5) = %s\0A\00"
@.str.12_pattern_matching.19 = constant [18 x i8] c"classify(0) = %s\0A\00"
@.str.12_pattern_matching.20 = constant [18 x i8] c"classify(4) = %s\0A\00"
@.str.12_pattern_matching.21 = constant [18 x i8] c"classify(7) = %s\0A\00"
@.str.12_pattern_matching.22 = constant [31 x i8] c"unwrap_or(Some(42), 0) = %lld\0A\00"
@.str.12_pattern_matching.23 = constant [28 x i8] c"unwrap_or(None, 99) = %lld\0A\00"
@.str.12_pattern_matching.24 = constant [8 x i8] c"0x%06X\0A\00"

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
  %alloca_10 = alloca i64, align 8
  %alloca_count_10 = alloca i64, align 8
  %alloca_9 = alloca i64, align 8
  %alloca_count_9 = alloca i64, align 8
  %alloca_8 = alloca i1, align 1
  %alloca_count_8 = alloca i1, align 1
  %alloca_7 = alloca { i64, i64, i64 }, align 8
  %alloca_count_7 = alloca { i64, i64, i64 }, align 8
  %alloca_6 = alloca i64, align 8
  %alloca_count_6 = alloca i64, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_11, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_13, align 8
  %load_16 = load [0 x { { ptr, i64 }, ptr }], ptr %alloca_count_13, align 8
  store [0 x { { ptr, i64 }, ptr }] %load_16, ptr %alloca_count_15, align 8
  store i64 0, ptr %alloca_count_10, align 8
  store i64 0, ptr %alloca_count_6, align 8
  store i64 0, ptr %alloca_count_9, align 8
  br label %bb1

bb1:                                              ; preds = %bb9, %bb0
  store i64 0, ptr %alloca_count_21, align 8
  %load_24 = load i64, ptr %alloca_count_9, align 8
  %load_25 = load i64, ptr %alloca_count_21, align 8
  %icmp_26 = icmp slt i64 %load_24, %load_25
  store i1 %icmp_26, ptr %alloca_count_23, align 1
  %load_28 = load i1, ptr %alloca_count_23, align 1
  br i1 %load_28, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_30 = load i64, ptr %alloca_count_9, align 8
  store i64 %load_30, ptr %alloca_count_29, align 8
  %load_33 = load i64, ptr %alloca_count_9, align 8
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
  %load_46 = load i64, ptr %alloca_count_10, align 8
  %load_47 = load i64, ptr %alloca_count_6, align 8
  %iop_48 = add i64 %load_46, %load_47
  store i64 %iop_48, ptr %alloca_count_45, align 8
  %load_51 = load i64, ptr %alloca_count_45, align 8
  store i64 %load_51, ptr %alloca_count_50, align 8
  %load_53 = load i64, ptr %alloca_count_10, align 8
  %load_54 = load i64, ptr %alloca_count_6, align 8
  %load_55 = load i64, ptr %alloca_count_50, align 8
  %call_56 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.0, i64 %load_53, i64 %load_54, i64 %load_55)
  br label %bb12

bb4:                                              ; preds = %bb2
  store i1 true, ptr %alloca_count_8, align 1
  br label %bb6

bb5:                                              ; preds = %bb2
  %landingpad = landingpad { ptr, i32 }
          catch ptr null
  store i1 false, ptr %alloca_count_8, align 1
  br label %bb6

bb12:                                             ; preds = %bb3
  %load_60 = load i64, ptr %alloca_count_50, align 8
  %load_61 = load i64, ptr %alloca_count_10, align 8
  %load_62 = load i64, ptr %alloca_count_6, align 8
  %insertvalue_63 = insertvalue { i64, i64, i64 } undef, i64 %load_60, 0
  %insertvalue_64 = insertvalue { i64, i64, i64 } %insertvalue_63, i64 %load_61, 1
  %insertvalue_65 = insertvalue { i64, i64, i64 } %insertvalue_64, i64 %load_62, 2
  store { i64, i64, i64 } %insertvalue_65, ptr %alloca_count_7, align 8
  %load_67 = load { i64, i64, i64 }, ptr %alloca_count_7, align 8
  ret { i64, i64, i64 } %load_67

bb6:                                              ; preds = %bb5, %bb4
  %load_69 = load i1, ptr %alloca_count_8, align 1
  store i1 %load_69, ptr %alloca_count_68, align 1
  %load_71 = load i1, ptr %alloca_count_68, align 1
  br i1 %load_71, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_72 = load i64, ptr %alloca_count_10, align 8
  %iop_73 = add i64 %load_72, 1
  store i64 %iop_73, ptr %alloca_count_10, align 8
  %load_76 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_77 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.1, { ptr, i64 } %load_76)
  br label %bb10

bb8:                                              ; preds = %bb6
  %load_78 = load i64, ptr %alloca_count_6, align 8
  %iop_79 = add i64 %load_78, 1
  store i64 %iop_79, ptr %alloca_count_6, align 8
  %load_82 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_83 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.2, { ptr, i64 } %load_82)
  br label %bb11

bb10:                                             ; preds = %bb7
  br label %bb9

bb11:                                             ; preds = %bb8
  br label %bb9

bb9:                                              ; preds = %bb11, %bb10
  %load_84 = load i64, ptr %alloca_count_9, align 8
  %iop_85 = add i64 %load_84, 1
  store i64 %iop_85, ptr %alloca_count_9, align 8
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

define internal ptr @describe(ptr %0) {
bb0:
  %alloca_140 = alloca i8, align 1
  %alloca_count_140 = alloca i8, align 1
  %alloca_133 = alloca i8, align 1
  %alloca_count_133 = alloca i8, align 1
  %alloca_126 = alloca i8, align 1
  %alloca_count_126 = alloca i8, align 1
  %alloca_119 = alloca i1, align 1
  %alloca_count_119 = alloca i1, align 1
  %alloca_111 = alloca i1, align 1
  %alloca_count_111 = alloca i1, align 1
  %alloca_102 = alloca i1, align 1
  %alloca_count_102 = alloca i1, align 1
  %alloca_94 = alloca i1, align 1
  %alloca_count_94 = alloca i1, align 1
  %alloca_92 = alloca ptr, align 8
  %alloca_count_92 = alloca ptr, align 8
  %alloca_91 = alloca ptr, align 8
  %alloca_count_91 = alloca ptr, align 8
  store ptr %0, ptr %alloca_count_92, align 8
  %load_95 = load ptr, ptr %alloca_count_92, align 8
  %load_97 = load i64, ptr %load_95, align 8
  %icmp_98 = icmp eq i64 %load_97, 0
  store i1 %icmp_98, ptr %alloca_count_94, align 1
  %load_100 = load i1, ptr %alloca_count_94, align 1
  br i1 %load_100, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  store ptr @.str.12_pattern_matching.3, ptr %alloca_count_91, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  %load_103 = load ptr, ptr %alloca_count_92, align 8
  %load_105 = load i64, ptr %load_103, align 8
  %icmp_106 = icmp eq i64 %load_105, 1
  store i1 %icmp_106, ptr %alloca_count_102, align 1
  %load_108 = load i1, ptr %alloca_count_102, align 1
  br i1 %load_108, label %bb4, label %bb5

bb1:                                              ; preds = %bb9, %bb8, %bb6, %bb4, %bb2
  %load_109 = load ptr, ptr %alloca_count_91, align 8
  ret ptr %load_109

bb4:                                              ; preds = %bb3
  store ptr @.str.12_pattern_matching.4, ptr %alloca_count_91, align 8
  br label %bb1

bb5:                                              ; preds = %bb3
  %load_112 = load ptr, ptr %alloca_count_92, align 8
  %load_114 = load i64, ptr %load_112, align 8
  %icmp_115 = icmp eq i64 %load_114, 2
  store i1 %icmp_115, ptr %alloca_count_111, align 1
  %load_117 = load i1, ptr %alloca_count_111, align 1
  br i1 %load_117, label %bb6, label %bb7

bb6:                                              ; preds = %bb5
  store ptr @.str.12_pattern_matching.5, ptr %alloca_count_91, align 8
  br label %bb1

bb7:                                              ; preds = %bb5
  %load_120 = load ptr, ptr %alloca_count_92, align 8
  %load_122 = load i64, ptr %load_120, align 8
  %icmp_123 = icmp eq i64 %load_122, 2
  store i1 %icmp_123, ptr %alloca_count_119, align 1
  %load_125 = load i1, ptr %alloca_count_119, align 1
  br i1 %load_125, label %bb8, label %bb9

bb8:                                              ; preds = %bb7
  %load_127 = load ptr, ptr %alloca_count_92, align 8
  %gep_129 = getelementptr inbounds i8, ptr %load_127, i64 8
  %load_131 = load i8, ptr %gep_129, align 1
  store i8 %load_131, ptr %alloca_count_126, align 1
  %load_134 = load ptr, ptr %alloca_count_92, align 8
  %gep_136 = getelementptr inbounds i8, ptr %load_134, i64 9
  %load_138 = load i8, ptr %gep_136, align 1
  store i8 %load_138, ptr %alloca_count_133, align 1
  %load_141 = load ptr, ptr %alloca_count_92, align 8
  %gep_143 = getelementptr inbounds i8, ptr %load_141, i64 10
  %load_145 = load i8, ptr %gep_143, align 1
  store i8 %load_145, ptr %alloca_count_140, align 1
  store ptr @.str.12_pattern_matching.6, ptr %alloca_count_91, align 8
  br label %bb1

bb9:                                              ; preds = %bb7
  br label %bb1
}

define internal ptr @classify(i64 %0) {
bb0:
  %alloca_174 = alloca i1, align 1
  %alloca_count_174 = alloca i1, align 1
  %alloca_170 = alloca i64, align 8
  %alloca_count_170 = alloca i64, align 8
  %alloca_167 = alloca i64, align 8
  %alloca_count_167 = alloca i64, align 8
  %alloca_161 = alloca i1, align 1
  %alloca_count_161 = alloca i1, align 1
  %alloca_158 = alloca i64, align 8
  %alloca_count_158 = alloca i64, align 8
  %alloca_151 = alloca i1, align 1
  %alloca_count_151 = alloca i1, align 1
  %alloca_149 = alloca i64, align 8
  %alloca_count_149 = alloca i64, align 8
  %alloca_148 = alloca ptr, align 8
  %alloca_count_148 = alloca ptr, align 8
  store i64 %0, ptr %alloca_count_149, align 8
  %load_152 = load i64, ptr %alloca_count_149, align 8
  %icmp_153 = icmp eq i64 %load_152, 0
  store i1 %icmp_153, ptr %alloca_count_151, align 1
  %load_155 = load i1, ptr %alloca_count_151, align 1
  br i1 %load_155, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  store ptr @.str.12_pattern_matching.7, ptr %alloca_count_148, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  br label %bb4

bb1:                                              ; preds = %bb10, %bb9, %bb6, %bb2
  %load_157 = load ptr, ptr %alloca_count_148, align 8
  ret ptr %load_157

bb4:                                              ; preds = %bb3
  %load_159 = load i64, ptr %alloca_count_149, align 8
  store i64 %load_159, ptr %alloca_count_158, align 8
  %load_162 = load i64, ptr %alloca_count_158, align 8
  %icmp_163 = icmp slt i64 %load_162, 0
  store i1 %icmp_163, ptr %alloca_count_161, align 1
  %load_165 = load i1, ptr %alloca_count_161, align 1
  br i1 %load_165, label %bb6, label %bb5

bb5:                                              ; preds = %bb4
  br label %bb7

bb6:                                              ; preds = %bb4
  store ptr @.str.12_pattern_matching.8, ptr %alloca_count_148, align 8
  br label %bb1

bb7:                                              ; preds = %bb5
  %load_168 = load i64, ptr %alloca_count_149, align 8
  store i64 %load_168, ptr %alloca_count_167, align 8
  %load_171 = load i64, ptr %alloca_count_167, align 8
  %iop_172 = srem i64 %load_171, 2
  store i64 %iop_172, ptr %alloca_count_170, align 8
  %load_175 = load i64, ptr %alloca_count_170, align 8
  %icmp_176 = icmp eq i64 %load_175, 0
  store i1 %icmp_176, ptr %alloca_count_174, align 1
  %load_178 = load i1, ptr %alloca_count_174, align 1
  br i1 %load_178, label %bb9, label %bb8

bb8:                                              ; preds = %bb7
  br label %bb10

bb9:                                              ; preds = %bb7
  store ptr @.str.12_pattern_matching.9, ptr %alloca_count_148, align 8
  br label %bb1

bb10:                                             ; preds = %bb8
  store ptr @.str.12_pattern_matching.10, ptr %alloca_count_148, align 8
  br label %bb1

bb11:                                             ; No predecessors!
  %load_181 = load ptr, ptr %alloca_count_148, align 8
  ret ptr %load_181
}

define internal i64 @unwrap_or({ i64, i64 } %0, i64 %1) {
bb0:
  %alloca_199 = alloca i1, align 1
  %alloca_count_199 = alloca i1, align 1
  %alloca_191 = alloca i64, align 8
  %alloca_count_191 = alloca i64, align 8
  %alloca_185 = alloca i1, align 1
  %alloca_count_185 = alloca i1, align 1
  %alloca_183 = alloca { i64, i64 }, align 8
  %alloca_count_183 = alloca { i64, i64 }, align 8
  %alloca_182 = alloca i64, align 8
  %alloca_count_182 = alloca i64, align 8
  store { i64, i64 } %0, ptr %alloca_count_183, align 8
  %load_187 = load i64, ptr %alloca_count_183, align 8
  %icmp_188 = icmp eq i64 %load_187, 0
  store i1 %icmp_188, ptr %alloca_count_185, align 1
  %load_190 = load i1, ptr %alloca_count_185, align 1
  br i1 %load_190, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  %gep_193 = getelementptr inbounds i8, ptr %alloca_count_183, i64 8
  %load_195 = load i64, ptr %gep_193, align 8
  store i64 %load_195, ptr %alloca_count_191, align 8
  %load_197 = load i64, ptr %alloca_count_191, align 8
  store i64 %load_197, ptr %alloca_count_182, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  %load_201 = load i64, ptr %alloca_count_183, align 8
  %icmp_202 = icmp eq i64 %load_201, 1
  store i1 %icmp_202, ptr %alloca_count_199, align 1
  %load_204 = load i1, ptr %alloca_count_199, align 1
  br i1 %load_204, label %bb4, label %bb5

bb1:                                              ; preds = %bb5, %bb4, %bb2
  %load_205 = load i64, ptr %alloca_count_182, align 8
  ret i64 %load_205

bb4:                                              ; preds = %bb3
  store i64 %1, ptr %alloca_count_182, align 8
  br label %bb1

bb5:                                              ; preds = %bb3
  br label %bb1
}

define i32 @main() {
bb0:
  %alloca_251 = alloca i64, align 8
  %alloca_count_251 = alloca i64, align 8
  %alloca_246 = alloca { i64, i64 }, align 8
  %alloca_count_246 = alloca { i64, i64 }, align 8
  %alloca_241 = alloca { i64, i64 }, align 8
  %alloca_count_241 = alloca { i64, i64 }, align 8
  %alloca_229 = alloca i64, align 8
  %alloca_count_229 = alloca i64, align 8
  %alloca_224 = alloca ptr, align 8
  %alloca_count_224 = alloca ptr, align 8
  %alloca_219 = alloca ptr, align 8
  %alloca_count_219 = alloca ptr, align 8
  %alloca_217 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_count_217 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_214 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_count_214 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_212 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_count_212 = alloca { i64, i8, i8, i8 }, align 8
  %call_207 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.11)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_208 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.12)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_209 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.13)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_210 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.14)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_211 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.15)
  br label %bb5

bb5:                                              ; preds = %bb4
  store { i64, i8, i8, i8 } zeroinitializer, ptr %alloca_count_212, align 8
  %load_215 = load { i64, i8, i8, i8 }, ptr %alloca_count_212, align 8
  store { i64, i8, i8, i8 } %load_215, ptr %alloca_count_214, align 8
  store { i64, i8, i8, i8 } { i64 2, i8 -128, i8 64, i8 32 }, ptr %alloca_count_217, align 8
  store ptr %alloca_count_214, ptr %alloca_count_219, align 8
  %load_221 = load ptr, ptr %alloca_count_219, align 8
  %call_222 = call ptr @describe(ptr %load_221)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_223 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.16, ptr %call_222)
  br label %bb7

bb7:                                              ; preds = %bb6
  store ptr %alloca_count_217, ptr %alloca_count_224, align 8
  %load_226 = load ptr, ptr %alloca_count_224, align 8
  %call_227 = call ptr @describe(ptr %load_226)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_228 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.17, ptr %call_227)
  br label %bb9

bb9:                                              ; preds = %bb8
  store i64 -5, ptr %alloca_count_229, align 8
  %load_232 = load i64, ptr %alloca_count_229, align 8
  %call_233 = call ptr @classify(i64 %load_232)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_234 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.18, ptr %call_233)
  br label %bb11

bb11:                                             ; preds = %bb10
  %call_235 = call ptr @classify(i64 0)
  br label %bb12

bb12:                                             ; preds = %bb11
  %call_236 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.19, ptr %call_235)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_237 = call ptr @classify(i64 4)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_238 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.20, ptr %call_237)
  br label %bb15

bb15:                                             ; preds = %bb14
  %call_239 = call ptr @classify(i64 7)
  br label %bb16

bb16:                                             ; preds = %bb15
  %call_240 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.21, ptr %call_239)
  br label %bb17

bb17:                                             ; preds = %bb16
  store { i64, i64 } { i64 0, i64 42 }, ptr %alloca_count_241, align 8
  %load_243 = load { i64, i64 }, ptr %alloca_count_241, align 8
  %call_244 = call i64 @unwrap_or({ i64, i64 } %load_243, i64 0)
  br label %bb18

bb18:                                             ; preds = %bb17
  %call_245 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.22, i64 %call_244)
  br label %bb19

bb19:                                             ; preds = %bb18
  store { i64, i64 } { i64 1, i64 0 }, ptr %alloca_count_246, align 8
  %load_248 = load { i64, i64 }, ptr %alloca_count_246, align 8
  %call_249 = call i64 @unwrap_or({ i64, i64 } %load_248, i64 99)
  br label %bb20

bb20:                                             ; preds = %bb19
  %call_250 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.23, i64 %call_249)
  br label %bb21

bb21:                                             ; preds = %bb20
  store i64 0, ptr %alloca_count_251, align 8
  %load_253 = load i64, ptr %alloca_count_251, align 8
  %call_254 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.24, i64 %load_253)
  br label %bb22

bb22:                                             ; preds = %bb21
  ret i32 0
}

define internal void @TestCase__run(ptr %0) {
bb0:
  ret void
}

declare i32 @printf(ptr, ...)

declare i32 @__gxx_personality_v0(...)
