; ModuleID = '06_struct_methods'
source_filename = "06_struct_methods"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_5 = internal constant [0 x { { ptr, i64 }, ptr }] zeroinitializer, align 8
@.str.06_struct_methods.0 = constant [51 x i8] c"test result: %lld passed; %lld failed; %lld total\0A\00"
@.str.06_struct_methods.1 = constant [13 x i8] c"  %s ... ok\0A\00"
@.str.06_struct_methods.2 = constant [17 x i8] c"  %s ... FAILED\0A\00"
@.str.06_struct_methods.3 = constant [37 x i8] c"\F0\9F\93\98 Tutorial: 06_struct_methods.fp\0A\00"
@.str.06_struct_methods.4 = constant [45 x i8] c"\F0\9F\A7\AD Focus: Struct methods and field access\0A\00"
@.str.06_struct_methods.5 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.06_struct_methods.6 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.06_struct_methods.7 = constant [2 x i8] c"\0A\00"
@.str.06_struct_methods.8 = constant [27 x i8] c"=== Struct Operations ===\0A\00"
@.str.06_struct_methods.9 = constant [19 x i8] c"p1 = (%lld, %lld)\0A\00"
@.str.06_struct_methods.10 = constant [19 x i8] c"p2 = (%lld, %lld)\0A\00"
@.str.06_struct_methods.11 = constant [35 x i8] c"p1 after translate = (%lld, %lld)\0A\00"
@.str.06_struct_methods.12 = constant [27 x i8] c"Distance\C2\B2(p1, p2) = %lld\0A\00"
@.str.06_struct_methods.13 = constant [23 x i8] c"Rectangle: %lld\C3\97%lld\0A\00"
@.str.06_struct_methods.14 = constant [15 x i8] c"  area = %lld\0A\00"
@.str.06_struct_methods.15 = constant [20 x i8] c"  perimeter = %lld\0A\00"
@.str.06_struct_methods.16 = constant [18 x i8] c"  is_square = %d\0A\00"

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
  %alloca_9 = alloca { i64, i64, i64 }, align 8
  %alloca_count_9 = alloca { i64, i64, i64 }, align 8
  %alloca_8 = alloca i1, align 1
  %alloca_count_8 = alloca i1, align 1
  %alloca_7 = alloca i64, align 8
  %alloca_count_7 = alloca i64, align 8
  %alloca_6 = alloca i64, align 8
  %alloca_count_6 = alloca i64, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_11, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_13, align 8
  %load_16 = load [0 x { { ptr, i64 }, ptr }], ptr %alloca_count_13, align 8
  store [0 x { { ptr, i64 }, ptr }] %load_16, ptr %alloca_count_15, align 8
  store i64 0, ptr %alloca_count_6, align 8
  store i64 0, ptr %alloca_count_7, align 8
  store i64 0, ptr %alloca_count_10, align 8
  br label %bb1

bb1:                                              ; preds = %bb9, %bb0
  store i64 0, ptr %alloca_count_21, align 8
  %load_24 = load i64, ptr %alloca_count_10, align 8
  %load_25 = load i64, ptr %alloca_count_21, align 8
  %icmp_26 = icmp slt i64 %load_24, %load_25
  store i1 %icmp_26, ptr %alloca_count_23, align 1
  %load_28 = load i1, ptr %alloca_count_23, align 1
  br i1 %load_28, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_30 = load i64, ptr %alloca_count_10, align 8
  store i64 %load_30, ptr %alloca_count_29, align 8
  %load_33 = load i64, ptr %alloca_count_10, align 8
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
  %load_47 = load i64, ptr %alloca_count_7, align 8
  %iop_48 = add i64 %load_46, %load_47
  store i64 %iop_48, ptr %alloca_count_45, align 8
  %load_51 = load i64, ptr %alloca_count_45, align 8
  store i64 %load_51, ptr %alloca_count_50, align 8
  %load_53 = load i64, ptr %alloca_count_6, align 8
  %load_54 = load i64, ptr %alloca_count_7, align 8
  %load_55 = load i64, ptr %alloca_count_50, align 8
  %call_56 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.0, i64 %load_53, i64 %load_54, i64 %load_55)
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
  %load_61 = load i64, ptr %alloca_count_6, align 8
  %load_62 = load i64, ptr %alloca_count_7, align 8
  %insertvalue_63 = insertvalue { i64, i64, i64 } undef, i64 %load_60, 0
  %insertvalue_64 = insertvalue { i64, i64, i64 } %insertvalue_63, i64 %load_61, 1
  %insertvalue_65 = insertvalue { i64, i64, i64 } %insertvalue_64, i64 %load_62, 2
  store { i64, i64, i64 } %insertvalue_65, ptr %alloca_count_9, align 8
  %load_67 = load { i64, i64, i64 }, ptr %alloca_count_9, align 8
  ret { i64, i64, i64 } %load_67

bb6:                                              ; preds = %bb5, %bb4
  %load_69 = load i1, ptr %alloca_count_8, align 1
  store i1 %load_69, ptr %alloca_count_68, align 1
  %load_71 = load i1, ptr %alloca_count_68, align 1
  br i1 %load_71, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_72 = load i64, ptr %alloca_count_6, align 8
  %iop_73 = add i64 %load_72, 1
  store i64 %iop_73, ptr %alloca_count_6, align 8
  %load_76 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_77 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.1, { ptr, i64 } %load_76)
  br label %bb10

bb8:                                              ; preds = %bb6
  %load_78 = load i64, ptr %alloca_count_7, align 8
  %iop_79 = add i64 %load_78, 1
  store i64 %iop_79, ptr %alloca_count_7, align 8
  %load_82 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_83 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.2, { ptr, i64 } %load_82)
  br label %bb11

bb10:                                             ; preds = %bb7
  br label %bb9

bb11:                                             ; preds = %bb8
  br label %bb9

bb9:                                              ; preds = %bb11, %bb10
  %load_84 = load i64, ptr %alloca_count_10, align 8
  %iop_85 = add i64 %load_84, 1
  store i64 %iop_85, ptr %alloca_count_10, align 8
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

define internal { i64, i64 } @Point__new(i64 %0, i64 %1) {
bb0:
  %alloca_91 = alloca { i64, i64 }, align 8
  %alloca_count_91 = alloca { i64, i64 }, align 8
  %insertvalue_92 = insertvalue { i64, i64 } undef, i64 %0, 0
  %insertvalue_93 = insertvalue { i64, i64 } %insertvalue_92, i64 %1, 1
  store { i64, i64 } %insertvalue_93, ptr %alloca_count_91, align 8
  %load_95 = load { i64, i64 }, ptr %alloca_count_91, align 8
  ret { i64, i64 } %load_95
}

define internal void @Point__translate(ptr %0, i64 %1, i64 %2) {
bb0:
  %alloca_96 = alloca ptr, align 8
  %alloca_count_96 = alloca ptr, align 8
  store ptr %0, ptr %alloca_count_96, align 8
  %load_98 = load ptr, ptr %alloca_count_96, align 8
  %load_100 = load ptr, ptr %alloca_count_96, align 8
  %load_102 = load i64, ptr %load_100, align 8
  %iop_103 = add i64 %load_102, %1
  store i64 %iop_103, ptr %load_98, align 8
  %load_105 = load ptr, ptr %alloca_count_96, align 8
  %gep_107 = getelementptr inbounds i8, ptr %load_105, i64 8
  %load_109 = load ptr, ptr %alloca_count_96, align 8
  %gep_111 = getelementptr inbounds i8, ptr %load_109, i64 8
  %load_113 = load i64, ptr %gep_111, align 8
  %iop_114 = add i64 %load_113, %2
  store i64 %iop_114, ptr %gep_107, align 8
  ret void
}

define internal i64 @Point__distance2(ptr %0, ptr %1) {
bb0:
  %alloca_146 = alloca i64, align 8
  %alloca_count_146 = alloca i64, align 8
  %alloca_141 = alloca i64, align 8
  %alloca_count_141 = alloca i64, align 8
  %alloca_138 = alloca i64, align 8
  %alloca_count_138 = alloca i64, align 8
  %alloca_127 = alloca i64, align 8
  %alloca_count_127 = alloca i64, align 8
  %alloca_124 = alloca i64, align 8
  %alloca_count_124 = alloca i64, align 8
  %alloca_117 = alloca i64, align 8
  %alloca_count_117 = alloca i64, align 8
  %alloca_116 = alloca i64, align 8
  %alloca_count_116 = alloca i64, align 8
  %load_119 = load i64, ptr %0, align 8
  %load_121 = load i64, ptr %1, align 8
  %iop_122 = sub i64 %load_119, %load_121
  store i64 %iop_122, ptr %alloca_count_117, align 8
  %load_125 = load i64, ptr %alloca_count_117, align 8
  store i64 %load_125, ptr %alloca_count_124, align 8
  %gep_129 = getelementptr inbounds i8, ptr %0, i64 8
  %load_131 = load i64, ptr %gep_129, align 8
  %gep_133 = getelementptr inbounds i8, ptr %1, i64 8
  %load_135 = load i64, ptr %gep_133, align 8
  %iop_136 = sub i64 %load_131, %load_135
  store i64 %iop_136, ptr %alloca_count_127, align 8
  %load_139 = load i64, ptr %alloca_count_127, align 8
  store i64 %load_139, ptr %alloca_count_138, align 8
  %load_142 = load i64, ptr %alloca_count_124, align 8
  %load_143 = load i64, ptr %alloca_count_124, align 8
  %iop_144 = mul i64 %load_142, %load_143
  store i64 %iop_144, ptr %alloca_count_141, align 8
  %load_147 = load i64, ptr %alloca_count_138, align 8
  %load_148 = load i64, ptr %alloca_count_138, align 8
  %iop_149 = mul i64 %load_147, %load_148
  store i64 %iop_149, ptr %alloca_count_146, align 8
  %load_151 = load i64, ptr %alloca_count_141, align 8
  %load_152 = load i64, ptr %alloca_count_146, align 8
  %iop_153 = add i64 %load_151, %load_152
  store i64 %iop_153, ptr %alloca_count_116, align 8
  %load_155 = load i64, ptr %alloca_count_116, align 8
  ret i64 %load_155
}

define internal { i64, i64 } @Rectangle__new(i64 %0, i64 %1) {
bb0:
  %alloca_156 = alloca { i64, i64 }, align 8
  %alloca_count_156 = alloca { i64, i64 }, align 8
  %insertvalue_157 = insertvalue { i64, i64 } undef, i64 %0, 0
  %insertvalue_158 = insertvalue { i64, i64 } %insertvalue_157, i64 %1, 1
  store { i64, i64 } %insertvalue_158, ptr %alloca_count_156, align 8
  %load_160 = load { i64, i64 }, ptr %alloca_count_156, align 8
  ret { i64, i64 } %load_160
}

define internal i64 @Rectangle__area(ptr %0) {
bb0:
  %alloca_161 = alloca i64, align 8
  %alloca_count_161 = alloca i64, align 8
  %load_163 = load i64, ptr %0, align 8
  %gep_165 = getelementptr inbounds i8, ptr %0, i64 8
  %load_167 = load i64, ptr %gep_165, align 8
  %iop_168 = mul i64 %load_163, %load_167
  store i64 %iop_168, ptr %alloca_count_161, align 8
  %load_170 = load i64, ptr %alloca_count_161, align 8
  ret i64 %load_170
}

define internal i64 @Rectangle__perimeter(ptr %0) {
bb0:
  %alloca_172 = alloca i64, align 8
  %alloca_count_172 = alloca i64, align 8
  %alloca_171 = alloca i64, align 8
  %alloca_count_171 = alloca i64, align 8
  %load_174 = load i64, ptr %0, align 8
  %gep_176 = getelementptr inbounds i8, ptr %0, i64 8
  %load_178 = load i64, ptr %gep_176, align 8
  %iop_179 = add i64 %load_174, %load_178
  store i64 %iop_179, ptr %alloca_count_172, align 8
  %load_181 = load i64, ptr %alloca_count_172, align 8
  %iop_182 = mul i64 2, %load_181
  store i64 %iop_182, ptr %alloca_count_171, align 8
  %load_184 = load i64, ptr %alloca_count_171, align 8
  ret i64 %load_184
}

define internal i1 @Rectangle__is_square(ptr %0) {
bb0:
  %alloca_185 = alloca i1, align 1
  %alloca_count_185 = alloca i1, align 1
  %load_187 = load i64, ptr %0, align 8
  %gep_189 = getelementptr inbounds i8, ptr %0, i64 8
  %load_191 = load i64, ptr %gep_189, align 8
  %icmp_192 = icmp eq i64 %load_187, %load_191
  store i1 %icmp_192, ptr %alloca_count_185, align 1
  %load_194 = load i1, ptr %alloca_count_185, align 1
  ret i1 %load_194
}

define i32 @main() {
bb0:
  %alloca_285 = alloca { i64, i64 }, align 8
  %alloca_count_285 = alloca { i64, i64 }, align 8
  %alloca_284 = alloca ptr, align 8
  %alloca_count_284 = alloca ptr, align 8
  %alloca_278 = alloca { i64, i64 }, align 8
  %alloca_count_278 = alloca { i64, i64 }, align 8
  %alloca_277 = alloca ptr, align 8
  %alloca_count_277 = alloca ptr, align 8
  %alloca_271 = alloca { i64, i64 }, align 8
  %alloca_count_271 = alloca { i64, i64 }, align 8
  %alloca_270 = alloca ptr, align 8
  %alloca_count_270 = alloca ptr, align 8
  %alloca_263 = alloca { i64, i64 }, align 8
  %alloca_count_263 = alloca { i64, i64 }, align 8
  %alloca_259 = alloca { i64, i64 }, align 8
  %alloca_count_259 = alloca { i64, i64 }, align 8
  %alloca_251 = alloca { i64, i64 }, align 8
  %alloca_count_251 = alloca { i64, i64 }, align 8
  %alloca_250 = alloca ptr, align 8
  %alloca_count_250 = alloca ptr, align 8
  %alloca_247 = alloca { i64, i64 }, align 8
  %alloca_count_247 = alloca { i64, i64 }, align 8
  %alloca_246 = alloca ptr, align 8
  %alloca_count_246 = alloca ptr, align 8
  %alloca_239 = alloca { i64, i64 }, align 8
  %alloca_count_239 = alloca { i64, i64 }, align 8
  %alloca_235 = alloca { i64, i64 }, align 8
  %alloca_count_235 = alloca { i64, i64 }, align 8
  %alloca_229 = alloca i64, align 8
  %alloca_count_229 = alloca i64, align 8
  %alloca_226 = alloca { i64, i64 }, align 8
  %alloca_count_226 = alloca { i64, i64 }, align 8
  %alloca_225 = alloca ptr, align 8
  %alloca_count_225 = alloca ptr, align 8
  %alloca_218 = alloca { i64, i64 }, align 8
  %alloca_count_218 = alloca { i64, i64 }, align 8
  %alloca_214 = alloca { i64, i64 }, align 8
  %alloca_count_214 = alloca { i64, i64 }, align 8
  %alloca_207 = alloca { i64, i64 }, align 8
  %alloca_count_207 = alloca { i64, i64 }, align 8
  %alloca_203 = alloca { i64, i64 }, align 8
  %alloca_count_203 = alloca { i64, i64 }, align 8
  %call_195 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.3)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_196 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.4)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_197 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.5)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_198 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.6)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_199 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.7)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_200 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.8)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_201 = call { i64, i64 } @Point__new(i64 10, i64 20)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_202 = call { i64, i64 } @Point__new(i64 5, i64 15)
  br label %bb8

bb8:                                              ; preds = %bb7
  store { i64, i64 } %call_201, ptr %alloca_count_203, align 8
  %load_206 = load i64, ptr %alloca_count_203, align 8
  store { i64, i64 } %call_201, ptr %alloca_count_207, align 8
  %gep_210 = getelementptr inbounds i8, ptr %alloca_count_207, i64 8
  %load_212 = load i64, ptr %gep_210, align 8
  %call_213 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.9, i64 %load_206, i64 %load_212)
  br label %bb9

bb9:                                              ; preds = %bb8
  store { i64, i64 } %call_202, ptr %alloca_count_214, align 8
  %load_217 = load i64, ptr %alloca_count_214, align 8
  store { i64, i64 } %call_202, ptr %alloca_count_218, align 8
  %gep_221 = getelementptr inbounds i8, ptr %alloca_count_218, i64 8
  %load_223 = load i64, ptr %gep_221, align 8
  %call_224 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.10, i64 %load_217, i64 %load_223)
  br label %bb10

bb10:                                             ; preds = %bb9
  store { i64, i64 } %call_201, ptr %alloca_count_226, align 8
  store ptr %alloca_count_226, ptr %alloca_count_225, align 8
  store i64 -4, ptr %alloca_count_229, align 8
  %load_232 = load ptr, ptr %alloca_count_225, align 8
  %load_233 = load i64, ptr %alloca_count_229, align 8
  call void @Point__translate(ptr %load_232, i64 3, i64 %load_233)
  br label %bb11

bb11:                                             ; preds = %bb10
  store { i64, i64 } %call_201, ptr %alloca_count_235, align 8
  %load_238 = load i64, ptr %alloca_count_235, align 8
  store { i64, i64 } %call_201, ptr %alloca_count_239, align 8
  %gep_242 = getelementptr inbounds i8, ptr %alloca_count_239, i64 8
  %load_244 = load i64, ptr %gep_242, align 8
  %call_245 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.11, i64 %load_238, i64 %load_244)
  br label %bb12

bb12:                                             ; preds = %bb11
  store { i64, i64 } %call_201, ptr %alloca_count_247, align 8
  store ptr %alloca_count_247, ptr %alloca_count_246, align 8
  store { i64, i64 } %call_202, ptr %alloca_count_251, align 8
  store ptr %alloca_count_251, ptr %alloca_count_250, align 8
  %load_254 = load ptr, ptr %alloca_count_246, align 8
  %load_255 = load ptr, ptr %alloca_count_250, align 8
  %call_256 = call i64 @Point__distance2(ptr %load_254, ptr %load_255)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_257 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.12, i64 %call_256)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_258 = call { i64, i64 } @Rectangle__new(i64 10, i64 5)
  br label %bb15

bb15:                                             ; preds = %bb14
  store { i64, i64 } %call_258, ptr %alloca_count_259, align 8
  %load_262 = load i64, ptr %alloca_count_259, align 8
  store { i64, i64 } %call_258, ptr %alloca_count_263, align 8
  %gep_266 = getelementptr inbounds i8, ptr %alloca_count_263, i64 8
  %load_268 = load i64, ptr %gep_266, align 8
  %call_269 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.13, i64 %load_262, i64 %load_268)
  br label %bb16

bb16:                                             ; preds = %bb15
  store { i64, i64 } %call_258, ptr %alloca_count_271, align 8
  store ptr %alloca_count_271, ptr %alloca_count_270, align 8
  %load_274 = load ptr, ptr %alloca_count_270, align 8
  %call_275 = call i64 @Rectangle__area(ptr %load_274)
  br label %bb17

bb17:                                             ; preds = %bb16
  %call_276 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.14, i64 %call_275)
  br label %bb18

bb18:                                             ; preds = %bb17
  store { i64, i64 } %call_258, ptr %alloca_count_278, align 8
  store ptr %alloca_count_278, ptr %alloca_count_277, align 8
  %load_281 = load ptr, ptr %alloca_count_277, align 8
  %call_282 = call i64 @Rectangle__perimeter(ptr %load_281)
  br label %bb19

bb19:                                             ; preds = %bb18
  %call_283 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.15, i64 %call_282)
  br label %bb20

bb20:                                             ; preds = %bb19
  store { i64, i64 } %call_258, ptr %alloca_count_285, align 8
  store ptr %alloca_count_285, ptr %alloca_count_284, align 8
  %load_288 = load ptr, ptr %alloca_count_284, align 8
  %call_289 = call i1 @Rectangle__is_square(ptr %load_288)
  br label %bb21

bb21:                                             ; preds = %bb20
  %zext = zext i1 %call_289 to i32
  %call_291 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.16, i32 %zext)
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
