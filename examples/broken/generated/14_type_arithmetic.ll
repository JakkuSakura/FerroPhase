; ModuleID = '14_type_arithmetic'
source_filename = "14_type_arithmetic"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_5 = internal constant [0 x { { ptr, i64 }, ptr }] zeroinitializer, align 8
@.str.14_type_arithmetic.0 = constant [51 x i8] c"test result: %lld passed; %lld failed; %lld total\0A\00"
@.str.14_type_arithmetic.1 = constant [13 x i8] c"  %s ... ok\0A\00"
@.str.14_type_arithmetic.2 = constant [17 x i8] c"  %s ... FAILED\0A\00"
@.str.14_type_arithmetic.3 = constant [38 x i8] c"\F0\9F\93\98 Tutorial: 14_type_arithmetic.fp\0A\00"
@.str.14_type_arithmetic.4 = constant [84 x i8] c"\F0\9F\A7\AD Focus: Type arithmetic examples: combining and relating types with operators.\0A\00"
@.str.14_type_arithmetic.5 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.14_type_arithmetic.6 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.14_type_arithmetic.7 = constant [2 x i8] c"\0A\00"
@.str.14_type_arithmetic.8 = constant [33 x i8] c"Foo a=%lld common=%lld foo=%lld\0A\00"
@.str.14_type_arithmetic.9 = constant [26 x i8] c"Bar common=%lld bar=%lld\0A\00"
@.str.14_type_arithmetic.10 = constant [49 x i8] c"FooPlusBar a=%lld common=%lld foo=%lld bar=%lld\0A\00"
@.str.14_type_arithmetic.11 = constant [29 x i8] c"FooMinusBar a=%lld foo=%lld\0A\00"
@.str.14_type_arithmetic.12 = constant [23 x i8] c"FooAndBar common=%lld\0A\00"
@.str.14_type_arithmetic.13 = constant [34 x i8] c"InlineRecord tag=%lld value=%lld\0A\00"
@.str.14_type_arithmetic.14 = constant [24 x i8] c"FooOrBar left sum=%lld\0A\00"
@.str.14_type_arithmetic.15 = constant [25 x i8] c"FooOrBar right sum=%lld\0A\00"
@.str.14_type_arithmetic.16 = constant [53 x i8] c"Int4[0]=%lld Int4[1]=%lld Int4[2]=%lld Int4[3]=%lld\0A\00"
@.str.14_type_arithmetic.17 = constant [6 x i8] c"hello\00"
@.str.14_type_arithmetic.18 = constant [6 x i8] c"green\00"
@.str.14_type_arithmetic.19 = constant [17 x i8] c"LiteralInt %lld\0A\00"
@.str.14_type_arithmetic.20 = constant [16 x i8] c"LiteralBool %d\0A\00"
@.str.14_type_arithmetic.21 = constant [15 x i8] c"LiteralStr %s\0A\00"
@.str.14_type_arithmetic.22 = constant [16 x i8] c"LiteralUnit %s\0A\00"
@.str.14_type_arithmetic.23 = constant [3 x i8] c"()\00"
@.str.14_type_arithmetic.24 = constant [16 x i8] c"LiteralNull %s\0A\00"
@.str.14_type_arithmetic.25 = constant [19 x i8] c"LiteralStrEnum %s\0A\00"
@.str.14_type_arithmetic.26 = constant [19 x i8] c"FooMaybe sum=%lld\0A\00"

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
  %alloca_7 = alloca i64, align 8
  %alloca_count_7 = alloca i64, align 8
  %alloca_6 = alloca i1, align 1
  %alloca_count_6 = alloca i1, align 1
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_11, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_13, align 8
  %load_16 = load [0 x { { ptr, i64 }, ptr }], ptr %alloca_count_13, align 8
  store [0 x { { ptr, i64 }, ptr }] %load_16, ptr %alloca_count_15, align 8
  store i64 0, ptr %alloca_count_9, align 8
  store i64 0, ptr %alloca_count_7, align 8
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
  %load_46 = load i64, ptr %alloca_count_9, align 8
  %load_47 = load i64, ptr %alloca_count_7, align 8
  %iop_48 = add i64 %load_46, %load_47
  store i64 %iop_48, ptr %alloca_count_45, align 8
  %load_51 = load i64, ptr %alloca_count_45, align 8
  store i64 %load_51, ptr %alloca_count_50, align 8
  %load_53 = load i64, ptr %alloca_count_9, align 8
  %load_54 = load i64, ptr %alloca_count_7, align 8
  %load_55 = load i64, ptr %alloca_count_50, align 8
  %call_56 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.0, i64 %load_53, i64 %load_54, i64 %load_55)
  br label %bb12

bb4:                                              ; preds = %bb2
  store i1 true, ptr %alloca_count_6, align 1
  br label %bb6

bb5:                                              ; preds = %bb2
  %landingpad = landingpad { ptr, i32 }
          catch ptr null
  store i1 false, ptr %alloca_count_6, align 1
  br label %bb6

bb12:                                             ; preds = %bb3
  %load_60 = load i64, ptr %alloca_count_50, align 8
  %load_61 = load i64, ptr %alloca_count_9, align 8
  %load_62 = load i64, ptr %alloca_count_7, align 8
  %insertvalue_63 = insertvalue { i64, i64, i64 } undef, i64 %load_60, 0
  %insertvalue_64 = insertvalue { i64, i64, i64 } %insertvalue_63, i64 %load_61, 1
  %insertvalue_65 = insertvalue { i64, i64, i64 } %insertvalue_64, i64 %load_62, 2
  store { i64, i64, i64 } %insertvalue_65, ptr %alloca_count_10, align 8
  %load_67 = load { i64, i64, i64 }, ptr %alloca_count_10, align 8
  ret { i64, i64, i64 } %load_67

bb6:                                              ; preds = %bb5, %bb4
  %load_69 = load i1, ptr %alloca_count_6, align 1
  store i1 %load_69, ptr %alloca_count_68, align 1
  %load_71 = load i1, ptr %alloca_count_68, align 1
  br i1 %load_71, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_72 = load i64, ptr %alloca_count_9, align 8
  %iop_73 = add i64 %load_72, 1
  store i64 %iop_73, ptr %alloca_count_9, align 8
  %load_76 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_77 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.1, { ptr, i64 } %load_76)
  br label %bb10

bb8:                                              ; preds = %bb6
  %load_78 = load i64, ptr %alloca_count_7, align 8
  %iop_79 = add i64 %load_78, 1
  store i64 %iop_79, ptr %alloca_count_7, align 8
  %load_82 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_83 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.2, { ptr, i64 } %load_82)
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

define internal i64 @describe_union({ i64, i64, i64, i64 } %0) {
bb0:
  %alloca_140 = alloca i64, align 8
  %alloca_count_140 = alloca i64, align 8
  %alloca_134 = alloca i64, align 8
  %alloca_count_134 = alloca i64, align 8
  %alloca_127 = alloca i1, align 1
  %alloca_count_127 = alloca i1, align 1
  %alloca_118 = alloca i64, align 8
  %alloca_count_118 = alloca i64, align 8
  %alloca_112 = alloca i64, align 8
  %alloca_count_112 = alloca i64, align 8
  %alloca_106 = alloca i64, align 8
  %alloca_count_106 = alloca i64, align 8
  %alloca_100 = alloca i64, align 8
  %alloca_count_100 = alloca i64, align 8
  %alloca_94 = alloca i1, align 1
  %alloca_count_94 = alloca i1, align 1
  %alloca_92 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_92 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_91 = alloca i64, align 8
  %alloca_count_91 = alloca i64, align 8
  store { i64, i64, i64, i64 } %0, ptr %alloca_count_92, align 8
  %load_96 = load i64, ptr %alloca_count_92, align 8
  %icmp_97 = icmp eq i64 %load_96, 0
  store i1 %icmp_97, ptr %alloca_count_94, align 1
  %load_99 = load i1, ptr %alloca_count_94, align 1
  br i1 %load_99, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  %gep_102 = getelementptr inbounds i8, ptr %alloca_count_92, i64 8
  %load_104 = load i64, ptr %gep_102, align 8
  store i64 %load_104, ptr %alloca_count_100, align 8
  %gep_108 = getelementptr inbounds i8, ptr %alloca_count_92, i64 16
  %load_110 = load i64, ptr %gep_108, align 8
  store i64 %load_110, ptr %alloca_count_106, align 8
  %gep_114 = getelementptr inbounds i8, ptr %alloca_count_92, i64 24
  %load_116 = load i64, ptr %gep_114, align 8
  store i64 %load_116, ptr %alloca_count_112, align 8
  %load_119 = load i64, ptr %alloca_count_100, align 8
  %load_120 = load i64, ptr %alloca_count_106, align 8
  %iop_121 = add i64 %load_119, %load_120
  store i64 %iop_121, ptr %alloca_count_118, align 8
  %load_123 = load i64, ptr %alloca_count_118, align 8
  %load_124 = load i64, ptr %alloca_count_112, align 8
  %iop_125 = add i64 %load_123, %load_124
  store i64 %iop_125, ptr %alloca_count_91, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  %load_129 = load i64, ptr %alloca_count_92, align 8
  %icmp_130 = icmp eq i64 %load_129, 1
  store i1 %icmp_130, ptr %alloca_count_127, align 1
  %load_132 = load i1, ptr %alloca_count_127, align 1
  br i1 %load_132, label %bb4, label %bb5

bb1:                                              ; preds = %bb5, %bb4, %bb2
  %load_133 = load i64, ptr %alloca_count_91, align 8
  ret i64 %load_133

bb4:                                              ; preds = %bb3
  %gep_136 = getelementptr inbounds i8, ptr %alloca_count_92, i64 8
  %load_138 = load i64, ptr %gep_136, align 8
  store i64 %load_138, ptr %alloca_count_134, align 8
  %gep_142 = getelementptr inbounds i8, ptr %alloca_count_92, i64 16
  %load_144 = load i64, ptr %gep_142, align 8
  store i64 %load_144, ptr %alloca_count_140, align 8
  %load_146 = load i64, ptr %alloca_count_134, align 8
  %load_147 = load i64, ptr %alloca_count_140, align 8
  %iop_148 = add i64 %load_146, %load_147
  store i64 %iop_148, ptr %alloca_count_91, align 8
  br label %bb1

bb5:                                              ; preds = %bb3
  br label %bb1
}

define internal i64 @describe_optional({ i64, i64, i64, i64 } %0) {
bb0:
  %alloca_187 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_187 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_177 = alloca i64, align 8
  %alloca_count_177 = alloca i64, align 8
  %alloca_171 = alloca i64, align 8
  %alloca_count_171 = alloca i64, align 8
  %alloca_165 = alloca i64, align 8
  %alloca_count_165 = alloca i64, align 8
  %alloca_159 = alloca i64, align 8
  %alloca_count_159 = alloca i64, align 8
  %alloca_153 = alloca i1, align 1
  %alloca_count_153 = alloca i1, align 1
  %alloca_151 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_151 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_150 = alloca i64, align 8
  %alloca_count_150 = alloca i64, align 8
  store { i64, i64, i64, i64 } %0, ptr %alloca_count_151, align 8
  %load_155 = load i64, ptr %alloca_count_151, align 8
  %icmp_156 = icmp eq i64 %load_155, 0
  store i1 %icmp_156, ptr %alloca_count_153, align 1
  %load_158 = load i1, ptr %alloca_count_153, align 1
  br i1 %load_158, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  %gep_161 = getelementptr inbounds i8, ptr %alloca_count_151, i64 8
  %load_163 = load i64, ptr %gep_161, align 8
  store i64 %load_163, ptr %alloca_count_159, align 8
  %gep_167 = getelementptr inbounds i8, ptr %alloca_count_151, i64 16
  %load_169 = load i64, ptr %gep_167, align 8
  store i64 %load_169, ptr %alloca_count_165, align 8
  %gep_173 = getelementptr inbounds i8, ptr %alloca_count_151, i64 24
  %load_175 = load i64, ptr %gep_173, align 8
  store i64 %load_175, ptr %alloca_count_171, align 8
  %load_178 = load i64, ptr %alloca_count_159, align 8
  %load_179 = load i64, ptr %alloca_count_165, align 8
  %iop_180 = add i64 %load_178, %load_179
  store i64 %iop_180, ptr %alloca_count_177, align 8
  %load_182 = load i64, ptr %alloca_count_177, align 8
  %load_183 = load i64, ptr %alloca_count_171, align 8
  %iop_184 = add i64 %load_182, %load_183
  store i64 %iop_184, ptr %alloca_count_150, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  br label %bb4

bb1:                                              ; preds = %bb4, %bb2
  %load_186 = load i64, ptr %alloca_count_150, align 8
  ret i64 %load_186

bb4:                                              ; preds = %bb3
  %load_188 = load { i64, i64, i64, i64 }, ptr %alloca_count_151, align 8
  store { i64, i64, i64, i64 } %load_188, ptr %alloca_count_187, align 8
  store i64 0, ptr %alloca_count_150, align 8
  br label %bb1

bb5:                                              ; No predecessors!
  %load_191 = load i64, ptr %alloca_count_150, align 8
  ret i64 %load_191
}

define i32 @main() {
bb0:
  %alloca_364 = alloca ptr, align 8
  %alloca_count_364 = alloca ptr, align 8
  %alloca_359 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_359 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_346 = alloca ptr, align 8
  %alloca_count_346 = alloca ptr, align 8
  %alloca_344 = alloca ptr, align 8
  %alloca_count_344 = alloca ptr, align 8
  %alloca_342 = alloca ptr, align 8
  %alloca_count_342 = alloca ptr, align 8
  %alloca_340 = alloca i1, align 1
  %alloca_count_340 = alloca i1, align 1
  %alloca_338 = alloca i64, align 8
  %alloca_count_338 = alloca i64, align 8
  %alloca_311 = alloca i64, align 8
  %alloca_count_311 = alloca i64, align 8
  %alloca_309 = alloca i64, align 8
  %alloca_count_309 = alloca i64, align 8
  %alloca_307 = alloca i64, align 8
  %alloca_count_307 = alloca i64, align 8
  %alloca_305 = alloca i64, align 8
  %alloca_count_305 = alloca i64, align 8
  %alloca_302 = alloca [4 x i64], align 8
  %alloca_count_302 = alloca [4 x i64], align 8
  %alloca_300 = alloca [4 x i64], align 8
  %alloca_count_300 = alloca [4 x i64], align 8
  %alloca_293 = alloca ptr, align 8
  %alloca_count_293 = alloca ptr, align 8
  %alloca_281 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_281 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_279 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_279 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_270 = alloca { i64, i64 }, align 8
  %alloca_count_270 = alloca { i64, i64 }, align 8
  %alloca_265 = alloca { i64 }, align 8
  %alloca_count_265 = alloca { i64 }, align 8
  %alloca_256 = alloca { i64, i64 }, align 8
  %alloca_count_256 = alloca { i64, i64 }, align 8
  %alloca_238 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_238 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_222 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_222 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_219 = alloca { i64, i64, i64 }, align 8
  %alloca_count_219 = alloca { i64, i64, i64 }, align 8
  %alloca_210 = alloca { i64, i64 }, align 8
  %alloca_count_210 = alloca { i64, i64 }, align 8
  %alloca_197 = alloca { i64, i64, i64 }, align 8
  %alloca_count_197 = alloca { i64, i64, i64 }, align 8
  %call_192 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.3)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_193 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.4)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_194 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.5)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_195 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.6)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_196 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.7)
  br label %bb5

bb5:                                              ; preds = %bb4
  store { i64, i64, i64 } { i64 1, i64 2, i64 3 }, ptr %alloca_count_197, align 8
  %load_200 = load i64, ptr %alloca_count_197, align 8
  %gep_202 = getelementptr inbounds i8, ptr %alloca_count_197, i64 8
  %load_204 = load i64, ptr %gep_202, align 8
  %gep_206 = getelementptr inbounds i8, ptr %alloca_count_197, i64 16
  %load_208 = load i64, ptr %gep_206, align 8
  %call_209 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.8, i64 %load_200, i64 %load_204, i64 %load_208)
  br label %bb6

bb6:                                              ; preds = %bb5
  store { i64, i64 } { i64 4, i64 5 }, ptr %alloca_count_210, align 8
  %load_213 = load i64, ptr %alloca_count_210, align 8
  %gep_215 = getelementptr inbounds i8, ptr %alloca_count_210, i64 8
  %load_217 = load i64, ptr %gep_215, align 8
  %call_218 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.9, i64 %load_213, i64 %load_217)
  br label %bb7

bb7:                                              ; preds = %bb6
  %load_220 = load { i64, i64, i64 }, ptr %alloca_count_197, align 8
  store { i64, i64, i64 } %load_220, ptr %alloca_count_219, align 8
  %load_224 = load i64, ptr %alloca_count_219, align 8
  %gep_226 = getelementptr inbounds i8, ptr %alloca_count_219, i64 8
  %load_228 = load i64, ptr %gep_226, align 8
  %gep_230 = getelementptr inbounds i8, ptr %alloca_count_219, i64 16
  %load_232 = load i64, ptr %gep_230, align 8
  %insertvalue_233 = insertvalue { i64, i64, i64, i64 } undef, i64 %load_224, 0
  %insertvalue_234 = insertvalue { i64, i64, i64, i64 } %insertvalue_233, i64 %load_228, 1
  %insertvalue_235 = insertvalue { i64, i64, i64, i64 } %insertvalue_234, i64 %load_232, 2
  %insertvalue_236 = insertvalue { i64, i64, i64, i64 } %insertvalue_235, i64 6, 3
  store { i64, i64, i64, i64 } %insertvalue_236, ptr %alloca_count_222, align 8
  %load_239 = load { i64, i64, i64, i64 }, ptr %alloca_count_222, align 8
  store { i64, i64, i64, i64 } %load_239, ptr %alloca_count_238, align 8
  %load_242 = load i64, ptr %alloca_count_238, align 8
  %gep_244 = getelementptr inbounds i8, ptr %alloca_count_238, i64 8
  %load_246 = load i64, ptr %gep_244, align 8
  %gep_248 = getelementptr inbounds i8, ptr %alloca_count_238, i64 16
  %load_250 = load i64, ptr %gep_248, align 8
  %gep_252 = getelementptr inbounds i8, ptr %alloca_count_238, i64 24
  %load_254 = load i64, ptr %gep_252, align 8
  %call_255 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.10, i64 %load_242, i64 %load_246, i64 %load_250, i64 %load_254)
  br label %bb8

bb8:                                              ; preds = %bb7
  store { i64, i64 } { i64 10, i64 20 }, ptr %alloca_count_256, align 8
  %load_259 = load i64, ptr %alloca_count_256, align 8
  %gep_261 = getelementptr inbounds i8, ptr %alloca_count_256, i64 8
  %load_263 = load i64, ptr %gep_261, align 8
  %call_264 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.11, i64 %load_259, i64 %load_263)
  br label %bb9

bb9:                                              ; preds = %bb8
  store { i64 } { i64 99 }, ptr %alloca_count_265, align 8
  %load_268 = load i64, ptr %alloca_count_265, align 8
  %call_269 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.12, i64 %load_268)
  br label %bb10

bb10:                                             ; preds = %bb9
  store { i64, i64 } { i64 7, i64 42 }, ptr %alloca_count_270, align 8
  %load_273 = load i64, ptr %alloca_count_270, align 8
  %gep_275 = getelementptr inbounds i8, ptr %alloca_count_270, i64 8
  %load_277 = load i64, ptr %gep_275, align 8
  %call_278 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.13, i64 %load_273, i64 %load_277)
  br label %bb11

bb11:                                             ; preds = %bb10
  store { i64, i64, i64, i64 } { i64 0, i64 2, i64 3, i64 4 }, ptr %alloca_count_279, align 8
  %load_283 = load i64, ptr %alloca_count_210, align 8
  %gep_285 = getelementptr inbounds i8, ptr %alloca_count_210, i64 8
  %load_287 = load i64, ptr %gep_285, align 8
  %insertvalue_289 = insertvalue { i64, i64, i64, i64 } { i64 1, i64 undef, i64 undef, i64 undef }, i64 %load_283, 1
  %insertvalue_290 = insertvalue { i64, i64, i64, i64 } %insertvalue_289, i64 %load_287, 2
  %insertvalue_291 = insertvalue { i64, i64, i64, i64 } %insertvalue_290, i64 0, 3
  store { i64, i64, i64, i64 } %insertvalue_291, ptr %alloca_count_281, align 8
  %load_294 = load { i64, i64, i64, i64 }, ptr %alloca_count_279, align 8
  %call_295 = call i64 @describe_union({ i64, i64, i64, i64 } %load_294)
  br label %bb12

bb12:                                             ; preds = %bb11
  %call_296 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.14, i64 %call_295)
  %load_297 = load { i64, i64, i64, i64 }, ptr %alloca_count_281, align 8
  %call_298 = call i64 @describe_union({ i64, i64, i64, i64 } %load_297)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_299 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.15, i64 %call_298)
  store [4 x i64] [i64 1, i64 2, i64 3, i64 4], ptr %alloca_count_300, align 8
  %load_303 = load [4 x i64], ptr %alloca_count_300, align 8
  store [4 x i64] %load_303, ptr %alloca_count_302, align 8
  store i64 0, ptr %alloca_count_305, align 8
  store i64 1, ptr %alloca_count_307, align 8
  store i64 2, ptr %alloca_count_309, align 8
  store i64 3, ptr %alloca_count_311, align 8
  %load_313 = load i64, ptr %alloca_count_305, align 8
  %iop_314 = mul i64 %load_313, 8
  %gep_316 = getelementptr inbounds i8, ptr %alloca_count_302, i64 %iop_314
  %load_318 = load i64, ptr %gep_316, align 8
  %load_319 = load i64, ptr %alloca_count_307, align 8
  %iop_320 = mul i64 %load_319, 8
  %gep_322 = getelementptr inbounds i8, ptr %alloca_count_302, i64 %iop_320
  %load_324 = load i64, ptr %gep_322, align 8
  %load_325 = load i64, ptr %alloca_count_309, align 8
  %iop_326 = mul i64 %load_325, 8
  %gep_328 = getelementptr inbounds i8, ptr %alloca_count_302, i64 %iop_326
  %load_330 = load i64, ptr %gep_328, align 8
  %load_331 = load i64, ptr %alloca_count_311, align 8
  %iop_332 = mul i64 %load_331, 8
  %gep_334 = getelementptr inbounds i8, ptr %alloca_count_302, i64 %iop_332
  %load_336 = load i64, ptr %gep_334, align 8
  %call_337 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.16, i64 %load_318, i64 %load_324, i64 %load_330, i64 %load_336)
  store i64 42, ptr %alloca_count_338, align 8
  store i1 true, ptr %alloca_count_340, align 1
  store ptr @.str.14_type_arithmetic.17, ptr %alloca_count_342, align 8
  store ptr null, ptr %alloca_count_344, align 8
  store ptr @.str.14_type_arithmetic.18, ptr %alloca_count_346, align 8
  %load_348 = load i64, ptr %alloca_count_338, align 8
  %call_349 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.19, i64 %load_348)
  %load_350 = load i1, ptr %alloca_count_340, align 1
  %call_351 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.20, i1 %load_350)
  %load_352 = load ptr, ptr %alloca_count_342, align 8
  %call_353 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.21, ptr %load_352)
  %call_354 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.22, ptr @.str.14_type_arithmetic.23)
  %load_355 = load ptr, ptr %alloca_count_344, align 8
  %call_356 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.24, ptr %load_355)
  %load_357 = load ptr, ptr %alloca_count_346, align 8
  %call_358 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.25, ptr %load_357)
  store { i64, i64, i64, i64 } { i64 0, i64 5, i64 6, i64 7 }, ptr %alloca_count_359, align 8
  %load_361 = load { i64, i64, i64, i64 }, ptr %alloca_count_359, align 8
  %call_362 = call i64 @describe_optional({ i64, i64, i64, i64 } %load_361)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_363 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.26, i64 %call_362)
  ret i32 0
}

define internal void @TestCase__run(ptr %0) {
bb0:
  ret void
}

declare i32 @printf(ptr, ...)

declare i32 @__gxx_personality_v0(...)
