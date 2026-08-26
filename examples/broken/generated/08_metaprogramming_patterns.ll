; ModuleID = '08_metaprogramming_patterns'
source_filename = "08_metaprogramming_patterns"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_5 = internal constant [0 x { { ptr, i64 }, ptr }] zeroinitializer, align 8
@.str.08_metaprogramming_patterns.0 = constant [51 x i8] c"test result: %lld passed; %lld failed; %lld total\0A\00"
@.str.08_metaprogramming_patterns.1 = constant [13 x i8] c"  %s ... ok\0A\00"
@.str.08_metaprogramming_patterns.2 = constant [17 x i8] c"  %s ... FAILED\0A\00"
@.str.08_metaprogramming_patterns.3 = constant [47 x i8] c"\F0\9F\93\98 Tutorial: 08_metaprogramming_patterns.fp\0A\00"
@.str.08_metaprogramming_patterns.4 = constant [70 x i8] c"\F0\9F\A7\AD Focus: Metaprogramming: const metadata + quote/splice execution\0A\00"
@.str.08_metaprogramming_patterns.5 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.08_metaprogramming_patterns.6 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.08_metaprogramming_patterns.7 = constant [2 x i8] c"\0A\00"
@.str.08_metaprogramming_patterns.8 = constant [32 x i8] c"=== Part 1: Const Metadata ===\0A\00"
@.str.08_metaprogramming_patterns.9 = constant [15 x i8] c"struct Point3D\00"
@.str.08_metaprogramming_patterns.10 = constant [32 x i8] c"%s has %lld fields (size=%lld)\0A\00"
@.str.08_metaprogramming_patterns.11 = constant [5 x i8] c"i64\0A\00"
@.str.08_metaprogramming_patterns.12 = constant [12 x i8] c"x type: %s\0A\00"
@.str.08_metaprogramming_patterns.13 = constant [9 x i8] c"fields:\0A\00"
@.str.08_metaprogramming_patterns.14 = constant [2 x i8] c"x\00"
@.str.08_metaprogramming_patterns.15 = constant [2 x i8] c"y\00"
@.str.08_metaprogramming_patterns.16 = constant [2 x i8] c"z\00"
@.str.08_metaprogramming_patterns.17 = constant [10 x i8] c"  %s: %s\0A\00"
@.str.08_metaprogramming_patterns.18 = constant [26 x i8] c"point=(%lld, %lld, %lld)\0A\00"
@.str.08_metaprogramming_patterns.19 = constant [7 x i8] c"origin\00"
@.str.08_metaprogramming_patterns.20 = constant [32 x i8] c"labeled=(%lld, %lld, %lld, %s)\0A\00"
@.str.08_metaprogramming_patterns.21 = constant [37 x i8] c"=== Part 2: Execute Quoted Code ===\0A\00"
@.str.08_metaprogramming_patterns.22 = constant [21 x i8] c"expr splice => %lld\0A\00"
@.str.08_metaprogramming_patterns.23 = constant [10 x i8] c"inspected\00"
@.str.08_metaprogramming_patterns.24 = constant [30 x i8] c"quote<fn> inspect => name=%s\0A\00"
@.str.08_metaprogramming_patterns.25 = constant [29 x i8] c"quote<[item]> count => %lld\0A\00"
@.str.08_metaprogramming_patterns.26 = constant [26 x i8] c"stmt splice => step=%lld\0A\00"
@.str.08_metaprogramming_patterns.27 = constant [16 x i8] c"metaprogramming\00"
@.str.08_metaprogramming_patterns.28 = constant [25 x i8] c"item splice => %s #%lld\0A\00"

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
  store i64 0, ptr %alloca_count_8, align 8
  store i64 0, ptr %alloca_count_10, align 8
  store i64 0, ptr %alloca_count_6, align 8
  br label %bb1

bb1:                                              ; preds = %bb9, %bb0
  store i64 0, ptr %alloca_count_21, align 8
  %load_24 = load i64, ptr %alloca_count_6, align 8
  %load_25 = load i64, ptr %alloca_count_21, align 8
  %icmp_26 = icmp slt i64 %load_24, %load_25
  store i1 %icmp_26, ptr %alloca_count_23, align 1
  %load_28 = load i1, ptr %alloca_count_23, align 1
  br i1 %load_28, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_30 = load i64, ptr %alloca_count_6, align 8
  store i64 %load_30, ptr %alloca_count_29, align 8
  %load_33 = load i64, ptr %alloca_count_6, align 8
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
  %load_46 = load i64, ptr %alloca_count_8, align 8
  %load_47 = load i64, ptr %alloca_count_10, align 8
  %iop_48 = add i64 %load_46, %load_47
  store i64 %iop_48, ptr %alloca_count_45, align 8
  %load_51 = load i64, ptr %alloca_count_45, align 8
  store i64 %load_51, ptr %alloca_count_50, align 8
  %load_53 = load i64, ptr %alloca_count_8, align 8
  %load_54 = load i64, ptr %alloca_count_10, align 8
  %load_55 = load i64, ptr %alloca_count_50, align 8
  %call_56 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.0, i64 %load_53, i64 %load_54, i64 %load_55)
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
  %load_61 = load i64, ptr %alloca_count_8, align 8
  %load_62 = load i64, ptr %alloca_count_10, align 8
  %insertvalue_63 = insertvalue { i64, i64, i64 } undef, i64 %load_60, 0
  %insertvalue_64 = insertvalue { i64, i64, i64 } %insertvalue_63, i64 %load_61, 1
  %insertvalue_65 = insertvalue { i64, i64, i64 } %insertvalue_64, i64 %load_62, 2
  store { i64, i64, i64 } %insertvalue_65, ptr %alloca_count_9, align 8
  %load_67 = load { i64, i64, i64 }, ptr %alloca_count_9, align 8
  ret { i64, i64, i64 } %load_67

bb6:                                              ; preds = %bb5, %bb4
  %load_69 = load i1, ptr %alloca_count_7, align 1
  store i1 %load_69, ptr %alloca_count_68, align 1
  %load_71 = load i1, ptr %alloca_count_68, align 1
  br i1 %load_71, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_72 = load i64, ptr %alloca_count_8, align 8
  %iop_73 = add i64 %load_72, 1
  store i64 %iop_73, ptr %alloca_count_8, align 8
  %load_76 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_77 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.1, { ptr, i64 } %load_76)
  br label %bb10

bb8:                                              ; preds = %bb6
  %load_78 = load i64, ptr %alloca_count_10, align 8
  %iop_79 = add i64 %load_78, 1
  store i64 %iop_79, ptr %alloca_count_10, align 8
  %load_82 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_83 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.2, { ptr, i64 } %load_82)
  br label %bb11

bb10:                                             ; preds = %bb7
  br label %bb9

bb11:                                             ; preds = %bb8
  br label %bb9

bb9:                                              ; preds = %bb11, %bb10
  %load_84 = load i64, ptr %alloca_count_6, align 8
  %iop_85 = add i64 %load_84, 1
  store i64 %iop_85, ptr %alloca_count_6, align 8
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

define i32 @main() {
bb0:
  %alloca_219 = alloca { ptr, i64 }, align 8
  %alloca_count_219 = alloca { ptr, i64 }, align 8
  %alloca_215 = alloca i64, align 8
  %alloca_count_215 = alloca i64, align 8
  %alloca_211 = alloca i64, align 8
  %alloca_count_211 = alloca i64, align 8
  %alloca_207 = alloca ptr, align 8
  %alloca_count_207 = alloca ptr, align 8
  %alloca_203 = alloca i64, align 8
  %alloca_count_203 = alloca i64, align 8
  %alloca_184 = alloca { i64, i64, i64, ptr }, align 8
  %alloca_count_184 = alloca { i64, i64, i64, ptr }, align 8
  %alloca_171 = alloca { i64, i64, i64 }, align 8
  %alloca_count_171 = alloca { i64, i64, i64 }, align 8
  %alloca_153 = alloca { ptr, ptr }, align 8
  %alloca_count_153 = alloca { ptr, ptr }, align 8
  %alloca_150 = alloca i64, align 8
  %alloca_count_150 = alloca i64, align 8
  %alloca_142 = alloca [3 x { ptr, ptr }], align 8
  %alloca_count_142 = alloca [3 x { ptr, ptr }], align 8
  %alloca_140 = alloca { ptr, ptr }, align 8
  %alloca_count_140 = alloca { ptr, ptr }, align 8
  %alloca_138 = alloca { ptr, ptr }, align 8
  %alloca_count_138 = alloca { ptr, ptr }, align 8
  %alloca_136 = alloca { ptr, ptr }, align 8
  %alloca_count_136 = alloca { ptr, ptr }, align 8
  %alloca_133 = alloca i64, align 8
  %alloca_count_133 = alloca i64, align 8
  %alloca_125 = alloca [3 x { ptr, ptr }], align 8
  %alloca_count_125 = alloca [3 x { ptr, ptr }], align 8
  %alloca_123 = alloca { ptr, ptr }, align 8
  %alloca_count_123 = alloca { ptr, ptr }, align 8
  %alloca_121 = alloca { ptr, ptr }, align 8
  %alloca_count_121 = alloca { ptr, ptr }, align 8
  %alloca_119 = alloca { ptr, ptr }, align 8
  %alloca_count_119 = alloca { ptr, ptr }, align 8
  %alloca_114 = alloca i1, align 1
  %alloca_count_114 = alloca i1, align 1
  %alloca_108 = alloca ptr, align 8
  %alloca_count_108 = alloca ptr, align 8
  %alloca_102 = alloca i64, align 8
  %alloca_count_102 = alloca i64, align 8
  %alloca_100 = alloca i64, align 8
  %alloca_count_100 = alloca i64, align 8
  %alloca_98 = alloca ptr, align 8
  %alloca_count_98 = alloca ptr, align 8
  %alloca_91 = alloca i64, align 8
  %alloca_count_91 = alloca i64, align 8
  %call_92 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.3)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_93 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.4)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_94 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.5)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_95 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.6)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_96 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.7)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_97 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.8)
  br label %bb6

bb6:                                              ; preds = %bb5
  store ptr @.str.08_metaprogramming_patterns.9, ptr %alloca_count_98, align 8
  store i64 3, ptr %alloca_count_100, align 8
  store i64 24, ptr %alloca_count_102, align 8
  %load_104 = load ptr, ptr %alloca_count_98, align 8
  %load_105 = load i64, ptr %alloca_count_100, align 8
  %load_106 = load i64, ptr %alloca_count_102, align 8
  %call_107 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.10, ptr %load_104, i64 %load_105, i64 %load_106)
  br label %bb7

bb7:                                              ; preds = %bb6
  store ptr @.str.08_metaprogramming_patterns.11, ptr %alloca_count_108, align 8
  %load_110 = load ptr, ptr %alloca_count_108, align 8
  %call_111 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.12, ptr %load_110)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_112 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.13)
  br label %bb9

bb9:                                              ; preds = %bb8
  store i64 0, ptr %alloca_count_91, align 8
  br label %bb10

bb10:                                             ; preds = %bb11, %bb9
  %load_115 = load i64, ptr %alloca_count_91, align 8
  %icmp_116 = icmp slt i64 %load_115, 3
  store i1 %icmp_116, ptr %alloca_count_114, align 1
  %load_118 = load i1, ptr %alloca_count_114, align 1
  br i1 %load_118, label %bb11, label %bb12

bb11:                                             ; preds = %bb10
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.14, ptr @.str.08_metaprogramming_patterns.11 }, ptr %alloca_count_119, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.15, ptr @.str.08_metaprogramming_patterns.11 }, ptr %alloca_count_121, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.16, ptr @.str.08_metaprogramming_patterns.11 }, ptr %alloca_count_123, align 8
  %load_126 = load { ptr, ptr }, ptr %alloca_count_119, align 8
  %load_127 = load { ptr, ptr }, ptr %alloca_count_121, align 8
  %load_128 = load { ptr, ptr }, ptr %alloca_count_123, align 8
  %insertvalue_129 = insertvalue [3 x { ptr, ptr }] undef, { ptr, ptr } %load_126, 0
  %insertvalue_130 = insertvalue [3 x { ptr, ptr }] %insertvalue_129, { ptr, ptr } %load_127, 1
  %insertvalue_131 = insertvalue [3 x { ptr, ptr }] %insertvalue_130, { ptr, ptr } %load_128, 2
  store [3 x { ptr, ptr }] %insertvalue_131, ptr %alloca_count_125, align 8
  %load_134 = load i64, ptr %alloca_count_91, align 8
  store i64 %load_134, ptr %alloca_count_133, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.14, ptr @.str.08_metaprogramming_patterns.11 }, ptr %alloca_count_136, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.15, ptr @.str.08_metaprogramming_patterns.11 }, ptr %alloca_count_138, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.16, ptr @.str.08_metaprogramming_patterns.11 }, ptr %alloca_count_140, align 8
  %load_143 = load { ptr, ptr }, ptr %alloca_count_136, align 8
  %load_144 = load { ptr, ptr }, ptr %alloca_count_138, align 8
  %load_145 = load { ptr, ptr }, ptr %alloca_count_140, align 8
  %insertvalue_146 = insertvalue [3 x { ptr, ptr }] undef, { ptr, ptr } %load_143, 0
  %insertvalue_147 = insertvalue [3 x { ptr, ptr }] %insertvalue_146, { ptr, ptr } %load_144, 1
  %insertvalue_148 = insertvalue [3 x { ptr, ptr }] %insertvalue_147, { ptr, ptr } %load_145, 2
  store [3 x { ptr, ptr }] %insertvalue_148, ptr %alloca_count_142, align 8
  %load_151 = load i64, ptr %alloca_count_91, align 8
  store i64 %load_151, ptr %alloca_count_150, align 8
  %load_154 = load i64, ptr %alloca_count_150, align 8
  %iop_155 = mul i64 %load_154, 16
  %gep_157 = getelementptr inbounds i8, ptr %alloca_count_142, i64 %iop_155
  %load_159 = load { ptr, ptr }, ptr %gep_157, align 8
  store { ptr, ptr } %load_159, ptr %alloca_count_153, align 8
  %load_162 = load ptr, ptr %alloca_count_153, align 8
  %gep_164 = getelementptr inbounds i8, ptr %alloca_count_153, i64 8
  %load_166 = load ptr, ptr %gep_164, align 8
  %call_167 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.17, ptr %load_162, ptr %load_166)
  %load_168 = load i64, ptr %alloca_count_91, align 8
  %iop_169 = add i64 %load_168, 1
  store i64 %iop_169, ptr %alloca_count_91, align 8
  br label %bb10

bb12:                                             ; preds = %bb10
  store { i64, i64, i64 } { i64 1, i64 2, i64 3 }, ptr %alloca_count_171, align 8
  %load_174 = load i64, ptr %alloca_count_171, align 8
  %gep_176 = getelementptr inbounds i8, ptr %alloca_count_171, i64 8
  %load_178 = load i64, ptr %gep_176, align 8
  %gep_180 = getelementptr inbounds i8, ptr %alloca_count_171, i64 16
  %load_182 = load i64, ptr %gep_180, align 8
  %call_183 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.18, i64 %load_174, i64 %load_178, i64 %load_182)
  br label %bb13

bb13:                                             ; preds = %bb12
  store { i64, i64, i64, ptr } { i64 4, i64 5, i64 6, ptr @.str.08_metaprogramming_patterns.19 }, ptr %alloca_count_184, align 8
  %load_187 = load i64, ptr %alloca_count_184, align 8
  %gep_189 = getelementptr inbounds i8, ptr %alloca_count_184, i64 8
  %load_191 = load i64, ptr %gep_189, align 8
  %gep_193 = getelementptr inbounds i8, ptr %alloca_count_184, i64 16
  %load_195 = load i64, ptr %gep_193, align 8
  %gep_197 = getelementptr inbounds i8, ptr %alloca_count_184, i64 24
  %load_199 = load ptr, ptr %gep_197, align 8
  %call_200 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.20, i64 %load_187, i64 %load_191, i64 %load_195, ptr %load_199)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_201 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.7)
  br label %bb15

bb15:                                             ; preds = %bb14
  %call_202 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.21)
  br label %bb16

bb16:                                             ; preds = %bb15
  store i64 20, ptr %alloca_count_203, align 8
  %load_205 = load i64, ptr %alloca_count_203, align 8
  %call_206 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.22, i64 %load_205)
  br label %bb17

bb17:                                             ; preds = %bb16
  store ptr @.str.08_metaprogramming_patterns.23, ptr %alloca_count_207, align 8
  %load_209 = load ptr, ptr %alloca_count_207, align 8
  %call_210 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.24, ptr %load_209)
  br label %bb18

bb18:                                             ; preds = %bb17
  store i64 2, ptr %alloca_count_211, align 8
  %load_213 = load i64, ptr %alloca_count_211, align 8
  %call_214 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.25, i64 %load_213)
  br label %bb19

bb19:                                             ; preds = %bb18
  store i64 21, ptr %alloca_count_215, align 8
  %load_217 = load i64, ptr %alloca_count_215, align 8
  %call_218 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.26, i64 %load_217)
  br label %bb20

bb20:                                             ; preds = %bb19
  %load_220 = load i64, ptr %alloca_count_203, align 8
  %insertvalue_222 = insertvalue { ptr, i64 } { ptr @.str.08_metaprogramming_patterns.27, i64 undef }, i64 %load_220, 1
  store { ptr, i64 } %insertvalue_222, ptr %alloca_count_219, align 8
  %load_225 = load ptr, ptr %alloca_count_219, align 8
  %gep_227 = getelementptr inbounds i8, ptr %alloca_count_219, i64 8
  %load_229 = load i64, ptr %gep_227, align 8
  %call_230 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.28, ptr %load_225, i64 %load_229)
  br label %bb21

bb21:                                             ; preds = %bb20
  ret i32 0
}

define internal void @TestCase__run(ptr %0) {
bb0:
  ret void
}

declare i32 @printf(ptr, ...)

declare i32 @__gxx_personality_v0(...)
