; ModuleID = '13_loops'
source_filename = "13_loops"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_5 = internal constant [0 x { { ptr, i64 }, ptr }] zeroinitializer, align 8
@.str.13_loops.0 = constant [51 x i8] c"test result: %lld passed; %lld failed; %lld total\0A\00"
@.str.13_loops.1 = constant [13 x i8] c"  %s ... ok\0A\00"
@.str.13_loops.2 = constant [17 x i8] c"  %s ... FAILED\0A\00"
@.str.13_loops.3 = constant [28 x i8] c"\F0\9F\93\98 Tutorial: 13_loops.fp\0A\00"
@.str.13_loops.4 = constant [52 x i8] c"\F0\9F\A7\AD Focus: Loop constructs: while, for, and loop.\0A\00"
@.str.13_loops.5 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.13_loops.6 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.13_loops.7 = constant [2 x i8] c"\0A\00"
@.str.13_loops.8 = constant [26 x i8] c"=== Loop Constructs ===\0A\0A\00"
@.str.13_loops.9 = constant [28 x i8] c"1. While loop - factorial:\0A\00"
@.str.13_loops.10 = constant [13 x i8] c"  5! = %lld\0A\00"
@.str.13_loops.11 = constant [13 x i8] c"  7! = %lld\0A\00"
@.str.13_loops.12 = constant [27 x i8] c"\0A2. For loop - sum range:\0A\00"
@.str.13_loops.13 = constant [21 x i8] c"  sum(1..10) = %lld\0A\00"
@.str.13_loops.14 = constant [21 x i8] c"  sum(5..15) = %lld\0A\00"
@.str.13_loops.15 = constant [33 x i8] c"\0A3. Loop with break expression:\0A\00"
@.str.13_loops.16 = constant [29 x i8] c"  First divisor of 24: %lld\0A\00"
@.str.13_loops.17 = constant [29 x i8] c"  First divisor of 17: %lld\0A\00"
@.str.13_loops.18 = constant [25 x i8] c"\0A4. Loop with continue:\0A\00"
@.str.13_loops.19 = constant [34 x i8] c"  Sum of even numbers < 10: %lld\0A\00"
@.str.13_loops.20 = constant [19 x i8] c"\0A5. Nested loops:\0A\00"
@.str.13_loops.21 = constant [21 x i8] c"\0A  Iterations: %lld\0A\00"
@.str.13_loops.22 = constant [29 x i8] c"\0A6. Compile-time recursion:\0A\00"
@.str.13_loops.23 = constant [29 x i8] c"  const_factorial(5) = %lld\0A\00"
@.str.13_loops.24 = constant [8 x i8] c"[%lld] \00"
@.str.13_loops.25 = constant [36 x i8] c"\0A\E2\9C\93 Loop constructs demonstrated!\0A\00"

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
  %alloca_8 = alloca i64, align 8
  %alloca_count_8 = alloca i64, align 8
  %alloca_7 = alloca { i64, i64, i64 }, align 8
  %alloca_count_7 = alloca { i64, i64, i64 }, align 8
  %alloca_6 = alloca i1, align 1
  %alloca_count_6 = alloca i1, align 1
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_11, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_13, align 8
  %load_16 = load [0 x { { ptr, i64 }, ptr }], ptr %alloca_count_13, align 8
  store [0 x { { ptr, i64 }, ptr }] %load_16, ptr %alloca_count_15, align 8
  store i64 0, ptr %alloca_count_8, align 8
  store i64 0, ptr %alloca_count_10, align 8
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
  %load_46 = load i64, ptr %alloca_count_8, align 8
  %load_47 = load i64, ptr %alloca_count_10, align 8
  %iop_48 = add i64 %load_46, %load_47
  store i64 %iop_48, ptr %alloca_count_45, align 8
  %load_51 = load i64, ptr %alloca_count_45, align 8
  store i64 %load_51, ptr %alloca_count_50, align 8
  %load_53 = load i64, ptr %alloca_count_8, align 8
  %load_54 = load i64, ptr %alloca_count_10, align 8
  %load_55 = load i64, ptr %alloca_count_50, align 8
  %call_56 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.0, i64 %load_53, i64 %load_54, i64 %load_55)
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
  %load_61 = load i64, ptr %alloca_count_8, align 8
  %load_62 = load i64, ptr %alloca_count_10, align 8
  %insertvalue_63 = insertvalue { i64, i64, i64 } undef, i64 %load_60, 0
  %insertvalue_64 = insertvalue { i64, i64, i64 } %insertvalue_63, i64 %load_61, 1
  %insertvalue_65 = insertvalue { i64, i64, i64 } %insertvalue_64, i64 %load_62, 2
  store { i64, i64, i64 } %insertvalue_65, ptr %alloca_count_7, align 8
  %load_67 = load { i64, i64, i64 }, ptr %alloca_count_7, align 8
  ret { i64, i64, i64 } %load_67

bb6:                                              ; preds = %bb5, %bb4
  %load_69 = load i1, ptr %alloca_count_6, align 1
  store i1 %load_69, ptr %alloca_count_68, align 1
  %load_71 = load i1, ptr %alloca_count_68, align 1
  br i1 %load_71, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_72 = load i64, ptr %alloca_count_8, align 8
  %iop_73 = add i64 %load_72, 1
  store i64 %iop_73, ptr %alloca_count_8, align 8
  %load_76 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_77 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.1, { ptr, i64 } %load_76)
  br label %bb10

bb8:                                              ; preds = %bb6
  %load_78 = load i64, ptr %alloca_count_10, align 8
  %iop_79 = add i64 %load_78, 1
  store i64 %iop_79, ptr %alloca_count_10, align 8
  %load_82 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_83 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.2, { ptr, i64 } %load_82)
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

define internal i64 @factorial(i64 %0) {
bb0:
  %alloca_96 = alloca i1, align 1
  %alloca_count_96 = alloca i1, align 1
  %alloca_93 = alloca i64, align 8
  %alloca_count_93 = alloca i64, align 8
  %alloca_92 = alloca i64, align 8
  %alloca_count_92 = alloca i64, align 8
  %alloca_91 = alloca i64, align 8
  %alloca_count_91 = alloca i64, align 8
  store i64 1, ptr %alloca_count_92, align 8
  store i64 1, ptr %alloca_count_93, align 8
  br label %bb1

bb1:                                              ; preds = %bb2, %bb0
  %load_97 = load i64, ptr %alloca_count_93, align 8
  %icmp_98 = icmp sle i64 %load_97, %0
  store i1 %icmp_98, ptr %alloca_count_96, align 1
  %load_100 = load i1, ptr %alloca_count_96, align 1
  br i1 %load_100, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_101 = load i64, ptr %alloca_count_92, align 8
  %load_102 = load i64, ptr %alloca_count_93, align 8
  %iop_103 = mul i64 %load_101, %load_102
  store i64 %iop_103, ptr %alloca_count_92, align 8
  %load_105 = load i64, ptr %alloca_count_93, align 8
  %iop_106 = add i64 %load_105, 1
  store i64 %iop_106, ptr %alloca_count_93, align 8
  br label %bb1

bb3:                                              ; preds = %bb1
  %load_108 = load i64, ptr %alloca_count_92, align 8
  store i64 %load_108, ptr %alloca_count_91, align 8
  %load_110 = load i64, ptr %alloca_count_91, align 8
  ret i64 %load_110
}

define internal i64 @const_factorial(i64 %0) {
bb0:
  %alloca_117 = alloca i64, align 8
  %alloca_count_117 = alloca i64, align 8
  %alloca_112 = alloca i1, align 1
  %alloca_count_112 = alloca i1, align 1
  %alloca_111 = alloca i64, align 8
  %alloca_count_111 = alloca i64, align 8
  %icmp_113 = icmp sle i64 %0, 1
  store i1 %icmp_113, ptr %alloca_count_112, align 1
  %load_115 = load i1, ptr %alloca_count_112, align 1
  br i1 %load_115, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  store i64 1, ptr %alloca_count_111, align 8
  br label %bb3

bb2:                                              ; preds = %bb0
  %iop_118 = sub i64 %0, 1
  store i64 %iop_118, ptr %alloca_count_117, align 8
  %load_120 = load i64, ptr %alloca_count_117, align 8
  %call_121 = call i64 @const_factorial(i64 %load_120)
  br label %bb4

bb3:                                              ; preds = %bb4, %bb1
  %load_122 = load i64, ptr %alloca_count_111, align 8
  ret i64 %load_122

bb4:                                              ; preds = %bb2
  %iop_123 = mul i64 %0, %call_121
  store i64 %iop_123, ptr %alloca_count_111, align 8
  br label %bb3
}

define internal i64 @sum_range(i64 %0, i64 %1) {
bb0:
  %alloca_130 = alloca i1, align 1
  %alloca_count_130 = alloca i1, align 1
  %alloca_127 = alloca i64, align 8
  %alloca_count_127 = alloca i64, align 8
  %alloca_126 = alloca i64, align 8
  %alloca_count_126 = alloca i64, align 8
  %alloca_125 = alloca i64, align 8
  %alloca_count_125 = alloca i64, align 8
  store i64 0, ptr %alloca_count_125, align 8
  store i64 %0, ptr %alloca_count_126, align 8
  br label %bb1

bb1:                                              ; preds = %bb2, %bb0
  %load_131 = load i64, ptr %alloca_count_126, align 8
  %icmp_132 = icmp slt i64 %load_131, %1
  store i1 %icmp_132, ptr %alloca_count_130, align 1
  %load_134 = load i1, ptr %alloca_count_130, align 1
  br i1 %load_134, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_135 = load i64, ptr %alloca_count_125, align 8
  %load_136 = load i64, ptr %alloca_count_126, align 8
  %iop_137 = add i64 %load_135, %load_136
  store i64 %iop_137, ptr %alloca_count_125, align 8
  %load_139 = load i64, ptr %alloca_count_126, align 8
  %iop_140 = add i64 %load_139, 1
  store i64 %iop_140, ptr %alloca_count_126, align 8
  br label %bb1

bb3:                                              ; preds = %bb1
  %load_142 = load i64, ptr %alloca_count_125, align 8
  store i64 %load_142, ptr %alloca_count_127, align 8
  %load_144 = load i64, ptr %alloca_count_127, align 8
  ret i64 %load_144
}

define internal i64 @find_first_divisor(i64 %0) {
bb0:
  %alloca_164 = alloca i1, align 1
  %alloca_count_164 = alloca i1, align 1
  %alloca_160 = alloca i64, align 8
  %alloca_count_160 = alloca i64, align 8
  %alloca_153 = alloca i1, align 1
  %alloca_count_153 = alloca i1, align 1
  %alloca_148 = alloca i64, align 8
  %alloca_count_148 = alloca i64, align 8
  %alloca_146 = alloca i64, align 8
  %alloca_count_146 = alloca i64, align 8
  %alloca_145 = alloca i64, align 8
  %alloca_count_145 = alloca i64, align 8
  store i64 2, ptr %alloca_count_145, align 8
  br label %bb1

bb1:                                              ; preds = %bb10, %bb0
  br label %bb2

bb2:                                              ; preds = %bb1
  %load_149 = load i64, ptr %alloca_count_145, align 8
  %load_150 = load i64, ptr %alloca_count_145, align 8
  %iop_151 = mul i64 %load_149, %load_150
  store i64 %iop_151, ptr %alloca_count_148, align 8
  %load_154 = load i64, ptr %alloca_count_148, align 8
  %icmp_155 = icmp sgt i64 %load_154, %0
  store i1 %icmp_155, ptr %alloca_count_153, align 1
  %load_157 = load i1, ptr %alloca_count_153, align 1
  br i1 %load_157, label %bb4, label %bb5

bb4:                                              ; preds = %bb2
  store i64 %0, ptr %alloca_count_146, align 8
  br label %bb3

bb5:                                              ; preds = %bb2
  br label %bb6

bb3:                                              ; preds = %bb8, %bb4
  %load_159 = load i64, ptr %alloca_count_146, align 8
  ret i64 %load_159

bb6:                                              ; preds = %bb7, %bb5
  %load_161 = load i64, ptr %alloca_count_145, align 8
  %iop_162 = srem i64 %0, %load_161
  store i64 %iop_162, ptr %alloca_count_160, align 8
  %load_165 = load i64, ptr %alloca_count_160, align 8
  %icmp_166 = icmp eq i64 %load_165, 0
  store i1 %icmp_166, ptr %alloca_count_164, align 1
  %load_168 = load i1, ptr %alloca_count_164, align 1
  br i1 %load_168, label %bb8, label %bb9

bb8:                                              ; preds = %bb6
  %load_169 = load i64, ptr %alloca_count_145, align 8
  store i64 %load_169, ptr %alloca_count_146, align 8
  br label %bb3

bb9:                                              ; preds = %bb6
  br label %bb10

bb10:                                             ; preds = %bb11, %bb9
  %load_171 = load i64, ptr %alloca_count_145, align 8
  %iop_172 = add i64 %load_171, 1
  store i64 %iop_172, ptr %alloca_count_145, align 8
  br label %bb1

bb7:                                              ; No predecessors!
  br label %bb6

bb11:                                             ; No predecessors!
  br label %bb10
}

define internal i64 @sum_even_numbers(i64 %0) {
bb0:
  %alloca_191 = alloca i1, align 1
  %alloca_count_191 = alloca i1, align 1
  %alloca_187 = alloca i64, align 8
  %alloca_count_187 = alloca i64, align 8
  %alloca_179 = alloca i1, align 1
  %alloca_count_179 = alloca i1, align 1
  %alloca_176 = alloca i64, align 8
  %alloca_count_176 = alloca i64, align 8
  %alloca_175 = alloca i64, align 8
  %alloca_count_175 = alloca i64, align 8
  %alloca_174 = alloca i64, align 8
  %alloca_count_174 = alloca i64, align 8
  store i64 0, ptr %alloca_count_175, align 8
  store i64 0, ptr %alloca_count_174, align 8
  br label %bb1

bb1:                                              ; preds = %bb6, %bb4, %bb0
  %load_180 = load i64, ptr %alloca_count_174, align 8
  %icmp_181 = icmp slt i64 %load_180, %0
  store i1 %icmp_181, ptr %alloca_count_179, align 1
  %load_183 = load i1, ptr %alloca_count_179, align 1
  br i1 %load_183, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_184 = load i64, ptr %alloca_count_174, align 8
  %iop_185 = add i64 %load_184, 1
  store i64 %iop_185, ptr %alloca_count_174, align 8
  %load_188 = load i64, ptr %alloca_count_174, align 8
  %iop_189 = srem i64 %load_188, 2
  store i64 %iop_189, ptr %alloca_count_187, align 8
  %load_192 = load i64, ptr %alloca_count_187, align 8
  %icmp_193 = icmp ne i64 %load_192, 0
  store i1 %icmp_193, ptr %alloca_count_191, align 1
  %load_195 = load i1, ptr %alloca_count_191, align 1
  br i1 %load_195, label %bb4, label %bb5

bb3:                                              ; preds = %bb1
  %load_196 = load i64, ptr %alloca_count_175, align 8
  store i64 %load_196, ptr %alloca_count_176, align 8
  %load_198 = load i64, ptr %alloca_count_176, align 8
  ret i64 %load_198

bb4:                                              ; preds = %bb2
  br label %bb1

bb5:                                              ; preds = %bb2
  br label %bb6

bb6:                                              ; preds = %bb7, %bb5
  %load_199 = load i64, ptr %alloca_count_175, align 8
  %load_200 = load i64, ptr %alloca_count_174, align 8
  %iop_201 = add i64 %load_199, %load_200
  store i64 %iop_201, ptr %alloca_count_175, align 8
  br label %bb1

bb7:                                              ; No predecessors!
  br label %bb6
}

define i32 @main() {
bb0:
  %alloca_259 = alloca i64, align 8
  %alloca_count_259 = alloca i64, align 8
  %alloca_250 = alloca i1, align 1
  %alloca_count_250 = alloca i1, align 1
  %alloca_241 = alloca i1, align 1
  %alloca_count_241 = alloca i1, align 1
  %alloca_233 = alloca i1, align 1
  %alloca_count_233 = alloca i1, align 1
  %alloca_205 = alloca i64, align 8
  %alloca_count_205 = alloca i64, align 8
  %alloca_204 = alloca i64, align 8
  %alloca_count_204 = alloca i64, align 8
  %alloca_203 = alloca i64, align 8
  %alloca_count_203 = alloca i64, align 8
  %call_206 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.3)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_207 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.4)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_208 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.5)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_209 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.6)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_210 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.7)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_211 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.8)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_212 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.9)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_213 = call i64 @factorial(i64 5)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_214 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.10, i64 %call_213)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_215 = call i64 @factorial(i64 7)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_216 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.11, i64 %call_215)
  br label %bb11

bb11:                                             ; preds = %bb10
  %call_217 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.12)
  br label %bb12

bb12:                                             ; preds = %bb11
  %call_218 = call i64 @sum_range(i64 1, i64 10)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_219 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.13, i64 %call_218)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_220 = call i64 @sum_range(i64 5, i64 15)
  br label %bb15

bb15:                                             ; preds = %bb14
  %call_221 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.14, i64 %call_220)
  br label %bb16

bb16:                                             ; preds = %bb15
  %call_222 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.15)
  br label %bb17

bb17:                                             ; preds = %bb16
  %call_223 = call i64 @find_first_divisor(i64 24)
  br label %bb18

bb18:                                             ; preds = %bb17
  %call_224 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.16, i64 %call_223)
  br label %bb19

bb19:                                             ; preds = %bb18
  %call_225 = call i64 @find_first_divisor(i64 17)
  br label %bb20

bb20:                                             ; preds = %bb19
  %call_226 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.17, i64 %call_225)
  br label %bb21

bb21:                                             ; preds = %bb20
  %call_227 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.18)
  br label %bb22

bb22:                                             ; preds = %bb21
  %call_228 = call i64 @sum_even_numbers(i64 10)
  br label %bb23

bb23:                                             ; preds = %bb22
  %call_229 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.19, i64 %call_228)
  br label %bb24

bb24:                                             ; preds = %bb23
  %call_230 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.20)
  br label %bb25

bb25:                                             ; preds = %bb24
  store i64 0, ptr %alloca_count_205, align 8
  store i64 1, ptr %alloca_count_204, align 8
  br label %bb26

bb26:                                             ; preds = %bb31, %bb25
  %load_234 = load i64, ptr %alloca_count_204, align 8
  %icmp_235 = icmp slt i64 %load_234, 4
  store i1 %icmp_235, ptr %alloca_count_233, align 1
  %load_237 = load i1, ptr %alloca_count_233, align 1
  br i1 %load_237, label %bb27, label %bb28

bb27:                                             ; preds = %bb26
  store i64 1, ptr %alloca_count_203, align 8
  br label %bb29

bb28:                                             ; preds = %bb26
  %load_239 = load i64, ptr %alloca_count_205, align 8
  %call_240 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.21, i64 %load_239)
  br label %bb35

bb29:                                             ; preds = %bb34, %bb27
  %load_242 = load i64, ptr %alloca_count_203, align 8
  %icmp_243 = icmp slt i64 %load_242, 4
  store i1 %icmp_243, ptr %alloca_count_241, align 1
  %load_245 = load i1, ptr %alloca_count_241, align 1
  br i1 %load_245, label %bb30, label %bb31

bb35:                                             ; preds = %bb28
  %call_246 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.22)
  br label %bb36

bb30:                                             ; preds = %bb29
  %load_247 = load i64, ptr %alloca_count_205, align 8
  %iop_248 = add i64 %load_247, 1
  store i64 %iop_248, ptr %alloca_count_205, align 8
  %load_251 = load i64, ptr %alloca_count_204, align 8
  %load_252 = load i64, ptr %alloca_count_203, align 8
  %icmp_253 = icmp eq i64 %load_251, %load_252
  store i1 %icmp_253, ptr %alloca_count_250, align 1
  %load_255 = load i1, ptr %alloca_count_250, align 1
  br i1 %load_255, label %bb32, label %bb33

bb31:                                             ; preds = %bb29
  %load_256 = load i64, ptr %alloca_count_204, align 8
  %iop_257 = add i64 %load_256, 1
  store i64 %iop_257, ptr %alloca_count_204, align 8
  br label %bb26

bb36:                                             ; preds = %bb35
  store i64 120, ptr %alloca_count_259, align 8
  %load_261 = load i64, ptr %alloca_count_259, align 8
  %call_262 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.23, i64 %load_261)
  br label %bb37

bb32:                                             ; preds = %bb30
  %load_263 = load i64, ptr %alloca_count_204, align 8
  %call_264 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.24, i64 %load_263)
  br label %bb34

bb33:                                             ; preds = %bb30
  br label %bb34

bb37:                                             ; preds = %bb36
  %call_265 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.25)
  br label %bb38

bb34:                                             ; preds = %bb33, %bb32
  %load_266 = load i64, ptr %alloca_count_203, align 8
  %iop_267 = add i64 %load_266, 1
  store i64 %iop_267, ptr %alloca_count_203, align 8
  br label %bb29

bb38:                                             ; preds = %bb37
  ret i32 0
}

define internal void @TestCase__run(ptr %0) {
bb0:
  ret void
}

declare i32 @printf(ptr, ...)

declare i32 @__gxx_personality_v0(...)
