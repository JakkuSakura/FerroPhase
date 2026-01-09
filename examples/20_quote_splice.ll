; ModuleID = '20_quote_splice'
source_filename = "20_quote_splice"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_5 = internal constant [0 x { { ptr, i64 }, ptr }] zeroinitializer, align 8
@.str.20_quote_splice.0 = constant [51 x i8] c"test result: %lld passed; %lld failed; %lld total\0A\00"
@.str.20_quote_splice.1 = constant [13 x i8] c"  %s ... ok\0A\00"
@.str.20_quote_splice.2 = constant [17 x i8] c"  %s ... FAILED\0A\00"
@.str.20_quote_splice.3 = constant [35 x i8] c"\F0\9F\93\98 Tutorial: 20_quote_splice.fp\0A\00"
@.str.20_quote_splice.4 = constant [82 x i8] c"\F0\9F\A7\AD Focus: Quote, splice, and emit: staged code generation with runtime output.\0A\00"
@.str.20_quote_splice.5 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.20_quote_splice.6 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.20_quote_splice.7 = constant [2 x i8] c"\0A\00"
@.str.20_quote_splice.8 = constant [12 x i8] c"result1=%d\0A\00"
@.str.20_quote_splice.9 = constant [12 x i8] c"result2=%d\0A\00"
@.str.20_quote_splice.10 = constant [15 x i8] c"step %lld: %d\0A\00"

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
  store i64 0, ptr %alloca_count_7, align 8
  store i64 0, ptr %alloca_count_6, align 8
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
  %load_46 = load i64, ptr %alloca_count_7, align 8
  %load_47 = load i64, ptr %alloca_count_6, align 8
  %iop_48 = add i64 %load_46, %load_47
  store i64 %iop_48, ptr %alloca_count_45, align 8
  %load_51 = load i64, ptr %alloca_count_45, align 8
  store i64 %load_51, ptr %alloca_count_50, align 8
  %load_53 = load i64, ptr %alloca_count_7, align 8
  %load_54 = load i64, ptr %alloca_count_6, align 8
  %load_55 = load i64, ptr %alloca_count_50, align 8
  %call_56 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.0, i64 %load_53, i64 %load_54, i64 %load_55)
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
  %load_61 = load i64, ptr %alloca_count_7, align 8
  %load_62 = load i64, ptr %alloca_count_6, align 8
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
  %load_72 = load i64, ptr %alloca_count_7, align 8
  %iop_73 = add i64 %load_72, 1
  store i64 %iop_73, ptr %alloca_count_7, align 8
  %load_76 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_77 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.1, { ptr, i64 } %load_76)
  br label %bb10

bb8:                                              ; preds = %bb6
  %load_78 = load i64, ptr %alloca_count_6, align 8
  %iop_79 = add i64 %load_78, 1
  store i64 %iop_79, ptr %alloca_count_6, align 8
  %load_82 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_83 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.2, { ptr, i64 } %load_82)
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

define i32 @main() {
bb0:
  %call_91 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.3)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_92 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.4)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_93 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.5)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_94 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.6)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_95 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.7)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_96 = call i32 @apply_ops__const_1(i32 0, i32 6)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_97 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.8, i32 %call_96)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_98 = call i32 @apply_ops__const_2(i32 10, i32 30)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_99 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.9, i32 %call_98)
  br label %bb9

bb9:                                              ; preds = %bb8
  ret i32 0
}

define internal i32 @apply_ops__const_1(i32 %0, i32 %1) {
bb0:
  %alloca_147 = alloca i1, align 1
  %alloca_count_147 = alloca i1, align 1
  %alloca_134 = alloca i1, align 1
  %alloca_count_134 = alloca i1, align 1
  %alloca_121 = alloca i1, align 1
  %alloca_count_121 = alloca i1, align 1
  %alloca_108 = alloca i1, align 1
  %alloca_count_108 = alloca i1, align 1
  %alloca_101 = alloca i32, align 4
  %alloca_count_101 = alloca i32, align 4
  %alloca_100 = alloca i32, align 4
  %alloca_count_100 = alloca i32, align 4
  store i32 %0, ptr %alloca_count_101, align 4
  %load_103 = load i32, ptr %alloca_count_101, align 4
  %zext = zext i32 %load_103 to i64
  %iop_104 = add i64 %zext, 1
  store i64 %iop_104, ptr %alloca_count_101, align 4
  %load_106 = load i32, ptr %alloca_count_101, align 4
  %call_107 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.10, i64 0, i32 %load_106)
  br label %bb1

bb1:                                              ; preds = %bb0
  %load_109 = load i32, ptr %alloca_count_101, align 4
  %zext1 = zext i32 %load_109 to i64
  %zext2 = zext i32 %1 to i64
  %icmp_110 = icmp sge i64 %zext1, %zext2
  store i1 %icmp_110, ptr %alloca_count_108, align 1
  %load_112 = load i1, ptr %alloca_count_108, align 1
  br i1 %load_112, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_113 = load i32, ptr %alloca_count_101, align 4
  store i32 %load_113, ptr %alloca_count_100, align 4
  %load_115 = load i32, ptr %alloca_count_100, align 4
  ret i32 %load_115

bb3:                                              ; preds = %bb1
  br label %bb4

bb4:                                              ; preds = %bb5, %bb3
  %load_116 = load i32, ptr %alloca_count_101, align 4
  %zext3 = zext i32 %load_116 to i64
  %iop_117 = add i64 %zext3, 2
  store i64 %iop_117, ptr %alloca_count_101, align 4
  %load_119 = load i32, ptr %alloca_count_101, align 4
  %call_120 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.10, i64 1, i32 %load_119)
  br label %bb6

bb6:                                              ; preds = %bb4
  %load_122 = load i32, ptr %alloca_count_101, align 4
  %zext4 = zext i32 %load_122 to i64
  %zext5 = zext i32 %1 to i64
  %icmp_123 = icmp sge i64 %zext4, %zext5
  store i1 %icmp_123, ptr %alloca_count_121, align 1
  %load_125 = load i1, ptr %alloca_count_121, align 1
  br i1 %load_125, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_126 = load i32, ptr %alloca_count_101, align 4
  store i32 %load_126, ptr %alloca_count_100, align 4
  %load_128 = load i32, ptr %alloca_count_100, align 4
  ret i32 %load_128

bb8:                                              ; preds = %bb6
  br label %bb9

bb9:                                              ; preds = %bb10, %bb8
  %load_129 = load i32, ptr %alloca_count_101, align 4
  %zext6 = zext i32 %load_129 to i64
  %iop_130 = add i64 %zext6, 3
  store i64 %iop_130, ptr %alloca_count_101, align 4
  %load_132 = load i32, ptr %alloca_count_101, align 4
  %call_133 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.10, i64 2, i32 %load_132)
  br label %bb11

bb11:                                             ; preds = %bb9
  %load_135 = load i32, ptr %alloca_count_101, align 4
  %zext7 = zext i32 %load_135 to i64
  %zext8 = zext i32 %1 to i64
  %icmp_136 = icmp sge i64 %zext7, %zext8
  store i1 %icmp_136, ptr %alloca_count_134, align 1
  %load_138 = load i1, ptr %alloca_count_134, align 1
  br i1 %load_138, label %bb12, label %bb13

bb12:                                             ; preds = %bb11
  %load_139 = load i32, ptr %alloca_count_101, align 4
  store i32 %load_139, ptr %alloca_count_100, align 4
  %load_141 = load i32, ptr %alloca_count_100, align 4
  ret i32 %load_141

bb13:                                             ; preds = %bb11
  br label %bb14

bb14:                                             ; preds = %bb15, %bb13
  %load_142 = load i32, ptr %alloca_count_101, align 4
  %zext9 = zext i32 %load_142 to i64
  %iop_143 = add i64 %zext9, 4
  store i64 %iop_143, ptr %alloca_count_101, align 4
  %load_145 = load i32, ptr %alloca_count_101, align 4
  %call_146 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.10, i64 3, i32 %load_145)
  br label %bb16

bb16:                                             ; preds = %bb14
  %load_148 = load i32, ptr %alloca_count_101, align 4
  %zext10 = zext i32 %load_148 to i64
  %zext11 = zext i32 %1 to i64
  %icmp_149 = icmp sge i64 %zext10, %zext11
  store i1 %icmp_149, ptr %alloca_count_147, align 1
  %load_151 = load i1, ptr %alloca_count_147, align 1
  br i1 %load_151, label %bb17, label %bb18

bb17:                                             ; preds = %bb16
  %load_152 = load i32, ptr %alloca_count_101, align 4
  store i32 %load_152, ptr %alloca_count_100, align 4
  %load_154 = load i32, ptr %alloca_count_100, align 4
  ret i32 %load_154

bb18:                                             ; preds = %bb16
  br label %bb19

bb19:                                             ; preds = %bb20, %bb18
  %load_155 = load i32, ptr %alloca_count_101, align 4
  store i32 %load_155, ptr %alloca_count_100, align 4
  %load_157 = load i32, ptr %alloca_count_100, align 4
  ret i32 %load_157

bb5:                                              ; No predecessors!
  br label %bb4

bb10:                                             ; No predecessors!
  br label %bb9

bb15:                                             ; No predecessors!
  br label %bb14

bb20:                                             ; No predecessors!
  br label %bb19
}

define internal i32 @apply_ops__const_2(i32 %0, i32 %1) {
bb0:
  %alloca_192 = alloca i1, align 1
  %alloca_count_192 = alloca i1, align 1
  %alloca_179 = alloca i1, align 1
  %alloca_count_179 = alloca i1, align 1
  %alloca_166 = alloca i1, align 1
  %alloca_count_166 = alloca i1, align 1
  %alloca_160 = alloca i32, align 4
  %alloca_count_160 = alloca i32, align 4
  %alloca_158 = alloca i32, align 4
  %alloca_count_158 = alloca i32, align 4
  store i32 %0, ptr %alloca_count_158, align 4
  %load_161 = load i32, ptr %alloca_count_158, align 4
  %zext = zext i32 %load_161 to i64
  %iop_162 = add i64 %zext, 5
  store i64 %iop_162, ptr %alloca_count_158, align 4
  %load_164 = load i32, ptr %alloca_count_158, align 4
  %call_165 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.10, i64 0, i32 %load_164)
  br label %bb1

bb1:                                              ; preds = %bb0
  %load_167 = load i32, ptr %alloca_count_158, align 4
  %zext1 = zext i32 %load_167 to i64
  %zext2 = zext i32 %1 to i64
  %icmp_168 = icmp sge i64 %zext1, %zext2
  store i1 %icmp_168, ptr %alloca_count_166, align 1
  %load_170 = load i1, ptr %alloca_count_166, align 1
  br i1 %load_170, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_171 = load i32, ptr %alloca_count_158, align 4
  store i32 %load_171, ptr %alloca_count_160, align 4
  %load_173 = load i32, ptr %alloca_count_160, align 4
  ret i32 %load_173

bb3:                                              ; preds = %bb1
  br label %bb4

bb4:                                              ; preds = %bb5, %bb3
  %load_174 = load i32, ptr %alloca_count_158, align 4
  %zext3 = zext i32 %load_174 to i64
  %iop_175 = add i64 %zext3, -2
  store i64 %iop_175, ptr %alloca_count_158, align 4
  %load_177 = load i32, ptr %alloca_count_158, align 4
  %call_178 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.10, i64 1, i32 %load_177)
  br label %bb6

bb6:                                              ; preds = %bb4
  %load_180 = load i32, ptr %alloca_count_158, align 4
  %zext4 = zext i32 %load_180 to i64
  %zext5 = zext i32 %1 to i64
  %icmp_181 = icmp sge i64 %zext4, %zext5
  store i1 %icmp_181, ptr %alloca_count_179, align 1
  %load_183 = load i1, ptr %alloca_count_179, align 1
  br i1 %load_183, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_184 = load i32, ptr %alloca_count_158, align 4
  store i32 %load_184, ptr %alloca_count_160, align 4
  %load_186 = load i32, ptr %alloca_count_160, align 4
  ret i32 %load_186

bb8:                                              ; preds = %bb6
  br label %bb9

bb9:                                              ; preds = %bb10, %bb8
  %load_187 = load i32, ptr %alloca_count_158, align 4
  %zext6 = zext i32 %load_187 to i64
  %iop_188 = add i64 %zext6, 7
  store i64 %iop_188, ptr %alloca_count_158, align 4
  %load_190 = load i32, ptr %alloca_count_158, align 4
  %call_191 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.10, i64 2, i32 %load_190)
  br label %bb11

bb11:                                             ; preds = %bb9
  %load_193 = load i32, ptr %alloca_count_158, align 4
  %zext7 = zext i32 %load_193 to i64
  %zext8 = zext i32 %1 to i64
  %icmp_194 = icmp sge i64 %zext7, %zext8
  store i1 %icmp_194, ptr %alloca_count_192, align 1
  %load_196 = load i1, ptr %alloca_count_192, align 1
  br i1 %load_196, label %bb12, label %bb13

bb12:                                             ; preds = %bb11
  %load_197 = load i32, ptr %alloca_count_158, align 4
  store i32 %load_197, ptr %alloca_count_160, align 4
  %load_199 = load i32, ptr %alloca_count_160, align 4
  ret i32 %load_199

bb13:                                             ; preds = %bb11
  br label %bb14

bb14:                                             ; preds = %bb15, %bb13
  %load_200 = load i32, ptr %alloca_count_158, align 4
  store i32 %load_200, ptr %alloca_count_160, align 4
  %load_202 = load i32, ptr %alloca_count_160, align 4
  ret i32 %load_202

bb5:                                              ; No predecessors!
  br label %bb4

bb10:                                             ; No predecessors!
  br label %bb9

bb15:                                             ; No predecessors!
  br label %bb14
}

define internal void @TestCase__run(ptr %0) {
bb0:
  ret void
}

declare i32 @printf(ptr, ...)

declare i32 @__gxx_personality_v0(...)
