; ModuleID = '02_string_processing'
source_filename = "02_string_processing"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_5 = internal constant [0 x { { ptr, i64 }, ptr }] zeroinitializer, align 8
@.str.02_string_processing.0 = constant [51 x i8] c"test result: %lld passed; %lld failed; %lld total\0A\00"
@.str.02_string_processing.1 = constant [13 x i8] c"  %s ... ok\0A\00"
@.str.02_string_processing.2 = constant [17 x i8] c"  %s ... FAILED\0A\00"
@.str.02_string_processing.3 = constant [40 x i8] c"\F0\9F\93\98 Tutorial: 02_string_processing.fp\0A\00"
@.str.02_string_processing.4 = constant [59 x i8] c"\F0\9F\A7\AD Focus: Compile-time string operations and intrinsics\0A\00"
@.str.02_string_processing.5 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.02_string_processing.6 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.02_string_processing.7 = constant [2 x i8] c"\0A\00"
@.str.02_string_processing.8 = constant [11 x i8] c"FerroPhase\00"
@.str.02_string_processing.9 = constant [20 x i8] c"name='%s' len=%llu\0A\00"
@.str.02_string_processing.10 = constant [6 x i8] c"0.1.0\00"
@.str.02_string_processing.11 = constant [23 x i8] c"version='%s' len=%llu\0A\00"
@.str.02_string_processing.12 = constant [47 x i8] c"prefix_ok=%d, suffix_ok=%d, contains_phase=%d\0A\00"
@.str.02_string_processing.13 = constant [6 x i8] c"Ferro\00"
@.str.02_string_processing.14 = constant [6 x i8] c"Phase\00"
@.str.02_string_processing.15 = constant [30 x i8] c"slices: short='%s' tail='%s'\0A\00"
@.str.02_string_processing.16 = constant [8 x i8] c"words:\0A\00"
@.str.02_string_processing.17 = constant [6 x i8] c"alpha\00"
@.str.02_string_processing.18 = constant [5 x i8] c"beta\00"
@.str.02_string_processing.19 = constant [6 x i8] c"gamma\00"
@.str.02_string_processing.20 = constant [6 x i8] c"delta\00"
@.str.02_string_processing.21 = constant [18 x i8] c"  %s -> len=%llu\0A\00"
@.str.02_string_processing.22 = constant [24 x i8] c"total word length=%llu\0A\00"
@.str.02_string_processing.23 = constant [19 x i8] c"empty=%d, long=%d\0A\00"
@.str.02_string_processing.24 = constant [18 x i8] c"FerroPhase v0.1.0\00"
@.str.02_string_processing.25 = constant [13 x i8] c"banner='%s'\0A\00"
@.str.02_string_processing.26 = constant [18 x i8] c"buffer_size=%llu\0A\00"

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
  %alloca_10 = alloca i1, align 1
  %alloca_count_10 = alloca i1, align 1
  %alloca_9 = alloca i64, align 8
  %alloca_count_9 = alloca i64, align 8
  %alloca_8 = alloca i64, align 8
  %alloca_count_8 = alloca i64, align 8
  %alloca_7 = alloca { i64, i64, i64 }, align 8
  %alloca_count_7 = alloca { i64, i64, i64 }, align 8
  %alloca_6 = alloca i64, align 8
  %alloca_count_6 = alloca i64, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_11, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_13, align 8
  %load_16 = load [0 x { { ptr, i64 }, ptr }], ptr %alloca_count_13, align 8
  store [0 x { { ptr, i64 }, ptr }] %load_16, ptr %alloca_count_15, align 8
  store i64 0, ptr %alloca_count_8, align 8
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
  %load_46 = load i64, ptr %alloca_count_8, align 8
  %load_47 = load i64, ptr %alloca_count_6, align 8
  %iop_48 = add i64 %load_46, %load_47
  store i64 %iop_48, ptr %alloca_count_45, align 8
  %load_51 = load i64, ptr %alloca_count_45, align 8
  store i64 %load_51, ptr %alloca_count_50, align 8
  %load_53 = load i64, ptr %alloca_count_8, align 8
  %load_54 = load i64, ptr %alloca_count_6, align 8
  %load_55 = load i64, ptr %alloca_count_50, align 8
  %call_56 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.0, i64 %load_53, i64 %load_54, i64 %load_55)
  br label %bb12

bb4:                                              ; preds = %bb2
  store i1 true, ptr %alloca_count_10, align 1
  br label %bb6

bb5:                                              ; preds = %bb2
  %landingpad = landingpad { ptr, i32 }
          catch ptr null
  store i1 false, ptr %alloca_count_10, align 1
  br label %bb6

bb12:                                             ; preds = %bb3
  %load_60 = load i64, ptr %alloca_count_50, align 8
  %load_61 = load i64, ptr %alloca_count_8, align 8
  %load_62 = load i64, ptr %alloca_count_6, align 8
  %insertvalue_63 = insertvalue { i64, i64, i64 } undef, i64 %load_60, 0
  %insertvalue_64 = insertvalue { i64, i64, i64 } %insertvalue_63, i64 %load_61, 1
  %insertvalue_65 = insertvalue { i64, i64, i64 } %insertvalue_64, i64 %load_62, 2
  store { i64, i64, i64 } %insertvalue_65, ptr %alloca_count_7, align 8
  %load_67 = load { i64, i64, i64 }, ptr %alloca_count_7, align 8
  ret { i64, i64, i64 } %load_67

bb6:                                              ; preds = %bb5, %bb4
  %load_69 = load i1, ptr %alloca_count_10, align 1
  store i1 %load_69, ptr %alloca_count_68, align 1
  %load_71 = load i1, ptr %alloca_count_68, align 1
  br i1 %load_71, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_72 = load i64, ptr %alloca_count_8, align 8
  %iop_73 = add i64 %load_72, 1
  store i64 %iop_73, ptr %alloca_count_8, align 8
  %load_76 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_77 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.1, { ptr, i64 } %load_76)
  br label %bb10

bb8:                                              ; preds = %bb6
  %load_78 = load i64, ptr %alloca_count_6, align 8
  %iop_79 = add i64 %load_78, 1
  store i64 %iop_79, ptr %alloca_count_6, align 8
  %load_82 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_83 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.2, { ptr, i64 } %load_82)
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

define i32 @main() {
bb0:
  %alloca_181 = alloca i64, align 8
  %alloca_count_181 = alloca i64, align 8
  %alloca_177 = alloca ptr, align 8
  %alloca_count_177 = alloca ptr, align 8
  %alloca_170 = alloca i1, align 1
  %alloca_count_170 = alloca i1, align 1
  %alloca_168 = alloca i1, align 1
  %alloca_count_168 = alloca i1, align 1
  %alloca_164 = alloca i64, align 8
  %alloca_count_164 = alloca i64, align 8
  %alloca_145 = alloca i64, align 8
  %alloca_count_145 = alloca i64, align 8
  %alloca_143 = alloca [4 x i64], align 8
  %alloca_count_143 = alloca [4 x i64], align 8
  %alloca_140 = alloca i64, align 8
  %alloca_count_140 = alloca i64, align 8
  %alloca_138 = alloca [4 x ptr], align 8
  %alloca_count_138 = alloca [4 x ptr], align 8
  %alloca_133 = alloca i1, align 1
  %alloca_count_133 = alloca i1, align 1
  %alloca_126 = alloca ptr, align 8
  %alloca_count_126 = alloca ptr, align 8
  %alloca_124 = alloca ptr, align 8
  %alloca_count_124 = alloca ptr, align 8
  %alloca_115 = alloca i1, align 1
  %alloca_count_115 = alloca i1, align 1
  %alloca_113 = alloca i1, align 1
  %alloca_count_113 = alloca i1, align 1
  %alloca_111 = alloca i1, align 1
  %alloca_count_111 = alloca i1, align 1
  %alloca_106 = alloca i64, align 8
  %alloca_count_106 = alloca i64, align 8
  %alloca_104 = alloca ptr, align 8
  %alloca_count_104 = alloca ptr, align 8
  %alloca_99 = alloca i64, align 8
  %alloca_count_99 = alloca i64, align 8
  %alloca_97 = alloca ptr, align 8
  %alloca_count_97 = alloca ptr, align 8
  %alloca_91 = alloca i64, align 8
  %alloca_count_91 = alloca i64, align 8
  %call_92 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.3)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_93 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.4)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_94 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.5)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_95 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.6)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_96 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.7)
  br label %bb5

bb5:                                              ; preds = %bb4
  store ptr @.str.02_string_processing.8, ptr %alloca_count_97, align 8
  store i64 10, ptr %alloca_count_99, align 8
  %load_101 = load ptr, ptr %alloca_count_97, align 8
  %load_102 = load i64, ptr %alloca_count_99, align 8
  %call_103 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.9, ptr %load_101, i64 %load_102)
  br label %bb6

bb6:                                              ; preds = %bb5
  store ptr @.str.02_string_processing.10, ptr %alloca_count_104, align 8
  store i64 5, ptr %alloca_count_106, align 8
  %load_108 = load ptr, ptr %alloca_count_104, align 8
  %load_109 = load i64, ptr %alloca_count_106, align 8
  %call_110 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.11, ptr %load_108, i64 %load_109)
  br label %bb7

bb7:                                              ; preds = %bb6
  store i1 true, ptr %alloca_count_111, align 1
  store i1 true, ptr %alloca_count_113, align 1
  store i1 true, ptr %alloca_count_115, align 1
  %load_117 = load i1, ptr %alloca_count_111, align 1
  %zext = zext i1 %load_117 to i32
  %load_119 = load i1, ptr %alloca_count_113, align 1
  %zext1 = zext i1 %load_119 to i32
  %load_121 = load i1, ptr %alloca_count_115, align 1
  %zext2 = zext i1 %load_121 to i32
  %call_123 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.12, i32 %zext, i32 %zext1, i32 %zext2)
  br label %bb8

bb8:                                              ; preds = %bb7
  store ptr @.str.02_string_processing.13, ptr %alloca_count_124, align 8
  store ptr @.str.02_string_processing.14, ptr %alloca_count_126, align 8
  %load_128 = load ptr, ptr %alloca_count_124, align 8
  %load_129 = load ptr, ptr %alloca_count_126, align 8
  %call_130 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.15, ptr %load_128, ptr %load_129)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_131 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.16)
  br label %bb10

bb10:                                             ; preds = %bb9
  store i64 0, ptr %alloca_count_91, align 8
  br label %bb11

bb11:                                             ; preds = %bb12, %bb10
  %load_134 = load i64, ptr %alloca_count_91, align 8
  %icmp_135 = icmp slt i64 %load_134, 4
  store i1 %icmp_135, ptr %alloca_count_133, align 1
  %load_137 = load i1, ptr %alloca_count_133, align 1
  br i1 %load_137, label %bb12, label %bb13

bb12:                                             ; preds = %bb11
  store [4 x ptr] [ptr @.str.02_string_processing.17, ptr @.str.02_string_processing.18, ptr @.str.02_string_processing.19, ptr @.str.02_string_processing.20], ptr %alloca_count_138, align 8
  %load_141 = load i64, ptr %alloca_count_91, align 8
  store i64 %load_141, ptr %alloca_count_140, align 8
  store [4 x i64] [i64 5, i64 4, i64 5, i64 5], ptr %alloca_count_143, align 8
  %load_146 = load i64, ptr %alloca_count_91, align 8
  store i64 %load_146, ptr %alloca_count_145, align 8
  %load_148 = load i64, ptr %alloca_count_140, align 8
  %iop_149 = mul i64 %load_148, 8
  %gep_151 = getelementptr inbounds i8, ptr %alloca_count_138, i64 %iop_149
  %load_153 = load ptr, ptr %gep_151, align 8
  %load_154 = load i64, ptr %alloca_count_145, align 8
  %iop_155 = mul i64 %load_154, 8
  %gep_157 = getelementptr inbounds i8, ptr %alloca_count_143, i64 %iop_155
  %load_159 = load i64, ptr %gep_157, align 8
  %call_160 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.21, ptr %load_153, i64 %load_159)
  %load_161 = load i64, ptr %alloca_count_91, align 8
  %iop_162 = add i64 %load_161, 1
  store i64 %iop_162, ptr %alloca_count_91, align 8
  br label %bb11

bb13:                                             ; preds = %bb11
  store i64 19, ptr %alloca_count_164, align 8
  %load_166 = load i64, ptr %alloca_count_164, align 8
  %call_167 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.22, i64 %load_166)
  br label %bb14

bb14:                                             ; preds = %bb13
  store i1 false, ptr %alloca_count_168, align 1
  store i1 true, ptr %alloca_count_170, align 1
  %load_172 = load i1, ptr %alloca_count_168, align 1
  %zext3 = zext i1 %load_172 to i32
  %load_174 = load i1, ptr %alloca_count_170, align 1
  %zext4 = zext i1 %load_174 to i32
  %call_176 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.23, i32 %zext3, i32 %zext4)
  br label %bb15

bb15:                                             ; preds = %bb14
  store ptr @.str.02_string_processing.24, ptr %alloca_count_177, align 8
  %load_179 = load ptr, ptr %alloca_count_177, align 8
  %call_180 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.25, ptr %load_179)
  br label %bb16

bb16:                                             ; preds = %bb15
  store i64 256, ptr %alloca_count_181, align 8
  %load_183 = load i64, ptr %alloca_count_181, align 8
  %call_184 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.26, i64 %load_183)
  br label %bb17

bb17:                                             ; preds = %bb16
  ret i32 0
}

define internal void @TestCase__run(ptr %0) {
bb0:
  ret void
}

declare i32 @printf(ptr, ...)

declare i32 @__gxx_personality_v0(...)
