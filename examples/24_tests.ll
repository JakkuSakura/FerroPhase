; ModuleID = '24_tests'
source_filename = "24_tests"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_5 = internal constant [2 x { { ptr, i64 }, ptr }] [{ { ptr, i64 }, ptr } { { ptr, i64 } { ptr @.str.24_tests.0, i64 16 }, ptr @adds_two_numbers }, { { ptr, i64 }, ptr } { { ptr, i64 } { ptr @.str.24_tests.1, i64 13 }, ptr @string_concat }], align 8
@.str.24_tests.0 = constant [17 x i8] c"adds_two_numbers\00"
@.str.24_tests.1 = constant [14 x i8] c"string_concat\00"
@.str.24_tests.2 = constant [51 x i8] c"test result: %lld passed; %lld failed; %lld total\0A\00"
@.str.24_tests.3 = constant [13 x i8] c"  %s ... ok\0A\00"
@.str.24_tests.4 = constant [17 x i8] c"  %s ... FAILED\0A\00"
@.str.24_tests.5 = constant [28 x i8] c"\F0\9F\93\98 Tutorial: 24_tests.fp\0A\00"
@.str.24_tests.6 = constant [55 x i8] c"\F0\9F\A7\AD Focus: minimal test harness using assert! macros\0A\00"
@.str.24_tests.7 = constant [67 x i8] c"\F0\9F\A7\AA What to look for: both tests pass with no assertion failures\0A\00"
@.str.24_tests.8 = constant [45 x i8] c"\E2\9C\85 Expectation: test report shows 2 passed\0A\00"
@.str.24_tests.9 = constant [2 x i8] c"\0A\00"
@.str.24_tests.10 = constant [41 x i8] c"Running tests via std::test::run_tests:\0A\00"
@.str.24_tests.11 = constant [47 x i8] c"Summary: %lld passed, %lld failed, %lld total\0A\00"
@.str.24_tests.12 = constant [32 x i8] c"assertion failed: left != right\00"
@.str.24_tests.13 = constant [8 x i8] c"hello, \00"
@.str.24_tests.14 = constant [6 x i8] c"world\00"
@.str.24_tests.15 = constant [13 x i8] c"hello, world\00"

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
  %alloca_92 = alloca i1, align 1
  %alloca_count_92 = alloca i1, align 1
  %alloca_74 = alloca i64, align 8
  %alloca_count_74 = alloca i64, align 8
  %alloca_69 = alloca i64, align 8
  %alloca_count_69 = alloca i64, align 8
  %alloca_59 = alloca { { ptr, i64 }, ptr }, align 8
  %alloca_count_59 = alloca { { ptr, i64 }, ptr }, align 8
  %alloca_56 = alloca i64, align 8
  %alloca_count_56 = alloca i64, align 8
  %alloca_53 = alloca i64, align 8
  %alloca_count_53 = alloca i64, align 8
  %alloca_47 = alloca i1, align 1
  %alloca_count_47 = alloca i1, align 1
  %alloca_45 = alloca i64, align 8
  %alloca_count_45 = alloca i64, align 8
  %alloca_39 = alloca [2 x { { ptr, i64 }, ptr }], align 8
  %alloca_count_39 = alloca [2 x { { ptr, i64 }, ptr }], align 8
  %alloca_33 = alloca [2 x { { ptr, i64 }, ptr }], align 8
  %alloca_count_33 = alloca [2 x { { ptr, i64 }, ptr }], align 8
  %alloca_29 = alloca { { ptr, i64 }, ptr }, align 8
  %alloca_count_29 = alloca { { ptr, i64 }, ptr }, align 8
  %alloca_25 = alloca { { ptr, i64 }, ptr }, align 8
  %alloca_count_25 = alloca { { ptr, i64 }, ptr }, align 8
  %alloca_19 = alloca [2 x { { ptr, i64 }, ptr }], align 8
  %alloca_count_19 = alloca [2 x { { ptr, i64 }, ptr }], align 8
  %alloca_15 = alloca { { ptr, i64 }, ptr }, align 8
  %alloca_count_15 = alloca { { ptr, i64 }, ptr }, align 8
  %alloca_11 = alloca { { ptr, i64 }, ptr }, align 8
  %alloca_count_11 = alloca { { ptr, i64 }, ptr }, align 8
  %alloca_10 = alloca i1, align 1
  %alloca_count_10 = alloca i1, align 1
  %alloca_9 = alloca i64, align 8
  %alloca_count_9 = alloca i64, align 8
  %alloca_8 = alloca i64, align 8
  %alloca_count_8 = alloca i64, align 8
  %alloca_7 = alloca i64, align 8
  %alloca_count_7 = alloca i64, align 8
  %alloca_6 = alloca { i64, i64, i64 }, align 8
  %alloca_count_6 = alloca { i64, i64, i64 }, align 8
  store { { ptr, i64 }, ptr } { { ptr, i64 } { ptr @.str.24_tests.0, i64 16 }, ptr @adds_two_numbers }, ptr %alloca_count_11, align 8
  store { { ptr, i64 }, ptr } { { ptr, i64 } { ptr @.str.24_tests.1, i64 13 }, ptr @string_concat }, ptr %alloca_count_15, align 8
  %load_20 = load { { ptr, i64 }, ptr }, ptr %alloca_count_11, align 8
  %load_21 = load { { ptr, i64 }, ptr }, ptr %alloca_count_15, align 8
  %insertvalue_22 = insertvalue [2 x { { ptr, i64 }, ptr }] undef, { { ptr, i64 }, ptr } %load_20, 0
  %insertvalue_23 = insertvalue [2 x { { ptr, i64 }, ptr }] %insertvalue_22, { { ptr, i64 }, ptr } %load_21, 1
  store [2 x { { ptr, i64 }, ptr }] %insertvalue_23, ptr %alloca_count_19, align 8
  store { { ptr, i64 }, ptr } { { ptr, i64 } { ptr @.str.24_tests.0, i64 16 }, ptr @adds_two_numbers }, ptr %alloca_count_25, align 8
  store { { ptr, i64 }, ptr } { { ptr, i64 } { ptr @.str.24_tests.1, i64 13 }, ptr @string_concat }, ptr %alloca_count_29, align 8
  %load_34 = load { { ptr, i64 }, ptr }, ptr %alloca_count_25, align 8
  %load_35 = load { { ptr, i64 }, ptr }, ptr %alloca_count_29, align 8
  %insertvalue_36 = insertvalue [2 x { { ptr, i64 }, ptr }] undef, { { ptr, i64 }, ptr } %load_34, 0
  %insertvalue_37 = insertvalue [2 x { { ptr, i64 }, ptr }] %insertvalue_36, { { ptr, i64 }, ptr } %load_35, 1
  store [2 x { { ptr, i64 }, ptr }] %insertvalue_37, ptr %alloca_count_33, align 8
  %load_40 = load [2 x { { ptr, i64 }, ptr }], ptr %alloca_count_33, align 8
  store [2 x { { ptr, i64 }, ptr }] %load_40, ptr %alloca_count_39, align 8
  store i64 0, ptr %alloca_count_9, align 8
  store i64 0, ptr %alloca_count_7, align 8
  store i64 0, ptr %alloca_count_8, align 8
  br label %bb1

bb1:                                              ; preds = %bb9, %bb0
  store i64 2, ptr %alloca_count_45, align 8
  %load_48 = load i64, ptr %alloca_count_8, align 8
  %load_49 = load i64, ptr %alloca_count_45, align 8
  %icmp_50 = icmp slt i64 %load_48, %load_49
  store i1 %icmp_50, ptr %alloca_count_47, align 1
  %load_52 = load i1, ptr %alloca_count_47, align 1
  br i1 %load_52, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_54 = load i64, ptr %alloca_count_8, align 8
  store i64 %load_54, ptr %alloca_count_53, align 8
  %load_57 = load i64, ptr %alloca_count_8, align 8
  store i64 %load_57, ptr %alloca_count_56, align 8
  %load_60 = load i64, ptr %alloca_count_56, align 8
  %iop_61 = mul i64 %load_60, 24
  %gep_63 = getelementptr inbounds i8, ptr %alloca_count_39, i64 %iop_61
  %load_65 = load { { ptr, i64 }, ptr }, ptr %gep_63, align 8
  store { { ptr, i64 }, ptr } %load_65, ptr %alloca_count_59, align 8
  %load_67 = load { { ptr, i64 }, ptr }, ptr %alloca_count_59, align 8
  invoke void @__closure0_call({ ptr } undef)
          to label %bb4 unwind label %bb5

bb3:                                              ; preds = %bb1
  %load_70 = load i64, ptr %alloca_count_9, align 8
  %load_71 = load i64, ptr %alloca_count_7, align 8
  %iop_72 = add i64 %load_70, %load_71
  store i64 %iop_72, ptr %alloca_count_69, align 8
  %load_75 = load i64, ptr %alloca_count_69, align 8
  store i64 %load_75, ptr %alloca_count_74, align 8
  %load_77 = load i64, ptr %alloca_count_9, align 8
  %load_78 = load i64, ptr %alloca_count_7, align 8
  %load_79 = load i64, ptr %alloca_count_74, align 8
  %call_80 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.2, i64 %load_77, i64 %load_78, i64 %load_79)
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
  %load_84 = load i64, ptr %alloca_count_74, align 8
  %load_85 = load i64, ptr %alloca_count_9, align 8
  %load_86 = load i64, ptr %alloca_count_7, align 8
  %insertvalue_87 = insertvalue { i64, i64, i64 } undef, i64 %load_84, 0
  %insertvalue_88 = insertvalue { i64, i64, i64 } %insertvalue_87, i64 %load_85, 1
  %insertvalue_89 = insertvalue { i64, i64, i64 } %insertvalue_88, i64 %load_86, 2
  store { i64, i64, i64 } %insertvalue_89, ptr %alloca_count_6, align 8
  %load_91 = load { i64, i64, i64 }, ptr %alloca_count_6, align 8
  ret { i64, i64, i64 } %load_91

bb6:                                              ; preds = %bb5, %bb4
  %load_93 = load i1, ptr %alloca_count_10, align 1
  store i1 %load_93, ptr %alloca_count_92, align 1
  %load_95 = load i1, ptr %alloca_count_92, align 1
  br i1 %load_95, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_96 = load i64, ptr %alloca_count_9, align 8
  %iop_97 = add i64 %load_96, 1
  store i64 %iop_97, ptr %alloca_count_9, align 8
  %load_100 = load { ptr, i64 }, ptr %alloca_count_59, align 8
  %call_101 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.3, { ptr, i64 } %load_100)
  br label %bb10

bb8:                                              ; preds = %bb6
  %load_102 = load i64, ptr %alloca_count_7, align 8
  %iop_103 = add i64 %load_102, 1
  store i64 %iop_103, ptr %alloca_count_7, align 8
  %load_106 = load { ptr, i64 }, ptr %alloca_count_59, align 8
  %call_107 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.4, { ptr, i64 } %load_106)
  br label %bb11

bb10:                                             ; preds = %bb7
  br label %bb9

bb11:                                             ; preds = %bb8
  br label %bb9

bb9:                                              ; preds = %bb11, %bb10
  %load_108 = load i64, ptr %alloca_count_8, align 8
  %iop_109 = add i64 %load_108, 1
  store i64 %iop_109, ptr %alloca_count_8, align 8
  br label %bb1
}

define internal { i64, i64, i64 } @std__test__run() {
bb0:
  %alloca_111 = alloca { i64, i64, i64 }, align 8
  %alloca_count_111 = alloca { i64, i64, i64 }, align 8
  %call_112 = call { i64, i64, i64 } @std__test__run_tests()
  store { i64, i64, i64 } %call_112, ptr %alloca_count_111, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %load_114 = load { i64, i64, i64 }, ptr %alloca_count_111, align 8
  ret { i64, i64, i64 } %load_114
}

define i32 @main() {
bb0:
  %alloca_134 = alloca { i64, i64, i64 }, align 8
  %alloca_count_134 = alloca { i64, i64, i64 }, align 8
  %alloca_128 = alloca { i64, i64, i64 }, align 8
  %alloca_count_128 = alloca { i64, i64, i64 }, align 8
  %alloca_122 = alloca { i64, i64, i64 }, align 8
  %alloca_count_122 = alloca { i64, i64, i64 }, align 8
  %call_115 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.5)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_116 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.6)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_117 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.7)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_118 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.8)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_119 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.9)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_120 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.10)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_121 = call { i64, i64, i64 } @std__test__run_tests()
  br label %bb7

bb7:                                              ; preds = %bb6
  store { i64, i64, i64 } %call_121, ptr %alloca_count_122, align 8
  %gep_125 = getelementptr inbounds i8, ptr %alloca_count_122, i64 8
  %load_127 = load i64, ptr %gep_125, align 8
  store { i64, i64, i64 } %call_121, ptr %alloca_count_128, align 8
  %gep_131 = getelementptr inbounds i8, ptr %alloca_count_128, i64 16
  %load_133 = load i64, ptr %gep_131, align 8
  store { i64, i64, i64 } %call_121, ptr %alloca_count_134, align 8
  %load_137 = load i64, ptr %alloca_count_134, align 8
  %call_138 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.11, i64 %load_127, i64 %load_133, i64 %load_137)
  br label %bb8

bb8:                                              ; preds = %bb7
  ret i32 0
}

define internal void @adds_two_numbers() {
bb0:
  %alloca_158 = alloca i1, align 1
  %alloca_count_158 = alloca i1, align 1
  %alloca_153 = alloca i1, align 1
  %alloca_count_153 = alloca i1, align 1
  %alloca_150 = alloca i64, align 8
  %alloca_count_150 = alloca i64, align 8
  %alloca_147 = alloca i64, align 8
  %alloca_count_147 = alloca i64, align 8
  %alloca_145 = alloca i64, align 8
  %alloca_count_145 = alloca i64, align 8
  %alloca_142 = alloca i64, align 8
  %alloca_count_142 = alloca i64, align 8
  %alloca_139 = alloca i64, align 8
  %alloca_count_139 = alloca i64, align 8
  store i64 3, ptr %alloca_count_139, align 8
  %load_143 = load i64, ptr %alloca_count_139, align 8
  store i64 %load_143, ptr %alloca_count_142, align 8
  store i64 3, ptr %alloca_count_145, align 8
  %load_148 = load i64, ptr %alloca_count_142, align 8
  store i64 %load_148, ptr %alloca_count_147, align 8
  %load_151 = load i64, ptr %alloca_count_145, align 8
  store i64 %load_151, ptr %alloca_count_150, align 8
  %load_154 = load i64, ptr %alloca_count_147, align 8
  %load_155 = load i64, ptr %alloca_count_150, align 8
  %icmp_156 = icmp eq i64 %load_154, %load_155
  store i1 %icmp_156, ptr %alloca_count_153, align 1
  %load_159 = load i1, ptr %alloca_count_153, align 1
  %zext = zext i1 %load_159 to i64
  %not_160 = xor i64 %zext, -1
  store i64 %not_160, ptr %alloca_count_158, align 1
  %load_162 = load i1, ptr %alloca_count_158, align 1
  br i1 %load_162, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  call void @fp_panic(ptr @.str.24_tests.12)
  br label %bb4

bb2:                                              ; preds = %bb0
  br label %bb3

bb4:                                              ; preds = %bb1
  unreachable

bb3:                                              ; preds = %bb5, %bb2
  ret void

bb5:                                              ; No predecessors!
  br label %bb3
}

define internal void @string_concat() {
bb0:
  %alloca_180 = alloca i1, align 1
  %alloca_count_180 = alloca i1, align 1
  %alloca_175 = alloca i1, align 1
  %alloca_count_175 = alloca i1, align 1
  %alloca_173 = alloca ptr, align 8
  %alloca_count_173 = alloca ptr, align 8
  %alloca_170 = alloca ptr, align 8
  %alloca_count_170 = alloca ptr, align 8
  %alloca_167 = alloca ptr, align 8
  %alloca_count_167 = alloca ptr, align 8
  %alloca_164 = alloca ptr, align 8
  %alloca_count_164 = alloca ptr, align 8
  store i64 add (i64 ptrtoint (ptr @.str.24_tests.13 to i64), i64 ptrtoint (ptr @.str.24_tests.14 to i64)), ptr %alloca_count_164, align 8
  %load_168 = load ptr, ptr %alloca_count_164, align 8
  store ptr %load_168, ptr %alloca_count_167, align 8
  %load_171 = load ptr, ptr %alloca_count_167, align 8
  store ptr %load_171, ptr %alloca_count_170, align 8
  store ptr @.str.24_tests.15, ptr %alloca_count_173, align 8
  %load_176 = load ptr, ptr %alloca_count_170, align 8
  %load_177 = load ptr, ptr %alloca_count_173, align 8
  %ptrtoint = ptrtoint ptr %load_176 to i64
  %ptrtoint1 = ptrtoint ptr %load_177 to i64
  %icmp_178 = icmp eq i64 %ptrtoint, %ptrtoint1
  store i1 %icmp_178, ptr %alloca_count_175, align 1
  %load_181 = load i1, ptr %alloca_count_175, align 1
  %zext = zext i1 %load_181 to i64
  %not_182 = xor i64 %zext, -1
  store i64 %not_182, ptr %alloca_count_180, align 1
  %load_184 = load i1, ptr %alloca_count_180, align 1
  br i1 %load_184, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  call void @fp_panic(ptr @.str.24_tests.12)
  br label %bb4

bb2:                                              ; preds = %bb0
  br label %bb3

bb4:                                              ; preds = %bb1
  unreachable

bb3:                                              ; preds = %bb5, %bb2
  ret void

bb5:                                              ; No predecessors!
  br label %bb3
}

define internal void @TestCase__run(ptr %0) {
bb0:
  ret void
}

declare i32 @printf(ptr, ...)

declare i32 @__gxx_personality_v0(...)

declare void @fp_panic(ptr)
