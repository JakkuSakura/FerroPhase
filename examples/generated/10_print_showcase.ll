; ModuleID = '10_print_showcase'
source_filename = "10_print_showcase"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_5 = internal constant [0 x { { ptr, i64 }, ptr }] zeroinitializer, align 8
@.str.10_print_showcase.0 = constant [51 x i8] c"test result: %lld passed; %lld failed; %lld total\0A\00"
@.str.10_print_showcase.1 = constant [13 x i8] c"  %s ... ok\0A\00"
@.str.10_print_showcase.2 = constant [17 x i8] c"  %s ... FAILED\0A\00"
@.str.10_print_showcase.3 = constant [37 x i8] c"\F0\9F\93\98 Tutorial: 10_print_showcase.fp\0A\00"
@.str.10_print_showcase.4 = constant [102 x i8] c"\F0\9F\A7\AD Focus: Comprehensive println!/print showcase covering variadic arguments and runtime formatting\0A\00"
@.str.10_print_showcase.5 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.10_print_showcase.6 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.10_print_showcase.7 = constant [2 x i8] c"\0A\00"
@.str.10_print_showcase.8 = constant [6 x i8] c"Hello\00"
@.str.10_print_showcase.9 = constant [20 x i8] c"World with newlines\00"
@.str.10_print_showcase.10 = constant [13 x i8] c"Number: %lld\00"
@.str.10_print_showcase.11 = constant [15 x i8] c"Boolean: %d %d\00"
@.str.10_print_showcase.12 = constant [21 x i8] c"Mixed: %lld %f %s %d\00"
@.str.10_print_showcase.13 = constant [5 x i8] c"text\00"
@.str.10_print_showcase.14 = constant [18 x i8] c"Namespace test %s\00"
@.str.10_print_showcase.15 = constant [12 x i8] c"still works\00"
@.str.10_print_showcase.16 = constant [14 x i8] c"value = %lld\0A\00"
@.str.10_print_showcase.17 = constant [26 x i8] c"math: %lld + %lld = %lld\0A\00"
@.str.10_print_showcase.18 = constant [11 x i8] c"float: %f\0A\00"
@.str.10_print_showcase.19 = constant [14 x i8] c"chars: %s %s\0A\00"
@.str.10_print_showcase.20 = constant [3 x i8] c"()\00"
@.str.10_print_showcase.21 = constant [21 x i8] c"tuple: (%lld, %lld)\0A\00"
@.str.10_print_showcase.22 = constant [14 x i8] c"bools: %d %d\0A\00"
@.str.10_print_showcase.23 = constant [17 x i8] c"This %s %s %s %s\00"
@.str.10_print_showcase.24 = constant [6 x i8] c"stays\00"
@.str.10_print_showcase.25 = constant [3 x i8] c"on\00"
@.str.10_print_showcase.26 = constant [4 x i8] c"one\00"
@.str.10_print_showcase.27 = constant [5 x i8] c"line\00"
@.str.10_print_showcase.28 = constant [27 x i8] c"Continuing without newline\00"
@.str.10_print_showcase.29 = constant [20 x i8] c" - appended content\00"
@.str.10_print_showcase.30 = constant [9 x i8] c"Unit: %s\00"
@.str.10_print_showcase.31 = constant [9 x i8] c"Null: %s\00"
@.str.10_print_showcase.32 = constant [5 x i8] c"null\00"
@.str.10_print_showcase.33 = constant [16 x i8] c"escaped: %s %s\0A\00"
@.str.10_print_showcase.34 = constant [12 x i8] c"line1\0Aline2\00"
@.str.10_print_showcase.35 = constant [8 x i8] c"tab\09end\00"

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
  %call_56 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.0, i64 %load_53, i64 %load_54, i64 %load_55)
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
  %call_77 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.1, { ptr, i64 } %load_76)
  br label %bb10

bb8:                                              ; preds = %bb6
  %load_78 = load i64, ptr %alloca_count_6, align 8
  %iop_79 = add i64 %load_78, 1
  store i64 %iop_79, ptr %alloca_count_6, align 8
  %load_82 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_83 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.2, { ptr, i64 } %load_82)
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
  %alloca_108 = alloca i64, align 8
  %alloca_count_108 = alloca i64, align 8
  %call_91 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.3)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_92 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.4)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_93 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.5)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_94 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.6)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_95 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.7)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_96 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.8)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_97 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.9)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_98 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.7)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_99 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.10, i64 42)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_102 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.11, i32 1, i32 0)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_104 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.12, i64 1, double 2.500000e+00, ptr @.str.10_print_showcase.13, i32 1)
  br label %bb11

bb11:                                             ; preds = %bb10
  %call_105 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.7)
  br label %bb12

bb12:                                             ; preds = %bb11
  %call_106 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.14, ptr @.str.10_print_showcase.15)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_107 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.7)
  br label %bb14

bb14:                                             ; preds = %bb13
  store i64 7, ptr %alloca_count_108, align 8
  %load_110 = load i64, ptr %alloca_count_108, align 8
  %call_111 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.16, i64 %load_110)
  br label %bb15

bb15:                                             ; preds = %bb14
  %call_112 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.17, i64 2, i64 3, i64 5)
  br label %bb16

bb16:                                             ; preds = %bb15
  %call_113 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.18, double 3.141590e+00)
  br label %bb17

bb17:                                             ; preds = %bb16
  %call_114 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.19, ptr @.str.10_print_showcase.20, ptr @.str.10_print_showcase.20)
  %call_115 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.21, i64 1, i64 2)
  br label %bb18

bb18:                                             ; preds = %bb17
  %call_118 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.22, i32 1, i32 0)
  br label %bb19

bb19:                                             ; preds = %bb18
  %call_119 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.23, ptr @.str.10_print_showcase.24, ptr @.str.10_print_showcase.25, ptr @.str.10_print_showcase.26, ptr @.str.10_print_showcase.27)
  br label %bb20

bb20:                                             ; preds = %bb19
  %call_120 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.7)
  br label %bb21

bb21:                                             ; preds = %bb20
  %call_121 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.28)
  br label %bb22

bb22:                                             ; preds = %bb21
  %call_122 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.29)
  br label %bb23

bb23:                                             ; preds = %bb22
  %call_123 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.7)
  br label %bb24

bb24:                                             ; preds = %bb23
  %call_124 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.30, ptr @.str.10_print_showcase.20)
  %call_125 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.31, ptr @.str.10_print_showcase.32)
  %call_126 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.7)
  br label %bb25

bb25:                                             ; preds = %bb24
  %call_127 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.33, ptr @.str.10_print_showcase.34, ptr @.str.10_print_showcase.35)
  br label %bb26

bb26:                                             ; preds = %bb25
  ret i32 0
}

define internal void @TestCase__run(ptr %0) {
bb0:
  ret void
}

declare i32 @printf(ptr, ...)

declare i32 @__gxx_personality_v0(...)
