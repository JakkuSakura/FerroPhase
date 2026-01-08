; ModuleID = '09_higher_order_functions'
source_filename = "09_higher_order_functions"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_17 = internal constant [0 x { { ptr, i64 }, ptr }] zeroinitializer, align 8
@.str.09_higher_order_functions.0 = constant [51 x i8] c"test result: %lld passed; %lld failed; %lld total\0A\00"
@.str.09_higher_order_functions.1 = constant [13 x i8] c"  %s ... ok\0A\00"
@.str.09_higher_order_functions.2 = constant [17 x i8] c"  %s ... FAILED\0A\00"
@.str.09_higher_order_functions.3 = constant [45 x i8] c"\F0\9F\93\98 Tutorial: 09_higher_order_functions.fp\0A\00"
@.str.09_higher_order_functions.4 = constant [81 x i8] c"\F0\9F\A7\AD Focus: Higher-order functions: passing functions as arguments and closures\0A\00"
@.str.09_higher_order_functions.5 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.09_higher_order_functions.6 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.09_higher_order_functions.7 = constant [2 x i8] c"\0A\00"
@.str.09_higher_order_functions.8 = constant [21 x i8] c"Generic operations:\0A\00"
@.str.09_higher_order_functions.9 = constant [15 x i8] c"\0AConditional:\0A\00"
@.str.09_higher_order_functions.10 = constant [29 x i8] c"apply_if(true, 5, 3) = %lld\0A\00"
@.str.09_higher_order_functions.11 = constant [30 x i8] c"apply_if(false, 5, 3) = %lld\0A\00"
@.str.09_higher_order_functions.12 = constant [19 x i8] c"\0AClosure factory:\0A\00"
@.str.09_higher_order_functions.13 = constant [18 x i8] c"add_10(5) = %lld\0A\00"
@.str.09_higher_order_functions.14 = constant [18 x i8] c"double(7) = %lld\0A\00"
@.str.09_higher_order_functions.15 = constant [26 x i8] c"apply(%lld, %lld) = %lld\0A\00"
@.str.09_higher_order_functions.16 = constant [20 x i8] c"apply(%f, %f) = %f\0A\00"

define internal i64 @__closure0_call({ i64 } %0, i64 %1) {
bb0:
  %alloca_1 = alloca { i64 }, align 8
  %alloca_count_1 = alloca { i64 }, align 8
  %alloca_0 = alloca i64, align 8
  %alloca_count_0 = alloca i64, align 8
  store { i64 } %0, ptr %alloca_count_1, align 8
  %load_4 = load i64, ptr %alloca_count_1, align 8
  %iop_5 = add i64 %1, %load_4
  store i64 %iop_5, ptr %alloca_count_0, align 8
  %load_7 = load i64, ptr %alloca_count_0, align 8
  ret i64 %load_7
}

define internal void @__closure1_call({ ptr } %0) {
bb0:
  %alloca_8 = alloca ptr, align 8
  %alloca_count_8 = alloca ptr, align 8
  store { ptr } %0, ptr %alloca_count_8, align 8
  %load_11 = load ptr, ptr %alloca_count_8, align 8
  call void @TestCase__run(ptr %load_11)
  br label %bb1

bb1:                                              ; preds = %bb0
  ret void
}

define internal i64 @__closure2_call({ i8 } %0, i64 %1) {
bb0:
  %alloca_13 = alloca i64, align 8
  %alloca_count_13 = alloca i64, align 8
  %iop_14 = mul i64 %1, 2
  store i64 %iop_14, ptr %alloca_count_13, align 8
  %load_16 = load i64, ptr %alloca_count_13, align 8
  ret i64 %load_16
}

define internal { i64, i64, i64 } @std__test__run_tests() personality ptr @__gxx_personality_v0 {
bb0:
  %alloca_80 = alloca i1, align 1
  %alloca_count_80 = alloca i1, align 1
  %alloca_62 = alloca i64, align 8
  %alloca_count_62 = alloca i64, align 8
  %alloca_57 = alloca i64, align 8
  %alloca_count_57 = alloca i64, align 8
  %alloca_47 = alloca { { ptr, i64 }, ptr }, align 8
  %alloca_count_47 = alloca { { ptr, i64 }, ptr }, align 8
  %alloca_44 = alloca i64, align 8
  %alloca_count_44 = alloca i64, align 8
  %alloca_41 = alloca i64, align 8
  %alloca_count_41 = alloca i64, align 8
  %alloca_35 = alloca i1, align 1
  %alloca_count_35 = alloca i1, align 1
  %alloca_33 = alloca i64, align 8
  %alloca_count_33 = alloca i64, align 8
  %alloca_27 = alloca [0 x { { ptr, i64 }, ptr }], align 8
  %alloca_count_27 = alloca [0 x { { ptr, i64 }, ptr }], align 8
  %alloca_25 = alloca [0 x { { ptr, i64 }, ptr }], align 8
  %alloca_count_25 = alloca [0 x { { ptr, i64 }, ptr }], align 8
  %alloca_23 = alloca [0 x { { ptr, i64 }, ptr }], align 8
  %alloca_count_23 = alloca [0 x { { ptr, i64 }, ptr }], align 8
  %alloca_22 = alloca i64, align 8
  %alloca_count_22 = alloca i64, align 8
  %alloca_21 = alloca i1, align 1
  %alloca_count_21 = alloca i1, align 1
  %alloca_20 = alloca i64, align 8
  %alloca_count_20 = alloca i64, align 8
  %alloca_19 = alloca { i64, i64, i64 }, align 8
  %alloca_count_19 = alloca { i64, i64, i64 }, align 8
  %alloca_18 = alloca i64, align 8
  %alloca_count_18 = alloca i64, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_23, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_25, align 8
  %load_28 = load [0 x { { ptr, i64 }, ptr }], ptr %alloca_count_25, align 8
  store [0 x { { ptr, i64 }, ptr }] %load_28, ptr %alloca_count_27, align 8
  store i64 0, ptr %alloca_count_22, align 8
  store i64 0, ptr %alloca_count_20, align 8
  store i64 0, ptr %alloca_count_18, align 8
  br label %bb1

bb1:                                              ; preds = %bb9, %bb0
  store i64 0, ptr %alloca_count_33, align 8
  %load_36 = load i64, ptr %alloca_count_18, align 8
  %load_37 = load i64, ptr %alloca_count_33, align 8
  %icmp_38 = icmp slt i64 %load_36, %load_37
  store i1 %icmp_38, ptr %alloca_count_35, align 1
  %load_40 = load i1, ptr %alloca_count_35, align 1
  br i1 %load_40, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_42 = load i64, ptr %alloca_count_18, align 8
  store i64 %load_42, ptr %alloca_count_41, align 8
  %load_45 = load i64, ptr %alloca_count_18, align 8
  store i64 %load_45, ptr %alloca_count_44, align 8
  %load_48 = load i64, ptr %alloca_count_44, align 8
  %iop_49 = mul i64 %load_48, 24
  %gep_51 = getelementptr inbounds i8, ptr %alloca_count_27, i64 %iop_49
  %load_53 = load { { ptr, i64 }, ptr }, ptr %gep_51, align 8
  store { { ptr, i64 }, ptr } %load_53, ptr %alloca_count_47, align 8
  %load_55 = load { { ptr, i64 }, ptr }, ptr %alloca_count_47, align 8
  invoke void @__closure1_call({ ptr } undef)
          to label %bb4 unwind label %bb5

bb3:                                              ; preds = %bb1
  %load_58 = load i64, ptr %alloca_count_22, align 8
  %load_59 = load i64, ptr %alloca_count_20, align 8
  %iop_60 = add i64 %load_58, %load_59
  store i64 %iop_60, ptr %alloca_count_57, align 8
  %load_63 = load i64, ptr %alloca_count_57, align 8
  store i64 %load_63, ptr %alloca_count_62, align 8
  %load_65 = load i64, ptr %alloca_count_22, align 8
  %load_66 = load i64, ptr %alloca_count_20, align 8
  %load_67 = load i64, ptr %alloca_count_62, align 8
  %call_68 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.0, i64 %load_65, i64 %load_66, i64 %load_67)
  br label %bb12

bb4:                                              ; preds = %bb2
  store i1 true, ptr %alloca_count_21, align 1
  br label %bb6

bb5:                                              ; preds = %bb2
  %landingpad = landingpad { ptr, i32 }
          catch ptr null
  store i1 false, ptr %alloca_count_21, align 1
  br label %bb6

bb12:                                             ; preds = %bb3
  %load_72 = load i64, ptr %alloca_count_62, align 8
  %load_73 = load i64, ptr %alloca_count_22, align 8
  %load_74 = load i64, ptr %alloca_count_20, align 8
  %insertvalue_75 = insertvalue { i64, i64, i64 } undef, i64 %load_72, 0
  %insertvalue_76 = insertvalue { i64, i64, i64 } %insertvalue_75, i64 %load_73, 1
  %insertvalue_77 = insertvalue { i64, i64, i64 } %insertvalue_76, i64 %load_74, 2
  store { i64, i64, i64 } %insertvalue_77, ptr %alloca_count_19, align 8
  %load_79 = load { i64, i64, i64 }, ptr %alloca_count_19, align 8
  ret { i64, i64, i64 } %load_79

bb6:                                              ; preds = %bb5, %bb4
  %load_81 = load i1, ptr %alloca_count_21, align 1
  store i1 %load_81, ptr %alloca_count_80, align 1
  %load_83 = load i1, ptr %alloca_count_80, align 1
  br i1 %load_83, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_84 = load i64, ptr %alloca_count_22, align 8
  %iop_85 = add i64 %load_84, 1
  store i64 %iop_85, ptr %alloca_count_22, align 8
  %load_88 = load { ptr, i64 }, ptr %alloca_count_47, align 8
  %call_89 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.1, { ptr, i64 } %load_88)
  br label %bb10

bb8:                                              ; preds = %bb6
  %load_90 = load i64, ptr %alloca_count_20, align 8
  %iop_91 = add i64 %load_90, 1
  store i64 %iop_91, ptr %alloca_count_20, align 8
  %load_94 = load { ptr, i64 }, ptr %alloca_count_47, align 8
  %call_95 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.2, { ptr, i64 } %load_94)
  br label %bb11

bb10:                                             ; preds = %bb7
  br label %bb9

bb11:                                             ; preds = %bb8
  br label %bb9

bb9:                                              ; preds = %bb11, %bb10
  %load_96 = load i64, ptr %alloca_count_18, align 8
  %iop_97 = add i64 %load_96, 1
  store i64 %iop_97, ptr %alloca_count_18, align 8
  br label %bb1
}

define internal { i64, i64, i64 } @std__test__run() {
bb0:
  %alloca_99 = alloca { i64, i64, i64 }, align 8
  %alloca_count_99 = alloca { i64, i64, i64 }, align 8
  %call_100 = call { i64, i64, i64 } @std__test__run_tests()
  store { i64, i64, i64 } %call_100, ptr %alloca_count_99, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %load_102 = load { i64, i64, i64 }, ptr %alloca_count_99, align 8
  ret { i64, i64, i64 } %load_102
}

define internal i64 @apply_if(i1 %0, i64 %1, i64 %2, ptr %3) {
bb0:
  %alloca_103 = alloca i64, align 8
  %alloca_count_103 = alloca i64, align 8
  br i1 %0, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  %call_104 = call i64 %3(i64 %1, i64 %2)
  store i64 %call_104, ptr %alloca_count_103, align 8
  br label %bb4

bb2:                                              ; preds = %bb0
  store i64 0, ptr %alloca_count_103, align 8
  br label %bb3

bb4:                                              ; preds = %bb1
  br label %bb3

bb3:                                              ; preds = %bb4, %bb2
  %load_107 = load i64, ptr %alloca_count_103, align 8
  ret i64 %load_107
}

define internal { i64 } @make_adder(i64 %0) {
bb0:
  %alloca_108 = alloca { i64 }, align 8
  %alloca_count_108 = alloca { i64 }, align 8
  %insertvalue_109 = insertvalue { i64 } undef, i64 %0, 0
  store { i64 } %insertvalue_109, ptr %alloca_count_108, align 8
  %load_111 = load { i64 }, ptr %alloca_count_108, align 8
  ret { i64 } %load_111
}

define i32 @main() {
bb0:
  %alloca_129 = alloca { i8 }, align 8
  %alloca_count_129 = alloca { i8 }, align 8
  %call_112 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.3)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_113 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.4)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_114 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.5)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_115 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.6)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_116 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.7)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_117 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.8)
  br label %bb6

bb6:                                              ; preds = %bb5
  call void @apply__mono_a7af9f593fdc4675_17(i64 10, i64 20, ptr @add__mono_a7af9f593fdc4675_18)
  br label %bb7

bb7:                                              ; preds = %bb6
  call void @apply__mono_d7ad91e83a08a980_17(double 1.500000e+00, double 2.500000e+00, ptr @add__mono_d7ad91e83a08a980_18)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_120 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.9)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_121 = call i64 @apply_if(i1 true, i64 5, i64 3, ptr @add__spec0)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_122 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.10, i64 %call_121)
  br label %bb11

bb11:                                             ; preds = %bb10
  %call_123 = call i64 @apply_if(i1 false, i64 5, i64 3, ptr @add__spec0)
  br label %bb12

bb12:                                             ; preds = %bb11
  %call_124 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.11, i64 %call_123)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_125 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.12)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_126 = call { i64 } @make_adder(i64 10)
  br label %bb15

bb15:                                             ; preds = %bb14
  %call_127 = call i64 @__closure0_call({ i64 } %call_126, i64 5)
  br label %bb16

bb16:                                             ; preds = %bb15
  %call_128 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.13, i64 %call_127)
  br label %bb17

bb17:                                             ; preds = %bb16
  store { i8 } zeroinitializer, ptr %alloca_count_129, align 1
  %load_131 = load { i8 }, ptr %alloca_count_129, align 1
  %call_132 = call i64 @__closure2_call({ i8 } %load_131, i64 7)
  br label %bb18

bb18:                                             ; preds = %bb17
  %call_133 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.14, i64 %call_132)
  br label %bb19

bb19:                                             ; preds = %bb18
  ret i32 0
}

define internal i64 @add__spec0(i64 %0, i64 %1) {
bb0:
  %alloca_134 = alloca i64, align 8
  %alloca_count_134 = alloca i64, align 8
  %iop_135 = add i64 %0, %1
  store i64 %iop_135, ptr %alloca_count_134, align 8
  %load_137 = load i64, ptr %alloca_count_134, align 8
  ret i64 %load_137
}

define internal void @apply__mono_a7af9f593fdc4675_17(i64 %0, i64 %1, ptr %2) {
bb0:
  %call_138 = call i64 %2(i64 %0, i64 %1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_139 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.15, i64 %0, i64 %1, i64 %call_138)
  ret void
}

define internal i64 @add__mono_a7af9f593fdc4675_18(i64 %0, i64 %1) {
bb0:
  %alloca_140 = alloca i64, align 8
  %alloca_count_140 = alloca i64, align 8
  %iop_141 = add i64 %0, %1
  store i64 %iop_141, ptr %alloca_count_140, align 8
  %load_143 = load i64, ptr %alloca_count_140, align 8
  ret i64 %load_143
}

define internal void @apply__mono_d7ad91e83a08a980_17(double %0, double %1, ptr %2) {
bb0:
  %call_144 = call double %2(double %0, double %1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_145 = call i32 (ptr, ...) @printf(ptr @.str.09_higher_order_functions.16, double %0, double %1, double %call_144)
  ret void
}

define internal double @add__mono_d7ad91e83a08a980_18(double %0, double %1) {
bb0:
  %alloca_146 = alloca double, align 8
  %alloca_count_146 = alloca double, align 8
  %fop_147 = fadd double %0, %1
  store double %fop_147, ptr %alloca_count_146, align 8
  %load_149 = load double, ptr %alloca_count_146, align 8
  ret double %load_149
}

define internal void @TestCase__run(ptr %0) {
bb0:
  ret void
}

declare i32 @printf(ptr, ...)

declare i32 @__gxx_personality_v0(...)
