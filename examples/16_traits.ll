; ModuleID = '16_traits'
source_filename = "16_traits"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_5 = internal constant [0 x { { ptr, i64 }, ptr }] zeroinitializer, align 8
@global_91 = internal constant i1 false, align 1
@.str.16_traits.0 = constant [51 x i8] c"test result: %lld passed; %lld failed; %lld total\0A\00"
@.str.16_traits.1 = constant [13 x i8] c"  %s ... ok\0A\00"
@.str.16_traits.2 = constant [17 x i8] c"  %s ... FAILED\0A\00"
@.str.16_traits.3 = constant [11 x i8] c"area=%.2f\0A\00"
@.str.16_traits.4 = constant [29 x i8] c"\F0\9F\93\98 Tutorial: 16_traits.fp\0A\00"
@.str.16_traits.5 = constant [67 x i8] c"\F0\9F\A7\AD Focus: Traits: defining shared behavior with default methods\0A\00"
@.str.16_traits.6 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.16_traits.7 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.16_traits.8 = constant [2 x i8] c"\0A\00"
@.str.16_traits.9 = constant [6 x i8] c"%.2f\0A\00"

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
  %alloca_8 = alloca { i64, i64, i64 }, align 8
  %alloca_count_8 = alloca { i64, i64, i64 }, align 8
  %alloca_7 = alloca i64, align 8
  %alloca_count_7 = alloca i64, align 8
  %alloca_6 = alloca i64, align 8
  %alloca_count_6 = alloca i64, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_11, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_13, align 8
  %load_16 = load [0 x { { ptr, i64 }, ptr }], ptr %alloca_count_13, align 8
  store [0 x { { ptr, i64 }, ptr }] %load_16, ptr %alloca_count_15, align 8
  store i64 0, ptr %alloca_count_7, align 8
  store i64 0, ptr %alloca_count_9, align 8
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
  %load_46 = load i64, ptr %alloca_count_7, align 8
  %load_47 = load i64, ptr %alloca_count_9, align 8
  %iop_48 = add i64 %load_46, %load_47
  store i64 %iop_48, ptr %alloca_count_45, align 8
  %load_51 = load i64, ptr %alloca_count_45, align 8
  store i64 %load_51, ptr %alloca_count_50, align 8
  %load_53 = load i64, ptr %alloca_count_7, align 8
  %load_54 = load i64, ptr %alloca_count_9, align 8
  %load_55 = load i64, ptr %alloca_count_50, align 8
  %call_56 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.0, i64 %load_53, i64 %load_54, i64 %load_55)
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
  %load_61 = load i64, ptr %alloca_count_7, align 8
  %load_62 = load i64, ptr %alloca_count_9, align 8
  %insertvalue_63 = insertvalue { i64, i64, i64 } undef, i64 %load_60, 0
  %insertvalue_64 = insertvalue { i64, i64, i64 } %insertvalue_63, i64 %load_61, 1
  %insertvalue_65 = insertvalue { i64, i64, i64 } %insertvalue_64, i64 %load_62, 2
  store { i64, i64, i64 } %insertvalue_65, ptr %alloca_count_8, align 8
  %load_67 = load { i64, i64, i64 }, ptr %alloca_count_8, align 8
  ret { i64, i64, i64 } %load_67

bb6:                                              ; preds = %bb5, %bb4
  %load_69 = load i1, ptr %alloca_count_10, align 1
  store i1 %load_69, ptr %alloca_count_68, align 1
  %load_71 = load i1, ptr %alloca_count_68, align 1
  br i1 %load_71, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_72 = load i64, ptr %alloca_count_7, align 8
  %iop_73 = add i64 %load_72, 1
  store i64 %iop_73, ptr %alloca_count_7, align 8
  %load_76 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_77 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.1, { ptr, i64 } %load_76)
  br label %bb10

bb8:                                              ; preds = %bb6
  %load_78 = load i64, ptr %alloca_count_9, align 8
  %iop_79 = add i64 %load_78, 1
  store i64 %iop_79, ptr %alloca_count_9, align 8
  %load_82 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_83 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.2, { ptr, i64 } %load_82)
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

define internal double @Circle__area(ptr %0) {
bb0:
  %alloca_93 = alloca double, align 8
  %alloca_count_93 = alloca double, align 8
  %alloca_92 = alloca double, align 8
  %alloca_count_92 = alloca double, align 8
  %load_95 = load double, ptr %0, align 8
  %fop_96 = fmul double 3.141590e+00, %load_95
  store double %fop_96, ptr %alloca_count_93, align 8
  %load_98 = load double, ptr %alloca_count_93, align 8
  %load_100 = load double, ptr %0, align 8
  %fop_101 = fmul double %load_98, %load_100
  store double %fop_101, ptr %alloca_count_92, align 8
  %load_103 = load double, ptr %alloca_count_92, align 8
  ret double %load_103
}

define internal void @Circle__describe(ptr %0) {
bb0:
  %call_104 = call double @Circle__area(ptr %0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_105 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.3, double %call_104)
  ret void
}

define internal double @Rectangle__area(ptr %0) {
bb0:
  %alloca_106 = alloca double, align 8
  %alloca_count_106 = alloca double, align 8
  %load_108 = load double, ptr %0, align 8
  %gep_110 = getelementptr inbounds i8, ptr %0, i64 8
  %load_112 = load double, ptr %gep_110, align 8
  %fop_113 = fmul double %load_108, %load_112
  store double %fop_113, ptr %alloca_count_106, align 8
  %load_115 = load double, ptr %alloca_count_106, align 8
  ret double %load_115
}

define internal void @Rectangle__describe(ptr %0) {
bb0:
  %call_116 = call double @Rectangle__area(ptr %0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_117 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.3, double %call_116)
  ret void
}

define i32 @main() {
bb0:
  %alloca_139 = alloca ptr, align 8
  %alloca_count_139 = alloca ptr, align 8
  %alloca_135 = alloca ptr, align 8
  %alloca_count_135 = alloca ptr, align 8
  %alloca_131 = alloca ptr, align 8
  %alloca_count_131 = alloca ptr, align 8
  %alloca_127 = alloca ptr, align 8
  %alloca_count_127 = alloca ptr, align 8
  %alloca_125 = alloca { double, double }, align 8
  %alloca_count_125 = alloca { double, double }, align 8
  %alloca_123 = alloca { double }, align 8
  %alloca_count_123 = alloca { double }, align 8
  %call_118 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.4)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_119 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.5)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_120 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.6)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_121 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.7)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_122 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.8)
  br label %bb5

bb5:                                              ; preds = %bb4
  store { double } { double 5.000000e+00 }, ptr %alloca_count_123, align 8
  store { double, double } { double 4.000000e+00, double 6.000000e+00 }, ptr %alloca_count_125, align 8
  store ptr %alloca_count_123, ptr %alloca_count_127, align 8
  %load_129 = load ptr, ptr %alloca_count_127, align 8
  call void @Circle__describe(ptr %load_129)
  br label %bb6

bb6:                                              ; preds = %bb5
  store ptr %alloca_count_125, ptr %alloca_count_131, align 8
  %load_133 = load ptr, ptr %alloca_count_131, align 8
  call void @Rectangle__describe(ptr %load_133)
  br label %bb7

bb7:                                              ; preds = %bb6
  store ptr %alloca_count_123, ptr %alloca_count_135, align 8
  %load_137 = load ptr, ptr %alloca_count_135, align 8
  call void @print_area__mono_85382269fdc78777_18(ptr %load_137)
  br label %bb8

bb8:                                              ; preds = %bb7
  store ptr %alloca_count_125, ptr %alloca_count_139, align 8
  %load_141 = load ptr, ptr %alloca_count_139, align 8
  call void @print_area__mono_93ff406d3b2fff38_18(ptr %load_141)
  br label %bb9

bb9:                                              ; preds = %bb8
  ret i32 0
}

define internal void @print_area__mono_85382269fdc78777_18(ptr %0) {
bb0:
  %call_143 = call double @Circle__area(ptr %0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_144 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.9, double %call_143)
  br label %bb2

bb2:                                              ; preds = %bb1
  ret void
}

define internal void @print_area__mono_93ff406d3b2fff38_18(ptr %0) {
bb0:
  %call_145 = call double @Rectangle__area(ptr %0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_146 = call i32 (ptr, ...) @printf(ptr @.str.16_traits.9, double %call_145)
  br label %bb2

bb2:                                              ; preds = %bb1
  ret void
}

define internal void @TestCase__run(ptr %0) {
bb0:
  ret void
}

declare i32 @printf(ptr, ...)

declare i32 @__gxx_personality_v0(...)
