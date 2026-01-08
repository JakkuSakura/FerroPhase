; ModuleID = '11_specialization_basics'
source_filename = "11_specialization_basics"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_5 = internal constant [0 x { { ptr, i64 }, ptr }] zeroinitializer, align 8
@.str.11_specialization_basics.0 = constant [51 x i8] c"test result: %lld passed; %lld failed; %lld total\0A\00"
@.str.11_specialization_basics.1 = constant [13 x i8] c"  %s ... ok\0A\00"
@.str.11_specialization_basics.2 = constant [17 x i8] c"  %s ... FAILED\0A\00"
@.str.11_specialization_basics.3 = constant [44 x i8] c"\F0\9F\93\98 Tutorial: 11_specialization_basics.fp\0A\00"
@.str.11_specialization_basics.4 = constant [66 x i8] c"\F0\9F\A7\AD Focus: Function specialization via generic monomorphization\0A\00"
@.str.11_specialization_basics.5 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.11_specialization_basics.6 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.11_specialization_basics.7 = constant [2 x i8] c"\0A\00"
@.str.11_specialization_basics.8 = constant [21 x i8] c"sum_pair i64 = %lld\0A\00"
@.str.11_specialization_basics.9 = constant [19 x i8] c"sum_pair f64 = %f\0A\00"
@.str.11_specialization_basics.10 = constant [26 x i8] c"specialized result: %lld\0A\00"
@.str.11_specialization_basics.11 = constant [24 x i8] c"specialized result: %f\0A\00"
@.str.11_specialization_basics.12 = constant [24 x i8] c"max(%lld, %lld) = %lld\0A\00"
@.str.11_specialization_basics.13 = constant [18 x i8] c"max(%f, %f) = %f\0A\00"

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
  %load_46 = load i64, ptr %alloca_count_7, align 8
  %load_47 = load i64, ptr %alloca_count_6, align 8
  %iop_48 = add i64 %load_46, %load_47
  store i64 %iop_48, ptr %alloca_count_45, align 8
  %load_51 = load i64, ptr %alloca_count_45, align 8
  store i64 %load_51, ptr %alloca_count_50, align 8
  %load_53 = load i64, ptr %alloca_count_7, align 8
  %load_54 = load i64, ptr %alloca_count_6, align 8
  %load_55 = load i64, ptr %alloca_count_50, align 8
  %call_56 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.0, i64 %load_53, i64 %load_54, i64 %load_55)
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
  store { i64, i64, i64 } %insertvalue_65, ptr %alloca_count_10, align 8
  %load_67 = load { i64, i64, i64 }, ptr %alloca_count_10, align 8
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
  %call_77 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.1, { ptr, i64 } %load_76)
  br label %bb10

bb8:                                              ; preds = %bb6
  %load_78 = load i64, ptr %alloca_count_6, align 8
  %iop_79 = add i64 %load_78, 1
  store i64 %iop_79, ptr %alloca_count_6, align 8
  %load_82 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_83 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.2, { ptr, i64 } %load_82)
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
  %alloca_100 = alloca { double, double }, align 8
  %alloca_count_100 = alloca { double, double }, align 8
  %alloca_98 = alloca { i64, i64 }, align 8
  %alloca_count_98 = alloca { i64, i64 }, align 8
  %call_91 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.3)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_92 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.4)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_93 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.5)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_94 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.6)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_95 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.7)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_96 = call i64 @pipeline__mono_a7af9f593fdc4675_16(i64 10, i64 20)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_97 = call double @pipeline__mono_d7ad91e83a08a980_16(double 1.500000e+00, double 2.500000e+00)
  br label %bb7

bb7:                                              ; preds = %bb6
  store { i64, i64 } { i64 3, i64 7 }, ptr %alloca_count_98, align 8
  store { double, double } { double 1.250000e+00, double 2.750000e+00 }, ptr %alloca_count_100, align 8
  %load_102 = load { i64, i64 }, ptr %alloca_count_98, align 8
  %call_103 = call i64 @sum_pair__mono_a7af9f593fdc4675_17({ i64, i64 } %load_102)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_104 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.8, i64 %call_103)
  %load_105 = load { double, double }, ptr %alloca_count_100, align 8
  %call_106 = call double @sum_pair__mono_d7ad91e83a08a980_17({ double, double } %load_105)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_107 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.9, double %call_106)
  %call_108 = call i64 @max__mono_a7af9f593fdc4675_18(i64 10, i64 3)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_109 = call double @max__mono_d7ad91e83a08a980_18(double 2.500000e+00, double 9.000000e+00)
  br label %bb11

bb11:                                             ; preds = %bb10
  ret i32 0
}

define internal i64 @add__mono_a7af9f593fdc4675_14(i64 %0, i64 %1) {
bb0:
  %alloca_110 = alloca i64, align 8
  %alloca_count_110 = alloca i64, align 8
  %iop_111 = add i64 %0, %1
  store i64 %iop_111, ptr %alloca_count_110, align 8
  %load_113 = load i64, ptr %alloca_count_110, align 8
  ret i64 %load_113
}

define internal i64 @double__mono_a7af9f593fdc4675_15(i64 %0) {
bb0:
  %alloca_114 = alloca i64, align 8
  %alloca_count_114 = alloca i64, align 8
  %iop_115 = add i64 %0, %0
  store i64 %iop_115, ptr %alloca_count_114, align 8
  %load_117 = load i64, ptr %alloca_count_114, align 8
  ret i64 %load_117
}

define internal i64 @pipeline__mono_a7af9f593fdc4675_16(i64 %0, i64 %1) {
bb0:
  %alloca_118 = alloca i64, align 8
  %alloca_count_118 = alloca i64, align 8
  %call_119 = call i64 @add__mono_a7af9f593fdc4675_14(i64 %0, i64 %1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_120 = call i64 @double__mono_a7af9f593fdc4675_15(i64 %call_119)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_121 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.10, i64 %call_120)
  store i64 %call_120, ptr %alloca_count_118, align 8
  %load_123 = load i64, ptr %alloca_count_118, align 8
  ret i64 %load_123
}

define internal double @add__mono_d7ad91e83a08a980_14(double %0, double %1) {
bb0:
  %alloca_124 = alloca double, align 8
  %alloca_count_124 = alloca double, align 8
  %fop_125 = fadd double %0, %1
  store double %fop_125, ptr %alloca_count_124, align 8
  %load_127 = load double, ptr %alloca_count_124, align 8
  ret double %load_127
}

define internal double @double__mono_d7ad91e83a08a980_15(double %0) {
bb0:
  %alloca_128 = alloca double, align 8
  %alloca_count_128 = alloca double, align 8
  %fop_129 = fadd double %0, %0
  store double %fop_129, ptr %alloca_count_128, align 8
  %load_131 = load double, ptr %alloca_count_128, align 8
  ret double %load_131
}

define internal double @pipeline__mono_d7ad91e83a08a980_16(double %0, double %1) {
bb0:
  %alloca_132 = alloca double, align 8
  %alloca_count_132 = alloca double, align 8
  %call_133 = call double @add__mono_d7ad91e83a08a980_14(double %0, double %1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_134 = call double @double__mono_d7ad91e83a08a980_15(double %call_133)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_135 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.11, double %call_134)
  store double %call_134, ptr %alloca_count_132, align 8
  %load_137 = load double, ptr %alloca_count_132, align 8
  ret double %load_137
}

define internal i64 @sum_pair__mono_a7af9f593fdc4675_17({ i64, i64 } %0) {
bb0:
  %alloca_143 = alloca { i64, i64 }, align 8
  %alloca_count_143 = alloca { i64, i64 }, align 8
  %alloca_139 = alloca { i64, i64 }, align 8
  %alloca_count_139 = alloca { i64, i64 }, align 8
  %alloca_138 = alloca i64, align 8
  %alloca_count_138 = alloca i64, align 8
  store { i64, i64 } %0, ptr %alloca_count_139, align 8
  %load_142 = load i64, ptr %alloca_count_139, align 8
  store { i64, i64 } %0, ptr %alloca_count_143, align 8
  %gep_146 = getelementptr inbounds i8, ptr %alloca_count_143, i64 8
  %load_148 = load i64, ptr %gep_146, align 8
  %iop_149 = add i64 %load_142, %load_148
  store i64 %iop_149, ptr %alloca_count_138, align 8
  %load_151 = load i64, ptr %alloca_count_138, align 8
  ret i64 %load_151
}

define internal double @sum_pair__mono_d7ad91e83a08a980_17({ double, double } %0) {
bb0:
  %alloca_157 = alloca { double, double }, align 8
  %alloca_count_157 = alloca { double, double }, align 8
  %alloca_153 = alloca { double, double }, align 8
  %alloca_count_153 = alloca { double, double }, align 8
  %alloca_152 = alloca double, align 8
  %alloca_count_152 = alloca double, align 8
  store { double, double } %0, ptr %alloca_count_153, align 8
  %load_156 = load double, ptr %alloca_count_153, align 8
  store { double, double } %0, ptr %alloca_count_157, align 8
  %gep_160 = getelementptr inbounds i8, ptr %alloca_count_157, i64 8
  %load_162 = load double, ptr %gep_160, align 8
  %fop_163 = fadd double %load_156, %load_162
  store double %fop_163, ptr %alloca_count_152, align 8
  %load_165 = load double, ptr %alloca_count_152, align 8
  ret double %load_165
}

define internal i64 @max__mono_a7af9f593fdc4675_18(i64 %0, i64 %1) {
bb0:
  %alloca_174 = alloca i64, align 8
  %alloca_count_174 = alloca i64, align 8
  %alloca_168 = alloca i1, align 1
  %alloca_count_168 = alloca i1, align 1
  %alloca_167 = alloca i64, align 8
  %alloca_count_167 = alloca i64, align 8
  %alloca_166 = alloca i64, align 8
  %alloca_count_166 = alloca i64, align 8
  %icmp_169 = icmp sgt i64 %0, %1
  store i1 %icmp_169, ptr %alloca_count_168, align 1
  %load_171 = load i1, ptr %alloca_count_168, align 1
  br i1 %load_171, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  store i64 %0, ptr %alloca_count_167, align 8
  br label %bb3

bb2:                                              ; preds = %bb0
  store i64 %1, ptr %alloca_count_167, align 8
  br label %bb3

bb3:                                              ; preds = %bb2, %bb1
  %load_175 = load i64, ptr %alloca_count_167, align 8
  store i64 %load_175, ptr %alloca_count_174, align 8
  %load_177 = load i64, ptr %alloca_count_174, align 8
  %call_178 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.12, i64 %0, i64 %1, i64 %load_177)
  %load_179 = load i64, ptr %alloca_count_174, align 8
  store i64 %load_179, ptr %alloca_count_166, align 8
  %load_181 = load i64, ptr %alloca_count_166, align 8
  ret i64 %load_181
}

define internal double @max__mono_d7ad91e83a08a980_18(double %0, double %1) {
bb0:
  %alloca_190 = alloca double, align 8
  %alloca_count_190 = alloca double, align 8
  %alloca_184 = alloca i1, align 1
  %alloca_count_184 = alloca i1, align 1
  %alloca_183 = alloca double, align 8
  %alloca_count_183 = alloca double, align 8
  %alloca_182 = alloca double, align 8
  %alloca_count_182 = alloca double, align 8
  %fcmp_185 = fcmp ogt double %0, %1
  store i1 %fcmp_185, ptr %alloca_count_184, align 1
  %load_187 = load i1, ptr %alloca_count_184, align 1
  br i1 %load_187, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  store double %0, ptr %alloca_count_182, align 8
  br label %bb3

bb2:                                              ; preds = %bb0
  store double %1, ptr %alloca_count_182, align 8
  br label %bb3

bb3:                                              ; preds = %bb2, %bb1
  %load_191 = load double, ptr %alloca_count_182, align 8
  store double %load_191, ptr %alloca_count_190, align 8
  %load_193 = load double, ptr %alloca_count_190, align 8
  %call_194 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.13, double %0, double %1, double %load_193)
  %load_195 = load double, ptr %alloca_count_190, align 8
  store double %load_195, ptr %alloca_count_183, align 8
  %load_197 = load double, ptr %alloca_count_183, align 8
  ret double %load_197
}

define internal void @TestCase__run(ptr %0) {
bb0:
  ret void
}

declare i32 @printf(ptr, ...)

declare i32 @__gxx_personality_v0(...)
