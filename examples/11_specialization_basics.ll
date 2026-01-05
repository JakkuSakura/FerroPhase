; ModuleID = '11_specialization_basics'
source_filename = "11_specialization_basics"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.11_specialization_basics.0 = constant [44 x i8] c"\F0\9F\93\98 Tutorial: 11_specialization_basics.fp\0A\00"
@.str.11_specialization_basics.1 = constant [66 x i8] c"\F0\9F\A7\AD Focus: Function specialization via generic monomorphization\0A\00"
@.str.11_specialization_basics.2 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.11_specialization_basics.3 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.11_specialization_basics.4 = constant [2 x i8] c"\0A\00"
@.str.11_specialization_basics.5 = constant [21 x i8] c"sum_pair i64 = %lld\0A\00"
@.str.11_specialization_basics.6 = constant [19 x i8] c"sum_pair f64 = %f\0A\00"
@.str.11_specialization_basics.7 = constant [26 x i8] c"specialized result: %lld\0A\00"
@.str.11_specialization_basics.8 = constant [24 x i8] c"specialized result: %f\0A\00"
@.str.11_specialization_basics.9 = constant [24 x i8] c"max(%lld, %lld) = %lld\0A\00"
@.str.11_specialization_basics.10 = constant [18 x i8] c"max(%f, %f) = %f\0A\00"

define i32 @main() {
bb0:
  %alloca_9 = alloca { double, double }, align 8
  %alloca_count_9 = alloca { double, double }, align 8
  %alloca_7 = alloca { i64, i64 }, align 8
  %alloca_count_7 = alloca { i64, i64 }, align 8
  %call_0 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.2)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.3)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_5 = call i64 @pipeline__mono_a7af9f593fdc4675_8(i64 10, i64 20)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_6 = call double @pipeline__mono_d7ad91e83a08a980_8(double 1.500000e+00, double 2.500000e+00)
  br label %bb7

bb7:                                              ; preds = %bb6
  store { i64, i64 } { i64 3, i64 7 }, ptr %alloca_count_7, align 8
  store { double, double } { double 1.250000e+00, double 2.750000e+00 }, ptr %alloca_count_9, align 8
  %load_11 = load { i64, i64 }, ptr %alloca_count_7, align 8
  %call_12 = call i64 @sum_pair__mono_a7af9f593fdc4675_9({ i64, i64 } %load_11)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_13 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.5, i64 %call_12)
  %load_14 = load { double, double }, ptr %alloca_count_9, align 8
  %call_15 = call double @sum_pair__mono_d7ad91e83a08a980_9({ double, double } %load_14)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_16 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.6, double %call_15)
  %call_17 = call i64 @max__mono_a7af9f593fdc4675_10(i64 10, i64 3)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_18 = call double @max__mono_d7ad91e83a08a980_10(double 2.500000e+00, double 9.000000e+00)
  br label %bb11

bb11:                                             ; preds = %bb10
  ret i32 0
}

declare i32 @printf(ptr, ...)

define internal i64 @pipeline__mono_a7af9f593fdc4675_8(i64 %0, i64 %1) {
bb0:
  %alloca_27 = alloca i64, align 8
  %alloca_count_27 = alloca i64, align 8
  %call_28 = call i64 @add__mono_a7af9f593fdc4675_6(i64 %0, i64 %1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_29 = call i64 @double__mono_a7af9f593fdc4675_7(i64 %call_28)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_30 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.7, i64 %call_29)
  store i64 %call_29, ptr %alloca_count_27, align 8
  %load_32 = load i64, ptr %alloca_count_27, align 8
  ret i64 %load_32
}

define internal double @pipeline__mono_d7ad91e83a08a980_8(double %0, double %1) {
bb0:
  %alloca_41 = alloca double, align 8
  %alloca_count_41 = alloca double, align 8
  %call_42 = call double @add__mono_d7ad91e83a08a980_6(double %0, double %1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_43 = call double @double__mono_d7ad91e83a08a980_7(double %call_42)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_44 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.8, double %call_43)
  store double %call_43, ptr %alloca_count_41, align 8
  %load_46 = load double, ptr %alloca_count_41, align 8
  ret double %load_46
}

define internal i64 @sum_pair__mono_a7af9f593fdc4675_9({ i64, i64 } %0) {
bb0:
  %alloca_52 = alloca { i64, i64 }, align 8
  %alloca_count_52 = alloca { i64, i64 }, align 8
  %alloca_48 = alloca { i64, i64 }, align 8
  %alloca_count_48 = alloca { i64, i64 }, align 8
  %alloca_47 = alloca i64, align 8
  %alloca_count_47 = alloca i64, align 8
  store { i64, i64 } %0, ptr %alloca_count_48, align 8
  %load_51 = load i64, ptr %alloca_count_48, align 8
  store { i64, i64 } %0, ptr %alloca_count_52, align 8
  %gep_55 = getelementptr inbounds i8, ptr %alloca_count_52, i64 8
  %load_57 = load i64, ptr %gep_55, align 8
  %iop_58 = add i64 %load_51, %load_57
  store i64 %iop_58, ptr %alloca_count_47, align 8
  %load_60 = load i64, ptr %alloca_count_47, align 8
  ret i64 %load_60
}

define internal double @sum_pair__mono_d7ad91e83a08a980_9({ double, double } %0) {
bb0:
  %alloca_66 = alloca { double, double }, align 8
  %alloca_count_66 = alloca { double, double }, align 8
  %alloca_62 = alloca { double, double }, align 8
  %alloca_count_62 = alloca { double, double }, align 8
  %alloca_61 = alloca double, align 8
  %alloca_count_61 = alloca double, align 8
  store { double, double } %0, ptr %alloca_count_62, align 8
  %load_65 = load double, ptr %alloca_count_62, align 8
  store { double, double } %0, ptr %alloca_count_66, align 8
  %gep_69 = getelementptr inbounds i8, ptr %alloca_count_66, i64 8
  %load_71 = load double, ptr %gep_69, align 8
  %fop_72 = fadd double %load_65, %load_71
  store double %fop_72, ptr %alloca_count_61, align 8
  %load_74 = load double, ptr %alloca_count_61, align 8
  ret double %load_74
}

define internal i64 @max__mono_a7af9f593fdc4675_10(i64 %0, i64 %1) {
bb0:
  %alloca_83 = alloca i64, align 8
  %alloca_count_83 = alloca i64, align 8
  %alloca_77 = alloca i1, align 1
  %alloca_count_77 = alloca i1, align 1
  %alloca_76 = alloca i64, align 8
  %alloca_count_76 = alloca i64, align 8
  %alloca_75 = alloca i64, align 8
  %alloca_count_75 = alloca i64, align 8
  %icmp_78 = icmp sgt i64 %0, %1
  store i1 %icmp_78, ptr %alloca_count_77, align 1
  %load_80 = load i1, ptr %alloca_count_77, align 1
  br i1 %load_80, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  store i64 %0, ptr %alloca_count_75, align 8
  br label %bb3

bb2:                                              ; preds = %bb0
  store i64 %1, ptr %alloca_count_75, align 8
  br label %bb3

bb3:                                              ; preds = %bb2, %bb1
  %load_84 = load i64, ptr %alloca_count_75, align 8
  store i64 %load_84, ptr %alloca_count_83, align 8
  %load_86 = load i64, ptr %alloca_count_83, align 8
  %call_87 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.9, i64 %0, i64 %1, i64 %load_86)
  %load_88 = load i64, ptr %alloca_count_83, align 8
  store i64 %load_88, ptr %alloca_count_76, align 8
  %load_90 = load i64, ptr %alloca_count_76, align 8
  ret i64 %load_90
}

define internal double @max__mono_d7ad91e83a08a980_10(double %0, double %1) {
bb0:
  %alloca_99 = alloca double, align 8
  %alloca_count_99 = alloca double, align 8
  %alloca_93 = alloca i1, align 1
  %alloca_count_93 = alloca i1, align 1
  %alloca_92 = alloca double, align 8
  %alloca_count_92 = alloca double, align 8
  %alloca_91 = alloca double, align 8
  %alloca_count_91 = alloca double, align 8
  %fcmp_94 = fcmp ogt double %0, %1
  store i1 %fcmp_94, ptr %alloca_count_93, align 1
  %load_96 = load i1, ptr %alloca_count_93, align 1
  br i1 %load_96, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  store double %0, ptr %alloca_count_91, align 8
  br label %bb3

bb2:                                              ; preds = %bb0
  store double %1, ptr %alloca_count_91, align 8
  br label %bb3

bb3:                                              ; preds = %bb2, %bb1
  %load_100 = load double, ptr %alloca_count_91, align 8
  store double %load_100, ptr %alloca_count_99, align 8
  %load_102 = load double, ptr %alloca_count_99, align 8
  %call_103 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.10, double %0, double %1, double %load_102)
  %load_104 = load double, ptr %alloca_count_99, align 8
  store double %load_104, ptr %alloca_count_92, align 8
  %load_106 = load double, ptr %alloca_count_92, align 8
  ret double %load_106
}

define internal i64 @add__mono_a7af9f593fdc4675_6(i64 %0, i64 %1) {
bb0:
  %alloca_19 = alloca i64, align 8
  %alloca_count_19 = alloca i64, align 8
  %iop_20 = add i64 %0, %1
  store i64 %iop_20, ptr %alloca_count_19, align 8
  %load_22 = load i64, ptr %alloca_count_19, align 8
  ret i64 %load_22
}

define internal i64 @double__mono_a7af9f593fdc4675_7(i64 %0) {
bb0:
  %alloca_23 = alloca i64, align 8
  %alloca_count_23 = alloca i64, align 8
  %iop_24 = add i64 %0, %0
  store i64 %iop_24, ptr %alloca_count_23, align 8
  %load_26 = load i64, ptr %alloca_count_23, align 8
  ret i64 %load_26
}

define internal double @add__mono_d7ad91e83a08a980_6(double %0, double %1) {
bb0:
  %alloca_33 = alloca double, align 8
  %alloca_count_33 = alloca double, align 8
  %fop_34 = fadd double %0, %1
  store double %fop_34, ptr %alloca_count_33, align 8
  %load_36 = load double, ptr %alloca_count_33, align 8
  ret double %load_36
}

define internal double @double__mono_d7ad91e83a08a980_7(double %0) {
bb0:
  %alloca_37 = alloca double, align 8
  %alloca_count_37 = alloca double, align 8
  %fop_38 = fadd double %0, %0
  store double %fop_38, ptr %alloca_count_37, align 8
  %load_40 = load double, ptr %alloca_count_37, align 8
  ret double %load_40
}
