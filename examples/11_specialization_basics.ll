; ModuleID = '11_specialization_basics'
source_filename = "11_specialization_basics"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.11_specialization_basics.0 = private unnamed_addr constant [21 x i8] c"sum_pair i64 = %lld\0A\00", align 1
@.str.11_specialization_basics.1 = private unnamed_addr constant [19 x i8] c"sum_pair f64 = %f\0A\00", align 1
@.str.11_specialization_basics.2 = private unnamed_addr constant [26 x i8] c"specialized result: %lld\0A\00", align 1
@.str.11_specialization_basics.3 = private unnamed_addr constant [24 x i8] c"specialized result: %f\0A\00", align 1
@.str.11_specialization_basics.4 = private unnamed_addr constant [24 x i8] c"max(%lld, %lld) = %lld\0A\00", align 1
@.str.11_specialization_basics.5 = private unnamed_addr constant [18 x i8] c"max(%f, %f) = %f\0A\00", align 1

define i32 @main() {
bb0:
  %alloca_4 = alloca { double, double }, align 8
  %alloca_count_4 = alloca { double, double }, align 8
  %alloca_2 = alloca { i64, i64 }, align 8
  %alloca_count_2 = alloca { i64, i64 }, align 8
  %call_0 = call i64 @pipeline__mono_a7af9f593fdc4675_3(i64 10, i64 20)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_1 = call double @pipeline__mono_d7ad91e83a08a980_3(double 1.500000e+00, double 2.500000e+00)
  br label %bb2

bb2:                                              ; preds = %bb1
  store { i64, i64 } { i64 3, i64 7 }, ptr %alloca_count_2, align 8
  store { double, double } { double 1.250000e+00, double 2.750000e+00 }, ptr %alloca_count_4, align 8
  %load_6 = load { i64, i64 }, ptr %alloca_count_2, align 8
  %call_7 = call i64 @sum_pair__mono_a7af9f593fdc4675_4({ i64, i64 } %load_6)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_8 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.0, i64 %call_7)
  %load_9 = load { double, double }, ptr %alloca_count_4, align 8
  %call_10 = call double @sum_pair__mono_d7ad91e83a08a980_4({ double, double } %load_9)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_11 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.1, double %call_10)
  %call_12 = call i64 @max__mono_a7af9f593fdc4675_5(i64 10, i64 3)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_13 = call double @max__mono_d7ad91e83a08a980_5(double 2.500000e+00, double 9.000000e+00)
  br label %bb6

bb6:                                              ; preds = %bb5
  ret i32 0
}

define internal i64 @pipeline__mono_a7af9f593fdc4675_3(i64 %0, i64 %1) {
bb0:
  %alloca_22 = alloca i64, align 8
  %alloca_count_22 = alloca i64, align 8
  %call_23 = call i64 @add__mono_a7af9f593fdc4675_1(i64 %0, i64 %1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_24 = call i64 @double__mono_a7af9f593fdc4675_2(i64 %call_23)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_25 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.2, i64 %call_24)
  store i64 %call_24, ptr %alloca_count_22, align 8
  %load_27 = load i64, ptr %alloca_count_22, align 8
  ret i64 %load_27
}

define internal double @pipeline__mono_d7ad91e83a08a980_3(double %0, double %1) {
bb0:
  %alloca_36 = alloca double, align 8
  %alloca_count_36 = alloca double, align 8
  %call_37 = call double @add__mono_d7ad91e83a08a980_1(double %0, double %1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_38 = call double @double__mono_d7ad91e83a08a980_2(double %call_37)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_39 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.3, double %call_38)
  store double %call_38, ptr %alloca_count_36, align 8
  %load_41 = load double, ptr %alloca_count_36, align 8
  ret double %load_41
}

define internal i64 @sum_pair__mono_a7af9f593fdc4675_4({ i64, i64 } %0) {
bb0:
  %alloca_47 = alloca { i64, i64 }, align 8
  %alloca_count_47 = alloca { i64, i64 }, align 8
  %alloca_43 = alloca { i64, i64 }, align 8
  %alloca_count_43 = alloca { i64, i64 }, align 8
  %alloca_42 = alloca i64, align 8
  %alloca_count_42 = alloca i64, align 8
  store { i64, i64 } %0, ptr %alloca_count_43, align 8
  %load_46 = load i64, ptr %alloca_count_43, align 8
  store { i64, i64 } %0, ptr %alloca_count_47, align 8
  %gep_50 = getelementptr inbounds i8, ptr %alloca_count_47, i64 8
  %load_52 = load i64, ptr %gep_50, align 8
  %iop_53 = add i64 %load_46, %load_52
  store i64 %iop_53, ptr %alloca_count_42, align 8
  %load_55 = load i64, ptr %alloca_count_42, align 8
  ret i64 %load_55
}

declare i32 @printf(ptr, ...)

define internal double @sum_pair__mono_d7ad91e83a08a980_4({ double, double } %0) {
bb0:
  %alloca_61 = alloca { double, double }, align 8
  %alloca_count_61 = alloca { double, double }, align 8
  %alloca_57 = alloca { double, double }, align 8
  %alloca_count_57 = alloca { double, double }, align 8
  %alloca_56 = alloca double, align 8
  %alloca_count_56 = alloca double, align 8
  store { double, double } %0, ptr %alloca_count_57, align 8
  %load_60 = load double, ptr %alloca_count_57, align 8
  store { double, double } %0, ptr %alloca_count_61, align 8
  %gep_64 = getelementptr inbounds i8, ptr %alloca_count_61, i64 8
  %load_66 = load double, ptr %gep_64, align 8
  %fop_67 = fadd double %load_60, %load_66
  store double %fop_67, ptr %alloca_count_56, align 8
  %load_69 = load double, ptr %alloca_count_56, align 8
  ret double %load_69
}

define internal i64 @max__mono_a7af9f593fdc4675_5(i64 %0, i64 %1) {
bb0:
  %alloca_78 = alloca i64, align 8
  %alloca_count_78 = alloca i64, align 8
  %alloca_72 = alloca i1, align 1
  %alloca_count_72 = alloca i1, align 1
  %alloca_71 = alloca i64, align 8
  %alloca_count_71 = alloca i64, align 8
  %alloca_70 = alloca i64, align 8
  %alloca_count_70 = alloca i64, align 8
  %icmp_73 = icmp sgt i64 %0, %1
  store i1 %icmp_73, ptr %alloca_count_72, align 1
  %load_75 = load i1, ptr %alloca_count_72, align 1
  br i1 %load_75, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  store i64 %0, ptr %alloca_count_71, align 8
  br label %bb3

bb2:                                              ; preds = %bb0
  store i64 %1, ptr %alloca_count_71, align 8
  br label %bb3

bb3:                                              ; preds = %bb2, %bb1
  %load_79 = load i64, ptr %alloca_count_71, align 8
  store i64 %load_79, ptr %alloca_count_78, align 8
  %load_81 = load i64, ptr %alloca_count_78, align 8
  %call_82 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.4, i64 %0, i64 %1, i64 %load_81)
  %load_83 = load i64, ptr %alloca_count_78, align 8
  store i64 %load_83, ptr %alloca_count_70, align 8
  %load_85 = load i64, ptr %alloca_count_70, align 8
  ret i64 %load_85
}

define internal double @max__mono_d7ad91e83a08a980_5(double %0, double %1) {
bb0:
  %alloca_94 = alloca double, align 8
  %alloca_count_94 = alloca double, align 8
  %alloca_88 = alloca i1, align 1
  %alloca_count_88 = alloca i1, align 1
  %alloca_87 = alloca double, align 8
  %alloca_count_87 = alloca double, align 8
  %alloca_86 = alloca double, align 8
  %alloca_count_86 = alloca double, align 8
  %fcmp_89 = fcmp ogt double %0, %1
  store i1 %fcmp_89, ptr %alloca_count_88, align 1
  %load_91 = load i1, ptr %alloca_count_88, align 1
  br i1 %load_91, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  store double %0, ptr %alloca_count_87, align 8
  br label %bb3

bb2:                                              ; preds = %bb0
  store double %1, ptr %alloca_count_87, align 8
  br label %bb3

bb3:                                              ; preds = %bb2, %bb1
  %load_95 = load double, ptr %alloca_count_87, align 8
  store double %load_95, ptr %alloca_count_94, align 8
  %load_97 = load double, ptr %alloca_count_94, align 8
  %call_98 = call i32 (ptr, ...) @printf(ptr @.str.11_specialization_basics.5, double %0, double %1, double %load_97)
  %load_99 = load double, ptr %alloca_count_94, align 8
  store double %load_99, ptr %alloca_count_86, align 8
  %load_101 = load double, ptr %alloca_count_86, align 8
  ret double %load_101
}

define internal i64 @add__mono_a7af9f593fdc4675_1(i64 %0, i64 %1) {
bb0:
  %alloca_14 = alloca i64, align 8
  %alloca_count_14 = alloca i64, align 8
  %iop_15 = add i64 %0, %1
  store i64 %iop_15, ptr %alloca_count_14, align 8
  %load_17 = load i64, ptr %alloca_count_14, align 8
  ret i64 %load_17
}

define internal i64 @double__mono_a7af9f593fdc4675_2(i64 %0) {
bb0:
  %alloca_18 = alloca i64, align 8
  %alloca_count_18 = alloca i64, align 8
  %iop_19 = add i64 %0, %0
  store i64 %iop_19, ptr %alloca_count_18, align 8
  %load_21 = load i64, ptr %alloca_count_18, align 8
  ret i64 %load_21
}

define internal double @add__mono_d7ad91e83a08a980_1(double %0, double %1) {
bb0:
  %alloca_28 = alloca double, align 8
  %alloca_count_28 = alloca double, align 8
  %fop_29 = fadd double %0, %1
  store double %fop_29, ptr %alloca_count_28, align 8
  %load_31 = load double, ptr %alloca_count_28, align 8
  ret double %load_31
}

define internal double @double__mono_d7ad91e83a08a980_2(double %0) {
bb0:
  %alloca_32 = alloca double, align 8
  %alloca_count_32 = alloca double, align 8
  %fop_33 = fadd double %0, %0
  store double %fop_33, ptr %alloca_count_32, align 8
  %load_35 = load double, ptr %alloca_count_32, align 8
  ret double %load_35
}
