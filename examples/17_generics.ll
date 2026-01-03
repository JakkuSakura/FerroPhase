; ModuleID = '17_generics'
source_filename = "17_generics"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.17_generics.0 = private unnamed_addr constant [6 x i8] c"hello\00", align 1
@.str.17_generics.1 = private unnamed_addr constant [12 x i8] c"(%lld, %s)\0A\00", align 1
@.str.17_generics.2 = private unnamed_addr constant [20 x i8] c"max(10, 20) = %lld\0A\00", align 1
@.str.17_generics.3 = private unnamed_addr constant [20 x i8] c"max(3.5, 2.1) = %f\0A\00", align 1
@.str.17_generics.4 = private unnamed_addr constant [6 x i8] c"%lld\0A\00", align 1

define i32 @main() {
bb0:
  %alloca_20 = alloca { i64, i64 }, align 8
  %alloca_count_20 = alloca { i64, i64 }, align 8
  %alloca_18 = alloca { i64, i64 }, align 8
  %alloca_count_18 = alloca { i64, i64 }, align 8
  %alloca_16 = alloca { i64, i64 }, align 8
  %alloca_count_16 = alloca { i64, i64 }, align 8
  %alloca_5 = alloca { i64, ptr }, align 8
  %alloca_count_5 = alloca { i64, ptr }, align 8
  %alloca_1 = alloca { i64, ptr }, align 8
  %alloca_count_1 = alloca { i64, ptr }, align 8
  %call_0 = call { i64, ptr } @Pair__new__mono_a3cae9220656bc7e(i64 42, ptr @.str.17_generics.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  store { i64, ptr } %call_0, ptr %alloca_count_1, align 8
  %load_4 = load i64, ptr %alloca_count_1, align 8
  store { i64, ptr } %call_0, ptr %alloca_count_5, align 8
  %gep_8 = getelementptr inbounds i8, ptr %alloca_count_5, i64 8
  %load_10 = load ptr, ptr %gep_8, align 8
  %call_11 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.1, i64 %load_4, ptr %load_10)
  %call_12 = call i64 @max__mono_a7af9f593fdc4675_6(i64 10, i64 20)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_13 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.2, i64 %call_12)
  %call_14 = call double @max__mono_d7ad91e83a08a980_6(double 3.500000e+00, double 2.100000e+00)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_15 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.3, double %call_14)
  store { i64, i64 } { i64 0, i64 100 }, ptr %alloca_count_16, align 8
  store { i64, i64 } { i64 1, i64 0 }, ptr %alloca_count_18, align 8
  %load_21 = load { i64, i64 }, ptr %alloca_count_18, align 8
  store { i64, i64 } %load_21, ptr %alloca_count_20, align 8
  %load_23 = load { i64, i64 }, ptr %alloca_count_16, align 8
  %call_24 = call i64 @Option__unwrap_or__mono_a7af9f593fdc4675({ i64, i64 } %load_23, i64 0)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_25 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.4, i64 %call_24)
  %load_26 = load { i64, i64 }, ptr %alloca_count_20, align 8
  %call_27 = call i64 @Option__unwrap_or__mono_a7af9f593fdc4675({ i64, i64 } %load_26, i64 99)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_28 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.4, i64 %call_27)
  ret i32 0
}

define internal { i64, ptr } @Pair__new__mono_a3cae9220656bc7e(i64 %0, ptr %1) {
bb0:
  %alloca_29 = alloca { i64, ptr }, align 8
  %alloca_count_29 = alloca { i64, ptr }, align 8
  %insertvalue_30 = insertvalue { i64, ptr } undef, i64 %0, 0
  %insertvalue_31 = insertvalue { i64, ptr } %insertvalue_30, ptr %1, 1
  store { i64, ptr } %insertvalue_31, ptr %alloca_count_29, align 8
  %load_33 = load { i64, ptr }, ptr %alloca_count_29, align 8
  ret { i64, ptr } %load_33
}

declare i32 @printf(ptr, ...)

define internal i64 @max__mono_a7af9f593fdc4675_6(i64 %0, i64 %1) {
bb0:
  %alloca_35 = alloca i1, align 1
  %alloca_count_35 = alloca i1, align 1
  %alloca_34 = alloca i64, align 8
  %alloca_count_34 = alloca i64, align 8
  %icmp_36 = icmp sgt i64 %0, %1
  store i1 %icmp_36, ptr %alloca_count_35, align 1
  %load_38 = load i1, ptr %alloca_count_35, align 1
  br i1 %load_38, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  store i64 %0, ptr %alloca_count_34, align 8
  br label %bb3

bb2:                                              ; preds = %bb0
  store i64 %1, ptr %alloca_count_34, align 8
  br label %bb3

bb3:                                              ; preds = %bb2, %bb1
  %load_41 = load i64, ptr %alloca_count_34, align 8
  ret i64 %load_41
}

define internal double @max__mono_d7ad91e83a08a980_6(double %0, double %1) {
bb0:
  %alloca_43 = alloca i1, align 1
  %alloca_count_43 = alloca i1, align 1
  %alloca_42 = alloca double, align 8
  %alloca_count_42 = alloca double, align 8
  %fcmp_44 = fcmp ogt double %0, %1
  store i1 %fcmp_44, ptr %alloca_count_43, align 1
  %load_46 = load i1, ptr %alloca_count_43, align 1
  br i1 %load_46, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  store double %0, ptr %alloca_count_42, align 8
  br label %bb3

bb2:                                              ; preds = %bb0
  store double %1, ptr %alloca_count_42, align 8
  br label %bb3

bb3:                                              ; preds = %bb2, %bb1
  %load_49 = load double, ptr %alloca_count_42, align 8
  ret double %load_49
}

define internal i64 @Option__unwrap_or__mono_a7af9f593fdc4675({ i64, i64 } %0, i64 %1) {
bb0:
  %alloca_67 = alloca i1, align 1
  %alloca_count_67 = alloca i1, align 1
  %alloca_59 = alloca i64, align 8
  %alloca_count_59 = alloca i64, align 8
  %alloca_53 = alloca i1, align 1
  %alloca_count_53 = alloca i1, align 1
  %alloca_51 = alloca { i64, i64 }, align 8
  %alloca_count_51 = alloca { i64, i64 }, align 8
  %alloca_50 = alloca i64, align 8
  %alloca_count_50 = alloca i64, align 8
  store { i64, i64 } %0, ptr %alloca_count_51, align 8
  %load_55 = load i64, ptr %alloca_count_51, align 8
  %icmp_56 = icmp eq i64 %load_55, 0
  store i1 %icmp_56, ptr %alloca_count_53, align 1
  %load_58 = load i1, ptr %alloca_count_53, align 1
  br i1 %load_58, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  %gep_61 = getelementptr inbounds i8, ptr %alloca_count_51, i64 8
  %load_63 = load i64, ptr %gep_61, align 8
  store i64 %load_63, ptr %alloca_count_59, align 8
  %load_65 = load i64, ptr %alloca_count_59, align 8
  store i64 %load_65, ptr %alloca_count_50, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  %load_69 = load i64, ptr %alloca_count_51, align 8
  %icmp_70 = icmp eq i64 %load_69, 1
  store i1 %icmp_70, ptr %alloca_count_67, align 1
  %load_72 = load i1, ptr %alloca_count_67, align 1
  br i1 %load_72, label %bb4, label %bb5

bb1:                                              ; preds = %bb5, %bb4, %bb2
  %load_73 = load i64, ptr %alloca_count_50, align 8
  ret i64 %load_73

bb4:                                              ; preds = %bb3
  store i64 %1, ptr %alloca_count_50, align 8
  br label %bb1

bb5:                                              ; preds = %bb3
  br label %bb1
}
