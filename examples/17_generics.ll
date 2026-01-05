; ModuleID = '17_generics'
source_filename = "17_generics"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.17_generics.0 = constant [31 x i8] c"\F0\9F\93\98 Tutorial: 17_generics.fp\0A\00"
@.str.17_generics.1 = constant [60 x i8] c"\F0\9F\A7\AD Focus: Generics: type parameters and monomorphization\0A\00"
@.str.17_generics.2 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.17_generics.3 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.17_generics.4 = constant [2 x i8] c"\0A\00"
@.str.17_generics.5 = constant [6 x i8] c"hello\00"
@.str.17_generics.6 = constant [19 x i8] c"pair = (%lld, %s)\0A\00"
@.str.17_generics.7 = constant [20 x i8] c"max(10, 20) = %lld\0A\00"
@.str.17_generics.8 = constant [20 x i8] c"max(3.5, 2.1) = %f\0A\00"
@.str.17_generics.9 = constant [32 x i8] c"unwrap_or(Some(100), 0) = %lld\0A\00"
@.str.17_generics.10 = constant [28 x i8] c"unwrap_or(None, 99) = %lld\0A\00"

define i32 @main() {
bb0:
  %alloca_25 = alloca { i64, i64 }, align 8
  %alloca_count_25 = alloca { i64, i64 }, align 8
  %alloca_23 = alloca { i64, i64 }, align 8
  %alloca_count_23 = alloca { i64, i64 }, align 8
  %alloca_21 = alloca { i64, i64 }, align 8
  %alloca_count_21 = alloca { i64, i64 }, align 8
  %alloca_10 = alloca { i64, ptr }, align 8
  %alloca_count_10 = alloca { i64, ptr }, align 8
  %alloca_6 = alloca { i64, ptr }, align 8
  %alloca_count_6 = alloca { i64, ptr }, align 8
  %call_0 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.2)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.3)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_5 = call { i64, ptr } @Pair__new__mono_a3cae9220656bc7e(i64 42, ptr @.str.17_generics.5)
  br label %bb6

bb6:                                              ; preds = %bb5
  store { i64, ptr } %call_5, ptr %alloca_count_6, align 8
  %load_9 = load i64, ptr %alloca_count_6, align 8
  store { i64, ptr } %call_5, ptr %alloca_count_10, align 8
  %gep_13 = getelementptr inbounds i8, ptr %alloca_count_10, i64 8
  %load_15 = load ptr, ptr %gep_13, align 8
  %call_16 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.6, i64 %load_9, ptr %load_15)
  %call_17 = call i64 @max__mono_a7af9f593fdc4675_11(i64 10, i64 20)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_18 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.7, i64 %call_17)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_19 = call double @max__mono_d7ad91e83a08a980_11(double 3.500000e+00, double 2.100000e+00)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_20 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.8, double %call_19)
  br label %bb10

bb10:                                             ; preds = %bb9
  store { i64, i64 } { i64 0, i64 100 }, ptr %alloca_count_21, align 8
  store { i64, i64 } { i64 1, i64 0 }, ptr %alloca_count_23, align 8
  %load_26 = load { i64, i64 }, ptr %alloca_count_23, align 8
  store { i64, i64 } %load_26, ptr %alloca_count_25, align 8
  %load_28 = load { i64, i64 }, ptr %alloca_count_21, align 8
  %call_29 = call i64 @Option__unwrap_or__mono_a7af9f593fdc4675({ i64, i64 } %load_28, i64 0)
  br label %bb11

bb11:                                             ; preds = %bb10
  %call_30 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.9, i64 %call_29)
  %load_31 = load { i64, i64 }, ptr %alloca_count_25, align 8
  %call_32 = call i64 @Option__unwrap_or__mono_a7af9f593fdc4675({ i64, i64 } %load_31, i64 99)
  br label %bb12

bb12:                                             ; preds = %bb11
  %call_33 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.10, i64 %call_32)
  ret i32 0
}

declare i32 @printf(ptr, ...)

define internal { i64, ptr } @Pair__new__mono_a3cae9220656bc7e(i64 %0, ptr %1) {
bb0:
  %alloca_34 = alloca { i64, ptr }, align 8
  %alloca_count_34 = alloca { i64, ptr }, align 8
  %insertvalue_35 = insertvalue { i64, ptr } undef, i64 %0, 0
  %insertvalue_36 = insertvalue { i64, ptr } %insertvalue_35, ptr %1, 1
  store { i64, ptr } %insertvalue_36, ptr %alloca_count_34, align 8
  %load_38 = load { i64, ptr }, ptr %alloca_count_34, align 8
  ret { i64, ptr } %load_38
}

define internal i64 @max__mono_a7af9f593fdc4675_11(i64 %0, i64 %1) {
bb0:
  %alloca_40 = alloca i1, align 1
  %alloca_count_40 = alloca i1, align 1
  %alloca_39 = alloca i64, align 8
  %alloca_count_39 = alloca i64, align 8
  %icmp_41 = icmp sgt i64 %0, %1
  store i1 %icmp_41, ptr %alloca_count_40, align 1
  %load_43 = load i1, ptr %alloca_count_40, align 1
  br i1 %load_43, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  store i64 %0, ptr %alloca_count_39, align 8
  br label %bb3

bb2:                                              ; preds = %bb0
  store i64 %1, ptr %alloca_count_39, align 8
  br label %bb3

bb3:                                              ; preds = %bb2, %bb1
  %load_46 = load i64, ptr %alloca_count_39, align 8
  ret i64 %load_46
}

define internal double @max__mono_d7ad91e83a08a980_11(double %0, double %1) {
bb0:
  %alloca_48 = alloca i1, align 1
  %alloca_count_48 = alloca i1, align 1
  %alloca_47 = alloca double, align 8
  %alloca_count_47 = alloca double, align 8
  %fcmp_49 = fcmp ogt double %0, %1
  store i1 %fcmp_49, ptr %alloca_count_48, align 1
  %load_51 = load i1, ptr %alloca_count_48, align 1
  br i1 %load_51, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  store double %0, ptr %alloca_count_47, align 8
  br label %bb3

bb2:                                              ; preds = %bb0
  store double %1, ptr %alloca_count_47, align 8
  br label %bb3

bb3:                                              ; preds = %bb2, %bb1
  %load_54 = load double, ptr %alloca_count_47, align 8
  ret double %load_54
}

define internal i64 @Option__unwrap_or__mono_a7af9f593fdc4675({ i64, i64 } %0, i64 %1) {
bb0:
  %alloca_72 = alloca i1, align 1
  %alloca_count_72 = alloca i1, align 1
  %alloca_64 = alloca i64, align 8
  %alloca_count_64 = alloca i64, align 8
  %alloca_58 = alloca i1, align 1
  %alloca_count_58 = alloca i1, align 1
  %alloca_56 = alloca { i64, i64 }, align 8
  %alloca_count_56 = alloca { i64, i64 }, align 8
  %alloca_55 = alloca i64, align 8
  %alloca_count_55 = alloca i64, align 8
  store { i64, i64 } %0, ptr %alloca_count_56, align 8
  %load_60 = load i64, ptr %alloca_count_56, align 8
  %icmp_61 = icmp eq i64 %load_60, 0
  store i1 %icmp_61, ptr %alloca_count_58, align 1
  %load_63 = load i1, ptr %alloca_count_58, align 1
  br i1 %load_63, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  %gep_66 = getelementptr inbounds i8, ptr %alloca_count_56, i64 8
  %load_68 = load i64, ptr %gep_66, align 8
  store i64 %load_68, ptr %alloca_count_64, align 8
  %load_70 = load i64, ptr %alloca_count_64, align 8
  store i64 %load_70, ptr %alloca_count_55, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  %load_74 = load i64, ptr %alloca_count_56, align 8
  %icmp_75 = icmp eq i64 %load_74, 1
  store i1 %icmp_75, ptr %alloca_count_72, align 1
  %load_77 = load i1, ptr %alloca_count_72, align 1
  br i1 %load_77, label %bb4, label %bb5

bb1:                                              ; preds = %bb5, %bb4, %bb2
  %load_78 = load i64, ptr %alloca_count_55, align 8
  ret i64 %load_78

bb4:                                              ; preds = %bb3
  store i64 %1, ptr %alloca_count_55, align 8
  br label %bb1

bb5:                                              ; preds = %bb3
  br label %bb1
}
