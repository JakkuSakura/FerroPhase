; ModuleID = '08_metaprogramming_patterns'
source_filename = "08_metaprogramming_patterns"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.08_metaprogramming_patterns.0 = private unnamed_addr constant [15 x i8] c"struct Point3D\00", align 1
@.str.08_metaprogramming_patterns.1 = private unnamed_addr constant [32 x i8] c"%s has %lld fields (size=%lld)\0A\00", align 1
@.str.08_metaprogramming_patterns.2 = private unnamed_addr constant [5 x i8] c"i64\0A\00", align 1
@.str.08_metaprogramming_patterns.3 = private unnamed_addr constant [12 x i8] c"x type: %s\0A\00", align 1
@.str.08_metaprogramming_patterns.4 = private unnamed_addr constant [9 x i8] c"fields:\0A\00", align 1
@.str.08_metaprogramming_patterns.5 = private unnamed_addr constant [2 x i8] c"x\00", align 1
@.str.08_metaprogramming_patterns.6 = private unnamed_addr constant [2 x i8] c"y\00", align 1
@.str.08_metaprogramming_patterns.7 = private unnamed_addr constant [2 x i8] c"z\00", align 1
@.str.08_metaprogramming_patterns.8 = private unnamed_addr constant [10 x i8] c"  %s: %s\0A\00", align 1
@.str.08_metaprogramming_patterns.9 = private unnamed_addr constant [26 x i8] c"point=(%lld, %lld, %lld)\0A\00", align 1
@.str.08_metaprogramming_patterns.10 = private unnamed_addr constant [7 x i8] c"origin\00", align 1
@.str.08_metaprogramming_patterns.11 = private unnamed_addr constant [32 x i8] c"labeled=(%lld, %lld, %lld, %s)\0A\00", align 1

define i32 @main() {
bb0:
  %alloca_87 = alloca { i64, i64, i64, ptr }, align 8
  %alloca_count_87 = alloca { i64, i64, i64, ptr }, align 8
  %alloca_74 = alloca { i64, i64, i64 }, align 8
  %alloca_count_74 = alloca { i64, i64, i64 }, align 8
  %alloca_56 = alloca { ptr, ptr }, align 8
  %alloca_count_56 = alloca { ptr, ptr }, align 8
  %alloca_53 = alloca i64, align 8
  %alloca_count_53 = alloca i64, align 8
  %alloca_45 = alloca [3 x { ptr, ptr }], align 8
  %alloca_count_45 = alloca [3 x { ptr, ptr }], align 8
  %alloca_43 = alloca { ptr, ptr }, align 8
  %alloca_count_43 = alloca { ptr, ptr }, align 8
  %alloca_41 = alloca { ptr, ptr }, align 8
  %alloca_count_41 = alloca { ptr, ptr }, align 8
  %alloca_39 = alloca { ptr, ptr }, align 8
  %alloca_count_39 = alloca { ptr, ptr }, align 8
  %alloca_36 = alloca i64, align 8
  %alloca_count_36 = alloca i64, align 8
  %alloca_28 = alloca [3 x { ptr, ptr }], align 8
  %alloca_count_28 = alloca [3 x { ptr, ptr }], align 8
  %alloca_26 = alloca { ptr, ptr }, align 8
  %alloca_count_26 = alloca { ptr, ptr }, align 8
  %alloca_24 = alloca { ptr, ptr }, align 8
  %alloca_count_24 = alloca { ptr, ptr }, align 8
  %alloca_22 = alloca { ptr, ptr }, align 8
  %alloca_count_22 = alloca { ptr, ptr }, align 8
  %alloca_17 = alloca i1, align 1
  %alloca_count_17 = alloca i1, align 1
  %alloca_11 = alloca ptr, align 8
  %alloca_count_11 = alloca ptr, align 8
  %alloca_5 = alloca i64, align 8
  %alloca_count_5 = alloca i64, align 8
  %alloca_3 = alloca i64, align 8
  %alloca_count_3 = alloca i64, align 8
  %alloca_1 = alloca ptr, align 8
  %alloca_count_1 = alloca ptr, align 8
  %alloca_0 = alloca i64, align 8
  %alloca_count_0 = alloca i64, align 8
  store ptr @.str.08_metaprogramming_patterns.0, ptr %alloca_count_1, align 8
  store i64 3, ptr %alloca_count_3, align 8
  store i64 24, ptr %alloca_count_5, align 8
  %load_7 = load ptr, ptr %alloca_count_1, align 8
  %load_8 = load i64, ptr %alloca_count_3, align 8
  %load_9 = load i64, ptr %alloca_count_5, align 8
  %call_10 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.1, ptr %load_7, i64 %load_8, i64 %load_9)
  store ptr @.str.08_metaprogramming_patterns.2, ptr %alloca_count_11, align 8
  %load_13 = load ptr, ptr %alloca_count_11, align 8
  %call_14 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.3, ptr %load_13)
  %call_15 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.4)
  store i64 0, ptr %alloca_count_0, align 8
  br label %bb1

bb1:                                              ; preds = %bb2, %bb0
  %load_18 = load i64, ptr %alloca_count_0, align 8
  %icmp_19 = icmp slt i64 %load_18, 3
  store i1 %icmp_19, ptr %alloca_count_17, align 1
  %load_21 = load i1, ptr %alloca_count_17, align 1
  br i1 %load_21, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.5, ptr @.str.08_metaprogramming_patterns.2 }, ptr %alloca_count_22, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.6, ptr @.str.08_metaprogramming_patterns.2 }, ptr %alloca_count_24, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.7, ptr @.str.08_metaprogramming_patterns.2 }, ptr %alloca_count_26, align 8
  %load_29 = load { ptr, ptr }, ptr %alloca_count_22, align 8
  %load_30 = load { ptr, ptr }, ptr %alloca_count_24, align 8
  %load_31 = load { ptr, ptr }, ptr %alloca_count_26, align 8
  %insertvalue_32 = insertvalue [3 x { ptr, ptr }] undef, { ptr, ptr } %load_29, 0
  %insertvalue_33 = insertvalue [3 x { ptr, ptr }] %insertvalue_32, { ptr, ptr } %load_30, 1
  %insertvalue_34 = insertvalue [3 x { ptr, ptr }] %insertvalue_33, { ptr, ptr } %load_31, 2
  store [3 x { ptr, ptr }] %insertvalue_34, ptr %alloca_count_28, align 8
  %load_37 = load i64, ptr %alloca_count_0, align 8
  store i64 %load_37, ptr %alloca_count_36, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.5, ptr @.str.08_metaprogramming_patterns.2 }, ptr %alloca_count_39, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.6, ptr @.str.08_metaprogramming_patterns.2 }, ptr %alloca_count_41, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.7, ptr @.str.08_metaprogramming_patterns.2 }, ptr %alloca_count_43, align 8
  %load_46 = load { ptr, ptr }, ptr %alloca_count_39, align 8
  %load_47 = load { ptr, ptr }, ptr %alloca_count_41, align 8
  %load_48 = load { ptr, ptr }, ptr %alloca_count_43, align 8
  %insertvalue_49 = insertvalue [3 x { ptr, ptr }] undef, { ptr, ptr } %load_46, 0
  %insertvalue_50 = insertvalue [3 x { ptr, ptr }] %insertvalue_49, { ptr, ptr } %load_47, 1
  %insertvalue_51 = insertvalue [3 x { ptr, ptr }] %insertvalue_50, { ptr, ptr } %load_48, 2
  store [3 x { ptr, ptr }] %insertvalue_51, ptr %alloca_count_45, align 8
  %load_54 = load i64, ptr %alloca_count_0, align 8
  store i64 %load_54, ptr %alloca_count_53, align 8
  %load_57 = load i64, ptr %alloca_count_53, align 8
  %iop_58 = mul i64 %load_57, 16
  %gep_60 = getelementptr inbounds i8, ptr %alloca_count_45, i64 %iop_58
  %load_62 = load { ptr, ptr }, ptr %gep_60, align 8
  store { ptr, ptr } %load_62, ptr %alloca_count_56, align 8
  %load_65 = load ptr, ptr %alloca_count_56, align 8
  %gep_67 = getelementptr inbounds i8, ptr %alloca_count_56, i64 8
  %load_69 = load ptr, ptr %gep_67, align 8
  %call_70 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.8, ptr %load_65, ptr %load_69)
  %load_71 = load i64, ptr %alloca_count_0, align 8
  %iop_72 = add i64 %load_71, 1
  store i64 %iop_72, ptr %alloca_count_0, align 8
  br label %bb1

bb3:                                              ; preds = %bb1
  store { i64, i64, i64 } { i64 1, i64 2, i64 3 }, ptr %alloca_count_74, align 8
  %load_77 = load i64, ptr %alloca_count_74, align 8
  %gep_79 = getelementptr inbounds i8, ptr %alloca_count_74, i64 8
  %load_81 = load i64, ptr %gep_79, align 8
  %gep_83 = getelementptr inbounds i8, ptr %alloca_count_74, i64 16
  %load_85 = load i64, ptr %gep_83, align 8
  %call_86 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.9, i64 %load_77, i64 %load_81, i64 %load_85)
  store { i64, i64, i64, ptr } { i64 4, i64 5, i64 6, ptr @.str.08_metaprogramming_patterns.10 }, ptr %alloca_count_87, align 8
  %load_90 = load i64, ptr %alloca_count_87, align 8
  %gep_92 = getelementptr inbounds i8, ptr %alloca_count_87, i64 8
  %load_94 = load i64, ptr %gep_92, align 8
  %gep_96 = getelementptr inbounds i8, ptr %alloca_count_87, i64 16
  %load_98 = load i64, ptr %gep_96, align 8
  %gep_100 = getelementptr inbounds i8, ptr %alloca_count_87, i64 24
  %load_102 = load ptr, ptr %gep_100, align 8
  %call_103 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.11, i64 %load_90, i64 %load_94, i64 %load_98, ptr %load_102)
  ret i32 0
}

declare i32 @printf(ptr, ...)
