; ModuleID = '08_metaprogramming_patterns'
source_filename = "08_metaprogramming_patterns"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.08_metaprogramming_patterns.0 = constant [47 x i8] c"\F0\9F\93\98 Tutorial: 08_metaprogramming_patterns.fp\0A\00"
@.str.08_metaprogramming_patterns.1 = constant [76 x i8] c"\F0\9F\A7\AD Focus: Metaprogramming: using const metadata to drive code generation\0A\00"
@.str.08_metaprogramming_patterns.2 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.08_metaprogramming_patterns.3 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.08_metaprogramming_patterns.4 = constant [2 x i8] c"\0A\00"
@.str.08_metaprogramming_patterns.5 = constant [15 x i8] c"struct Point3D\00"
@.str.08_metaprogramming_patterns.6 = constant [32 x i8] c"%s has %lld fields (size=%lld)\0A\00"
@.str.08_metaprogramming_patterns.7 = constant [5 x i8] c"i64\0A\00"
@.str.08_metaprogramming_patterns.8 = constant [12 x i8] c"x type: %s\0A\00"
@.str.08_metaprogramming_patterns.9 = constant [9 x i8] c"fields:\0A\00"
@.str.08_metaprogramming_patterns.10 = constant [2 x i8] c"x\00"
@.str.08_metaprogramming_patterns.11 = constant [2 x i8] c"y\00"
@.str.08_metaprogramming_patterns.12 = constant [2 x i8] c"z\00"
@.str.08_metaprogramming_patterns.13 = constant [10 x i8] c"  %s: %s\0A\00"
@.str.08_metaprogramming_patterns.14 = constant [26 x i8] c"point=(%lld, %lld, %lld)\0A\00"
@.str.08_metaprogramming_patterns.15 = constant [7 x i8] c"origin\00"
@.str.08_metaprogramming_patterns.16 = constant [32 x i8] c"labeled=(%lld, %lld, %lld, %s)\0A\00"

define i32 @main() {
bb0:
  %alloca_92 = alloca { i64, i64, i64, ptr }, align 8
  %alloca_count_92 = alloca { i64, i64, i64, ptr }, align 8
  %alloca_79 = alloca { i64, i64, i64 }, align 8
  %alloca_count_79 = alloca { i64, i64, i64 }, align 8
  %alloca_61 = alloca { ptr, ptr }, align 8
  %alloca_count_61 = alloca { ptr, ptr }, align 8
  %alloca_58 = alloca i64, align 8
  %alloca_count_58 = alloca i64, align 8
  %alloca_50 = alloca [3 x { ptr, ptr }], align 8
  %alloca_count_50 = alloca [3 x { ptr, ptr }], align 8
  %alloca_48 = alloca { ptr, ptr }, align 8
  %alloca_count_48 = alloca { ptr, ptr }, align 8
  %alloca_46 = alloca { ptr, ptr }, align 8
  %alloca_count_46 = alloca { ptr, ptr }, align 8
  %alloca_44 = alloca { ptr, ptr }, align 8
  %alloca_count_44 = alloca { ptr, ptr }, align 8
  %alloca_41 = alloca i64, align 8
  %alloca_count_41 = alloca i64, align 8
  %alloca_33 = alloca [3 x { ptr, ptr }], align 8
  %alloca_count_33 = alloca [3 x { ptr, ptr }], align 8
  %alloca_31 = alloca { ptr, ptr }, align 8
  %alloca_count_31 = alloca { ptr, ptr }, align 8
  %alloca_29 = alloca { ptr, ptr }, align 8
  %alloca_count_29 = alloca { ptr, ptr }, align 8
  %alloca_27 = alloca { ptr, ptr }, align 8
  %alloca_count_27 = alloca { ptr, ptr }, align 8
  %alloca_22 = alloca i1, align 1
  %alloca_count_22 = alloca i1, align 1
  %alloca_16 = alloca ptr, align 8
  %alloca_count_16 = alloca ptr, align 8
  %alloca_10 = alloca i64, align 8
  %alloca_count_10 = alloca i64, align 8
  %alloca_8 = alloca i64, align 8
  %alloca_count_8 = alloca i64, align 8
  %alloca_6 = alloca ptr, align 8
  %alloca_count_6 = alloca ptr, align 8
  %alloca_0 = alloca i64, align 8
  %alloca_count_0 = alloca i64, align 8
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.2)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.3)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_5 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  store ptr @.str.08_metaprogramming_patterns.5, ptr %alloca_count_6, align 8
  store i64 3, ptr %alloca_count_8, align 8
  store i64 24, ptr %alloca_count_10, align 8
  %load_12 = load ptr, ptr %alloca_count_6, align 8
  %load_13 = load i64, ptr %alloca_count_8, align 8
  %load_14 = load i64, ptr %alloca_count_10, align 8
  %call_15 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.6, ptr %load_12, i64 %load_13, i64 %load_14)
  br label %bb6

bb6:                                              ; preds = %bb5
  store ptr @.str.08_metaprogramming_patterns.7, ptr %alloca_count_16, align 8
  %load_18 = load ptr, ptr %alloca_count_16, align 8
  %call_19 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.8, ptr %load_18)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_20 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.9)
  br label %bb8

bb8:                                              ; preds = %bb7
  store i64 0, ptr %alloca_count_0, align 8
  br label %bb9

bb9:                                              ; preds = %bb10, %bb8
  %load_23 = load i64, ptr %alloca_count_0, align 8
  %icmp_24 = icmp slt i64 %load_23, 3
  store i1 %icmp_24, ptr %alloca_count_22, align 1
  %load_26 = load i1, ptr %alloca_count_22, align 1
  br i1 %load_26, label %bb10, label %bb11

bb10:                                             ; preds = %bb9
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.10, ptr @.str.08_metaprogramming_patterns.7 }, ptr %alloca_count_27, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.11, ptr @.str.08_metaprogramming_patterns.7 }, ptr %alloca_count_29, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.12, ptr @.str.08_metaprogramming_patterns.7 }, ptr %alloca_count_31, align 8
  %load_34 = load { ptr, ptr }, ptr %alloca_count_27, align 8
  %load_35 = load { ptr, ptr }, ptr %alloca_count_29, align 8
  %load_36 = load { ptr, ptr }, ptr %alloca_count_31, align 8
  %insertvalue_37 = insertvalue [3 x { ptr, ptr }] undef, { ptr, ptr } %load_34, 0
  %insertvalue_38 = insertvalue [3 x { ptr, ptr }] %insertvalue_37, { ptr, ptr } %load_35, 1
  %insertvalue_39 = insertvalue [3 x { ptr, ptr }] %insertvalue_38, { ptr, ptr } %load_36, 2
  store [3 x { ptr, ptr }] %insertvalue_39, ptr %alloca_count_33, align 8
  %load_42 = load i64, ptr %alloca_count_0, align 8
  store i64 %load_42, ptr %alloca_count_41, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.10, ptr @.str.08_metaprogramming_patterns.7 }, ptr %alloca_count_44, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.11, ptr @.str.08_metaprogramming_patterns.7 }, ptr %alloca_count_46, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.12, ptr @.str.08_metaprogramming_patterns.7 }, ptr %alloca_count_48, align 8
  %load_51 = load { ptr, ptr }, ptr %alloca_count_44, align 8
  %load_52 = load { ptr, ptr }, ptr %alloca_count_46, align 8
  %load_53 = load { ptr, ptr }, ptr %alloca_count_48, align 8
  %insertvalue_54 = insertvalue [3 x { ptr, ptr }] undef, { ptr, ptr } %load_51, 0
  %insertvalue_55 = insertvalue [3 x { ptr, ptr }] %insertvalue_54, { ptr, ptr } %load_52, 1
  %insertvalue_56 = insertvalue [3 x { ptr, ptr }] %insertvalue_55, { ptr, ptr } %load_53, 2
  store [3 x { ptr, ptr }] %insertvalue_56, ptr %alloca_count_50, align 8
  %load_59 = load i64, ptr %alloca_count_0, align 8
  store i64 %load_59, ptr %alloca_count_58, align 8
  %load_62 = load i64, ptr %alloca_count_58, align 8
  %iop_63 = mul i64 %load_62, 16
  %gep_65 = getelementptr inbounds i8, ptr %alloca_count_50, i64 %iop_63
  %load_67 = load { ptr, ptr }, ptr %gep_65, align 8
  store { ptr, ptr } %load_67, ptr %alloca_count_61, align 8
  %load_70 = load ptr, ptr %alloca_count_61, align 8
  %gep_72 = getelementptr inbounds i8, ptr %alloca_count_61, i64 8
  %load_74 = load ptr, ptr %gep_72, align 8
  %call_75 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.13, ptr %load_70, ptr %load_74)
  %load_76 = load i64, ptr %alloca_count_0, align 8
  %iop_77 = add i64 %load_76, 1
  store i64 %iop_77, ptr %alloca_count_0, align 8
  br label %bb9

bb11:                                             ; preds = %bb9
  store { i64, i64, i64 } { i64 1, i64 2, i64 3 }, ptr %alloca_count_79, align 8
  %load_82 = load i64, ptr %alloca_count_79, align 8
  %gep_84 = getelementptr inbounds i8, ptr %alloca_count_79, i64 8
  %load_86 = load i64, ptr %gep_84, align 8
  %gep_88 = getelementptr inbounds i8, ptr %alloca_count_79, i64 16
  %load_90 = load i64, ptr %gep_88, align 8
  %call_91 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.14, i64 %load_82, i64 %load_86, i64 %load_90)
  br label %bb12

bb12:                                             ; preds = %bb11
  store { i64, i64, i64, ptr } { i64 4, i64 5, i64 6, ptr @.str.08_metaprogramming_patterns.15 }, ptr %alloca_count_92, align 8
  %load_95 = load i64, ptr %alloca_count_92, align 8
  %gep_97 = getelementptr inbounds i8, ptr %alloca_count_92, i64 8
  %load_99 = load i64, ptr %gep_97, align 8
  %gep_101 = getelementptr inbounds i8, ptr %alloca_count_92, i64 16
  %load_103 = load i64, ptr %gep_101, align 8
  %gep_105 = getelementptr inbounds i8, ptr %alloca_count_92, i64 24
  %load_107 = load ptr, ptr %gep_105, align 8
  %call_108 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.16, i64 %load_95, i64 %load_99, i64 %load_103, ptr %load_107)
  br label %bb13

bb13:                                             ; preds = %bb12
  ret i32 0
}

declare i32 @printf(ptr, ...)
