; ModuleID = '08_metaprogramming_patterns'
source_filename = "08_metaprogramming_patterns"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.08_metaprogramming_patterns.0 = private unnamed_addr constant [47 x i8] c"\F0\9F\93\98 Tutorial: 08_metaprogramming_patterns.fp\0A\00", align 1
@.str.08_metaprogramming_patterns.1 = private unnamed_addr constant [76 x i8] c"\F0\9F\A7\AD Focus: Metaprogramming: using const metadata to drive code generation\0A\00", align 1
@.str.08_metaprogramming_patterns.2 = private unnamed_addr constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00", align 1
@.str.08_metaprogramming_patterns.3 = private unnamed_addr constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00", align 1
@.str.08_metaprogramming_patterns.4 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@.str.08_metaprogramming_patterns.5 = private unnamed_addr constant [15 x i8] c"struct Point3D\00", align 1
@.str.08_metaprogramming_patterns.6 = private unnamed_addr constant [32 x i8] c"%s has %lld fields (size=%lld)\0A\00", align 1
@.str.08_metaprogramming_patterns.7 = private unnamed_addr constant [5 x i8] c"i64\0A\00", align 1
@.str.08_metaprogramming_patterns.8 = private unnamed_addr constant [12 x i8] c"x type: %s\0A\00", align 1
@.str.08_metaprogramming_patterns.9 = private unnamed_addr constant [9 x i8] c"fields:\0A\00", align 1
@.str.08_metaprogramming_patterns.10 = private unnamed_addr constant [10 x i8] c"  %s: %s\0A\00", align 1
@.str.08_metaprogramming_patterns.11 = private unnamed_addr constant [26 x i8] c"point=(%lld, %lld, %lld)\0A\00", align 1
@.str.08_metaprogramming_patterns.12 = private unnamed_addr constant [7 x i8] c"origin\00", align 1
@.str.08_metaprogramming_patterns.13 = private unnamed_addr constant [32 x i8] c"labeled=(%lld, %lld, %lld, %s)\0A\00", align 1

define i32 @main() {
bb0:
  %alloca_77 = alloca { i64, i64, i64, ptr }, align 8
  %alloca_count_77 = alloca { i64, i64, i64, ptr }, align 8
  %alloca_64 = alloca { i64, i64, i64 }, align 8
  %alloca_count_64 = alloca { i64, i64, i64 }, align 8
  %alloca_44 = alloca { ptr, ptr }, align 8
  %alloca_count_44 = alloca { ptr, ptr }, align 8
  %alloca_41 = alloca i64, align 8
  %alloca_count_41 = alloca i64, align 8
  %alloca_39 = alloca { ptr, i64 }, align 8
  %alloca_count_39 = alloca { ptr, i64 }, align 8
  %alloca_36 = alloca i64, align 8
  %alloca_count_36 = alloca i64, align 8
  %alloca_34 = alloca { ptr, i64 }, align 8
  %alloca_count_34 = alloca { ptr, i64 }, align 8
  %alloca_28 = alloca i1, align 1
  %alloca_count_28 = alloca i1, align 1
  %alloca_24 = alloca i64, align 8
  %alloca_count_24 = alloca i64, align 8
  %alloca_22 = alloca { ptr, i64 }, align 8
  %alloca_count_22 = alloca { ptr, i64 }, align 8
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
  store { ptr, i64 } zeroinitializer, ptr %alloca_count_22, align 8
  %load_25 = load { ptr, i64 }, ptr %alloca_count_22, align 8
  %extractvalue_26 = extractvalue { ptr, i64 } %load_25, 1
  store i64 %extractvalue_26, ptr %alloca_count_24, align 8
  %load_29 = load i64, ptr %alloca_count_0, align 8
  %load_30 = load i64, ptr %alloca_count_24, align 8
  %icmp_31 = icmp slt i64 %load_29, %load_30
  store i1 %icmp_31, ptr %alloca_count_28, align 1
  %load_33 = load i1, ptr %alloca_count_28, align 1
  br i1 %load_33, label %bb10, label %bb11

bb10:                                             ; preds = %bb9
  store { ptr, i64 } zeroinitializer, ptr %alloca_count_34, align 8
  %load_37 = load i64, ptr %alloca_count_0, align 8
  store i64 %load_37, ptr %alloca_count_36, align 8
  store { ptr, i64 } zeroinitializer, ptr %alloca_count_39, align 8
  %load_42 = load i64, ptr %alloca_count_0, align 8
  store i64 %load_42, ptr %alloca_count_41, align 8
  %load_45 = load { ptr, i64 }, ptr %alloca_count_39, align 8
  %extractvalue_46 = extractvalue { ptr, i64 } %load_45, 0
  %load_47 = load i64, ptr %alloca_count_41, align 8
  %iop_48 = mul i64 %load_47, 16
  %gep_50 = getelementptr inbounds i8, ptr %extractvalue_46, i64 %iop_48
  %load_52 = load { ptr, ptr }, ptr %gep_50, align 8
  store { ptr, ptr } %load_52, ptr %alloca_count_44, align 8
  %load_55 = load ptr, ptr %alloca_count_44, align 8
  %gep_57 = getelementptr inbounds i8, ptr %alloca_count_44, i64 8
  %load_59 = load ptr, ptr %gep_57, align 8
  %call_60 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.10, ptr %load_55, ptr %load_59)
  %load_61 = load i64, ptr %alloca_count_0, align 8
  %iop_62 = add i64 %load_61, 1
  store i64 %iop_62, ptr %alloca_count_0, align 8
  br label %bb9

bb11:                                             ; preds = %bb9
  store { i64, i64, i64 } { i64 1, i64 2, i64 3 }, ptr %alloca_count_64, align 8
  %load_67 = load i64, ptr %alloca_count_64, align 8
  %gep_69 = getelementptr inbounds i8, ptr %alloca_count_64, i64 8
  %load_71 = load i64, ptr %gep_69, align 8
  %gep_73 = getelementptr inbounds i8, ptr %alloca_count_64, i64 16
  %load_75 = load i64, ptr %gep_73, align 8
  %call_76 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.11, i64 %load_67, i64 %load_71, i64 %load_75)
  br label %bb12

bb12:                                             ; preds = %bb11
  store { i64, i64, i64, ptr } { i64 4, i64 5, i64 6, ptr @.str.08_metaprogramming_patterns.12 }, ptr %alloca_count_77, align 8
  %load_80 = load i64, ptr %alloca_count_77, align 8
  %gep_82 = getelementptr inbounds i8, ptr %alloca_count_77, i64 8
  %load_84 = load i64, ptr %gep_82, align 8
  %gep_86 = getelementptr inbounds i8, ptr %alloca_count_77, i64 16
  %load_88 = load i64, ptr %gep_86, align 8
  %gep_90 = getelementptr inbounds i8, ptr %alloca_count_77, i64 24
  %load_92 = load ptr, ptr %gep_90, align 8
  %call_93 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.13, i64 %load_80, i64 %load_84, i64 %load_88, ptr %load_92)
  br label %bb13

bb13:                                             ; preds = %bb12
  ret i32 0
}

declare i32 @printf(ptr, ...)
