; ModuleID = '08_metaprogramming_patterns'
source_filename = "08_metaprogramming_patterns"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.08_metaprogramming_patterns.0 = constant [47 x i8] c"\F0\9F\93\98 Tutorial: 08_metaprogramming_patterns.fp\0A\00"
@.str.08_metaprogramming_patterns.1 = constant [70 x i8] c"\F0\9F\A7\AD Focus: Metaprogramming: const metadata + quote/splice execution\0A\00"
@.str.08_metaprogramming_patterns.2 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.08_metaprogramming_patterns.3 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.08_metaprogramming_patterns.4 = constant [2 x i8] c"\0A\00"
@.str.08_metaprogramming_patterns.5 = constant [32 x i8] c"=== Part 1: Const Metadata ===\0A\00"
@.str.08_metaprogramming_patterns.6 = constant [15 x i8] c"struct Point3D\00"
@.str.08_metaprogramming_patterns.7 = constant [32 x i8] c"%s has %lld fields (size=%lld)\0A\00"
@.str.08_metaprogramming_patterns.8 = constant [5 x i8] c"i64\0A\00"
@.str.08_metaprogramming_patterns.9 = constant [12 x i8] c"x type: %s\0A\00"
@.str.08_metaprogramming_patterns.10 = constant [9 x i8] c"fields:\0A\00"
@.str.08_metaprogramming_patterns.11 = constant [2 x i8] c"x\00"
@.str.08_metaprogramming_patterns.12 = constant [2 x i8] c"y\00"
@.str.08_metaprogramming_patterns.13 = constant [2 x i8] c"z\00"
@.str.08_metaprogramming_patterns.14 = constant [10 x i8] c"  %s: %s\0A\00"
@.str.08_metaprogramming_patterns.15 = constant [26 x i8] c"point=(%lld, %lld, %lld)\0A\00"
@.str.08_metaprogramming_patterns.16 = constant [7 x i8] c"origin\00"
@.str.08_metaprogramming_patterns.17 = constant [32 x i8] c"labeled=(%lld, %lld, %lld, %s)\0A\00"
@.str.08_metaprogramming_patterns.18 = constant [37 x i8] c"=== Part 2: Execute Quoted Code ===\0A\00"
@.str.08_metaprogramming_patterns.19 = constant [21 x i8] c"expr splice => %lld\0A\00"
@.str.08_metaprogramming_patterns.20 = constant [10 x i8] c"inspected\00"
@.str.08_metaprogramming_patterns.21 = constant [30 x i8] c"quote<fn> inspect => name=%s\0A\00"
@.str.08_metaprogramming_patterns.22 = constant [29 x i8] c"quote<[item]> count => %lld\0A\00"
@.str.08_metaprogramming_patterns.23 = constant [26 x i8] c"stmt splice => step=%lld\0A\00"
@.str.08_metaprogramming_patterns.24 = constant [16 x i8] c"metaprogramming\00"
@.str.08_metaprogramming_patterns.25 = constant [25 x i8] c"item splice => %s #%lld\0A\00"

define i32 @main() {
bb0:
  %alloca_128 = alloca { ptr, i64 }, align 8
  %alloca_count_128 = alloca { ptr, i64 }, align 8
  %alloca_124 = alloca i64, align 8
  %alloca_count_124 = alloca i64, align 8
  %alloca_120 = alloca i64, align 8
  %alloca_count_120 = alloca i64, align 8
  %alloca_116 = alloca ptr, align 8
  %alloca_count_116 = alloca ptr, align 8
  %alloca_112 = alloca i64, align 8
  %alloca_count_112 = alloca i64, align 8
  %alloca_93 = alloca { i64, i64, i64, ptr }, align 8
  %alloca_count_93 = alloca { i64, i64, i64, ptr }, align 8
  %alloca_80 = alloca { i64, i64, i64 }, align 8
  %alloca_count_80 = alloca { i64, i64, i64 }, align 8
  %alloca_62 = alloca { ptr, ptr }, align 8
  %alloca_count_62 = alloca { ptr, ptr }, align 8
  %alloca_59 = alloca i64, align 8
  %alloca_count_59 = alloca i64, align 8
  %alloca_51 = alloca [3 x { ptr, ptr }], align 8
  %alloca_count_51 = alloca [3 x { ptr, ptr }], align 8
  %alloca_49 = alloca { ptr, ptr }, align 8
  %alloca_count_49 = alloca { ptr, ptr }, align 8
  %alloca_47 = alloca { ptr, ptr }, align 8
  %alloca_count_47 = alloca { ptr, ptr }, align 8
  %alloca_45 = alloca { ptr, ptr }, align 8
  %alloca_count_45 = alloca { ptr, ptr }, align 8
  %alloca_42 = alloca i64, align 8
  %alloca_count_42 = alloca i64, align 8
  %alloca_34 = alloca [3 x { ptr, ptr }], align 8
  %alloca_count_34 = alloca [3 x { ptr, ptr }], align 8
  %alloca_32 = alloca { ptr, ptr }, align 8
  %alloca_count_32 = alloca { ptr, ptr }, align 8
  %alloca_30 = alloca { ptr, ptr }, align 8
  %alloca_count_30 = alloca { ptr, ptr }, align 8
  %alloca_28 = alloca { ptr, ptr }, align 8
  %alloca_count_28 = alloca { ptr, ptr }, align 8
  %alloca_23 = alloca i1, align 1
  %alloca_count_23 = alloca i1, align 1
  %alloca_17 = alloca ptr, align 8
  %alloca_count_17 = alloca ptr, align 8
  %alloca_11 = alloca i64, align 8
  %alloca_count_11 = alloca i64, align 8
  %alloca_9 = alloca i64, align 8
  %alloca_count_9 = alloca i64, align 8
  %alloca_7 = alloca ptr, align 8
  %alloca_count_7 = alloca ptr, align 8
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
  %call_6 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.5)
  br label %bb6

bb6:                                              ; preds = %bb5
  store ptr @.str.08_metaprogramming_patterns.6, ptr %alloca_count_7, align 8
  store i64 3, ptr %alloca_count_9, align 8
  store i64 24, ptr %alloca_count_11, align 8
  %load_13 = load ptr, ptr %alloca_count_7, align 8
  %load_14 = load i64, ptr %alloca_count_9, align 8
  %load_15 = load i64, ptr %alloca_count_11, align 8
  %call_16 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.7, ptr %load_13, i64 %load_14, i64 %load_15)
  br label %bb7

bb7:                                              ; preds = %bb6
  store ptr @.str.08_metaprogramming_patterns.8, ptr %alloca_count_17, align 8
  %load_19 = load ptr, ptr %alloca_count_17, align 8
  %call_20 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.9, ptr %load_19)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_21 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.10)
  br label %bb9

bb9:                                              ; preds = %bb8
  store i64 0, ptr %alloca_count_0, align 8
  br label %bb10

bb10:                                             ; preds = %bb11, %bb9
  %load_24 = load i64, ptr %alloca_count_0, align 8
  %icmp_25 = icmp slt i64 %load_24, 3
  store i1 %icmp_25, ptr %alloca_count_23, align 1
  %load_27 = load i1, ptr %alloca_count_23, align 1
  br i1 %load_27, label %bb11, label %bb12

bb11:                                             ; preds = %bb10
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.11, ptr @.str.08_metaprogramming_patterns.8 }, ptr %alloca_count_28, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.12, ptr @.str.08_metaprogramming_patterns.8 }, ptr %alloca_count_30, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.13, ptr @.str.08_metaprogramming_patterns.8 }, ptr %alloca_count_32, align 8
  %load_35 = load { ptr, ptr }, ptr %alloca_count_28, align 8
  %load_36 = load { ptr, ptr }, ptr %alloca_count_30, align 8
  %load_37 = load { ptr, ptr }, ptr %alloca_count_32, align 8
  %insertvalue_38 = insertvalue [3 x { ptr, ptr }] undef, { ptr, ptr } %load_35, 0
  %insertvalue_39 = insertvalue [3 x { ptr, ptr }] %insertvalue_38, { ptr, ptr } %load_36, 1
  %insertvalue_40 = insertvalue [3 x { ptr, ptr }] %insertvalue_39, { ptr, ptr } %load_37, 2
  store [3 x { ptr, ptr }] %insertvalue_40, ptr %alloca_count_34, align 8
  %load_43 = load i64, ptr %alloca_count_0, align 8
  store i64 %load_43, ptr %alloca_count_42, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.11, ptr @.str.08_metaprogramming_patterns.8 }, ptr %alloca_count_45, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.12, ptr @.str.08_metaprogramming_patterns.8 }, ptr %alloca_count_47, align 8
  store { ptr, ptr } { ptr @.str.08_metaprogramming_patterns.13, ptr @.str.08_metaprogramming_patterns.8 }, ptr %alloca_count_49, align 8
  %load_52 = load { ptr, ptr }, ptr %alloca_count_45, align 8
  %load_53 = load { ptr, ptr }, ptr %alloca_count_47, align 8
  %load_54 = load { ptr, ptr }, ptr %alloca_count_49, align 8
  %insertvalue_55 = insertvalue [3 x { ptr, ptr }] undef, { ptr, ptr } %load_52, 0
  %insertvalue_56 = insertvalue [3 x { ptr, ptr }] %insertvalue_55, { ptr, ptr } %load_53, 1
  %insertvalue_57 = insertvalue [3 x { ptr, ptr }] %insertvalue_56, { ptr, ptr } %load_54, 2
  store [3 x { ptr, ptr }] %insertvalue_57, ptr %alloca_count_51, align 8
  %load_60 = load i64, ptr %alloca_count_0, align 8
  store i64 %load_60, ptr %alloca_count_59, align 8
  %load_63 = load i64, ptr %alloca_count_59, align 8
  %iop_64 = mul i64 %load_63, 16
  %gep_66 = getelementptr inbounds i8, ptr %alloca_count_51, i64 %iop_64
  %load_68 = load { ptr, ptr }, ptr %gep_66, align 8
  store { ptr, ptr } %load_68, ptr %alloca_count_62, align 8
  %load_71 = load ptr, ptr %alloca_count_62, align 8
  %gep_73 = getelementptr inbounds i8, ptr %alloca_count_62, i64 8
  %load_75 = load ptr, ptr %gep_73, align 8
  %call_76 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.14, ptr %load_71, ptr %load_75)
  %load_77 = load i64, ptr %alloca_count_0, align 8
  %iop_78 = add i64 %load_77, 1
  store i64 %iop_78, ptr %alloca_count_0, align 8
  br label %bb10

bb12:                                             ; preds = %bb10
  store { i64, i64, i64 } { i64 1, i64 2, i64 3 }, ptr %alloca_count_80, align 8
  %load_83 = load i64, ptr %alloca_count_80, align 8
  %gep_85 = getelementptr inbounds i8, ptr %alloca_count_80, i64 8
  %load_87 = load i64, ptr %gep_85, align 8
  %gep_89 = getelementptr inbounds i8, ptr %alloca_count_80, i64 16
  %load_91 = load i64, ptr %gep_89, align 8
  %call_92 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.15, i64 %load_83, i64 %load_87, i64 %load_91)
  br label %bb13

bb13:                                             ; preds = %bb12
  store { i64, i64, i64, ptr } { i64 4, i64 5, i64 6, ptr @.str.08_metaprogramming_patterns.16 }, ptr %alloca_count_93, align 8
  %load_96 = load i64, ptr %alloca_count_93, align 8
  %gep_98 = getelementptr inbounds i8, ptr %alloca_count_93, i64 8
  %load_100 = load i64, ptr %gep_98, align 8
  %gep_102 = getelementptr inbounds i8, ptr %alloca_count_93, i64 16
  %load_104 = load i64, ptr %gep_102, align 8
  %gep_106 = getelementptr inbounds i8, ptr %alloca_count_93, i64 24
  %load_108 = load ptr, ptr %gep_106, align 8
  %call_109 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.17, i64 %load_96, i64 %load_100, i64 %load_104, ptr %load_108)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_110 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.4)
  br label %bb15

bb15:                                             ; preds = %bb14
  %call_111 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.18)
  br label %bb16

bb16:                                             ; preds = %bb15
  store i64 20, ptr %alloca_count_112, align 8
  %load_114 = load i64, ptr %alloca_count_112, align 8
  %call_115 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.19, i64 %load_114)
  br label %bb17

bb17:                                             ; preds = %bb16
  store ptr @.str.08_metaprogramming_patterns.20, ptr %alloca_count_116, align 8
  %load_118 = load ptr, ptr %alloca_count_116, align 8
  %call_119 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.21, ptr %load_118)
  br label %bb18

bb18:                                             ; preds = %bb17
  store i64 2, ptr %alloca_count_120, align 8
  %load_122 = load i64, ptr %alloca_count_120, align 8
  %call_123 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.22, i64 %load_122)
  br label %bb19

bb19:                                             ; preds = %bb18
  store i64 21, ptr %alloca_count_124, align 8
  %load_126 = load i64, ptr %alloca_count_124, align 8
  %call_127 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.23, i64 %load_126)
  br label %bb20

bb20:                                             ; preds = %bb19
  %load_129 = load i64, ptr %alloca_count_112, align 8
  %insertvalue_131 = insertvalue { ptr, i64 } { ptr @.str.08_metaprogramming_patterns.24, i64 undef }, i64 %load_129, 1
  store { ptr, i64 } %insertvalue_131, ptr %alloca_count_128, align 8
  %load_134 = load ptr, ptr %alloca_count_128, align 8
  %gep_136 = getelementptr inbounds i8, ptr %alloca_count_128, i64 8
  %load_138 = load i64, ptr %gep_136, align 8
  %call_139 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.25, ptr %load_134, i64 %load_138)
  br label %bb21

bb21:                                             ; preds = %bb20
  ret i32 0
}

declare i32 @printf(ptr, ...)
