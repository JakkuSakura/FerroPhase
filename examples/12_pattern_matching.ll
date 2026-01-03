; ModuleID = '12_pattern_matching'
source_filename = "12_pattern_matching"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.12_pattern_matching.0 = private unnamed_addr constant [4 x i8] c"red\00", align 1
@.str.12_pattern_matching.1 = private unnamed_addr constant [6 x i8] c"green\00", align 1
@.str.12_pattern_matching.2 = private unnamed_addr constant [8 x i8] c"red rgb\00", align 1
@.str.12_pattern_matching.3 = private unnamed_addr constant [11 x i8] c"custom rgb\00", align 1
@.str.12_pattern_matching.4 = private unnamed_addr constant [5 x i8] c"zero\00", align 1
@.str.12_pattern_matching.5 = private unnamed_addr constant [9 x i8] c"negative\00", align 1
@.str.12_pattern_matching.6 = private unnamed_addr constant [5 x i8] c"even\00", align 1
@.str.12_pattern_matching.7 = private unnamed_addr constant [4 x i8] c"odd\00", align 1
@.str.12_pattern_matching.8 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str.12_pattern_matching.9 = private unnamed_addr constant [6 x i8] c"%lld\0A\00", align 1
@.str.12_pattern_matching.10 = private unnamed_addr constant [8 x i8] c"0x%06X\0A\00", align 1

define internal ptr @describe(ptr %0) {
bb0:
  %alloca_49 = alloca i8, align 1
  %alloca_count_49 = alloca i8, align 1
  %alloca_42 = alloca i8, align 1
  %alloca_count_42 = alloca i8, align 1
  %alloca_35 = alloca i8, align 1
  %alloca_count_35 = alloca i8, align 1
  %alloca_28 = alloca i1, align 1
  %alloca_count_28 = alloca i1, align 1
  %alloca_20 = alloca i1, align 1
  %alloca_count_20 = alloca i1, align 1
  %alloca_11 = alloca i1, align 1
  %alloca_count_11 = alloca i1, align 1
  %alloca_3 = alloca i1, align 1
  %alloca_count_3 = alloca i1, align 1
  %alloca_1 = alloca ptr, align 8
  %alloca_count_1 = alloca ptr, align 8
  %alloca_0 = alloca ptr, align 8
  %alloca_count_0 = alloca ptr, align 8
  store ptr %0, ptr %alloca_count_1, align 8
  %load_4 = load ptr, ptr %alloca_count_1, align 8
  %load_6 = load i64, ptr %load_4, align 8
  %icmp_7 = icmp eq i64 %load_6, 0
  store i1 %icmp_7, ptr %alloca_count_3, align 1
  %load_9 = load i1, ptr %alloca_count_3, align 1
  br i1 %load_9, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  store ptr @.str.12_pattern_matching.0, ptr %alloca_count_0, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  %load_12 = load ptr, ptr %alloca_count_1, align 8
  %load_14 = load i64, ptr %load_12, align 8
  %icmp_15 = icmp eq i64 %load_14, 1
  store i1 %icmp_15, ptr %alloca_count_11, align 1
  %load_17 = load i1, ptr %alloca_count_11, align 1
  br i1 %load_17, label %bb4, label %bb5

bb1:                                              ; preds = %bb9, %bb8, %bb6, %bb4, %bb2
  %load_18 = load ptr, ptr %alloca_count_0, align 8
  ret ptr %load_18

bb4:                                              ; preds = %bb3
  store ptr @.str.12_pattern_matching.1, ptr %alloca_count_0, align 8
  br label %bb1

bb5:                                              ; preds = %bb3
  %load_21 = load ptr, ptr %alloca_count_1, align 8
  %load_23 = load i64, ptr %load_21, align 8
  %icmp_24 = icmp eq i64 %load_23, 2
  store i1 %icmp_24, ptr %alloca_count_20, align 1
  %load_26 = load i1, ptr %alloca_count_20, align 1
  br i1 %load_26, label %bb6, label %bb7

bb6:                                              ; preds = %bb5
  store ptr @.str.12_pattern_matching.2, ptr %alloca_count_0, align 8
  br label %bb1

bb7:                                              ; preds = %bb5
  %load_29 = load ptr, ptr %alloca_count_1, align 8
  %load_31 = load i64, ptr %load_29, align 8
  %icmp_32 = icmp eq i64 %load_31, 2
  store i1 %icmp_32, ptr %alloca_count_28, align 1
  %load_34 = load i1, ptr %alloca_count_28, align 1
  br i1 %load_34, label %bb8, label %bb9

bb8:                                              ; preds = %bb7
  %load_36 = load ptr, ptr %alloca_count_1, align 8
  %gep_38 = getelementptr inbounds i8, ptr %load_36, i64 8
  %load_40 = load i8, ptr %gep_38, align 1
  store i8 %load_40, ptr %alloca_count_35, align 1
  %load_43 = load ptr, ptr %alloca_count_1, align 8
  %gep_45 = getelementptr inbounds i8, ptr %load_43, i64 9
  %load_47 = load i8, ptr %gep_45, align 1
  store i8 %load_47, ptr %alloca_count_42, align 1
  %load_50 = load ptr, ptr %alloca_count_1, align 8
  %gep_52 = getelementptr inbounds i8, ptr %load_50, i64 10
  %load_54 = load i8, ptr %gep_52, align 1
  store i8 %load_54, ptr %alloca_count_49, align 1
  store ptr @.str.12_pattern_matching.3, ptr %alloca_count_0, align 8
  br label %bb1

bb9:                                              ; preds = %bb7
  br label %bb1
}

define internal ptr @classify(i64 %0) {
bb0:
  %alloca_83 = alloca i1, align 1
  %alloca_count_83 = alloca i1, align 1
  %alloca_79 = alloca i64, align 8
  %alloca_count_79 = alloca i64, align 8
  %alloca_76 = alloca i64, align 8
  %alloca_count_76 = alloca i64, align 8
  %alloca_70 = alloca i1, align 1
  %alloca_count_70 = alloca i1, align 1
  %alloca_67 = alloca i64, align 8
  %alloca_count_67 = alloca i64, align 8
  %alloca_60 = alloca i1, align 1
  %alloca_count_60 = alloca i1, align 1
  %alloca_58 = alloca i64, align 8
  %alloca_count_58 = alloca i64, align 8
  %alloca_57 = alloca ptr, align 8
  %alloca_count_57 = alloca ptr, align 8
  store i64 %0, ptr %alloca_count_58, align 8
  %load_61 = load i64, ptr %alloca_count_58, align 8
  %icmp_62 = icmp eq i64 %load_61, 0
  store i1 %icmp_62, ptr %alloca_count_60, align 1
  %load_64 = load i1, ptr %alloca_count_60, align 1
  br i1 %load_64, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  store ptr @.str.12_pattern_matching.4, ptr %alloca_count_57, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  br label %bb4

bb1:                                              ; preds = %bb10, %bb9, %bb6, %bb2
  %load_66 = load ptr, ptr %alloca_count_57, align 8
  ret ptr %load_66

bb4:                                              ; preds = %bb3
  %load_68 = load i64, ptr %alloca_count_58, align 8
  store i64 %load_68, ptr %alloca_count_67, align 8
  %load_71 = load i64, ptr %alloca_count_67, align 8
  %icmp_72 = icmp slt i64 %load_71, 0
  store i1 %icmp_72, ptr %alloca_count_70, align 1
  %load_74 = load i1, ptr %alloca_count_70, align 1
  br i1 %load_74, label %bb6, label %bb5

bb5:                                              ; preds = %bb4
  br label %bb7

bb6:                                              ; preds = %bb4
  store ptr @.str.12_pattern_matching.5, ptr %alloca_count_57, align 8
  br label %bb1

bb7:                                              ; preds = %bb5
  %load_77 = load i64, ptr %alloca_count_58, align 8
  store i64 %load_77, ptr %alloca_count_76, align 8
  %load_80 = load i64, ptr %alloca_count_76, align 8
  %iop_81 = srem i64 %load_80, 2
  store i64 %iop_81, ptr %alloca_count_79, align 8
  %load_84 = load i64, ptr %alloca_count_79, align 8
  %icmp_85 = icmp eq i64 %load_84, 0
  store i1 %icmp_85, ptr %alloca_count_83, align 1
  %load_87 = load i1, ptr %alloca_count_83, align 1
  br i1 %load_87, label %bb9, label %bb8

bb8:                                              ; preds = %bb7
  br label %bb10

bb9:                                              ; preds = %bb7
  store ptr @.str.12_pattern_matching.6, ptr %alloca_count_57, align 8
  br label %bb1

bb10:                                             ; preds = %bb8
  store ptr @.str.12_pattern_matching.7, ptr %alloca_count_57, align 8
  br label %bb1

bb11:                                             ; No predecessors!
  %load_90 = load ptr, ptr %alloca_count_57, align 8
  ret ptr %load_90
}

define internal i64 @unwrap_or({ i64, i64 } %0, i64 %1) {
bb0:
  %alloca_108 = alloca i1, align 1
  %alloca_count_108 = alloca i1, align 1
  %alloca_100 = alloca i64, align 8
  %alloca_count_100 = alloca i64, align 8
  %alloca_94 = alloca i1, align 1
  %alloca_count_94 = alloca i1, align 1
  %alloca_92 = alloca { i64, i64 }, align 8
  %alloca_count_92 = alloca { i64, i64 }, align 8
  %alloca_91 = alloca i64, align 8
  %alloca_count_91 = alloca i64, align 8
  store { i64, i64 } %0, ptr %alloca_count_92, align 8
  %load_96 = load i64, ptr %alloca_count_92, align 8
  %icmp_97 = icmp eq i64 %load_96, 0
  store i1 %icmp_97, ptr %alloca_count_94, align 1
  %load_99 = load i1, ptr %alloca_count_94, align 1
  br i1 %load_99, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  %gep_102 = getelementptr inbounds i8, ptr %alloca_count_92, i64 8
  %load_104 = load i64, ptr %gep_102, align 8
  store i64 %load_104, ptr %alloca_count_100, align 8
  %load_106 = load i64, ptr %alloca_count_100, align 8
  store i64 %load_106, ptr %alloca_count_91, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  %load_110 = load i64, ptr %alloca_count_92, align 8
  %icmp_111 = icmp eq i64 %load_110, 1
  store i1 %icmp_111, ptr %alloca_count_108, align 1
  %load_113 = load i1, ptr %alloca_count_108, align 1
  br i1 %load_113, label %bb4, label %bb5

bb1:                                              ; preds = %bb5, %bb4, %bb2
  %load_114 = load i64, ptr %alloca_count_91, align 8
  ret i64 %load_114

bb4:                                              ; preds = %bb3
  store i64 %1, ptr %alloca_count_91, align 8
  br label %bb1

bb5:                                              ; preds = %bb3
  br label %bb1
}

define i32 @main() {
bb0:
  %alloca_155 = alloca i64, align 8
  %alloca_count_155 = alloca i64, align 8
  %alloca_150 = alloca { i64, i64 }, align 8
  %alloca_count_150 = alloca { i64, i64 }, align 8
  %alloca_145 = alloca { i64, i64 }, align 8
  %alloca_count_145 = alloca { i64, i64 }, align 8
  %alloca_133 = alloca i64, align 8
  %alloca_count_133 = alloca i64, align 8
  %alloca_128 = alloca ptr, align 8
  %alloca_count_128 = alloca ptr, align 8
  %alloca_123 = alloca ptr, align 8
  %alloca_count_123 = alloca ptr, align 8
  %alloca_121 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_count_121 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_118 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_count_118 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_116 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_count_116 = alloca { i64, i8, i8, i8 }, align 8
  store { i64, i8, i8, i8 } zeroinitializer, ptr %alloca_count_116, align 8
  %load_119 = load { i64, i8, i8, i8 }, ptr %alloca_count_116, align 8
  store { i64, i8, i8, i8 } %load_119, ptr %alloca_count_118, align 8
  store { i64, i8, i8, i8 } { i64 2, i8 -128, i8 64, i8 32 }, ptr %alloca_count_121, align 8
  store ptr %alloca_count_118, ptr %alloca_count_123, align 8
  %load_125 = load ptr, ptr %alloca_count_123, align 8
  %call_126 = call ptr @describe(ptr %load_125)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_127 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_126)
  store ptr %alloca_count_121, ptr %alloca_count_128, align 8
  %load_130 = load ptr, ptr %alloca_count_128, align 8
  %call_131 = call ptr @describe(ptr %load_130)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_132 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_131)
  store i64 -5, ptr %alloca_count_133, align 8
  %load_136 = load i64, ptr %alloca_count_133, align 8
  %call_137 = call ptr @classify(i64 %load_136)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_138 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_137)
  %call_139 = call ptr @classify(i64 0)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_140 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_139)
  %call_141 = call ptr @classify(i64 4)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_142 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_141)
  %call_143 = call ptr @classify(i64 7)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_144 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_143)
  store { i64, i64 } { i64 0, i64 42 }, ptr %alloca_count_145, align 8
  %load_147 = load { i64, i64 }, ptr %alloca_count_145, align 8
  %call_148 = call i64 @unwrap_or({ i64, i64 } %load_147, i64 0)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_149 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.9, i64 %call_148)
  store { i64, i64 } { i64 1, i64 0 }, ptr %alloca_count_150, align 8
  %load_152 = load { i64, i64 }, ptr %alloca_count_150, align 8
  %call_153 = call i64 @unwrap_or({ i64, i64 } %load_152, i64 99)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_154 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.9, i64 %call_153)
  store i64 0, ptr %alloca_count_155, align 8
  %load_157 = load i64, ptr %alloca_count_155, align 8
  %call_158 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.10, i64 %load_157)
  ret i32 0
}

declare i32 @printf(ptr, ...)
