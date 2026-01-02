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
  store i1 false, ptr %alloca_count_0, align 8
  br label %bb1
}

define internal ptr @classify(i64 %0) {
bb0:
  %alloca_84 = alloca i1, align 1
  %alloca_count_84 = alloca i1, align 1
  %alloca_80 = alloca i64, align 8
  %alloca_count_80 = alloca i64, align 8
  %alloca_77 = alloca i64, align 8
  %alloca_count_77 = alloca i64, align 8
  %alloca_71 = alloca i1, align 1
  %alloca_count_71 = alloca i1, align 1
  %alloca_68 = alloca i64, align 8
  %alloca_count_68 = alloca i64, align 8
  %alloca_61 = alloca i1, align 1
  %alloca_count_61 = alloca i1, align 1
  %alloca_59 = alloca i64, align 8
  %alloca_count_59 = alloca i64, align 8
  %alloca_58 = alloca ptr, align 8
  %alloca_count_58 = alloca ptr, align 8
  store i64 %0, ptr %alloca_count_59, align 8
  %load_62 = load i64, ptr %alloca_count_59, align 8
  %icmp_63 = icmp eq i64 %load_62, 0
  store i1 %icmp_63, ptr %alloca_count_61, align 1
  %load_65 = load i1, ptr %alloca_count_61, align 1
  br i1 %load_65, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  store ptr @.str.12_pattern_matching.4, ptr %alloca_count_58, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  br label %bb4

bb1:                                              ; preds = %bb10, %bb9, %bb6, %bb2
  %load_67 = load ptr, ptr %alloca_count_58, align 8
  ret ptr %load_67

bb4:                                              ; preds = %bb3
  %load_69 = load i64, ptr %alloca_count_59, align 8
  store i64 %load_69, ptr %alloca_count_68, align 8
  %load_72 = load i64, ptr %alloca_count_68, align 8
  %icmp_73 = icmp slt i64 %load_72, 0
  store i1 %icmp_73, ptr %alloca_count_71, align 1
  %load_75 = load i1, ptr %alloca_count_71, align 1
  br i1 %load_75, label %bb6, label %bb5

bb5:                                              ; preds = %bb4
  br label %bb7

bb6:                                              ; preds = %bb4
  store ptr @.str.12_pattern_matching.5, ptr %alloca_count_58, align 8
  br label %bb1

bb7:                                              ; preds = %bb5
  %load_78 = load i64, ptr %alloca_count_59, align 8
  store i64 %load_78, ptr %alloca_count_77, align 8
  %load_81 = load i64, ptr %alloca_count_77, align 8
  %iop_82 = srem i64 %load_81, 2
  store i64 %iop_82, ptr %alloca_count_80, align 8
  %load_85 = load i64, ptr %alloca_count_80, align 8
  %icmp_86 = icmp eq i64 %load_85, 0
  store i1 %icmp_86, ptr %alloca_count_84, align 1
  %load_88 = load i1, ptr %alloca_count_84, align 1
  br i1 %load_88, label %bb9, label %bb8

bb8:                                              ; preds = %bb7
  br label %bb10

bb9:                                              ; preds = %bb7
  store ptr @.str.12_pattern_matching.6, ptr %alloca_count_58, align 8
  br label %bb1

bb10:                                             ; preds = %bb8
  store ptr @.str.12_pattern_matching.7, ptr %alloca_count_58, align 8
  br label %bb1

bb11:                                             ; No predecessors!
  store i1 false, ptr %alloca_count_58, align 8
  %load_92 = load ptr, ptr %alloca_count_58, align 8
  ret ptr %load_92
}

define internal i64 @unwrap_or({ i64, i64 } %0, i64 %1) {
bb0:
  %alloca_110 = alloca i1, align 1
  %alloca_count_110 = alloca i1, align 1
  %alloca_102 = alloca i64, align 8
  %alloca_count_102 = alloca i64, align 8
  %alloca_96 = alloca i1, align 1
  %alloca_count_96 = alloca i1, align 1
  %alloca_94 = alloca { i64, i64 }, align 8
  %alloca_count_94 = alloca { i64, i64 }, align 8
  %alloca_93 = alloca i64, align 8
  %alloca_count_93 = alloca i64, align 8
  store { i64, i64 } %0, ptr %alloca_count_94, align 8
  %load_98 = load i64, ptr %alloca_count_94, align 8
  %icmp_99 = icmp eq i64 %load_98, 0
  store i1 %icmp_99, ptr %alloca_count_96, align 1
  %load_101 = load i1, ptr %alloca_count_96, align 1
  br i1 %load_101, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  %gep_104 = getelementptr inbounds i8, ptr %alloca_count_94, i64 8
  %load_106 = load i64, ptr %gep_104, align 8
  store i64 %load_106, ptr %alloca_count_102, align 8
  %load_108 = load i64, ptr %alloca_count_102, align 8
  store i64 %load_108, ptr %alloca_count_93, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  %load_112 = load i64, ptr %alloca_count_94, align 8
  %icmp_113 = icmp eq i64 %load_112, 1
  store i1 %icmp_113, ptr %alloca_count_110, align 1
  %load_115 = load i1, ptr %alloca_count_110, align 1
  br i1 %load_115, label %bb4, label %bb5

bb1:                                              ; preds = %bb5, %bb4, %bb2
  %load_116 = load i64, ptr %alloca_count_93, align 8
  ret i64 %load_116

bb4:                                              ; preds = %bb3
  store i64 %1, ptr %alloca_count_93, align 8
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
  %alloca_129 = alloca ptr, align 8
  %alloca_count_129 = alloca ptr, align 8
  %alloca_125 = alloca ptr, align 8
  %alloca_count_125 = alloca ptr, align 8
  %alloca_123 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_count_123 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_120 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_count_120 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_118 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_count_118 = alloca { i64, i8, i8, i8 }, align 8
  store { i64, i8, i8, i8 } zeroinitializer, ptr %alloca_count_118, align 8
  %load_121 = load { i64, i8, i8, i8 }, ptr %alloca_count_118, align 8
  store { i64, i8, i8, i8 } %load_121, ptr %alloca_count_120, align 8
  store { i64, i8, i8, i8 } { i64 2, i8 -128, i8 64, i8 32 }, ptr %alloca_count_123, align 8
  store ptr %alloca_count_120, ptr %alloca_count_125, align 8
  %call_127 = call ptr @describe(ptr %alloca_count_125)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_128 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_127)
  br label %bb2

bb2:                                              ; preds = %bb1
  store ptr %alloca_count_123, ptr %alloca_count_129, align 8
  %call_131 = call ptr @describe(ptr %alloca_count_129)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_132 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_131)
  br label %bb4

bb4:                                              ; preds = %bb3
  store i64 -5, ptr %alloca_count_133, align 8
  %load_136 = load i64, ptr %alloca_count_133, align 8
  %call_137 = call ptr @classify(i64 %load_136)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_138 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_137)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_139 = call ptr @classify(i64 0)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_140 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_139)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_141 = call ptr @classify(i64 4)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_142 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_141)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_143 = call ptr @classify(i64 7)
  br label %bb11

bb11:                                             ; preds = %bb10
  %call_144 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_143)
  br label %bb12

bb12:                                             ; preds = %bb11
  store { i64, i64 } { i64 0, i64 42 }, ptr %alloca_count_145, align 8
  %load_147 = load { i64, i64 }, ptr %alloca_count_145, align 8
  %call_148 = call i64 @unwrap_or({ i64, i64 } %load_147, i64 0)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_149 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.9, i64 %call_148)
  br label %bb14

bb14:                                             ; preds = %bb13
  store { i64, i64 } { i64 1, i64 0 }, ptr %alloca_count_150, align 8
  %load_152 = load { i64, i64 }, ptr %alloca_count_150, align 8
  %call_153 = call i64 @unwrap_or({ i64, i64 } %load_152, i64 99)
  br label %bb15

bb15:                                             ; preds = %bb14
  %call_154 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.9, i64 %call_153)
  br label %bb16

bb16:                                             ; preds = %bb15
  store i64 0, ptr %alloca_count_155, align 8
  %load_157 = load i64, ptr %alloca_count_155, align 8
  %call_158 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.10, i64 %load_157)
  br label %bb17

bb17:                                             ; preds = %bb16
  ret i32 0
}

declare i32 @printf(ptr, ...)
