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
  %alloca_69 = alloca ptr, align 8
  %alloca_count_69 = alloca ptr, align 8
  %alloca_66 = alloca ptr, align 8
  %alloca_count_66 = alloca ptr, align 8
  %alloca_63 = alloca ptr, align 8
  %alloca_count_63 = alloca ptr, align 8
  %alloca_58 = alloca i1, align 1
  %alloca_count_58 = alloca i1, align 1
  %alloca_53 = alloca ptr, align 8
  %alloca_count_53 = alloca ptr, align 8
  %alloca_50 = alloca ptr, align 8
  %alloca_count_50 = alloca ptr, align 8
  %alloca_47 = alloca ptr, align 8
  %alloca_count_47 = alloca ptr, align 8
  %alloca_42 = alloca i1, align 1
  %alloca_count_42 = alloca i1, align 1
  %alloca_35 = alloca i1, align 1
  %alloca_count_35 = alloca i1, align 1
  %alloca_29 = alloca i1, align 1
  %alloca_count_29 = alloca i1, align 1
  %alloca_23 = alloca i1, align 1
  %alloca_count_23 = alloca i1, align 1
  %alloca_17 = alloca i1, align 1
  %alloca_count_17 = alloca i1, align 1
  %alloca_15 = alloca ptr, align 8
  %alloca_count_15 = alloca ptr, align 8
  %alloca_10 = alloca i1, align 1
  %alloca_count_10 = alloca i1, align 1
  %alloca_4 = alloca i1, align 1
  %alloca_count_4 = alloca i1, align 1
  %alloca_2 = alloca ptr, align 8
  %alloca_count_2 = alloca ptr, align 8
  %alloca_1 = alloca ptr, align 8
  %alloca_count_1 = alloca ptr, align 8
  %alloca_0 = alloca ptr, align 8
  %alloca_count_0 = alloca ptr, align 8
  store ptr %0, ptr %alloca_count_2, align 8
  %load_5 = load ptr, ptr %alloca_count_2, align 8
  %ptrtoint = ptrtoint ptr %load_5 to i64
  %icmp_6 = icmp eq i64 %ptrtoint, 0
  store i1 %icmp_6, ptr %alloca_count_4, align 1
  %load_8 = load i1, ptr %alloca_count_4, align 1
  br i1 %load_8, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  store ptr @.str.12_pattern_matching.0, ptr %alloca_count_1, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  %load_11 = load ptr, ptr %alloca_count_2, align 8
  %ptrtoint1 = ptrtoint ptr %load_11 to i64
  %icmp_12 = icmp eq i64 %ptrtoint1, 1
  store i1 %icmp_12, ptr %alloca_count_10, align 1
  %load_14 = load i1, ptr %alloca_count_10, align 1
  br i1 %load_14, label %bb4, label %bb5

bb1:                                              ; preds = %bb9, %bb8, %bb6, %bb4, %bb2
  store ptr %0, ptr %alloca_count_15, align 8
  %load_18 = load ptr, ptr %alloca_count_15, align 8
  %ptrtoint2 = ptrtoint ptr %load_18 to i64
  %icmp_19 = icmp eq i64 %ptrtoint2, 0
  store i1 %icmp_19, ptr %alloca_count_17, align 1
  %load_21 = load i1, ptr %alloca_count_17, align 1
  br i1 %load_21, label %bb11, label %bb12

bb4:                                              ; preds = %bb3
  store ptr @.str.12_pattern_matching.1, ptr %alloca_count_1, align 8
  br label %bb1

bb5:                                              ; preds = %bb3
  %load_24 = load ptr, ptr %alloca_count_2, align 8
  %ptrtoint3 = ptrtoint ptr %load_24 to i64
  %icmp_25 = icmp eq i64 %ptrtoint3, 2
  store i1 %icmp_25, ptr %alloca_count_23, align 1
  %load_27 = load i1, ptr %alloca_count_23, align 1
  br i1 %load_27, label %bb6, label %bb7

bb11:                                             ; preds = %bb1
  store ptr @.str.12_pattern_matching.0, ptr %alloca_count_0, align 8
  br label %bb10

bb12:                                             ; preds = %bb1
  %load_30 = load ptr, ptr %alloca_count_15, align 8
  %ptrtoint4 = ptrtoint ptr %load_30 to i64
  %icmp_31 = icmp eq i64 %ptrtoint4, 1
  store i1 %icmp_31, ptr %alloca_count_29, align 1
  %load_33 = load i1, ptr %alloca_count_29, align 1
  br i1 %load_33, label %bb13, label %bb14

bb6:                                              ; preds = %bb5
  store ptr @.str.12_pattern_matching.2, ptr %alloca_count_1, align 8
  br label %bb1

bb7:                                              ; preds = %bb5
  %load_36 = load ptr, ptr %alloca_count_2, align 8
  %ptrtoint5 = ptrtoint ptr %load_36 to i64
  %icmp_37 = icmp eq i64 %ptrtoint5, 2
  store i1 %icmp_37, ptr %alloca_count_35, align 1
  %load_39 = load i1, ptr %alloca_count_35, align 1
  br i1 %load_39, label %bb8, label %bb9

bb10:                                             ; preds = %bb18, %bb17, %bb15, %bb13, %bb11
  %load_40 = load ptr, ptr %alloca_count_0, align 8
  ret ptr %load_40

bb13:                                             ; preds = %bb12
  store ptr @.str.12_pattern_matching.1, ptr %alloca_count_0, align 8
  br label %bb10

bb14:                                             ; preds = %bb12
  %load_43 = load ptr, ptr %alloca_count_15, align 8
  %ptrtoint6 = ptrtoint ptr %load_43 to i64
  %icmp_44 = icmp eq i64 %ptrtoint6, 2
  store i1 %icmp_44, ptr %alloca_count_42, align 1
  %load_46 = load i1, ptr %alloca_count_42, align 1
  br i1 %load_46, label %bb15, label %bb16

bb8:                                              ; preds = %bb7
  %load_48 = load ptr, ptr %alloca_count_2, align 8
  store ptr %load_48, ptr %alloca_count_47, align 8
  %load_51 = load ptr, ptr %alloca_count_2, align 8
  store ptr %load_51, ptr %alloca_count_50, align 8
  %load_54 = load ptr, ptr %alloca_count_2, align 8
  store ptr %load_54, ptr %alloca_count_53, align 8
  store ptr @.str.12_pattern_matching.3, ptr %alloca_count_1, align 8
  br label %bb1

bb9:                                              ; preds = %bb7
  br label %bb1

bb15:                                             ; preds = %bb14
  store ptr @.str.12_pattern_matching.2, ptr %alloca_count_0, align 8
  br label %bb10

bb16:                                             ; preds = %bb14
  %load_59 = load ptr, ptr %alloca_count_15, align 8
  %ptrtoint7 = ptrtoint ptr %load_59 to i64
  %icmp_60 = icmp eq i64 %ptrtoint7, 2
  store i1 %icmp_60, ptr %alloca_count_58, align 1
  %load_62 = load i1, ptr %alloca_count_58, align 1
  br i1 %load_62, label %bb17, label %bb18

bb17:                                             ; preds = %bb16
  %load_64 = load ptr, ptr %alloca_count_15, align 8
  store ptr %load_64, ptr %alloca_count_63, align 8
  %load_67 = load ptr, ptr %alloca_count_15, align 8
  store ptr %load_67, ptr %alloca_count_66, align 8
  %load_70 = load ptr, ptr %alloca_count_15, align 8
  store ptr %load_70, ptr %alloca_count_69, align 8
  store ptr @.str.12_pattern_matching.3, ptr %alloca_count_0, align 8
  br label %bb10

bb18:                                             ; preds = %bb16
  store i1 false, ptr %alloca_count_0, align 8
  br label %bb10
}

define internal ptr @classify(i64 %0) {
bb0:
  %alloca_131 = alloca i1, align 1
  %alloca_count_131 = alloca i1, align 1
  %alloca_127 = alloca i64, align 8
  %alloca_count_127 = alloca i64, align 8
  %alloca_124 = alloca i64, align 8
  %alloca_count_124 = alloca i64, align 8
  %alloca_117 = alloca i1, align 1
  %alloca_count_117 = alloca i1, align 1
  %alloca_113 = alloca i64, align 8
  %alloca_count_113 = alloca i64, align 8
  %alloca_110 = alloca i64, align 8
  %alloca_count_110 = alloca i64, align 8
  %alloca_105 = alloca i1, align 1
  %alloca_count_105 = alloca i1, align 1
  %alloca_102 = alloca i64, align 8
  %alloca_count_102 = alloca i64, align 8
  %alloca_94 = alloca i1, align 1
  %alloca_count_94 = alloca i1, align 1
  %alloca_91 = alloca i64, align 8
  %alloca_count_91 = alloca i64, align 8
  %alloca_86 = alloca i1, align 1
  %alloca_count_86 = alloca i1, align 1
  %alloca_84 = alloca i64, align 8
  %alloca_count_84 = alloca i64, align 8
  %alloca_78 = alloca i1, align 1
  %alloca_count_78 = alloca i1, align 1
  %alloca_76 = alloca i64, align 8
  %alloca_count_76 = alloca i64, align 8
  %alloca_75 = alloca ptr, align 8
  %alloca_count_75 = alloca ptr, align 8
  %alloca_74 = alloca ptr, align 8
  %alloca_count_74 = alloca ptr, align 8
  store i64 %0, ptr %alloca_count_76, align 8
  %load_79 = load i64, ptr %alloca_count_76, align 8
  %icmp_80 = icmp eq i64 %load_79, 0
  store i1 %icmp_80, ptr %alloca_count_78, align 1
  %load_82 = load i1, ptr %alloca_count_78, align 1
  br i1 %load_82, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  store ptr @.str.12_pattern_matching.4, ptr %alloca_count_74, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  br label %bb4

bb1:                                              ; preds = %bb10, %bb9, %bb6, %bb2
  store i64 %0, ptr %alloca_count_84, align 8
  %load_87 = load i64, ptr %alloca_count_84, align 8
  %icmp_88 = icmp eq i64 %load_87, 0
  store i1 %icmp_88, ptr %alloca_count_86, align 1
  %load_90 = load i1, ptr %alloca_count_86, align 1
  br i1 %load_90, label %bb13, label %bb14

bb4:                                              ; preds = %bb3
  %load_92 = load i64, ptr %alloca_count_76, align 8
  store i64 %load_92, ptr %alloca_count_91, align 8
  %load_95 = load i64, ptr %alloca_count_91, align 8
  %icmp_96 = icmp slt i64 %load_95, 0
  store i1 %icmp_96, ptr %alloca_count_94, align 1
  %load_98 = load i1, ptr %alloca_count_94, align 1
  br i1 %load_98, label %bb6, label %bb5

bb13:                                             ; preds = %bb1
  store ptr @.str.12_pattern_matching.4, ptr %alloca_count_75, align 8
  br label %bb12

bb14:                                             ; preds = %bb1
  br label %bb15

bb5:                                              ; preds = %bb4
  br label %bb7

bb6:                                              ; preds = %bb4
  store ptr @.str.12_pattern_matching.5, ptr %alloca_count_74, align 8
  br label %bb1

bb12:                                             ; preds = %bb21, %bb20, %bb17, %bb13
  %load_101 = load ptr, ptr %alloca_count_75, align 8
  ret ptr %load_101

bb15:                                             ; preds = %bb14
  %load_103 = load i64, ptr %alloca_count_84, align 8
  store i64 %load_103, ptr %alloca_count_102, align 8
  %load_106 = load i64, ptr %alloca_count_102, align 8
  %icmp_107 = icmp slt i64 %load_106, 0
  store i1 %icmp_107, ptr %alloca_count_105, align 1
  %load_109 = load i1, ptr %alloca_count_105, align 1
  br i1 %load_109, label %bb17, label %bb16

bb7:                                              ; preds = %bb5
  %load_111 = load i64, ptr %alloca_count_76, align 8
  store i64 %load_111, ptr %alloca_count_110, align 8
  %load_114 = load i64, ptr %alloca_count_110, align 8
  %iop_115 = srem i64 %load_114, 2
  store i64 %iop_115, ptr %alloca_count_113, align 8
  %load_118 = load i64, ptr %alloca_count_113, align 8
  %icmp_119 = icmp eq i64 %load_118, 0
  store i1 %icmp_119, ptr %alloca_count_117, align 1
  %load_121 = load i1, ptr %alloca_count_117, align 1
  br i1 %load_121, label %bb9, label %bb8

bb16:                                             ; preds = %bb15
  br label %bb18

bb17:                                             ; preds = %bb15
  store ptr @.str.12_pattern_matching.5, ptr %alloca_count_75, align 8
  br label %bb12

bb8:                                              ; preds = %bb7
  br label %bb10

bb9:                                              ; preds = %bb7
  store ptr @.str.12_pattern_matching.6, ptr %alloca_count_74, align 8
  br label %bb1

bb18:                                             ; preds = %bb16
  %load_125 = load i64, ptr %alloca_count_84, align 8
  store i64 %load_125, ptr %alloca_count_124, align 8
  %load_128 = load i64, ptr %alloca_count_124, align 8
  %iop_129 = srem i64 %load_128, 2
  store i64 %iop_129, ptr %alloca_count_127, align 8
  %load_132 = load i64, ptr %alloca_count_127, align 8
  %icmp_133 = icmp eq i64 %load_132, 0
  store i1 %icmp_133, ptr %alloca_count_131, align 1
  %load_135 = load i1, ptr %alloca_count_131, align 1
  br i1 %load_135, label %bb20, label %bb19

bb10:                                             ; preds = %bb8
  store ptr @.str.12_pattern_matching.7, ptr %alloca_count_74, align 8
  br label %bb1

bb19:                                             ; preds = %bb18
  br label %bb21

bb20:                                             ; preds = %bb18
  store ptr @.str.12_pattern_matching.6, ptr %alloca_count_75, align 8
  br label %bb12

bb21:                                             ; preds = %bb19
  store ptr @.str.12_pattern_matching.7, ptr %alloca_count_75, align 8
  br label %bb12

bb11:                                             ; No predecessors!
  %load_139 = load ptr, ptr %alloca_count_75, align 8
  ret ptr %load_139

bb22:                                             ; No predecessors!
  store i1 false, ptr %alloca_count_75, align 8
  %load_141 = load ptr, ptr %alloca_count_75, align 8
  ret ptr %load_141
}

define internal i64 @unwrap_or({ i64, i64 } %0, i64 %1) {
bb0:
  %alloca_183 = alloca i1, align 1
  %alloca_count_183 = alloca i1, align 1
  %alloca_175 = alloca i64, align 8
  %alloca_count_175 = alloca i64, align 8
  %alloca_168 = alloca i1, align 1
  %alloca_count_168 = alloca i1, align 1
  %alloca_166 = alloca { i64, i64 }, align 8
  %alloca_count_166 = alloca { i64, i64 }, align 8
  %alloca_160 = alloca i1, align 1
  %alloca_count_160 = alloca i1, align 1
  %alloca_152 = alloca i64, align 8
  %alloca_count_152 = alloca i64, align 8
  %alloca_146 = alloca i1, align 1
  %alloca_count_146 = alloca i1, align 1
  %alloca_144 = alloca { i64, i64 }, align 8
  %alloca_count_144 = alloca { i64, i64 }, align 8
  %alloca_143 = alloca i64, align 8
  %alloca_count_143 = alloca i64, align 8
  %alloca_142 = alloca i64, align 8
  %alloca_count_142 = alloca i64, align 8
  store { i64, i64 } %0, ptr %alloca_count_144, align 8
  %load_148 = load i64, ptr %alloca_count_144, align 8
  %icmp_149 = icmp eq i64 %load_148, 0
  store i1 %icmp_149, ptr %alloca_count_146, align 1
  %load_151 = load i1, ptr %alloca_count_146, align 1
  br i1 %load_151, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  %gep_154 = getelementptr inbounds i8, ptr %alloca_count_144, i64 8
  %load_156 = load i64, ptr %gep_154, align 8
  store i64 %load_156, ptr %alloca_count_152, align 8
  %load_158 = load i64, ptr %alloca_count_152, align 8
  store i64 %load_158, ptr %alloca_count_143, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  %load_162 = load i64, ptr %alloca_count_144, align 8
  %icmp_163 = icmp eq i64 %load_162, 1
  store i1 %icmp_163, ptr %alloca_count_160, align 1
  %load_165 = load i1, ptr %alloca_count_160, align 1
  br i1 %load_165, label %bb4, label %bb5

bb1:                                              ; preds = %bb5, %bb4, %bb2
  store { i64, i64 } %0, ptr %alloca_count_166, align 8
  %load_170 = load i64, ptr %alloca_count_166, align 8
  %icmp_171 = icmp eq i64 %load_170, 0
  store i1 %icmp_171, ptr %alloca_count_168, align 1
  %load_173 = load i1, ptr %alloca_count_168, align 1
  br i1 %load_173, label %bb7, label %bb8

bb4:                                              ; preds = %bb3
  store i64 %1, ptr %alloca_count_143, align 8
  br label %bb1

bb5:                                              ; preds = %bb3
  br label %bb1

bb7:                                              ; preds = %bb1
  %gep_177 = getelementptr inbounds i8, ptr %alloca_count_166, i64 8
  %load_179 = load i64, ptr %gep_177, align 8
  store i64 %load_179, ptr %alloca_count_175, align 8
  %load_181 = load i64, ptr %alloca_count_175, align 8
  store i64 %load_181, ptr %alloca_count_142, align 8
  br label %bb6

bb8:                                              ; preds = %bb1
  %load_185 = load i64, ptr %alloca_count_166, align 8
  %icmp_186 = icmp eq i64 %load_185, 1
  store i1 %icmp_186, ptr %alloca_count_183, align 1
  %load_188 = load i1, ptr %alloca_count_183, align 1
  br i1 %load_188, label %bb9, label %bb10

bb6:                                              ; preds = %bb10, %bb9, %bb7
  %load_189 = load i64, ptr %alloca_count_142, align 8
  ret i64 %load_189

bb9:                                              ; preds = %bb8
  store i64 %1, ptr %alloca_count_142, align 8
  br label %bb6

bb10:                                             ; preds = %bb8
  br label %bb6
}

define i32 @main() {
bb0:
  %alloca_228 = alloca i64, align 8
  %alloca_count_228 = alloca i64, align 8
  %alloca_223 = alloca { i64, i64 }, align 8
  %alloca_count_223 = alloca { i64, i64 }, align 8
  %alloca_218 = alloca { i64, i64 }, align 8
  %alloca_count_218 = alloca { i64, i64 }, align 8
  %alloca_206 = alloca i64, align 8
  %alloca_count_206 = alloca i64, align 8
  %alloca_202 = alloca ptr, align 8
  %alloca_count_202 = alloca ptr, align 8
  %alloca_198 = alloca ptr, align 8
  %alloca_count_198 = alloca ptr, align 8
  %alloca_196 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_count_196 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_193 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_count_193 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_191 = alloca { i64, i8, i8, i8 }, align 8
  %alloca_count_191 = alloca { i64, i8, i8, i8 }, align 8
  store { i64, i8, i8, i8 } zeroinitializer, ptr %alloca_count_191, align 8
  %load_194 = load { i64, i8, i8, i8 }, ptr %alloca_count_191, align 8
  store { i64, i8, i8, i8 } %load_194, ptr %alloca_count_193, align 8
  store { i64, i8, i8, i8 } { i64 2, i8 -128, i8 64, i8 32 }, ptr %alloca_count_196, align 8
  store ptr %alloca_count_193, ptr %alloca_count_198, align 8
  %call_200 = call ptr @describe(ptr %alloca_count_198)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_201 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_200)
  br label %bb2

bb2:                                              ; preds = %bb1
  store ptr %alloca_count_196, ptr %alloca_count_202, align 8
  %call_204 = call ptr @describe(ptr %alloca_count_202)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_205 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_204)
  br label %bb4

bb4:                                              ; preds = %bb3
  store i64 -5, ptr %alloca_count_206, align 8
  %load_209 = load i64, ptr %alloca_count_206, align 8
  %call_210 = call ptr @classify(i64 %load_209)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_211 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_210)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_212 = call ptr @classify(i64 0)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_213 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_212)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_214 = call ptr @classify(i64 4)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_215 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_214)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_216 = call ptr @classify(i64 7)
  br label %bb11

bb11:                                             ; preds = %bb10
  %call_217 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.8, ptr %call_216)
  br label %bb12

bb12:                                             ; preds = %bb11
  store { i64, i64 } { i64 0, i64 42 }, ptr %alloca_count_218, align 8
  %load_220 = load { i64, i64 }, ptr %alloca_count_218, align 8
  %call_221 = call i64 @unwrap_or({ i64, i64 } %load_220, i64 0)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_222 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.9, i64 %call_221)
  br label %bb14

bb14:                                             ; preds = %bb13
  store { i64, i64 } { i64 1, i64 0 }, ptr %alloca_count_223, align 8
  %load_225 = load { i64, i64 }, ptr %alloca_count_223, align 8
  %call_226 = call i64 @unwrap_or({ i64, i64 } %load_225, i64 99)
  br label %bb15

bb15:                                             ; preds = %bb14
  %call_227 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.9, i64 %call_226)
  br label %bb16

bb16:                                             ; preds = %bb15
  store i64 0, ptr %alloca_count_228, align 8
  %load_230 = load i64, ptr %alloca_count_228, align 8
  %call_231 = call i32 (ptr, ...) @printf(ptr @.str.12_pattern_matching.10, i64 %load_230)
  br label %bb17

bb17:                                             ; preds = %bb16
  ret i32 0
}

declare i32 @printf(ptr, ...)
