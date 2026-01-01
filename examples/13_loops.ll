; ModuleID = '13_loops'
source_filename = "13_loops"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.13_loops.0 = private unnamed_addr constant [26 x i8] c"=== Loop Constructs ===\0A\0A\00", align 1
@.str.13_loops.1 = private unnamed_addr constant [28 x i8] c"1. While loop - factorial:\0A\00", align 1
@.str.13_loops.2 = private unnamed_addr constant [13 x i8] c"  5! = %lld\0A\00", align 1
@.str.13_loops.3 = private unnamed_addr constant [13 x i8] c"  7! = %lld\0A\00", align 1
@.str.13_loops.4 = private unnamed_addr constant [27 x i8] c"\0A2. For loop - sum range:\0A\00", align 1
@.str.13_loops.5 = private unnamed_addr constant [21 x i8] c"  sum(1..10) = %lld\0A\00", align 1
@.str.13_loops.6 = private unnamed_addr constant [21 x i8] c"  sum(5..15) = %lld\0A\00", align 1
@.str.13_loops.7 = private unnamed_addr constant [33 x i8] c"\0A3. Loop with break expression:\0A\00", align 1
@.str.13_loops.8 = private unnamed_addr constant [29 x i8] c"  First divisor of 24: %lld\0A\00", align 1
@.str.13_loops.9 = private unnamed_addr constant [29 x i8] c"  First divisor of 17: %lld\0A\00", align 1
@.str.13_loops.10 = private unnamed_addr constant [25 x i8] c"\0A4. Loop with continue:\0A\00", align 1
@.str.13_loops.11 = private unnamed_addr constant [34 x i8] c"  Sum of even numbers < 10: %lld\0A\00", align 1
@.str.13_loops.12 = private unnamed_addr constant [19 x i8] c"\0A5. Nested loops:\0A\00", align 1
@.str.13_loops.13 = private unnamed_addr constant [21 x i8] c"\0A  Iterations: %lld\0A\00", align 1
@.str.13_loops.14 = private unnamed_addr constant [41 x i8] c"\0A6. Compile-time iteration (simulated):\0A\00", align 1
@.str.13_loops.15 = private unnamed_addr constant [19 x i8] c"  const 5! = %lld\0A\00", align 1
@.str.13_loops.16 = private unnamed_addr constant [36 x i8] c"\0A\E2\9C\93 Loop constructs demonstrated!\0A\00", align 1

define internal i64 @factorial(i64 %0) {
bb0:
  %alloca_0 = alloca i64, align 8
  %alloca_count_0 = alloca i64, align 8
  %alloca_1 = alloca i64, align 8
  %alloca_count_1 = alloca i64, align 8
  %alloca_2 = alloca i64, align 8
  %alloca_count_2 = alloca i64, align 8
  store i64 1, ptr %alloca_count_2, align 8
  store i64 1, ptr %alloca_count_0, align 8
  br label %bb1

bb1:                                              ; preds = %bb2, %bb0
  %alloca_5 = alloca i1, align 1
  %alloca_count_5 = alloca i1, align 1
  %load_6 = load i64, ptr %alloca_count_0, align 8
  %icmp_7 = icmp sle i64 %load_6, %0
  store i1 %icmp_7, ptr %alloca_count_5, align 1
  %load_9 = load i1, ptr %alloca_count_5, align 1
  br i1 %load_9, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_10 = load i64, ptr %alloca_count_2, align 8
  %load_11 = load i64, ptr %alloca_count_0, align 8
  %iop_12 = mul i64 %load_10, %load_11
  store i64 %iop_12, ptr %alloca_count_2, align 8
  %load_14 = load i64, ptr %alloca_count_0, align 8
  %iop_15 = add i64 %load_14, 1
  store i64 %iop_15, ptr %alloca_count_0, align 8
  br label %bb1

bb3:                                              ; preds = %bb1
  %load_17 = load i64, ptr %alloca_count_2, align 8
  store i64 %load_17, ptr %alloca_count_1, align 8
  %load_19 = load i64, ptr %alloca_count_1, align 8
  ret i64 %load_19
}

define internal i64 @sum_range(i64 %0, i64 %1) {
bb0:
  %alloca_20 = alloca i64, align 8
  %alloca_count_20 = alloca i64, align 8
  %alloca_21 = alloca i64, align 8
  %alloca_count_21 = alloca i64, align 8
  %alloca_22 = alloca i64, align 8
  %alloca_count_22 = alloca i64, align 8
  store i64 0, ptr %alloca_count_20, align 8
  store i64 %0, ptr %alloca_count_21, align 8
  br label %bb1

bb1:                                              ; preds = %bb2, %bb0
  %alloca_25 = alloca i1, align 1
  %alloca_count_25 = alloca i1, align 1
  %load_26 = load i64, ptr %alloca_count_21, align 8
  %icmp_27 = icmp slt i64 %load_26, %1
  store i1 %icmp_27, ptr %alloca_count_25, align 1
  %load_29 = load i1, ptr %alloca_count_25, align 1
  br i1 %load_29, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_30 = load i64, ptr %alloca_count_20, align 8
  %load_31 = load i64, ptr %alloca_count_21, align 8
  %iop_32 = add i64 %load_30, %load_31
  store i64 %iop_32, ptr %alloca_count_20, align 8
  %load_34 = load i64, ptr %alloca_count_21, align 8
  %iop_35 = add i64 %load_34, 1
  store i64 %iop_35, ptr %alloca_count_21, align 8
  br label %bb1

bb3:                                              ; preds = %bb1
  %load_37 = load i64, ptr %alloca_count_20, align 8
  store i64 %load_37, ptr %alloca_count_22, align 8
  %load_39 = load i64, ptr %alloca_count_22, align 8
  ret i64 %load_39
}

define internal i64 @find_first_divisor(i64 %0) {
bb0:
  %alloca_40 = alloca i64, align 8
  %alloca_count_40 = alloca i64, align 8
  %alloca_41 = alloca i64, align 8
  %alloca_count_41 = alloca i64, align 8
  %alloca_42 = alloca i64, align 8
  %alloca_count_42 = alloca i64, align 8
  store i64 2, ptr %alloca_count_40, align 8
  br label %bb1

bb1:                                              ; preds = %bb10, %bb0
  br label %bb2

bb2:                                              ; preds = %bb1
  %alloca_44 = alloca i64, align 8
  %alloca_count_44 = alloca i64, align 8
  %load_45 = load i64, ptr %alloca_count_40, align 8
  %load_46 = load i64, ptr %alloca_count_40, align 8
  %iop_47 = mul i64 %load_45, %load_46
  store i64 %iop_47, ptr %alloca_count_44, align 8
  %alloca_49 = alloca i1, align 1
  %alloca_count_49 = alloca i1, align 1
  %load_50 = load i64, ptr %alloca_count_44, align 8
  %icmp_51 = icmp sgt i64 %load_50, %0
  store i1 %icmp_51, ptr %alloca_count_49, align 1
  %load_53 = load i1, ptr %alloca_count_49, align 1
  br i1 %load_53, label %bb4, label %bb5

bb4:                                              ; preds = %bb2
  store i64 %0, ptr %alloca_count_42, align 8
  br label %bb3

bb5:                                              ; preds = %bb2
  br label %bb6

bb3:                                              ; preds = %bb8, %bb4
  br label %bb12

bb6:                                              ; preds = %bb7, %bb5
  %alloca_55 = alloca i64, align 8
  %alloca_count_55 = alloca i64, align 8
  %load_56 = load i64, ptr %alloca_count_40, align 8
  %iop_57 = srem i64 %0, %load_56
  store i64 %iop_57, ptr %alloca_count_55, align 8
  %alloca_59 = alloca i1, align 1
  %alloca_count_59 = alloca i1, align 1
  %load_60 = load i64, ptr %alloca_count_55, align 8
  %icmp_61 = icmp eq i64 %load_60, 0
  store i1 %icmp_61, ptr %alloca_count_59, align 1
  %load_63 = load i1, ptr %alloca_count_59, align 1
  br i1 %load_63, label %bb8, label %bb9

bb12:                                             ; preds = %bb21, %bb3
  br label %bb13

bb8:                                              ; preds = %bb6
  %load_64 = load i64, ptr %alloca_count_40, align 8
  store i64 %load_64, ptr %alloca_count_42, align 8
  br label %bb3

bb9:                                              ; preds = %bb6
  br label %bb10

bb13:                                             ; preds = %bb12
  %alloca_66 = alloca i64, align 8
  %alloca_count_66 = alloca i64, align 8
  %load_67 = load i64, ptr %alloca_count_40, align 8
  %load_68 = load i64, ptr %alloca_count_40, align 8
  %iop_69 = mul i64 %load_67, %load_68
  store i64 %iop_69, ptr %alloca_count_66, align 8
  %alloca_71 = alloca i1, align 1
  %alloca_count_71 = alloca i1, align 1
  %load_72 = load i64, ptr %alloca_count_66, align 8
  %icmp_73 = icmp sgt i64 %load_72, %0
  store i1 %icmp_73, ptr %alloca_count_71, align 1
  %load_75 = load i1, ptr %alloca_count_71, align 1
  br i1 %load_75, label %bb15, label %bb16

bb10:                                             ; preds = %bb11, %bb9
  %load_76 = load i64, ptr %alloca_count_40, align 8
  %iop_77 = add i64 %load_76, 1
  store i64 %iop_77, ptr %alloca_count_40, align 8
  br label %bb1

bb15:                                             ; preds = %bb13
  store i64 %0, ptr %alloca_count_41, align 8
  br label %bb14

bb16:                                             ; preds = %bb13
  br label %bb17

bb14:                                             ; preds = %bb19, %bb15
  %load_80 = load i64, ptr %alloca_count_41, align 8
  ret i64 %load_80

bb17:                                             ; preds = %bb18, %bb16
  %alloca_81 = alloca i64, align 8
  %alloca_count_81 = alloca i64, align 8
  %load_82 = load i64, ptr %alloca_count_40, align 8
  %iop_83 = srem i64 %0, %load_82
  store i64 %iop_83, ptr %alloca_count_81, align 8
  %alloca_85 = alloca i1, align 1
  %alloca_count_85 = alloca i1, align 1
  %load_86 = load i64, ptr %alloca_count_81, align 8
  %icmp_87 = icmp eq i64 %load_86, 0
  store i1 %icmp_87, ptr %alloca_count_85, align 1
  %load_89 = load i1, ptr %alloca_count_85, align 1
  br i1 %load_89, label %bb19, label %bb20

bb19:                                             ; preds = %bb17
  %load_90 = load i64, ptr %alloca_count_40, align 8
  store i64 %load_90, ptr %alloca_count_41, align 8
  br label %bb14

bb20:                                             ; preds = %bb17
  br label %bb21

bb21:                                             ; preds = %bb22, %bb20
  %load_92 = load i64, ptr %alloca_count_40, align 8
  %iop_93 = add i64 %load_92, 1
  store i64 %iop_93, ptr %alloca_count_40, align 8
  br label %bb12

bb7:                                              ; No predecessors!
  br label %bb6

bb11:                                             ; No predecessors!
  br label %bb10

bb18:                                             ; No predecessors!
  br label %bb17

bb22:                                             ; No predecessors!
  br label %bb21
}

define internal i64 @sum_even_numbers(i64 %0) {
bb0:
  %alloca_95 = alloca i64, align 8
  %alloca_count_95 = alloca i64, align 8
  %alloca_96 = alloca i64, align 8
  %alloca_count_96 = alloca i64, align 8
  %alloca_97 = alloca i64, align 8
  %alloca_count_97 = alloca i64, align 8
  store i64 0, ptr %alloca_count_97, align 8
  store i64 0, ptr %alloca_count_95, align 8
  br label %bb1

bb1:                                              ; preds = %bb6, %bb4, %bb0
  %alloca_100 = alloca i1, align 1
  %alloca_count_100 = alloca i1, align 1
  %load_101 = load i64, ptr %alloca_count_95, align 8
  %icmp_102 = icmp slt i64 %load_101, %0
  store i1 %icmp_102, ptr %alloca_count_100, align 1
  %load_104 = load i1, ptr %alloca_count_100, align 1
  br i1 %load_104, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_105 = load i64, ptr %alloca_count_95, align 8
  %iop_106 = add i64 %load_105, 1
  store i64 %iop_106, ptr %alloca_count_95, align 8
  %alloca_108 = alloca i64, align 8
  %alloca_count_108 = alloca i64, align 8
  %load_109 = load i64, ptr %alloca_count_95, align 8
  %iop_110 = srem i64 %load_109, 2
  store i64 %iop_110, ptr %alloca_count_108, align 8
  %alloca_112 = alloca i1, align 1
  %alloca_count_112 = alloca i1, align 1
  %load_113 = load i64, ptr %alloca_count_108, align 8
  %icmp_114 = icmp ne i64 %load_113, 0
  store i1 %icmp_114, ptr %alloca_count_112, align 1
  %load_116 = load i1, ptr %alloca_count_112, align 1
  br i1 %load_116, label %bb4, label %bb5

bb3:                                              ; preds = %bb1
  %load_117 = load i64, ptr %alloca_count_97, align 8
  store i64 %load_117, ptr %alloca_count_96, align 8
  %load_119 = load i64, ptr %alloca_count_96, align 8
  ret i64 %load_119

bb4:                                              ; preds = %bb2
  br label %bb1

bb5:                                              ; preds = %bb2
  br label %bb6

bb6:                                              ; preds = %bb7, %bb5
  %load_120 = load i64, ptr %alloca_count_97, align 8
  %load_121 = load i64, ptr %alloca_count_95, align 8
  %iop_122 = add i64 %load_120, %load_121
  store i64 %iop_122, ptr %alloca_count_97, align 8
  br label %bb1

bb7:                                              ; No predecessors!
  br label %bb6
}

define internal void @main() {
bb0:
  %alloca_124 = alloca i64, align 8
  %alloca_count_124 = alloca i64, align 8
  %alloca_125 = alloca i64, align 8
  %alloca_count_125 = alloca i64, align 8
  %alloca_126 = alloca i64, align 8
  %alloca_count_126 = alloca i64, align 8
  %alloca_127 = alloca i64, align 8
  %alloca_count_127 = alloca i64, align 8
  %call_128 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_129 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_130 = call i64 @factorial(i64 5)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_131 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.2, i64 %call_130)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_132 = call i64 @factorial(i64 7)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_133 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.3, i64 %call_132)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_134 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.4)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_135 = call i64 @sum_range(i64 1, i64 10)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_136 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.5, i64 %call_135)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_137 = call i64 @sum_range(i64 5, i64 15)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_138 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.6, i64 %call_137)
  br label %bb11

bb11:                                             ; preds = %bb10
  %call_139 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.7)
  br label %bb12

bb12:                                             ; preds = %bb11
  %call_140 = call i64 @find_first_divisor(i64 24)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_141 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.8, i64 %call_140)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_142 = call i64 @find_first_divisor(i64 17)
  br label %bb15

bb15:                                             ; preds = %bb14
  %call_143 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.9, i64 %call_142)
  br label %bb16

bb16:                                             ; preds = %bb15
  %call_144 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.10)
  br label %bb17

bb17:                                             ; preds = %bb16
  %call_145 = call i64 @sum_even_numbers(i64 10)
  br label %bb18

bb18:                                             ; preds = %bb17
  %call_146 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.11, i64 %call_145)
  br label %bb19

bb19:                                             ; preds = %bb18
  %call_147 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.12)
  br label %bb20

bb20:                                             ; preds = %bb19
  store i64 0, ptr %alloca_count_125, align 8
  store i64 1, ptr %alloca_count_127, align 8
  br label %bb21

bb21:                                             ; preds = %bb35, %bb20
  %alloca_150 = alloca i1, align 1
  %alloca_count_150 = alloca i1, align 1
  %load_151 = load i64, ptr %alloca_count_127, align 8
  %icmp_152 = icmp slt i64 %load_151, 4
  store i1 %icmp_152, ptr %alloca_count_150, align 1
  %load_154 = load i1, ptr %alloca_count_150, align 1
  br i1 %load_154, label %bb22, label %bb23

bb22:                                             ; preds = %bb21
  store i64 1, ptr %alloca_count_124, align 8
  br label %bb24

bb23:                                             ; preds = %bb21
  %load_156 = load i64, ptr %alloca_count_125, align 8
  %call_157 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.13, i64 %load_156)
  br label %bb42

bb24:                                             ; preds = %bb32, %bb22
  %alloca_158 = alloca i1, align 1
  %alloca_count_158 = alloca i1, align 1
  %load_159 = load i64, ptr %alloca_count_124, align 8
  %icmp_160 = icmp slt i64 %load_159, 4
  store i1 %icmp_160, ptr %alloca_count_158, align 1
  %load_162 = load i1, ptr %alloca_count_158, align 1
  br i1 %load_162, label %bb25, label %bb26

bb42:                                             ; preds = %bb23
  %call_163 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.14)
  br label %bb43

bb25:                                             ; preds = %bb24
  %load_164 = load i64, ptr %alloca_count_125, align 8
  %iop_165 = add i64 %load_164, 1
  store i64 %iop_165, ptr %alloca_count_125, align 8
  %alloca_167 = alloca i1, align 1
  %alloca_count_167 = alloca i1, align 1
  %load_168 = load i64, ptr %alloca_count_127, align 8
  %load_169 = load i64, ptr %alloca_count_124, align 8
  %icmp_170 = icmp eq i64 %load_168, %load_169
  store i1 %icmp_170, ptr %alloca_count_167, align 1
  %load_172 = load i1, ptr %alloca_count_167, align 1
  br i1 %load_172, label %bb27, label %bb28

bb26:                                             ; preds = %bb24
  store i64 1, ptr %alloca_count_126, align 8
  br label %bb33

bb43:                                             ; preds = %bb42
  %alloca_174 = alloca i64, align 8
  %alloca_count_174 = alloca i64, align 8
  store i64 120, ptr %alloca_count_174, align 8
  %load_176 = load i64, ptr %alloca_count_174, align 8
  %call_177 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.15, i64 %load_176)
  br label %bb44

bb27:                                             ; preds = %bb25
  br label %bb29

bb28:                                             ; preds = %bb25
  br label %bb29

bb33:                                             ; preds = %bb41, %bb26
  %alloca_178 = alloca i1, align 1
  %alloca_count_178 = alloca i1, align 1
  %load_179 = load i64, ptr %alloca_count_126, align 8
  %icmp_180 = icmp slt i64 %load_179, 4
  store i1 %icmp_180, ptr %alloca_count_178, align 1
  %load_182 = load i1, ptr %alloca_count_178, align 1
  br i1 %load_182, label %bb34, label %bb35

bb44:                                             ; preds = %bb43
  %call_183 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.16)
  br label %bb45

bb29:                                             ; preds = %bb28, %bb27
  %alloca_184 = alloca i1, align 1
  %alloca_count_184 = alloca i1, align 1
  %load_185 = load i64, ptr %alloca_count_127, align 8
  %load_186 = load i64, ptr %alloca_count_124, align 8
  %icmp_187 = icmp eq i64 %load_185, %load_186
  store i1 %icmp_187, ptr %alloca_count_184, align 1
  %load_189 = load i1, ptr %alloca_count_184, align 1
  br i1 %load_189, label %bb30, label %bb31

bb34:                                             ; preds = %bb33
  %load_190 = load i64, ptr %alloca_count_125, align 8
  %iop_191 = add i64 %load_190, 1
  store i64 %iop_191, ptr %alloca_count_125, align 8
  %alloca_193 = alloca i1, align 1
  %alloca_count_193 = alloca i1, align 1
  %load_194 = load i64, ptr %alloca_count_127, align 8
  %load_195 = load i64, ptr %alloca_count_126, align 8
  %icmp_196 = icmp eq i64 %load_194, %load_195
  store i1 %icmp_196, ptr %alloca_count_193, align 1
  %load_198 = load i1, ptr %alloca_count_193, align 1
  br i1 %load_198, label %bb36, label %bb37

bb35:                                             ; preds = %bb33
  %load_199 = load i64, ptr %alloca_count_127, align 8
  %iop_200 = add i64 %load_199, 1
  store i64 %iop_200, ptr %alloca_count_127, align 8
  br label %bb21

bb45:                                             ; preds = %bb44
  ret void

bb30:                                             ; preds = %bb29
  br label %bb32

bb31:                                             ; preds = %bb29
  br label %bb32

bb36:                                             ; preds = %bb34
  br label %bb38

bb37:                                             ; preds = %bb34
  br label %bb38

bb32:                                             ; preds = %bb31, %bb30
  %load_202 = load i64, ptr %alloca_count_124, align 8
  %iop_203 = add i64 %load_202, 1
  store i64 %iop_203, ptr %alloca_count_124, align 8
  br label %bb24

bb38:                                             ; preds = %bb37, %bb36
  %alloca_205 = alloca i1, align 1
  %alloca_count_205 = alloca i1, align 1
  %load_206 = load i64, ptr %alloca_count_127, align 8
  %load_207 = load i64, ptr %alloca_count_126, align 8
  %icmp_208 = icmp eq i64 %load_206, %load_207
  store i1 %icmp_208, ptr %alloca_count_205, align 1
  %load_210 = load i1, ptr %alloca_count_205, align 1
  br i1 %load_210, label %bb39, label %bb40

bb39:                                             ; preds = %bb38
  br label %bb41

bb40:                                             ; preds = %bb38
  br label %bb41

bb41:                                             ; preds = %bb40, %bb39
  %load_211 = load i64, ptr %alloca_count_126, align 8
  %iop_212 = add i64 %load_211, 1
  store i64 %iop_212, ptr %alloca_count_126, align 8
  br label %bb33
}

declare i32 @printf(ptr, ...)
