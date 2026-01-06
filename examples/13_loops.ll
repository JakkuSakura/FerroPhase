; ModuleID = '13_loops'
source_filename = "13_loops"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.13_loops.0 = constant [28 x i8] c"\F0\9F\93\98 Tutorial: 13_loops.fp\0A\00"
@.str.13_loops.1 = constant [52 x i8] c"\F0\9F\A7\AD Focus: Loop constructs: while, for, and loop.\0A\00"
@.str.13_loops.2 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.13_loops.3 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.13_loops.4 = constant [2 x i8] c"\0A\00"
@.str.13_loops.5 = constant [26 x i8] c"=== Loop Constructs ===\0A\0A\00"
@.str.13_loops.6 = constant [28 x i8] c"1. While loop - factorial:\0A\00"
@.str.13_loops.7 = constant [13 x i8] c"  5! = %lld\0A\00"
@.str.13_loops.8 = constant [13 x i8] c"  7! = %lld\0A\00"
@.str.13_loops.9 = constant [27 x i8] c"\0A2. For loop - sum range:\0A\00"
@.str.13_loops.10 = constant [21 x i8] c"  sum(1..10) = %lld\0A\00"
@.str.13_loops.11 = constant [21 x i8] c"  sum(5..15) = %lld\0A\00"
@.str.13_loops.12 = constant [33 x i8] c"\0A3. Loop with break expression:\0A\00"
@.str.13_loops.13 = constant [29 x i8] c"  First divisor of 24: %lld\0A\00"
@.str.13_loops.14 = constant [29 x i8] c"  First divisor of 17: %lld\0A\00"
@.str.13_loops.15 = constant [25 x i8] c"\0A4. Loop with continue:\0A\00"
@.str.13_loops.16 = constant [34 x i8] c"  Sum of even numbers < 10: %lld\0A\00"
@.str.13_loops.17 = constant [19 x i8] c"\0A5. Nested loops:\0A\00"
@.str.13_loops.18 = constant [21 x i8] c"\0A  Iterations: %lld\0A\00"
@.str.13_loops.19 = constant [29 x i8] c"\0A6. Compile-time recursion:\0A\00"
@.str.13_loops.20 = constant [29 x i8] c"  const_factorial(5) = %lld\0A\00"
@.str.13_loops.21 = constant [8 x i8] c"[%lld] \00"
@.str.13_loops.22 = constant [36 x i8] c"\0A\E2\9C\93 Loop constructs demonstrated!\0A\00"

define internal i64 @factorial(i64 %0) {
bb0:
  %alloca_5 = alloca i1, align 1
  %alloca_count_5 = alloca i1, align 1
  %alloca_2 = alloca i64, align 8
  %alloca_count_2 = alloca i64, align 8
  %alloca_1 = alloca i64, align 8
  %alloca_count_1 = alloca i64, align 8
  %alloca_0 = alloca i64, align 8
  %alloca_count_0 = alloca i64, align 8
  store i64 1, ptr %alloca_count_0, align 8
  store i64 1, ptr %alloca_count_2, align 8
  br label %bb1

bb1:                                              ; preds = %bb2, %bb0
  %load_6 = load i64, ptr %alloca_count_2, align 8
  %icmp_7 = icmp sle i64 %load_6, %0
  store i1 %icmp_7, ptr %alloca_count_5, align 1
  %load_9 = load i1, ptr %alloca_count_5, align 1
  br i1 %load_9, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_10 = load i64, ptr %alloca_count_0, align 8
  %load_11 = load i64, ptr %alloca_count_2, align 8
  %iop_12 = mul i64 %load_10, %load_11
  store i64 %iop_12, ptr %alloca_count_0, align 8
  %load_14 = load i64, ptr %alloca_count_2, align 8
  %iop_15 = add i64 %load_14, 1
  store i64 %iop_15, ptr %alloca_count_2, align 8
  br label %bb1

bb3:                                              ; preds = %bb1
  %load_17 = load i64, ptr %alloca_count_0, align 8
  store i64 %load_17, ptr %alloca_count_1, align 8
  %load_19 = load i64, ptr %alloca_count_1, align 8
  ret i64 %load_19
}

define internal i64 @const_factorial(i64 %0) {
bb0:
  %alloca_26 = alloca i64, align 8
  %alloca_count_26 = alloca i64, align 8
  %alloca_21 = alloca i1, align 1
  %alloca_count_21 = alloca i1, align 1
  %alloca_20 = alloca i64, align 8
  %alloca_count_20 = alloca i64, align 8
  %icmp_22 = icmp sle i64 %0, 1
  store i1 %icmp_22, ptr %alloca_count_21, align 1
  %load_24 = load i1, ptr %alloca_count_21, align 1
  br i1 %load_24, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  store i64 1, ptr %alloca_count_20, align 8
  br label %bb3

bb2:                                              ; preds = %bb0
  %iop_27 = sub i64 %0, 1
  store i64 %iop_27, ptr %alloca_count_26, align 8
  %load_29 = load i64, ptr %alloca_count_26, align 8
  %call_30 = call i64 @const_factorial(i64 %load_29)
  br label %bb4

bb3:                                              ; preds = %bb4, %bb1
  %load_31 = load i64, ptr %alloca_count_20, align 8
  ret i64 %load_31

bb4:                                              ; preds = %bb2
  %iop_32 = mul i64 %0, %call_30
  store i64 %iop_32, ptr %alloca_count_20, align 8
  br label %bb3
}

define internal i64 @sum_range(i64 %0, i64 %1) {
bb0:
  %alloca_39 = alloca i1, align 1
  %alloca_count_39 = alloca i1, align 1
  %alloca_36 = alloca i64, align 8
  %alloca_count_36 = alloca i64, align 8
  %alloca_35 = alloca i64, align 8
  %alloca_count_35 = alloca i64, align 8
  %alloca_34 = alloca i64, align 8
  %alloca_count_34 = alloca i64, align 8
  store i64 0, ptr %alloca_count_36, align 8
  store i64 %0, ptr %alloca_count_35, align 8
  br label %bb1

bb1:                                              ; preds = %bb2, %bb0
  %load_40 = load i64, ptr %alloca_count_35, align 8
  %icmp_41 = icmp slt i64 %load_40, %1
  store i1 %icmp_41, ptr %alloca_count_39, align 1
  %load_43 = load i1, ptr %alloca_count_39, align 1
  br i1 %load_43, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_44 = load i64, ptr %alloca_count_36, align 8
  %load_45 = load i64, ptr %alloca_count_35, align 8
  %iop_46 = add i64 %load_44, %load_45
  store i64 %iop_46, ptr %alloca_count_36, align 8
  %load_48 = load i64, ptr %alloca_count_35, align 8
  %iop_49 = add i64 %load_48, 1
  store i64 %iop_49, ptr %alloca_count_35, align 8
  br label %bb1

bb3:                                              ; preds = %bb1
  %load_51 = load i64, ptr %alloca_count_36, align 8
  store i64 %load_51, ptr %alloca_count_34, align 8
  %load_53 = load i64, ptr %alloca_count_34, align 8
  ret i64 %load_53
}

define internal i64 @find_first_divisor(i64 %0) {
bb0:
  %alloca_73 = alloca i1, align 1
  %alloca_count_73 = alloca i1, align 1
  %alloca_69 = alloca i64, align 8
  %alloca_count_69 = alloca i64, align 8
  %alloca_62 = alloca i1, align 1
  %alloca_count_62 = alloca i1, align 1
  %alloca_57 = alloca i64, align 8
  %alloca_count_57 = alloca i64, align 8
  %alloca_55 = alloca i64, align 8
  %alloca_count_55 = alloca i64, align 8
  %alloca_54 = alloca i64, align 8
  %alloca_count_54 = alloca i64, align 8
  store i64 2, ptr %alloca_count_54, align 8
  br label %bb1

bb1:                                              ; preds = %bb10, %bb0
  br label %bb2

bb2:                                              ; preds = %bb1
  %load_58 = load i64, ptr %alloca_count_54, align 8
  %load_59 = load i64, ptr %alloca_count_54, align 8
  %iop_60 = mul i64 %load_58, %load_59
  store i64 %iop_60, ptr %alloca_count_57, align 8
  %load_63 = load i64, ptr %alloca_count_57, align 8
  %icmp_64 = icmp sgt i64 %load_63, %0
  store i1 %icmp_64, ptr %alloca_count_62, align 1
  %load_66 = load i1, ptr %alloca_count_62, align 1
  br i1 %load_66, label %bb4, label %bb5

bb4:                                              ; preds = %bb2
  store i64 %0, ptr %alloca_count_55, align 8
  br label %bb3

bb5:                                              ; preds = %bb2
  br label %bb6

bb3:                                              ; preds = %bb8, %bb4
  %load_68 = load i64, ptr %alloca_count_55, align 8
  ret i64 %load_68

bb6:                                              ; preds = %bb7, %bb5
  %load_70 = load i64, ptr %alloca_count_54, align 8
  %iop_71 = srem i64 %0, %load_70
  store i64 %iop_71, ptr %alloca_count_69, align 8
  %load_74 = load i64, ptr %alloca_count_69, align 8
  %icmp_75 = icmp eq i64 %load_74, 0
  store i1 %icmp_75, ptr %alloca_count_73, align 1
  %load_77 = load i1, ptr %alloca_count_73, align 1
  br i1 %load_77, label %bb8, label %bb9

bb8:                                              ; preds = %bb6
  %load_78 = load i64, ptr %alloca_count_54, align 8
  store i64 %load_78, ptr %alloca_count_55, align 8
  br label %bb3

bb9:                                              ; preds = %bb6
  br label %bb10

bb10:                                             ; preds = %bb11, %bb9
  %load_80 = load i64, ptr %alloca_count_54, align 8
  %iop_81 = add i64 %load_80, 1
  store i64 %iop_81, ptr %alloca_count_54, align 8
  br label %bb1

bb7:                                              ; No predecessors!
  br label %bb6

bb11:                                             ; No predecessors!
  br label %bb10
}

define internal i64 @sum_even_numbers(i64 %0) {
bb0:
  %alloca_100 = alloca i1, align 1
  %alloca_count_100 = alloca i1, align 1
  %alloca_96 = alloca i64, align 8
  %alloca_count_96 = alloca i64, align 8
  %alloca_88 = alloca i1, align 1
  %alloca_count_88 = alloca i1, align 1
  %alloca_85 = alloca i64, align 8
  %alloca_count_85 = alloca i64, align 8
  %alloca_84 = alloca i64, align 8
  %alloca_count_84 = alloca i64, align 8
  %alloca_83 = alloca i64, align 8
  %alloca_count_83 = alloca i64, align 8
  store i64 0, ptr %alloca_count_84, align 8
  store i64 0, ptr %alloca_count_85, align 8
  br label %bb1

bb1:                                              ; preds = %bb6, %bb4, %bb0
  %load_89 = load i64, ptr %alloca_count_85, align 8
  %icmp_90 = icmp slt i64 %load_89, %0
  store i1 %icmp_90, ptr %alloca_count_88, align 1
  %load_92 = load i1, ptr %alloca_count_88, align 1
  br i1 %load_92, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_93 = load i64, ptr %alloca_count_85, align 8
  %iop_94 = add i64 %load_93, 1
  store i64 %iop_94, ptr %alloca_count_85, align 8
  %load_97 = load i64, ptr %alloca_count_85, align 8
  %iop_98 = srem i64 %load_97, 2
  store i64 %iop_98, ptr %alloca_count_96, align 8
  %load_101 = load i64, ptr %alloca_count_96, align 8
  %icmp_102 = icmp ne i64 %load_101, 0
  store i1 %icmp_102, ptr %alloca_count_100, align 1
  %load_104 = load i1, ptr %alloca_count_100, align 1
  br i1 %load_104, label %bb4, label %bb5

bb3:                                              ; preds = %bb1
  %load_105 = load i64, ptr %alloca_count_84, align 8
  store i64 %load_105, ptr %alloca_count_83, align 8
  %load_107 = load i64, ptr %alloca_count_83, align 8
  ret i64 %load_107

bb4:                                              ; preds = %bb2
  br label %bb1

bb5:                                              ; preds = %bb2
  br label %bb6

bb6:                                              ; preds = %bb7, %bb5
  %load_108 = load i64, ptr %alloca_count_84, align 8
  %load_109 = load i64, ptr %alloca_count_85, align 8
  %iop_110 = add i64 %load_108, %load_109
  store i64 %iop_110, ptr %alloca_count_84, align 8
  br label %bb1

bb7:                                              ; No predecessors!
  br label %bb6
}

define i32 @main() {
bb0:
  %alloca_168 = alloca i64, align 8
  %alloca_count_168 = alloca i64, align 8
  %alloca_159 = alloca i1, align 1
  %alloca_count_159 = alloca i1, align 1
  %alloca_150 = alloca i1, align 1
  %alloca_count_150 = alloca i1, align 1
  %alloca_142 = alloca i1, align 1
  %alloca_count_142 = alloca i1, align 1
  %alloca_114 = alloca i64, align 8
  %alloca_count_114 = alloca i64, align 8
  %alloca_113 = alloca i64, align 8
  %alloca_count_113 = alloca i64, align 8
  %alloca_112 = alloca i64, align 8
  %alloca_count_112 = alloca i64, align 8
  %call_115 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_116 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_117 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.2)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_118 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.3)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_119 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_120 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.5)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_121 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.6)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_122 = call i64 @factorial(i64 5)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_123 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.7, i64 %call_122)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_124 = call i64 @factorial(i64 7)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_125 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.8, i64 %call_124)
  br label %bb11

bb11:                                             ; preds = %bb10
  %call_126 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.9)
  br label %bb12

bb12:                                             ; preds = %bb11
  %call_127 = call i64 @sum_range(i64 1, i64 10)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_128 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.10, i64 %call_127)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_129 = call i64 @sum_range(i64 5, i64 15)
  br label %bb15

bb15:                                             ; preds = %bb14
  %call_130 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.11, i64 %call_129)
  br label %bb16

bb16:                                             ; preds = %bb15
  %call_131 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.12)
  br label %bb17

bb17:                                             ; preds = %bb16
  %call_132 = call i64 @find_first_divisor(i64 24)
  br label %bb18

bb18:                                             ; preds = %bb17
  %call_133 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.13, i64 %call_132)
  br label %bb19

bb19:                                             ; preds = %bb18
  %call_134 = call i64 @find_first_divisor(i64 17)
  br label %bb20

bb20:                                             ; preds = %bb19
  %call_135 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.14, i64 %call_134)
  br label %bb21

bb21:                                             ; preds = %bb20
  %call_136 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.15)
  br label %bb22

bb22:                                             ; preds = %bb21
  %call_137 = call i64 @sum_even_numbers(i64 10)
  br label %bb23

bb23:                                             ; preds = %bb22
  %call_138 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.16, i64 %call_137)
  br label %bb24

bb24:                                             ; preds = %bb23
  %call_139 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.17)
  br label %bb25

bb25:                                             ; preds = %bb24
  store i64 0, ptr %alloca_count_112, align 8
  store i64 1, ptr %alloca_count_113, align 8
  br label %bb26

bb26:                                             ; preds = %bb31, %bb25
  %load_143 = load i64, ptr %alloca_count_113, align 8
  %icmp_144 = icmp slt i64 %load_143, 4
  store i1 %icmp_144, ptr %alloca_count_142, align 1
  %load_146 = load i1, ptr %alloca_count_142, align 1
  br i1 %load_146, label %bb27, label %bb28

bb27:                                             ; preds = %bb26
  store i64 1, ptr %alloca_count_114, align 8
  br label %bb29

bb28:                                             ; preds = %bb26
  %load_148 = load i64, ptr %alloca_count_112, align 8
  %call_149 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.18, i64 %load_148)
  br label %bb35

bb29:                                             ; preds = %bb34, %bb27
  %load_151 = load i64, ptr %alloca_count_114, align 8
  %icmp_152 = icmp slt i64 %load_151, 4
  store i1 %icmp_152, ptr %alloca_count_150, align 1
  %load_154 = load i1, ptr %alloca_count_150, align 1
  br i1 %load_154, label %bb30, label %bb31

bb35:                                             ; preds = %bb28
  %call_155 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.19)
  br label %bb36

bb30:                                             ; preds = %bb29
  %load_156 = load i64, ptr %alloca_count_112, align 8
  %iop_157 = add i64 %load_156, 1
  store i64 %iop_157, ptr %alloca_count_112, align 8
  %load_160 = load i64, ptr %alloca_count_113, align 8
  %load_161 = load i64, ptr %alloca_count_114, align 8
  %icmp_162 = icmp eq i64 %load_160, %load_161
  store i1 %icmp_162, ptr %alloca_count_159, align 1
  %load_164 = load i1, ptr %alloca_count_159, align 1
  br i1 %load_164, label %bb32, label %bb33

bb31:                                             ; preds = %bb29
  %load_165 = load i64, ptr %alloca_count_113, align 8
  %iop_166 = add i64 %load_165, 1
  store i64 %iop_166, ptr %alloca_count_113, align 8
  br label %bb26

bb36:                                             ; preds = %bb35
  store i64 120, ptr %alloca_count_168, align 8
  %load_170 = load i64, ptr %alloca_count_168, align 8
  %call_171 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.20, i64 %load_170)
  br label %bb37

bb32:                                             ; preds = %bb30
  %load_172 = load i64, ptr %alloca_count_113, align 8
  %call_173 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.21, i64 %load_172)
  br label %bb34

bb33:                                             ; preds = %bb30
  br label %bb34

bb37:                                             ; preds = %bb36
  %call_174 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.22)
  br label %bb38

bb34:                                             ; preds = %bb33, %bb32
  %load_175 = load i64, ptr %alloca_count_114, align 8
  %iop_176 = add i64 %load_175, 1
  store i64 %iop_176, ptr %alloca_count_114, align 8
  br label %bb29

bb38:                                             ; preds = %bb37
  ret i32 0
}

declare i32 @printf(ptr, ...)
