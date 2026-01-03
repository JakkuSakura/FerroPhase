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
@.str.13_loops.16 = private unnamed_addr constant [8 x i8] c"[%lld] \00", align 1
@.str.13_loops.17 = private unnamed_addr constant [36 x i8] c"\0A\E2\9C\93 Loop constructs demonstrated!\0A\00", align 1

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

define internal i64 @sum_range(i64 %0, i64 %1) {
bb0:
  %alloca_25 = alloca i1, align 1
  %alloca_count_25 = alloca i1, align 1
  %alloca_22 = alloca i64, align 8
  %alloca_count_22 = alloca i64, align 8
  %alloca_21 = alloca i64, align 8
  %alloca_count_21 = alloca i64, align 8
  %alloca_20 = alloca i64, align 8
  %alloca_count_20 = alloca i64, align 8
  store i64 0, ptr %alloca_count_20, align 8
  store i64 %0, ptr %alloca_count_21, align 8
  br label %bb1

bb1:                                              ; preds = %bb2, %bb0
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
  %alloca_59 = alloca i1, align 1
  %alloca_count_59 = alloca i1, align 1
  %alloca_55 = alloca i64, align 8
  %alloca_count_55 = alloca i64, align 8
  %alloca_48 = alloca i1, align 1
  %alloca_count_48 = alloca i1, align 1
  %alloca_43 = alloca i64, align 8
  %alloca_count_43 = alloca i64, align 8
  %alloca_41 = alloca i64, align 8
  %alloca_count_41 = alloca i64, align 8
  %alloca_40 = alloca i64, align 8
  %alloca_count_40 = alloca i64, align 8
  store i64 2, ptr %alloca_count_40, align 8
  br label %bb1

bb1:                                              ; preds = %bb10, %bb0
  br label %bb2

bb2:                                              ; preds = %bb1
  %load_44 = load i64, ptr %alloca_count_40, align 8
  %load_45 = load i64, ptr %alloca_count_40, align 8
  %iop_46 = mul i64 %load_44, %load_45
  store i64 %iop_46, ptr %alloca_count_43, align 8
  %load_49 = load i64, ptr %alloca_count_43, align 8
  %icmp_50 = icmp sgt i64 %load_49, %0
  store i1 %icmp_50, ptr %alloca_count_48, align 1
  %load_52 = load i1, ptr %alloca_count_48, align 1
  br i1 %load_52, label %bb4, label %bb5

bb4:                                              ; preds = %bb2
  store i64 %0, ptr %alloca_count_41, align 8
  br label %bb3

bb5:                                              ; preds = %bb2
  br label %bb6

bb3:                                              ; preds = %bb8, %bb4
  %load_54 = load i64, ptr %alloca_count_41, align 8
  ret i64 %load_54

bb6:                                              ; preds = %bb7, %bb5
  %load_56 = load i64, ptr %alloca_count_40, align 8
  %iop_57 = srem i64 %0, %load_56
  store i64 %iop_57, ptr %alloca_count_55, align 8
  %load_60 = load i64, ptr %alloca_count_55, align 8
  %icmp_61 = icmp eq i64 %load_60, 0
  store i1 %icmp_61, ptr %alloca_count_59, align 1
  %load_63 = load i1, ptr %alloca_count_59, align 1
  br i1 %load_63, label %bb8, label %bb9

bb8:                                              ; preds = %bb6
  %load_64 = load i64, ptr %alloca_count_40, align 8
  store i64 %load_64, ptr %alloca_count_41, align 8
  br label %bb3

bb9:                                              ; preds = %bb6
  br label %bb10

bb10:                                             ; preds = %bb11, %bb9
  %load_66 = load i64, ptr %alloca_count_40, align 8
  %iop_67 = add i64 %load_66, 1
  store i64 %iop_67, ptr %alloca_count_40, align 8
  br label %bb1

bb7:                                              ; No predecessors!
  br label %bb6

bb11:                                             ; No predecessors!
  br label %bb10
}

define internal i64 @sum_even_numbers(i64 %0) {
bb0:
  %alloca_86 = alloca i1, align 1
  %alloca_count_86 = alloca i1, align 1
  %alloca_82 = alloca i64, align 8
  %alloca_count_82 = alloca i64, align 8
  %alloca_74 = alloca i1, align 1
  %alloca_count_74 = alloca i1, align 1
  %alloca_71 = alloca i64, align 8
  %alloca_count_71 = alloca i64, align 8
  %alloca_70 = alloca i64, align 8
  %alloca_count_70 = alloca i64, align 8
  %alloca_69 = alloca i64, align 8
  %alloca_count_69 = alloca i64, align 8
  store i64 0, ptr %alloca_count_70, align 8
  store i64 0, ptr %alloca_count_71, align 8
  br label %bb1

bb1:                                              ; preds = %bb6, %bb4, %bb0
  %load_75 = load i64, ptr %alloca_count_71, align 8
  %icmp_76 = icmp slt i64 %load_75, %0
  store i1 %icmp_76, ptr %alloca_count_74, align 1
  %load_78 = load i1, ptr %alloca_count_74, align 1
  br i1 %load_78, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_79 = load i64, ptr %alloca_count_71, align 8
  %iop_80 = add i64 %load_79, 1
  store i64 %iop_80, ptr %alloca_count_71, align 8
  %load_83 = load i64, ptr %alloca_count_71, align 8
  %iop_84 = srem i64 %load_83, 2
  store i64 %iop_84, ptr %alloca_count_82, align 8
  %load_87 = load i64, ptr %alloca_count_82, align 8
  %icmp_88 = icmp ne i64 %load_87, 0
  store i1 %icmp_88, ptr %alloca_count_86, align 1
  %load_90 = load i1, ptr %alloca_count_86, align 1
  br i1 %load_90, label %bb4, label %bb5

bb3:                                              ; preds = %bb1
  %load_91 = load i64, ptr %alloca_count_70, align 8
  store i64 %load_91, ptr %alloca_count_69, align 8
  %load_93 = load i64, ptr %alloca_count_69, align 8
  ret i64 %load_93

bb4:                                              ; preds = %bb2
  br label %bb1

bb5:                                              ; preds = %bb2
  br label %bb6

bb6:                                              ; preds = %bb7, %bb5
  %load_94 = load i64, ptr %alloca_count_70, align 8
  %load_95 = load i64, ptr %alloca_count_71, align 8
  %iop_96 = add i64 %load_94, %load_95
  store i64 %iop_96, ptr %alloca_count_70, align 8
  br label %bb1

bb7:                                              ; No predecessors!
  br label %bb6
}

define i32 @main() {
bb0:
  %alloca_149 = alloca i64, align 8
  %alloca_count_149 = alloca i64, align 8
  %alloca_140 = alloca i1, align 1
  %alloca_count_140 = alloca i1, align 1
  %alloca_131 = alloca i1, align 1
  %alloca_count_131 = alloca i1, align 1
  %alloca_123 = alloca i1, align 1
  %alloca_count_123 = alloca i1, align 1
  %alloca_100 = alloca i64, align 8
  %alloca_count_100 = alloca i64, align 8
  %alloca_99 = alloca i64, align 8
  %alloca_count_99 = alloca i64, align 8
  %alloca_98 = alloca i64, align 8
  %alloca_count_98 = alloca i64, align 8
  %call_101 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_102 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_103 = call i64 @factorial(i64 5)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_104 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.2, i64 %call_103)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_105 = call i64 @factorial(i64 7)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_106 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.3, i64 %call_105)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_107 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.4)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_108 = call i64 @sum_range(i64 1, i64 10)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_109 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.5, i64 %call_108)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_110 = call i64 @sum_range(i64 5, i64 15)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_111 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.6, i64 %call_110)
  br label %bb11

bb11:                                             ; preds = %bb10
  %call_112 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.7)
  br label %bb12

bb12:                                             ; preds = %bb11
  %call_113 = call i64 @find_first_divisor(i64 24)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_114 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.8, i64 %call_113)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_115 = call i64 @find_first_divisor(i64 17)
  br label %bb15

bb15:                                             ; preds = %bb14
  %call_116 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.9, i64 %call_115)
  br label %bb16

bb16:                                             ; preds = %bb15
  %call_117 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.10)
  br label %bb17

bb17:                                             ; preds = %bb16
  %call_118 = call i64 @sum_even_numbers(i64 10)
  br label %bb18

bb18:                                             ; preds = %bb17
  %call_119 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.11, i64 %call_118)
  br label %bb19

bb19:                                             ; preds = %bb18
  %call_120 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.12)
  br label %bb20

bb20:                                             ; preds = %bb19
  store i64 0, ptr %alloca_count_100, align 8
  store i64 1, ptr %alloca_count_98, align 8
  br label %bb21

bb21:                                             ; preds = %bb26, %bb20
  %load_124 = load i64, ptr %alloca_count_98, align 8
  %icmp_125 = icmp slt i64 %load_124, 4
  store i1 %icmp_125, ptr %alloca_count_123, align 1
  %load_127 = load i1, ptr %alloca_count_123, align 1
  br i1 %load_127, label %bb22, label %bb23

bb22:                                             ; preds = %bb21
  store i64 1, ptr %alloca_count_99, align 8
  br label %bb24

bb23:                                             ; preds = %bb21
  %load_129 = load i64, ptr %alloca_count_100, align 8
  %call_130 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.13, i64 %load_129)
  br label %bb31

bb24:                                             ; preds = %bb29, %bb22
  %load_132 = load i64, ptr %alloca_count_99, align 8
  %icmp_133 = icmp slt i64 %load_132, 4
  store i1 %icmp_133, ptr %alloca_count_131, align 1
  %load_135 = load i1, ptr %alloca_count_131, align 1
  br i1 %load_135, label %bb25, label %bb26

bb31:                                             ; preds = %bb23
  %call_136 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.14)
  br label %bb32

bb25:                                             ; preds = %bb24
  %load_137 = load i64, ptr %alloca_count_100, align 8
  %iop_138 = add i64 %load_137, 1
  store i64 %iop_138, ptr %alloca_count_100, align 8
  %load_141 = load i64, ptr %alloca_count_98, align 8
  %load_142 = load i64, ptr %alloca_count_99, align 8
  %icmp_143 = icmp eq i64 %load_141, %load_142
  store i1 %icmp_143, ptr %alloca_count_140, align 1
  %load_145 = load i1, ptr %alloca_count_140, align 1
  br i1 %load_145, label %bb27, label %bb28

bb26:                                             ; preds = %bb24
  %load_146 = load i64, ptr %alloca_count_98, align 8
  %iop_147 = add i64 %load_146, 1
  store i64 %iop_147, ptr %alloca_count_98, align 8
  br label %bb21

bb32:                                             ; preds = %bb31
  store i64 120, ptr %alloca_count_149, align 8
  %load_151 = load i64, ptr %alloca_count_149, align 8
  %call_152 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.15, i64 %load_151)
  br label %bb33

bb27:                                             ; preds = %bb25
  %load_153 = load i64, ptr %alloca_count_98, align 8
  %call_154 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.16, i64 %load_153)
  br label %bb30

bb28:                                             ; preds = %bb25
  br label %bb29

bb33:                                             ; preds = %bb32
  %call_155 = call i32 (ptr, ...) @printf(ptr @.str.13_loops.17)
  br label %bb34

bb30:                                             ; preds = %bb27
  br label %bb29

bb29:                                             ; preds = %bb30, %bb28
  %load_156 = load i64, ptr %alloca_count_99, align 8
  %iop_157 = add i64 %load_156, 1
  store i64 %iop_157, ptr %alloca_count_99, align 8
  br label %bb24

bb34:                                             ; preds = %bb33
  ret i32 0
}

declare i32 @printf(ptr, ...)
