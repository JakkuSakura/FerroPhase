; ModuleID = '10_print_showcase'
source_filename = "10_print_showcase"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.10_print_showcase.0 = private unnamed_addr constant [37 x i8] c"\F0\9F\93\98 Tutorial: 10_print_showcase.fp\0A\00", align 1
@.str.10_print_showcase.1 = private unnamed_addr constant [102 x i8] c"\F0\9F\A7\AD Focus: Comprehensive println!/print showcase covering variadic arguments and runtime formatting\0A\00", align 1
@.str.10_print_showcase.2 = private unnamed_addr constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00", align 1
@.str.10_print_showcase.3 = private unnamed_addr constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00", align 1
@.str.10_print_showcase.4 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@.str.10_print_showcase.5 = private unnamed_addr constant [6 x i8] c"Hello\00", align 1
@.str.10_print_showcase.6 = private unnamed_addr constant [20 x i8] c"World with newlines\00", align 1
@.str.10_print_showcase.7 = private unnamed_addr constant [13 x i8] c"Number: %lld\00", align 1
@.str.10_print_showcase.8 = private unnamed_addr constant [15 x i8] c"Boolean: %d %d\00", align 1
@.str.10_print_showcase.9 = private unnamed_addr constant [21 x i8] c"Mixed: %lld %f %s %d\00", align 1
@.str.10_print_showcase.10 = private unnamed_addr constant [5 x i8] c"text\00", align 1
@.str.10_print_showcase.11 = private unnamed_addr constant [18 x i8] c"Namespace test %s\00", align 1
@.str.10_print_showcase.12 = private unnamed_addr constant [12 x i8] c"still works\00", align 1
@.str.10_print_showcase.13 = private unnamed_addr constant [14 x i8] c"value = %lld\0A\00", align 1
@.str.10_print_showcase.14 = private unnamed_addr constant [26 x i8] c"math: %lld + %lld = %lld\0A\00", align 1
@.str.10_print_showcase.15 = private unnamed_addr constant [11 x i8] c"float: %f\0A\00", align 1
@.str.10_print_showcase.16 = private unnamed_addr constant [14 x i8] c"chars: %s %s\0A\00", align 1
@.str.10_print_showcase.17 = private unnamed_addr constant [3 x i8] c"()\00", align 1
@.str.10_print_showcase.18 = private unnamed_addr constant [21 x i8] c"tuple: (%lld, %lld)\0A\00", align 1
@.str.10_print_showcase.19 = private unnamed_addr constant [14 x i8] c"bools: %d %d\0A\00", align 1
@.str.10_print_showcase.20 = private unnamed_addr constant [17 x i8] c"This %s %s %s %s\00", align 1
@.str.10_print_showcase.21 = private unnamed_addr constant [6 x i8] c"stays\00", align 1
@.str.10_print_showcase.22 = private unnamed_addr constant [3 x i8] c"on\00", align 1
@.str.10_print_showcase.23 = private unnamed_addr constant [4 x i8] c"one\00", align 1
@.str.10_print_showcase.24 = private unnamed_addr constant [5 x i8] c"line\00", align 1
@.str.10_print_showcase.25 = private unnamed_addr constant [27 x i8] c"Continuing without newline\00", align 1
@.str.10_print_showcase.26 = private unnamed_addr constant [20 x i8] c" - appended content\00", align 1
@.str.10_print_showcase.27 = private unnamed_addr constant [9 x i8] c"Unit: %s\00", align 1
@.str.10_print_showcase.28 = private unnamed_addr constant [9 x i8] c"Null: %s\00", align 1
@.str.10_print_showcase.29 = private unnamed_addr constant [5 x i8] c"null\00", align 1
@.str.10_print_showcase.30 = private unnamed_addr constant [16 x i8] c"escaped: %s %s\0A\00", align 1
@.str.10_print_showcase.31 = private unnamed_addr constant [12 x i8] c"line1\0Aline2\00", align 1
@.str.10_print_showcase.32 = private unnamed_addr constant [8 x i8] c"tab\09end\00", align 1

define i32 @main() {
bb0:
  %alloca_17 = alloca i64, align 8
  %alloca_count_17 = alloca i64, align 8
  %call_0 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.2)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.3)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_5 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.5)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_6 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.6)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_7 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.4)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_8 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.7, i64 42)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_11 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.8, i32 1, i32 0)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_13 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.9, i64 1, double 2.500000e+00, ptr @.str.10_print_showcase.10, i32 1)
  br label %bb11

bb11:                                             ; preds = %bb10
  %call_14 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.4)
  br label %bb12

bb12:                                             ; preds = %bb11
  %call_15 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.11, ptr @.str.10_print_showcase.12)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_16 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.4)
  br label %bb14

bb14:                                             ; preds = %bb13
  store i64 7, ptr %alloca_count_17, align 8
  %load_19 = load i64, ptr %alloca_count_17, align 8
  %call_20 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.13, i64 %load_19)
  br label %bb15

bb15:                                             ; preds = %bb14
  %call_21 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.14, i64 2, i64 3, i64 5)
  br label %bb16

bb16:                                             ; preds = %bb15
  %call_22 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.15, double 3.141590e+00)
  br label %bb17

bb17:                                             ; preds = %bb16
  %call_23 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.16, ptr @.str.10_print_showcase.17, ptr @.str.10_print_showcase.17)
  %call_24 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.18, i64 1, i64 2)
  br label %bb18

bb18:                                             ; preds = %bb17
  %call_27 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.19, i32 1, i32 0)
  br label %bb19

bb19:                                             ; preds = %bb18
  %call_28 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.20, ptr @.str.10_print_showcase.21, ptr @.str.10_print_showcase.22, ptr @.str.10_print_showcase.23, ptr @.str.10_print_showcase.24)
  br label %bb20

bb20:                                             ; preds = %bb19
  %call_29 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.4)
  br label %bb21

bb21:                                             ; preds = %bb20
  %call_30 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.25)
  br label %bb22

bb22:                                             ; preds = %bb21
  %call_31 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.26)
  br label %bb23

bb23:                                             ; preds = %bb22
  %call_32 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.4)
  br label %bb24

bb24:                                             ; preds = %bb23
  %call_33 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.27, ptr @.str.10_print_showcase.17)
  %call_34 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.28, ptr @.str.10_print_showcase.29)
  %call_35 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.4)
  br label %bb25

bb25:                                             ; preds = %bb24
  %call_36 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.30, ptr @.str.10_print_showcase.31, ptr @.str.10_print_showcase.32)
  br label %bb26

bb26:                                             ; preds = %bb25
  ret i32 0
}

declare i32 @printf(ptr, ...)
