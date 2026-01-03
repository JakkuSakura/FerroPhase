; ModuleID = '10_print_showcase'
source_filename = "10_print_showcase"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.10_print_showcase.0 = private unnamed_addr constant [6 x i8] c"Hello\00", align 1
@.str.10_print_showcase.1 = private unnamed_addr constant [20 x i8] c"World with newlines\00", align 1
@.str.10_print_showcase.2 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@.str.10_print_showcase.3 = private unnamed_addr constant [13 x i8] c"Number: %lld\00", align 1
@.str.10_print_showcase.4 = private unnamed_addr constant [15 x i8] c"Boolean: %d %d\00", align 1
@.str.10_print_showcase.5 = private unnamed_addr constant [21 x i8] c"Mixed: %lld %f %s %d\00", align 1
@.str.10_print_showcase.6 = private unnamed_addr constant [5 x i8] c"text\00", align 1
@.str.10_print_showcase.7 = private unnamed_addr constant [18 x i8] c"Namespace test %s\00", align 1
@.str.10_print_showcase.8 = private unnamed_addr constant [12 x i8] c"still works\00", align 1
@.str.10_print_showcase.9 = private unnamed_addr constant [14 x i8] c"value = %lld\0A\00", align 1
@.str.10_print_showcase.10 = private unnamed_addr constant [26 x i8] c"math: %lld + %lld = %lld\0A\00", align 1
@.str.10_print_showcase.11 = private unnamed_addr constant [11 x i8] c"float: %f\0A\00", align 1
@.str.10_print_showcase.12 = private unnamed_addr constant [14 x i8] c"chars: %s %s\0A\00", align 1
@.str.10_print_showcase.13 = private unnamed_addr constant [3 x i8] c"()\00", align 1
@.str.10_print_showcase.14 = private unnamed_addr constant [21 x i8] c"tuple: (%lld, %lld)\0A\00", align 1
@.str.10_print_showcase.15 = private unnamed_addr constant [14 x i8] c"bools: %d %d\0A\00", align 1
@.str.10_print_showcase.16 = private unnamed_addr constant [17 x i8] c"This %s %s %s %s\00", align 1
@.str.10_print_showcase.17 = private unnamed_addr constant [6 x i8] c"stays\00", align 1
@.str.10_print_showcase.18 = private unnamed_addr constant [3 x i8] c"on\00", align 1
@.str.10_print_showcase.19 = private unnamed_addr constant [4 x i8] c"one\00", align 1
@.str.10_print_showcase.20 = private unnamed_addr constant [5 x i8] c"line\00", align 1
@.str.10_print_showcase.21 = private unnamed_addr constant [27 x i8] c"Continuing without newline\00", align 1
@.str.10_print_showcase.22 = private unnamed_addr constant [20 x i8] c" - appended content\00", align 1
@.str.10_print_showcase.23 = private unnamed_addr constant [9 x i8] c"Unit: %s\00", align 1
@.str.10_print_showcase.24 = private unnamed_addr constant [9 x i8] c"Null: %s\00", align 1
@.str.10_print_showcase.25 = private unnamed_addr constant [5 x i8] c"null\00", align 1
@.str.10_print_showcase.26 = private unnamed_addr constant [16 x i8] c"escaped: %s %s\0A\00", align 1
@.str.10_print_showcase.27 = private unnamed_addr constant [12 x i8] c"line1\0Aline2\00", align 1
@.str.10_print_showcase.28 = private unnamed_addr constant [8 x i8] c"tab\09end\00", align 1

define i32 @main() {
bb0:
  %alloca_9 = alloca i64, align 8
  %alloca_count_9 = alloca i64, align 8
  %call_0 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.0)
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.1)
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.2)
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.3, i64 42)
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.4, i1 true, i1 false)
  %call_5 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.5, i64 1, double 2.500000e+00, ptr @.str.10_print_showcase.6, i1 true)
  %call_6 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.2)
  %call_7 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.7, ptr @.str.10_print_showcase.8)
  %call_8 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.2)
  store i64 7, ptr %alloca_count_9, align 8
  %load_11 = load i64, ptr %alloca_count_9, align 8
  %call_12 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.9, i64 %load_11)
  %call_13 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.10, i64 2, i64 3, i64 5)
  %call_14 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.11, double 3.141590e+00)
  %call_15 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.12, ptr @.str.10_print_showcase.13, ptr @.str.10_print_showcase.13)
  %call_16 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.14, i64 1, i64 2)
  %call_17 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.15, i1 true, i1 false)
  %call_18 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.16, ptr @.str.10_print_showcase.17, ptr @.str.10_print_showcase.18, ptr @.str.10_print_showcase.19, ptr @.str.10_print_showcase.20)
  %call_19 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.2)
  %call_20 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.21)
  %call_21 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.22)
  %call_22 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.2)
  %call_23 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.23, ptr @.str.10_print_showcase.13)
  %call_24 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.24, ptr @.str.10_print_showcase.25)
  %call_25 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.2)
  %call_26 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.26, ptr @.str.10_print_showcase.27, ptr @.str.10_print_showcase.28)
  ret i32 0
}

declare i32 @printf(ptr, ...)
