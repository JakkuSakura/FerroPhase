; ModuleID = '10_print_showcase'
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.10_print_showcase.0 = constant [6 x i8] c"Hello\00", align 1
@.str.10_print_showcase.1 = constant [20 x i8] c"World with newlines\00", align 1
@.str.10_print_showcase.10 = constant [20 x i8] [i8 109, i8 97, i8 116, i8 104, i8 58, i8 32, i8 37, i8 100, i8 32, i8 43, i8 32, i8 37, i8 100, i8 32, i8 61, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.10_print_showcase.11 = constant [17 x i8] c"This %s %s %s %s\00", align 1
@.str.10_print_showcase.12 = constant [6 x i8] c"stays\00", align 1
@.str.10_print_showcase.13 = constant [3 x i8] c"on\00", align 1
@.str.10_print_showcase.14 = constant [4 x i8] c"one\00", align 1
@.str.10_print_showcase.15 = constant [5 x i8] c"line\00", align 1
@.str.10_print_showcase.16 = constant [27 x i8] c"Continuing without newline\00", align 1
@.str.10_print_showcase.17 = constant [20 x i8] c" - appended content\00", align 1
@.str.10_print_showcase.18 = constant [9 x i8] c"Unit: %s\00", align 1
@.str.10_print_showcase.19 = constant [3 x i8] c"()\00", align 1
@.str.10_print_showcase.2 = constant [2 x i8] [i8 10, i8 0], align 1
@.str.10_print_showcase.20 = constant [9 x i8] c"Null: %s\00", align 1
@.str.10_print_showcase.21 = constant [7 x i8] c"<none>\00", align 1
@.str.10_print_showcase.3 = constant [11 x i8] c"Number: %d\00", align 1
@.str.10_print_showcase.4 = constant [15 x i8] c"Boolean: %d %d\00", align 1
@.str.10_print_showcase.5 = constant [19 x i8] c"Mixed: %d %f %s %d\00", align 1
@.str.10_print_showcase.6 = constant [5 x i8] c"text\00", align 1
@.str.10_print_showcase.7 = constant [18 x i8] c"Namespace test %s\00", align 1
@.str.10_print_showcase.8 = constant [12 x i8] c"still works\00", align 1
@.str.10_print_showcase.9 = constant [12 x i8] [i8 118, i8 97, i8 108, i8 117, i8 101, i8 32, i8 61, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %call_0 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.0)
  br label %bb1
bb1:
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.1)
  br label %bb2
bb2:
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.2)
  br label %bb3
bb3:
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.3, i64 42)
  br label %bb4
bb4:
  %zext_4 = zext i1 1 to i32
  %zext_5 = zext i1 0 to i32
  %call_6 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.4, i32 %zext_4, i32 %zext_5)
  br label %bb5
bb5:
  %zext_7 = zext i1 1 to i32
  %call_8 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.5, i64 1, double 2.500000e0, ptr @.str.10_print_showcase.6, i32 %zext_7)
  br label %bb6
bb6:
  %call_9 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.2)
  br label %bb7
bb7:
  %call_10 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.7, ptr @.str.10_print_showcase.8)
  br label %bb8
bb8:
  %call_11 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.2)
  br label %bb9
bb9:
  %alloca_12 = alloca i64, align 8
  store i64 7, ptr %alloca_12
  %load_14 = load i64, ptr %alloca_12
  %call_15 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.9, i64 %load_14)
  br label %bb10
bb10:
  %call_16 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.10, i64 2, i64 3, i64 5)
  br label %bb11
bb11:
  %call_17 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.11, ptr @.str.10_print_showcase.12, ptr @.str.10_print_showcase.13, ptr @.str.10_print_showcase.14, ptr @.str.10_print_showcase.15)
  br label %bb12
bb12:
  %call_18 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.2)
  br label %bb13
bb13:
  %call_19 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.16)
  br label %bb14
bb14:
  %call_20 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.17)
  br label %bb15
bb15:
  %call_21 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.2)
  br label %bb16
bb16:
  %call_22 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.18, ptr @.str.10_print_showcase.19)
  br label %bb17
bb17:
  %call_23 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.20, ptr @.str.10_print_showcase.21)
  br label %bb18
bb18:
  %call_24 = call i32 (ptr, ...) @printf(ptr @.str.10_print_showcase.2)
  br label %bb19
bb19:
  ret i32 0
}

