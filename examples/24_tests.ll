; ModuleID = '24_tests'
source_filename = "24_tests"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.24_tests.0 = constant [28 x i8] c"\F0\9F\93\98 Tutorial: 24_tests.fp\0A\00"
@.str.24_tests.1 = constant [69 x i8] c"\F0\9F\A7\AD Focus: minimal test registration example plus manual execution\0A\00"
@.str.24_tests.2 = constant [52 x i8] c"\F0\9F\A7\AA What to look for: assert() should not trigger\0A\00"
@.str.24_tests.3 = constant [68 x i8] c"\E2\9C\85 Expectation: both tests run without printing assertion failure\0A\00"
@.str.24_tests.4 = constant [2 x i8] c"\0A\00"
@.str.24_tests.5 = constant [25 x i8] c"Running tests manually:\0A\00"
@.str.24_tests.6 = constant [27 x i8] c"  adds_two_numbers ... ok\0A\00"
@.str.24_tests.7 = constant [24 x i8] c"  string_concat ... ok\0A\00"
@.str.24_tests.8 = constant [18 x i8] c"assertion failed\0A\00"
@.str.24_tests.9 = constant [8 x i8] c"hello, \00"
@.str.24_tests.10 = constant [6 x i8] c"world\00"
@.str.24_tests.11 = constant [13 x i8] c"hello, world\00"

define i32 @main() {
bb0:
  %call_0 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.2)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.3)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_5 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.5)
  br label %bb6

bb6:                                              ; preds = %bb5
  call void @adds_two_numbers()
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_7 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.6)
  br label %bb8

bb8:                                              ; preds = %bb7
  call void @string_concat()
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_9 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.7)
  br label %bb10

bb10:                                             ; preds = %bb9
  ret i32 0
}

declare i32 @printf(ptr, ...)

define internal void @adds_two_numbers() {
bb0:
  %alloca_23 = alloca i1, align 1
  %alloca_count_23 = alloca i1, align 1
  %alloca_18 = alloca i1, align 1
  %alloca_count_18 = alloca i1, align 1
  %alloca_16 = alloca i64, align 8
  %alloca_count_16 = alloca i64, align 8
  %alloca_13 = alloca i64, align 8
  %alloca_count_13 = alloca i64, align 8
  %alloca_10 = alloca i64, align 8
  %alloca_count_10 = alloca i64, align 8
  store i64 3, ptr %alloca_count_10, align 8
  %load_14 = load i64, ptr %alloca_count_10, align 8
  store i64 %load_14, ptr %alloca_count_13, align 8
  store i64 3, ptr %alloca_count_16, align 8
  %load_19 = load i64, ptr %alloca_count_13, align 8
  %load_20 = load i64, ptr %alloca_count_16, align 8
  %icmp_21 = icmp eq i64 %load_19, %load_20
  store i1 %icmp_21, ptr %alloca_count_18, align 1
  %load_24 = load i1, ptr %alloca_count_18, align 1
  %zext = zext i1 %load_24 to i64
  %not_25 = xor i64 %zext, -1
  store i64 %not_25, ptr %alloca_count_23, align 1
  %load_27 = load i1, ptr %alloca_count_23, align 1
  br i1 %load_27, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  %call_28 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.8)
  br label %bb4

bb2:                                              ; preds = %bb0
  br label %bb3

bb4:                                              ; preds = %bb1
  br label %bb3

bb3:                                              ; preds = %bb4, %bb2
  ret void
}

define internal void @string_concat() {
bb0:
  %alloca_39 = alloca i1, align 1
  %alloca_count_39 = alloca i1, align 1
  %alloca_35 = alloca i1, align 1
  %alloca_count_35 = alloca i1, align 1
  %alloca_32 = alloca ptr, align 8
  %alloca_count_32 = alloca ptr, align 8
  %alloca_29 = alloca ptr, align 8
  %alloca_count_29 = alloca ptr, align 8
  store i64 add (i64 ptrtoint (ptr @.str.24_tests.9 to i64), i64 ptrtoint (ptr @.str.24_tests.10 to i64)), ptr %alloca_count_29, align 8
  %load_33 = load ptr, ptr %alloca_count_29, align 8
  store ptr %load_33, ptr %alloca_count_32, align 8
  %load_36 = load ptr, ptr %alloca_count_32, align 8
  %ptrtoint = ptrtoint ptr %load_36 to i64
  %icmp_37 = icmp eq i64 %ptrtoint, ptrtoint (ptr @.str.24_tests.11 to i64)
  store i1 %icmp_37, ptr %alloca_count_35, align 1
  %load_40 = load i1, ptr %alloca_count_35, align 1
  %zext = zext i1 %load_40 to i64
  %not_41 = xor i64 %zext, -1
  store i64 %not_41, ptr %alloca_count_39, align 1
  %load_43 = load i1, ptr %alloca_count_39, align 1
  br i1 %load_43, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  %call_44 = call i32 (ptr, ...) @printf(ptr @.str.24_tests.8)
  br label %bb4

bb2:                                              ; preds = %bb0
  br label %bb3

bb4:                                              ; preds = %bb1
  br label %bb3

bb3:                                              ; preds = %bb4, %bb2
  ret void
}
