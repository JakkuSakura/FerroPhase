; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [23 x i8] [i8 65, i8 61, i8 37, i8 100, i8 44, i8 32, i8 66, i8 61, i8 37, i8 100, i8 44, i8 32, i8 65, i8 32, i8 38, i8 38, i8 32, i8 66, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %alloca_0 = alloca i32, align 4
  %alloca_1 = alloca i1, align 1
  store i1 1, ptr %alloca_1
  %alloca_3 = alloca i1, align 1
  %load_4 = load i1, ptr %alloca_1
  store i1 %load_4, ptr %alloca_3
  %alloca_6 = alloca i1, align 1
  store i1 0, ptr %alloca_6
  %alloca_8 = alloca i1, align 1
  %load_9 = load i1, ptr %alloca_6
  store i1 %load_9, ptr %alloca_8
  %alloca_11 = alloca i1, align 1
  %load_12 = load i1, ptr %alloca_3
  %load_13 = load i1, ptr %alloca_8
  %and_14 = and i1 %load_12, %load_13
  store i1 %and_14, ptr %alloca_11
  %alloca_16 = alloca i1, align 1
  %load_17 = load i1, ptr %alloca_11
  store i1 %load_17, ptr %alloca_16
  %load_19 = load i1, ptr %alloca_3
  %load_20 = load i1, ptr %alloca_8
  %load_21 = load i1, ptr %alloca_16
  call i32 (ptr, ...) @printf(ptr @.str.0, i1 %load_19, i1 %load_20, i1 %load_21)
  br label %bb1
bb1:
  store i32 0, ptr %alloca_0
  ret i32 0
}

