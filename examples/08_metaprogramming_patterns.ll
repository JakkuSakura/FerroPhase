; ModuleID = '08_metaprogramming_patterns'
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.08_metaprogramming_patterns.0 = constant [20 x i8] [i8 37, i8 115, i8 32, i8 104, i8 97, i8 115, i8 32, i8 37, i8 108, i8 108, i8 117, i8 32, i8 102, i8 105, i8 101, i8 108, i8 100, i8 115, i8 10, i8 0], align 1
@.str.08_metaprogramming_patterns.1 = constant [24 x i8] [i8 116, i8 97, i8 103, i8 32, i8 100, i8 105, i8 115, i8 99, i8 114, i8 105, i8 109, i8 105, i8 110, i8 97, i8 110, i8 116, i8 58, i8 32, i8 37, i8 104, i8 104, i8 117, i8 10, i8 0], align 1
@.str.08_metaprogramming_patterns.2 = constant [8 x i8] c"Point3D\00", align 1
declare i32 @printf(ptr, ...)
define i64 @field_count() {
bb0:
  %alloca_18 = alloca i64, align 8
  store i64 3, ptr %alloca_18
  %load_20 = load i64, ptr %alloca_18
  ret i64 %load_20
}

define i32 @main() {
bb0:
  %call_0 = call ptr () @type_name()
  br label %bb1
bb1:
  %call_1 = call i64 () @field_count()
  br label %bb2
bb2:
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.0, ptr %call_0, i64 %call_1)
  br label %bb3
bb3:
  %alloca_3 = alloca i64, align 8
  store { i64 } { i64 1 }, ptr %alloca_3
  %alloca_5 = alloca i64, align 8
  %load_6 = load i64, ptr %alloca_3
  store i64 %load_6, ptr %alloca_5
  %alloca_8 = alloca i8, align 1
  %load_9 = load i64, ptr %alloca_5
  %sext_trunc_10 = trunc i64 %load_9 to i8
  store i8 %sext_trunc_10, ptr %alloca_8
  %load_12 = load i8, ptr %alloca_8
  %zext_13 = zext i8 %load_12 to i32
  %call_14 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.1, i32 %zext_13)
  br label %bb4
bb4:
  ret i32 0
}

define ptr @type_name() {
bb0:
  %alloca_15 = alloca ptr, align 8
  store ptr @.str.08_metaprogramming_patterns.2, ptr %alloca_15
  %load_17 = load ptr, ptr %alloca_15
  ret ptr %load_17
}

