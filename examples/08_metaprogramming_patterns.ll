; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [18 x i8] [i8 37, i8 115, i8 32, i8 104, i8 97, i8 115, i8 32, i8 37, i8 100, i8 32, i8 102, i8 105, i8 101, i8 108, i8 100, i8 115, i8 10, i8 0], align 1
@.str.1 = constant [22 x i8] [i8 116, i8 97, i8 103, i8 32, i8 100, i8 105, i8 115, i8 99, i8 114, i8 105, i8 109, i8 105, i8 110, i8 97, i8 110, i8 116, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.2 = constant [8 x i8] c"Point3D\00", align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %call_0 = call ptr () @type_name()
  br label %bb1
bb1:
  %call_1 = call i64 () @field_count()
  br label %bb2
bb2:
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.0, ptr %call_0, i64 %call_1)
  br label %bb3
bb3:
  %alloca_3 = alloca i64, align 8
  store i64 1, ptr %alloca_3
  %alloca_5 = alloca i8, align 1
  %load_6 = load i64, ptr %alloca_3
  %sext_trunc_7 = trunc i64 %load_6 to i8
  store i8 %sext_trunc_7, ptr %alloca_5
  %load_9 = load i8, ptr %alloca_5
  %zext_10 = zext i8 %load_9 to i32
  %call_11 = call i32 (ptr, ...) @printf(ptr @.str.1, i32 %zext_10)
  br label %bb4
bb4:
  ret i32 0
}

define ptr @type_name() {
bb0:
  %alloca_12 = alloca ptr, align 8
  store ptr @.str.2, ptr %alloca_12
  ret ptr @.str.2
}

define i64 @field_count() {
bb0:
  %alloca_14 = alloca i64, align 8
  store i64 3, ptr %alloca_14
  ret i64 3
}

