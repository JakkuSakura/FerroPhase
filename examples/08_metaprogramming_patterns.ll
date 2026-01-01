; ModuleID = '08_metaprogramming_patterns'
source_filename = "08_metaprogramming_patterns"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.08_metaprogramming_patterns.0 = private unnamed_addr constant [20 x i8] c"%s has %llu fields\0A\00", align 1
@.str.08_metaprogramming_patterns.1 = private unnamed_addr constant [24 x i8] c"tag discriminant: %hhu\0A\00", align 1
@.str.08_metaprogramming_patterns.2 = private unnamed_addr constant [8 x i8] c"Point3D\00", align 1

define i32 @main() {
bb0:
  %alloca_8 = alloca i8, align 1
  %alloca_count_8 = alloca i8, align 1
  %alloca_5 = alloca i64, align 8
  %alloca_count_5 = alloca i64, align 8
  %alloca_3 = alloca i64, align 8
  %alloca_count_3 = alloca i64, align 8
  %call_0 = call ptr @Point3D__type_name()
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_1 = call i64 @Point3D__field_count()
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.0, ptr %call_0, i64 %call_1)
  br label %bb3

bb3:                                              ; preds = %bb2
  store i64 1, ptr %alloca_count_3, align 8
  %load_6 = load i64, ptr %alloca_count_3, align 8
  store i64 %load_6, ptr %alloca_count_5, align 8
  %load_9 = load i64, ptr %alloca_count_5, align 8
  %trunc = trunc i64 %load_9 to i8
  store i8 %trunc, ptr %alloca_count_8, align 1
  %load_12 = load i8, ptr %alloca_count_8, align 1
  %zext = zext i8 %load_12 to i32
  %call_14 = call i32 (ptr, ...) @printf(ptr @.str.08_metaprogramming_patterns.1, i32 %zext)
  br label %bb4

bb4:                                              ; preds = %bb3
  ret i32 0
}

define internal ptr @Point3D__type_name() {
bb0:
  %alloca_15 = alloca ptr, align 8
  %alloca_count_15 = alloca ptr, align 8
  store ptr @.str.08_metaprogramming_patterns.2, ptr %alloca_count_15, align 8
  %load_17 = load ptr, ptr %alloca_count_15, align 8
  ret ptr %load_17
}

define internal i64 @Point3D__field_count() {
bb0:
  %alloca_18 = alloca i64, align 8
  %alloca_count_18 = alloca i64, align 8
  store i64 3, ptr %alloca_count_18, align 8
  %load_20 = load i64, ptr %alloca_count_18, align 8
  ret i64 %load_20
}

declare i32 @printf(ptr, ...)
