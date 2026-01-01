; ModuleID = '05_struct_generation'
source_filename = "05_struct_generation"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.05_struct_generation.0 = private unnamed_addr constant [16 x i8] c"x=%lld, y=%lld\0A\00", align 1
@.str.05_struct_generation.1 = private unnamed_addr constant [18 x i8] c"array size: %lld\0A\00", align 1

define i32 @main() {
bb0:
  %alloca_11 = alloca [256 x i64], align 8
  %alloca_count_11 = alloca [256 x i64], align 8
  %alloca_2 = alloca { i64, i64 }, align 8
  %alloca_count_2 = alloca { i64, i64 }, align 8
  %alloca_0 = alloca { i64, i64 }, align 8
  %alloca_count_0 = alloca { i64, i64 }, align 8
  store { i64, i64 } { i64 100, i64 20 }, ptr %alloca_count_0, align 8
  store { i64, i64 } { i64 100, i64 20 }, ptr %alloca_count_2, align 8
  %load_5 = load i64, ptr %alloca_count_0, align 8
  %gep_7 = getelementptr inbounds i8, ptr %alloca_count_2, i64 8
  %load_9 = load i64, ptr %gep_7, align 8
  %call_10 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.0, i64 %load_5, i64 %load_9)
  br label %bb1

bb1:                                              ; preds = %bb0
  store [256 x i64] zeroinitializer, ptr %alloca_count_11, align 8
  %call_13 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.1, i64 256)
  br label %bb2

bb2:                                              ; preds = %bb1
  ret i32 0
}

declare i32 @printf(ptr, ...)
