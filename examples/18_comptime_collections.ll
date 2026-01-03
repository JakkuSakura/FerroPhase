; ModuleID = '18_comptime_collections'
source_filename = "18_comptime_collections"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_0 = internal constant i64 6, align 8
@global_1 = internal constant i64 16, align 8
@global_2 = internal constant i64 4, align 8
@.str.18_comptime_collections.0 = private unnamed_addr constant [34 x i8] c"=== Compile-time Collections ===\0A\00", align 1
@.str.18_comptime_collections.1 = private unnamed_addr constant [15 x i8] c"Vec literals:\0A\00", align 1
@.str.18_comptime_collections.2 = private unnamed_addr constant [25 x i8] c"  primes: %lld elements\0A\00", align 1
@.str.18_comptime_collections.3 = private unnamed_addr constant [30 x i8] c"  zero buffer: %lld elements\0A\00", align 1
@.str.18_comptime_collections.4 = private unnamed_addr constant [37 x i8] c"\0AHashMap literal via HashMap::from:\0A\00", align 1
@.str.18_comptime_collections.5 = private unnamed_addr constant [39 x i8] c"  tracked HTTP statuses: %lld entries\0A\00", align 1

define i32 @main() {
bb0:
  %alloca_14 = alloca i64, align 8
  %alloca_count_14 = alloca i64, align 8
  %alloca_9 = alloca i64, align 8
  %alloca_count_9 = alloca i64, align 8
  %alloca_5 = alloca i64, align 8
  %alloca_count_5 = alloca i64, align 8
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  store i64 6, ptr %alloca_count_5, align 8
  %load_7 = load i64, ptr %alloca_count_5, align 8
  %call_8 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.2, i64 %load_7)
  br label %bb3

bb3:                                              ; preds = %bb2
  store i64 16, ptr %alloca_count_9, align 8
  %load_11 = load i64, ptr %alloca_count_9, align 8
  %call_12 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.3, i64 %load_11)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_13 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  store i64 4, ptr %alloca_count_14, align 8
  %load_16 = load i64, ptr %alloca_count_14, align 8
  %call_17 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.5, i64 %load_16)
  br label %bb6

bb6:                                              ; preds = %bb5
  ret i32 0
}

declare i32 @printf(ptr, ...)
