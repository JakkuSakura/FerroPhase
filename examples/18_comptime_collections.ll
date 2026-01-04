; ModuleID = '18_comptime_collections'
source_filename = "18_comptime_collections"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.18_comptime_collections.0 = private unnamed_addr constant [34 x i8] c"=== Compile-time Collections ===\0A\00", align 1
@.str.18_comptime_collections.1 = private unnamed_addr constant [15 x i8] c"Vec literals:\0A\00", align 1
@.str.18_comptime_collections.2 = private unnamed_addr constant [31 x i8] c"  primes: %lld elements -> %s\0A\00", align 1
@.str.18_comptime_collections.3 = private unnamed_addr constant [21 x i8] c"[2, 3, 5, 7, 11, 13]\00", align 1
@.str.18_comptime_collections.4 = private unnamed_addr constant [30 x i8] c"  zero buffer: %lld elements\0A\00", align 1
@.str.18_comptime_collections.5 = private unnamed_addr constant [46 x i8] c"  first four zeros: [%lld, %lld, %lld, %lld]\0A\00", align 1
@.str.18_comptime_collections.6 = private unnamed_addr constant [37 x i8] c"\0AHashMap literal via HashMap::from:\0A\00", align 1
@.str.18_comptime_collections.7 = private unnamed_addr constant [39 x i8] c"  tracked HTTP statuses: %lld entries\0A\00", align 1
@.str.18_comptime_collections.8 = private unnamed_addr constant [14 x i8] c"  ok => %lld\0A\00", align 1
@.str.18_comptime_collections.9 = private unnamed_addr constant [21 x i8] c"  not_found => %lld\0A\00", align 1

define i32 @main() {
bb0:
  %call_0 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.2, i64 6, ptr @.str.18_comptime_collections.3)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.4, i64 16)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.5, i64 0, i64 0, i64 0, i64 0)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_5 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.6)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_6 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.7, i64 4)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_7 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.8, i64 200)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_8 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.9, i64 404)
  br label %bb9

bb9:                                              ; preds = %bb8
  ret i32 0
}

declare i32 @printf(ptr, ...)
