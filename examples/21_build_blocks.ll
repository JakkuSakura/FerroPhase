; ModuleID = '21_build_blocks'
source_filename = "21_build_blocks"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.21_build_blocks.0 = constant [35 x i8] c"\F0\9F\93\98 Tutorial: 21_build_blocks.fp\0A\00"
@.str.21_build_blocks.1 = constant [80 x i8] c"\F0\9F\A7\AD Focus: Build blocks with typed quote tokens and expression-driven splice.\0A\00"
@.str.21_build_blocks.2 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.21_build_blocks.3 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.21_build_blocks.4 = constant [2 x i8] c"\0A\00"
@.str.21_build_blocks.5 = constant [41 x i8] c"build_items(true) -> Alpha { id: %lld }\0A\00"
@.str.21_build_blocks.6 = constant [43 x i8] c"build_items_2(false) -> Beta { id: %lld }\0A\00"
@.str.21_build_blocks.7 = constant [39 x i8] c"generated types are usable at runtime\0A\00"

define i32 @main() {
bb0:
  %alloca_10 = alloca { i64 }, align 8
  %alloca_count_10 = alloca { i64 }, align 8
  %alloca_5 = alloca { i64 }, align 8
  %alloca_count_5 = alloca { i64 }, align 8
  %call_0 = call i32 (ptr, ...) @printf(ptr @.str.21_build_blocks.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.21_build_blocks.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.21_build_blocks.2)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.21_build_blocks.3)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.21_build_blocks.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  store { i64 } { i64 1 }, ptr %alloca_count_5, align 8
  %load_8 = load i64, ptr %alloca_count_5, align 8
  %call_9 = call i32 (ptr, ...) @printf(ptr @.str.21_build_blocks.5, i64 %load_8)
  br label %bb6

bb6:                                              ; preds = %bb5
  store { i64 } { i64 2 }, ptr %alloca_count_10, align 8
  %load_13 = load i64, ptr %alloca_count_10, align 8
  %call_14 = call i32 (ptr, ...) @printf(ptr @.str.21_build_blocks.6, i64 %load_13)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_15 = call i32 (ptr, ...) @printf(ptr @.str.21_build_blocks.7)
  br label %bb8

bb8:                                              ; preds = %bb7
  ret i32 0
}

declare i32 @printf(ptr, ...)
