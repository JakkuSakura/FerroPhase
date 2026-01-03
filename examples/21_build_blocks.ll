; ModuleID = '21_build_blocks'
source_filename = "21_build_blocks"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

define internal ptr @build_items(i1 %0) {
bb0:
  %alloca_0 = alloca ptr, align 8
  %alloca_count_0 = alloca ptr, align 8
  %load_1 = load ptr, ptr %alloca_count_0, align 8
  ret ptr %load_1
}

define internal ptr @build_items_2(i1 %0) {
bb0:
  %alloca_2 = alloca ptr, align 8
  %alloca_count_2 = alloca ptr, align 8
  br i1 %0, label %bb1, label %bb2

bb1:                                              ; preds = %bb0
  br label %bb3

bb2:                                              ; preds = %bb0
  br label %bb3

bb3:                                              ; preds = %bb2, %bb1
  %load_3 = load ptr, ptr %alloca_count_2, align 8
  ret ptr %load_3
}

define i32 @main() {
bb0:
  %alloca_12 = alloca i64, align 8
  %alloca_count_12 = alloca i64, align 8
  %alloca_10 = alloca { i64 }, align 8
  %alloca_count_10 = alloca { i64 }, align 8
  %alloca_6 = alloca i64, align 8
  %alloca_count_6 = alloca i64, align 8
  %alloca_4 = alloca { i64 }, align 8
  %alloca_count_4 = alloca { i64 }, align 8
  store { i64 } { i64 1 }, ptr %alloca_count_4, align 8
  %load_8 = load i64, ptr %alloca_count_4, align 8
  store i64 %load_8, ptr %alloca_count_6, align 8
  store { i64 } { i64 2 }, ptr %alloca_count_10, align 8
  %load_14 = load i64, ptr %alloca_count_10, align 8
  store i64 %load_14, ptr %alloca_count_12, align 8
  ret i32 0
}
