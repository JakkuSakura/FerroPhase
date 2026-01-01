; ModuleID = '21_build_blocks'
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

define ptr @build_items(i1 %arg0) {
bb0:
  %alloca_0 = alloca ptr, align 8
  store {  } {  }, ptr %alloca_0
  %load_2 = load ptr, ptr %alloca_0
  ret ptr %load_2
}

define ptr @build_items_2(i1 %arg0) {
bb0:
  %alloca_3 = alloca ptr, align 8
  %cond_bool_1_2 = icmp ne i1 %arg0, 0
  br i1 %cond_bool_1_2, label %bb1, label %bb2
bb1:
  br label %bb3
bb2:
  br label %bb3
bb3:
  %cond_bool_4_5 = icmp ne i1 %arg0, 0
  br i1 %cond_bool_4_5, label %bb4, label %bb5
bb4:
  store {  } {  }, ptr %alloca_3
  br label %bb6
bb5:
  store {  } {  }, ptr %alloca_3
  br label %bb6
bb6:
  %load_6 = load ptr, ptr %alloca_3
  ret ptr %load_6
}

define i32 @main() {
bb0:
  %alloca_7 = alloca { i64 }, align 8
  store { i64 } { i64 1 }, ptr %alloca_7
  %alloca_9 = alloca i64, align 8
  %bitcast_10 = bitcast ptr %alloca_7 to ptr
  %load_11 = load i64, ptr %bitcast_10
  store i64 %load_11, ptr %alloca_9
  %alloca_13 = alloca { i64 }, align 8
  store { i64 } { i64 2 }, ptr %alloca_13
  %alloca_15 = alloca i64, align 8
  %bitcast_16 = bitcast ptr %alloca_13 to ptr
  %load_17 = load i64, ptr %bitcast_16
  store i64 %load_17, ptr %alloca_15
  ret i32 0
}

