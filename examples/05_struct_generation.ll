; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [39 x i8] c"=== Generated Struct Configuration ===\00", align 1
@.str.1 = constant [54 x i8] c"  logging=true profiling=false priority=1 clients=128\00", align 1
@.str.2 = constant [33 x i8] c"  metrics: window=8 smoothing=3 \00", align 1
@.str.3 = constant [43 x i8] [i8 10, i8 91, i8 111, i8 107, i8 93, i8 32, i8 83, i8 116, i8 114, i8 117, i8 99, i8 116, i8 32, i8 103, i8 101, i8 110, i8 101, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 115, i8 99, i8 101, i8 110, i8 97, i8 114, i8 105, i8 111, i8 115, i8 32, i8 99, i8 111, i8 109, i8 112, i8 108, i8 101, i8 116, i8 101, i8 0], align 1
declare void @func_0()
declare i32 @puts(ptr)
define i32 @main() {
bb0:
  call i32 (ptr) @puts(ptr @.str.0)
  br label %bb1
bb1:
  call i32 (ptr) @puts(ptr @.str.1)
  br label %bb2
bb2:
  call i32 (ptr) @puts(ptr @.str.2)
  br label %bb3
bb3:
  call i32 (ptr) @puts(ptr @.str.3)
  br label %bb4
bb4:
  ret i32 0
}

