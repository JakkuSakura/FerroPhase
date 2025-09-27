; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [62 x i8] c"Config: buffer=4KB, connections=150, timeout=30000ms, debug=0\00", align 1
@.str.1 = constant [38 x i8] c"Computed: factorial=120, is_pow2=true\00", align 1
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
  ret i32 0
}

