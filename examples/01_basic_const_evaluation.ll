; ModuleID = ''
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx12.0.0"

@.str.0 = constant [62 x i8] c"Config: buffer=4KB, connections=150, timeout=30000ms, debug=0\00", align 1
@.str.1 = constant [38 x i8] c"Computed: factorial=120, is_pow2=true\00", align 1
declare void @func_0()
declare i32 @puts(ptr)
define i32 @main() {
bb0:
  call i32 @puts(ptr getelementptr inbounds ([62 x i8], ptr @.str.0, i32 0, i32 0))
  br label %bb1
bb1:
  call i32 @puts(ptr getelementptr inbounds ([38 x i8], ptr @.str.1, i32 0, i32 0))
  br label %bb2
bb2:
  ret i32 0
}

