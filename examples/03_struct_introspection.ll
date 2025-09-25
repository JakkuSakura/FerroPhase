; ModuleID = ''
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx12.0.0"

@.str.0 = constant [29 x i8] c"=== Struct Introspection ===\00", align 1
@.str.1 = constant [21 x i8] c"Point size: 16 bytes\00", align 1
@.str.2 = constant [21 x i8] c"Color size: 24 bytes\00", align 1
@.str.3 = constant [16 x i8] c"Point fields: 2\00", align 1
@.str.4 = constant [16 x i8] c"Color fields: 3\00", align 1
@.str.5 = constant [18 x i8] c"Point has x: true\00", align 1
@.str.6 = constant [19 x i8] c"Point has z: false\00", align 1
@.str.7 = constant [17 x i8] c"Point methods: 0\00", align 1
@.str.8 = constant [17 x i8] c"Color methods: 0\00", align 1
@.str.9 = constant [30 x i8] [i8 10, i8 226, i8 156, i8 147, i8 32, i8 73, i8 110, i8 116, i8 114, i8 111, i8 115, i8 112, i8 101, i8 99, i8 116, i8 105, i8 111, i8 110, i8 32, i8 99, i8 111, i8 109, i8 112, i8 108, i8 101, i8 116, i8 101, i8 100, i8 33, i8 0], align 1
declare void @func_0()
declare i32 @puts(ptr)
define i32 @main() {
bb0:
  call i32 @puts(ptr getelementptr inbounds ([29 x i8], ptr @.str.0, i32 0, i32 0))
  br label %bb1
bb1:
  call i32 @puts(ptr getelementptr inbounds ([21 x i8], ptr @.str.1, i32 0, i32 0))
  br label %bb2
bb2:
  call i32 @puts(ptr getelementptr inbounds ([21 x i8], ptr @.str.2, i32 0, i32 0))
  br label %bb3
bb3:
  call i32 @puts(ptr getelementptr inbounds ([16 x i8], ptr @.str.3, i32 0, i32 0))
  br label %bb4
bb4:
  call i32 @puts(ptr getelementptr inbounds ([16 x i8], ptr @.str.4, i32 0, i32 0))
  br label %bb5
bb5:
  call i32 @puts(ptr getelementptr inbounds ([18 x i8], ptr @.str.5, i32 0, i32 0))
  br label %bb6
bb6:
  call i32 @puts(ptr getelementptr inbounds ([19 x i8], ptr @.str.6, i32 0, i32 0))
  br label %bb7
bb7:
  call i32 @puts(ptr getelementptr inbounds ([17 x i8], ptr @.str.7, i32 0, i32 0))
  br label %bb8
bb8:
  call i32 @puts(ptr getelementptr inbounds ([17 x i8], ptr @.str.8, i32 0, i32 0))
  br label %bb9
bb9:
  call i32 @puts(ptr getelementptr inbounds ([30 x i8], ptr @.str.9, i32 0, i32 0))
  br label %bb10
bb10:
  ret i32 0
}

