; ModuleID = ''
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx12.0.0"

@.str.0 = constant [19 x i8] c"=== While Loop ===\00", align 1
@.str.1 = constant [13 x i8] [i8 67, i8 111, i8 117, i8 110, i8 116, i8 101, i8 114, i8 58, i8 32, i8 123, i8 125, i8 10, i8 0], align 1
@.str.2 = constant [19 x i8] [i8 70, i8 105, i8 110, i8 97, i8 108, i8 32, i8 99, i8 111, i8 117, i8 110, i8 116, i8 101, i8 114, i8 58, i8 32, i8 123, i8 125, i8 10, i8 0], align 1
@.str.3 = constant [34 x i8] [i8 10, i8 61, i8 61, i8 61, i8 32, i8 73, i8 110, i8 102, i8 105, i8 110, i8 105, i8 116, i8 101, i8 32, i8 76, i8 111, i8 111, i8 112, i8 32, i8 119, i8 105, i8 116, i8 104, i8 32, i8 66, i8 114, i8 101, i8 97, i8 107, i8 32, i8 61, i8 61, i8 61, i8 0], align 1
@.str.4 = constant [28 x i8] [i8 10, i8 61, i8 61, i8 61, i8 32, i8 76, i8 111, i8 111, i8 112, i8 32, i8 119, i8 105, i8 116, i8 104, i8 32, i8 67, i8 111, i8 110, i8 116, i8 105, i8 110, i8 117, i8 101, i8 32, i8 61, i8 61, i8 61, i8 0], align 1
@.str.5 = constant [20 x i8] [i8 76, i8 111, i8 111, i8 112, i8 32, i8 105, i8 116, i8 101, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 58, i8 32, i8 123, i8 125, i8 10, i8 0], align 1
@.str.6 = constant [16 x i8] [i8 78, i8 111, i8 116, i8 32, i8 50, i8 32, i8 111, i8 114, i8 32, i8 52, i8 58, i8 32, i8 123, i8 125, i8 10, i8 0], align 1
@.str.7 = constant [22 x i8] [i8 10, i8 61, i8 61, i8 61, i8 32, i8 78, i8 101, i8 115, i8 116, i8 101, i8 100, i8 32, i8 76, i8 111, i8 111, i8 112, i8 115, i8 32, i8 61, i8 61, i8 61, i8 0], align 1
@.str.8 = constant [12 x i8] [i8 120, i8 61, i8 123, i8 125, i8 44, i8 32, i8 121, i8 61, i8 123, i8 125, i8 10, i8 0], align 1
@.str.9 = constant [30 x i8] [i8 10, i8 67, i8 111, i8 110, i8 116, i8 114, i8 111, i8 108, i8 32, i8 102, i8 108, i8 111, i8 119, i8 32, i8 100, i8 101, i8 109, i8 111, i8 32, i8 99, i8 111, i8 109, i8 112, i8 108, i8 101, i8 116, i8 101, i8 100, i8 33, i8 0], align 1
declare void @func_0()
declare i32 @puts(ptr)
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  call i32 @puts(ptr getelementptr inbounds ([19 x i8], ptr @.str.0, i32 0, i32 0))
  br label %bb1
bb1:
  br label %bb2
bb2:
  br i1 1, label %bb4, label %bb5
bb4:
  call i32 @printf(ptr getelementptr inbounds ([13 x i8], ptr @.str.1, i32 0, i32 0), i64 0)
  br label %bb7
bb5:
  br label %bb3
bb7:
  br label %bb6
bb3:
  br label %bb6
bb6:
  br label %bb2
bb8:
  call i32 @printf(ptr getelementptr inbounds ([19 x i8], ptr @.str.2, i32 0, i32 0), i64 0)
  br label %bb9
bb9:
  call i32 @puts(ptr getelementptr inbounds ([34 x i8], ptr @.str.3, i32 0, i32 0))
  br label %bb10
bb10:
  br label %bb11
bb11:
  br i1 0, label %bb13, label %bb14
bb12:
  call i32 @puts(ptr getelementptr inbounds ([28 x i8], ptr @.str.4, i32 0, i32 0))
  br label %bb17
bb13:
  br label %bb15
bb14:
  br label %bb15
bb15:
  call i32 @printf(ptr getelementptr inbounds ([20 x i8], ptr @.str.5, i32 0, i32 0), i64 0)
  br label %bb16
bb16:
  br label %bb11
bb17:
  br label %bb18
bb18:
  br i1 1, label %bb20, label %bb21
bb19:
  br label %bb22
bb20:
  %add_11 = add i32 0, 0
  %cond_bool_23_24 = icmp ne i32 %add_11, 0
  br i1 %cond_bool_23_24, label %bb23, label %bb24
bb21:
  br label %bb19
bb22:
  br label %bb18
bb23:
  br label %bb25
bb24:
  br label %bb25
bb25:
  call i32 @printf(ptr getelementptr inbounds ([16 x i8], ptr @.str.6, i32 0, i32 0), i64 0)
  br label %bb26
bb26:
  br label %bb22
bb27:
  call i32 @puts(ptr getelementptr inbounds ([22 x i8], ptr @.str.7, i32 0, i32 0))
  br label %bb28
bb28:
  br label %bb29
bb29:
  br i1 1, label %bb31, label %bb32
bb30:
  br label %bb33
bb31:
  br label %bb34
bb32:
  br label %bb30
bb33:
  br label %bb29
bb34:
  br i1 1, label %bb36, label %bb37
bb35:
  br label %bb38
bb36:
  call i32 @printf(ptr getelementptr inbounds ([12 x i8], ptr @.str.8, i32 0, i32 0), i64 0, i64 0)
  br label %bb39
bb37:
  br label %bb35
bb38:
  br label %bb34
bb39:
  br label %bb38
bb40:
  br label %bb33
bb41:
  call i32 @puts(ptr getelementptr inbounds ([30 x i8], ptr @.str.9, i32 0, i32 0))
  br label %bb42
bb42:
  ret i32 0
}

