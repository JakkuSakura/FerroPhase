; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [27 x i8] [i8 61, i8 61, i8 61, i8 32, i8 83, i8 116, i8 114, i8 117, i8 99, i8 116, i8 32, i8 79, i8 112, i8 101, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 115, i8 32, i8 61, i8 61, i8 61, i8 10, i8 0], align 1
@.str.1 = constant [19 x i8] [i8 112, i8 49, i8 32, i8 61, i8 32, i8 40, i8 37, i8 108, i8 108, i8 100, i8 44, i8 32, i8 37, i8 108, i8 108, i8 100, i8 41, i8 10, i8 0], align 1
@.str.2 = constant [19 x i8] [i8 112, i8 50, i8 32, i8 61, i8 32, i8 40, i8 37, i8 108, i8 108, i8 100, i8 44, i8 32, i8 37, i8 108, i8 108, i8 100, i8 41, i8 10, i8 0], align 1
@.str.3 = constant [35 x i8] [i8 112, i8 49, i8 32, i8 97, i8 102, i8 116, i8 101, i8 114, i8 32, i8 116, i8 114, i8 97, i8 110, i8 115, i8 108, i8 97, i8 116, i8 101, i8 32, i8 61, i8 32, i8 40, i8 37, i8 108, i8 108, i8 100, i8 44, i8 32, i8 37, i8 108, i8 108, i8 100, i8 41, i8 10, i8 0], align 1
@.str.4 = constant [27 x i8] [i8 68, i8 105, i8 115, i8 116, i8 97, i8 110, i8 99, i8 101, i8 194, i8 178, i8 40, i8 112, i8 49, i8 44, i8 32, i8 112, i8 50, i8 41, i8 32, i8 61, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.5 = constant [23 x i8] [i8 82, i8 101, i8 99, i8 116, i8 97, i8 110, i8 103, i8 108, i8 101, i8 58, i8 32, i8 37, i8 108, i8 108, i8 100, i8 195, i8 151, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.6 = constant [15 x i8] [i8 32, i8 32, i8 97, i8 114, i8 101, i8 97, i8 32, i8 61, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.7 = constant [20 x i8] [i8 32, i8 32, i8 112, i8 101, i8 114, i8 105, i8 109, i8 101, i8 116, i8 101, i8 114, i8 32, i8 61, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.8 = constant [18 x i8] [i8 32, i8 32, i8 105, i8 115, i8 95, i8 115, i8 113, i8 117, i8 97, i8 114, i8 101, i8 32, i8 61, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i64 @Point__new(i32 %arg0, i32 %arg1) {
bb0:
  ret i64 0
}

define void @Point__translate(i32 %arg0, i32 %arg1, i32 %arg2) {
bb0:
  %alloca_0 = alloca i64, align 8
  %load_1 = load i64, ptr %alloca_0
  %add_2 = add i64 %load_1, %arg1
  store i64 %add_2, ptr %alloca_0
  %load_4 = load i64, ptr %alloca_0
  %add_5 = add i64 %load_4, %arg2
  store i64 %add_5, ptr %alloca_0
  ret void
}

define i64 @Point__distance2(i32 %arg0, i32 %arg1) {
bb0:
  %sub_7 = sub i64 %arg0, %arg1
  %sub_8 = sub i64 %arg0, %arg1
  %mul_9 = mul i64 %sub_7, %sub_7
  %mul_10 = mul i64 %sub_8, %sub_8
  %add_11 = add i64 %mul_9, %mul_10
  %mul_12 = mul i64 %sub_7, %sub_7
  %mul_13 = mul i64 %sub_8, %sub_8
  %add_14 = add i64 %mul_12, %mul_13
  ret i64 %add_14
}

define i64 @Rectangle__new(i32 %arg0, i32 %arg1) {
bb0:
  ret i64 0
}

define i64 @Rectangle__area(i32 %arg0) {
bb0:
  %mul_15 = mul i64 %arg0, %arg0
  ret i64 %mul_15
}

define i64 @Rectangle__perimeter(i32 %arg0) {
bb0:
  %add_16 = add i64 %arg0, %arg0
  %mul_17 = mul i64 2, %add_16
  ret i64 %mul_17
}

define i1 @Rectangle__is_square(i32 %arg0) {
bb0:
  %icmp_18 = icmp eq i64 %arg0, %arg0
  ret i1 %icmp_18
}

define i32 @main() {
bb0:
  call i32 (ptr, ...) @printf(ptr @.str.0)
  br label %bb1
bb1:
  %call_20 = call i64 (i64, i64) @Point__new(i64 10, i64 20)
  br label %bb2
bb2:
  %call_21 = call i64 (i64, i64) @Point__new(i64 5, i64 15)
  br label %bb3
bb3:
  call i32 (ptr, ...) @printf(ptr @.str.1, i64 %call_20, i64 %call_20)
  br label %bb4
bb4:
  call i32 (ptr, ...) @printf(ptr @.str.2, i64 %call_21, i64 %call_21)
  br label %bb5
bb5:
  %sub_24 = sub i64 0, 4
  call void (i64, i64, i64) @translate(i64 %call_20, i64 3, i64 %sub_24)
  br label %bb6
bb6:
  call i32 (ptr, ...) @printf(ptr @.str.3, i64 %call_20, i64 %call_20)
  br label %bb7
bb7:
  %call_27 = call i64 (i64, i64) @distance2(i64 %call_20, i64 %call_21)
  br label %bb8
bb8:
  call i32 (ptr, ...) @printf(ptr @.str.4, i64 %call_27)
  br label %bb9
bb9:
  %call_29 = call i64 (i64, i64) @Rectangle__new(i64 10, i64 5)
  br label %bb10
bb10:
  call i32 (ptr, ...) @printf(ptr @.str.5, i64 %call_29, i64 %call_29)
  br label %bb11
bb11:
  %call_31 = call i64 (i64) @area(i64 %call_29)
  br label %bb12
bb12:
  call i32 (ptr, ...) @printf(ptr @.str.6, i64 %call_31)
  br label %bb13
bb13:
  %call_33 = call i64 (i64) @perimeter(i64 %call_29)
  br label %bb14
bb14:
  call i32 (ptr, ...) @printf(ptr @.str.7, i64 %call_33)
  br label %bb15
bb15:
  %call_35 = call i1 (i64) @is_square(i64 %call_29)
  br label %bb16
bb16:
  call i32 (ptr, ...) @printf(ptr @.str.8, i1 %call_35)
  br label %bb17
bb17:
  ret i32 0
}

