; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [30 x i8] [i8 61, i8 61, i8 61, i8 32, i8 83, i8 116, i8 114, i8 117, i8 99, i8 116, i8 32, i8 73, i8 110, i8 116, i8 114, i8 111, i8 115, i8 112, i8 101, i8 99, i8 116, i8 105, i8 111, i8 110, i8 32, i8 61, i8 61, i8 61, i8 10, i8 0], align 1
@.str.1 = constant [22 x i8] [i8 80, i8 111, i8 105, i8 110, i8 116, i8 32, i8 115, i8 105, i8 122, i8 101, i8 58, i8 32, i8 37, i8 100, i8 32, i8 98, i8 121, i8 116, i8 101, i8 115, i8 10, i8 0], align 1
@.str.2 = constant [22 x i8] [i8 67, i8 111, i8 108, i8 111, i8 114, i8 32, i8 115, i8 105, i8 122, i8 101, i8 58, i8 32, i8 37, i8 100, i8 32, i8 98, i8 121, i8 116, i8 101, i8 115, i8 10, i8 0], align 1
@.str.3 = constant [18 x i8] [i8 80, i8 111, i8 105, i8 110, i8 116, i8 32, i8 102, i8 105, i8 101, i8 108, i8 100, i8 115, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.4 = constant [18 x i8] [i8 67, i8 111, i8 108, i8 111, i8 114, i8 32, i8 102, i8 105, i8 101, i8 108, i8 100, i8 115, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.5 = constant [17 x i8] [i8 80, i8 111, i8 105, i8 110, i8 116, i8 32, i8 104, i8 97, i8 115, i8 32, i8 120, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.6 = constant [17 x i8] [i8 80, i8 111, i8 105, i8 110, i8 116, i8 32, i8 104, i8 97, i8 115, i8 32, i8 122, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.7 = constant [19 x i8] [i8 80, i8 111, i8 105, i8 110, i8 116, i8 32, i8 109, i8 101, i8 116, i8 104, i8 111, i8 100, i8 115, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.8 = constant [19 x i8] [i8 67, i8 111, i8 108, i8 111, i8 114, i8 32, i8 109, i8 101, i8 116, i8 104, i8 111, i8 100, i8 115, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.9 = constant [31 x i8] [i8 10, i8 226, i8 156, i8 147, i8 32, i8 73, i8 110, i8 116, i8 114, i8 111, i8 115, i8 112, i8 101, i8 99, i8 116, i8 105, i8 111, i8 110, i8 32, i8 99, i8 111, i8 109, i8 112, i8 108, i8 101, i8 116, i8 101, i8 100, i8 33, i8 10, i8 0], align 1
@.str.10 = constant [29 x i8] [i8 10, i8 61, i8 61, i8 61, i8 32, i8 84, i8 114, i8 97, i8 110, i8 115, i8 112, i8 105, i8 108, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 68, i8 101, i8 109, i8 111, i8 32, i8 61, i8 61, i8 61, i8 10, i8 0], align 1
@.str.11 = constant [29 x i8] [i8 84, i8 114, i8 97, i8 110, i8 115, i8 112, i8 105, i8 108, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 116, i8 97, i8 114, i8 103, i8 101, i8 116, i8 32, i8 115, i8 105, i8 122, i8 101, i8 115, i8 58, i8 10, i8 0], align 1
@.str.12 = constant [27 x i8] [i8 32, i8 32, i8 80, i8 111, i8 105, i8 110, i8 116, i8 58, i8 32, i8 37, i8 100, i8 32, i8 98, i8 121, i8 116, i8 101, i8 115, i8 32, i8 40, i8 99, i8 111, i8 110, i8 115, i8 116, i8 41, i8 10, i8 0], align 1
@.str.13 = constant [27 x i8] [i8 32, i8 32, i8 67, i8 111, i8 108, i8 111, i8 114, i8 58, i8 32, i8 37, i8 100, i8 32, i8 98, i8 121, i8 116, i8 101, i8 115, i8 32, i8 40, i8 99, i8 111, i8 110, i8 115, i8 116, i8 41, i8 10, i8 0], align 1
@.str.14 = constant [22 x i8] [i8 32, i8 32, i8 67, i8 111, i8 109, i8 98, i8 105, i8 110, i8 101, i8 100, i8 58, i8 32, i8 37, i8 100, i8 32, i8 98, i8 121, i8 116, i8 101, i8 115, i8 10, i8 0], align 1
@.str.15 = constant [20 x i8] [i8 82, i8 117, i8 110, i8 116, i8 105, i8 109, i8 101, i8 32, i8 105, i8 110, i8 115, i8 116, i8 97, i8 110, i8 99, i8 101, i8 115, i8 58, i8 10, i8 0], align 1
@.str.16 = constant [20 x i8] [i8 32, i8 32, i8 79, i8 114, i8 105, i8 103, i8 105, i8 110, i8 58, i8 32, i8 40, i8 37, i8 102, i8 44, i8 32, i8 37, i8 102, i8 41, i8 10, i8 0], align 1
@.str.17 = constant [24 x i8] [i8 32, i8 32, i8 82, i8 101, i8 100, i8 58, i8 32, i8 114, i8 103, i8 98, i8 40, i8 37, i8 100, i8 44, i8 32, i8 37, i8 100, i8 44, i8 32, i8 37, i8 100, i8 41, i8 10, i8 0], align 1
@.str.18 = constant [54 x i8] [i8 10, i8 226, i8 156, i8 147, i8 32, i8 73, i8 110, i8 116, i8 114, i8 111, i8 115, i8 112, i8 101, i8 99, i8 116, i8 105, i8 111, i8 110, i8 32, i8 101, i8 110, i8 97, i8 98, i8 108, i8 101, i8 115, i8 32, i8 101, i8 120, i8 116, i8 101, i8 114, i8 110, i8 97, i8 108, i8 32, i8 99, i8 111, i8 100, i8 101, i8 32, i8 103, i8 101, i8 110, i8 101, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 33, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %call_0 = call i32 (ptr, ...) @printf(ptr @.str.0)
  br label %bb1
bb1:
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.1, i64 16)
  br label %bb2
bb2:
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.2, i64 24)
  br label %bb3
bb3:
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.3, i64 2)
  br label %bb4
bb4:
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.4, i64 3)
  br label %bb5
bb5:
  %zext_5 = zext i1 1 to i32
  %call_6 = call i32 (ptr, ...) @printf(ptr @.str.5, i32 %zext_5)
  br label %bb6
bb6:
  %zext_7 = zext i1 0 to i32
  %call_8 = call i32 (ptr, ...) @printf(ptr @.str.6, i32 %zext_7)
  br label %bb7
bb7:
  %call_9 = call i32 (ptr, ...) @printf(ptr @.str.7, i64 0)
  br label %bb8
bb8:
  %call_10 = call i32 (ptr, ...) @printf(ptr @.str.8, i64 0)
  br label %bb9
bb9:
  %call_11 = call i32 (ptr, ...) @printf(ptr @.str.9)
  br label %bb10
bb10:
  %call_12 = call i32 (ptr, ...) @printf(ptr @.str.10)
  br label %bb11
bb11:
  %alloca_13 = alloca { double, double }, align 8
  store { double, double } { double 0.000000e0, double 0.000000e0 }, ptr %alloca_13
  %alloca_15 = alloca { i8, i8, i8 }, align 1
  store { i8, i8, i8 } { i8 255, i8 0, i8 0 }, ptr %alloca_15
  %call_17 = call i32 (ptr, ...) @printf(ptr @.str.11)
  br label %bb12
bb12:
  %call_18 = call i32 (ptr, ...) @printf(ptr @.str.12, i64 16)
  br label %bb13
bb13:
  %call_19 = call i32 (ptr, ...) @printf(ptr @.str.13, i64 24)
  br label %bb14
bb14:
  %call_20 = call i32 (ptr, ...) @printf(ptr @.str.14, i64 40)
  br label %bb15
bb15:
  %call_21 = call i32 (ptr, ...) @printf(ptr @.str.15)
  br label %bb16
bb16:
  %bitcast_22 = bitcast ptr %alloca_13 to ptr
  %load_23 = load double, ptr %bitcast_22
  %bitcast_24 = bitcast ptr %alloca_13 to ptr
  %gep_25 = getelementptr inbounds i8, ptr %bitcast_24, i64 8
  %bitcast_26 = bitcast ptr %gep_25 to ptr
  %load_27 = load double, ptr %bitcast_26
  %call_28 = call i32 (ptr, ...) @printf(ptr @.str.16, double %load_23, double %load_27)
  br label %bb17
bb17:
  %bitcast_29 = bitcast ptr %alloca_15 to ptr
  %load_30 = load i8, ptr %bitcast_29
  %zext_31 = zext i8 %load_30 to i32
  %bitcast_32 = bitcast ptr %alloca_15 to ptr
  %gep_33 = getelementptr inbounds i8, ptr %bitcast_32, i64 1
  %bitcast_34 = bitcast ptr %gep_33 to ptr
  %load_35 = load i8, ptr %bitcast_34
  %zext_36 = zext i8 %load_35 to i32
  %bitcast_37 = bitcast ptr %alloca_15 to ptr
  %gep_38 = getelementptr inbounds i8, ptr %bitcast_37, i64 2
  %bitcast_39 = bitcast ptr %gep_38 to ptr
  %load_40 = load i8, ptr %bitcast_39
  %zext_41 = zext i8 %load_40 to i32
  %call_42 = call i32 (ptr, ...) @printf(ptr @.str.17, i32 %zext_31, i32 %zext_36, i32 %zext_41)
  br label %bb18
bb18:
  %call_43 = call i32 (ptr, ...) @printf(ptr @.str.18)
  br label %bb19
bb19:
  ret i32 0
}

