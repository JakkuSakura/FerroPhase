; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [21 x i8] [i8 71, i8 101, i8 110, i8 101, i8 114, i8 105, i8 99, i8 32, i8 111, i8 112, i8 101, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 115, i8 58, i8 10, i8 0], align 1
@.str.1 = constant [15 x i8] [i8 10, i8 67, i8 111, i8 110, i8 100, i8 105, i8 116, i8 105, i8 111, i8 110, i8 97, i8 108, i8 58, i8 10, i8 0], align 1
@.str.2 = constant [4 x i8] [i8 37, i8 100, i8 10, i8 0], align 1
@.str.3 = constant [19 x i8] [i8 10, i8 67, i8 108, i8 111, i8 115, i8 117, i8 114, i8 101, i8 32, i8 102, i8 97, i8 99, i8 116, i8 111, i8 114, i8 121, i8 58, i8 10, i8 0], align 1
@.str.4 = constant [16 x i8] [i8 97, i8 100, i8 100, i8 95, i8 49, i8 48, i8 40, i8 53, i8 41, i8 32, i8 61, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.5 = constant [16 x i8] [i8 100, i8 111, i8 117, i8 98, i8 108, i8 101, i8 40, i8 55, i8 41, i8 32, i8 61, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.6 = constant [4 x i8] [i8 37, i8 102, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i64 @__closure0_call({ i64 } %arg0, i64 %arg1) {
bb0:
  %alloca_0 = alloca i64, align 8
  %alloca_1 = alloca { i64 }, align 8
  store { i64 } %arg0, ptr %alloca_1
  %bitcast_3 = bitcast ptr %alloca_1 to ptr
  %load_4 = load i64, ptr %bitcast_3
  %add_5 = add i64 %arg1, %load_4
  store i64 %add_5, ptr %alloca_0
  %load_7 = load i64, ptr %alloca_0
  ret i64 %load_7
}

define i64 @__closure1_call({ i8 } %arg0, i64 %arg1) {
bb0:
  %alloca_8 = alloca i64, align 8
  %mul_9 = mul i64 %arg1, 2
  store i64 %mul_9, ptr %alloca_8
  %load_11 = load i64, ptr %alloca_8
  ret i64 %load_11
}

define i64 @apply_if(i1 %arg0, i64 %arg1, i64 %arg2, ptr %arg3) {
bb0:
  %alloca_12 = alloca i64, align 8
  %cond_bool_1_2 = icmp ne i1 %arg0, 0
  br i1 %cond_bool_1_2, label %bb1, label %bb2
bb1:
  %call_13 = call i64 (i64, i64) %arg3(i64 %arg1, i64 %arg2)
  store i64 %call_13, ptr %alloca_12
  br label %bb4
bb2:
  store i64 0, ptr %alloca_12
  br label %bb3
bb4:
  br label %bb3
bb3:
  %load_16 = load i64, ptr %alloca_12
  ret i64 %load_16
}

define { i64 } @make_adder(i64 %arg0) {
bb0:
  %alloca_17 = alloca { i64 }, align 8
  %insertvalue_18 = insertvalue { i64 } undef, i64 %arg0, 0
  store { i64 } %insertvalue_18, ptr %alloca_17
  %load_20 = load { i64 }, ptr %alloca_17
  ret { i64 } %load_20
}

define i32 @main() {
bb0:
  %call_21 = call i32 (ptr, ...) @printf(ptr @.str.0)
  br label %bb1
bb1:
  call void (i64, i64, ptr) @apply__spec0(i64 10, i64 20, ptr @add__spec0)
  br label %bb2
bb2:
  call void (double, double, ptr) @apply__spec1(double 1.500000e0, double 2.500000e0, ptr @add__spec1)
  br label %bb3
bb3:
  %call_24 = call i32 (ptr, ...) @printf(ptr @.str.1)
  br label %bb4
bb4:
  %call_25 = call i64 (i1, i64, i64, ptr) @apply_if(i1 1, i64 5, i64 3, ptr @add__spec0)
  br label %bb5
bb5:
  %call_26 = call i32 (ptr, ...) @printf(ptr @.str.2, i64 %call_25)
  br label %bb6
bb6:
  %call_27 = call i64 (i1, i64, i64, ptr) @apply_if(i1 0, i64 5, i64 3, ptr @add__spec0)
  br label %bb7
bb7:
  %call_28 = call i32 (ptr, ...) @printf(ptr @.str.2, i64 %call_27)
  br label %bb8
bb8:
  %call_29 = call i32 (ptr, ...) @printf(ptr @.str.3)
  br label %bb9
bb9:
  %call_30 = call { i64 } (i64) @make_adder(i64 10)
  br label %bb10
bb10:
  %call_31 = call i64 ({ i64 }, i64) @__closure0_call({ i64 } %call_30, i64 5)
  br label %bb11
bb11:
  %call_32 = call i32 (ptr, ...) @printf(ptr @.str.4, i64 %call_31)
  br label %bb12
bb12:
  %alloca_33 = alloca { i8 }, align 1
  store { i8 } { i8 0 }, ptr %alloca_33
  %load_35 = load { i8 }, ptr %alloca_33
  %call_36 = call i64 ({ i8 }, i64) @__closure1_call({ i8 } %load_35, i64 7)
  br label %bb13
bb13:
  %call_37 = call i32 (ptr, ...) @printf(ptr @.str.5, i64 %call_36)
  br label %bb14
bb14:
  ret i32 0
}

define void @apply__spec0(i64 %arg0, i64 %arg1, ptr %arg2) {
bb0:
  %call_38 = call i64 (i64, i64) %arg2(i64 %arg0, i64 %arg1)
  br label %bb1
bb1:
  %call_39 = call i32 (ptr, ...) @printf(ptr @.str.2, i64 %call_38)
  br label %bb2
bb2:
  ret void
}

define i64 @add__spec0(i64 %arg0, i64 %arg1) {
bb0:
  %alloca_40 = alloca i64, align 8
  %add_41 = add i64 %arg0, %arg1
  store i64 %add_41, ptr %alloca_40
  %load_43 = load i64, ptr %alloca_40
  ret i64 %load_43
}

define void @apply__spec1(double %arg0, double %arg1, ptr %arg2) {
bb0:
  %call_44 = call double (double, double) %arg2(double %arg0, double %arg1)
  br label %bb1
bb1:
  %call_45 = call i32 (ptr, ...) @printf(ptr @.str.6, double %call_44)
  br label %bb2
bb2:
  ret void
}

define double @add__spec1(double %arg0, double %arg1) {
bb0:
  %alloca_46 = alloca double, align 8
  %fadd_47 = fadd double %arg0, %arg1
  store double %fadd_47, ptr %alloca_46
  %load_49 = load double, ptr %alloca_46
  ret double %load_49
}

