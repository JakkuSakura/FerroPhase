; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [4 x i8] [i8 37, i8 100, i8 10, i8 0], align 1
@.str.1 = constant [4 x i8] [i8 37, i8 102, i8 10, i8 0], align 1
@.str.2 = constant [21 x i8] [i8 71, i8 101, i8 110, i8 101, i8 114, i8 105, i8 99, i8 32, i8 111, i8 112, i8 101, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 115, i8 58, i8 10, i8 0], align 1
@.str.3 = constant [15 x i8] [i8 10, i8 67, i8 111, i8 110, i8 100, i8 105, i8 116, i8 105, i8 111, i8 110, i8 97, i8 108, i8 58, i8 10, i8 0], align 1
@.str.4 = constant [19 x i8] [i8 10, i8 67, i8 108, i8 111, i8 115, i8 117, i8 114, i8 101, i8 32, i8 102, i8 97, i8 99, i8 116, i8 111, i8 114, i8 121, i8 58, i8 10, i8 0], align 1
@.str.5 = constant [16 x i8] [i8 97, i8 100, i8 100, i8 95, i8 49, i8 48, i8 40, i8 53, i8 41, i8 32, i8 61, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.6 = constant [16 x i8] [i8 100, i8 111, i8 117, i8 98, i8 108, i8 101, i8 40, i8 55, i8 41, i8 32, i8 61, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i64 @add__i64(i64 %arg0, i64 %arg1) {
bb0:
  %alloca_0 = alloca i64, align 8
  %add_1 = add i64 %arg0, %arg1
  store i64 %add_1, ptr %alloca_0
  ret i64 %add_1
}

define void @apply__i64_op_add__i64(i64 %arg0, i64 %arg1) {
bb0:
  %call_3 = call i64 (i64, i64) @add__i64(i64 %arg0, i64 %arg1)
  br label %bb1
bb1:
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.0, i64 %call_3)
  br label %bb2
bb2:
  ret void
}

define double @add__f64(double %arg0, double %arg1) {
bb0:
  %alloca_5 = alloca double, align 8
  %fadd_6 = fadd double %arg0, double %arg1
  store double %fadd_6, ptr %alloca_5
  ret double %fadd_6
}

define void @apply__f64_op_add__f64(double %arg0, double %arg1) {
bb0:
  %call_8 = call double (double, double) @add__f64(double %arg0, double %arg1)
  br label %bb1
bb1:
  %call_9 = call i32 (ptr, ...) @printf(ptr @.str.1, double %call_8)
  br label %bb2
bb2:
  ret void
}

define i64 @apply_if__op_add__i64(i1 %arg0, i64 %arg1, i64 %arg2) {
bb0:
  %alloca_10 = alloca i64, align 8
  %cond_bool_1_2 = icmp ne i1 %arg0, 0
  br i1 %cond_bool_1_2, label %bb1, label %bb2
bb1:
  %call_11 = call i64 (i64, i64) @add__i64(i64 %arg1, i64 %arg2)
  br label %bb4
bb2:
  store i64 0, ptr %alloca_10
  br label %bb3
bb4:
  br label %bb3
bb3:
  ret i64 0
}

define i64 @__closure0_call({ i64 } %arg0, i64 %arg1) {
bb0:
  %alloca_13 = alloca i64, align 8
  %alloca_14 = alloca { i64 }, align 8
  store { i64 } %arg0, ptr %alloca_14
  %bitcast_16 = bitcast ptr %alloca_14 to ptr
  %load_17 = load i64, ptr %bitcast_16
  %add_18 = add i64 %arg1, %load_17
  store i64 %add_18, ptr %alloca_13
  ret i64 %add_18
}

define i64 @__closure1_call(void %arg0, i64 %arg1) {
bb0:
  %alloca_20 = alloca i64, align 8
  %mul_21 = mul i64 %arg1, 2
  store i64 %mul_21, ptr %alloca_20
  ret i64 %mul_21
}

define void @make_adder(i64 %arg0) {
bb0:
  %insertvalue_23 = insertvalue void undef, i64 %arg0, 0
  ret void
}

define i32 @main() {
bb0:
  %call_24 = call i32 (ptr, ...) @printf(ptr @.str.2)
  br label %bb1
bb1:
  call void (i64, i64) @apply__i64_op_add__i64(i64 10, i64 20)
  br label %bb2
bb2:
  call void (double, double) @apply__f64_op_add__f64(double 1.500000e0, double 2.500000e0)
  br label %bb3
bb3:
  %call_27 = call i32 (ptr, ...) @printf(ptr @.str.3)
  br label %bb4
bb4:
  %call_28 = call i64 (i1, i64, i64) @apply_if__op_add__i64(i1 1, i64 5, i64 3)
  br label %bb5
bb5:
  %call_29 = call i32 (ptr, ...) @printf(ptr @.str.0, i64 %call_28)
  br label %bb6
bb6:
  %call_30 = call i64 (i1, i64, i64) @apply_if__op_add__i64(i1 0, i64 5, i64 3)
  br label %bb7
bb7:
  %call_31 = call i32 (ptr, ...) @printf(ptr @.str.0, i64 %call_30)
  br label %bb8
bb8:
  %call_32 = call i32 (ptr, ...) @printf(ptr @.str.4)
  br label %bb9
bb9:
  call void (i64) @make_adder(i64 10)
  br label %bb10
bb10:
  %bitcast_34 = bitcast void undef to { i64 }
  %call_35 = call i64 ({ i64 }, i64) @__closure0_call({ i64 } %bitcast_34, i64 5)
  br label %bb11
bb11:
  %call_36 = call i32 (ptr, ...) @printf(ptr @.str.5, i64 %call_35)
  br label %bb12
bb12:
  %call_37 = call i64 (void, i64) @__closure1_call({  } {  }, i64 7)
  br label %bb13
bb13:
  %call_38 = call i32 (ptr, ...) @printf(ptr @.str.6, i64 %call_37)
  br label %bb14
bb14:
  ret i32 0
}

