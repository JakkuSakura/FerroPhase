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
define void @__closure0_call({ i64 } %arg0, void %arg1) {
bb0:
  %alloca_0 = alloca { i64 }, align 8
  store { i64 } %arg0, ptr %alloca_0
  %bitcast_2 = bitcast ptr %alloca_0 to ptr
  %load_3 = load i64, ptr %bitcast_2
  %add_4 = add void %arg1, %load_3
  ret void
}

define void @__closure1_call(void %arg0, void %arg1) {
bb0:
  %mul_5 = mul void %arg1, 2
  ret void
}

define i64 @add__i64(i64 %arg0, i64 %arg1) {
bb0:
  %alloca_6 = alloca i64, align 8
  %add_7 = add i64 %arg0, %arg1
  store i64 %add_7, ptr %alloca_6
  ret i64 %add_7
}

define void @apply__i64_op_add__i64(i64 %arg0, i64 %arg1) {
bb0:
  %call_9 = call i64 (i64, i64) @add__i64(i64 %arg0, i64 %arg1)
  br label %bb1
bb1:
  %call_10 = call i32 (ptr, ...) @printf(ptr @.str.0, i64 %call_9)
  br label %bb2
bb2:
  ret void
}

define double @add__f64(double %arg0, double %arg1) {
bb0:
  %alloca_11 = alloca double, align 8
  %fadd_12 = fadd double %arg0, double %arg1
  store double %fadd_12, ptr %alloca_11
  ret double %fadd_12
}

define void @apply__f64_op_add__f64(double %arg0, double %arg1) {
bb0:
  %call_14 = call double (double, double) @add__f64(double %arg0, double %arg1)
  br label %bb1
bb1:
  %call_15 = call i32 (ptr, ...) @printf(ptr @.str.1, double %call_14)
  br label %bb2
bb2:
  ret void
}

define i64 @apply_if__op_add__i64(i1 %arg0, i64 %arg1, i64 %arg2) {
bb0:
  %alloca_16 = alloca i64, align 8
  %cond_bool_1_2 = icmp ne i1 %arg0, 0
  br i1 %cond_bool_1_2, label %bb1, label %bb2
bb1:
  %call_17 = call i64 (i64, i64) @add__i64(i64 %arg1, i64 %arg2)
  br label %bb4
bb2:
  store i64 0, ptr %alloca_16
  br label %bb3
bb4:
  br label %bb3
bb3:
  ret i64 0
}

define void @make_adder(i64 %arg0) {
bb0:
  %insertvalue_19 = insertvalue void undef, i64 %arg0, 0
  ret void
}

define i32 @main() {
bb0:
  %call_20 = call i32 (ptr, ...) @printf(ptr @.str.2)
  br label %bb1
bb1:
  call void (i64, i64) @apply__i64_op_add__i64(i64 10, i64 20)
  br label %bb2
bb2:
  call void (double, double) @apply__f64_op_add__f64(double 1.500000e0, double 2.500000e0)
  br label %bb3
bb3:
  %call_23 = call i32 (ptr, ...) @printf(ptr @.str.3)
  br label %bb4
bb4:
  %call_24 = call i64 (i1, i64, i64) @apply_if__op_add__i64(i1 1, i64 5, i64 3)
  br label %bb5
bb5:
  %call_25 = call i32 (ptr, ...) @printf(ptr @.str.0, i64 %call_24)
  br label %bb6
bb6:
  %call_26 = call i64 (i1, i64, i64) @apply_if__op_add__i64(i1 0, i64 5, i64 3)
  br label %bb7
bb7:
  %call_27 = call i32 (ptr, ...) @printf(ptr @.str.0, i64 %call_26)
  br label %bb8
bb8:
  %call_28 = call i32 (ptr, ...) @printf(ptr @.str.4)
  br label %bb9
bb9:
  call void (i64) @make_adder(i64 10)
  br label %bb10
bb10:
  %bitcast_30 = bitcast void undef to { i64 }
  %bitcast_31 = bitcast i64 5 to void
  call void ({ i64 }, void) @__closure0_call({ i64 } %bitcast_30, void %bitcast_31)
  br label %bb11
bb11:
  %call_33 = call i32 (ptr, ...) @printf(ptr @.str.5, void undef)
  br label %bb12
bb12:
  %bitcast_34 = bitcast i64 7 to void
  call void (void, void) @__closure1_call({  } {  }, void %bitcast_34)
  br label %bb13
bb13:
  %call_36 = call i32 (ptr, ...) @printf(ptr @.str.6, void undef)
  br label %bb14
bb14:
  ret i32 0
}

