; ModuleID = '17_generics'
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.17_generics.0 = constant [6 x i8] c"hello\00", align 1
@.str.17_generics.1 = constant [10 x i8] [i8 40, i8 37, i8 115, i8 44, i8 32, i8 37, i8 115, i8 41, i8 10, i8 0], align 1
@.str.17_generics.2 = constant [20 x i8] [i8 109, i8 97, i8 120, i8 40, i8 49, i8 48, i8 44, i8 32, i8 50, i8 48, i8 41, i8 32, i8 61, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.17_generics.3 = constant [20 x i8] [i8 109, i8 97, i8 120, i8 40, i8 51, i8 46, i8 53, i8 44, i8 32, i8 50, i8 46, i8 49, i8 41, i8 32, i8 61, i8 32, i8 37, i8 102, i8 10, i8 0], align 1
@.str.17_generics.4 = constant [4 x i8] [i8 37, i8 115, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i64 @Option__Some(i64 %arg0) {
bb0:
  %alloca_76 = alloca i64, align 8
  store i64 0, ptr %alloca_76
  %load_78 = load i64, ptr %alloca_76
  ret i64 %load_78
}

define i32 @main() {
bb0:
  %bitcast_48 = bitcast i64 42 to ptr
  %call_49 = call { ptr, ptr } (ptr, ptr) @new(ptr %bitcast_48, ptr @.str.17_generics.0)
  br label %bb1
bb1:
  %alloca_50 = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %call_49, ptr %alloca_50
  %bitcast_52 = bitcast ptr %alloca_50 to ptr
  %load_53 = load ptr, ptr %bitcast_52
  %alloca_54 = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %call_49, ptr %alloca_54
  %bitcast_56 = bitcast ptr %alloca_54 to ptr
  %gep_57 = getelementptr inbounds i8, ptr %bitcast_56, i64 8
  %bitcast_58 = bitcast ptr %gep_57 to ptr
  %load_59 = load ptr, ptr %bitcast_58
  %call_60 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.1, ptr %load_53, ptr %load_59)
  br label %bb2
bb2:
  %bitcast_61 = bitcast i64 10 to double
  %bitcast_62 = bitcast i64 20 to double
  %call_63 = call ptr (double, double) @max(double %bitcast_61, double %bitcast_62)
  br label %bb3
bb3:
  %call_64 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.2, ptr %call_63)
  br label %bb4
bb4:
  %call_65 = call ptr (double, double) @max(double 3.500000e0, double 2.100000e0)
  br label %bb5
bb5:
  %call_66 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.3, ptr %call_65)
  br label %bb6
bb6:
  %call_67 = call i64 (i64) @Option__Some(i64 100)
  br label %bb7
bb7:
  %alloca_68 = alloca i64, align 8
  store i64 1, ptr %alloca_68
  %call_70 = call ptr (i64, ptr) @unwrap_or(i64 %call_67, ptr null)
  br label %bb8
bb8:
  %call_71 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.4, ptr %call_70)
  br label %bb9
bb9:
  %load_72 = load i64, ptr %alloca_68
  %bitcast_73 = bitcast i64 99 to ptr
  %call_74 = call ptr (i64, ptr) @unwrap_or(i64 %load_72, ptr %bitcast_73)
  br label %bb10
bb10:
  %call_75 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.4, ptr %call_74)
  br label %bb11
bb11:
  ret i32 0
}

define ptr @max(double %arg0, double %arg1) {
bb0:
  %alloca_79 = alloca ptr, align 8
  store i64 0, ptr %alloca_79
  %load_81 = load ptr, ptr %alloca_79
  ret ptr %load_81
}

define { ptr, ptr } @new(ptr %arg0, ptr %arg1) {
bb0:
  %alloca_0 = alloca { ptr, ptr }, align 8
  %insertvalue_1 = insertvalue { ptr, ptr } undef, ptr %arg0, 0
  %insertvalue_2 = insertvalue { ptr, ptr } %insertvalue_1, ptr %arg1, 1
  %insertvalue_3 = insertvalue { ptr, ptr } undef, ptr %arg0, 0
  %insertvalue_4 = insertvalue { ptr, ptr } %insertvalue_3, ptr %arg1, 1
  store { ptr, ptr } %insertvalue_4, ptr %alloca_0
  %load_6 = load { ptr, ptr }, ptr %alloca_0
  ret { ptr, ptr } %load_6
}

define ptr @unwrap_or(i64 %arg0, ptr %arg1) {
bb0:
  %alloca_7 = alloca ptr, align 8
  %alloca_8 = alloca ptr, align 8
  %alloca_9 = alloca i64, align 8
  store i64 %arg0, ptr %alloca_9
  %alloca_11 = alloca i1, align 1
  %load_12 = load i64, ptr %alloca_9
  %icmp_13 = icmp eq i64 %load_12, 0
  store i1 %icmp_13, ptr %alloca_11
  %load_15 = load i1, ptr %alloca_11
  br i1 %load_15, label %bb2, label %bb3
bb2:
  %alloca_16 = alloca i64, align 8
  %load_17 = load i64, ptr %alloca_9
  store i64 %load_17, ptr %alloca_16
  %load_19 = load i64, ptr %alloca_16
  store i64 %load_19, ptr %alloca_8
  br label %bb1
bb3:
  %alloca_21 = alloca i1, align 1
  %load_22 = load i64, ptr %alloca_9
  %icmp_23 = icmp eq i64 %load_22, 1
  store i1 %icmp_23, ptr %alloca_21
  %load_25 = load i1, ptr %alloca_21
  br i1 %load_25, label %bb4, label %bb5
bb1:
  %alloca_26 = alloca i64, align 8
  store i64 %arg0, ptr %alloca_26
  %alloca_28 = alloca i1, align 1
  %load_29 = load i64, ptr %alloca_26
  %icmp_30 = icmp eq i64 %load_29, 0
  store i1 %icmp_30, ptr %alloca_28
  %load_32 = load i1, ptr %alloca_28
  br i1 %load_32, label %bb7, label %bb8
bb4:
  store ptr %arg1, ptr %alloca_8
  br label %bb1
bb5:
  store {  } {  }, ptr %alloca_8
  br label %bb1
bb7:
  %alloca_35 = alloca i64, align 8
  %load_36 = load i64, ptr %alloca_26
  store i64 %load_36, ptr %alloca_35
  %load_38 = load i64, ptr %alloca_35
  store i64 %load_38, ptr %alloca_7
  br label %bb6
bb8:
  %alloca_40 = alloca i1, align 1
  %load_41 = load i64, ptr %alloca_26
  %icmp_42 = icmp eq i64 %load_41, 1
  store i1 %icmp_42, ptr %alloca_40
  %load_44 = load i1, ptr %alloca_40
  br i1 %load_44, label %bb9, label %bb10
bb6:
  %load_45 = load ptr, ptr %alloca_7
  ret ptr %load_45
bb9:
  store ptr %arg1, ptr %alloca_7
  br label %bb6
bb10:
  store {  } {  }, ptr %alloca_7
  br label %bb6
}

