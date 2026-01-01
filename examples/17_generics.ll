; ModuleID = '17_generics'
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.17_generics.0 = constant [6 x i8] c"hello\00", align 1
@.str.17_generics.1 = constant [10 x i8] [i8 40, i8 37, i8 115, i8 44, i8 32, i8 37, i8 115, i8 41, i8 10, i8 0], align 1
@.str.17_generics.2 = constant [20 x i8] [i8 109, i8 97, i8 120, i8 40, i8 49, i8 48, i8 44, i8 32, i8 50, i8 48, i8 41, i8 32, i8 61, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.17_generics.3 = constant [20 x i8] [i8 109, i8 97, i8 120, i8 40, i8 51, i8 46, i8 53, i8 44, i8 32, i8 50, i8 46, i8 49, i8 41, i8 32, i8 61, i8 32, i8 37, i8 102, i8 10, i8 0], align 1
@.str.17_generics.4 = constant [4 x i8] [i8 37, i8 115, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define ptr @Option__unwrap_or({ i64, ptr } %arg0, ptr %arg1) {
bb0:
  %alloca_7 = alloca ptr, align 8
  %alloca_8 = alloca ptr, align 8
  %alloca_9 = alloca { i64, ptr }, align 8
  store { i64, ptr } %arg0, ptr %alloca_9
  %alloca_11 = alloca i1, align 1
  %bitcast_12 = bitcast ptr %alloca_9 to ptr
  %load_13 = load i64, ptr %bitcast_12
  %icmp_14 = icmp eq i64 %load_13, 0
  store i1 %icmp_14, ptr %alloca_11
  %load_16 = load i1, ptr %alloca_11
  br i1 %load_16, label %bb2, label %bb3
bb2:
  %alloca_17 = alloca ptr, align 8
  %bitcast_18 = bitcast ptr %alloca_9 to ptr
  %gep_19 = getelementptr inbounds i8, ptr %bitcast_18, i64 8
  %bitcast_20 = bitcast ptr %gep_19 to ptr
  %load_21 = load ptr, ptr %bitcast_20
  store ptr %load_21, ptr %alloca_17
  %load_23 = load ptr, ptr %alloca_17
  store ptr %load_23, ptr %alloca_7
  br label %bb1
bb3:
  %alloca_25 = alloca i1, align 1
  %bitcast_26 = bitcast ptr %alloca_9 to ptr
  %load_27 = load i64, ptr %bitcast_26
  %icmp_28 = icmp eq i64 %load_27, 1
  store i1 %icmp_28, ptr %alloca_25
  %load_30 = load i1, ptr %alloca_25
  br i1 %load_30, label %bb4, label %bb5
bb1:
  %alloca_31 = alloca { i64, ptr }, align 8
  store { i64, ptr } %arg0, ptr %alloca_31
  %alloca_33 = alloca i1, align 1
  %bitcast_34 = bitcast ptr %alloca_31 to ptr
  %load_35 = load i64, ptr %bitcast_34
  %icmp_36 = icmp eq i64 %load_35, 0
  store i1 %icmp_36, ptr %alloca_33
  %load_38 = load i1, ptr %alloca_33
  br i1 %load_38, label %bb7, label %bb8
bb4:
  store ptr %arg1, ptr %alloca_7
  br label %bb1
bb5:
  store {  } {  }, ptr %alloca_7
  br label %bb1
bb7:
  %alloca_41 = alloca ptr, align 8
  %bitcast_42 = bitcast ptr %alloca_31 to ptr
  %gep_43 = getelementptr inbounds i8, ptr %bitcast_42, i64 8
  %bitcast_44 = bitcast ptr %gep_43 to ptr
  %load_45 = load ptr, ptr %bitcast_44
  store ptr %load_45, ptr %alloca_41
  %load_47 = load ptr, ptr %alloca_41
  store ptr %load_47, ptr %alloca_8
  br label %bb6
bb8:
  %alloca_49 = alloca i1, align 1
  %bitcast_50 = bitcast ptr %alloca_31 to ptr
  %load_51 = load i64, ptr %bitcast_50
  %icmp_52 = icmp eq i64 %load_51, 1
  store i1 %icmp_52, ptr %alloca_49
  %load_54 = load i1, ptr %alloca_49
  br i1 %load_54, label %bb9, label %bb10
bb6:
  %load_55 = load ptr, ptr %alloca_8
  ret ptr %load_55
bb9:
  store ptr %arg1, ptr %alloca_8
  br label %bb6
bb10:
  store {  } {  }, ptr %alloca_8
  br label %bb6
}

define { ptr, ptr } @Pair__new(ptr %arg0, ptr %arg1) {
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

define i32 @main() {
bb0:
  %inttoptr_58 = inttoptr i64 42 to ptr
  %call_59 = call { ptr, ptr } (ptr, ptr) @Pair__new(ptr %inttoptr_58, ptr @.str.17_generics.0)
  br label %bb1
bb1:
  %alloca_60 = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %call_59, ptr %alloca_60
  %bitcast_62 = bitcast ptr %alloca_60 to ptr
  %load_63 = load ptr, ptr %bitcast_62
  %alloca_64 = alloca { ptr, ptr }, align 8
  store { ptr, ptr } %call_59, ptr %alloca_64
  %bitcast_66 = bitcast ptr %alloca_64 to ptr
  %gep_67 = getelementptr inbounds i8, ptr %bitcast_66, i64 8
  %bitcast_68 = bitcast ptr %gep_67 to ptr
  %load_69 = load ptr, ptr %bitcast_68
  %call_70 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.1, ptr %load_63, ptr %load_69)
  br label %bb2
bb2:
  %call_71 = call ptr (double, double) @max(double 1.000000e1, double 2.000000e1)
  br label %bb3
bb3:
  %call_72 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.2, ptr %call_71)
  br label %bb4
bb4:
  %call_73 = call ptr (double, double) @max(double 3.500000e0, double 2.100000e0)
  br label %bb5
bb5:
  %call_74 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.3, ptr %call_73)
  br label %bb6
bb6:
  %alloca_75 = alloca { i64, i64 }, align 8
  store { i64, i64 } { i64 0, i64 100 }, ptr %alloca_75
  %alloca_77 = alloca { i64, i64 }, align 8
  store { i64, i64 } { i64 1, i64 0 }, ptr %alloca_77
  %alloca_79 = alloca { i64, i64 }, align 8
  %load_80 = load { i64, i64 }, ptr %alloca_77
  store { i64, i64 } %load_80, ptr %alloca_79
  %load_82 = load { i64, i64 }, ptr %alloca_75
  %bitcast_83 = bitcast { i64, i64 } %load_82 to { i64, ptr }
  %call_84 = call ptr ({ i64, ptr }, ptr) @Option__unwrap_or({ i64, ptr } %bitcast_83, ptr null)
  br label %bb7
bb7:
  %call_85 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.4, ptr %call_84)
  br label %bb8
bb8:
  %load_86 = load { i64, i64 }, ptr %alloca_79
  %bitcast_87 = bitcast { i64, i64 } %load_86 to { i64, ptr }
  %inttoptr_88 = inttoptr i64 99 to ptr
  %call_89 = call ptr ({ i64, ptr }, ptr) @Option__unwrap_or({ i64, ptr } %bitcast_87, ptr %inttoptr_88)
  br label %bb9
bb9:
  %call_90 = call i32 (ptr, ...) @printf(ptr @.str.17_generics.4, ptr %call_89)
  br label %bb10
bb10:
  ret i32 0
}

define ptr @max(double %arg0, double %arg1) {
bb0:
  %alloca_91 = alloca ptr, align 8
  store i64 0, ptr %alloca_91
  %load_93 = load ptr, ptr %alloca_91
  ret ptr %load_93
}

