; ModuleID = '15_enums'
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.15_enums.0 = constant [6 x i8] c"point\00", align 1
@.str.15_enums.1 = constant [7 x i8] c"circle\00", align 1
@.str.15_enums.2 = constant [10 x i8] c"rectangle\00", align 1
@.str.15_enums.3 = constant [4 x i8] [i8 37, i8 115, i8 10, i8 0], align 1
@.str.15_enums.4 = constant [18 x i8] [i8 100, i8 105, i8 115, i8 99, i8 114, i8 105, i8 109, i8 105, i8 110, i8 97, i8 110, i8 116, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
@.str.15_enums.5 = constant [11 x i8] [i8 99, i8 111, i8 110, i8 115, i8 116, i8 58, i8 32, i8 37, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i64 @Option__Some(i64 %arg0) {
bb0:
  %alloca_125 = alloca i64, align 8
  store i64 0, ptr %alloca_125
  %load_127 = load i64, ptr %alloca_125
  ret i64 %load_127
}

define ptr @Shape__Circle(i64 %arg0) {
bb0:
  %alloca_128 = alloca ptr, align 8
  store i64 0, ptr %alloca_128
  %load_130 = load ptr, ptr %alloca_128
  ret ptr %load_130
}

define ptr @describe(ptr %arg0) {
bb0:
  %alloca_0 = alloca ptr, align 8
  %alloca_1 = alloca ptr, align 8
  %alloca_2 = alloca ptr, align 8
  store ptr %arg0, ptr %alloca_2
  %alloca_4 = alloca i1, align 1
  %load_5 = load ptr, ptr %alloca_2
  %icmp_6 = icmp eq ptr %load_5, 0
  store i1 %icmp_6, ptr %alloca_4
  %load_8 = load i1, ptr %alloca_4
  br i1 %load_8, label %bb2, label %bb3
bb2:
  store ptr @.str.15_enums.0, ptr %alloca_0
  br label %bb1
bb3:
  %alloca_10 = alloca i1, align 1
  %load_11 = load ptr, ptr %alloca_2
  %icmp_12 = icmp eq ptr %load_11, 1
  store i1 %icmp_12, ptr %alloca_10
  %load_14 = load i1, ptr %alloca_10
  br i1 %load_14, label %bb4, label %bb5
bb1:
  %alloca_15 = alloca ptr, align 8
  store ptr %arg0, ptr %alloca_15
  %alloca_17 = alloca i1, align 1
  %load_18 = load ptr, ptr %alloca_15
  %icmp_19 = icmp eq ptr %load_18, 0
  store i1 %icmp_19, ptr %alloca_17
  %load_21 = load i1, ptr %alloca_17
  br i1 %load_21, label %bb9, label %bb10
bb4:
  store ptr @.str.15_enums.1, ptr %alloca_0
  br label %bb1
bb5:
  %alloca_23 = alloca i1, align 1
  %load_24 = load ptr, ptr %alloca_2
  %icmp_25 = icmp eq ptr %load_24, 2
  store i1 %icmp_25, ptr %alloca_23
  %load_27 = load i1, ptr %alloca_23
  br i1 %load_27, label %bb6, label %bb7
bb9:
  store ptr @.str.15_enums.0, ptr %alloca_1
  br label %bb8
bb10:
  %alloca_29 = alloca i1, align 1
  %load_30 = load ptr, ptr %alloca_15
  %icmp_31 = icmp eq ptr %load_30, 1
  store i1 %icmp_31, ptr %alloca_29
  %load_33 = load i1, ptr %alloca_29
  br i1 %load_33, label %bb11, label %bb12
bb6:
  store ptr @.str.15_enums.2, ptr %alloca_0
  br label %bb1
bb7:
  store {  } {  }, ptr %alloca_0
  br label %bb1
bb8:
  %load_36 = load ptr, ptr %alloca_1
  ret ptr %load_36
bb11:
  store ptr @.str.15_enums.1, ptr %alloca_1
  br label %bb8
bb12:
  %alloca_38 = alloca i1, align 1
  %load_39 = load ptr, ptr %alloca_15
  %icmp_40 = icmp eq ptr %load_39, 2
  store i1 %icmp_40, ptr %alloca_38
  %load_42 = load i1, ptr %alloca_38
  br i1 %load_42, label %bb13, label %bb14
bb13:
  store ptr @.str.15_enums.2, ptr %alloca_1
  br label %bb8
bb14:
  store {  } {  }, ptr %alloca_1
  store i1 0, ptr %alloca_1
  br label %bb8
}

define i32 @main() {
bb0:
  %alloca_87 = alloca i64, align 8
  store i64 0, ptr %alloca_87
  %call_89 = call ptr (i64) @Shape__Circle(i64 10)
  br label %bb1
bb1:
  %alloca_90 = alloca { i64, i64 }, align 8
  store { i64, i64 } { i64 5, i64 3 }, ptr %alloca_90
  %alloca_92 = alloca ptr, align 8
  store ptr %alloca_87, ptr %alloca_92
  %call_94 = call ptr (ptr) @describe(ptr %alloca_92)
  br label %bb2
bb2:
  %call_95 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.3, ptr %call_94)
  br label %bb3
bb3:
  %alloca_96 = alloca ptr, align 8
  store ptr %call_89, ptr %alloca_96
  %call_98 = call ptr (ptr) @describe(ptr %alloca_96)
  br label %bb4
bb4:
  %call_99 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.3, ptr %call_98)
  br label %bb5
bb5:
  %alloca_100 = alloca ptr, align 8
  store ptr %alloca_90, ptr %alloca_100
  %call_102 = call ptr (ptr) @describe(ptr %alloca_100)
  br label %bb6
bb6:
  %call_103 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.3, ptr %call_102)
  br label %bb7
bb7:
  %alloca_104 = alloca i64, align 8
  store i64 5, ptr %alloca_104
  %alloca_106 = alloca i32, align 4
  %load_107 = load i64, ptr %alloca_104
  %sext_trunc_108 = trunc i64 %load_107 to i32
  store i32 %sext_trunc_108, ptr %alloca_106
  %load_110 = load i32, ptr %alloca_106
  %call_111 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.4, i32 %load_110)
  br label %bb8
bb8:
  %call_112 = call i64 (i64) @Option__Some(i64 42)
  br label %bb9
bb9:
  %alloca_113 = alloca i64, align 8
  store i64 1, ptr %alloca_113
  %call_115 = call ptr (i64, ptr) @unwrap_or(i64 %call_112, ptr null)
  br label %bb10
bb10:
  %call_116 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.3, ptr %call_115)
  br label %bb11
bb11:
  %load_117 = load i64, ptr %alloca_113
  %bitcast_118 = bitcast i64 99 to ptr
  %call_119 = call ptr (i64, ptr) @unwrap_or(i64 %load_117, ptr %bitcast_118)
  br label %bb12
bb12:
  %call_120 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.3, ptr %call_119)
  br label %bb13
bb13:
  %alloca_121 = alloca i32, align 4
  store i64 2, ptr %alloca_121
  %load_123 = load i32, ptr %alloca_121
  %call_124 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.5, i32 %load_123)
  br label %bb14
bb14:
  ret i32 0
}

define ptr @unwrap_or(i64 %arg0, ptr %arg1) {
bb0:
  %alloca_46 = alloca ptr, align 8
  %alloca_47 = alloca ptr, align 8
  %alloca_48 = alloca i64, align 8
  store i64 %arg0, ptr %alloca_48
  %alloca_50 = alloca i1, align 1
  %load_51 = load i64, ptr %alloca_48
  %icmp_52 = icmp eq i64 %load_51, 0
  store i1 %icmp_52, ptr %alloca_50
  %load_54 = load i1, ptr %alloca_50
  br i1 %load_54, label %bb2, label %bb3
bb2:
  %alloca_55 = alloca i64, align 8
  %load_56 = load i64, ptr %alloca_48
  store i64 %load_56, ptr %alloca_55
  %load_58 = load i64, ptr %alloca_55
  store i64 %load_58, ptr %alloca_46
  br label %bb1
bb3:
  %alloca_60 = alloca i1, align 1
  %load_61 = load i64, ptr %alloca_48
  %icmp_62 = icmp eq i64 %load_61, 1
  store i1 %icmp_62, ptr %alloca_60
  %load_64 = load i1, ptr %alloca_60
  br i1 %load_64, label %bb4, label %bb5
bb1:
  %alloca_65 = alloca i64, align 8
  store i64 %arg0, ptr %alloca_65
  %alloca_67 = alloca i1, align 1
  %load_68 = load i64, ptr %alloca_65
  %icmp_69 = icmp eq i64 %load_68, 0
  store i1 %icmp_69, ptr %alloca_67
  %load_71 = load i1, ptr %alloca_67
  br i1 %load_71, label %bb7, label %bb8
bb4:
  store ptr %arg1, ptr %alloca_46
  br label %bb1
bb5:
  store {  } {  }, ptr %alloca_46
  br label %bb1
bb7:
  %alloca_74 = alloca i64, align 8
  %load_75 = load i64, ptr %alloca_65
  store i64 %load_75, ptr %alloca_74
  %load_77 = load i64, ptr %alloca_74
  store i64 %load_77, ptr %alloca_47
  br label %bb6
bb8:
  %alloca_79 = alloca i1, align 1
  %load_80 = load i64, ptr %alloca_65
  %icmp_81 = icmp eq i64 %load_80, 1
  store i1 %icmp_81, ptr %alloca_79
  %load_83 = load i1, ptr %alloca_79
  br i1 %load_83, label %bb9, label %bb10
bb6:
  %load_84 = load ptr, ptr %alloca_47
  ret ptr %load_84
bb9:
  store ptr %arg1, ptr %alloca_47
  br label %bb6
bb10:
  store {  } {  }, ptr %alloca_47
  br label %bb6
}

