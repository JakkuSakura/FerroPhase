; ModuleID = '15_enums'
source_filename = "15_enums"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.15_enums.0 = private unnamed_addr constant [6 x i8] c"point\00", align 1
@.str.15_enums.1 = private unnamed_addr constant [7 x i8] c"circle\00", align 1
@.str.15_enums.2 = private unnamed_addr constant [10 x i8] c"rectangle\00", align 1
@.str.15_enums.3 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str.15_enums.4 = private unnamed_addr constant [18 x i8] c"discriminant: %d\0A\00", align 1
@.str.15_enums.5 = private unnamed_addr constant [6 x i8] c"%lld\0A\00", align 1
@.str.15_enums.6 = private unnamed_addr constant [11 x i8] c"const: %d\0A\00", align 1

define internal ptr @Shape__describe(ptr %0) {
bb0:
  %alloca_20 = alloca i1, align 1
  %alloca_count_20 = alloca i1, align 1
  %alloca_11 = alloca i1, align 1
  %alloca_count_11 = alloca i1, align 1
  %alloca_3 = alloca i1, align 1
  %alloca_count_3 = alloca i1, align 1
  %alloca_1 = alloca ptr, align 8
  %alloca_count_1 = alloca ptr, align 8
  %alloca_0 = alloca ptr, align 8
  %alloca_count_0 = alloca ptr, align 8
  store ptr %0, ptr %alloca_count_1, align 8
  %load_4 = load ptr, ptr %alloca_count_1, align 8
  %load_6 = load i64, ptr %load_4, align 8
  %icmp_7 = icmp eq i64 %load_6, 0
  store i1 %icmp_7, ptr %alloca_count_3, align 1
  %load_9 = load i1, ptr %alloca_count_3, align 1
  br i1 %load_9, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  store ptr @.str.15_enums.0, ptr %alloca_count_0, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  %load_12 = load ptr, ptr %alloca_count_1, align 8
  %load_14 = load i64, ptr %load_12, align 8
  %icmp_15 = icmp eq i64 %load_14, 1
  store i1 %icmp_15, ptr %alloca_count_11, align 1
  %load_17 = load i1, ptr %alloca_count_11, align 1
  br i1 %load_17, label %bb4, label %bb5

bb1:                                              ; preds = %bb7, %bb6, %bb4, %bb2
  %load_18 = load ptr, ptr %alloca_count_0, align 8
  ret ptr %load_18

bb4:                                              ; preds = %bb3
  store ptr @.str.15_enums.1, ptr %alloca_count_0, align 8
  br label %bb1

bb5:                                              ; preds = %bb3
  %load_21 = load ptr, ptr %alloca_count_1, align 8
  %load_23 = load i64, ptr %load_21, align 8
  %icmp_24 = icmp eq i64 %load_23, 2
  store i1 %icmp_24, ptr %alloca_count_20, align 1
  %load_26 = load i1, ptr %alloca_count_20, align 1
  br i1 %load_26, label %bb6, label %bb7

bb6:                                              ; preds = %bb5
  store ptr @.str.15_enums.2, ptr %alloca_count_0, align 8
  br label %bb1

bb7:                                              ; preds = %bb5
  br label %bb1
}

define i32 @main() {
bb0:
  %alloca_76 = alloca i32, align 4
  %alloca_count_76 = alloca i32, align 4
  %alloca_67 = alloca { i64, i64 }, align 8
  %alloca_count_67 = alloca { i64, i64 }, align 8
  %alloca_65 = alloca { i64, i64 }, align 8
  %alloca_count_65 = alloca { i64, i64 }, align 8
  %alloca_63 = alloca { i64, i64 }, align 8
  %alloca_count_63 = alloca { i64, i64 }, align 8
  %alloca_57 = alloca i32, align 4
  %alloca_count_57 = alloca i32, align 4
  %alloca_54 = alloca i64, align 8
  %alloca_count_54 = alloca i64, align 8
  %alloca_52 = alloca i64, align 8
  %alloca_count_52 = alloca i64, align 8
  %alloca_47 = alloca ptr, align 8
  %alloca_count_47 = alloca ptr, align 8
  %alloca_42 = alloca ptr, align 8
  %alloca_count_42 = alloca ptr, align 8
  %alloca_37 = alloca ptr, align 8
  %alloca_count_37 = alloca ptr, align 8
  %alloca_35 = alloca { i64, i64, i64 }, align 8
  %alloca_count_35 = alloca { i64, i64, i64 }, align 8
  %alloca_33 = alloca { i64, i64, i64 }, align 8
  %alloca_count_33 = alloca { i64, i64, i64 }, align 8
  %alloca_30 = alloca { i64, i64, i64 }, align 8
  %alloca_count_30 = alloca { i64, i64, i64 }, align 8
  %alloca_28 = alloca { i64, i64, i64 }, align 8
  %alloca_count_28 = alloca { i64, i64, i64 }, align 8
  store { i64, i64, i64 } zeroinitializer, ptr %alloca_count_28, align 8
  %load_31 = load { i64, i64, i64 }, ptr %alloca_count_28, align 8
  store { i64, i64, i64 } %load_31, ptr %alloca_count_30, align 8
  store { i64, i64, i64 } { i64 1, i64 10, i64 0 }, ptr %alloca_count_33, align 8
  store { i64, i64, i64 } { i64 2, i64 5, i64 3 }, ptr %alloca_count_35, align 8
  store ptr %alloca_count_30, ptr %alloca_count_37, align 8
  %load_39 = load ptr, ptr %alloca_count_37, align 8
  %call_40 = call ptr @Shape__describe(ptr %load_39)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_41 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.3, ptr %call_40)
  br label %bb2

bb2:                                              ; preds = %bb1
  store ptr %alloca_count_33, ptr %alloca_count_42, align 8
  %load_44 = load ptr, ptr %alloca_count_42, align 8
  %call_45 = call ptr @Shape__describe(ptr %load_44)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_46 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.3, ptr %call_45)
  br label %bb4

bb4:                                              ; preds = %bb3
  store ptr %alloca_count_35, ptr %alloca_count_47, align 8
  %load_49 = load ptr, ptr %alloca_count_47, align 8
  %call_50 = call ptr @Shape__describe(ptr %load_49)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_51 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.3, ptr %call_50)
  br label %bb6

bb6:                                              ; preds = %bb5
  store i64 5, ptr %alloca_count_52, align 8
  %load_55 = load i64, ptr %alloca_count_52, align 8
  store i64 %load_55, ptr %alloca_count_54, align 8
  %load_58 = load i64, ptr %alloca_count_54, align 8
  %trunc = trunc i64 %load_58 to i32
  store i32 %trunc, ptr %alloca_count_57, align 4
  %load_61 = load i32, ptr %alloca_count_57, align 4
  %call_62 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.4, i32 %load_61)
  br label %bb7

bb7:                                              ; preds = %bb6
  store { i64, i64 } { i64 0, i64 42 }, ptr %alloca_count_63, align 8
  store { i64, i64 } { i64 1, i64 0 }, ptr %alloca_count_65, align 8
  %load_68 = load { i64, i64 }, ptr %alloca_count_65, align 8
  store { i64, i64 } %load_68, ptr %alloca_count_67, align 8
  %load_70 = load { i64, i64 }, ptr %alloca_count_63, align 8
  %call_71 = call i64 @Option__unwrap_or__mono_a7af9f593fdc4675({ i64, i64 } %load_70, i64 0)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_72 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.5, i64 %call_71)
  br label %bb9

bb9:                                              ; preds = %bb8
  %load_73 = load { i64, i64 }, ptr %alloca_count_67, align 8
  %call_74 = call i64 @Option__unwrap_or__mono_a7af9f593fdc4675({ i64, i64 } %load_73, i64 99)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_75 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.5, i64 %call_74)
  br label %bb11

bb11:                                             ; preds = %bb10
  store i64 2, ptr %alloca_count_76, align 4
  %load_78 = load i32, ptr %alloca_count_76, align 4
  %call_79 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.6, i32 %load_78)
  br label %bb12

bb12:                                             ; preds = %bb11
  ret i32 0
}

declare i32 @printf(ptr, ...)

define internal i64 @Option__unwrap_or__mono_a7af9f593fdc4675({ i64, i64 } %0, i64 %1) {
bb0:
  %alloca_97 = alloca i1, align 1
  %alloca_count_97 = alloca i1, align 1
  %alloca_89 = alloca i64, align 8
  %alloca_count_89 = alloca i64, align 8
  %alloca_83 = alloca i1, align 1
  %alloca_count_83 = alloca i1, align 1
  %alloca_81 = alloca { i64, i64 }, align 8
  %alloca_count_81 = alloca { i64, i64 }, align 8
  %alloca_80 = alloca i64, align 8
  %alloca_count_80 = alloca i64, align 8
  store { i64, i64 } %0, ptr %alloca_count_81, align 8
  %load_85 = load i64, ptr %alloca_count_81, align 8
  %icmp_86 = icmp eq i64 %load_85, 0
  store i1 %icmp_86, ptr %alloca_count_83, align 1
  %load_88 = load i1, ptr %alloca_count_83, align 1
  br i1 %load_88, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  %gep_91 = getelementptr inbounds i8, ptr %alloca_count_81, i64 8
  %load_93 = load i64, ptr %gep_91, align 8
  store i64 %load_93, ptr %alloca_count_89, align 8
  %load_95 = load i64, ptr %alloca_count_89, align 8
  store i64 %load_95, ptr %alloca_count_80, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  %load_99 = load i64, ptr %alloca_count_81, align 8
  %icmp_100 = icmp eq i64 %load_99, 1
  store i1 %icmp_100, ptr %alloca_count_97, align 1
  %load_102 = load i1, ptr %alloca_count_97, align 1
  br i1 %load_102, label %bb4, label %bb5

bb1:                                              ; preds = %bb5, %bb4, %bb2
  %load_103 = load i64, ptr %alloca_count_80, align 8
  ret i64 %load_103

bb4:                                              ; preds = %bb3
  store i64 %1, ptr %alloca_count_80, align 8
  br label %bb1

bb5:                                              ; preds = %bb3
  br label %bb1
}
