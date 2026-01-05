; ModuleID = '15_enums'
source_filename = "15_enums"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.15_enums.0 = constant [6 x i8] c"point\00"
@.str.15_enums.1 = constant [7 x i8] c"circle\00"
@.str.15_enums.2 = constant [10 x i8] c"rectangle\00"
@.str.15_enums.3 = constant [28 x i8] c"\F0\9F\93\98 Tutorial: 15_enums.fp\0A\00"
@.str.15_enums.4 = constant [75 x i8] c"\F0\9F\A7\AD Focus: Enum variants: unit, tuple, struct variants and discriminants\0A\00"
@.str.15_enums.5 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.15_enums.6 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.15_enums.7 = constant [2 x i8] c"\0A\00"
@.str.15_enums.8 = constant [19 x i8] c"shape point -> %s\0A\00"
@.str.15_enums.9 = constant [20 x i8] c"shape circle -> %s\0A\00"
@.str.15_enums.10 = constant [23 x i8] c"shape rectangle -> %s\0A\00"
@.str.15_enums.11 = constant [18 x i8] c"discriminant: %d\0A\00"
@.str.15_enums.12 = constant [31 x i8] c"unwrap_or(Some(42), 0) = %lld\0A\00"
@.str.15_enums.13 = constant [28 x i8] c"unwrap_or(None, 99) = %lld\0A\00"
@.str.15_enums.14 = constant [11 x i8] c"const: %d\0A\00"

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
  %alloca_81 = alloca i32, align 4
  %alloca_count_81 = alloca i32, align 4
  %alloca_72 = alloca { i64, i64 }, align 8
  %alloca_count_72 = alloca { i64, i64 }, align 8
  %alloca_70 = alloca { i64, i64 }, align 8
  %alloca_count_70 = alloca { i64, i64 }, align 8
  %alloca_68 = alloca { i64, i64 }, align 8
  %alloca_count_68 = alloca { i64, i64 }, align 8
  %alloca_62 = alloca i32, align 4
  %alloca_count_62 = alloca i32, align 4
  %alloca_59 = alloca i64, align 8
  %alloca_count_59 = alloca i64, align 8
  %alloca_57 = alloca i64, align 8
  %alloca_count_57 = alloca i64, align 8
  %alloca_52 = alloca ptr, align 8
  %alloca_count_52 = alloca ptr, align 8
  %alloca_47 = alloca ptr, align 8
  %alloca_count_47 = alloca ptr, align 8
  %alloca_42 = alloca ptr, align 8
  %alloca_count_42 = alloca ptr, align 8
  %alloca_40 = alloca { i64, i64, i64 }, align 8
  %alloca_count_40 = alloca { i64, i64, i64 }, align 8
  %alloca_38 = alloca { i64, i64, i64 }, align 8
  %alloca_count_38 = alloca { i64, i64, i64 }, align 8
  %alloca_35 = alloca { i64, i64, i64 }, align 8
  %alloca_count_35 = alloca { i64, i64, i64 }, align 8
  %alloca_33 = alloca { i64, i64, i64 }, align 8
  %alloca_count_33 = alloca { i64, i64, i64 }, align 8
  %call_28 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.3)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_29 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.4)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_30 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.5)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_31 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.6)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_32 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.7)
  br label %bb5

bb5:                                              ; preds = %bb4
  store { i64, i64, i64 } zeroinitializer, ptr %alloca_count_33, align 8
  %load_36 = load { i64, i64, i64 }, ptr %alloca_count_33, align 8
  store { i64, i64, i64 } %load_36, ptr %alloca_count_35, align 8
  store { i64, i64, i64 } { i64 1, i64 10, i64 0 }, ptr %alloca_count_38, align 8
  store { i64, i64, i64 } { i64 2, i64 5, i64 3 }, ptr %alloca_count_40, align 8
  store ptr %alloca_count_35, ptr %alloca_count_42, align 8
  %load_44 = load ptr, ptr %alloca_count_42, align 8
  %call_45 = call ptr @Shape__describe(ptr %load_44)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_46 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.8, ptr %call_45)
  br label %bb7

bb7:                                              ; preds = %bb6
  store ptr %alloca_count_38, ptr %alloca_count_47, align 8
  %load_49 = load ptr, ptr %alloca_count_47, align 8
  %call_50 = call ptr @Shape__describe(ptr %load_49)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_51 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.9, ptr %call_50)
  br label %bb9

bb9:                                              ; preds = %bb8
  store ptr %alloca_count_40, ptr %alloca_count_52, align 8
  %load_54 = load ptr, ptr %alloca_count_52, align 8
  %call_55 = call ptr @Shape__describe(ptr %load_54)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_56 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.10, ptr %call_55)
  br label %bb11

bb11:                                             ; preds = %bb10
  store i64 5, ptr %alloca_count_57, align 8
  %load_60 = load i64, ptr %alloca_count_57, align 8
  store i64 %load_60, ptr %alloca_count_59, align 8
  %load_63 = load i64, ptr %alloca_count_59, align 8
  %trunc = trunc i64 %load_63 to i32
  store i32 %trunc, ptr %alloca_count_62, align 4
  %load_66 = load i32, ptr %alloca_count_62, align 4
  %call_67 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.11, i32 %load_66)
  br label %bb12

bb12:                                             ; preds = %bb11
  store { i64, i64 } { i64 0, i64 42 }, ptr %alloca_count_68, align 8
  store { i64, i64 } { i64 1, i64 0 }, ptr %alloca_count_70, align 8
  %load_73 = load { i64, i64 }, ptr %alloca_count_70, align 8
  store { i64, i64 } %load_73, ptr %alloca_count_72, align 8
  %load_75 = load { i64, i64 }, ptr %alloca_count_68, align 8
  %call_76 = call i64 @Option__unwrap_or__mono_a7af9f593fdc4675({ i64, i64 } %load_75, i64 0)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_77 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.12, i64 %call_76)
  %load_78 = load { i64, i64 }, ptr %alloca_count_72, align 8
  %call_79 = call i64 @Option__unwrap_or__mono_a7af9f593fdc4675({ i64, i64 } %load_78, i64 99)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_80 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.13, i64 %call_79)
  store i64 2, ptr %alloca_count_81, align 4
  %load_83 = load i32, ptr %alloca_count_81, align 4
  %call_84 = call i32 (ptr, ...) @printf(ptr @.str.15_enums.14, i32 %load_83)
  br label %bb15

bb15:                                             ; preds = %bb14
  ret i32 0
}

declare i32 @printf(ptr, ...)

define internal i64 @Option__unwrap_or__mono_a7af9f593fdc4675({ i64, i64 } %0, i64 %1) {
bb0:
  %alloca_102 = alloca i1, align 1
  %alloca_count_102 = alloca i1, align 1
  %alloca_94 = alloca i64, align 8
  %alloca_count_94 = alloca i64, align 8
  %alloca_88 = alloca i1, align 1
  %alloca_count_88 = alloca i1, align 1
  %alloca_86 = alloca { i64, i64 }, align 8
  %alloca_count_86 = alloca { i64, i64 }, align 8
  %alloca_85 = alloca i64, align 8
  %alloca_count_85 = alloca i64, align 8
  store { i64, i64 } %0, ptr %alloca_count_86, align 8
  %load_90 = load i64, ptr %alloca_count_86, align 8
  %icmp_91 = icmp eq i64 %load_90, 0
  store i1 %icmp_91, ptr %alloca_count_88, align 1
  %load_93 = load i1, ptr %alloca_count_88, align 1
  br i1 %load_93, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  %gep_96 = getelementptr inbounds i8, ptr %alloca_count_86, i64 8
  %load_98 = load i64, ptr %gep_96, align 8
  store i64 %load_98, ptr %alloca_count_94, align 8
  %load_100 = load i64, ptr %alloca_count_94, align 8
  store i64 %load_100, ptr %alloca_count_85, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  %load_104 = load i64, ptr %alloca_count_86, align 8
  %icmp_105 = icmp eq i64 %load_104, 1
  store i1 %icmp_105, ptr %alloca_count_102, align 1
  %load_107 = load i1, ptr %alloca_count_102, align 1
  br i1 %load_107, label %bb4, label %bb5

bb1:                                              ; preds = %bb5, %bb4, %bb2
  %load_108 = load i64, ptr %alloca_count_85, align 8
  ret i64 %load_108

bb4:                                              ; preds = %bb3
  store i64 %1, ptr %alloca_count_85, align 8
  br label %bb1

bb5:                                              ; preds = %bb3
  br label %bb1
}
