; ModuleID = '04_struct_introspection'
source_filename = "04_struct_introspection"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.04_struct_introspection.0 = private unnamed_addr constant [30 x i8] c"=== Struct Introspection ===\0A\00", align 1
@.str.04_struct_introspection.1 = private unnamed_addr constant [24 x i8] c"Point size: %llu bytes\0A\00", align 1
@.str.04_struct_introspection.2 = private unnamed_addr constant [24 x i8] c"Color size: %llu bytes\0A\00", align 1
@.str.04_struct_introspection.3 = private unnamed_addr constant [20 x i8] c"Point fields: %llu\0A\00", align 1
@.str.04_struct_introspection.4 = private unnamed_addr constant [20 x i8] c"Color fields: %llu\0A\00", align 1
@.str.04_struct_introspection.5 = private unnamed_addr constant [17 x i8] c"Point has x: %d\0A\00", align 1
@.str.04_struct_introspection.6 = private unnamed_addr constant [17 x i8] c"Point has z: %d\0A\00", align 1
@.str.04_struct_introspection.7 = private unnamed_addr constant [21 x i8] c"Point methods: %llu\0A\00", align 1
@.str.04_struct_introspection.8 = private unnamed_addr constant [21 x i8] c"Color methods: %llu\0A\00", align 1
@.str.04_struct_introspection.9 = private unnamed_addr constant [31 x i8] c"\0A\E2\9C\93 Introspection completed!\0A\00", align 1
@.str.04_struct_introspection.10 = private unnamed_addr constant [29 x i8] c"\0A=== Transpilation Demo ===\0A\00", align 1
@.str.04_struct_introspection.11 = private unnamed_addr constant [29 x i8] c"Transpilation target sizes:\0A\00", align 1
@.str.04_struct_introspection.12 = private unnamed_addr constant [29 x i8] c"  Point: %llu bytes (const)\0A\00", align 1
@.str.04_struct_introspection.13 = private unnamed_addr constant [29 x i8] c"  Color: %llu bytes (const)\0A\00", align 1
@.str.04_struct_introspection.14 = private unnamed_addr constant [24 x i8] c"  Combined: %llu bytes\0A\00", align 1
@.str.04_struct_introspection.15 = private unnamed_addr constant [20 x i8] c"Runtime instances:\0A\00", align 1
@.str.04_struct_introspection.16 = private unnamed_addr constant [20 x i8] c"  Origin: (%f, %f)\0A\00", align 1
@.str.04_struct_introspection.17 = private unnamed_addr constant [30 x i8] c"  Red: rgb(%hhu, %hhu, %hhu)\0A\00", align 1
@.str.04_struct_introspection.18 = private unnamed_addr constant [54 x i8] c"\0A\E2\9C\93 Introspection enables external code generation!\0A\00", align 1

define internal void @main() {
bb0:
  %call_0 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.1, i64 16)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.2, i64 3)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.3, i64 2)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.4, i64 3)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_6 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.5, i32 1)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_8 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.6, i32 0)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_9 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.7, i64 0)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_10 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.8, i64 0)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_11 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.9)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_12 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.10)
  br label %bb11

bb11:                                             ; preds = %bb10
  %alloca_13 = alloca { double, double }, align 8
  %alloca_count_13 = alloca { double, double }, align 8
  store { double, double } zeroinitializer, ptr %alloca_count_13, align 8
  %alloca_15 = alloca { i8, i8, i8 }, align 8
  %alloca_count_15 = alloca { i8, i8, i8 }, align 8
  store { i8, i8, i8 } { i8 -1, i8 0, i8 0 }, ptr %alloca_count_15, align 1
  %call_17 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.11)
  br label %bb12

bb12:                                             ; preds = %bb11
  %alloca_18 = alloca i64, align 8
  %alloca_count_18 = alloca i64, align 8
  store i64 16, ptr %alloca_count_18, align 8
  %load_20 = load i64, ptr %alloca_count_18, align 8
  %call_21 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.12, i64 %load_20)
  br label %bb13

bb13:                                             ; preds = %bb12
  %alloca_22 = alloca i64, align 8
  %alloca_count_22 = alloca i64, align 8
  store i64 24, ptr %alloca_count_22, align 8
  %load_24 = load i64, ptr %alloca_count_22, align 8
  %call_25 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.13, i64 %load_24)
  br label %bb14

bb14:                                             ; preds = %bb13
  %alloca_26 = alloca i64, align 8
  %alloca_count_26 = alloca i64, align 8
  store i64 40, ptr %alloca_count_26, align 8
  %load_28 = load i64, ptr %alloca_count_26, align 8
  %call_29 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.14, i64 %load_28)
  br label %bb15

bb15:                                             ; preds = %bb14
  %call_30 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.15)
  br label %bb16

bb16:                                             ; preds = %bb15
  %load_32 = load double, ptr %alloca_count_13, align 8
  %gep_34 = getelementptr inbounds i8, ptr %alloca_count_13, i64 8
  %load_36 = load double, ptr %gep_34, align 8
  %call_37 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.16, double %load_32, double %load_36)
  br label %bb17

bb17:                                             ; preds = %bb16
  %load_39 = load i8, ptr %alloca_count_15, align 1
  %zext = zext i8 %load_39 to i32
  %gep_42 = getelementptr inbounds i8, ptr %alloca_count_15, i64 1
  %load_44 = load i8, ptr %gep_42, align 1
  %zext1 = zext i8 %load_44 to i32
  %gep_47 = getelementptr inbounds i8, ptr %alloca_count_15, i64 2
  %load_49 = load i8, ptr %gep_47, align 1
  %zext2 = zext i8 %load_49 to i32
  %call_51 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.17, i32 %zext, i32 %zext1, i32 %zext2)
  br label %bb18

bb18:                                             ; preds = %bb17
  %call_52 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.18)
  br label %bb19

bb19:                                             ; preds = %bb18
  ret void
}

declare i32 @printf(ptr, ...)
