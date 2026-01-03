; ModuleID = '04_struct_introspection'
source_filename = "04_struct_introspection"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.04_struct_introspection.0 = private unnamed_addr constant [30 x i8] c"=== Struct Introspection ===\0A\00", align 1
@.str.04_struct_introspection.1 = private unnamed_addr constant [24 x i8] c"Point size: %lld bytes\0A\00", align 1
@.str.04_struct_introspection.2 = private unnamed_addr constant [24 x i8] c"Color size: %lld bytes\0A\00", align 1
@.str.04_struct_introspection.3 = private unnamed_addr constant [20 x i8] c"Point fields: %lld\0A\00", align 1
@.str.04_struct_introspection.4 = private unnamed_addr constant [20 x i8] c"Color fields: %lld\0A\00", align 1
@.str.04_struct_introspection.5 = private unnamed_addr constant [17 x i8] c"Point has x: %d\0A\00", align 1
@.str.04_struct_introspection.6 = private unnamed_addr constant [17 x i8] c"Point has z: %d\0A\00", align 1
@.str.04_struct_introspection.7 = private unnamed_addr constant [21 x i8] c"Point methods: %lld\0A\00", align 1
@.str.04_struct_introspection.8 = private unnamed_addr constant [21 x i8] c"Color methods: %lld\0A\00", align 1
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

define i32 @main() {
bb0:
  %alloca_24 = alloca i64, align 8
  %alloca_count_24 = alloca i64, align 8
  %alloca_20 = alloca i64, align 8
  %alloca_count_20 = alloca i64, align 8
  %alloca_16 = alloca i64, align 8
  %alloca_count_16 = alloca i64, align 8
  %alloca_13 = alloca { i8, i8, i8 }, align 8
  %alloca_count_13 = alloca { i8, i8, i8 }, align 8
  %alloca_11 = alloca { double, double }, align 8
  %alloca_count_11 = alloca { double, double }, align 8
  %call_0 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.0)
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.1, i64 16)
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.2, i64 24)
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.3, i64 2)
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.4, i64 3)
  %call_5 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.5, i1 true)
  %call_6 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.6, i1 false)
  %call_7 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.7, i64 0)
  %call_8 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.8, i64 0)
  %call_9 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.9)
  %call_10 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.10)
  store { double, double } zeroinitializer, ptr %alloca_count_11, align 8
  store { i8, i8, i8 } { i8 -1, i8 0, i8 0 }, ptr %alloca_count_13, align 1
  %call_15 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.11)
  store i64 16, ptr %alloca_count_16, align 8
  %load_18 = load i64, ptr %alloca_count_16, align 8
  %call_19 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.12, i64 %load_18)
  store i64 24, ptr %alloca_count_20, align 8
  %load_22 = load i64, ptr %alloca_count_20, align 8
  %call_23 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.13, i64 %load_22)
  store i64 40, ptr %alloca_count_24, align 8
  %load_26 = load i64, ptr %alloca_count_24, align 8
  %call_27 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.14, i64 %load_26)
  %call_28 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.15)
  %load_30 = load double, ptr %alloca_count_11, align 8
  %gep_32 = getelementptr inbounds i8, ptr %alloca_count_11, i64 8
  %load_34 = load double, ptr %gep_32, align 8
  %call_35 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.16, double %load_30, double %load_34)
  %load_37 = load i8, ptr %alloca_count_13, align 1
  %gep_39 = getelementptr inbounds i8, ptr %alloca_count_13, i64 1
  %load_41 = load i8, ptr %gep_39, align 1
  %gep_43 = getelementptr inbounds i8, ptr %alloca_count_13, i64 2
  %load_45 = load i8, ptr %gep_43, align 1
  %call_46 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.17, i8 %load_37, i8 %load_41, i8 %load_45)
  %call_47 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.18)
  ret i32 0
}

declare i32 @printf(ptr, ...)
