; ModuleID = '05_struct_generation'
source_filename = "05_struct_generation"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.05_struct_generation.0 = private unnamed_addr constant [5 x i8] c"core\00", align 1
@.str.05_struct_generation.1 = private unnamed_addr constant [23 x i8] c"base: id=%lld name=%s\0A\00", align 1
@.str.05_struct_generation.2 = private unnamed_addr constant [8 x i8] c"primary\00", align 1
@.str.05_struct_generation.3 = private unnamed_addr constant [7 x i8] c"strict\00", align 1
@.str.05_struct_generation.4 = private unnamed_addr constant [33 x i8] c"config: id=%lld name=%s mode=%s\0A\00", align 1
@.str.05_struct_generation.5 = private unnamed_addr constant [7 x i8] c"shadow\00", align 1
@.str.05_struct_generation.6 = private unnamed_addr constant [8 x i8] c"relaxed\00", align 1
@.str.05_struct_generation.7 = private unnamed_addr constant [39 x i8] c"config clone: id=%lld name=%s mode=%s\0A\00", align 1
@.str.05_struct_generation.8 = private unnamed_addr constant [21 x i8] c"config fields: %lld\0A\00", align 1
@.str.05_struct_generation.9 = private unnamed_addr constant [21 x i8] c"config has mode: %d\0A\00", align 1
@.str.05_struct_generation.10 = private unnamed_addr constant [28 x i8] c"config has max_retries: %d\0A\00", align 1
@.str.05_struct_generation.11 = private unnamed_addr constant [19 x i8] c"config size: %lld\0A\00", align 1
@.str.05_struct_generation.12 = private unnamed_addr constant [17 x i8] c"config type: %s\0A\00", align 1
@.str.05_struct_generation.13 = private unnamed_addr constant [14 x i8] c"struct Config\00", align 1

define i32 @main() {
bb0:
  %alloca_22 = alloca { i64, ptr, ptr }, align 8
  %alloca_count_22 = alloca { i64, ptr, ptr }, align 8
  %alloca_9 = alloca { i64, ptr, ptr }, align 8
  %alloca_count_9 = alloca { i64, ptr, ptr }, align 8
  %alloca_0 = alloca { i64, ptr }, align 8
  %alloca_count_0 = alloca { i64, ptr }, align 8
  store { i64, ptr } { i64 1, ptr @.str.05_struct_generation.0 }, ptr %alloca_count_0, align 8
  %load_3 = load i64, ptr %alloca_count_0, align 8
  %gep_5 = getelementptr inbounds i8, ptr %alloca_count_0, i64 8
  %load_7 = load ptr, ptr %gep_5, align 8
  %call_8 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.1, i64 %load_3, ptr %load_7)
  store { i64, ptr, ptr } { i64 2, ptr @.str.05_struct_generation.2, ptr @.str.05_struct_generation.3 }, ptr %alloca_count_9, align 8
  %load_12 = load i64, ptr %alloca_count_9, align 8
  %gep_14 = getelementptr inbounds i8, ptr %alloca_count_9, i64 8
  %load_16 = load ptr, ptr %gep_14, align 8
  %gep_18 = getelementptr inbounds i8, ptr %alloca_count_9, i64 16
  %load_20 = load ptr, ptr %gep_18, align 8
  %call_21 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.4, i64 %load_12, ptr %load_16, ptr %load_20)
  store { i64, ptr, ptr } { i64 3, ptr @.str.05_struct_generation.5, ptr @.str.05_struct_generation.6 }, ptr %alloca_count_22, align 8
  %load_25 = load i64, ptr %alloca_count_22, align 8
  %gep_27 = getelementptr inbounds i8, ptr %alloca_count_22, i64 8
  %load_29 = load ptr, ptr %gep_27, align 8
  %gep_31 = getelementptr inbounds i8, ptr %alloca_count_22, i64 16
  %load_33 = load ptr, ptr %gep_31, align 8
  %call_34 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.7, i64 %load_25, ptr %load_29, ptr %load_33)
  %call_35 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.8, i64 3)
  %call_36 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.9, i1 true)
  %call_37 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.10, i1 false)
  %call_38 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.11, i64 24)
  %call_39 = call i32 (ptr, ...) @printf(ptr @.str.05_struct_generation.12, ptr @.str.05_struct_generation.13)
  ret i32 0
}

declare i32 @printf(ptr, ...)
