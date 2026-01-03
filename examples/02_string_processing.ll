; ModuleID = '02_string_processing'
source_filename = "02_string_processing"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.02_string_processing.0 = private unnamed_addr constant [11 x i8] c"FerroPhase\00", align 1
@.str.02_string_processing.1 = private unnamed_addr constant [20 x i8] c"name='%s' len=%llu\0A\00", align 1
@.str.02_string_processing.2 = private unnamed_addr constant [6 x i8] c"0.1.0\00", align 1
@.str.02_string_processing.3 = private unnamed_addr constant [23 x i8] c"version='%s' len=%llu\0A\00", align 1
@.str.02_string_processing.4 = private unnamed_addr constant [47 x i8] c"prefix_ok=%d, suffix_ok=%d, contains_phase=%d\0A\00", align 1
@.str.02_string_processing.5 = private unnamed_addr constant [6 x i8] c"Ferro\00", align 1
@.str.02_string_processing.6 = private unnamed_addr constant [6 x i8] c"Phase\00", align 1
@.str.02_string_processing.7 = private unnamed_addr constant [30 x i8] c"slices: short='%s' tail='%s'\0A\00", align 1
@.str.02_string_processing.8 = private unnamed_addr constant [8 x i8] c"words:\0A\00", align 1
@.str.02_string_processing.9 = private unnamed_addr constant [6 x i8] c"alpha\00", align 1
@.str.02_string_processing.10 = private unnamed_addr constant [5 x i8] c"beta\00", align 1
@.str.02_string_processing.11 = private unnamed_addr constant [6 x i8] c"gamma\00", align 1
@.str.02_string_processing.12 = private unnamed_addr constant [6 x i8] c"delta\00", align 1
@.str.02_string_processing.13 = private unnamed_addr constant [18 x i8] c"  %s -> len=%llu\0A\00", align 1
@.str.02_string_processing.14 = private unnamed_addr constant [24 x i8] c"total word length=%llu\0A\00", align 1
@.str.02_string_processing.15 = private unnamed_addr constant [19 x i8] c"empty=%d, long=%d\0A\00", align 1
@.str.02_string_processing.16 = private unnamed_addr constant [18 x i8] c"FerroPhase v0.1.0\00", align 1
@.str.02_string_processing.17 = private unnamed_addr constant [13 x i8] c"banner='%s'\0A\00", align 1
@.str.02_string_processing.18 = private unnamed_addr constant [18 x i8] c"buffer_size=%llu\0A\00", align 1

define i32 @main() {
bb0:
  %alloca_80 = alloca i64, align 8
  %alloca_count_80 = alloca i64, align 8
  %alloca_76 = alloca ptr, align 8
  %alloca_count_76 = alloca ptr, align 8
  %alloca_71 = alloca i1, align 1
  %alloca_count_71 = alloca i1, align 1
  %alloca_69 = alloca i1, align 1
  %alloca_count_69 = alloca i1, align 1
  %alloca_65 = alloca i64, align 8
  %alloca_count_65 = alloca i64, align 8
  %alloca_46 = alloca i64, align 8
  %alloca_count_46 = alloca i64, align 8
  %alloca_44 = alloca [4 x i64], align 8
  %alloca_count_44 = alloca [4 x i64], align 8
  %alloca_41 = alloca i64, align 8
  %alloca_count_41 = alloca i64, align 8
  %alloca_39 = alloca [4 x ptr], align 8
  %alloca_count_39 = alloca [4 x ptr], align 8
  %alloca_34 = alloca i1, align 1
  %alloca_count_34 = alloca i1, align 1
  %alloca_27 = alloca ptr, align 8
  %alloca_count_27 = alloca ptr, align 8
  %alloca_25 = alloca ptr, align 8
  %alloca_count_25 = alloca ptr, align 8
  %alloca_19 = alloca i1, align 1
  %alloca_count_19 = alloca i1, align 1
  %alloca_17 = alloca i1, align 1
  %alloca_count_17 = alloca i1, align 1
  %alloca_15 = alloca i1, align 1
  %alloca_count_15 = alloca i1, align 1
  %alloca_10 = alloca i64, align 8
  %alloca_count_10 = alloca i64, align 8
  %alloca_8 = alloca ptr, align 8
  %alloca_count_8 = alloca ptr, align 8
  %alloca_3 = alloca i64, align 8
  %alloca_count_3 = alloca i64, align 8
  %alloca_1 = alloca ptr, align 8
  %alloca_count_1 = alloca ptr, align 8
  %alloca_0 = alloca i64, align 8
  %alloca_count_0 = alloca i64, align 8
  store ptr @.str.02_string_processing.0, ptr %alloca_count_1, align 8
  store i64 10, ptr %alloca_count_3, align 8
  %load_5 = load ptr, ptr %alloca_count_1, align 8
  %load_6 = load i64, ptr %alloca_count_3, align 8
  %call_7 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.1, ptr %load_5, i64 %load_6)
  store ptr @.str.02_string_processing.2, ptr %alloca_count_8, align 8
  store i64 5, ptr %alloca_count_10, align 8
  %load_12 = load ptr, ptr %alloca_count_8, align 8
  %load_13 = load i64, ptr %alloca_count_10, align 8
  %call_14 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.3, ptr %load_12, i64 %load_13)
  store i1 true, ptr %alloca_count_15, align 1
  store i1 true, ptr %alloca_count_17, align 1
  store i1 true, ptr %alloca_count_19, align 1
  %load_21 = load i1, ptr %alloca_count_15, align 1
  %load_22 = load i1, ptr %alloca_count_17, align 1
  %load_23 = load i1, ptr %alloca_count_19, align 1
  %call_24 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.4, i1 %load_21, i1 %load_22, i1 %load_23)
  store ptr @.str.02_string_processing.5, ptr %alloca_count_25, align 8
  store ptr @.str.02_string_processing.6, ptr %alloca_count_27, align 8
  %load_29 = load ptr, ptr %alloca_count_25, align 8
  %load_30 = load ptr, ptr %alloca_count_27, align 8
  %call_31 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.7, ptr %load_29, ptr %load_30)
  %call_32 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.8)
  store i64 0, ptr %alloca_count_0, align 8
  br label %bb1

bb1:                                              ; preds = %bb2, %bb0
  %load_35 = load i64, ptr %alloca_count_0, align 8
  %icmp_36 = icmp slt i64 %load_35, 4
  store i1 %icmp_36, ptr %alloca_count_34, align 1
  %load_38 = load i1, ptr %alloca_count_34, align 1
  br i1 %load_38, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  store [4 x ptr] [ptr @.str.02_string_processing.9, ptr @.str.02_string_processing.10, ptr @.str.02_string_processing.11, ptr @.str.02_string_processing.12], ptr %alloca_count_39, align 8
  %load_42 = load i64, ptr %alloca_count_0, align 8
  store i64 %load_42, ptr %alloca_count_41, align 8
  store [4 x i64] [i64 5, i64 4, i64 5, i64 5], ptr %alloca_count_44, align 8
  %load_47 = load i64, ptr %alloca_count_0, align 8
  store i64 %load_47, ptr %alloca_count_46, align 8
  %load_49 = load i64, ptr %alloca_count_41, align 8
  %iop_50 = mul i64 %load_49, 8
  %gep_52 = getelementptr inbounds i8, ptr %alloca_count_39, i64 %iop_50
  %load_54 = load ptr, ptr %gep_52, align 8
  %load_55 = load i64, ptr %alloca_count_46, align 8
  %iop_56 = mul i64 %load_55, 8
  %gep_58 = getelementptr inbounds i8, ptr %alloca_count_44, i64 %iop_56
  %load_60 = load i64, ptr %gep_58, align 8
  %call_61 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.13, ptr %load_54, i64 %load_60)
  %load_62 = load i64, ptr %alloca_count_0, align 8
  %iop_63 = add i64 %load_62, 1
  store i64 %iop_63, ptr %alloca_count_0, align 8
  br label %bb1

bb3:                                              ; preds = %bb1
  store i64 19, ptr %alloca_count_65, align 8
  %load_67 = load i64, ptr %alloca_count_65, align 8
  %call_68 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.14, i64 %load_67)
  store i1 false, ptr %alloca_count_69, align 1
  store i1 true, ptr %alloca_count_71, align 1
  %load_73 = load i1, ptr %alloca_count_69, align 1
  %load_74 = load i1, ptr %alloca_count_71, align 1
  %call_75 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.15, i1 %load_73, i1 %load_74)
  store ptr @.str.02_string_processing.16, ptr %alloca_count_76, align 8
  %load_78 = load ptr, ptr %alloca_count_76, align 8
  %call_79 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.17, ptr %load_78)
  store i64 256, ptr %alloca_count_80, align 8
  %load_82 = load i64, ptr %alloca_count_80, align 8
  %call_83 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.18, i64 %load_82)
  ret i32 0
}

declare i32 @printf(ptr, ...)
