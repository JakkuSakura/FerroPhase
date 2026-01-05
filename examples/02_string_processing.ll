; ModuleID = '02_string_processing'
source_filename = "02_string_processing"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.02_string_processing.0 = constant [40 x i8] c"\F0\9F\93\98 Tutorial: 02_string_processing.fp\0A\00"
@.str.02_string_processing.1 = constant [59 x i8] c"\F0\9F\A7\AD Focus: Compile-time string operations and intrinsics\0A\00"
@.str.02_string_processing.2 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.02_string_processing.3 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.02_string_processing.4 = constant [2 x i8] c"\0A\00"
@.str.02_string_processing.5 = constant [11 x i8] c"FerroPhase\00"
@.str.02_string_processing.6 = constant [20 x i8] c"name='%s' len=%llu\0A\00"
@.str.02_string_processing.7 = constant [6 x i8] c"0.1.0\00"
@.str.02_string_processing.8 = constant [23 x i8] c"version='%s' len=%llu\0A\00"
@.str.02_string_processing.9 = constant [47 x i8] c"prefix_ok=%d, suffix_ok=%d, contains_phase=%d\0A\00"
@.str.02_string_processing.10 = constant [6 x i8] c"Ferro\00"
@.str.02_string_processing.11 = constant [6 x i8] c"Phase\00"
@.str.02_string_processing.12 = constant [30 x i8] c"slices: short='%s' tail='%s'\0A\00"
@.str.02_string_processing.13 = constant [8 x i8] c"words:\0A\00"
@.str.02_string_processing.14 = constant [6 x i8] c"alpha\00"
@.str.02_string_processing.15 = constant [5 x i8] c"beta\00"
@.str.02_string_processing.16 = constant [6 x i8] c"gamma\00"
@.str.02_string_processing.17 = constant [6 x i8] c"delta\00"
@.str.02_string_processing.18 = constant [18 x i8] c"  %s -> len=%llu\0A\00"
@.str.02_string_processing.19 = constant [24 x i8] c"total word length=%llu\0A\00"
@.str.02_string_processing.20 = constant [19 x i8] c"empty=%d, long=%d\0A\00"
@.str.02_string_processing.21 = constant [18 x i8] c"FerroPhase v0.1.0\00"
@.str.02_string_processing.22 = constant [13 x i8] c"banner='%s'\0A\00"
@.str.02_string_processing.23 = constant [18 x i8] c"buffer_size=%llu\0A\00"

define i32 @main() {
bb0:
  %alloca_90 = alloca i64, align 8
  %alloca_count_90 = alloca i64, align 8
  %alloca_86 = alloca ptr, align 8
  %alloca_count_86 = alloca ptr, align 8
  %alloca_79 = alloca i1, align 1
  %alloca_count_79 = alloca i1, align 1
  %alloca_77 = alloca i1, align 1
  %alloca_count_77 = alloca i1, align 1
  %alloca_73 = alloca i64, align 8
  %alloca_count_73 = alloca i64, align 8
  %alloca_54 = alloca i64, align 8
  %alloca_count_54 = alloca i64, align 8
  %alloca_52 = alloca [4 x i64], align 8
  %alloca_count_52 = alloca [4 x i64], align 8
  %alloca_49 = alloca i64, align 8
  %alloca_count_49 = alloca i64, align 8
  %alloca_47 = alloca [4 x ptr], align 8
  %alloca_count_47 = alloca [4 x ptr], align 8
  %alloca_42 = alloca i1, align 1
  %alloca_count_42 = alloca i1, align 1
  %alloca_35 = alloca ptr, align 8
  %alloca_count_35 = alloca ptr, align 8
  %alloca_33 = alloca ptr, align 8
  %alloca_count_33 = alloca ptr, align 8
  %alloca_24 = alloca i1, align 1
  %alloca_count_24 = alloca i1, align 1
  %alloca_22 = alloca i1, align 1
  %alloca_count_22 = alloca i1, align 1
  %alloca_20 = alloca i1, align 1
  %alloca_count_20 = alloca i1, align 1
  %alloca_15 = alloca i64, align 8
  %alloca_count_15 = alloca i64, align 8
  %alloca_13 = alloca ptr, align 8
  %alloca_count_13 = alloca ptr, align 8
  %alloca_8 = alloca i64, align 8
  %alloca_count_8 = alloca i64, align 8
  %alloca_6 = alloca ptr, align 8
  %alloca_count_6 = alloca ptr, align 8
  %alloca_0 = alloca i64, align 8
  %alloca_count_0 = alloca i64, align 8
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.2)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.3)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_5 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  store ptr @.str.02_string_processing.5, ptr %alloca_count_6, align 8
  store i64 10, ptr %alloca_count_8, align 8
  %load_10 = load ptr, ptr %alloca_count_6, align 8
  %load_11 = load i64, ptr %alloca_count_8, align 8
  %call_12 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.6, ptr %load_10, i64 %load_11)
  br label %bb6

bb6:                                              ; preds = %bb5
  store ptr @.str.02_string_processing.7, ptr %alloca_count_13, align 8
  store i64 5, ptr %alloca_count_15, align 8
  %load_17 = load ptr, ptr %alloca_count_13, align 8
  %load_18 = load i64, ptr %alloca_count_15, align 8
  %call_19 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.8, ptr %load_17, i64 %load_18)
  br label %bb7

bb7:                                              ; preds = %bb6
  store i1 true, ptr %alloca_count_20, align 1
  store i1 true, ptr %alloca_count_22, align 1
  store i1 true, ptr %alloca_count_24, align 1
  %load_26 = load i1, ptr %alloca_count_20, align 1
  %zext = zext i1 %load_26 to i32
  %load_28 = load i1, ptr %alloca_count_22, align 1
  %zext1 = zext i1 %load_28 to i32
  %load_30 = load i1, ptr %alloca_count_24, align 1
  %zext2 = zext i1 %load_30 to i32
  %call_32 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.9, i32 %zext, i32 %zext1, i32 %zext2)
  br label %bb8

bb8:                                              ; preds = %bb7
  store ptr @.str.02_string_processing.10, ptr %alloca_count_33, align 8
  store ptr @.str.02_string_processing.11, ptr %alloca_count_35, align 8
  %load_37 = load ptr, ptr %alloca_count_33, align 8
  %load_38 = load ptr, ptr %alloca_count_35, align 8
  %call_39 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.12, ptr %load_37, ptr %load_38)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_40 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.13)
  br label %bb10

bb10:                                             ; preds = %bb9
  store i64 0, ptr %alloca_count_0, align 8
  br label %bb11

bb11:                                             ; preds = %bb12, %bb10
  %load_43 = load i64, ptr %alloca_count_0, align 8
  %icmp_44 = icmp slt i64 %load_43, 4
  store i1 %icmp_44, ptr %alloca_count_42, align 1
  %load_46 = load i1, ptr %alloca_count_42, align 1
  br i1 %load_46, label %bb12, label %bb13

bb12:                                             ; preds = %bb11
  store [4 x ptr] [ptr @.str.02_string_processing.14, ptr @.str.02_string_processing.15, ptr @.str.02_string_processing.16, ptr @.str.02_string_processing.17], ptr %alloca_count_47, align 8
  %load_50 = load i64, ptr %alloca_count_0, align 8
  store i64 %load_50, ptr %alloca_count_49, align 8
  store [4 x i64] [i64 5, i64 4, i64 5, i64 5], ptr %alloca_count_52, align 8
  %load_55 = load i64, ptr %alloca_count_0, align 8
  store i64 %load_55, ptr %alloca_count_54, align 8
  %load_57 = load i64, ptr %alloca_count_49, align 8
  %iop_58 = mul i64 %load_57, 8
  %gep_60 = getelementptr inbounds i8, ptr %alloca_count_47, i64 %iop_58
  %load_62 = load ptr, ptr %gep_60, align 8
  %load_63 = load i64, ptr %alloca_count_54, align 8
  %iop_64 = mul i64 %load_63, 8
  %gep_66 = getelementptr inbounds i8, ptr %alloca_count_52, i64 %iop_64
  %load_68 = load i64, ptr %gep_66, align 8
  %call_69 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.18, ptr %load_62, i64 %load_68)
  %load_70 = load i64, ptr %alloca_count_0, align 8
  %iop_71 = add i64 %load_70, 1
  store i64 %iop_71, ptr %alloca_count_0, align 8
  br label %bb11

bb13:                                             ; preds = %bb11
  store i64 19, ptr %alloca_count_73, align 8
  %load_75 = load i64, ptr %alloca_count_73, align 8
  %call_76 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.19, i64 %load_75)
  br label %bb14

bb14:                                             ; preds = %bb13
  store i1 false, ptr %alloca_count_77, align 1
  store i1 true, ptr %alloca_count_79, align 1
  %load_81 = load i1, ptr %alloca_count_77, align 1
  %zext3 = zext i1 %load_81 to i32
  %load_83 = load i1, ptr %alloca_count_79, align 1
  %zext4 = zext i1 %load_83 to i32
  %call_85 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.20, i32 %zext3, i32 %zext4)
  br label %bb15

bb15:                                             ; preds = %bb14
  store ptr @.str.02_string_processing.21, ptr %alloca_count_86, align 8
  %load_88 = load ptr, ptr %alloca_count_86, align 8
  %call_89 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.22, ptr %load_88)
  br label %bb16

bb16:                                             ; preds = %bb15
  store i64 256, ptr %alloca_count_90, align 8
  %load_92 = load i64, ptr %alloca_count_90, align 8
  %call_93 = call i32 (ptr, ...) @printf(ptr @.str.02_string_processing.23, i64 %load_92)
  br label %bb17

bb17:                                             ; preds = %bb16
  ret i32 0
}

declare i32 @printf(ptr, ...)
