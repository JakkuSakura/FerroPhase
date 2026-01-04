; ModuleID = '20_quote_splice'
source_filename = "20_quote_splice"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.20_quote_splice.0 = private unnamed_addr constant [15 x i8] c"step %lld: %d\0A\00", align 1
@.str.20_quote_splice.1 = private unnamed_addr constant [35 x i8] c"\F0\9F\93\98 Tutorial: 20_quote_splice.fp\0A\00", align 1
@.str.20_quote_splice.2 = private unnamed_addr constant [82 x i8] c"\F0\9F\A7\AD Focus: Quote, splice, and emit: staged code generation with runtime output.\0A\00", align 1
@.str.20_quote_splice.3 = private unnamed_addr constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00", align 1
@.str.20_quote_splice.4 = private unnamed_addr constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00", align 1
@.str.20_quote_splice.5 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@.str.20_quote_splice.6 = private unnamed_addr constant [12 x i8] c"result1=%d\0A\00", align 1
@.str.20_quote_splice.7 = private unnamed_addr constant [12 x i8] c"result2=%d\0A\00", align 1

define internal i32 @apply_ops({ ptr, i64 } %0, i32 %1, i32 %2) {
bb0:
  %alloca_55 = alloca i1, align 1
  %alloca_count_55 = alloca i1, align 1
  %alloca_36 = alloca i1, align 1
  %alloca_count_36 = alloca i1, align 1
  %alloca_32 = alloca i32, align 4
  %alloca_count_32 = alloca i32, align 4
  %alloca_23 = alloca i32, align 4
  %alloca_count_23 = alloca i32, align 4
  %alloca_20 = alloca i64, align 8
  %alloca_count_20 = alloca i64, align 8
  %alloca_17 = alloca i64, align 8
  %alloca_count_17 = alloca i64, align 8
  %alloca_14 = alloca i64, align 8
  %alloca_count_14 = alloca i64, align 8
  %alloca_8 = alloca i1, align 1
  %alloca_count_8 = alloca i1, align 1
  %alloca_5 = alloca i64, align 8
  %alloca_count_5 = alloca i64, align 8
  %alloca_3 = alloca i64, align 8
  %alloca_count_3 = alloca i64, align 8
  %alloca_2 = alloca i32, align 4
  %alloca_count_2 = alloca i32, align 4
  %alloca_0 = alloca i32, align 4
  %alloca_count_0 = alloca i32, align 4
  store i32 %1, ptr %alloca_count_0, align 4
  store i64 0, ptr %alloca_count_3, align 8
  br label %bb1

bb1:                                              ; preds = %bb9, %bb0
  %extractvalue_6 = extractvalue { ptr, i64 } %0, 1
  store i64 %extractvalue_6, ptr %alloca_count_5, align 8
  %load_9 = load i64, ptr %alloca_count_3, align 8
  %load_10 = load i64, ptr %alloca_count_5, align 8
  %icmp_11 = icmp slt i64 %load_9, %load_10
  store i1 %icmp_11, ptr %alloca_count_8, align 1
  %load_13 = load i1, ptr %alloca_count_8, align 1
  br i1 %load_13, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_15 = load i64, ptr %alloca_count_3, align 8
  store i64 %load_15, ptr %alloca_count_14, align 8
  %load_18 = load i64, ptr %alloca_count_3, align 8
  store i64 %load_18, ptr %alloca_count_17, align 8
  %load_21 = load i64, ptr %alloca_count_3, align 8
  store i64 %load_21, ptr %alloca_count_20, align 8
  %extractvalue_24 = extractvalue { ptr, i64 } %0, 0
  %load_25 = load i64, ptr %alloca_count_20, align 8
  %iop_26 = mul i64 %load_25, 4
  %gep_28 = getelementptr inbounds i8, ptr %extractvalue_24, i64 %iop_26
  %load_30 = load i32, ptr %gep_28, align 4
  store i32 %load_30, ptr %alloca_count_23, align 4
  %load_33 = load i32, ptr %alloca_count_23, align 4
  %zext = zext i32 %load_33 to i64
  %iop_34 = srem i64 %zext, 2
  store i64 %iop_34, ptr %alloca_count_32, align 4
  %load_37 = load i32, ptr %alloca_count_32, align 4
  %zext1 = zext i32 %load_37 to i64
  %icmp_38 = icmp eq i64 %zext1, 0
  store i1 %icmp_38, ptr %alloca_count_36, align 1
  %load_40 = load i1, ptr %alloca_count_36, align 1
  br i1 %load_40, label %bb4, label %bb5

bb3:                                              ; preds = %bb1
  %load_41 = load i32, ptr %alloca_count_0, align 4
  store i32 %load_41, ptr %alloca_count_2, align 4
  %load_43 = load i32, ptr %alloca_count_2, align 4
  ret i32 %load_43

bb4:                                              ; preds = %bb2
  %load_44 = load i32, ptr %alloca_count_0, align 4
  %load_45 = load i32, ptr %alloca_count_23, align 4
  %zext2 = zext i32 %load_44 to i64
  %zext3 = zext i32 %load_45 to i64
  %iop_46 = add i64 %zext2, %zext3
  store i64 %iop_46, ptr %alloca_count_0, align 4
  br label %bb6

bb5:                                              ; preds = %bb2
  %load_48 = load i32, ptr %alloca_count_0, align 4
  %load_49 = load i32, ptr %alloca_count_23, align 4
  %zext4 = zext i32 %load_48 to i64
  %zext5 = zext i32 %load_49 to i64
  %iop_50 = add i64 %zext4, %zext5
  store i64 %iop_50, ptr %alloca_count_0, align 4
  br label %bb6

bb6:                                              ; preds = %bb5, %bb4
  %load_52 = load i64, ptr %alloca_count_14, align 8
  %load_53 = load i32, ptr %alloca_count_0, align 4
  %call_54 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.0, i64 %load_52, i32 %load_53)
  %load_56 = load i32, ptr %alloca_count_0, align 4
  %zext6 = zext i32 %load_56 to i64
  %zext7 = zext i32 %2 to i64
  %icmp_57 = icmp sge i64 %zext6, %zext7
  store i1 %icmp_57, ptr %alloca_count_55, align 1
  %load_59 = load i1, ptr %alloca_count_55, align 1
  br i1 %load_59, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_60 = load i32, ptr %alloca_count_0, align 4
  store i32 %load_60, ptr %alloca_count_2, align 4
  %load_62 = load i32, ptr %alloca_count_2, align 4
  ret i32 %load_62

bb8:                                              ; preds = %bb6
  br label %bb9

bb9:                                              ; preds = %bb10, %bb8
  %load_63 = load i64, ptr %alloca_count_3, align 8
  %iop_64 = add i64 %load_63, 1
  store i64 %iop_64, ptr %alloca_count_3, align 8
  br label %bb1

bb10:                                             ; No predecessors!
  br label %bb9
}

declare i32 @printf(ptr, ...)

define i32 @main() {
bb0:
  %alloca_81 = alloca [3 x i32], align 4
  %alloca_count_81 = alloca [3 x i32], align 4
  %alloca_78 = alloca i64, align 8
  %alloca_count_78 = alloca i64, align 8
  %alloca_71 = alloca [4 x i32], align 4
  %alloca_count_71 = alloca [4 x i32], align 4
  %call_66 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_67 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.2)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_68 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.3)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_69 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.4)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_70 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.5)
  br label %bb5

bb5:                                              ; preds = %bb4
  store [4 x i32] [i32 1, i32 2, i32 3, i32 4], ptr %alloca_count_71, align 4
  %gep_73 = getelementptr inbounds [4 x i32], ptr %alloca_count_71, i64 0, i64 0
  %insertvalue_74 = insertvalue { ptr, i64 } undef, ptr %gep_73, 0
  %insertvalue_75 = insertvalue { ptr, i64 } %insertvalue_74, i64 4, 1
  %call_76 = call i32 @apply_ops({ ptr, i64 } %insertvalue_75, i32 0, i32 6)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_77 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.6, i32 %call_76)
  br label %bb7

bb7:                                              ; preds = %bb6
  store i64 -2, ptr %alloca_count_78, align 8
  %load_82 = load i64, ptr %alloca_count_78, align 8
  %trunc = trunc i64 %load_82 to i32
  %insertvalue_85 = insertvalue [3 x i32] [i32 5, i32 undef, i32 undef], i32 %trunc, 1
  %insertvalue_86 = insertvalue [3 x i32] %insertvalue_85, i32 7, 2
  store [3 x i32] %insertvalue_86, ptr %alloca_count_81, align 4
  %gep_88 = getelementptr inbounds [3 x i32], ptr %alloca_count_81, i64 0, i64 0
  %insertvalue_89 = insertvalue { ptr, i64 } undef, ptr %gep_88, 0
  %insertvalue_90 = insertvalue { ptr, i64 } %insertvalue_89, i64 3, 1
  %call_91 = call i32 @apply_ops({ ptr, i64 } %insertvalue_90, i32 10, i32 30)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_92 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.7, i32 %call_91)
  br label %bb9

bb9:                                              ; preds = %bb8
  ret i32 0
}
