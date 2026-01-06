; ModuleID = '20_quote_splice'
source_filename = "20_quote_splice"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.20_quote_splice.0 = constant [35 x i8] c"\F0\9F\93\98 Tutorial: 20_quote_splice.fp\0A\00"
@.str.20_quote_splice.1 = constant [82 x i8] c"\F0\9F\A7\AD Focus: Quote, splice, and emit: staged code generation with runtime output.\0A\00"
@.str.20_quote_splice.2 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.20_quote_splice.3 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.20_quote_splice.4 = constant [2 x i8] c"\0A\00"
@.str.20_quote_splice.5 = constant [12 x i8] c"result1=%d\0A\00"
@.str.20_quote_splice.6 = constant [12 x i8] c"result2=%d\0A\00"
@.str.20_quote_splice.7 = constant [15 x i8] c"step %lld: %d\0A\00"

define i32 @main() {
bb0:
  %call_0 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.2)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.3)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_5 = call i32 @apply_ops__const_1(i32 0, i32 6)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_6 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.5, i32 %call_5)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_7 = call i32 @apply_ops__const_2(i32 10, i32 30)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_8 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.6, i32 %call_7)
  br label %bb9

bb9:                                              ; preds = %bb8
  ret i32 0
}

declare i32 @printf(ptr, ...)

define internal i32 @apply_ops__const_1(i32 %0, i32 %1) {
bb0:
  %alloca_56 = alloca i1, align 1
  %alloca_count_56 = alloca i1, align 1
  %alloca_43 = alloca i1, align 1
  %alloca_count_43 = alloca i1, align 1
  %alloca_30 = alloca i1, align 1
  %alloca_count_30 = alloca i1, align 1
  %alloca_17 = alloca i1, align 1
  %alloca_count_17 = alloca i1, align 1
  %alloca_10 = alloca i32, align 4
  %alloca_count_10 = alloca i32, align 4
  %alloca_9 = alloca i32, align 4
  %alloca_count_9 = alloca i32, align 4
  store i32 %0, ptr %alloca_count_10, align 4
  %load_12 = load i32, ptr %alloca_count_10, align 4
  %zext = zext i32 %load_12 to i64
  %iop_13 = add i64 %zext, 1
  store i64 %iop_13, ptr %alloca_count_10, align 4
  %load_15 = load i32, ptr %alloca_count_10, align 4
  %call_16 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.7, i64 0, i32 %load_15)
  br label %bb1

bb1:                                              ; preds = %bb0
  %load_18 = load i32, ptr %alloca_count_10, align 4
  %zext1 = zext i32 %load_18 to i64
  %zext2 = zext i32 %1 to i64
  %icmp_19 = icmp sge i64 %zext1, %zext2
  store i1 %icmp_19, ptr %alloca_count_17, align 1
  %load_21 = load i1, ptr %alloca_count_17, align 1
  br i1 %load_21, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_22 = load i32, ptr %alloca_count_10, align 4
  store i32 %load_22, ptr %alloca_count_9, align 4
  %load_24 = load i32, ptr %alloca_count_9, align 4
  ret i32 %load_24

bb3:                                              ; preds = %bb1
  br label %bb4

bb4:                                              ; preds = %bb5, %bb3
  %load_25 = load i32, ptr %alloca_count_10, align 4
  %zext3 = zext i32 %load_25 to i64
  %iop_26 = add i64 %zext3, 2
  store i64 %iop_26, ptr %alloca_count_10, align 4
  %load_28 = load i32, ptr %alloca_count_10, align 4
  %call_29 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.7, i64 1, i32 %load_28)
  br label %bb6

bb6:                                              ; preds = %bb4
  %load_31 = load i32, ptr %alloca_count_10, align 4
  %zext4 = zext i32 %load_31 to i64
  %zext5 = zext i32 %1 to i64
  %icmp_32 = icmp sge i64 %zext4, %zext5
  store i1 %icmp_32, ptr %alloca_count_30, align 1
  %load_34 = load i1, ptr %alloca_count_30, align 1
  br i1 %load_34, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_35 = load i32, ptr %alloca_count_10, align 4
  store i32 %load_35, ptr %alloca_count_9, align 4
  %load_37 = load i32, ptr %alloca_count_9, align 4
  ret i32 %load_37

bb8:                                              ; preds = %bb6
  br label %bb9

bb9:                                              ; preds = %bb10, %bb8
  %load_38 = load i32, ptr %alloca_count_10, align 4
  %zext6 = zext i32 %load_38 to i64
  %iop_39 = add i64 %zext6, 3
  store i64 %iop_39, ptr %alloca_count_10, align 4
  %load_41 = load i32, ptr %alloca_count_10, align 4
  %call_42 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.7, i64 2, i32 %load_41)
  br label %bb11

bb11:                                             ; preds = %bb9
  %load_44 = load i32, ptr %alloca_count_10, align 4
  %zext7 = zext i32 %load_44 to i64
  %zext8 = zext i32 %1 to i64
  %icmp_45 = icmp sge i64 %zext7, %zext8
  store i1 %icmp_45, ptr %alloca_count_43, align 1
  %load_47 = load i1, ptr %alloca_count_43, align 1
  br i1 %load_47, label %bb12, label %bb13

bb12:                                             ; preds = %bb11
  %load_48 = load i32, ptr %alloca_count_10, align 4
  store i32 %load_48, ptr %alloca_count_9, align 4
  %load_50 = load i32, ptr %alloca_count_9, align 4
  ret i32 %load_50

bb13:                                             ; preds = %bb11
  br label %bb14

bb14:                                             ; preds = %bb15, %bb13
  %load_51 = load i32, ptr %alloca_count_10, align 4
  %zext9 = zext i32 %load_51 to i64
  %iop_52 = add i64 %zext9, 4
  store i64 %iop_52, ptr %alloca_count_10, align 4
  %load_54 = load i32, ptr %alloca_count_10, align 4
  %call_55 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.7, i64 3, i32 %load_54)
  br label %bb16

bb16:                                             ; preds = %bb14
  %load_57 = load i32, ptr %alloca_count_10, align 4
  %zext10 = zext i32 %load_57 to i64
  %zext11 = zext i32 %1 to i64
  %icmp_58 = icmp sge i64 %zext10, %zext11
  store i1 %icmp_58, ptr %alloca_count_56, align 1
  %load_60 = load i1, ptr %alloca_count_56, align 1
  br i1 %load_60, label %bb17, label %bb18

bb17:                                             ; preds = %bb16
  %load_61 = load i32, ptr %alloca_count_10, align 4
  store i32 %load_61, ptr %alloca_count_9, align 4
  %load_63 = load i32, ptr %alloca_count_9, align 4
  ret i32 %load_63

bb18:                                             ; preds = %bb16
  br label %bb19

bb19:                                             ; preds = %bb20, %bb18
  %load_64 = load i32, ptr %alloca_count_10, align 4
  store i32 %load_64, ptr %alloca_count_9, align 4
  %load_66 = load i32, ptr %alloca_count_9, align 4
  ret i32 %load_66

bb5:                                              ; No predecessors!
  br label %bb4

bb10:                                             ; No predecessors!
  br label %bb9

bb15:                                             ; No predecessors!
  br label %bb14

bb20:                                             ; No predecessors!
  br label %bb19
}

define internal i32 @apply_ops__const_2(i32 %0, i32 %1) {
bb0:
  %alloca_101 = alloca i1, align 1
  %alloca_count_101 = alloca i1, align 1
  %alloca_88 = alloca i1, align 1
  %alloca_count_88 = alloca i1, align 1
  %alloca_75 = alloca i1, align 1
  %alloca_count_75 = alloca i1, align 1
  %alloca_69 = alloca i32, align 4
  %alloca_count_69 = alloca i32, align 4
  %alloca_67 = alloca i32, align 4
  %alloca_count_67 = alloca i32, align 4
  store i32 %0, ptr %alloca_count_67, align 4
  %load_70 = load i32, ptr %alloca_count_67, align 4
  %zext = zext i32 %load_70 to i64
  %iop_71 = add i64 %zext, 5
  store i64 %iop_71, ptr %alloca_count_67, align 4
  %load_73 = load i32, ptr %alloca_count_67, align 4
  %call_74 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.7, i64 0, i32 %load_73)
  br label %bb1

bb1:                                              ; preds = %bb0
  %load_76 = load i32, ptr %alloca_count_67, align 4
  %zext1 = zext i32 %load_76 to i64
  %zext2 = zext i32 %1 to i64
  %icmp_77 = icmp sge i64 %zext1, %zext2
  store i1 %icmp_77, ptr %alloca_count_75, align 1
  %load_79 = load i1, ptr %alloca_count_75, align 1
  br i1 %load_79, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_80 = load i32, ptr %alloca_count_67, align 4
  store i32 %load_80, ptr %alloca_count_69, align 4
  %load_82 = load i32, ptr %alloca_count_69, align 4
  ret i32 %load_82

bb3:                                              ; preds = %bb1
  br label %bb4

bb4:                                              ; preds = %bb5, %bb3
  %load_83 = load i32, ptr %alloca_count_67, align 4
  %zext3 = zext i32 %load_83 to i64
  %iop_84 = add i64 %zext3, -2
  store i64 %iop_84, ptr %alloca_count_67, align 4
  %load_86 = load i32, ptr %alloca_count_67, align 4
  %call_87 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.7, i64 1, i32 %load_86)
  br label %bb6

bb6:                                              ; preds = %bb4
  %load_89 = load i32, ptr %alloca_count_67, align 4
  %zext4 = zext i32 %load_89 to i64
  %zext5 = zext i32 %1 to i64
  %icmp_90 = icmp sge i64 %zext4, %zext5
  store i1 %icmp_90, ptr %alloca_count_88, align 1
  %load_92 = load i1, ptr %alloca_count_88, align 1
  br i1 %load_92, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_93 = load i32, ptr %alloca_count_67, align 4
  store i32 %load_93, ptr %alloca_count_69, align 4
  %load_95 = load i32, ptr %alloca_count_69, align 4
  ret i32 %load_95

bb8:                                              ; preds = %bb6
  br label %bb9

bb9:                                              ; preds = %bb10, %bb8
  %load_96 = load i32, ptr %alloca_count_67, align 4
  %zext6 = zext i32 %load_96 to i64
  %iop_97 = add i64 %zext6, 7
  store i64 %iop_97, ptr %alloca_count_67, align 4
  %load_99 = load i32, ptr %alloca_count_67, align 4
  %call_100 = call i32 (ptr, ...) @printf(ptr @.str.20_quote_splice.7, i64 2, i32 %load_99)
  br label %bb11

bb11:                                             ; preds = %bb9
  %load_102 = load i32, ptr %alloca_count_67, align 4
  %zext7 = zext i32 %load_102 to i64
  %zext8 = zext i32 %1 to i64
  %icmp_103 = icmp sge i64 %zext7, %zext8
  store i1 %icmp_103, ptr %alloca_count_101, align 1
  %load_105 = load i1, ptr %alloca_count_101, align 1
  br i1 %load_105, label %bb12, label %bb13

bb12:                                             ; preds = %bb11
  %load_106 = load i32, ptr %alloca_count_67, align 4
  store i32 %load_106, ptr %alloca_count_69, align 4
  %load_108 = load i32, ptr %alloca_count_69, align 4
  ret i32 %load_108

bb13:                                             ; preds = %bb11
  br label %bb14

bb14:                                             ; preds = %bb15, %bb13
  %load_109 = load i32, ptr %alloca_count_67, align 4
  store i32 %load_109, ptr %alloca_count_69, align 4
  %load_111 = load i32, ptr %alloca_count_69, align 4
  ret i32 %load_111

bb5:                                              ; No predecessors!
  br label %bb4

bb10:                                             ; No predecessors!
  br label %bb9

bb15:                                             ; No predecessors!
  br label %bb14
}
