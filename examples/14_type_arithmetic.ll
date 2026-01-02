; ModuleID = '14_type_arithmetic'
source_filename = "14_type_arithmetic"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.14_type_arithmetic.0 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str.14_type_arithmetic.1 = private unnamed_addr constant [10 x i8] c"<unknown>\00", align 1

define internal i64 @describe_union({ i64, i64, i64, i64 } %0) {
bb0:
  %alloca_107 = alloca i64, align 8
  %alloca_count_107 = alloca i64, align 8
  %alloca_101 = alloca i64, align 8
  %alloca_count_101 = alloca i64, align 8
  %alloca_94 = alloca i1, align 1
  %alloca_count_94 = alloca i1, align 1
  %alloca_85 = alloca i64, align 8
  %alloca_count_85 = alloca i64, align 8
  %alloca_79 = alloca i64, align 8
  %alloca_count_79 = alloca i64, align 8
  %alloca_73 = alloca i64, align 8
  %alloca_count_73 = alloca i64, align 8
  %alloca_67 = alloca i64, align 8
  %alloca_count_67 = alloca i64, align 8
  %alloca_57 = alloca i64, align 8
  %alloca_count_57 = alloca i64, align 8
  %alloca_51 = alloca i64, align 8
  %alloca_count_51 = alloca i64, align 8
  %alloca_45 = alloca i1, align 1
  %alloca_count_45 = alloca i1, align 1
  %alloca_43 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_43 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_37 = alloca i1, align 1
  %alloca_count_37 = alloca i1, align 1
  %alloca_28 = alloca i64, align 8
  %alloca_count_28 = alloca i64, align 8
  %alloca_22 = alloca i64, align 8
  %alloca_count_22 = alloca i64, align 8
  %alloca_16 = alloca i64, align 8
  %alloca_count_16 = alloca i64, align 8
  %alloca_10 = alloca i64, align 8
  %alloca_count_10 = alloca i64, align 8
  %alloca_4 = alloca i1, align 1
  %alloca_count_4 = alloca i1, align 1
  %alloca_2 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_2 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_1 = alloca i64, align 8
  %alloca_count_1 = alloca i64, align 8
  %alloca_0 = alloca i64, align 8
  %alloca_count_0 = alloca i64, align 8
  store { i64, i64, i64, i64 } %0, ptr %alloca_count_2, align 8
  %load_6 = load i64, ptr %alloca_count_2, align 8
  %icmp_7 = icmp eq i64 %load_6, 0
  store i1 %icmp_7, ptr %alloca_count_4, align 1
  %load_9 = load i1, ptr %alloca_count_4, align 1
  br i1 %load_9, label %bb2, label %bb3

bb2:                                              ; preds = %bb0
  %gep_12 = getelementptr inbounds i8, ptr %alloca_count_2, i64 8
  %load_14 = load i64, ptr %gep_12, align 8
  store i64 %load_14, ptr %alloca_count_10, align 8
  %gep_18 = getelementptr inbounds i8, ptr %alloca_count_2, i64 16
  %load_20 = load i64, ptr %gep_18, align 8
  store i64 %load_20, ptr %alloca_count_16, align 8
  %gep_24 = getelementptr inbounds i8, ptr %alloca_count_2, i64 24
  %load_26 = load i64, ptr %gep_24, align 8
  store i64 %load_26, ptr %alloca_count_22, align 8
  %load_29 = load i64, ptr %alloca_count_10, align 8
  %load_30 = load i64, ptr %alloca_count_16, align 8
  %iop_31 = add i64 %load_29, %load_30
  store i64 %iop_31, ptr %alloca_count_28, align 8
  %load_33 = load i64, ptr %alloca_count_28, align 8
  %load_34 = load i64, ptr %alloca_count_22, align 8
  %iop_35 = add i64 %load_33, %load_34
  store i64 %iop_35, ptr %alloca_count_0, align 8
  br label %bb1

bb3:                                              ; preds = %bb0
  %load_39 = load i64, ptr %alloca_count_2, align 8
  %icmp_40 = icmp eq i64 %load_39, 1
  store i1 %icmp_40, ptr %alloca_count_37, align 1
  %load_42 = load i1, ptr %alloca_count_37, align 1
  br i1 %load_42, label %bb4, label %bb5

bb1:                                              ; preds = %bb5, %bb4, %bb2
  store { i64, i64, i64, i64 } %0, ptr %alloca_count_43, align 8
  %load_47 = load i64, ptr %alloca_count_43, align 8
  %icmp_48 = icmp eq i64 %load_47, 0
  store i1 %icmp_48, ptr %alloca_count_45, align 1
  %load_50 = load i1, ptr %alloca_count_45, align 1
  br i1 %load_50, label %bb7, label %bb8

bb4:                                              ; preds = %bb3
  %gep_53 = getelementptr inbounds i8, ptr %alloca_count_2, i64 8
  %load_55 = load i64, ptr %gep_53, align 8
  store i64 %load_55, ptr %alloca_count_51, align 8
  %gep_59 = getelementptr inbounds i8, ptr %alloca_count_2, i64 16
  %load_61 = load i64, ptr %gep_59, align 8
  store i64 %load_61, ptr %alloca_count_57, align 8
  %load_63 = load i64, ptr %alloca_count_51, align 8
  %load_64 = load i64, ptr %alloca_count_57, align 8
  %iop_65 = add i64 %load_63, %load_64
  store i64 %iop_65, ptr %alloca_count_0, align 8
  br label %bb1

bb5:                                              ; preds = %bb3
  br label %bb1

bb7:                                              ; preds = %bb1
  %gep_69 = getelementptr inbounds i8, ptr %alloca_count_43, i64 8
  %load_71 = load i64, ptr %gep_69, align 8
  store i64 %load_71, ptr %alloca_count_67, align 8
  %gep_75 = getelementptr inbounds i8, ptr %alloca_count_43, i64 16
  %load_77 = load i64, ptr %gep_75, align 8
  store i64 %load_77, ptr %alloca_count_73, align 8
  %gep_81 = getelementptr inbounds i8, ptr %alloca_count_43, i64 24
  %load_83 = load i64, ptr %gep_81, align 8
  store i64 %load_83, ptr %alloca_count_79, align 8
  %load_86 = load i64, ptr %alloca_count_67, align 8
  %load_87 = load i64, ptr %alloca_count_73, align 8
  %iop_88 = add i64 %load_86, %load_87
  store i64 %iop_88, ptr %alloca_count_85, align 8
  %load_90 = load i64, ptr %alloca_count_85, align 8
  %load_91 = load i64, ptr %alloca_count_79, align 8
  %iop_92 = add i64 %load_90, %load_91
  store i64 %iop_92, ptr %alloca_count_1, align 8
  br label %bb6

bb8:                                              ; preds = %bb1
  %load_96 = load i64, ptr %alloca_count_43, align 8
  %icmp_97 = icmp eq i64 %load_96, 1
  store i1 %icmp_97, ptr %alloca_count_94, align 1
  %load_99 = load i1, ptr %alloca_count_94, align 1
  br i1 %load_99, label %bb9, label %bb10

bb6:                                              ; preds = %bb10, %bb9, %bb7
  %load_100 = load i64, ptr %alloca_count_1, align 8
  ret i64 %load_100

bb9:                                              ; preds = %bb8
  %gep_103 = getelementptr inbounds i8, ptr %alloca_count_43, i64 8
  %load_105 = load i64, ptr %gep_103, align 8
  store i64 %load_105, ptr %alloca_count_101, align 8
  %gep_109 = getelementptr inbounds i8, ptr %alloca_count_43, i64 16
  %load_111 = load i64, ptr %gep_109, align 8
  store i64 %load_111, ptr %alloca_count_107, align 8
  %load_113 = load i64, ptr %alloca_count_101, align 8
  %load_114 = load i64, ptr %alloca_count_107, align 8
  %iop_115 = add i64 %load_113, %load_114
  store i64 %iop_115, ptr %alloca_count_1, align 8
  br label %bb6

bb10:                                             ; preds = %bb8
  br label %bb6
}

define i32 @main() {
bb0:
  %alloca_126 = alloca [4 x i64], align 8
  %alloca_count_126 = alloca [4 x i64], align 8
  %alloca_124 = alloca [4 x i64], align 8
  %alloca_count_124 = alloca [4 x i64], align 8
  %alloca_121 = alloca ptr, align 8
  %alloca_count_121 = alloca ptr, align 8
  %alloca_119 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_119 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_117 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_117 = alloca { i64, i64, i64, i64 }, align 8
  store { i64, i64, i64, i64 } { i64 0, i64 1, i64 2, i64 3 }, ptr %alloca_count_117, align 8
  store { i64, i64, i64, i64 } { i64 1, i64 4, i64 5, i64 0 }, ptr %alloca_count_119, align 8
  %call_122 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.0, ptr @.str.14_type_arithmetic.1)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_123 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.0, ptr @.str.14_type_arithmetic.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  store [4 x i64] [i64 1, i64 2, i64 3, i64 4], ptr %alloca_count_124, align 8
  %load_127 = load [4 x i64], ptr %alloca_count_124, align 8
  store [4 x i64] %load_127, ptr %alloca_count_126, align 8
  ret i32 0
}

declare i32 @printf(ptr, ...)

define internal void @proposed_syntax_examples({ i64, i64, i64 } %0, { i64, i64, i64, i64 } %1) {
bb0:
  %alloca_129 = alloca { i64, i64, i64, i64 }, align 8
  %alloca_count_129 = alloca { i64, i64, i64, i64 }, align 8
  store { i64, i64, i64, i64 } %1, ptr %alloca_count_129, align 8
  ret void
}

define internal void @print_display(ptr %0) {
bb0:
  %call_131 = call i32 (ptr, ...) @printf(ptr @.str.14_type_arithmetic.0, ptr @.str.14_type_arithmetic.1)
  br label %bb1

bb1:                                              ; preds = %bb0
  ret void
}
