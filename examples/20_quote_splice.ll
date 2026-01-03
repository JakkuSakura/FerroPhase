; ModuleID = '20_quote_splice'
source_filename = "20_quote_splice"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

define internal i32 @first_gt(ptr %0, ptr %1) {
bb0:
  %alloca_25 = alloca i1, align 1
  %alloca_count_25 = alloca i1, align 1
  %alloca_22 = alloca i64, align 8
  %alloca_count_22 = alloca i64, align 8
  %alloca_14 = alloca i32, align 4
  %alloca_count_14 = alloca i32, align 4
  %alloca_11 = alloca i64, align 8
  %alloca_count_11 = alloca i64, align 8
  %alloca_8 = alloca i64, align 8
  %alloca_count_8 = alloca i64, align 8
  %alloca_3 = alloca i1, align 1
  %alloca_count_3 = alloca i1, align 1
  %alloca_1 = alloca i32, align 4
  %alloca_count_1 = alloca i32, align 4
  %alloca_0 = alloca i64, align 8
  %alloca_count_0 = alloca i64, align 8
  store i64 0, ptr %alloca_count_0, align 8
  br label %bb1

bb1:                                              ; preds = %bb6, %bb0
  %load_4 = load i64, ptr %alloca_count_0, align 8
  %icmp_5 = icmp slt i64 %load_4, 0
  store i1 %icmp_5, ptr %alloca_count_3, align 1
  %load_7 = load i1, ptr %alloca_count_3, align 1
  br i1 %load_7, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_9 = load i64, ptr %alloca_count_0, align 8
  store i64 %load_9, ptr %alloca_count_8, align 8
  %load_12 = load i64, ptr %alloca_count_0, align 8
  store i64 %load_12, ptr %alloca_count_11, align 8
  %load_15 = load i64, ptr %alloca_count_11, align 8
  %iop_16 = mul i64 %load_15, 4
  %gep_18 = getelementptr inbounds i8, ptr %0, i64 %iop_16
  %load_20 = load i32, ptr %gep_18, align 4
  store i32 %load_20, ptr %alloca_count_14, align 4
  %load_23 = load i64, ptr %alloca_count_8, align 8
  store i64 %load_23, ptr %alloca_count_22, align 8
  %load_26 = load i32, ptr %alloca_count_14, align 4
  %load_27 = load i64, ptr %alloca_count_22, align 8
  %iop_28 = mul i64 %load_27, 4
  %gep_30 = getelementptr inbounds i8, ptr %1, i64 %iop_28
  %load_32 = load i32, ptr %gep_30, align 4
  %zext = zext i32 %load_26 to i64
  %zext1 = zext i32 %load_32 to i64
  %icmp_33 = icmp sgt i64 %zext, %zext1
  store i1 %icmp_33, ptr %alloca_count_25, align 1
  %load_35 = load i1, ptr %alloca_count_25, align 1
  br i1 %load_35, label %bb4, label %bb5

bb3:                                              ; preds = %bb1
  store i64 0, ptr %alloca_count_1, align 4
  %load_37 = load i32, ptr %alloca_count_1, align 4
  ret i32 %load_37

bb4:                                              ; preds = %bb2
  br label %bb6

bb5:                                              ; preds = %bb2
  br label %bb6

bb6:                                              ; preds = %bb5, %bb4
  %load_38 = load i64, ptr %alloca_count_0, align 8
  %iop_39 = add i64 %load_38, 1
  store i64 %iop_39, ptr %alloca_count_0, align 8
  br label %bb1
}

define i32 @main() {
bb0:
  %alloca_43 = alloca ptr, align 8
  %alloca_count_43 = alloca ptr, align 8
  %alloca_41 = alloca ptr, align 8
  %alloca_count_41 = alloca ptr, align 8
  store [3 x i64] [i64 1, i64 2, i64 5], ptr %alloca_count_41, align 8
  store [3 x i64] [i64 0, i64 1, i64 3], ptr %alloca_count_43, align 8
  %load_45 = load ptr, ptr %alloca_count_41, align 8
  %load_46 = load ptr, ptr %alloca_count_43, align 8
  %call_47 = call i32 @first_gt(ptr %load_45, ptr %load_46)
  br label %bb1

bb1:                                              ; preds = %bb0
  ret i32 0
}
