; ModuleID = '23_runtime_collections'
source_filename = "23_runtime_collections"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.23_runtime_collections.0 = constant [42 x i8] c"\F0\9F\93\98 Tutorial: 23_runtime_collections.fp\0A\00"
@.str.23_runtime_collections.1 = constant [57 x i8] c"\F0\9F\A7\AD Focus: runtime list/map access with linear search.\0A\00"
@.str.23_runtime_collections.2 = constant [64 x i8] c"\F0\9F\A7\AA What to look for: values resolved from container accesses\0A\00"
@.str.23_runtime_collections.3 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.23_runtime_collections.4 = constant [2 x i8] c"\0A\00"
@.str.23_runtime_collections.5 = constant [29 x i8] c"=== Runtime Collections ===\0A\00"
@.str.23_runtime_collections.6 = constant [22 x i8] c"numbers[%lld] = %lld\0A\00"
@.str.23_runtime_collections.7 = constant [20 x i8] c"numbers.len = %lld\0A\00"
@.str.23_runtime_collections.8 = constant [3 x i8] c"ok\00"
@.str.23_runtime_collections.9 = constant [9 x i8] c"accepted\00"
@.str.23_runtime_collections.10 = constant [5 x i8] c"nope\00"
@.str.23_runtime_collections.11 = constant [19 x i8] c"statuses[%s] = %s\0A\00"
@.str.23_runtime_collections.12 = constant [3 x i8] c"()\00"
@.str.23_runtime_collections.13 = constant [21 x i8] c"statuses.len = %lld\0A\00"

define i32 @main() {
bb0:
  %alloca_89 = alloca i64, align 8
  %alloca_count_89 = alloca i64, align 8
  %alloca_82 = alloca ptr, align 8
  %alloca_count_82 = alloca ptr, align 8
  %alloca_60 = alloca { ptr, i64 }, align 8
  %alloca_count_60 = alloca { ptr, i64 }, i32 3, align 8
  %alloca_59 = alloca ptr, align 8
  %alloca_count_59 = alloca ptr, align 8
  %alloca_57 = alloca { ptr, i64 }, align 8
  %alloca_count_57 = alloca { ptr, i64 }, align 8
  %alloca_55 = alloca { ptr, i64 }, align 8
  %alloca_count_55 = alloca { ptr, i64 }, align 8
  %alloca_53 = alloca { ptr, i64 }, align 8
  %alloca_count_53 = alloca { ptr, i64 }, align 8
  %alloca_49 = alloca i64, align 8
  %alloca_count_49 = alloca i64, align 8
  %alloca_36 = alloca i64, align 8
  %alloca_count_36 = alloca i64, align 8
  %alloca_33 = alloca i64, align 8
  %alloca_count_33 = alloca i64, align 8
  %alloca_31 = alloca i64, align 8
  %alloca_count_31 = alloca i64, align 8
  %alloca_7 = alloca i64, align 8
  %alloca_count_7 = alloca i64, i32 4, align 8
  %alloca_6 = alloca { ptr, i64 }, align 8
  %alloca_count_6 = alloca { ptr, i64 }, align 8
  %call_0 = call i32 (ptr, ...) @printf(ptr @.str.23_runtime_collections.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_1 = call i32 (ptr, ...) @printf(ptr @.str.23_runtime_collections.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_2 = call i32 (ptr, ...) @printf(ptr @.str.23_runtime_collections.2)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.23_runtime_collections.3)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.23_runtime_collections.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_5 = call i32 (ptr, ...) @printf(ptr @.str.23_runtime_collections.5)
  br label %bb6

bb6:                                              ; preds = %bb5
  %gep_10 = getelementptr inbounds i8, ptr %alloca_count_7, i64 0
  store i64 10, ptr %gep_10, align 8
  %gep_15 = getelementptr inbounds i8, ptr %alloca_count_7, i64 8
  store i64 20, ptr %gep_15, align 8
  %gep_20 = getelementptr inbounds i8, ptr %alloca_count_7, i64 16
  store i64 30, ptr %gep_20, align 8
  %gep_25 = getelementptr inbounds i8, ptr %alloca_count_7, i64 24
  store i64 40, ptr %gep_25, align 8
  %insertvalue_28 = insertvalue { ptr, i64 } undef, ptr %alloca_count_7, 0
  %insertvalue_29 = insertvalue { ptr, i64 } %insertvalue_28, i64 4, 1
  store { ptr, i64 } %insertvalue_29, ptr %alloca_count_6, align 8
  store i64 2, ptr %alloca_count_31, align 8
  %load_34 = load i64, ptr %alloca_count_31, align 8
  store i64 %load_34, ptr %alloca_count_33, align 8
  %load_37 = load { ptr, i64 }, ptr %alloca_count_6, align 8
  %extractvalue_38 = extractvalue { ptr, i64 } %load_37, 0
  %load_39 = load i64, ptr %alloca_count_31, align 8
  %iop_40 = mul i64 %load_39, 8
  %gep_42 = getelementptr inbounds i8, ptr %extractvalue_38, i64 %iop_40
  %load_44 = load i64, ptr %gep_42, align 8
  store i64 %load_44, ptr %alloca_count_36, align 8
  %load_46 = load i64, ptr %alloca_count_31, align 8
  %load_47 = load i64, ptr %alloca_count_36, align 8
  %call_48 = call i32 (ptr, ...) @printf(ptr @.str.23_runtime_collections.6, i64 %load_46, i64 %load_47)
  br label %bb7

bb7:                                              ; preds = %bb6
  store i64 4, ptr %alloca_count_49, align 8
  %load_51 = load i64, ptr %alloca_count_49, align 8
  %call_52 = call i32 (ptr, ...) @printf(ptr @.str.23_runtime_collections.7, i64 %load_51)
  br label %bb8

bb8:                                              ; preds = %bb7
  store { ptr, i64 } { ptr @.str.23_runtime_collections.8, i64 200 }, ptr %alloca_count_53, align 8
  store { ptr, i64 } { ptr @.str.23_runtime_collections.9, i64 202 }, ptr %alloca_count_55, align 8
  store { ptr, i64 } { ptr @.str.23_runtime_collections.10, i64 404 }, ptr %alloca_count_57, align 8
  %load_61 = load { ptr, i64 }, ptr %alloca_count_53, align 8
  %gep_64 = getelementptr inbounds i8, ptr %alloca_count_60, i64 0
  store { ptr, i64 } %load_61, ptr %gep_64, align 8
  %load_67 = load { ptr, i64 }, ptr %alloca_count_55, align 8
  %gep_70 = getelementptr inbounds i8, ptr %alloca_count_60, i64 16
  store { ptr, i64 } %load_67, ptr %gep_70, align 8
  %load_73 = load { ptr, i64 }, ptr %alloca_count_57, align 8
  %gep_76 = getelementptr inbounds i8, ptr %alloca_count_60, i64 32
  store { ptr, i64 } %load_73, ptr %gep_76, align 8
  %insertvalue_79 = insertvalue { ptr, i64 } undef, ptr %alloca_count_60, 0
  %insertvalue_80 = insertvalue { ptr, i64 } %insertvalue_79, i64 3, 1
  store { ptr, i64 } %insertvalue_80, ptr %alloca_count_59, align 8
  store ptr @.str.23_runtime_collections.9, ptr %alloca_count_82, align 8
  %load_84 = load ptr, ptr %alloca_count_59, align 8
  %load_85 = load ptr, ptr %alloca_count_82, align 8
  call void @opaque__get_unchecked(ptr %load_84, ptr %load_85)
  br label %bb9

bb9:                                              ; preds = %bb8
  %load_87 = load ptr, ptr %alloca_count_82, align 8
  %call_88 = call i32 (ptr, ...) @printf(ptr @.str.23_runtime_collections.11, ptr %load_87, ptr @.str.23_runtime_collections.12)
  store i64 3, ptr %alloca_count_89, align 8
  %load_91 = load i64, ptr %alloca_count_89, align 8
  %call_92 = call i32 (ptr, ...) @printf(ptr @.str.23_runtime_collections.13, i64 %load_91)
  br label %bb10

bb10:                                             ; preds = %bb9
  ret i32 0
}

declare i32 @printf(ptr, ...)

define internal void @opaque__get_unchecked(ptr %0, ptr %1) {
bb0:
  ret void
}
