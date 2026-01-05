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
@.str.23_runtime_collections.11 = constant [21 x i8] c"statuses[%s] = %lld\0A\00"
@.str.23_runtime_collections.12 = constant [21 x i8] c"statuses.len = %lld\0A\00"

define i32 @main() {
bb0:
  %alloca_116 = alloca i64, align 8
  %alloca_count_116 = alloca i64, align 8
  %alloca_81 = alloca i64, align 8
  %alloca_count_81 = alloca i64, align 8
  %alloca_79 = alloca ptr, align 8
  %alloca_count_79 = alloca ptr, align 8
  %alloca_54 = alloca { ptr, i64 }, align 8
  %alloca_count_54 = alloca { ptr, i64 }, i32 3, align 8
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
  %gep_59 = getelementptr inbounds i8, ptr %alloca_count_54, i64 0
  store { ptr, i64 } { ptr @.str.23_runtime_collections.8, i64 200 }, ptr %gep_59, align 8
  %gep_66 = getelementptr inbounds i8, ptr %alloca_count_54, i64 16
  store { ptr, i64 } { ptr @.str.23_runtime_collections.9, i64 202 }, ptr %gep_66, align 8
  %gep_73 = getelementptr inbounds i8, ptr %alloca_count_54, i64 32
  store { ptr, i64 } { ptr @.str.23_runtime_collections.10, i64 404 }, ptr %gep_73, align 8
  %insertvalue_76 = insertvalue { ptr, i64 } undef, ptr %alloca_count_54, 0
  %insertvalue_77 = insertvalue { ptr, i64 } %insertvalue_76, i64 3, 1
  store { ptr, i64 } %insertvalue_77, ptr %alloca_count_53, align 8
  store ptr @.str.23_runtime_collections.9, ptr %alloca_count_79, align 8
  %load_82 = load { ptr, i64 }, ptr %alloca_count_53, align 8
  %extractvalue_83 = extractvalue { ptr, i64 } %load_82, 0
  %load_84 = load ptr, ptr %alloca_count_79, align 8
  %gep_87 = getelementptr inbounds i8, ptr %extractvalue_83, i64 0
  %load_89 = load { ptr, i64 }, ptr %gep_87, align 8
  %extractvalue_90 = extractvalue { ptr, i64 } %load_89, 0
  %extractvalue_91 = extractvalue { ptr, i64 } %load_89, 1
  %ptrtoint = ptrtoint ptr %extractvalue_90 to i64
  %ptrtoint1 = ptrtoint ptr %load_84 to i64
  %icmp_92 = icmp eq i64 %ptrtoint, %ptrtoint1
  %select_93 = select i1 %icmp_92, i64 %extractvalue_91, i64 undef
  %gep_96 = getelementptr inbounds i8, ptr %extractvalue_83, i64 16
  %load_98 = load { ptr, i64 }, ptr %gep_96, align 8
  %extractvalue_99 = extractvalue { ptr, i64 } %load_98, 0
  %extractvalue_100 = extractvalue { ptr, i64 } %load_98, 1
  %ptrtoint2 = ptrtoint ptr %extractvalue_99 to i64
  %ptrtoint3 = ptrtoint ptr %load_84 to i64
  %icmp_101 = icmp eq i64 %ptrtoint2, %ptrtoint3
  %select_102 = select i1 %icmp_101, i64 %extractvalue_100, i64 %select_93
  %gep_105 = getelementptr inbounds i8, ptr %extractvalue_83, i64 32
  %load_107 = load { ptr, i64 }, ptr %gep_105, align 8
  %extractvalue_108 = extractvalue { ptr, i64 } %load_107, 0
  %extractvalue_109 = extractvalue { ptr, i64 } %load_107, 1
  %ptrtoint4 = ptrtoint ptr %extractvalue_108 to i64
  %ptrtoint5 = ptrtoint ptr %load_84 to i64
  %icmp_110 = icmp eq i64 %ptrtoint4, %ptrtoint5
  %select_111 = select i1 %icmp_110, i64 %extractvalue_109, i64 %select_102
  store i64 %select_111, ptr %alloca_count_81, align 8
  %load_113 = load ptr, ptr %alloca_count_79, align 8
  %load_114 = load i64, ptr %alloca_count_81, align 8
  %call_115 = call i32 (ptr, ...) @printf(ptr @.str.23_runtime_collections.11, ptr %load_113, i64 %load_114)
  store i64 3, ptr %alloca_count_116, align 8
  %load_118 = load i64, ptr %alloca_count_116, align 8
  %call_119 = call i32 (ptr, ...) @printf(ptr @.str.23_runtime_collections.12, i64 %load_118)
  br label %bb9

bb9:                                              ; preds = %bb8
  ret i32 0
}

declare i32 @printf(ptr, ...)
