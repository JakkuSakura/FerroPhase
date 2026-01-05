; ModuleID = '18_comptime_collections'
source_filename = "18_comptime_collections"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_0 = internal constant [6 x i64] [i64 2, i64 3, i64 5, i64 7, i64 11, i64 13], align 8
@global_1 = internal constant [16 x i64] zeroinitializer, align 8
@global_2 = internal constant { ptr, i64 } { ptr @__const_data_0, i64 4 }, align 8
@__const_data_0 = internal constant [4 x { ptr, i64 }] [{ ptr, i64 } { ptr @.str.18_comptime_collections.0, i64 200 }, { ptr, i64 } { ptr @.str.18_comptime_collections.1, i64 201 }, { ptr, i64 } { ptr @.str.18_comptime_collections.2, i64 202 }, { ptr, i64 } { ptr @.str.18_comptime_collections.3, i64 404 }], align 8
@.str.18_comptime_collections.0 = constant [3 x i8] c"ok\00"
@.str.18_comptime_collections.1 = constant [8 x i8] c"created\00"
@.str.18_comptime_collections.2 = constant [9 x i8] c"accepted\00"
@.str.18_comptime_collections.3 = constant [10 x i8] c"not_found\00"
@.str.18_comptime_collections.4 = constant [43 x i8] c"\F0\9F\93\98 Tutorial: 18_comptime_collections.fp\0A\00"
@.str.18_comptime_collections.5 = constant [65 x i8] c"\F0\9F\A7\AD Focus: Showcase compile-time Vec and HashMap construction.\0A\00"
@.str.18_comptime_collections.6 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.18_comptime_collections.7 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.18_comptime_collections.8 = constant [2 x i8] c"\0A\00"
@.str.18_comptime_collections.9 = constant [34 x i8] c"=== Compile-time Collections ===\0A\00"
@.str.18_comptime_collections.10 = constant [15 x i8] c"Vec literals:\0A\00"
@.str.18_comptime_collections.11 = constant [31 x i8] c"  primes: %llu elements -> %s\0A\00"
@.str.18_comptime_collections.12 = constant [21 x i8] c"[2, 3, 5, 7, 11, 13]\00"
@.str.18_comptime_collections.13 = constant [30 x i8] c"  zero buffer: %lld elements\0A\00"
@.str.18_comptime_collections.14 = constant [46 x i8] c"  first four zeros: [%lld, %lld, %lld, %lld]\0A\00"
@.str.18_comptime_collections.15 = constant [37 x i8] c"\0AHashMap literal via HashMap::from:\0A\00"
@.str.18_comptime_collections.16 = constant [39 x i8] c"  tracked HTTP statuses: %lld entries\0A\00"
@.str.18_comptime_collections.17 = constant [14 x i8] c"  ok => %lld\0A\00"
@.str.18_comptime_collections.18 = constant [21 x i8] c"  not_found => %lld\0A\00"

define i32 @main() {
bb0:
  %alloca_150 = alloca i64, align 8
  %alloca_count_150 = alloca i64, align 8
  %alloca_146 = alloca i64, align 8
  %alloca_count_146 = alloca i64, align 8
  %alloca_142 = alloca i64, align 8
  %alloca_count_142 = alloca i64, align 8
  %alloca_114 = alloca { ptr, i64 }, align 8
  %alloca_count_114 = alloca { ptr, i64 }, i32 4, align 8
  %alloca_113 = alloca { ptr, i64 }, align 8
  %alloca_count_113 = alloca { ptr, i64 }, align 8
  %alloca_111 = alloca { ptr, i64 }, align 8
  %alloca_count_111 = alloca { ptr, i64 }, align 8
  %alloca_109 = alloca { ptr, i64 }, align 8
  %alloca_count_109 = alloca { ptr, i64 }, align 8
  %alloca_107 = alloca { ptr, i64 }, align 8
  %alloca_count_107 = alloca { ptr, i64 }, align 8
  %alloca_105 = alloca { ptr, i64 }, align 8
  %alloca_count_105 = alloca { ptr, i64 }, align 8
  %alloca_77 = alloca { ptr, i64 }, align 8
  %alloca_count_77 = alloca { ptr, i64 }, i32 4, align 8
  %alloca_76 = alloca { ptr, i64 }, align 8
  %alloca_count_76 = alloca { ptr, i64 }, align 8
  %alloca_74 = alloca { ptr, i64 }, align 8
  %alloca_count_74 = alloca { ptr, i64 }, align 8
  %alloca_72 = alloca { ptr, i64 }, align 8
  %alloca_count_72 = alloca { ptr, i64 }, align 8
  %alloca_70 = alloca { ptr, i64 }, align 8
  %alloca_count_70 = alloca { ptr, i64 }, align 8
  %alloca_68 = alloca { ptr, i64 }, align 8
  %alloca_count_68 = alloca { ptr, i64 }, align 8
  %alloca_40 = alloca i64, align 8
  %alloca_count_40 = alloca i64, align 8
  %alloca_38 = alloca [16 x i64], align 8
  %alloca_count_38 = alloca [16 x i64], align 8
  %alloca_36 = alloca i64, align 8
  %alloca_count_36 = alloca i64, align 8
  %alloca_34 = alloca [16 x i64], align 8
  %alloca_count_34 = alloca [16 x i64], align 8
  %alloca_32 = alloca i64, align 8
  %alloca_count_32 = alloca i64, align 8
  %alloca_30 = alloca [16 x i64], align 8
  %alloca_count_30 = alloca [16 x i64], align 8
  %alloca_28 = alloca i64, align 8
  %alloca_count_28 = alloca i64, align 8
  %alloca_26 = alloca [16 x i64], align 8
  %alloca_count_26 = alloca [16 x i64], align 8
  %alloca_22 = alloca i64, align 8
  %alloca_count_22 = alloca i64, align 8
  %alloca_20 = alloca [16 x i64], align 8
  %alloca_count_20 = alloca [16 x i64], align 8
  %alloca_18 = alloca [16 x i64], align 8
  %alloca_count_18 = alloca [16 x i64], align 8
  %alloca_14 = alloca i64, align 8
  %alloca_count_14 = alloca i64, align 8
  %alloca_12 = alloca [6 x i64], align 8
  %alloca_count_12 = alloca [6 x i64], align 8
  %alloca_10 = alloca [6 x i64], align 8
  %alloca_count_10 = alloca [6 x i64], align 8
  %call_3 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.4)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_4 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.5)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_5 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.6)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_6 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.7)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_7 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.8)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_8 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.9)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_9 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.10)
  br label %bb7

bb7:                                              ; preds = %bb6
  store [6 x i64] [i64 2, i64 3, i64 5, i64 7, i64 11, i64 13], ptr %alloca_count_10, align 8
  store [6 x i64] [i64 2, i64 3, i64 5, i64 7, i64 11, i64 13], ptr %alloca_count_12, align 8
  store i64 6, ptr %alloca_count_14, align 8
  %load_16 = load i64, ptr %alloca_count_14, align 8
  %call_17 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.11, i64 %load_16, ptr @.str.18_comptime_collections.12)
  store [16 x i64] zeroinitializer, ptr %alloca_count_18, align 8
  store [16 x i64] zeroinitializer, ptr %alloca_count_20, align 8
  store i64 16, ptr %alloca_count_22, align 8
  %load_24 = load i64, ptr %alloca_count_22, align 8
  %call_25 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.13, i64 %load_24)
  br label %bb8

bb8:                                              ; preds = %bb7
  store [16 x i64] zeroinitializer, ptr %alloca_count_26, align 8
  store i64 0, ptr %alloca_count_28, align 8
  store [16 x i64] zeroinitializer, ptr %alloca_count_30, align 8
  store i64 1, ptr %alloca_count_32, align 8
  store [16 x i64] zeroinitializer, ptr %alloca_count_34, align 8
  store i64 2, ptr %alloca_count_36, align 8
  store [16 x i64] zeroinitializer, ptr %alloca_count_38, align 8
  store i64 3, ptr %alloca_count_40, align 8
  %load_42 = load i64, ptr %alloca_count_28, align 8
  %iop_43 = mul i64 %load_42, 8
  %gep_45 = getelementptr inbounds i8, ptr %alloca_count_26, i64 %iop_43
  %load_47 = load i64, ptr %gep_45, align 8
  %load_48 = load i64, ptr %alloca_count_32, align 8
  %iop_49 = mul i64 %load_48, 8
  %gep_51 = getelementptr inbounds i8, ptr %alloca_count_30, i64 %iop_49
  %load_53 = load i64, ptr %gep_51, align 8
  %load_54 = load i64, ptr %alloca_count_36, align 8
  %iop_55 = mul i64 %load_54, 8
  %gep_57 = getelementptr inbounds i8, ptr %alloca_count_34, i64 %iop_55
  %load_59 = load i64, ptr %gep_57, align 8
  %load_60 = load i64, ptr %alloca_count_40, align 8
  %iop_61 = mul i64 %load_60, 8
  %gep_63 = getelementptr inbounds i8, ptr %alloca_count_38, i64 %iop_61
  %load_65 = load i64, ptr %gep_63, align 8
  %call_66 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.14, i64 %load_47, i64 %load_53, i64 %load_59, i64 %load_65)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_67 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.15)
  br label %bb10

bb10:                                             ; preds = %bb9
  store { ptr, i64 } { ptr @.str.18_comptime_collections.0, i64 200 }, ptr %alloca_count_68, align 8
  store { ptr, i64 } { ptr @.str.18_comptime_collections.1, i64 201 }, ptr %alloca_count_70, align 8
  store { ptr, i64 } { ptr @.str.18_comptime_collections.2, i64 202 }, ptr %alloca_count_72, align 8
  store { ptr, i64 } { ptr @.str.18_comptime_collections.3, i64 404 }, ptr %alloca_count_74, align 8
  %load_78 = load { ptr, i64 }, ptr %alloca_count_68, align 8
  %gep_81 = getelementptr inbounds i8, ptr %alloca_count_77, i64 0
  store { ptr, i64 } %load_78, ptr %gep_81, align 8
  %load_84 = load { ptr, i64 }, ptr %alloca_count_70, align 8
  %gep_87 = getelementptr inbounds i8, ptr %alloca_count_77, i64 16
  store { ptr, i64 } %load_84, ptr %gep_87, align 8
  %load_90 = load { ptr, i64 }, ptr %alloca_count_72, align 8
  %gep_93 = getelementptr inbounds i8, ptr %alloca_count_77, i64 32
  store { ptr, i64 } %load_90, ptr %gep_93, align 8
  %load_96 = load { ptr, i64 }, ptr %alloca_count_74, align 8
  %gep_99 = getelementptr inbounds i8, ptr %alloca_count_77, i64 48
  store { ptr, i64 } %load_96, ptr %gep_99, align 8
  %insertvalue_102 = insertvalue { ptr, i64 } undef, ptr %alloca_count_77, 0
  %insertvalue_103 = insertvalue { ptr, i64 } %insertvalue_102, i64 4, 1
  store { ptr, i64 } %insertvalue_103, ptr %alloca_count_76, align 8
  store { ptr, i64 } { ptr @.str.18_comptime_collections.0, i64 200 }, ptr %alloca_count_105, align 8
  store { ptr, i64 } { ptr @.str.18_comptime_collections.1, i64 201 }, ptr %alloca_count_107, align 8
  store { ptr, i64 } { ptr @.str.18_comptime_collections.2, i64 202 }, ptr %alloca_count_109, align 8
  store { ptr, i64 } { ptr @.str.18_comptime_collections.3, i64 404 }, ptr %alloca_count_111, align 8
  %load_115 = load { ptr, i64 }, ptr %alloca_count_105, align 8
  %gep_118 = getelementptr inbounds i8, ptr %alloca_count_114, i64 0
  store { ptr, i64 } %load_115, ptr %gep_118, align 8
  %load_121 = load { ptr, i64 }, ptr %alloca_count_107, align 8
  %gep_124 = getelementptr inbounds i8, ptr %alloca_count_114, i64 16
  store { ptr, i64 } %load_121, ptr %gep_124, align 8
  %load_127 = load { ptr, i64 }, ptr %alloca_count_109, align 8
  %gep_130 = getelementptr inbounds i8, ptr %alloca_count_114, i64 32
  store { ptr, i64 } %load_127, ptr %gep_130, align 8
  %load_133 = load { ptr, i64 }, ptr %alloca_count_111, align 8
  %gep_136 = getelementptr inbounds i8, ptr %alloca_count_114, i64 48
  store { ptr, i64 } %load_133, ptr %gep_136, align 8
  %insertvalue_139 = insertvalue { ptr, i64 } undef, ptr %alloca_count_114, 0
  %insertvalue_140 = insertvalue { ptr, i64 } %insertvalue_139, i64 4, 1
  store { ptr, i64 } %insertvalue_140, ptr %alloca_count_113, align 8
  store i64 4, ptr %alloca_count_142, align 8
  %load_144 = load i64, ptr %alloca_count_142, align 8
  %call_145 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.16, i64 %load_144)
  br label %bb11

bb11:                                             ; preds = %bb10
  store i64 200, ptr %alloca_count_146, align 8
  %load_148 = load i64, ptr %alloca_count_146, align 8
  %call_149 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.17, i64 %load_148)
  store i64 404, ptr %alloca_count_150, align 8
  %load_152 = load i64, ptr %alloca_count_150, align 8
  %call_153 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.18, i64 %load_152)
  ret i32 0
}

declare i32 @printf(ptr, ...)
