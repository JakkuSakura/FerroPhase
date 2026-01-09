; ModuleID = '18_comptime_collections'
source_filename = "18_comptime_collections"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_5 = internal constant [0 x { { ptr, i64 }, ptr }] zeroinitializer, align 8
@global_91 = internal constant [6 x i64] [i64 2, i64 3, i64 5, i64 7, i64 11, i64 13], align 8
@global_92 = internal constant [16 x i64] zeroinitializer, align 8
@global_93 = internal constant { ptr, i64 } { ptr @__const_data_0, i64 4 }, align 8
@__const_data_0 = internal constant [4 x { ptr, i64 }] [{ ptr, i64 } { ptr @.str.18_comptime_collections.0, i64 200 }, { ptr, i64 } { ptr @.str.18_comptime_collections.1, i64 201 }, { ptr, i64 } { ptr @.str.18_comptime_collections.2, i64 202 }, { ptr, i64 } { ptr @.str.18_comptime_collections.3, i64 404 }], align 8
@.str.18_comptime_collections.0 = constant [3 x i8] c"ok\00"
@.str.18_comptime_collections.1 = constant [8 x i8] c"created\00"
@.str.18_comptime_collections.2 = constant [9 x i8] c"accepted\00"
@.str.18_comptime_collections.3 = constant [10 x i8] c"not_found\00"
@.str.18_comptime_collections.4 = constant [51 x i8] c"test result: %lld passed; %lld failed; %lld total\0A\00"
@.str.18_comptime_collections.5 = constant [13 x i8] c"  %s ... ok\0A\00"
@.str.18_comptime_collections.6 = constant [17 x i8] c"  %s ... FAILED\0A\00"
@.str.18_comptime_collections.7 = constant [43 x i8] c"\F0\9F\93\98 Tutorial: 18_comptime_collections.fp\0A\00"
@.str.18_comptime_collections.8 = constant [65 x i8] c"\F0\9F\A7\AD Focus: Showcase compile-time Vec and HashMap construction.\0A\00"
@.str.18_comptime_collections.9 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.18_comptime_collections.10 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.18_comptime_collections.11 = constant [2 x i8] c"\0A\00"
@.str.18_comptime_collections.12 = constant [34 x i8] c"=== Compile-time Collections ===\0A\00"
@.str.18_comptime_collections.13 = constant [15 x i8] c"Vec literals:\0A\00"
@.str.18_comptime_collections.14 = constant [31 x i8] c"  primes: %llu elements -> %s\0A\00"
@.str.18_comptime_collections.15 = constant [21 x i8] c"[2, 3, 5, 7, 11, 13]\00"
@.str.18_comptime_collections.16 = constant [30 x i8] c"  zero buffer: %lld elements\0A\00"
@.str.18_comptime_collections.17 = constant [46 x i8] c"  first four zeros: [%lld, %lld, %lld, %lld]\0A\00"
@.str.18_comptime_collections.18 = constant [37 x i8] c"\0AHashMap literal via HashMap::from:\0A\00"
@.str.18_comptime_collections.19 = constant [39 x i8] c"  tracked HTTP statuses: %lld entries\0A\00"
@.str.18_comptime_collections.20 = constant [14 x i8] c"  ok => %lld\0A\00"
@.str.18_comptime_collections.21 = constant [21 x i8] c"  not_found => %lld\0A\00"

define internal void @__closure0_call({ ptr } %0) {
bb0:
  %alloca_0 = alloca ptr, align 8
  %alloca_count_0 = alloca ptr, align 8
  store { ptr } %0, ptr %alloca_count_0, align 8
  %load_3 = load ptr, ptr %alloca_count_0, align 8
  call void @TestCase__run(ptr %load_3)
  br label %bb1

bb1:                                              ; preds = %bb0
  ret void
}

define internal { i64, i64, i64 } @std__test__run_tests() personality ptr @__gxx_personality_v0 {
bb0:
  %alloca_68 = alloca i1, align 1
  %alloca_count_68 = alloca i1, align 1
  %alloca_50 = alloca i64, align 8
  %alloca_count_50 = alloca i64, align 8
  %alloca_45 = alloca i64, align 8
  %alloca_count_45 = alloca i64, align 8
  %alloca_35 = alloca { { ptr, i64 }, ptr }, align 8
  %alloca_count_35 = alloca { { ptr, i64 }, ptr }, align 8
  %alloca_32 = alloca i64, align 8
  %alloca_count_32 = alloca i64, align 8
  %alloca_29 = alloca i64, align 8
  %alloca_count_29 = alloca i64, align 8
  %alloca_23 = alloca i1, align 1
  %alloca_count_23 = alloca i1, align 1
  %alloca_21 = alloca i64, align 8
  %alloca_count_21 = alloca i64, align 8
  %alloca_15 = alloca [0 x { { ptr, i64 }, ptr }], align 8
  %alloca_count_15 = alloca [0 x { { ptr, i64 }, ptr }], align 8
  %alloca_13 = alloca [0 x { { ptr, i64 }, ptr }], align 8
  %alloca_count_13 = alloca [0 x { { ptr, i64 }, ptr }], align 8
  %alloca_11 = alloca [0 x { { ptr, i64 }, ptr }], align 8
  %alloca_count_11 = alloca [0 x { { ptr, i64 }, ptr }], align 8
  %alloca_10 = alloca i64, align 8
  %alloca_count_10 = alloca i64, align 8
  %alloca_9 = alloca i1, align 1
  %alloca_count_9 = alloca i1, align 1
  %alloca_8 = alloca i64, align 8
  %alloca_count_8 = alloca i64, align 8
  %alloca_7 = alloca { i64, i64, i64 }, align 8
  %alloca_count_7 = alloca { i64, i64, i64 }, align 8
  %alloca_6 = alloca i64, align 8
  %alloca_count_6 = alloca i64, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_11, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_13, align 8
  %load_16 = load [0 x { { ptr, i64 }, ptr }], ptr %alloca_count_13, align 8
  store [0 x { { ptr, i64 }, ptr }] %load_16, ptr %alloca_count_15, align 8
  store i64 0, ptr %alloca_count_8, align 8
  store i64 0, ptr %alloca_count_6, align 8
  store i64 0, ptr %alloca_count_10, align 8
  br label %bb1

bb1:                                              ; preds = %bb9, %bb0
  store i64 0, ptr %alloca_count_21, align 8
  %load_24 = load i64, ptr %alloca_count_10, align 8
  %load_25 = load i64, ptr %alloca_count_21, align 8
  %icmp_26 = icmp slt i64 %load_24, %load_25
  store i1 %icmp_26, ptr %alloca_count_23, align 1
  %load_28 = load i1, ptr %alloca_count_23, align 1
  br i1 %load_28, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_30 = load i64, ptr %alloca_count_10, align 8
  store i64 %load_30, ptr %alloca_count_29, align 8
  %load_33 = load i64, ptr %alloca_count_10, align 8
  store i64 %load_33, ptr %alloca_count_32, align 8
  %load_36 = load i64, ptr %alloca_count_32, align 8
  %iop_37 = mul i64 %load_36, 24
  %gep_39 = getelementptr inbounds i8, ptr %alloca_count_15, i64 %iop_37
  %load_41 = load { { ptr, i64 }, ptr }, ptr %gep_39, align 8
  store { { ptr, i64 }, ptr } %load_41, ptr %alloca_count_35, align 8
  %load_43 = load { { ptr, i64 }, ptr }, ptr %alloca_count_35, align 8
  invoke void @__closure0_call({ ptr } undef)
          to label %bb4 unwind label %bb5

bb3:                                              ; preds = %bb1
  %load_46 = load i64, ptr %alloca_count_8, align 8
  %load_47 = load i64, ptr %alloca_count_6, align 8
  %iop_48 = add i64 %load_46, %load_47
  store i64 %iop_48, ptr %alloca_count_45, align 8
  %load_51 = load i64, ptr %alloca_count_45, align 8
  store i64 %load_51, ptr %alloca_count_50, align 8
  %load_53 = load i64, ptr %alloca_count_8, align 8
  %load_54 = load i64, ptr %alloca_count_6, align 8
  %load_55 = load i64, ptr %alloca_count_50, align 8
  %call_56 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.4, i64 %load_53, i64 %load_54, i64 %load_55)
  br label %bb12

bb4:                                              ; preds = %bb2
  store i1 true, ptr %alloca_count_9, align 1
  br label %bb6

bb5:                                              ; preds = %bb2
  %landingpad = landingpad { ptr, i32 }
          catch ptr null
  store i1 false, ptr %alloca_count_9, align 1
  br label %bb6

bb12:                                             ; preds = %bb3
  %load_60 = load i64, ptr %alloca_count_50, align 8
  %load_61 = load i64, ptr %alloca_count_8, align 8
  %load_62 = load i64, ptr %alloca_count_6, align 8
  %insertvalue_63 = insertvalue { i64, i64, i64 } undef, i64 %load_60, 0
  %insertvalue_64 = insertvalue { i64, i64, i64 } %insertvalue_63, i64 %load_61, 1
  %insertvalue_65 = insertvalue { i64, i64, i64 } %insertvalue_64, i64 %load_62, 2
  store { i64, i64, i64 } %insertvalue_65, ptr %alloca_count_7, align 8
  %load_67 = load { i64, i64, i64 }, ptr %alloca_count_7, align 8
  ret { i64, i64, i64 } %load_67

bb6:                                              ; preds = %bb5, %bb4
  %load_69 = load i1, ptr %alloca_count_9, align 1
  store i1 %load_69, ptr %alloca_count_68, align 1
  %load_71 = load i1, ptr %alloca_count_68, align 1
  br i1 %load_71, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_72 = load i64, ptr %alloca_count_8, align 8
  %iop_73 = add i64 %load_72, 1
  store i64 %iop_73, ptr %alloca_count_8, align 8
  %load_76 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_77 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.5, { ptr, i64 } %load_76)
  br label %bb10

bb8:                                              ; preds = %bb6
  %load_78 = load i64, ptr %alloca_count_6, align 8
  %iop_79 = add i64 %load_78, 1
  store i64 %iop_79, ptr %alloca_count_6, align 8
  %load_82 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_83 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.6, { ptr, i64 } %load_82)
  br label %bb11

bb10:                                             ; preds = %bb7
  br label %bb9

bb11:                                             ; preds = %bb8
  br label %bb9

bb9:                                              ; preds = %bb11, %bb10
  %load_84 = load i64, ptr %alloca_count_10, align 8
  %iop_85 = add i64 %load_84, 1
  store i64 %iop_85, ptr %alloca_count_10, align 8
  br label %bb1
}

define internal { i64, i64, i64 } @std__test__run() {
bb0:
  %alloca_87 = alloca { i64, i64, i64 }, align 8
  %alloca_count_87 = alloca { i64, i64, i64 }, align 8
  %call_88 = call { i64, i64, i64 } @std__test__run_tests()
  store { i64, i64, i64 } %call_88, ptr %alloca_count_87, align 8
  br label %bb1

bb1:                                              ; preds = %bb0
  %load_90 = load { i64, i64, i64 }, ptr %alloca_count_87, align 8
  ret { i64, i64, i64 } %load_90
}

define i32 @main() {
bb0:
  %alloca_241 = alloca i64, align 8
  %alloca_count_241 = alloca i64, align 8
  %alloca_237 = alloca i64, align 8
  %alloca_count_237 = alloca i64, align 8
  %alloca_233 = alloca i64, align 8
  %alloca_count_233 = alloca i64, align 8
  %alloca_205 = alloca { ptr, i64 }, align 8
  %alloca_count_205 = alloca { ptr, i64 }, i32 4, align 8
  %alloca_204 = alloca { ptr, i64 }, align 8
  %alloca_count_204 = alloca { ptr, i64 }, align 8
  %alloca_202 = alloca { ptr, i64 }, align 8
  %alloca_count_202 = alloca { ptr, i64 }, align 8
  %alloca_200 = alloca { ptr, i64 }, align 8
  %alloca_count_200 = alloca { ptr, i64 }, align 8
  %alloca_198 = alloca { ptr, i64 }, align 8
  %alloca_count_198 = alloca { ptr, i64 }, align 8
  %alloca_196 = alloca { ptr, i64 }, align 8
  %alloca_count_196 = alloca { ptr, i64 }, align 8
  %alloca_168 = alloca { ptr, i64 }, align 8
  %alloca_count_168 = alloca { ptr, i64 }, i32 4, align 8
  %alloca_167 = alloca { ptr, i64 }, align 8
  %alloca_count_167 = alloca { ptr, i64 }, align 8
  %alloca_165 = alloca { ptr, i64 }, align 8
  %alloca_count_165 = alloca { ptr, i64 }, align 8
  %alloca_163 = alloca { ptr, i64 }, align 8
  %alloca_count_163 = alloca { ptr, i64 }, align 8
  %alloca_161 = alloca { ptr, i64 }, align 8
  %alloca_count_161 = alloca { ptr, i64 }, align 8
  %alloca_159 = alloca { ptr, i64 }, align 8
  %alloca_count_159 = alloca { ptr, i64 }, align 8
  %alloca_131 = alloca i64, align 8
  %alloca_count_131 = alloca i64, align 8
  %alloca_129 = alloca [16 x i64], align 8
  %alloca_count_129 = alloca [16 x i64], align 8
  %alloca_127 = alloca i64, align 8
  %alloca_count_127 = alloca i64, align 8
  %alloca_125 = alloca [16 x i64], align 8
  %alloca_count_125 = alloca [16 x i64], align 8
  %alloca_123 = alloca i64, align 8
  %alloca_count_123 = alloca i64, align 8
  %alloca_121 = alloca [16 x i64], align 8
  %alloca_count_121 = alloca [16 x i64], align 8
  %alloca_119 = alloca i64, align 8
  %alloca_count_119 = alloca i64, align 8
  %alloca_117 = alloca [16 x i64], align 8
  %alloca_count_117 = alloca [16 x i64], align 8
  %alloca_113 = alloca i64, align 8
  %alloca_count_113 = alloca i64, align 8
  %alloca_111 = alloca [16 x i64], align 8
  %alloca_count_111 = alloca [16 x i64], align 8
  %alloca_109 = alloca [16 x i64], align 8
  %alloca_count_109 = alloca [16 x i64], align 8
  %alloca_105 = alloca i64, align 8
  %alloca_count_105 = alloca i64, align 8
  %alloca_103 = alloca [6 x i64], align 8
  %alloca_count_103 = alloca [6 x i64], align 8
  %alloca_101 = alloca [6 x i64], align 8
  %alloca_count_101 = alloca [6 x i64], align 8
  %call_94 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.7)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_95 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.8)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_96 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.9)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_97 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.10)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_98 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.11)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_99 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.12)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_100 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.13)
  br label %bb7

bb7:                                              ; preds = %bb6
  store [6 x i64] [i64 2, i64 3, i64 5, i64 7, i64 11, i64 13], ptr %alloca_count_101, align 8
  store [6 x i64] [i64 2, i64 3, i64 5, i64 7, i64 11, i64 13], ptr %alloca_count_103, align 8
  store i64 6, ptr %alloca_count_105, align 8
  %load_107 = load i64, ptr %alloca_count_105, align 8
  %call_108 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.14, i64 %load_107, ptr @.str.18_comptime_collections.15)
  store [16 x i64] zeroinitializer, ptr %alloca_count_109, align 8
  store [16 x i64] zeroinitializer, ptr %alloca_count_111, align 8
  store i64 16, ptr %alloca_count_113, align 8
  %load_115 = load i64, ptr %alloca_count_113, align 8
  %call_116 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.16, i64 %load_115)
  br label %bb8

bb8:                                              ; preds = %bb7
  store [16 x i64] zeroinitializer, ptr %alloca_count_117, align 8
  store i64 0, ptr %alloca_count_119, align 8
  store [16 x i64] zeroinitializer, ptr %alloca_count_121, align 8
  store i64 1, ptr %alloca_count_123, align 8
  store [16 x i64] zeroinitializer, ptr %alloca_count_125, align 8
  store i64 2, ptr %alloca_count_127, align 8
  store [16 x i64] zeroinitializer, ptr %alloca_count_129, align 8
  store i64 3, ptr %alloca_count_131, align 8
  %load_133 = load i64, ptr %alloca_count_119, align 8
  %iop_134 = mul i64 %load_133, 8
  %gep_136 = getelementptr inbounds i8, ptr %alloca_count_117, i64 %iop_134
  %load_138 = load i64, ptr %gep_136, align 8
  %load_139 = load i64, ptr %alloca_count_123, align 8
  %iop_140 = mul i64 %load_139, 8
  %gep_142 = getelementptr inbounds i8, ptr %alloca_count_121, i64 %iop_140
  %load_144 = load i64, ptr %gep_142, align 8
  %load_145 = load i64, ptr %alloca_count_127, align 8
  %iop_146 = mul i64 %load_145, 8
  %gep_148 = getelementptr inbounds i8, ptr %alloca_count_125, i64 %iop_146
  %load_150 = load i64, ptr %gep_148, align 8
  %load_151 = load i64, ptr %alloca_count_131, align 8
  %iop_152 = mul i64 %load_151, 8
  %gep_154 = getelementptr inbounds i8, ptr %alloca_count_129, i64 %iop_152
  %load_156 = load i64, ptr %gep_154, align 8
  %call_157 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.17, i64 %load_138, i64 %load_144, i64 %load_150, i64 %load_156)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_158 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.18)
  br label %bb10

bb10:                                             ; preds = %bb9
  store { ptr, i64 } { ptr @.str.18_comptime_collections.0, i64 200 }, ptr %alloca_count_159, align 8
  store { ptr, i64 } { ptr @.str.18_comptime_collections.1, i64 201 }, ptr %alloca_count_161, align 8
  store { ptr, i64 } { ptr @.str.18_comptime_collections.2, i64 202 }, ptr %alloca_count_163, align 8
  store { ptr, i64 } { ptr @.str.18_comptime_collections.3, i64 404 }, ptr %alloca_count_165, align 8
  %load_169 = load { ptr, i64 }, ptr %alloca_count_159, align 8
  %gep_172 = getelementptr inbounds i8, ptr %alloca_count_168, i64 0
  store { ptr, i64 } %load_169, ptr %gep_172, align 8
  %load_175 = load { ptr, i64 }, ptr %alloca_count_161, align 8
  %gep_178 = getelementptr inbounds i8, ptr %alloca_count_168, i64 16
  store { ptr, i64 } %load_175, ptr %gep_178, align 8
  %load_181 = load { ptr, i64 }, ptr %alloca_count_163, align 8
  %gep_184 = getelementptr inbounds i8, ptr %alloca_count_168, i64 32
  store { ptr, i64 } %load_181, ptr %gep_184, align 8
  %load_187 = load { ptr, i64 }, ptr %alloca_count_165, align 8
  %gep_190 = getelementptr inbounds i8, ptr %alloca_count_168, i64 48
  store { ptr, i64 } %load_187, ptr %gep_190, align 8
  %insertvalue_193 = insertvalue { ptr, i64 } undef, ptr %alloca_count_168, 0
  %insertvalue_194 = insertvalue { ptr, i64 } %insertvalue_193, i64 4, 1
  store { ptr, i64 } %insertvalue_194, ptr %alloca_count_167, align 8
  store { ptr, i64 } { ptr @.str.18_comptime_collections.0, i64 200 }, ptr %alloca_count_196, align 8
  store { ptr, i64 } { ptr @.str.18_comptime_collections.1, i64 201 }, ptr %alloca_count_198, align 8
  store { ptr, i64 } { ptr @.str.18_comptime_collections.2, i64 202 }, ptr %alloca_count_200, align 8
  store { ptr, i64 } { ptr @.str.18_comptime_collections.3, i64 404 }, ptr %alloca_count_202, align 8
  %load_206 = load { ptr, i64 }, ptr %alloca_count_196, align 8
  %gep_209 = getelementptr inbounds i8, ptr %alloca_count_205, i64 0
  store { ptr, i64 } %load_206, ptr %gep_209, align 8
  %load_212 = load { ptr, i64 }, ptr %alloca_count_198, align 8
  %gep_215 = getelementptr inbounds i8, ptr %alloca_count_205, i64 16
  store { ptr, i64 } %load_212, ptr %gep_215, align 8
  %load_218 = load { ptr, i64 }, ptr %alloca_count_200, align 8
  %gep_221 = getelementptr inbounds i8, ptr %alloca_count_205, i64 32
  store { ptr, i64 } %load_218, ptr %gep_221, align 8
  %load_224 = load { ptr, i64 }, ptr %alloca_count_202, align 8
  %gep_227 = getelementptr inbounds i8, ptr %alloca_count_205, i64 48
  store { ptr, i64 } %load_224, ptr %gep_227, align 8
  %insertvalue_230 = insertvalue { ptr, i64 } undef, ptr %alloca_count_205, 0
  %insertvalue_231 = insertvalue { ptr, i64 } %insertvalue_230, i64 4, 1
  store { ptr, i64 } %insertvalue_231, ptr %alloca_count_204, align 8
  store i64 4, ptr %alloca_count_233, align 8
  %load_235 = load i64, ptr %alloca_count_233, align 8
  %call_236 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.19, i64 %load_235)
  br label %bb11

bb11:                                             ; preds = %bb10
  store i64 200, ptr %alloca_count_237, align 8
  %load_239 = load i64, ptr %alloca_count_237, align 8
  %call_240 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.20, i64 %load_239)
  store i64 404, ptr %alloca_count_241, align 8
  %load_243 = load i64, ptr %alloca_count_241, align 8
  %call_244 = call i32 (ptr, ...) @printf(ptr @.str.18_comptime_collections.21, i64 %load_243)
  ret i32 0
}

define internal void @TestCase__run(ptr %0) {
bb0:
  ret void
}

declare i32 @printf(ptr, ...)

declare i32 @__gxx_personality_v0(...)
