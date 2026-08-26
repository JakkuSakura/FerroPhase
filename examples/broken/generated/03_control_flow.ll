; ModuleID = '03_control_flow'
source_filename = "03_control_flow"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_5 = internal constant [0 x { { ptr, i64 }, ptr }] zeroinitializer, align 8
@.str.03_control_flow.0 = constant [51 x i8] c"test result: %lld passed; %lld failed; %lld total\0A\00"
@.str.03_control_flow.1 = constant [13 x i8] c"  %s ... ok\0A\00"
@.str.03_control_flow.2 = constant [17 x i8] c"  %s ... FAILED\0A\00"
@.str.03_control_flow.3 = constant [35 x i8] c"\F0\9F\93\98 Tutorial: 03_control_flow.fp\0A\00"
@.str.03_control_flow.4 = constant [81 x i8] c"\F0\9F\A7\AD Focus: Control flow: if/else expressions with const and runtime evaluation\0A\00"
@.str.03_control_flow.5 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.03_control_flow.6 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.03_control_flow.7 = constant [2 x i8] c"\0A\00"
@.str.03_control_flow.8 = constant [5 x i8] c"warm\00"
@.str.03_control_flow.9 = constant [15 x i8] c"%lld\C2\B0C is %s\0A\00"
@.str.03_control_flow.10 = constant [8 x i8] c"outdoor\00"
@.str.03_control_flow.11 = constant [15 x i8] c"Suggested: %s\0A\00"
@.str.03_control_flow.12 = constant [2 x i8] c"B\00"
@.str.03_control_flow.13 = constant [23 x i8] c"Score %lld = grade %s\0A\00"
@.str.03_control_flow.14 = constant [5 x i8] c"high\00"
@.str.03_control_flow.15 = constant [18 x i8] c"Value %lld is %s\0A\00"
@.str.03_control_flow.16 = constant [7 x i8] c"medium\00"
@.str.03_control_flow.17 = constant [4 x i8] c"low\00"

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
  %alloca_10 = alloca i1, align 1
  %alloca_count_10 = alloca i1, align 1
  %alloca_9 = alloca { i64, i64, i64 }, align 8
  %alloca_count_9 = alloca { i64, i64, i64 }, align 8
  %alloca_8 = alloca i64, align 8
  %alloca_count_8 = alloca i64, align 8
  %alloca_7 = alloca i64, align 8
  %alloca_count_7 = alloca i64, align 8
  %alloca_6 = alloca i64, align 8
  %alloca_count_6 = alloca i64, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_11, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_13, align 8
  %load_16 = load [0 x { { ptr, i64 }, ptr }], ptr %alloca_count_13, align 8
  store [0 x { { ptr, i64 }, ptr }] %load_16, ptr %alloca_count_15, align 8
  store i64 0, ptr %alloca_count_8, align 8
  store i64 0, ptr %alloca_count_7, align 8
  store i64 0, ptr %alloca_count_6, align 8
  br label %bb1

bb1:                                              ; preds = %bb9, %bb0
  store i64 0, ptr %alloca_count_21, align 8
  %load_24 = load i64, ptr %alloca_count_6, align 8
  %load_25 = load i64, ptr %alloca_count_21, align 8
  %icmp_26 = icmp slt i64 %load_24, %load_25
  store i1 %icmp_26, ptr %alloca_count_23, align 1
  %load_28 = load i1, ptr %alloca_count_23, align 1
  br i1 %load_28, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_30 = load i64, ptr %alloca_count_6, align 8
  store i64 %load_30, ptr %alloca_count_29, align 8
  %load_33 = load i64, ptr %alloca_count_6, align 8
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
  %load_47 = load i64, ptr %alloca_count_7, align 8
  %iop_48 = add i64 %load_46, %load_47
  store i64 %iop_48, ptr %alloca_count_45, align 8
  %load_51 = load i64, ptr %alloca_count_45, align 8
  store i64 %load_51, ptr %alloca_count_50, align 8
  %load_53 = load i64, ptr %alloca_count_8, align 8
  %load_54 = load i64, ptr %alloca_count_7, align 8
  %load_55 = load i64, ptr %alloca_count_50, align 8
  %call_56 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.0, i64 %load_53, i64 %load_54, i64 %load_55)
  br label %bb12

bb4:                                              ; preds = %bb2
  store i1 true, ptr %alloca_count_10, align 1
  br label %bb6

bb5:                                              ; preds = %bb2
  %landingpad = landingpad { ptr, i32 }
          catch ptr null
  store i1 false, ptr %alloca_count_10, align 1
  br label %bb6

bb12:                                             ; preds = %bb3
  %load_60 = load i64, ptr %alloca_count_50, align 8
  %load_61 = load i64, ptr %alloca_count_8, align 8
  %load_62 = load i64, ptr %alloca_count_7, align 8
  %insertvalue_63 = insertvalue { i64, i64, i64 } undef, i64 %load_60, 0
  %insertvalue_64 = insertvalue { i64, i64, i64 } %insertvalue_63, i64 %load_61, 1
  %insertvalue_65 = insertvalue { i64, i64, i64 } %insertvalue_64, i64 %load_62, 2
  store { i64, i64, i64 } %insertvalue_65, ptr %alloca_count_9, align 8
  %load_67 = load { i64, i64, i64 }, ptr %alloca_count_9, align 8
  ret { i64, i64, i64 } %load_67

bb6:                                              ; preds = %bb5, %bb4
  %load_69 = load i1, ptr %alloca_count_10, align 1
  store i1 %load_69, ptr %alloca_count_68, align 1
  %load_71 = load i1, ptr %alloca_count_68, align 1
  br i1 %load_71, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_72 = load i64, ptr %alloca_count_8, align 8
  %iop_73 = add i64 %load_72, 1
  store i64 %iop_73, ptr %alloca_count_8, align 8
  %load_76 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_77 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.1, { ptr, i64 } %load_76)
  br label %bb10

bb8:                                              ; preds = %bb6
  %load_78 = load i64, ptr %alloca_count_7, align 8
  %iop_79 = add i64 %load_78, 1
  store i64 %iop_79, ptr %alloca_count_7, align 8
  %load_82 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_83 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.2, { ptr, i64 } %load_82)
  br label %bb11

bb10:                                             ; preds = %bb7
  br label %bb9

bb11:                                             ; preds = %bb8
  br label %bb9

bb9:                                              ; preds = %bb11, %bb10
  %load_84 = load i64, ptr %alloca_count_6, align 8
  %iop_85 = add i64 %load_84, 1
  store i64 %iop_85, ptr %alloca_count_6, align 8
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
  %alloca_128 = alloca ptr, align 8
  %alloca_count_128 = alloca ptr, align 8
  %alloca_123 = alloca i1, align 1
  %alloca_count_123 = alloca i1, align 1
  %alloca_117 = alloca i1, align 1
  %alloca_count_117 = alloca i1, align 1
  %alloca_115 = alloca i64, align 8
  %alloca_count_115 = alloca i64, align 8
  %alloca_110 = alloca ptr, align 8
  %alloca_count_110 = alloca ptr, align 8
  %alloca_108 = alloca i64, align 8
  %alloca_count_108 = alloca i64, align 8
  %alloca_104 = alloca ptr, align 8
  %alloca_count_104 = alloca ptr, align 8
  %alloca_99 = alloca ptr, align 8
  %alloca_count_99 = alloca ptr, align 8
  %alloca_97 = alloca i64, align 8
  %alloca_count_97 = alloca i64, align 8
  %alloca_91 = alloca ptr, align 8
  %alloca_count_91 = alloca ptr, align 8
  %call_92 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.3)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_93 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.4)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_94 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.5)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_95 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.6)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_96 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.7)
  br label %bb5

bb5:                                              ; preds = %bb4
  store i64 25, ptr %alloca_count_97, align 8
  store ptr @.str.03_control_flow.8, ptr %alloca_count_99, align 8
  %load_101 = load i64, ptr %alloca_count_97, align 8
  %load_102 = load ptr, ptr %alloca_count_99, align 8
  %call_103 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.9, i64 %load_101, ptr %load_102)
  br label %bb6

bb6:                                              ; preds = %bb5
  store ptr @.str.03_control_flow.10, ptr %alloca_count_104, align 8
  %load_106 = load ptr, ptr %alloca_count_104, align 8
  %call_107 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.11, ptr %load_106)
  br label %bb7

bb7:                                              ; preds = %bb6
  store i64 85, ptr %alloca_count_108, align 8
  store ptr @.str.03_control_flow.12, ptr %alloca_count_110, align 8
  %load_112 = load i64, ptr %alloca_count_108, align 8
  %load_113 = load ptr, ptr %alloca_count_110, align 8
  %call_114 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.13, i64 %load_112, ptr %load_113)
  br label %bb8

bb8:                                              ; preds = %bb7
  store i64 42, ptr %alloca_count_115, align 8
  %load_118 = load i64, ptr %alloca_count_115, align 8
  %icmp_119 = icmp sgt i64 %load_118, 50
  store i1 %icmp_119, ptr %alloca_count_117, align 1
  %load_121 = load i1, ptr %alloca_count_117, align 1
  br i1 %load_121, label %bb9, label %bb10

bb9:                                              ; preds = %bb8
  store ptr @.str.03_control_flow.14, ptr %alloca_count_91, align 8
  br label %bb11

bb10:                                             ; preds = %bb8
  %load_124 = load i64, ptr %alloca_count_115, align 8
  %icmp_125 = icmp sgt i64 %load_124, 25
  store i1 %icmp_125, ptr %alloca_count_123, align 1
  %load_127 = load i1, ptr %alloca_count_123, align 1
  br i1 %load_127, label %bb12, label %bb13

bb11:                                             ; preds = %bb14, %bb9
  %load_129 = load ptr, ptr %alloca_count_91, align 8
  store ptr %load_129, ptr %alloca_count_128, align 8
  %load_131 = load i64, ptr %alloca_count_115, align 8
  %load_132 = load ptr, ptr %alloca_count_128, align 8
  %call_133 = call i32 (ptr, ...) @printf(ptr @.str.03_control_flow.15, i64 %load_131, ptr %load_132)
  br label %bb15

bb12:                                             ; preds = %bb10
  store ptr @.str.03_control_flow.16, ptr %alloca_count_91, align 8
  br label %bb14

bb13:                                             ; preds = %bb10
  store ptr @.str.03_control_flow.17, ptr %alloca_count_91, align 8
  br label %bb14

bb15:                                             ; preds = %bb11
  ret i32 0

bb14:                                             ; preds = %bb13, %bb12
  br label %bb11
}

define internal void @TestCase__run(ptr %0) {
bb0:
  ret void
}

declare i32 @printf(ptr, ...)

declare i32 @__gxx_personality_v0(...)
