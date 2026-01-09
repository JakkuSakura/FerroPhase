; ModuleID = '25_module_imports'
source_filename = "25_module_imports"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_5 = internal constant [0 x { { ptr, i64 }, ptr }] zeroinitializer, align 8
@global_91 = internal constant ptr @.str.25_module_imports.0, align 8
@global_112 = internal constant ptr @.str.25_module_imports.1, align 8
@.str.25_module_imports.0 = constant [6 x i8] c"Hello\00"
@.str.25_module_imports.1 = constant [12 x i8] c"file module\00"
@.str.25_module_imports.2 = constant [51 x i8] c"test result: %lld passed; %lld failed; %lld total\0A\00"
@.str.25_module_imports.3 = constant [13 x i8] c"  %s ... ok\0A\00"
@.str.25_module_imports.4 = constant [17 x i8] c"  %s ... FAILED\0A\00"
@.str.25_module_imports.5 = constant [8 x i8] c"%s %s!\0A\00"
@.str.25_module_imports.6 = constant [36 x i8] c"\F0\9F\93\A6 Example: 25_module_imports.fp\0A\00"
@.str.25_module_imports.7 = constant [66 x i8] c"\F0\9F\94\A7 Focus: inline modules + external file modules + std imports\0A\00"
@.str.25_module_imports.8 = constant [59 x i8] c"\F0\9F\94\8D Expect: greetings + math output using imported names\0A\00"
@.str.25_module_imports.9 = constant [2 x i8] c"\0A\00"
@.str.25_module_imports.10 = constant [6 x i8] c"Ferro\00"
@.str.25_module_imports.11 = constant [15 x i8] c"module imports\00"
@.str.25_module_imports.12 = constant [16 x i8] c"external module\00"
@.str.25_module_imports.13 = constant [24 x i8] c"math.add(8, 34) = %lld\0A\00"
@.str.25_module_imports.14 = constant [30 x i8] c"file_math.add(10, 32) = %lld\0A\00"
@.str.25_module_imports.15 = constant [17 x i8] c"[%s] Hello, %s!\0A\00"
@.str.25_module_imports.16 = constant [12 x i8] c"echo: %lld\0A\00"

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
  %alloca_9 = alloca { i64, i64, i64 }, align 8
  %alloca_count_9 = alloca { i64, i64, i64 }, align 8
  %alloca_8 = alloca i64, align 8
  %alloca_count_8 = alloca i64, align 8
  %alloca_7 = alloca i64, align 8
  %alloca_count_7 = alloca i64, align 8
  %alloca_6 = alloca i1, align 1
  %alloca_count_6 = alloca i1, align 1
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_11, align 8
  store [0 x { { ptr, i64 }, ptr }] zeroinitializer, ptr %alloca_count_13, align 8
  %load_16 = load [0 x { { ptr, i64 }, ptr }], ptr %alloca_count_13, align 8
  store [0 x { { ptr, i64 }, ptr }] %load_16, ptr %alloca_count_15, align 8
  store i64 0, ptr %alloca_count_8, align 8
  store i64 0, ptr %alloca_count_7, align 8
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
  %load_47 = load i64, ptr %alloca_count_7, align 8
  %iop_48 = add i64 %load_46, %load_47
  store i64 %iop_48, ptr %alloca_count_45, align 8
  %load_51 = load i64, ptr %alloca_count_45, align 8
  store i64 %load_51, ptr %alloca_count_50, align 8
  %load_53 = load i64, ptr %alloca_count_8, align 8
  %load_54 = load i64, ptr %alloca_count_7, align 8
  %load_55 = load i64, ptr %alloca_count_50, align 8
  %call_56 = call i32 (ptr, ...) @printf(ptr @.str.25_module_imports.2, i64 %load_53, i64 %load_54, i64 %load_55)
  br label %bb12

bb4:                                              ; preds = %bb2
  store i1 true, ptr %alloca_count_6, align 1
  br label %bb6

bb5:                                              ; preds = %bb2
  %landingpad = landingpad { ptr, i32 }
          catch ptr null
  store i1 false, ptr %alloca_count_6, align 1
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
  %load_69 = load i1, ptr %alloca_count_6, align 1
  store i1 %load_69, ptr %alloca_count_68, align 1
  %load_71 = load i1, ptr %alloca_count_68, align 1
  br i1 %load_71, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_72 = load i64, ptr %alloca_count_8, align 8
  %iop_73 = add i64 %load_72, 1
  store i64 %iop_73, ptr %alloca_count_8, align 8
  %load_76 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_77 = call i32 (ptr, ...) @printf(ptr @.str.25_module_imports.3, { ptr, i64 } %load_76)
  br label %bb10

bb8:                                              ; preds = %bb6
  %load_78 = load i64, ptr %alloca_count_7, align 8
  %iop_79 = add i64 %load_78, 1
  store i64 %iop_79, ptr %alloca_count_7, align 8
  %load_82 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_83 = call i32 (ptr, ...) @printf(ptr @.str.25_module_imports.4, { ptr, i64 } %load_82)
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

define internal void @helpers__greet(ptr %0) {
bb0:
  %alloca_92 = alloca ptr, align 8
  %alloca_count_92 = alloca ptr, align 8
  store ptr @.str.25_module_imports.0, ptr %alloca_count_92, align 8
  %load_94 = load ptr, ptr %alloca_count_92, align 8
  %call_95 = call i32 (ptr, ...) @printf(ptr @.str.25_module_imports.5, ptr %load_94, ptr %0)
  br label %bb1

bb1:                                              ; preds = %bb0
  ret void
}

define internal i64 @helpers__math__add(i64 %0, i64 %1) {
bb0:
  %alloca_96 = alloca i64, align 8
  %alloca_count_96 = alloca i64, align 8
  %iop_97 = add i64 %0, %1
  store i64 %iop_97, ptr %alloca_count_96, align 8
  %load_99 = load i64, ptr %alloca_count_96, align 8
  ret i64 %load_99
}

define i32 @main() {
bb0:
  %call_100 = call i32 (ptr, ...) @printf(ptr @.str.25_module_imports.6)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_101 = call i32 (ptr, ...) @printf(ptr @.str.25_module_imports.7)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_102 = call i32 (ptr, ...) @printf(ptr @.str.25_module_imports.8)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_103 = call i32 (ptr, ...) @printf(ptr @.str.25_module_imports.9)
  br label %bb4

bb4:                                              ; preds = %bb3
  call void @helpers__greet(ptr @.str.25_module_imports.10)
  br label %bb5

bb5:                                              ; preds = %bb4
  call void @helpers__greet(ptr @.str.25_module_imports.11)
  br label %bb6

bb6:                                              ; preds = %bb5
  call void @modules__helpers__greet_from_file(ptr @.str.25_module_imports.12)
  br label %bb7

bb7:                                              ; preds = %bb6
  call void @echo__mono_a7af9f593fdc4675_18(i64 2025)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_108 = call i64 @helpers__math__add(i64 8, i64 34)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_109 = call i32 (ptr, ...) @printf(ptr @.str.25_module_imports.13, i64 %call_108)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_110 = call i64 @modules__helpers__math__add(i64 10, i64 32)
  br label %bb11

bb11:                                             ; preds = %bb10
  %call_111 = call i32 (ptr, ...) @printf(ptr @.str.25_module_imports.14, i64 %call_110)
  br label %bb12

bb12:                                             ; preds = %bb11
  ret i32 0
}

define internal void @modules__helpers__greet_from_file(ptr %0) {
bb0:
  %alloca_113 = alloca ptr, align 8
  %alloca_count_113 = alloca ptr, align 8
  store ptr @.str.25_module_imports.1, ptr %alloca_count_113, align 8
  %load_115 = load ptr, ptr %alloca_count_113, align 8
  %call_116 = call i32 (ptr, ...) @printf(ptr @.str.25_module_imports.15, ptr %load_115, ptr %0)
  br label %bb1

bb1:                                              ; preds = %bb0
  ret void
}

define internal i64 @modules__helpers__math__add(i64 %0, i64 %1) {
bb0:
  %alloca_117 = alloca i64, align 8
  %alloca_count_117 = alloca i64, align 8
  %iop_118 = add i64 %0, %1
  store i64 %iop_118, ptr %alloca_count_117, align 8
  %load_120 = load i64, ptr %alloca_count_117, align 8
  ret i64 %load_120
}

define internal void @echo__mono_a7af9f593fdc4675_18(i64 %0) {
bb0:
  %call_121 = call i32 (ptr, ...) @printf(ptr @.str.25_module_imports.16, i64 %0)
  ret void
}

define internal void @TestCase__run(ptr %0) {
bb0:
  ret void
}

declare i32 @printf(ptr, ...)

declare i32 @__gxx_personality_v0(...)
