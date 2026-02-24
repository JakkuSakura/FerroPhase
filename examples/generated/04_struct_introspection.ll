; ModuleID = '04_struct_introspection'
source_filename = "04_struct_introspection"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@global_5 = internal constant [0 x { { ptr, i64 }, ptr }] zeroinitializer, align 8
@.str.04_struct_introspection.0 = constant [51 x i8] c"test result: %lld passed; %lld failed; %lld total\0A\00"
@.str.04_struct_introspection.1 = constant [13 x i8] c"  %s ... ok\0A\00"
@.str.04_struct_introspection.2 = constant [17 x i8] c"  %s ... FAILED\0A\00"
@.str.04_struct_introspection.3 = constant [43 x i8] c"\F0\9F\93\98 Tutorial: 04_struct_introspection.fp\0A\00"
@.str.04_struct_introspection.4 = constant [48 x i8] c"\F0\9F\A7\AD Focus: Struct introspection demonstration\0A\00"
@.str.04_struct_introspection.5 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.04_struct_introspection.6 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.04_struct_introspection.7 = constant [2 x i8] c"\0A\00"
@.str.04_struct_introspection.8 = constant [30 x i8] c"=== Struct Introspection ===\0A\00"
@.str.04_struct_introspection.9 = constant [24 x i8] c"Point size: %lld bytes\0A\00"
@.str.04_struct_introspection.10 = constant [24 x i8] c"Color size: %lld bytes\0A\00"
@.str.04_struct_introspection.11 = constant [20 x i8] c"Point fields: %lld\0A\00"
@.str.04_struct_introspection.12 = constant [20 x i8] c"Color fields: %lld\0A\00"
@.str.04_struct_introspection.13 = constant [17 x i8] c"Point has x: %d\0A\00"
@.str.04_struct_introspection.14 = constant [17 x i8] c"Point has z: %d\0A\00"
@.str.04_struct_introspection.15 = constant [21 x i8] c"Point methods: %lld\0A\00"
@.str.04_struct_introspection.16 = constant [21 x i8] c"Color methods: %lld\0A\00"
@.str.04_struct_introspection.17 = constant [31 x i8] c"\0A\E2\9C\93 Introspection completed!\0A\00"
@.str.04_struct_introspection.18 = constant [29 x i8] c"\0A=== Transpilation Demo ===\0A\00"
@.str.04_struct_introspection.19 = constant [29 x i8] c"Transpilation target sizes:\0A\00"
@.str.04_struct_introspection.20 = constant [29 x i8] c"  Point: %llu bytes (const)\0A\00"
@.str.04_struct_introspection.21 = constant [29 x i8] c"  Color: %llu bytes (const)\0A\00"
@.str.04_struct_introspection.22 = constant [24 x i8] c"  Combined: %llu bytes\0A\00"
@.str.04_struct_introspection.23 = constant [20 x i8] c"Runtime instances:\0A\00"
@.str.04_struct_introspection.24 = constant [20 x i8] c"  Origin: (%f, %f)\0A\00"
@.str.04_struct_introspection.25 = constant [30 x i8] c"  Red: rgb(%hhu, %hhu, %hhu)\0A\00"
@.str.04_struct_introspection.26 = constant [54 x i8] c"\0A\E2\9C\93 Introspection enables external code generation!\0A\00"

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
  %alloca_10 = alloca { i64, i64, i64 }, align 8
  %alloca_count_10 = alloca { i64, i64, i64 }, align 8
  %alloca_9 = alloca i1, align 1
  %alloca_count_9 = alloca i1, align 1
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
  store i64 0, ptr %alloca_count_7, align 8
  store i64 0, ptr %alloca_count_6, align 8
  store i64 0, ptr %alloca_count_8, align 8
  br label %bb1

bb1:                                              ; preds = %bb9, %bb0
  store i64 0, ptr %alloca_count_21, align 8
  %load_24 = load i64, ptr %alloca_count_8, align 8
  %load_25 = load i64, ptr %alloca_count_21, align 8
  %icmp_26 = icmp slt i64 %load_24, %load_25
  store i1 %icmp_26, ptr %alloca_count_23, align 1
  %load_28 = load i1, ptr %alloca_count_23, align 1
  br i1 %load_28, label %bb2, label %bb3

bb2:                                              ; preds = %bb1
  %load_30 = load i64, ptr %alloca_count_8, align 8
  store i64 %load_30, ptr %alloca_count_29, align 8
  %load_33 = load i64, ptr %alloca_count_8, align 8
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
  %load_46 = load i64, ptr %alloca_count_7, align 8
  %load_47 = load i64, ptr %alloca_count_6, align 8
  %iop_48 = add i64 %load_46, %load_47
  store i64 %iop_48, ptr %alloca_count_45, align 8
  %load_51 = load i64, ptr %alloca_count_45, align 8
  store i64 %load_51, ptr %alloca_count_50, align 8
  %load_53 = load i64, ptr %alloca_count_7, align 8
  %load_54 = load i64, ptr %alloca_count_6, align 8
  %load_55 = load i64, ptr %alloca_count_50, align 8
  %call_56 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.0, i64 %load_53, i64 %load_54, i64 %load_55)
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
  %load_61 = load i64, ptr %alloca_count_7, align 8
  %load_62 = load i64, ptr %alloca_count_6, align 8
  %insertvalue_63 = insertvalue { i64, i64, i64 } undef, i64 %load_60, 0
  %insertvalue_64 = insertvalue { i64, i64, i64 } %insertvalue_63, i64 %load_61, 1
  %insertvalue_65 = insertvalue { i64, i64, i64 } %insertvalue_64, i64 %load_62, 2
  store { i64, i64, i64 } %insertvalue_65, ptr %alloca_count_10, align 8
  %load_67 = load { i64, i64, i64 }, ptr %alloca_count_10, align 8
  ret { i64, i64, i64 } %load_67

bb6:                                              ; preds = %bb5, %bb4
  %load_69 = load i1, ptr %alloca_count_9, align 1
  store i1 %load_69, ptr %alloca_count_68, align 1
  %load_71 = load i1, ptr %alloca_count_68, align 1
  br i1 %load_71, label %bb7, label %bb8

bb7:                                              ; preds = %bb6
  %load_72 = load i64, ptr %alloca_count_7, align 8
  %iop_73 = add i64 %load_72, 1
  store i64 %iop_73, ptr %alloca_count_7, align 8
  %load_76 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_77 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.1, { ptr, i64 } %load_76)
  br label %bb10

bb8:                                              ; preds = %bb6
  %load_78 = load i64, ptr %alloca_count_6, align 8
  %iop_79 = add i64 %load_78, 1
  store i64 %iop_79, ptr %alloca_count_6, align 8
  %load_82 = load { ptr, i64 }, ptr %alloca_count_35, align 8
  %call_83 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.2, { ptr, i64 } %load_82)
  br label %bb11

bb10:                                             ; preds = %bb7
  br label %bb9

bb11:                                             ; preds = %bb8
  br label %bb9

bb9:                                              ; preds = %bb11, %bb10
  %load_84 = load i64, ptr %alloca_count_8, align 8
  %iop_85 = add i64 %load_84, 1
  store i64 %iop_85, ptr %alloca_count_8, align 8
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
  %alloca_122 = alloca i64, align 8
  %alloca_count_122 = alloca i64, align 8
  %alloca_118 = alloca i64, align 8
  %alloca_count_118 = alloca i64, align 8
  %alloca_114 = alloca i64, align 8
  %alloca_count_114 = alloca i64, align 8
  %alloca_111 = alloca { i8, i8, i8 }, align 8
  %alloca_count_111 = alloca { i8, i8, i8 }, align 8
  %alloca_109 = alloca { double, double }, align 8
  %alloca_count_109 = alloca { double, double }, align 8
  %call_91 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.3)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_92 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.4)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_93 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.5)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_94 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.6)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_95 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.7)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_96 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.8)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_97 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.9, i64 16)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_98 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.10, i64 24)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_99 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.11, i64 2)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_100 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.12, i64 3)
  br label %bb10

bb10:                                             ; preds = %bb9
  %call_102 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.13, i32 1)
  br label %bb11

bb11:                                             ; preds = %bb10
  %call_104 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.14, i32 0)
  br label %bb12

bb12:                                             ; preds = %bb11
  %call_105 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.15, i64 0)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_106 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.16, i64 0)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_107 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.17)
  br label %bb15

bb15:                                             ; preds = %bb14
  %call_108 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.18)
  br label %bb16

bb16:                                             ; preds = %bb15
  store { double, double } zeroinitializer, ptr %alloca_count_109, align 8
  store { i8, i8, i8 } { i8 -1, i8 0, i8 0 }, ptr %alloca_count_111, align 1
  %call_113 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.19)
  br label %bb17

bb17:                                             ; preds = %bb16
  store i64 16, ptr %alloca_count_114, align 8
  %load_116 = load i64, ptr %alloca_count_114, align 8
  %call_117 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.20, i64 %load_116)
  br label %bb18

bb18:                                             ; preds = %bb17
  store i64 24, ptr %alloca_count_118, align 8
  %load_120 = load i64, ptr %alloca_count_118, align 8
  %call_121 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.21, i64 %load_120)
  br label %bb19

bb19:                                             ; preds = %bb18
  store i64 40, ptr %alloca_count_122, align 8
  %load_124 = load i64, ptr %alloca_count_122, align 8
  %call_125 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.22, i64 %load_124)
  br label %bb20

bb20:                                             ; preds = %bb19
  %call_126 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.23)
  br label %bb21

bb21:                                             ; preds = %bb20
  %load_128 = load double, ptr %alloca_count_109, align 8
  %gep_130 = getelementptr inbounds i8, ptr %alloca_count_109, i64 8
  %load_132 = load double, ptr %gep_130, align 8
  %call_133 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.24, double %load_128, double %load_132)
  br label %bb22

bb22:                                             ; preds = %bb21
  %load_135 = load i8, ptr %alloca_count_111, align 1
  %zext = zext i8 %load_135 to i32
  %gep_138 = getelementptr inbounds i8, ptr %alloca_count_111, i64 1
  %load_140 = load i8, ptr %gep_138, align 1
  %zext1 = zext i8 %load_140 to i32
  %gep_143 = getelementptr inbounds i8, ptr %alloca_count_111, i64 2
  %load_145 = load i8, ptr %gep_143, align 1
  %zext2 = zext i8 %load_145 to i32
  %call_147 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.25, i32 %zext, i32 %zext1, i32 %zext2)
  br label %bb23

bb23:                                             ; preds = %bb22
  %call_148 = call i32 (ptr, ...) @printf(ptr @.str.04_struct_introspection.26)
  br label %bb24

bb24:                                             ; preds = %bb23
  ret i32 0
}

define internal void @TestCase__run(ptr %0) {
bb0:
  ret void
}

declare i32 @printf(ptr, ...)

declare i32 @__gxx_personality_v0(...)
