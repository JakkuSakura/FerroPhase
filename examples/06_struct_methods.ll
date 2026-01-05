; ModuleID = '06_struct_methods'
source_filename = "06_struct_methods"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.06_struct_methods.0 = constant [37 x i8] c"\F0\9F\93\98 Tutorial: 06_struct_methods.fp\0A\00"
@.str.06_struct_methods.1 = constant [45 x i8] c"\F0\9F\A7\AD Focus: Struct methods and field access\0A\00"
@.str.06_struct_methods.2 = constant [46 x i8] c"\F0\9F\A7\AA What to look for: labeled outputs below\0A\00"
@.str.06_struct_methods.3 = constant [39 x i8] c"\E2\9C\85 Expectation: outputs match labels\0A\00"
@.str.06_struct_methods.4 = constant [2 x i8] c"\0A\00"
@.str.06_struct_methods.5 = constant [27 x i8] c"=== Struct Operations ===\0A\00"
@.str.06_struct_methods.6 = constant [19 x i8] c"p1 = (%lld, %lld)\0A\00"
@.str.06_struct_methods.7 = constant [19 x i8] c"p2 = (%lld, %lld)\0A\00"
@.str.06_struct_methods.8 = constant [35 x i8] c"p1 after translate = (%lld, %lld)\0A\00"
@.str.06_struct_methods.9 = constant [27 x i8] c"Distance\C2\B2(p1, p2) = %lld\0A\00"
@.str.06_struct_methods.10 = constant [23 x i8] c"Rectangle: %lld\C3\97%lld\0A\00"
@.str.06_struct_methods.11 = constant [15 x i8] c"  area = %lld\0A\00"
@.str.06_struct_methods.12 = constant [20 x i8] c"  perimeter = %lld\0A\00"
@.str.06_struct_methods.13 = constant [18 x i8] c"  is_square = %d\0A\00"

define internal { i64, i64 } @Point__new(i64 %0, i64 %1) {
bb0:
  %alloca_0 = alloca { i64, i64 }, align 8
  %alloca_count_0 = alloca { i64, i64 }, align 8
  %insertvalue_1 = insertvalue { i64, i64 } undef, i64 %0, 0
  %insertvalue_2 = insertvalue { i64, i64 } %insertvalue_1, i64 %1, 1
  store { i64, i64 } %insertvalue_2, ptr %alloca_count_0, align 8
  %load_4 = load { i64, i64 }, ptr %alloca_count_0, align 8
  ret { i64, i64 } %load_4
}

define internal void @Point__translate(ptr %0, i64 %1, i64 %2) {
bb0:
  %alloca_5 = alloca ptr, align 8
  %alloca_count_5 = alloca ptr, align 8
  store ptr %0, ptr %alloca_count_5, align 8
  %load_7 = load ptr, ptr %alloca_count_5, align 8
  %load_9 = load ptr, ptr %alloca_count_5, align 8
  %load_11 = load i64, ptr %load_9, align 8
  %iop_12 = add i64 %load_11, %1
  store i64 %iop_12, ptr %load_7, align 8
  %load_14 = load ptr, ptr %alloca_count_5, align 8
  %gep_16 = getelementptr inbounds i8, ptr %load_14, i64 8
  %load_18 = load ptr, ptr %alloca_count_5, align 8
  %gep_20 = getelementptr inbounds i8, ptr %load_18, i64 8
  %load_22 = load i64, ptr %gep_20, align 8
  %iop_23 = add i64 %load_22, %2
  store i64 %iop_23, ptr %gep_16, align 8
  ret void
}

define internal i64 @Point__distance2(ptr %0, ptr %1) {
bb0:
  %alloca_55 = alloca i64, align 8
  %alloca_count_55 = alloca i64, align 8
  %alloca_50 = alloca i64, align 8
  %alloca_count_50 = alloca i64, align 8
  %alloca_47 = alloca i64, align 8
  %alloca_count_47 = alloca i64, align 8
  %alloca_36 = alloca i64, align 8
  %alloca_count_36 = alloca i64, align 8
  %alloca_33 = alloca i64, align 8
  %alloca_count_33 = alloca i64, align 8
  %alloca_26 = alloca i64, align 8
  %alloca_count_26 = alloca i64, align 8
  %alloca_25 = alloca i64, align 8
  %alloca_count_25 = alloca i64, align 8
  %load_28 = load i64, ptr %0, align 8
  %load_30 = load i64, ptr %1, align 8
  %iop_31 = sub i64 %load_28, %load_30
  store i64 %iop_31, ptr %alloca_count_26, align 8
  %load_34 = load i64, ptr %alloca_count_26, align 8
  store i64 %load_34, ptr %alloca_count_33, align 8
  %gep_38 = getelementptr inbounds i8, ptr %0, i64 8
  %load_40 = load i64, ptr %gep_38, align 8
  %gep_42 = getelementptr inbounds i8, ptr %1, i64 8
  %load_44 = load i64, ptr %gep_42, align 8
  %iop_45 = sub i64 %load_40, %load_44
  store i64 %iop_45, ptr %alloca_count_36, align 8
  %load_48 = load i64, ptr %alloca_count_36, align 8
  store i64 %load_48, ptr %alloca_count_47, align 8
  %load_51 = load i64, ptr %alloca_count_33, align 8
  %load_52 = load i64, ptr %alloca_count_33, align 8
  %iop_53 = mul i64 %load_51, %load_52
  store i64 %iop_53, ptr %alloca_count_50, align 8
  %load_56 = load i64, ptr %alloca_count_47, align 8
  %load_57 = load i64, ptr %alloca_count_47, align 8
  %iop_58 = mul i64 %load_56, %load_57
  store i64 %iop_58, ptr %alloca_count_55, align 8
  %load_60 = load i64, ptr %alloca_count_50, align 8
  %load_61 = load i64, ptr %alloca_count_55, align 8
  %iop_62 = add i64 %load_60, %load_61
  store i64 %iop_62, ptr %alloca_count_25, align 8
  %load_64 = load i64, ptr %alloca_count_25, align 8
  ret i64 %load_64
}

define internal { i64, i64 } @Rectangle__new(i64 %0, i64 %1) {
bb0:
  %alloca_65 = alloca { i64, i64 }, align 8
  %alloca_count_65 = alloca { i64, i64 }, align 8
  %insertvalue_66 = insertvalue { i64, i64 } undef, i64 %0, 0
  %insertvalue_67 = insertvalue { i64, i64 } %insertvalue_66, i64 %1, 1
  store { i64, i64 } %insertvalue_67, ptr %alloca_count_65, align 8
  %load_69 = load { i64, i64 }, ptr %alloca_count_65, align 8
  ret { i64, i64 } %load_69
}

define internal i64 @Rectangle__area(ptr %0) {
bb0:
  %alloca_70 = alloca i64, align 8
  %alloca_count_70 = alloca i64, align 8
  %load_72 = load i64, ptr %0, align 8
  %gep_74 = getelementptr inbounds i8, ptr %0, i64 8
  %load_76 = load i64, ptr %gep_74, align 8
  %iop_77 = mul i64 %load_72, %load_76
  store i64 %iop_77, ptr %alloca_count_70, align 8
  %load_79 = load i64, ptr %alloca_count_70, align 8
  ret i64 %load_79
}

define internal i64 @Rectangle__perimeter(ptr %0) {
bb0:
  %alloca_81 = alloca i64, align 8
  %alloca_count_81 = alloca i64, align 8
  %alloca_80 = alloca i64, align 8
  %alloca_count_80 = alloca i64, align 8
  %load_83 = load i64, ptr %0, align 8
  %gep_85 = getelementptr inbounds i8, ptr %0, i64 8
  %load_87 = load i64, ptr %gep_85, align 8
  %iop_88 = add i64 %load_83, %load_87
  store i64 %iop_88, ptr %alloca_count_81, align 8
  %load_90 = load i64, ptr %alloca_count_81, align 8
  %iop_91 = mul i64 2, %load_90
  store i64 %iop_91, ptr %alloca_count_80, align 8
  %load_93 = load i64, ptr %alloca_count_80, align 8
  ret i64 %load_93
}

define internal i1 @Rectangle__is_square(ptr %0) {
bb0:
  %alloca_94 = alloca i1, align 1
  %alloca_count_94 = alloca i1, align 1
  %load_96 = load i64, ptr %0, align 8
  %gep_98 = getelementptr inbounds i8, ptr %0, i64 8
  %load_100 = load i64, ptr %gep_98, align 8
  %icmp_101 = icmp eq i64 %load_96, %load_100
  store i1 %icmp_101, ptr %alloca_count_94, align 1
  %load_103 = load i1, ptr %alloca_count_94, align 1
  ret i1 %load_103
}

define i32 @main() {
bb0:
  %alloca_194 = alloca { i64, i64 }, align 8
  %alloca_count_194 = alloca { i64, i64 }, align 8
  %alloca_193 = alloca ptr, align 8
  %alloca_count_193 = alloca ptr, align 8
  %alloca_187 = alloca { i64, i64 }, align 8
  %alloca_count_187 = alloca { i64, i64 }, align 8
  %alloca_186 = alloca ptr, align 8
  %alloca_count_186 = alloca ptr, align 8
  %alloca_180 = alloca { i64, i64 }, align 8
  %alloca_count_180 = alloca { i64, i64 }, align 8
  %alloca_179 = alloca ptr, align 8
  %alloca_count_179 = alloca ptr, align 8
  %alloca_172 = alloca { i64, i64 }, align 8
  %alloca_count_172 = alloca { i64, i64 }, align 8
  %alloca_168 = alloca { i64, i64 }, align 8
  %alloca_count_168 = alloca { i64, i64 }, align 8
  %alloca_160 = alloca { i64, i64 }, align 8
  %alloca_count_160 = alloca { i64, i64 }, align 8
  %alloca_159 = alloca ptr, align 8
  %alloca_count_159 = alloca ptr, align 8
  %alloca_156 = alloca { i64, i64 }, align 8
  %alloca_count_156 = alloca { i64, i64 }, align 8
  %alloca_155 = alloca ptr, align 8
  %alloca_count_155 = alloca ptr, align 8
  %alloca_148 = alloca { i64, i64 }, align 8
  %alloca_count_148 = alloca { i64, i64 }, align 8
  %alloca_144 = alloca { i64, i64 }, align 8
  %alloca_count_144 = alloca { i64, i64 }, align 8
  %alloca_138 = alloca i64, align 8
  %alloca_count_138 = alloca i64, align 8
  %alloca_135 = alloca { i64, i64 }, align 8
  %alloca_count_135 = alloca { i64, i64 }, align 8
  %alloca_134 = alloca ptr, align 8
  %alloca_count_134 = alloca ptr, align 8
  %alloca_127 = alloca { i64, i64 }, align 8
  %alloca_count_127 = alloca { i64, i64 }, align 8
  %alloca_123 = alloca { i64, i64 }, align 8
  %alloca_count_123 = alloca { i64, i64 }, align 8
  %alloca_116 = alloca { i64, i64 }, align 8
  %alloca_count_116 = alloca { i64, i64 }, align 8
  %alloca_112 = alloca { i64, i64 }, align 8
  %alloca_count_112 = alloca { i64, i64 }, align 8
  %call_104 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_105 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.1)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_106 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.2)
  br label %bb3

bb3:                                              ; preds = %bb2
  %call_107 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.3)
  br label %bb4

bb4:                                              ; preds = %bb3
  %call_108 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.4)
  br label %bb5

bb5:                                              ; preds = %bb4
  %call_109 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.5)
  br label %bb6

bb6:                                              ; preds = %bb5
  %call_110 = call { i64, i64 } @Point__new(i64 10, i64 20)
  br label %bb7

bb7:                                              ; preds = %bb6
  %call_111 = call { i64, i64 } @Point__new(i64 5, i64 15)
  br label %bb8

bb8:                                              ; preds = %bb7
  store { i64, i64 } %call_110, ptr %alloca_count_112, align 8
  %load_115 = load i64, ptr %alloca_count_112, align 8
  store { i64, i64 } %call_110, ptr %alloca_count_116, align 8
  %gep_119 = getelementptr inbounds i8, ptr %alloca_count_116, i64 8
  %load_121 = load i64, ptr %gep_119, align 8
  %call_122 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.6, i64 %load_115, i64 %load_121)
  br label %bb9

bb9:                                              ; preds = %bb8
  store { i64, i64 } %call_111, ptr %alloca_count_123, align 8
  %load_126 = load i64, ptr %alloca_count_123, align 8
  store { i64, i64 } %call_111, ptr %alloca_count_127, align 8
  %gep_130 = getelementptr inbounds i8, ptr %alloca_count_127, i64 8
  %load_132 = load i64, ptr %gep_130, align 8
  %call_133 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.7, i64 %load_126, i64 %load_132)
  br label %bb10

bb10:                                             ; preds = %bb9
  store { i64, i64 } %call_110, ptr %alloca_count_135, align 8
  store ptr %alloca_count_135, ptr %alloca_count_134, align 8
  store i64 -4, ptr %alloca_count_138, align 8
  %load_141 = load ptr, ptr %alloca_count_134, align 8
  %load_142 = load i64, ptr %alloca_count_138, align 8
  call void @Point__translate(ptr %load_141, i64 3, i64 %load_142)
  br label %bb11

bb11:                                             ; preds = %bb10
  store { i64, i64 } %call_110, ptr %alloca_count_144, align 8
  %load_147 = load i64, ptr %alloca_count_144, align 8
  store { i64, i64 } %call_110, ptr %alloca_count_148, align 8
  %gep_151 = getelementptr inbounds i8, ptr %alloca_count_148, i64 8
  %load_153 = load i64, ptr %gep_151, align 8
  %call_154 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.8, i64 %load_147, i64 %load_153)
  br label %bb12

bb12:                                             ; preds = %bb11
  store { i64, i64 } %call_110, ptr %alloca_count_156, align 8
  store ptr %alloca_count_156, ptr %alloca_count_155, align 8
  store { i64, i64 } %call_111, ptr %alloca_count_160, align 8
  store ptr %alloca_count_160, ptr %alloca_count_159, align 8
  %load_163 = load ptr, ptr %alloca_count_155, align 8
  %load_164 = load ptr, ptr %alloca_count_159, align 8
  %call_165 = call i64 @Point__distance2(ptr %load_163, ptr %load_164)
  br label %bb13

bb13:                                             ; preds = %bb12
  %call_166 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.9, i64 %call_165)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_167 = call { i64, i64 } @Rectangle__new(i64 10, i64 5)
  br label %bb15

bb15:                                             ; preds = %bb14
  store { i64, i64 } %call_167, ptr %alloca_count_168, align 8
  %load_171 = load i64, ptr %alloca_count_168, align 8
  store { i64, i64 } %call_167, ptr %alloca_count_172, align 8
  %gep_175 = getelementptr inbounds i8, ptr %alloca_count_172, i64 8
  %load_177 = load i64, ptr %gep_175, align 8
  %call_178 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.10, i64 %load_171, i64 %load_177)
  br label %bb16

bb16:                                             ; preds = %bb15
  store { i64, i64 } %call_167, ptr %alloca_count_180, align 8
  store ptr %alloca_count_180, ptr %alloca_count_179, align 8
  %load_183 = load ptr, ptr %alloca_count_179, align 8
  %call_184 = call i64 @Rectangle__area(ptr %load_183)
  br label %bb17

bb17:                                             ; preds = %bb16
  %call_185 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.11, i64 %call_184)
  br label %bb18

bb18:                                             ; preds = %bb17
  store { i64, i64 } %call_167, ptr %alloca_count_187, align 8
  store ptr %alloca_count_187, ptr %alloca_count_186, align 8
  %load_190 = load ptr, ptr %alloca_count_186, align 8
  %call_191 = call i64 @Rectangle__perimeter(ptr %load_190)
  br label %bb19

bb19:                                             ; preds = %bb18
  %call_192 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.12, i64 %call_191)
  br label %bb20

bb20:                                             ; preds = %bb19
  store { i64, i64 } %call_167, ptr %alloca_count_194, align 8
  store ptr %alloca_count_194, ptr %alloca_count_193, align 8
  %load_197 = load ptr, ptr %alloca_count_193, align 8
  %call_198 = call i1 @Rectangle__is_square(ptr %load_197)
  br label %bb21

bb21:                                             ; preds = %bb20
  %zext = zext i1 %call_198 to i32
  %call_200 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.13, i32 %zext)
  br label %bb22

bb22:                                             ; preds = %bb21
  ret i32 0
}

declare i32 @printf(ptr, ...)
