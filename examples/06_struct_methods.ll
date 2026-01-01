; ModuleID = '06_struct_methods'
source_filename = "06_struct_methods"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-darwin25.0.0"

@.str.06_struct_methods.0 = private unnamed_addr constant [27 x i8] c"=== Struct Operations ===\0A\00", align 1
@.str.06_struct_methods.1 = private unnamed_addr constant [19 x i8] c"p1 = (%lld, %lld)\0A\00", align 1
@.str.06_struct_methods.2 = private unnamed_addr constant [19 x i8] c"p2 = (%lld, %lld)\0A\00", align 1
@.str.06_struct_methods.3 = private unnamed_addr constant [35 x i8] c"p1 after translate = (%lld, %lld)\0A\00", align 1
@.str.06_struct_methods.4 = private unnamed_addr constant [27 x i8] c"Distance\C2\B2(p1, p2) = %lld\0A\00", align 1
@.str.06_struct_methods.5 = private unnamed_addr constant [23 x i8] c"Rectangle: %lld\C3\97%lld\0A\00", align 1
@.str.06_struct_methods.6 = private unnamed_addr constant [15 x i8] c"  area = %lld\0A\00", align 1
@.str.06_struct_methods.7 = private unnamed_addr constant [20 x i8] c"  perimeter = %lld\0A\00", align 1
@.str.06_struct_methods.8 = private unnamed_addr constant [18 x i8] c"  is_square = %d\0A\00", align 1

define internal { i64, i64 } @Point__new(i64 %0, i64 %1) {
bb0:
  %alloca_0 = alloca { i64, i64 }, align 8
  %alloca_count_0 = alloca { i64, i64 }, align 8
  %insertvalue_1 = insertvalue { i64, i64 } undef, i64 %0, 0
  %insertvalue_2 = insertvalue { i64, i64 } %insertvalue_1, i64 %1, 1
  %insertvalue_3 = insertvalue { i64, i64 } undef, i64 %0, 0
  %insertvalue_4 = insertvalue { i64, i64 } %insertvalue_3, i64 %1, 1
  store { i64, i64 } %insertvalue_4, ptr %alloca_count_0, align 8
  %load_6 = load { i64, i64 }, ptr %alloca_count_0, align 8
  ret { i64, i64 } %load_6
}

define internal void @Point__translate(ptr %0, i64 %1, i64 %2) {
bb0:
  %load_9 = load i64, ptr %0, align 8
  %iop_10 = add i64 %load_9, %1
  store i64 %iop_10, ptr %0, align 8
  %gep_13 = getelementptr inbounds i8, ptr %0, i64 8
  %gep_16 = getelementptr inbounds i8, ptr %0, i64 8
  %load_18 = load i64, ptr %gep_16, align 8
  %iop_19 = add i64 %load_18, %2
  store i64 %iop_19, ptr %gep_13, align 8
  ret void
}

define internal i64 @Point__distance2(ptr %0, ptr %1) {
bb0:
  %alloca_66 = alloca i64, align 8
  %alloca_count_66 = alloca i64, align 8
  %alloca_61 = alloca i64, align 8
  %alloca_count_61 = alloca i64, align 8
  %alloca_56 = alloca i64, align 8
  %alloca_count_56 = alloca i64, align 8
  %alloca_51 = alloca i64, align 8
  %alloca_count_51 = alloca i64, align 8
  %alloca_46 = alloca i64, align 8
  %alloca_count_46 = alloca i64, align 8
  %alloca_43 = alloca i64, align 8
  %alloca_count_43 = alloca i64, align 8
  %alloca_32 = alloca i64, align 8
  %alloca_count_32 = alloca i64, align 8
  %alloca_29 = alloca i64, align 8
  %alloca_count_29 = alloca i64, align 8
  %alloca_22 = alloca i64, align 8
  %alloca_count_22 = alloca i64, align 8
  %alloca_21 = alloca i64, align 8
  %alloca_count_21 = alloca i64, align 8
  %load_24 = load i64, ptr %0, align 8
  %load_26 = load i64, ptr %1, align 8
  %iop_27 = sub i64 %load_24, %load_26
  store i64 %iop_27, ptr %alloca_count_22, align 8
  %load_30 = load i64, ptr %alloca_count_22, align 8
  store i64 %load_30, ptr %alloca_count_29, align 8
  %gep_34 = getelementptr inbounds i8, ptr %0, i64 8
  %load_36 = load i64, ptr %gep_34, align 8
  %gep_38 = getelementptr inbounds i8, ptr %1, i64 8
  %load_40 = load i64, ptr %gep_38, align 8
  %iop_41 = sub i64 %load_36, %load_40
  store i64 %iop_41, ptr %alloca_count_32, align 8
  %load_44 = load i64, ptr %alloca_count_32, align 8
  store i64 %load_44, ptr %alloca_count_43, align 8
  %load_47 = load i64, ptr %alloca_count_29, align 8
  %load_48 = load i64, ptr %alloca_count_29, align 8
  %iop_49 = mul i64 %load_47, %load_48
  store i64 %iop_49, ptr %alloca_count_46, align 8
  %load_52 = load i64, ptr %alloca_count_43, align 8
  %load_53 = load i64, ptr %alloca_count_43, align 8
  %iop_54 = mul i64 %load_52, %load_53
  store i64 %iop_54, ptr %alloca_count_51, align 8
  %load_57 = load i64, ptr %alloca_count_46, align 8
  %load_58 = load i64, ptr %alloca_count_51, align 8
  %iop_59 = add i64 %load_57, %load_58
  store i64 %iop_59, ptr %alloca_count_56, align 8
  %load_62 = load i64, ptr %alloca_count_29, align 8
  %load_63 = load i64, ptr %alloca_count_29, align 8
  %iop_64 = mul i64 %load_62, %load_63
  store i64 %iop_64, ptr %alloca_count_61, align 8
  %load_67 = load i64, ptr %alloca_count_43, align 8
  %load_68 = load i64, ptr %alloca_count_43, align 8
  %iop_69 = mul i64 %load_67, %load_68
  store i64 %iop_69, ptr %alloca_count_66, align 8
  %load_71 = load i64, ptr %alloca_count_61, align 8
  %load_72 = load i64, ptr %alloca_count_66, align 8
  %iop_73 = add i64 %load_71, %load_72
  store i64 %iop_73, ptr %alloca_count_21, align 8
  %load_75 = load i64, ptr %alloca_count_21, align 8
  ret i64 %load_75
}

define internal { i64, i64 } @Rectangle__new(i64 %0, i64 %1) {
bb0:
  %alloca_76 = alloca { i64, i64 }, align 8
  %alloca_count_76 = alloca { i64, i64 }, align 8
  %insertvalue_77 = insertvalue { i64, i64 } undef, i64 %0, 0
  %insertvalue_78 = insertvalue { i64, i64 } %insertvalue_77, i64 %1, 1
  %insertvalue_79 = insertvalue { i64, i64 } undef, i64 %0, 0
  %insertvalue_80 = insertvalue { i64, i64 } %insertvalue_79, i64 %1, 1
  store { i64, i64 } %insertvalue_80, ptr %alloca_count_76, align 8
  %load_82 = load { i64, i64 }, ptr %alloca_count_76, align 8
  ret { i64, i64 } %load_82
}

define internal i64 @Rectangle__area(ptr %0) {
bb0:
  %alloca_84 = alloca i64, align 8
  %alloca_count_84 = alloca i64, align 8
  %alloca_83 = alloca i64, align 8
  %alloca_count_83 = alloca i64, align 8
  %load_86 = load i64, ptr %0, align 8
  %gep_88 = getelementptr inbounds i8, ptr %0, i64 8
  %load_90 = load i64, ptr %gep_88, align 8
  %iop_91 = mul i64 %load_86, %load_90
  store i64 %iop_91, ptr %alloca_count_84, align 8
  %load_94 = load i64, ptr %0, align 8
  %gep_96 = getelementptr inbounds i8, ptr %0, i64 8
  %load_98 = load i64, ptr %gep_96, align 8
  %iop_99 = mul i64 %load_94, %load_98
  store i64 %iop_99, ptr %alloca_count_83, align 8
  %load_101 = load i64, ptr %alloca_count_83, align 8
  ret i64 %load_101
}

define internal i64 @Rectangle__perimeter(ptr %0) {
bb0:
  %alloca_116 = alloca i64, align 8
  %alloca_count_116 = alloca i64, align 8
  %alloca_112 = alloca i64, align 8
  %alloca_count_112 = alloca i64, align 8
  %alloca_103 = alloca i64, align 8
  %alloca_count_103 = alloca i64, align 8
  %alloca_102 = alloca i64, align 8
  %alloca_count_102 = alloca i64, align 8
  %load_105 = load i64, ptr %0, align 8
  %gep_107 = getelementptr inbounds i8, ptr %0, i64 8
  %load_109 = load i64, ptr %gep_107, align 8
  %iop_110 = add i64 %load_105, %load_109
  store i64 %iop_110, ptr %alloca_count_103, align 8
  %load_113 = load i64, ptr %alloca_count_103, align 8
  %iop_114 = mul i64 2, %load_113
  store i64 %iop_114, ptr %alloca_count_112, align 8
  %load_118 = load i64, ptr %0, align 8
  %gep_120 = getelementptr inbounds i8, ptr %0, i64 8
  %load_122 = load i64, ptr %gep_120, align 8
  %iop_123 = add i64 %load_118, %load_122
  store i64 %iop_123, ptr %alloca_count_116, align 8
  %load_125 = load i64, ptr %alloca_count_116, align 8
  %iop_126 = mul i64 2, %load_125
  store i64 %iop_126, ptr %alloca_count_102, align 8
  %load_128 = load i64, ptr %alloca_count_102, align 8
  ret i64 %load_128
}

define internal i1 @Rectangle__is_square(ptr %0) {
bb0:
  %alloca_130 = alloca i1, align 1
  %alloca_count_130 = alloca i1, align 1
  %alloca_129 = alloca i1, align 1
  %alloca_count_129 = alloca i1, align 1
  %load_132 = load i64, ptr %0, align 8
  %gep_134 = getelementptr inbounds i8, ptr %0, i64 8
  %load_136 = load i64, ptr %gep_134, align 8
  %icmp_137 = icmp eq i64 %load_132, %load_136
  store i1 %icmp_137, ptr %alloca_count_130, align 1
  %load_140 = load i64, ptr %0, align 8
  %gep_142 = getelementptr inbounds i8, ptr %0, i64 8
  %load_144 = load i64, ptr %gep_142, align 8
  %icmp_145 = icmp eq i64 %load_140, %load_144
  store i1 %icmp_145, ptr %alloca_count_129, align 1
  %load_147 = load i1, ptr %alloca_count_129, align 1
  ret i1 %load_147
}

define i32 @main() {
bb0:
  %alloca_217 = alloca ptr, align 8
  %alloca_count_217 = alloca ptr, align 8
  %alloca_213 = alloca ptr, align 8
  %alloca_count_213 = alloca ptr, align 8
  %alloca_209 = alloca ptr, align 8
  %alloca_count_209 = alloca ptr, align 8
  %alloca_202 = alloca { i64, i64 }, align 8
  %alloca_count_202 = alloca { i64, i64 }, align 8
  %alloca_198 = alloca { i64, i64 }, align 8
  %alloca_count_198 = alloca { i64, i64 }, align 8
  %alloca_193 = alloca ptr, align 8
  %alloca_count_193 = alloca ptr, align 8
  %alloca_191 = alloca ptr, align 8
  %alloca_count_191 = alloca ptr, align 8
  %alloca_184 = alloca { i64, i64 }, align 8
  %alloca_count_184 = alloca { i64, i64 }, align 8
  %alloca_180 = alloca { i64, i64 }, align 8
  %alloca_count_180 = alloca { i64, i64 }, align 8
  %alloca_175 = alloca i64, align 8
  %alloca_count_175 = alloca i64, align 8
  %alloca_173 = alloca ptr, align 8
  %alloca_count_173 = alloca ptr, align 8
  %alloca_166 = alloca { i64, i64 }, align 8
  %alloca_count_166 = alloca { i64, i64 }, align 8
  %alloca_162 = alloca { i64, i64 }, align 8
  %alloca_count_162 = alloca { i64, i64 }, align 8
  %alloca_155 = alloca { i64, i64 }, align 8
  %alloca_count_155 = alloca { i64, i64 }, align 8
  %alloca_151 = alloca { i64, i64 }, align 8
  %alloca_count_151 = alloca { i64, i64 }, align 8
  %call_148 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_149 = call { i64, i64 } @Point__new(i64 10, i64 20)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_150 = call { i64, i64 } @Point__new(i64 5, i64 15)
  br label %bb3

bb3:                                              ; preds = %bb2
  store { i64, i64 } %call_149, ptr %alloca_count_151, align 8
  %load_154 = load i64, ptr %alloca_count_151, align 8
  store { i64, i64 } %call_149, ptr %alloca_count_155, align 8
  %gep_158 = getelementptr inbounds i8, ptr %alloca_count_155, i64 8
  %load_160 = load i64, ptr %gep_158, align 8
  %call_161 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.1, i64 %load_154, i64 %load_160)
  br label %bb4

bb4:                                              ; preds = %bb3
  store { i64, i64 } %call_150, ptr %alloca_count_162, align 8
  %load_165 = load i64, ptr %alloca_count_162, align 8
  store { i64, i64 } %call_150, ptr %alloca_count_166, align 8
  %gep_169 = getelementptr inbounds i8, ptr %alloca_count_166, i64 8
  %load_171 = load i64, ptr %gep_169, align 8
  %call_172 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.2, i64 %load_165, i64 %load_171)
  br label %bb5

bb5:                                              ; preds = %bb4
  store { i64, i64 } %call_149, ptr %alloca_count_173, align 8
  store i64 -4, ptr %alloca_count_175, align 8
  %load_178 = load i64, ptr %alloca_count_175, align 8
  call void @Point__translate(ptr %alloca_count_173, i64 3, i64 %load_178)
  br label %bb6

bb6:                                              ; preds = %bb5
  store { i64, i64 } %call_149, ptr %alloca_count_180, align 8
  %load_183 = load i64, ptr %alloca_count_180, align 8
  store { i64, i64 } %call_149, ptr %alloca_count_184, align 8
  %gep_187 = getelementptr inbounds i8, ptr %alloca_count_184, i64 8
  %load_189 = load i64, ptr %gep_187, align 8
  %call_190 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.3, i64 %load_183, i64 %load_189)
  br label %bb7

bb7:                                              ; preds = %bb6
  store { i64, i64 } %call_149, ptr %alloca_count_191, align 8
  store { i64, i64 } %call_150, ptr %alloca_count_193, align 8
  %call_195 = call i64 @Point__distance2(ptr %alloca_count_191, ptr %alloca_count_193)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_196 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.4, i64 %call_195)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_197 = call { i64, i64 } @Rectangle__new(i64 10, i64 5)
  br label %bb10

bb10:                                             ; preds = %bb9
  store { i64, i64 } %call_197, ptr %alloca_count_198, align 8
  %load_201 = load i64, ptr %alloca_count_198, align 8
  store { i64, i64 } %call_197, ptr %alloca_count_202, align 8
  %gep_205 = getelementptr inbounds i8, ptr %alloca_count_202, i64 8
  %load_207 = load i64, ptr %gep_205, align 8
  %call_208 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.5, i64 %load_201, i64 %load_207)
  br label %bb11

bb11:                                             ; preds = %bb10
  store { i64, i64 } %call_197, ptr %alloca_count_209, align 8
  %call_211 = call i64 @Rectangle__area(ptr %alloca_count_209)
  br label %bb12

bb12:                                             ; preds = %bb11
  %call_212 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.6, i64 %call_211)
  br label %bb13

bb13:                                             ; preds = %bb12
  store { i64, i64 } %call_197, ptr %alloca_count_213, align 8
  %call_215 = call i64 @Rectangle__perimeter(ptr %alloca_count_213)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_216 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.7, i64 %call_215)
  br label %bb15

bb15:                                             ; preds = %bb14
  store { i64, i64 } %call_197, ptr %alloca_count_217, align 8
  %call_219 = call i1 @Rectangle__is_square(ptr %alloca_count_217)
  br label %bb16

bb16:                                             ; preds = %bb15
  %zext = zext i1 %call_219 to i32
  %call_221 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.8, i32 %zext)
  br label %bb17

bb17:                                             ; preds = %bb16
  ret i32 0
}

declare i32 @printf(ptr, ...)
