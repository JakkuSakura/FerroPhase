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
  store { i64, i64 } %insertvalue_2, ptr %alloca_count_0, align 8
  %load_4 = load { i64, i64 }, ptr %alloca_count_0, align 8
  ret { i64, i64 } %load_4
}

define internal void @Point__translate(ptr %0, i64 %1, i64 %2) {
bb0:
  %load_7 = load i64, ptr %0, align 8
  %iop_8 = add i64 %load_7, %1
  store i64 %iop_8, ptr %0, align 8
  %gep_11 = getelementptr inbounds i8, ptr %0, i64 8
  %gep_14 = getelementptr inbounds i8, ptr %0, i64 8
  %load_16 = load i64, ptr %gep_14, align 8
  %iop_17 = add i64 %load_16, %2
  store i64 %iop_17, ptr %gep_11, align 8
  ret void
}

define internal i64 @Point__distance2(ptr %0, ptr %1) {
bb0:
  %alloca_49 = alloca i64, align 8
  %alloca_count_49 = alloca i64, align 8
  %alloca_44 = alloca i64, align 8
  %alloca_count_44 = alloca i64, align 8
  %alloca_41 = alloca i64, align 8
  %alloca_count_41 = alloca i64, align 8
  %alloca_30 = alloca i64, align 8
  %alloca_count_30 = alloca i64, align 8
  %alloca_27 = alloca i64, align 8
  %alloca_count_27 = alloca i64, align 8
  %alloca_20 = alloca i64, align 8
  %alloca_count_20 = alloca i64, align 8
  %alloca_19 = alloca i64, align 8
  %alloca_count_19 = alloca i64, align 8
  %load_22 = load i64, ptr %0, align 8
  %load_24 = load i64, ptr %1, align 8
  %iop_25 = sub i64 %load_22, %load_24
  store i64 %iop_25, ptr %alloca_count_20, align 8
  %load_28 = load i64, ptr %alloca_count_20, align 8
  store i64 %load_28, ptr %alloca_count_27, align 8
  %gep_32 = getelementptr inbounds i8, ptr %0, i64 8
  %load_34 = load i64, ptr %gep_32, align 8
  %gep_36 = getelementptr inbounds i8, ptr %1, i64 8
  %load_38 = load i64, ptr %gep_36, align 8
  %iop_39 = sub i64 %load_34, %load_38
  store i64 %iop_39, ptr %alloca_count_30, align 8
  %load_42 = load i64, ptr %alloca_count_30, align 8
  store i64 %load_42, ptr %alloca_count_41, align 8
  %load_45 = load i64, ptr %alloca_count_27, align 8
  %load_46 = load i64, ptr %alloca_count_27, align 8
  %iop_47 = mul i64 %load_45, %load_46
  store i64 %iop_47, ptr %alloca_count_44, align 8
  %load_50 = load i64, ptr %alloca_count_41, align 8
  %load_51 = load i64, ptr %alloca_count_41, align 8
  %iop_52 = mul i64 %load_50, %load_51
  store i64 %iop_52, ptr %alloca_count_49, align 8
  %load_54 = load i64, ptr %alloca_count_44, align 8
  %load_55 = load i64, ptr %alloca_count_49, align 8
  %iop_56 = add i64 %load_54, %load_55
  store i64 %iop_56, ptr %alloca_count_19, align 8
  %load_58 = load i64, ptr %alloca_count_19, align 8
  ret i64 %load_58
}

define internal { i64, i64 } @Rectangle__new(i64 %0, i64 %1) {
bb0:
  %alloca_59 = alloca { i64, i64 }, align 8
  %alloca_count_59 = alloca { i64, i64 }, align 8
  %insertvalue_60 = insertvalue { i64, i64 } undef, i64 %0, 0
  %insertvalue_61 = insertvalue { i64, i64 } %insertvalue_60, i64 %1, 1
  store { i64, i64 } %insertvalue_61, ptr %alloca_count_59, align 8
  %load_63 = load { i64, i64 }, ptr %alloca_count_59, align 8
  ret { i64, i64 } %load_63
}

define internal i64 @Rectangle__area(ptr %0) {
bb0:
  %alloca_64 = alloca i64, align 8
  %alloca_count_64 = alloca i64, align 8
  %load_66 = load i64, ptr %0, align 8
  %gep_68 = getelementptr inbounds i8, ptr %0, i64 8
  %load_70 = load i64, ptr %gep_68, align 8
  %iop_71 = mul i64 %load_66, %load_70
  store i64 %iop_71, ptr %alloca_count_64, align 8
  %load_73 = load i64, ptr %alloca_count_64, align 8
  ret i64 %load_73
}

define internal i64 @Rectangle__perimeter(ptr %0) {
bb0:
  %alloca_75 = alloca i64, align 8
  %alloca_count_75 = alloca i64, align 8
  %alloca_74 = alloca i64, align 8
  %alloca_count_74 = alloca i64, align 8
  %load_77 = load i64, ptr %0, align 8
  %gep_79 = getelementptr inbounds i8, ptr %0, i64 8
  %load_81 = load i64, ptr %gep_79, align 8
  %iop_82 = add i64 %load_77, %load_81
  store i64 %iop_82, ptr %alloca_count_75, align 8
  %load_84 = load i64, ptr %alloca_count_75, align 8
  %iop_85 = mul i64 2, %load_84
  store i64 %iop_85, ptr %alloca_count_74, align 8
  %load_87 = load i64, ptr %alloca_count_74, align 8
  ret i64 %load_87
}

define internal i1 @Rectangle__is_square(ptr %0) {
bb0:
  %alloca_88 = alloca i1, align 1
  %alloca_count_88 = alloca i1, align 1
  %load_90 = load i64, ptr %0, align 8
  %gep_92 = getelementptr inbounds i8, ptr %0, i64 8
  %load_94 = load i64, ptr %gep_92, align 8
  %icmp_95 = icmp eq i64 %load_90, %load_94
  store i1 %icmp_95, ptr %alloca_count_88, align 1
  %load_97 = load i1, ptr %alloca_count_88, align 1
  ret i1 %load_97
}

define i32 @main() {
bb0:
  %alloca_183 = alloca { i64, i64 }, align 8
  %alloca_count_183 = alloca { i64, i64 }, align 8
  %alloca_182 = alloca ptr, align 8
  %alloca_count_182 = alloca ptr, align 8
  %alloca_176 = alloca { i64, i64 }, align 8
  %alloca_count_176 = alloca { i64, i64 }, align 8
  %alloca_175 = alloca ptr, align 8
  %alloca_count_175 = alloca ptr, align 8
  %alloca_169 = alloca { i64, i64 }, align 8
  %alloca_count_169 = alloca { i64, i64 }, align 8
  %alloca_168 = alloca ptr, align 8
  %alloca_count_168 = alloca ptr, align 8
  %alloca_161 = alloca { i64, i64 }, align 8
  %alloca_count_161 = alloca { i64, i64 }, align 8
  %alloca_157 = alloca { i64, i64 }, align 8
  %alloca_count_157 = alloca { i64, i64 }, align 8
  %alloca_149 = alloca { i64, i64 }, align 8
  %alloca_count_149 = alloca { i64, i64 }, align 8
  %alloca_148 = alloca ptr, align 8
  %alloca_count_148 = alloca ptr, align 8
  %alloca_145 = alloca { i64, i64 }, align 8
  %alloca_count_145 = alloca { i64, i64 }, align 8
  %alloca_144 = alloca ptr, align 8
  %alloca_count_144 = alloca ptr, align 8
  %alloca_137 = alloca { i64, i64 }, align 8
  %alloca_count_137 = alloca { i64, i64 }, align 8
  %alloca_133 = alloca { i64, i64 }, align 8
  %alloca_count_133 = alloca { i64, i64 }, align 8
  %alloca_127 = alloca i64, align 8
  %alloca_count_127 = alloca i64, align 8
  %alloca_124 = alloca { i64, i64 }, align 8
  %alloca_count_124 = alloca { i64, i64 }, align 8
  %alloca_123 = alloca ptr, align 8
  %alloca_count_123 = alloca ptr, align 8
  %alloca_116 = alloca { i64, i64 }, align 8
  %alloca_count_116 = alloca { i64, i64 }, align 8
  %alloca_112 = alloca { i64, i64 }, align 8
  %alloca_count_112 = alloca { i64, i64 }, align 8
  %alloca_105 = alloca { i64, i64 }, align 8
  %alloca_count_105 = alloca { i64, i64 }, align 8
  %alloca_101 = alloca { i64, i64 }, align 8
  %alloca_count_101 = alloca { i64, i64 }, align 8
  %call_98 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.0)
  br label %bb1

bb1:                                              ; preds = %bb0
  %call_99 = call { i64, i64 } @Point__new(i64 10, i64 20)
  br label %bb2

bb2:                                              ; preds = %bb1
  %call_100 = call { i64, i64 } @Point__new(i64 5, i64 15)
  br label %bb3

bb3:                                              ; preds = %bb2
  store { i64, i64 } %call_99, ptr %alloca_count_101, align 8
  %load_104 = load i64, ptr %alloca_count_101, align 8
  store { i64, i64 } %call_99, ptr %alloca_count_105, align 8
  %gep_108 = getelementptr inbounds i8, ptr %alloca_count_105, i64 8
  %load_110 = load i64, ptr %gep_108, align 8
  %call_111 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.1, i64 %load_104, i64 %load_110)
  br label %bb4

bb4:                                              ; preds = %bb3
  store { i64, i64 } %call_100, ptr %alloca_count_112, align 8
  %load_115 = load i64, ptr %alloca_count_112, align 8
  store { i64, i64 } %call_100, ptr %alloca_count_116, align 8
  %gep_119 = getelementptr inbounds i8, ptr %alloca_count_116, i64 8
  %load_121 = load i64, ptr %gep_119, align 8
  %call_122 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.2, i64 %load_115, i64 %load_121)
  br label %bb5

bb5:                                              ; preds = %bb4
  store { i64, i64 } %call_99, ptr %alloca_count_124, align 8
  store ptr %alloca_count_124, ptr %alloca_count_123, align 8
  store i64 -4, ptr %alloca_count_127, align 8
  %load_130 = load ptr, ptr %alloca_count_123, align 8
  %load_131 = load i64, ptr %alloca_count_127, align 8
  call void @Point__translate(ptr %load_130, i64 3, i64 %load_131)
  br label %bb6

bb6:                                              ; preds = %bb5
  store { i64, i64 } %call_99, ptr %alloca_count_133, align 8
  %load_136 = load i64, ptr %alloca_count_133, align 8
  store { i64, i64 } %call_99, ptr %alloca_count_137, align 8
  %gep_140 = getelementptr inbounds i8, ptr %alloca_count_137, i64 8
  %load_142 = load i64, ptr %gep_140, align 8
  %call_143 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.3, i64 %load_136, i64 %load_142)
  br label %bb7

bb7:                                              ; preds = %bb6
  store { i64, i64 } %call_99, ptr %alloca_count_145, align 8
  store ptr %alloca_count_145, ptr %alloca_count_144, align 8
  store { i64, i64 } %call_100, ptr %alloca_count_149, align 8
  store ptr %alloca_count_149, ptr %alloca_count_148, align 8
  %load_152 = load ptr, ptr %alloca_count_144, align 8
  %load_153 = load ptr, ptr %alloca_count_148, align 8
  %call_154 = call i64 @Point__distance2(ptr %load_152, ptr %load_153)
  br label %bb8

bb8:                                              ; preds = %bb7
  %call_155 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.4, i64 %call_154)
  br label %bb9

bb9:                                              ; preds = %bb8
  %call_156 = call { i64, i64 } @Rectangle__new(i64 10, i64 5)
  br label %bb10

bb10:                                             ; preds = %bb9
  store { i64, i64 } %call_156, ptr %alloca_count_157, align 8
  %load_160 = load i64, ptr %alloca_count_157, align 8
  store { i64, i64 } %call_156, ptr %alloca_count_161, align 8
  %gep_164 = getelementptr inbounds i8, ptr %alloca_count_161, i64 8
  %load_166 = load i64, ptr %gep_164, align 8
  %call_167 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.5, i64 %load_160, i64 %load_166)
  br label %bb11

bb11:                                             ; preds = %bb10
  store { i64, i64 } %call_156, ptr %alloca_count_169, align 8
  store ptr %alloca_count_169, ptr %alloca_count_168, align 8
  %load_172 = load ptr, ptr %alloca_count_168, align 8
  %call_173 = call i64 @Rectangle__area(ptr %load_172)
  br label %bb12

bb12:                                             ; preds = %bb11
  %call_174 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.6, i64 %call_173)
  br label %bb13

bb13:                                             ; preds = %bb12
  store { i64, i64 } %call_156, ptr %alloca_count_176, align 8
  store ptr %alloca_count_176, ptr %alloca_count_175, align 8
  %load_179 = load ptr, ptr %alloca_count_175, align 8
  %call_180 = call i64 @Rectangle__perimeter(ptr %load_179)
  br label %bb14

bb14:                                             ; preds = %bb13
  %call_181 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.7, i64 %call_180)
  br label %bb15

bb15:                                             ; preds = %bb14
  store { i64, i64 } %call_156, ptr %alloca_count_183, align 8
  store ptr %alloca_count_183, ptr %alloca_count_182, align 8
  %load_186 = load ptr, ptr %alloca_count_182, align 8
  %call_187 = call i1 @Rectangle__is_square(ptr %load_186)
  br label %bb16

bb16:                                             ; preds = %bb15
  %zext = zext i1 %call_187 to i32
  %call_189 = call i32 (ptr, ...) @printf(ptr @.str.06_struct_methods.8, i32 %zext)
  br label %bb17

bb17:                                             ; preds = %bb16
  ret i32 0
}

declare i32 @printf(ptr, ...)
