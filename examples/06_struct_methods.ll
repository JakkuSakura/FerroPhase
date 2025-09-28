; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [27 x i8] [i8 61, i8 61, i8 61, i8 32, i8 83, i8 116, i8 114, i8 117, i8 99, i8 116, i8 32, i8 79, i8 112, i8 101, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 115, i8 32, i8 61, i8 61, i8 61, i8 10, i8 0], align 1
@.str.1 = constant [19 x i8] [i8 112, i8 49, i8 32, i8 61, i8 32, i8 40, i8 37, i8 108, i8 108, i8 100, i8 44, i8 32, i8 37, i8 108, i8 108, i8 100, i8 41, i8 10, i8 0], align 1
@.str.2 = constant [19 x i8] [i8 112, i8 50, i8 32, i8 61, i8 32, i8 40, i8 37, i8 108, i8 108, i8 100, i8 44, i8 32, i8 37, i8 108, i8 108, i8 100, i8 41, i8 10, i8 0], align 1
@.str.3 = constant [35 x i8] [i8 112, i8 49, i8 32, i8 97, i8 102, i8 116, i8 101, i8 114, i8 32, i8 116, i8 114, i8 97, i8 110, i8 115, i8 108, i8 97, i8 116, i8 101, i8 32, i8 61, i8 32, i8 40, i8 37, i8 108, i8 108, i8 100, i8 44, i8 32, i8 37, i8 108, i8 108, i8 100, i8 41, i8 10, i8 0], align 1
@.str.4 = constant [27 x i8] [i8 68, i8 105, i8 115, i8 116, i8 97, i8 110, i8 99, i8 101, i8 194, i8 178, i8 40, i8 112, i8 49, i8 44, i8 32, i8 112, i8 50, i8 41, i8 32, i8 61, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.5 = constant [23 x i8] [i8 82, i8 101, i8 99, i8 116, i8 97, i8 110, i8 103, i8 108, i8 101, i8 58, i8 32, i8 37, i8 108, i8 108, i8 100, i8 195, i8 151, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.6 = constant [15 x i8] [i8 32, i8 32, i8 97, i8 114, i8 101, i8 97, i8 32, i8 61, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.7 = constant [20 x i8] [i8 32, i8 32, i8 112, i8 101, i8 114, i8 105, i8 109, i8 101, i8 116, i8 101, i8 114, i8 32, i8 61, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
@.str.8 = constant [20 x i8] [i8 32, i8 32, i8 105, i8 115, i8 95, i8 115, i8 113, i8 117, i8 97, i8 114, i8 101, i8 32, i8 61, i8 32, i8 37, i8 108, i8 108, i8 100, i8 10, i8 0], align 1
declare ptr @malloc(i64)
declare i32 @printf(ptr, ...)
define ptr @Point__new(i64 %arg0, i64 %arg1) {
bb0:
  %call_2 = call ptr (i64) @malloc(i64 16)
  %bitcast_3 = bitcast ptr %call_2 to ptr
  store i64 %arg0, ptr %bitcast_3
  %gep_5 = getelementptr inbounds i64, ptr %bitcast_3, i64 1
  store i64 %arg1, ptr %gep_5
  ret ptr %bitcast_3
}

define void @Point__translate(ptr %arg0, i64 %arg1, i64 %arg2) {
bb0:
  %load_29 = load i64, ptr %arg0
  %add_30 = add i64 %load_29, %arg1
  store i64 %add_30, ptr %arg0
  %gep_32 = getelementptr inbounds i64, ptr %arg0, i64 1
  %load_33 = load i64, ptr %gep_32
  %add_34 = add i64 %load_33, %arg2
  store i64 %add_34, ptr %gep_32
  ret void
}

define i64 @Point__distance2(ptr %arg0, ptr %arg1) {
bb0:
  %load_97 = load i64, ptr %arg0
  %gep_98 = getelementptr inbounds i64, ptr %arg0, i64 1
  %load_99 = load i64, ptr %gep_98
  %load_100 = load i64, ptr %arg1
  %gep_101 = getelementptr inbounds i64, ptr %arg1, i64 1
  %load_102 = load i64, ptr %gep_101
  %sub_103 = sub i64 %load_97, %load_100
  %sub_104 = sub i64 %load_99, %load_102
  %mul_105 = mul i64 %sub_103, %sub_103
  %mul_106 = mul i64 %sub_104, %sub_104
  %add_107 = add i64 %mul_105, %mul_106
  ret i64 %add_107
}

define ptr @Rectangle__new(i64 %arg0, i64 %arg1) {
bb0:
  %call_110 = call ptr (i64) @malloc(i64 16)
  %bitcast_111 = bitcast ptr %call_110 to ptr
  store i64 %arg0, ptr %bitcast_111
  %gep_113 = getelementptr inbounds i64, ptr %bitcast_111, i64 1
  store i64 %arg1, ptr %gep_113
  ret ptr %bitcast_111
}

define i64 @Rectangle__area(ptr %arg0) {
bb0:
  %load_129 = load i64, ptr %arg0
  %gep_130 = getelementptr inbounds i64, ptr %arg0, i64 1
  %load_131 = load i64, ptr %gep_130
  %mul_132 = mul i64 %load_129, %load_131
  ret i64 %mul_132
}

define i64 @Rectangle__perimeter(ptr %arg0) {
bb0:
  %load_154 = load i64, ptr %arg0
  %gep_155 = getelementptr inbounds i64, ptr %arg0, i64 1
  %load_156 = load i64, ptr %gep_155
  %add_157 = add i64 %load_154, %load_156
  %mul_158 = mul i64 2, %add_157
  ret i64 %mul_158
}

define i1 @Rectangle__is_square(ptr %arg0) {
bb0:
  %load_173 = load i64, ptr %arg0
  %gep_174 = getelementptr inbounds i64, ptr %arg0, i64 1
  %load_175 = load i64, ptr %gep_174
  %icmp_176 = icmp eq i64 %load_173, %load_175
  ret i1 %icmp_176
}

define i32 @main() {
bb0:
  call i32 (ptr, ...) @printf(ptr @.str.0)
  %call_255 = call ptr (i64, i64) @Point__new(i64 10, i64 20)
  %call_256 = call ptr (i64, i64) @Point__new(i64 5, i64 15)
  %load_257 = load i64, ptr %call_255
  %gep_258 = getelementptr inbounds i64, ptr %call_255, i64 1
  %load_259 = load i64, ptr %gep_258
  call i32 (ptr, ...) @printf(ptr @.str.1, i64 %load_257, i64 %load_259)
  %load_261 = load i64, ptr %call_256
  %gep_262 = getelementptr inbounds i64, ptr %call_256, i64 1
  %load_263 = load i64, ptr %gep_262
  call i32 (ptr, ...) @printf(ptr @.str.2, i64 %load_261, i64 %load_263)
  call void (ptr, i64, i64) @Point__translate(ptr %call_255, i64 3, i64 18446744073709551612)
  %load_266 = load i64, ptr %call_255
  %gep_267 = getelementptr inbounds i64, ptr %call_255, i64 1
  %load_268 = load i64, ptr %gep_267
  call i32 (ptr, ...) @printf(ptr @.str.3, i64 %load_266, i64 %load_268)
  %call_270 = call i64 (ptr, ptr) @Point__distance2(ptr %call_255, ptr %call_256)
  call i32 (ptr, ...) @printf(ptr @.str.4, i64 %call_270)
  %call_272 = call ptr (i64, i64) @Rectangle__new(i64 10, i64 5)
  %load_273 = load i64, ptr %call_272
  %gep_274 = getelementptr inbounds i64, ptr %call_272, i64 1
  %load_275 = load i64, ptr %gep_274
  call i32 (ptr, ...) @printf(ptr @.str.5, i64 %load_273, i64 %load_275)
  %call_277 = call i64 (ptr) @Rectangle__area(ptr %call_272)
  call i32 (ptr, ...) @printf(ptr @.str.6, i64 %call_277)
  %call_279 = call i64 (ptr) @Rectangle__perimeter(ptr %call_272)
  call i32 (ptr, ...) @printf(ptr @.str.7, i64 %call_279)
  %call_281 = call i1 (ptr) @Rectangle__is_square(ptr %call_272)
  %zext_282 = zext i1 %call_281 to i64
  call i32 (ptr, ...) @printf(ptr @.str.8, i64 %zext_282)
  ret i32 0
}

