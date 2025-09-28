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
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  call i32 (ptr, ...) @printf(ptr @.str.0)
  %call_98 = call ptr (i64, i64) @Point__new(i64 10, i64 20)
  %call_99 = call ptr (i64, i64) @Point__new(i64 5, i64 15)
  %load_100 = load i64, ptr %call_98
  %gep_101 = getelementptr inbounds i64, ptr %call_98, i64 1
  %load_102 = load i64, ptr %gep_101
  call i32 (ptr, ...) @printf(ptr @.str.1, i64 %load_100, i64 %load_102)
  %load_104 = load i64, ptr %call_99
  %gep_105 = getelementptr inbounds i64, ptr %call_99, i64 1
  %load_106 = load i64, ptr %gep_105
  call i32 (ptr, ...) @printf(ptr @.str.2, i64 %load_104, i64 %load_106)
  call void (ptr, i64, i64) @Point__translate(ptr %call_98, i64 3, i64 18446744073709551612)
  %load_109 = load i64, ptr %call_98
  %gep_110 = getelementptr inbounds i64, ptr %call_98, i64 1
  %load_111 = load i64, ptr %gep_110
  call i32 (ptr, ...) @printf(ptr @.str.3, i64 %load_109, i64 %load_111)
  %call_113 = call i64 (ptr, ptr) @Point__distance2(ptr %call_98, ptr %call_99)
  call i32 (ptr, ...) @printf(ptr @.str.4, i64 %call_113)
  %call_115 = call ptr (i64, i64) @Rectangle__new(i64 10, i64 5)
  %load_116 = load i64, ptr %call_115
  %gep_117 = getelementptr inbounds i64, ptr %call_115, i64 1
  %load_118 = load i64, ptr %gep_117
  call i32 (ptr, ...) @printf(ptr @.str.5, i64 %load_116, i64 %load_118)
  %call_120 = call i64 (ptr) @Rectangle__area(ptr %call_115)
  call i32 (ptr, ...) @printf(ptr @.str.6, i64 %call_120)
  %call_122 = call i64 (ptr) @Rectangle__perimeter(ptr %call_115)
  call i32 (ptr, ...) @printf(ptr @.str.7, i64 %call_122)
  %call_124 = call i1 (ptr) @Rectangle__is_square(ptr %call_115)
  %zext_125 = zext i1 %call_124 to i64
  call i32 (ptr, ...) @printf(ptr @.str.8, i64 %zext_125)
  ret i32 0
}

