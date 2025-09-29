; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [64 x i8] [i8 67, i8 111, i8 110, i8 102, i8 105, i8 103, i8 58, i8 32, i8 98, i8 117, i8 102, i8 102, i8 101, i8 114, i8 61, i8 37, i8 108, i8 117, i8 75, i8 66, i8 44, i8 32, i8 99, i8 111, i8 110, i8 110, i8 101, i8 99, i8 116, i8 105, i8 111, i8 110, i8 115, i8 61, i8 37, i8 100, i8 44, i8 32, i8 116, i8 105, i8 109, i8 101, i8 111, i8 117, i8 116, i8 61, i8 37, i8 108, i8 108, i8 117, i8 109, i8 115, i8 44, i8 32, i8 100, i8 101, i8 98, i8 117, i8 103, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
@.str.1 = constant [38 x i8] [i8 67, i8 111, i8 109, i8 112, i8 117, i8 116, i8 101, i8 100, i8 58, i8 32, i8 102, i8 97, i8 99, i8 116, i8 111, i8 114, i8 105, i8 97, i8 108, i8 61, i8 37, i8 108, i8 108, i8 100, i8 44, i8 32, i8 105, i8 115, i8 95, i8 112, i8 111, i8 119, i8 50, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
declare i32 @printf(ptr, ...)
define i32 @main() {
bb0:
  %alloca_0 = alloca i32, align 4
  %alloca_1 = alloca i32, align 4
  %alloca_2 = alloca i64, align 8
  store i64 4096, ptr %alloca_2
  %alloca_4 = alloca i64, align 8
  %load_5 = load i64, ptr %alloca_2
  store i64 %load_5, ptr %alloca_4
  %alloca_7 = alloca i32, align 4
  store i64 150, ptr %alloca_7
  %alloca_9 = alloca i32, align 4
  %load_10 = load i32, ptr %alloca_7
  store i32 %load_10, ptr %alloca_9
  %alloca_12 = alloca i64, align 8
  store i64 30000, ptr %alloca_12
  %alloca_14 = alloca i64, align 8
  %load_15 = load i64, ptr %alloca_12
  store i64 %load_15, ptr %alloca_14
  %alloca_17 = alloca i64, align 8
  store i64 120, ptr %alloca_17
  %alloca_19 = alloca i64, align 8
  %load_20 = load i64, ptr %alloca_17
  store i64 %load_20, ptr %alloca_19
  %alloca_22 = alloca i64, align 8
  store i64 1, ptr %alloca_22
  %alloca_24 = alloca i64, align 8
  %load_25 = load i64, ptr %alloca_4
  %load_26 = load i64, ptr %alloca_22
  %sub_27 = sub i64 %load_25, %load_26
  store i64 %sub_27, ptr %alloca_24
  %alloca_29 = alloca i64, align 8
  %load_30 = load i64, ptr %alloca_4
  %load_31 = load i64, ptr %alloca_24
  %and_32 = and i64 %load_30, %load_31
  store i64 %and_32, ptr %alloca_29
  %alloca_34 = alloca i64, align 8
  store i64 0, ptr %alloca_34
  %alloca_36 = alloca i1, align 1
  %load_37 = load i64, ptr %alloca_29
  %load_38 = load i64, ptr %alloca_34
  %icmp_39 = icmp eq i64 %load_37, %load_38
  store i1 %icmp_39, ptr %alloca_36
  %alloca_41 = alloca i1, align 1
  %load_42 = load i1, ptr %alloca_36
  store i1 %load_42, ptr %alloca_41
  %alloca_44 = alloca i1, align 1
  store i1 0, ptr %alloca_44
  %load_46 = load i1, ptr %alloca_44
  br i1 %load_46, label %bb1, label %bb2
bb1:
  %alloca_47 = alloca i64, align 8
  store i64 2, ptr %alloca_47
  %load_49 = load i64, ptr %alloca_47
  store i64 %load_49, ptr %alloca_1
  br label %bb3
bb2:
  %alloca_51 = alloca i64, align 8
  store i64 0, ptr %alloca_51
  %load_53 = load i64, ptr %alloca_51
  store i64 %load_53, ptr %alloca_1
  br label %bb3
bb3:
  %alloca_55 = alloca i32, align 4
  %load_56 = load i32, ptr %alloca_1
  store i32 %load_56, ptr %alloca_55
  %alloca_58 = alloca i64, align 8
  store i64 0, ptr %alloca_58
  %alloca_60 = alloca i64, align 8
  %load_61 = load i64, ptr %alloca_58
  store i64 %load_61, ptr %alloca_60
  %alloca_63 = alloca i32, align 4
  %alloca_64 = alloca i64, align 8
  store i64 1024, ptr %alloca_64
  %alloca_66 = alloca i64, align 8
  %load_67 = load i64, ptr %alloca_4
  %load_68 = load i64, ptr %alloca_64
  %div_69 = udiv i64 %load_67, %load_68
  store i64 %div_69, ptr %alloca_66
  %load_71 = load i64, ptr %alloca_66
  %load_72 = load i32, ptr %alloca_9
  %load_73 = load i64, ptr %alloca_14
  %load_74 = load i32, ptr %alloca_55
  call i32 (ptr, ...) @printf(ptr @.str.0, i64 %load_71, i32 %load_72, i64 %load_73, i32 %load_74)
  br label %bb4
bb4:
  %alloca_76 = alloca i32, align 4
  %load_77 = load i64, ptr %alloca_19
  %load_78 = load i1, ptr %alloca_41
  call i32 (ptr, ...) @printf(ptr @.str.1, i64 %load_77, i1 %load_78)
  br label %bb5
bb5:
  store i32 0, ptr %alloca_0
  ret i32 0
}

