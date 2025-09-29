; ModuleID = ''
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@.str.0 = constant [25 x i8] [i8 66, i8 117, i8 102, i8 102, i8 101, i8 114, i8 32, i8 115, i8 105, i8 122, i8 101, i8 58, i8 32, i8 37, i8 108, i8 108, i8 117, i8 32, i8 66, i8 121, i8 116, i8 101, i8 115, i8 10, i8 0], align 1
@.str.1 = constant [65 x i8] [i8 67, i8 111, i8 110, i8 102, i8 105, i8 103, i8 58, i8 32, i8 98, i8 117, i8 102, i8 102, i8 101, i8 114, i8 61, i8 37, i8 108, i8 108, i8 117, i8 75, i8 66, i8 44, i8 32, i8 99, i8 111, i8 110, i8 110, i8 101, i8 99, i8 116, i8 105, i8 111, i8 110, i8 115, i8 61, i8 37, i8 100, i8 44, i8 32, i8 116, i8 105, i8 109, i8 101, i8 111, i8 117, i8 116, i8 61, i8 37, i8 108, i8 108, i8 117, i8 109, i8 115, i8 44, i8 32, i8 100, i8 101, i8 98, i8 117, i8 103, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
@.str.2 = constant [38 x i8] [i8 67, i8 111, i8 109, i8 112, i8 117, i8 116, i8 101, i8 100, i8 58, i8 32, i8 102, i8 97, i8 99, i8 116, i8 111, i8 114, i8 105, i8 97, i8 108, i8 61, i8 37, i8 108, i8 108, i8 100, i8 44, i8 32, i8 105, i8 115, i8 95, i8 112, i8 111, i8 119, i8 50, i8 61, i8 37, i8 100, i8 10, i8 0], align 1
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
  %load_7 = load i64, ptr %alloca_4
  call i32 (ptr, ...) @printf(ptr @.str.0, i64 %load_7)
  br label %bb1
bb1:
  %alloca_9 = alloca i32, align 4
  store i64 150, ptr %alloca_9
  %alloca_11 = alloca i32, align 4
  %load_12 = load i32, ptr %alloca_9
  store i32 %load_12, ptr %alloca_11
  %alloca_14 = alloca i64, align 8
  store i64 30000, ptr %alloca_14
  %alloca_16 = alloca i64, align 8
  %load_17 = load i64, ptr %alloca_14
  store i64 %load_17, ptr %alloca_16
  %alloca_19 = alloca i64, align 8
  store i64 120, ptr %alloca_19
  %alloca_21 = alloca i64, align 8
  %load_22 = load i64, ptr %alloca_19
  store i64 %load_22, ptr %alloca_21
  %alloca_24 = alloca i64, align 8
  store i64 1, ptr %alloca_24
  %alloca_26 = alloca i64, align 8
  %load_27 = load i64, ptr %alloca_4
  %load_28 = load i64, ptr %alloca_24
  %sub_29 = sub i64 %load_27, %load_28
  store i64 %sub_29, ptr %alloca_26
  %alloca_31 = alloca i64, align 8
  %load_32 = load i64, ptr %alloca_4
  %load_33 = load i64, ptr %alloca_26
  %and_34 = and i64 %load_32, %load_33
  store i64 %and_34, ptr %alloca_31
  %alloca_36 = alloca i64, align 8
  store i64 0, ptr %alloca_36
  %alloca_38 = alloca i1, align 1
  %load_39 = load i64, ptr %alloca_31
  %load_40 = load i64, ptr %alloca_36
  %icmp_41 = icmp eq i64 %load_39, %load_40
  store i1 %icmp_41, ptr %alloca_38
  %alloca_43 = alloca i1, align 1
  %load_44 = load i1, ptr %alloca_38
  store i1 %load_44, ptr %alloca_43
  %alloca_46 = alloca i1, align 1
  store i1 1, ptr %alloca_46
  %load_48 = load i1, ptr %alloca_46
  br i1 %load_48, label %bb2, label %bb3
bb2:
  %alloca_49 = alloca i64, align 8
  store i64 2, ptr %alloca_49
  %load_51 = load i64, ptr %alloca_49
  store i64 %load_51, ptr %alloca_1
  br label %bb4
bb3:
  %alloca_53 = alloca i64, align 8
  store i64 0, ptr %alloca_53
  %load_55 = load i64, ptr %alloca_53
  store i64 %load_55, ptr %alloca_1
  br label %bb4
bb4:
  %alloca_57 = alloca i32, align 4
  %load_58 = load i32, ptr %alloca_1
  store i32 %load_58, ptr %alloca_57
  %alloca_60 = alloca i64, align 8
  store i64 0, ptr %alloca_60
  %alloca_62 = alloca i64, align 8
  %load_63 = load i64, ptr %alloca_60
  store i64 %load_63, ptr %alloca_62
  %alloca_65 = alloca i64, align 8
  store i64 1024, ptr %alloca_65
  %alloca_67 = alloca i64, align 8
  %load_68 = load i64, ptr %alloca_4
  %load_69 = load i64, ptr %alloca_65
  %div_70 = udiv i64 %load_68, %load_69
  store i64 %div_70, ptr %alloca_67
  %load_72 = load i64, ptr %alloca_67
  %load_73 = load i32, ptr %alloca_11
  %load_74 = load i64, ptr %alloca_16
  %load_75 = load i32, ptr %alloca_57
  call i32 (ptr, ...) @printf(ptr @.str.1, i64 %load_72, i32 %load_73, i64 %load_74, i32 %load_75)
  br label %bb5
bb5:
  %load_77 = load i64, ptr %alloca_21
  %load_78 = load i1, ptr %alloca_43
  call i32 (ptr, ...) @printf(ptr @.str.2, i64 %load_77, i1 %load_78)
  br label %bb6
bb6:
  store i32 0, ptr %alloca_0
  ret i32 0
}

