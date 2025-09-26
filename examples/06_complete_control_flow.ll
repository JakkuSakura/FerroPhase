; ModuleID = ''
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx12.0.0"

@.str.0 = constant [41 x i8] c"=== FerroPhase Control Flow Features ===\00", align 1
@.str.1 = constant [37 x i8] [i8 10, i8 49, i8 46, i8 32, i8 67, i8 111, i8 110, i8 115, i8 116, i8 32, i8 69, i8 118, i8 97, i8 108, i8 117, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 73, i8 102, i8 32, i8 69, i8 120, i8 112, i8 114, i8 101, i8 115, i8 115, i8 105, i8 111, i8 110, i8 115, i8 58, i8 0], align 1
@.str.2 = constant [26 x i8] [i8 84, i8 101, i8 109, i8 112, i8 101, i8 114, i8 97, i8 116, i8 117, i8 114, i8 101, i8 32, i8 50, i8 53, i8 194, i8 176, i8 67, i8 32, i8 105, i8 115, i8 32, i8 119, i8 97, i8 114, i8 109, i8 0], align 1
@.str.3 = constant [19 x i8] [i8 10, i8 50, i8 46, i8 32, i8 66, i8 111, i8 111, i8 108, i8 101, i8 97, i8 110, i8 32, i8 76, i8 111, i8 103, i8 105, i8 99, i8 58, i8 0], align 1
@.str.4 = constant [48 x i8] c"Weather verdict: Perfect for outdoor activities\00", align 1
@.str.5 = constant [29 x i8] [i8 10, i8 51, i8 46, i8 32, i8 83, i8 116, i8 114, i8 105, i8 110, i8 103, i8 45, i8 98, i8 97, i8 115, i8 101, i8 100, i8 32, i8 67, i8 111, i8 110, i8 100, i8 105, i8 116, i8 105, i8 111, i8 110, i8 115, i8 58, i8 0], align 1
@.str.6 = constant [37 x i8] c"FerroPhase 0.1.0 - Development Build\00", align 1
@.str.7 = constant [29 x i8] [i8 10, i8 52, i8 46, i8 32, i8 77, i8 97, i8 116, i8 104, i8 101, i8 109, i8 97, i8 116, i8 105, i8 99, i8 97, i8 108, i8 32, i8 67, i8 111, i8 110, i8 100, i8 105, i8 116, i8 105, i8 111, i8 110, i8 115, i8 58, i8 0], align 1
@.str.8 = constant [39 x i8] c"Triangle with sides 8, 12, 20: Scalene\00", align 1
@.str.9 = constant [31 x i8] [i8 10, i8 53, i8 46, i8 32, i8 67, i8 111, i8 109, i8 112, i8 108, i8 101, i8 120, i8 32, i8 78, i8 101, i8 115, i8 116, i8 101, i8 100, i8 32, i8 67, i8 111, i8 110, i8 100, i8 105, i8 116, i8 105, i8 111, i8 110, i8 115, i8 58, i8 0], align 1
@.str.10 = constant [33 x i8] c"Scores: 85, 92, 78 (Average: 85)\00", align 1
@.str.11 = constant [29 x i8] c"Assessment: Good Performance\00", align 1
@.str.12 = constant [28 x i8] [i8 10, i8 54, i8 46, i8 32, i8 82, i8 117, i8 110, i8 116, i8 105, i8 109, i8 101, i8 32, i8 73, i8 102, i8 32, i8 69, i8 120, i8 112, i8 114, i8 101, i8 115, i8 115, i8 105, i8 111, i8 110, i8 115, i8 58, i8 0], align 1
@.str.13 = constant [25 x i8] c"Runtime value {} is high\00", align 1
@.str.14 = constant [38 x i8] [i8 10, i8 61, i8 61, i8 61, i8 32, i8 67, i8 111, i8 110, i8 116, i8 114, i8 111, i8 108, i8 32, i8 70, i8 108, i8 111, i8 119, i8 32, i8 70, i8 101, i8 97, i8 116, i8 117, i8 114, i8 101, i8 32, i8 83, i8 117, i8 109, i8 109, i8 97, i8 114, i8 121, i8 32, i8 61, i8 61, i8 61, i8 0], align 1
@.str.15 = constant [27 x i8] c"Runtime value {} is medium\00", align 1
@.str.16 = constant [24 x i8] c"Runtime value {} is low\00", align 1
@.str.17 = constant [30 x i8] [i8 226, i8 156, i8 147, i8 32, i8 66, i8 97, i8 115, i8 105, i8 99, i8 32, i8 105, i8 102, i8 45, i8 101, i8 108, i8 115, i8 101, i8 32, i8 101, i8 120, i8 112, i8 114, i8 101, i8 115, i8 115, i8 105, i8 111, i8 110, i8 115, i8 0], align 1
@.str.18 = constant [26 x i8] [i8 226, i8 156, i8 147, i8 32, i8 78, i8 101, i8 115, i8 116, i8 101, i8 100, i8 32, i8 105, i8 102, i8 45, i8 101, i8 108, i8 115, i8 101, i8 32, i8 99, i8 104, i8 97, i8 105, i8 110, i8 115, i8 0], align 1
@.str.19 = constant [37 x i8] [i8 226, i8 156, i8 147, i8 32, i8 66, i8 111, i8 111, i8 108, i8 101, i8 97, i8 110, i8 32, i8 108, i8 111, i8 103, i8 105, i8 99, i8 32, i8 111, i8 112, i8 101, i8 114, i8 97, i8 116, i8 111, i8 114, i8 115, i8 32, i8 40, i8 38, i8 38, i8 44, i8 32, i8 124, i8 124, i8 41, i8 0], align 1
@.str.20 = constant [43 x i8] [i8 226, i8 156, i8 147, i8 32, i8 83, i8 116, i8 114, i8 105, i8 110, i8 103, i8 32, i8 109, i8 101, i8 116, i8 104, i8 111, i8 100, i8 32, i8 99, i8 111, i8 110, i8 100, i8 105, i8 116, i8 105, i8 111, i8 110, i8 115, i8 32, i8 40, i8 46, i8 99, i8 111, i8 110, i8 116, i8 97, i8 105, i8 110, i8 115, i8 40, i8 41, i8 41, i8 0], align 1
@.str.21 = constant [29 x i8] [i8 226, i8 156, i8 147, i8 32, i8 77, i8 97, i8 116, i8 104, i8 101, i8 109, i8 97, i8 116, i8 105, i8 99, i8 97, i8 108, i8 32, i8 99, i8 111, i8 109, i8 112, i8 97, i8 114, i8 105, i8 115, i8 111, i8 110, i8 115, i8 0], align 1
@.str.22 = constant [30 x i8] [i8 226, i8 156, i8 147, i8 32, i8 67, i8 111, i8 109, i8 112, i8 108, i8 101, i8 120, i8 32, i8 110, i8 101, i8 115, i8 116, i8 101, i8 100, i8 32, i8 99, i8 111, i8 110, i8 100, i8 105, i8 116, i8 105, i8 111, i8 110, i8 115, i8 0], align 1
@.str.23 = constant [29 x i8] [i8 226, i8 156, i8 147, i8 32, i8 67, i8 111, i8 110, i8 115, i8 116, i8 32, i8 101, i8 118, i8 97, i8 108, i8 117, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 115, i8 117, i8 112, i8 112, i8 111, i8 114, i8 116, i8 0], align 1
@.str.24 = constant [31 x i8] [i8 226, i8 156, i8 147, i8 32, i8 82, i8 117, i8 110, i8 116, i8 105, i8 109, i8 101, i8 32, i8 101, i8 118, i8 97, i8 108, i8 117, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 115, i8 117, i8 112, i8 112, i8 111, i8 114, i8 116, i8 0], align 1
@.str.25 = constant [52 x i8] [i8 226, i8 156, i8 147, i8 32, i8 65, i8 83, i8 84, i8 226, i8 134, i8 146, i8 72, i8 73, i8 82, i8 226, i8 134, i8 146, i8 84, i8 72, i8 73, i8 82, i8 226, i8 134, i8 146, i8 77, i8 73, i8 82, i8 226, i8 134, i8 146, i8 76, i8 73, i8 82, i8 226, i8 134, i8 146, i8 76, i8 76, i8 86, i8 77, i8 32, i8 99, i8 111, i8 109, i8 112, i8 105, i8 108, i8 97, i8 116, i8 105, i8 111, i8 110, i8 0], align 1
@.str.26 = constant [50 x i8] [i8 10, i8 70, i8 101, i8 114, i8 114, i8 111, i8 80, i8 104, i8 97, i8 115, i8 101, i8 32, i8 99, i8 111, i8 110, i8 116, i8 114, i8 111, i8 108, i8 32, i8 102, i8 108, i8 111, i8 119, i8 32, i8 115, i8 121, i8 115, i8 116, i8 101, i8 109, i8 32, i8 111, i8 112, i8 101, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 97, i8 108, i8 33, i8 32, i8 240, i8 159, i8 154, i8 128, i8 0], align 1
declare void @func_0()
declare i32 @puts(ptr)
define i32 @main() {
bb0:
  call i32 @puts(ptr getelementptr inbounds ([41 x i8], ptr @.str.0, i32 0, i32 0))
  br label %bb1
bb1:
  call i32 @puts(ptr getelementptr inbounds ([37 x i8], ptr @.str.1, i32 0, i32 0))
  br label %bb2
bb2:
  call i32 @puts(ptr getelementptr inbounds ([26 x i8], ptr @.str.2, i32 0, i32 0))
  br label %bb3
bb3:
  call i32 @puts(ptr getelementptr inbounds ([19 x i8], ptr @.str.3, i32 0, i32 0))
  br label %bb4
bb4:
  call i32 @puts(ptr getelementptr inbounds ([48 x i8], ptr @.str.4, i32 0, i32 0))
  br label %bb5
bb5:
  call i32 @puts(ptr getelementptr inbounds ([29 x i8], ptr @.str.5, i32 0, i32 0))
  br label %bb6
bb6:
  call i32 @puts(ptr getelementptr inbounds ([37 x i8], ptr @.str.6, i32 0, i32 0))
  br label %bb7
bb7:
  call i32 @puts(ptr getelementptr inbounds ([29 x i8], ptr @.str.7, i32 0, i32 0))
  br label %bb8
bb8:
  call i32 @puts(ptr getelementptr inbounds ([39 x i8], ptr @.str.8, i32 0, i32 0))
  br label %bb9
bb9:
  call i32 @puts(ptr getelementptr inbounds ([31 x i8], ptr @.str.9, i32 0, i32 0))
  br label %bb10
bb10:
  call i32 @puts(ptr getelementptr inbounds ([33 x i8], ptr @.str.10, i32 0, i32 0))
  br label %bb11
bb11:
  call i32 @puts(ptr getelementptr inbounds ([29 x i8], ptr @.str.11, i32 0, i32 0))
  br label %bb12
bb12:
  call i32 @puts(ptr getelementptr inbounds ([28 x i8], ptr @.str.12, i32 0, i32 0))
  br label %bb13
bb13:
  ret i32 0
bb14:
  call i32 @puts(ptr getelementptr inbounds ([25 x i8], ptr @.str.13, i32 0, i32 0), i32 42)
  br label %bb17
bb15:
  ret i32 0
bb16:
  call i32 @puts(ptr getelementptr inbounds ([38 x i8], ptr @.str.14, i32 0, i32 0))
  br label %bb23
bb17:
  br label %bb16
bb18:
  call i32 @puts(ptr getelementptr inbounds ([27 x i8], ptr @.str.15, i32 0, i32 0), i32 42)
  br label %bb21
bb19:
  call i32 @puts(ptr getelementptr inbounds ([24 x i8], ptr @.str.16, i32 0, i32 0), i32 42)
  br label %bb22
bb20:
  br label %bb16
bb21:
  br label %bb20
bb22:
  br label %bb20
bb23:
  call i32 @puts(ptr getelementptr inbounds ([30 x i8], ptr @.str.17, i32 0, i32 0))
  br label %bb24
bb24:
  call i32 @puts(ptr getelementptr inbounds ([26 x i8], ptr @.str.18, i32 0, i32 0))
  br label %bb25
bb25:
  call i32 @puts(ptr getelementptr inbounds ([37 x i8], ptr @.str.19, i32 0, i32 0))
  br label %bb26
bb26:
  call i32 @puts(ptr getelementptr inbounds ([43 x i8], ptr @.str.20, i32 0, i32 0))
  br label %bb27
bb27:
  call i32 @puts(ptr getelementptr inbounds ([29 x i8], ptr @.str.21, i32 0, i32 0))
  br label %bb28
bb28:
  call i32 @puts(ptr getelementptr inbounds ([30 x i8], ptr @.str.22, i32 0, i32 0))
  br label %bb29
bb29:
  call i32 @puts(ptr getelementptr inbounds ([29 x i8], ptr @.str.23, i32 0, i32 0))
  br label %bb30
bb30:
  call i32 @puts(ptr getelementptr inbounds ([31 x i8], ptr @.str.24, i32 0, i32 0))
  br label %bb31
bb31:
  call i32 @puts(ptr getelementptr inbounds ([52 x i8], ptr @.str.25, i32 0, i32 0))
  br label %bb32
bb32:
  call i32 @puts(ptr getelementptr inbounds ([50 x i8], ptr @.str.26, i32 0, i32 0))
  br label %bb33
bb33:
  ret i32 0
}

