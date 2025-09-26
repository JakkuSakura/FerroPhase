; ModuleID = ''
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx12.0.0"

@.str.0 = constant [34 x i8] c"=== String Processing Results ===\00", align 1
@.str.1 = constant [34 x i8] c"App name: FerroPhase (length: 10)\00", align 1
@.str.2 = constant [27 x i8] c"Version: 1.0.0 (length: 5)\00", align 1
@.str.3 = constant [39 x i8] c"Banner: FerroPhase v1.0.0 (length: 17)\00", align 1
@.str.4 = constant [25 x i8] [i8 10, i8 61, i8 61, i8 61, i8 32, i8 83, i8 116, i8 114, i8 105, i8 110, i8 103, i8 32, i8 65, i8 110, i8 97, i8 108, i8 121, i8 115, i8 105, i8 115, i8 32, i8 61, i8 61, i8 61, i8 0], align 1
@.str.5 = constant [26 x i8] c"Version has 'beta': false\00", align 1
@.str.6 = constant [22 x i8] c"Version has '.': true\00", align 1
@.str.7 = constant [30 x i8] c"Banner contains version: true\00", align 1
@.str.8 = constant [25 x i8] c"App name is empty: false\00", align 1
@.str.9 = constant [27 x i8] c"Dot position in version: 1\00", align 1
@.str.10 = constant [38 x i8] [i8 10, i8 61, i8 61, i8 61, i8 32, i8 83, i8 116, i8 114, i8 105, i8 110, i8 103, i8 32, i8 67, i8 111, i8 110, i8 99, i8 97, i8 116, i8 101, i8 110, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 82, i8 101, i8 115, i8 117, i8 108, i8 116, i8 115, i8 32, i8 61, i8 61, i8 61, i8 0], align 1
@.str.11 = constant [39 x i8] c"Banner (+ operator): FerroPhase v1.0.0\00", align 1
@.str.12 = constant [47 x i8] c"Config file (concat! macro): FerroPhase.config\00", align 1
@.str.13 = constant [46 x i8] c"Log file (multi-concat): FerroPhase_1.0.0.log\00", align 1
@.str.14 = constant [51 x i8] c"Error log file (+ operator): FerroPhase_errors.log\00", align 1
@.str.15 = constant [27 x i8] c"Debug prefix: [FerroPhase]\00", align 1
@.str.16 = constant [60 x i8] c"Status message: Application FerroPhase version 1.0.0 loaded\00", align 1
@.str.17 = constant [28 x i8] [i8 10, i8 61, i8 61, i8 61, i8 32, i8 70, i8 105, i8 108, i8 101, i8 32, i8 67, i8 111, i8 110, i8 102, i8 105, i8 103, i8 117, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 61, i8 61, i8 61, i8 0], align 1
@.str.18 = constant [17 x i8] c"Buffer size: 0KB\00", align 1
@.str.19 = constant [25 x i8] c"Needs large buffer: true\00", align 1
@.str.20 = constant [25 x i8] [i8 10, i8 61, i8 61, i8 61, i8 32, i8 67, i8 111, i8 109, i8 112, i8 117, i8 116, i8 101, i8 100, i8 32, i8 86, i8 97, i8 108, i8 117, i8 101, i8 115, i8 32, i8 61, i8 61, i8 61, i8 0], align 1
@.str.21 = constant [17 x i8] c"Simple hash: 315\00", align 1
@.str.22 = constant [19 x i8] c"Computed hash: 316\00", align 1
@.str.23 = constant [42 x i8] c"Hash computation: (10 * 31) + 5 + 1 = 316\00", align 1
@.str.24 = constant [35 x i8] [i8 10, i8 61, i8 61, i8 61, i8 32, i8 86, i8 97, i8 108, i8 105, i8 100, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 38, i8 32, i8 79, i8 112, i8 116, i8 105, i8 109, i8 105, i8 122, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 61, i8 61, i8 61, i8 0], align 1
@.str.25 = constant [26 x i8] c"Is valid app config: true\00", align 1
@.str.26 = constant [22 x i8] c"Config complexity: 32\00", align 1
@.str.27 = constant [34 x i8] c"Recommended optimization level: 3\00", align 1
@.str.28 = constant [39 x i8] [i8 226, i8 156, i8 147, i8 32, i8 65, i8 112, i8 112, i8 108, i8 105, i8 99, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 99, i8 111, i8 110, i8 102, i8 105, i8 103, i8 117, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 105, i8 115, i8 32, i8 118, i8 97, i8 108, i8 105, i8 100, i8 0], align 1
@.str.29 = constant [43 x i8] [i8 226, i8 154, i8 160, i8 32, i8 65, i8 112, i8 112, i8 108, i8 105, i8 99, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 99, i8 111, i8 110, i8 102, i8 105, i8 103, i8 117, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 110, i8 101, i8 101, i8 100, i8 115, i8 32, i8 114, i8 101, i8 118, i8 105, i8 101, i8 119, i8 0], align 1
@.str.30 = constant [37 x i8] [i8 240, i8 159, i8 154, i8 167, i8 32, i8 68, i8 101, i8 118, i8 101, i8 108, i8 111, i8 112, i8 109, i8 101, i8 110, i8 116, i8 47, i8 66, i8 101, i8 116, i8 97, i8 32, i8 98, i8 117, i8 105, i8 108, i8 100, i8 32, i8 100, i8 101, i8 116, i8 101, i8 99, i8 116, i8 101, i8 100, i8 0], align 1
@.str.31 = constant [28 x i8] [i8 240, i8 159, i8 154, i8 128, i8 32, i8 80, i8 114, i8 111, i8 100, i8 117, i8 99, i8 116, i8 105, i8 111, i8 110, i8 32, i8 98, i8 117, i8 105, i8 108, i8 100, i8 32, i8 114, i8 101, i8 97, i8 100, i8 121, i8 0], align 1
@.str.32 = constant [36 x i8] [i8 10, i8 61, i8 61, i8 61, i8 32, i8 67, i8 111, i8 110, i8 99, i8 97, i8 116, i8 101, i8 110, i8 97, i8 116, i8 105, i8 111, i8 110, i8 32, i8 77, i8 101, i8 116, i8 104, i8 111, i8 100, i8 115, i8 32, i8 68, i8 101, i8 109, i8 111, i8 32, i8 61, i8 61, i8 61, i8 0], align 1
@.str.33 = constant [19 x i8] c"Method: + operator\00", align 1
@.str.34 = constant [22 x i8] c"Method: concat! macro\00", align 1
@.str.35 = constant [17 x i8] [i8 10, i8 61, i8 61, i8 61, i8 32, i8 83, i8 117, i8 109, i8 109, i8 97, i8 114, i8 121, i8 32, i8 61, i8 61, i8 61, i8 0], align 1
@.str.36 = constant [50 x i8] c"Processed 3 base strings with 32 total characters\00", align 1
@.str.37 = constant [30 x i8] c"Uppercase variant: FERROPHASE\00", align 1
@.str.38 = constant [52 x i8] c"Generated 7 concatenated strings during compilation\00", align 1
declare void @func_0()
declare i32 @puts(ptr)
define i32 @main() {
bb0:
  call i32 @puts(ptr getelementptr inbounds ([34 x i8], ptr @.str.0, i32 0, i32 0))
  br label %bb1
bb1:
  call i32 @puts(ptr getelementptr inbounds ([34 x i8], ptr @.str.1, i32 0, i32 0))
  br label %bb2
bb2:
  call i32 @puts(ptr getelementptr inbounds ([27 x i8], ptr @.str.2, i32 0, i32 0))
  br label %bb3
bb3:
  call i32 @puts(ptr getelementptr inbounds ([39 x i8], ptr @.str.3, i32 0, i32 0))
  br label %bb4
bb4:
  call i32 @puts(ptr getelementptr inbounds ([25 x i8], ptr @.str.4, i32 0, i32 0))
  br label %bb5
bb5:
  call i32 @puts(ptr getelementptr inbounds ([26 x i8], ptr @.str.5, i32 0, i32 0))
  br label %bb6
bb6:
  call i32 @puts(ptr getelementptr inbounds ([22 x i8], ptr @.str.6, i32 0, i32 0))
  br label %bb7
bb7:
  call i32 @puts(ptr getelementptr inbounds ([30 x i8], ptr @.str.7, i32 0, i32 0))
  br label %bb8
bb8:
  call i32 @puts(ptr getelementptr inbounds ([25 x i8], ptr @.str.8, i32 0, i32 0))
  br label %bb9
bb9:
  call i32 @puts(ptr getelementptr inbounds ([27 x i8], ptr @.str.9, i32 0, i32 0))
  br label %bb10
bb10:
  call i32 @puts(ptr getelementptr inbounds ([38 x i8], ptr @.str.10, i32 0, i32 0))
  br label %bb11
bb11:
  call i32 @puts(ptr getelementptr inbounds ([39 x i8], ptr @.str.11, i32 0, i32 0))
  br label %bb12
bb12:
  call i32 @puts(ptr getelementptr inbounds ([47 x i8], ptr @.str.12, i32 0, i32 0))
  br label %bb13
bb13:
  call i32 @puts(ptr getelementptr inbounds ([46 x i8], ptr @.str.13, i32 0, i32 0))
  br label %bb14
bb14:
  call i32 @puts(ptr getelementptr inbounds ([51 x i8], ptr @.str.14, i32 0, i32 0))
  br label %bb15
bb15:
  call i32 @puts(ptr getelementptr inbounds ([27 x i8], ptr @.str.15, i32 0, i32 0))
  br label %bb16
bb16:
  call i32 @puts(ptr getelementptr inbounds ([60 x i8], ptr @.str.16, i32 0, i32 0))
  br label %bb17
bb17:
  call i32 @puts(ptr getelementptr inbounds ([28 x i8], ptr @.str.17, i32 0, i32 0))
  br label %bb18
bb18:
  call i32 @puts(ptr getelementptr inbounds ([17 x i8], ptr @.str.18, i32 0, i32 0))
  br label %bb19
bb19:
  call i32 @puts(ptr getelementptr inbounds ([25 x i8], ptr @.str.19, i32 0, i32 0))
  br label %bb20
bb20:
  call i32 @puts(ptr getelementptr inbounds ([25 x i8], ptr @.str.20, i32 0, i32 0))
  br label %bb21
bb21:
  call i32 @puts(ptr getelementptr inbounds ([17 x i8], ptr @.str.21, i32 0, i32 0))
  br label %bb22
bb22:
  call i32 @puts(ptr getelementptr inbounds ([19 x i8], ptr @.str.22, i32 0, i32 0))
  br label %bb23
bb23:
  call i32 @puts(ptr getelementptr inbounds ([42 x i8], ptr @.str.23, i32 0, i32 0))
  br label %bb24
bb24:
  call i32 @puts(ptr getelementptr inbounds ([35 x i8], ptr @.str.24, i32 0, i32 0))
  br label %bb25
bb25:
  call i32 @puts(ptr getelementptr inbounds ([26 x i8], ptr @.str.25, i32 0, i32 0))
  br label %bb26
bb26:
  call i32 @puts(ptr getelementptr inbounds ([22 x i8], ptr @.str.26, i32 0, i32 0))
  br label %bb27
bb27:
  call i32 @puts(ptr getelementptr inbounds ([34 x i8], ptr @.str.27, i32 0, i32 0))
  br label %bb28
bb28:
  br i1 1, label %bb29, label %bb30
bb29:
  call i32 @puts(ptr getelementptr inbounds ([39 x i8], ptr @.str.28, i32 0, i32 0))
  br label %bb32
bb30:
  call i32 @puts(ptr getelementptr inbounds ([43 x i8], ptr @.str.29, i32 0, i32 0))
  br label %bb33
bb32:
  br label %bb31
bb33:
  br label %bb31
bb31:
  br i1 0, label %bb34, label %bb35
bb34:
  call i32 @puts(ptr getelementptr inbounds ([37 x i8], ptr @.str.30, i32 0, i32 0))
  br label %bb37
bb35:
  call i32 @puts(ptr getelementptr inbounds ([28 x i8], ptr @.str.31, i32 0, i32 0))
  br label %bb38
bb37:
  br label %bb36
bb38:
  br label %bb36
bb36:
  call i32 @puts(ptr getelementptr inbounds ([36 x i8], ptr @.str.32, i32 0, i32 0))
  br label %bb39
bb39:
  call i32 @puts(ptr getelementptr inbounds ([19 x i8], ptr @.str.33, i32 0, i32 0))
  br label %bb40
bb40:
  call i32 @puts(ptr getelementptr inbounds ([22 x i8], ptr @.str.34, i32 0, i32 0))
  br label %bb41
bb41:
  call i32 @puts(ptr getelementptr inbounds ([17 x i8], ptr @.str.35, i32 0, i32 0))
  br label %bb42
bb42:
  call i32 @puts(ptr getelementptr inbounds ([50 x i8], ptr @.str.36, i32 0, i32 0))
  br label %bb43
bb43:
  call i32 @puts(ptr getelementptr inbounds ([30 x i8], ptr @.str.37, i32 0, i32 0))
  br label %bb44
bb44:
  call i32 @puts(ptr getelementptr inbounds ([52 x i8], ptr @.str.38, i32 0, i32 0))
  br label %bb45
bb45:
  ret i32 0
}

