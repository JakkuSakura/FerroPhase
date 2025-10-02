; ModuleID = 'ferrophase_const_eval'
@.str0 = private unnamed_addr constant [30 x i8] c"=== Struct Introspection ===\0A\00"
@.str1 = private unnamed_addr constant [22 x i8] c"Point size: 16 bytes\0A\00"
@.str2 = private unnamed_addr constant [22 x i8] c"Color size: 24 bytes\0A\00"
@.str3 = private unnamed_addr constant [17 x i8] c"Point fields: 2\0A\00"
@.str4 = private unnamed_addr constant [17 x i8] c"Color fields: 3\0A\00"
@.str5 = private unnamed_addr constant [19 x i8] c"Point has x: true\0A\00"
@.str6 = private unnamed_addr constant [20 x i8] c"Point has z: false\0A\00"
@.str7 = private unnamed_addr constant [18 x i8] c"Point methods: 0\0A\00"
@.str8 = private unnamed_addr constant [18 x i8] c"Color methods: 0\0A\00"
@.str9 = private unnamed_addr constant [31 x i8] c"\0A\E2\9C\93 Introspection completed!\0A\00"
@.str10 = private unnamed_addr constant [29 x i8] c"\0A=== Transpilation Demo ===\0A\00"
@.str11 = private unnamed_addr constant [29 x i8] c"Transpilation target sizes:\0A\00"
@.str12 = private unnamed_addr constant [27 x i8] c"  Point: 16 bytes (const)\0A\00"
@.str13 = private unnamed_addr constant [27 x i8] c"  Color: 24 bytes (const)\0A\00"
@.str14 = private unnamed_addr constant [22 x i8] c"  Combined: 40 bytes\0A\00"
@.str15 = private unnamed_addr constant [20 x i8] c"Runtime instances:\0A\00"
@.str16 = private unnamed_addr constant [22 x i8] c"  Origin: (0.0, 0.0)\0A\00"
@.str17 = private unnamed_addr constant [23 x i8] c"  Red: rgb(255, 0, 0)\0A\00"
@.str18 = private unnamed_addr constant [54 x i8] c"\0A\E2\9C\93 Introspection enables external code generation!\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %call0 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([30 x i8], [30 x i8]* @.str0, i64 0, i64 0))
  %call1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([22 x i8], [22 x i8]* @.str1, i64 0, i64 0))
  %call2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([22 x i8], [22 x i8]* @.str2, i64 0, i64 0))
  %call3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str3, i64 0, i64 0))
  %call4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str4, i64 0, i64 0))
  %call5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([19 x i8], [19 x i8]* @.str5, i64 0, i64 0))
  %call6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([20 x i8], [20 x i8]* @.str6, i64 0, i64 0))
  %call7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str7, i64 0, i64 0))
  %call8 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str8, i64 0, i64 0))
  %call9 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str9, i64 0, i64 0))
  %call10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([29 x i8], [29 x i8]* @.str10, i64 0, i64 0))
  %call11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([29 x i8], [29 x i8]* @.str11, i64 0, i64 0))
  %call12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([27 x i8], [27 x i8]* @.str12, i64 0, i64 0))
  %call13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([27 x i8], [27 x i8]* @.str13, i64 0, i64 0))
  %call14 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([22 x i8], [22 x i8]* @.str14, i64 0, i64 0))
  %call15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([20 x i8], [20 x i8]* @.str15, i64 0, i64 0))
  %call16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([22 x i8], [22 x i8]* @.str16, i64 0, i64 0))
  %call17 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([23 x i8], [23 x i8]* @.str17, i64 0, i64 0))
  %call18 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([54 x i8], [54 x i8]* @.str18, i64 0, i64 0))
  ret i32 0
}
