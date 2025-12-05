# FerroPhase 扩展语法（G3）

本文件汇总 G3 需求涉及的扩展语法、重写位置与测试要点，独立于上司已有文档。

## 关键词与宏

- `quote { … }`：捕获代码为 `ExprKind::Quote`，根据内容推断 item/stmt/expr 片段种类。
- `splice ( … )`：在表达式 / 语句 / 条目位置插入 QuoteToken，种类不匹配应产生日志/诊断。
- `const { … }`：解析为 `ExprKind::IntrinsicCall(ConstBlock)`，块内容原样保留。
- `emit! { … }`：语法糖，解析时重写为 `splice ( quote { … } )`，重写位置：
  `crates/fp-lang/src/parser/expr.rs` 内的 `parse_expr_prec`。

## 结构化类型与类型代数

- 结构化类型字面量：`struct { field: Ty, other }` → `TypeStructural`（已实现）。
- 类型代数：
  - 加法：`A + B`（已实现）
  - 减法：`A - B`（已实现）
  - 交集：`A & B`（已实现）
  - 并集：`A | B`（已实现，`TypeBinaryOpKind::Union`）
  解析入口：`crates/fp-lang/src/parser/items.rs` 的 `peek_type_binop / apply_type_binop`。

## 受限场景与诊断

- `splice` 在不允许的上下文（错误的片段种类 / 受限位置）应记录诊断，携带 span。
- `emit!` 仅允许出现在 `const { … }` 内；其他位置应报错。
- 宏/属性在解析阶段保留为 AST，不展开。

## 测试策略（正反例）

- quote / splice / emit / const block：正例确保 CST→AST 保留；反例覆盖种类不匹配、位置受限。
- 类型代数：正例覆盖 `A | B`、`(A | B) - C` 等组合；反例可覆盖优先级/不合法操作。
- 结构化类型字面量：正例（空、显式、简写字段），反例（重复字段、缺冒号等）。

## 运行时集成要点

- `emit!` 重写后复用 quote/splice 管线；下游看到的都是统一的 AST 形态。
- 诊断与日志沿用 tracing/diagnostic 管线，需包含源文件/行列信息。
