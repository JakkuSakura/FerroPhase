# G3 CST→AST 降级策略与诊断要求

本文件描述解析阶段哪些节点直接降级、哪些保留为特殊 AST 节点，以及诊断/日志规范。

## 节点保留/降级原则

- 直接降级为 AST：常规 Rust 语法（项/表达式/模式/类型等）、结构化类型、控制流。
- 保留为特殊节点：
  - `quote { … }` → `ExprKind::Quote`（携带块 AST，含推断的片段种类）
  - `splice ( … )` → `ExprKind::Splice`（引用 QuoteToken）
  - `const { … }` → `ExprKind::IntrinsicCall(ConstBlock)`，内部 block 原样保留
  - `emit! { … }` → 解析期重写为 `splice ( quote { … } )`
- 宏调用/属性：不在降级阶段展开，保留为 AST 节点交给后续宏系统。

## 解析入口与重写位置

- CST→AST 主入口：`crates/fp-lang/src/parser/lower.rs`
- quote/splice/emit 关键重写：`crates/fp-lang/src/parser/expr.rs`（`parse_expr_prec`）

## 诊断与日志

- 解析失败或上下文不匹配（如 splice 片段种类错误）应生成诊断，携带 span；不要仅 `eprintln!`。
- `emit!` 在非 `const {}` 场景应报错。
- tracing 输出可用于调试，但面向用户的错误需通过 diagnostics 管线。

## 测试要求

- 每个 G3 功能点至少一组正例 + 反例：
  - quote/splice/emit/const block
  - 类型代数 `|`、结构化类型字面量
  - 受限上下文（非法 splice、非法 emit! 场景）
- 反例测试应断言出现诊断/错误，而不仅是“解析失败”。

## 非功能注意

- 解析性能：全仓解析 ≤ 10s（release，本机）；如未达标需记录改进计划。
- 内存：峰值 ≤ 512MB；同样需记录测量与改进计划。
- 日志：错误输出需包含源文件/行列；调试输出应避免污染正常 stderr。
