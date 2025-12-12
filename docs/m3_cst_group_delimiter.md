# CST 规范：Group/Delimiter（M3 口径）

本文件总结 FerroPhase 前端的 CST（Concrete Syntax Tree）在 M3 口径下的“功能形态”：CST 只负责忠实表达源码的 token 流、trivia（空白/注释）与通用分组结构（括号/大括号/中括号），不承担“识别具体语法构造名”的职责。

## 一句话规范

**CST 只表达通用的 token 流、trivia（空白/注释）以及成对分隔符形成的分组结构；CST 不对任何具体关键字/语法构造（包括 `quote/splice/emit/const`）做节点级分类。**

## 前端主路径

- 入口：`source -> lexemes(含 trivia) -> CST`
- 进入现有语法解析器前：`CST -> flatten/filter -> parser tokens`
- 语义 AST：`parser tokens -> AST`
- 语法糖/扩展语法（如 `emit!`）的规范化：在 `CST->tokens` 之后、进入 token-based parser 之前的 token-pass 完成（而不是用 CST kind 区分“特殊节点”）。

## 数据模型

### Kind 集合

CST 仅允许以下节点类型：

- `Root`
- `Group(Delimiter)`
- `Token`
- `TriviaWhitespace | TriviaLineComment | TriviaBlockComment`

其中：

```text
Delimiter = Paren | Brace | Bracket
```

### Group 语义与不变量

- Group 表示“由成对分隔符包围的一个子 token-stream”，仅按分隔符类型区分：
  - `Paren` 对应 `(...)`
  - `Brace` 对应 `{...}`
  - `Bracket` 对应 `[...]`
- Group 的 `children` **不包含**分隔符 token（`(`、`)`、`{`、`}`、`[`、`]`）：
  - 分隔符信息由 `Delimiter` 表达，而不是靠把分隔符当作普通 token 插入 children。
- Group 的 `span` 应覆盖整个分组范围：
  - 理想情况：覆盖从开分隔符到闭分隔符（含两端）
  - 至少：覆盖到闭分隔符（便于诊断定位“分组未闭合/闭合位置”）

### Token/Trivia 语义

- `Token` 节点携带该 token 在源码中的原始文本片段（raw slice），不做 normalize。
- `Trivia*` 节点同样携带原始文本片段（例如 `//...\n` 含换行），以便保持“源码形态”。
- `span` 的 file_id 与 lo/hi（字节偏移）用于诊断；file_id 的注入应发生在前端入口（file mode 可由 path 映射，expr mode 可用虚拟 id）。

## 约束（关键纪律）

- CST 层不做“按关键字/构造名”的节点区分：
  - 禁止出现 `CstKind::Quote/Splice/ConstBlock/EmitMacro` 这类构造名节点。
  - `quote/splice/emit/const` 仅以 token 形式存在（必要时结合 Group 结构参与模式匹配）。
- CST→AST 阶段可以基于 token 模式匹配实现语法糖改写（desugar/lowering）：
  - 例如：`emit ! Group(Brace{...})` 可在 token-pass 中改写为 `splice ( quote Group(Brace{...}) )` 再交给现有 token-based parser。

## 示例（直观树形）

> 以下示例仅用于说明“CST 长相”，不是 AST。

### 示例 1：`emit! { 1 }`

```text
Root
  Token("emit")
  Token("!")
  Group(Brace)
    Token("1")
```

### 示例 2：`splice ( quote { x } )`

```text
Root
  Token("splice")
  Group(Paren)
    Token("quote")
    Group(Brace)
      Token("x")
```
