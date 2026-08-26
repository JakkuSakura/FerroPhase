use super::*;

/// nested snippets self-consistent no matter how deep they end up embedded.
impl KotlinEmitter {
    pub(super) fn render_expr(&mut self, expr: &Expr) -> Result<String> {
        match expr.kind() {
            ExprKind::Value(val) => {
                let rendered = render_value(val);
                // Kotlin has no byte-literal syntax — a `u8` value (self.g. from a Rust
                // byte literal `b':'`) needs an explicit `.toByte()` conversion to be
                // usable where an actual `Byte` (not `Int`) is expected.
                let is_u8 = matches!(
                    fp_core::ast::resolved_expr_type(expr.id()),
                    Some(Ty::Primitive(TypePrimitive::Int(TypeInt::U8)))
                );
                if is_u8 && matches!(val.as_ref(), Value::Int(_) | Value::UInt(_)) {
                    Ok(format!("{}.toByte()", rendered))
                } else {
                    Ok(rendered)
                }
            }
            ExprKind::Name(name) => {
                let raw = name.to_string();
                // `FerroIntrinsicNormalizer` already rewrites a bare `None` to
                // `Value::Null` during normalization, and pattern rendering
                // special-cases it too, but a bare `None` used directly as an
                // expression (self.g. a tuple-constructor argument) can still reach
                // here unrewritten — handle it defensively at the backend level.
                if raw == "None" {
                    return Ok("null".to_string());
                }
                // A bare `self` used as a call receiver (`self.other_method()`,
                // rendered here since `render_expr` sees just the receiver
                // expression, not the surrounding `Select`) — Kotlin has no
                // implicit local named `self`; `this` is the equivalent and,
                // unlike the field-access case (`ExprKind::Select` below, which
                // drops it entirely), stays valid written out explicitly even
                // inside an extension function body.
                if raw == "self" {
                    return Ok("this".to_string());
                }
                // A module-qualified enum-variant VALUE (self.g. Rust
                // `types::FileStatus::Modified` used as a plain value, not a
                // match pattern) — resolve the declaring enum's real name from
                // this expression's own resolved type (`Ty::Expr` wrapping the
                // real `DefPath`-derived path built by
                // `HirToAstLifter::def_id_to_ty`) rather than guessing from the
                // path text: a `DefPath`'s own declaring segment is
                // structurally always last, so `ty()`'s last path segment is
                // the real enum name regardless of how many module segments
                // precede it. Look the exact Kotlin spelling up in the same
                // `enum_variant_names` registry `render_match_pat` already
                // uses for the pattern case.
                if let Some(enum_name) =
                    enum_name_from_ty(fp_core::ast::resolved_expr_type(expr.id()).as_ref())
                {
                    let variant_name = match name {
                        fp_core::ast::Name::Path(p) => p.segments.last().map(|s| s.name.clone()),
                        fp_core::ast::Name::Ident(id) => Some(id.name.clone()),
                        _ => None,
                    };
                    if let Some(kotlin_variant) = variant_name.as_deref().and_then(|v| {
                        self.enum_variant_names
                            .get(&enum_name)
                            .and_then(|m| m.get(v))
                    }) {
                        return Ok(format!("{}.{}", enum_name, kotlin_variant));
                    }
                }
                // Not a resolved enum-variant value (plain function/const
                // reference, `Self::`-shorthand path, or type info genuinely
                // unavailable) — an ordinary qualified name, rendered from its
                // real segments with no enum-specific guessing.
                Ok(qualified_name_with_self(
                    name,
                    self.current_self_name.as_deref(),
                ))
            }
            ExprKind::Id(id) => Ok(id.to_string()),

            ExprKind::Invoke(inv) => {
                match &inv.target {
                    ExprInvokeTarget::Method(sel) => {
                        // `.or_else(|_| fallback)` / `.unwrap_or_else(|_| fallback)` on a
                        // nullable value — Kotlin's `?:` already lazily evaluates its RHS
                        // only when the LHS is null, which matches these methods' fallback
                        // semantics exactly (the error/ignored-value closure param is never
                        // used in this codebase). `.run`/`.let` (the generic method-name
                        // mapping used elsewhere) would evaluate the fallback unconditionally.
                        if matches!(sel.field.name.as_str(), "or_else" | "unwrap_or_else")
                            && inv.args.len() == 1
                        {
                            if let ExprKind::Closure(cl) = inv.args[0].kind() {
                                let obj = self.render_expr(&sel.obj)?;
                                let body = self.render_expr(&cl.body)?;
                                let rhs = if body.contains('\n') {
                                    format!("run {{\n{}\n}}", body)
                                } else {
                                    body
                                };
                                return Ok(format!("{} ?: {}", obj, rhs));
                            }
                        }
                        // `Option::map_or(default, |x| body)` has no Kotlin equivalent method —
                        // rewrite structurally as `obj?.let { x -> body } ?: default`.
                        if sel.field.name.as_str() == "map_or" && inv.args.len() == 2 {
                            if let ExprKind::Closure(cl) = inv.args[1].kind() {
                                let obj = self.render_expr(&sel.obj)?;
                                let default = self.render_expr(&inv.args[0])?;
                                let param = cl
                                    .params
                                    .first()
                                    .map(ident_from_pattern)
                                    .unwrap_or_else(|| "it".to_string());
                                let body = self.render_expr(&cl.body)?;
                                return Ok(format!(
                                    "{}?.let {{ {} -> {} }} ?: {}",
                                    obj, param, body, default
                                ));
                            }
                        }
                        // `.map_err(SomeError::Variant)` / `.map_err(some_fn)` — Rust
                        // lets a tuple-variant constructor (or any named function) be
                        // passed as a bare value where a closure is expected, since
                        // it's itself a first-class `Fn(T) -> U` item. Kotlin's
                        // equivalent (a variant's own constructor, referenced through
                        // a dotted qualifier) isn't usable as a bare value the same
                        // way — `CoreError.IO` isn't a value, only `CoreError.IO(x)`
                        // is — so wrap it in an explicit one-arg lambda instead.
                        // `map_err` itself has no dedicated Result-mapping support
                        // here; this only needs to compile, matching every other
                        // Result-shaped call in this file.
                        if sel.field.name.as_str() == "map_err"
                            && inv.args.len() == 1
                            && !matches!(inv.args[0].kind(), ExprKind::Closure(_))
                        {
                            let obj = self.render_expr(&sel.obj)?;
                            let ctor = self.render_expr(&inv.args[0])?;
                            return Ok(format!("{}.map_err {{ __e -> {}(__e) }}", obj, ctor));
                        }
                        // `Option::take()` — replaces a `var` with `None`/`null`, returning
                        // the old value. Kotlin has no equivalent method; model it directly.
                        // A function *parameter* receiver can't be reassigned at all in
                        // Kotlin (always an implicit `val`, unlike a `let`-bound local) —
                        // drop the reset for those (see `current_fn_params`'s doc comment).
                        if sel.field.name.as_str() == "take" && inv.args.is_empty() {
                            if let ExprKind::Name(name) = sel.obj.kind() {
                                let obj = self.render_expr(&sel.obj)?;
                                if self.current_fn_params.contains(&name_to_string(name)) {
                                    return Ok(obj);
                                }
                                return Ok(format!(
                                    "run {{ val __t = {0}; {0} = null; __t }}",
                                    obj
                                ));
                            }
                        }
                        let obj = self.render_expr(&sel.obj)?;
                        // `round`/`log2` have no Kotlin member-method equivalent — both are
                        // top-level `kotlin.math` functions taking the receiver as an
                        // argument (`kotlin.math.round(x)`, not `x.round()`).
                        if matches!(sel.field.name.as_str(), "round" | "log2")
                            && inv.args.is_empty()
                        {
                            return Ok(format!("kotlin.math.{}({})", sel.field.name.as_str(), obj));
                        }
                        // `.map` is ambiguous: `Option::map`/`Result::map` need Kotlin's
                        // `.let { }` (no built-in `.map` on nullable types), but
                        // `Iterator::map` needs Kotlin's own (identically-named) `.map { }`,
                        // unchanged — the generic table below assumes the former. Detect the
                        // latter structurally: an iterator-producing receiver just before it.
                        let is_iterator_map = sel.field.name.as_str() == "map" && {
                            if let ExprKind::Invoke(recv_inv) = sel.obj.kind() {
                                if let ExprInvokeTarget::Method(recv_sel) = &recv_inv.target {
                                    matches!(
                                        recv_sel.field.name.as_str(),
                                        "iter" | "iter_mut" | "into_iter" | "lines" | "chars"
                                    )
                                } else {
                                    false
                                }
                            } else {
                                false
                            }
                        };
                        // `.len()` needs `.size` on a List but `.length` on a String —
                        // `map_kt_method` alone can't tell which, so check whether the
                        // receiver's name is a known List (see `field_element_types`).
                        let is_len_on_list = sel.field.name.as_str() == "len"
                            && is_known_list_receiver(&sel.obj, self);
                        // `.find` is also ambiguous: Rust's `str::find(pat: &str)` (a
                        // substring search, needs Kotlin's `indexOf`, which takes a
                        // `String` and returns `Int`) vs. `Iterator::find(predicate)`
                        // (needs Kotlin's own identically-named, closure-taking
                        // `find` — unchanged). Disambiguate by the argument's shape.
                        let is_string_find = sel.field.name.as_str() == "find"
                            && !matches!(
                                inv.args.first().map(|a| a.kind()),
                                Some(ExprKind::Closure(_))
                            );
                        // `.clone()` maps to `.copy()` (Kotlin's data-class convention)
                        // by default, but a `String`/`enum class` has no `.copy()` —
                        // already immutable, the call should just drop (see
                        // `is_known_string_receiver`/`is_known_enum_receiver`).
                        let is_clone_dropped = sel.field.name.as_str() == "clone"
                            && (is_known_string_receiver(&sel.obj, self)
                                || is_known_enum_receiver(&sel.obj, self));
                        // `str::parse::<T>()` needs Kotlin's `.toLong()` — but a user's own
                        // inherent/associated method that happens to be named `parse` (self.g. a
                        // winnow-combinator-style `RefNode::parse`) is a different, unrelated
                        // function that must stay `.parse(...)` unchanged. Only the real
                        // `String`-receiver case gets the numeric-string mapping.
                        let is_string_parse = sel.field.name.as_str() == "parse"
                            && is_known_string_receiver(&sel.obj, self);
                        // `map_kt_method`'s fallthrough (no entry for this Rust
                        // name) returns the name unchanged — that's fine for a
                        // known bare-property mapping (`len` → `length`, no
                        // parens needed), but a genuine unmapped method (any
                        // user-defined trait/inherent method, self.g. a custom
                        // `RepoBackend::workdir()`) still needs real Kotlin call
                        // parens even with zero args, unlike an actual property.
                        let mut is_unmapped_passthrough = false;
                        let method_name = if is_iterator_map {
                            "map".to_string()
                        } else if is_len_on_list {
                            // Rust's `.len()` always returns `usize`, which this
                            // workspace's type registry always maps to Kotlin
                            // `Long` (see `kotlin_type_from_ty`) — but Kotlin's
                            // `List.size` is natively `Int`. Coerce here so every
                            // `.len()` call is `Long`-typed like every other
                            // `usize` value, matching the convention rather than
                            // Kotlin's native collection API.
                            "size.toLong()".to_string()
                        } else if is_string_find {
                            "indexOf".to_string()
                        } else if is_clone_dropped {
                            "".to_string()
                        } else if is_string_parse {
                            "toLong()".to_string()
                        } else {
                            let mapped = map_kt_method(sel.field.name.as_str());
                            is_unmapped_passthrough = mapped == sel.field.name.as_str();
                            mapped
                        };
                        // `is_ascii_alphabetic`/etc. map to Kotlin `Char` methods (`isLetter()`),
                        // but their receiver here is a `Byte` (indexed out of `.as_bytes()`) —
                        // bridge it to a `Char` first.
                        let obj = if matches!(
                            method_name.as_str(),
                            "isLetter()" | "isDigit()" | "isWhitespace()" | "isLetterOrDigit()"
                        ) && is_byte_array_index(&sel.obj)
                        {
                            format!("{}.toInt().toChar()", obj)
                        } else {
                            obj
                        };
                        // Kotlin's `String.replace` only overloads `(Char, Char)` or
                        // `(String, String)` — Rust's `str::replace` allows a char pattern
                        // with a string replacement, so a `Char` arg here needs coercing to
                        // a one-character string to match the mixed-type call.
                        let is_replace = sel.field.name.as_str() == "replace";
                        // `removePrefix`/`removeSuffix` (mapped from Rust's
                        // `strip_prefix`/`strip_suffix`/`trim_end_matches`) take a
                        // `CharSequence`, not `Char` — Rust's char-pattern overloads
                        // need the same char-to-one-character-string coercion `replace` does.
                        let needs_char_as_string = is_replace
                            || matches!(
                                sel.field.name.as_str(),
                                "strip_prefix" | "strip_suffix" | "trim_end_matches"
                            );
                        let args: Vec<String> = inv
                            .args
                            .iter()
                            .map(|a| {
                                if needs_char_as_string {
                                    if let ExprKind::Value(v) = a.kind() {
                                        if let Value::Char(c) = v.as_ref() {
                                            return Ok(format!(
                                                "\"{}\"",
                                                escape_str_for_kt(&c.value.to_string())
                                            ));
                                        }
                                    }
                                }
                                self.render_expr(a)
                            })
                            .collect::<Result<Vec<_>>>()?;
                        if method_name.is_empty() {
                            Ok(obj)
                        } else if method_name == "!!" {
                            Ok(format!("{}!!", obj))
                        } else if args.is_empty() {
                            if is_unmapped_passthrough {
                                Ok(format!("{}.{}()", obj, method_name))
                            } else {
                                Ok(format!("{}.{}", obj, method_name))
                            }
                        } else if method_name.ends_with("()") {
                            let base = &method_name[..method_name.len() - 2];
                            Ok(format!("{}.{}({})", obj, base, args.join(", ")))
                        } else {
                            Ok(format!("{}.{}({})", obj, method_name, args.join(", ")))
                        }
                    }
                    _ => {
                        let name = invoke_name(&inv.target)?;
                        // `Self::other_fn(...)` — Kotlin has no `Self` expression-
                        // position equivalent; swap in the real class/enum name
                        // (see `current_self_name`'s doc comment).
                        let name = if let Some(self_name) = &self.current_self_name {
                            substitute_self_prefix(&name, self_name)
                        } else {
                            name
                        };
                        // `std::env::current_dir()` — a zero-arg free function whose Kotlin
                        // equivalent needs one arg (`System.getProperty("user.dir")`), which
                        // the generic `map_kt_path` + "always append (args)" pipeline below
                        // can't express without producing a spurious trailing `()`.
                        if name == "std::env::current_dir" && inv.args.is_empty() {
                            return Ok("System.getProperty(\"user.dir\")".to_string());
                        }
                        // Crates with no safe target-language equivalent (toml, serde_json,
                        // tokio, ...) — render as an explicit stub instead of a broken
                        // identifier reference. `TODO()` is typed `Nothing`, so it compiles
                        // in any expression position.
                        if let Some(pos) = name.rfind("::") {
                            let prefix = name[..pos].replace("::", ".");
                            if known_package(&prefix) == KnownPackage::Unsupported {
                                return Ok(format!("TODO(\"unsupported: {}\")", name));
                            }
                        }
                        // Rewrite type prefix in function paths like `PathBuf::from` → `Path.of`
                        let mapped = map_kt_path(&name);
                        let args: Vec<String> = inv
                            .args
                            .iter()
                            .map(|a| self.render_expr(a))
                            .collect::<Result<Vec<_>>>()?;
                        Ok(format!("{}({})", mapped, args.join(", ")))
                    }
                }
            }

            ExprKind::Select(sel) => {
                let obj = self.render_expr(&sel.obj)?;
                if obj == "self" {
                    let field = map_kt_field(sel.field.name.as_str());
                    Ok(field)
                } else {
                    let field = map_kt_field(sel.field.name.as_str());
                    Ok(format!("{}.{}", obj, field))
                }
            }

            ExprKind::Index(idx) => {
                // Rust's `&s[..end]`/`s[start..]`/`&s[start..end]` (slicing with an
                // omitted bound is common) has no direct Kotlin equivalent —
                // `obj[range]` isn't valid indexing syntax. `String.substring`
                // shares `..`'s start-inclusive/end-exclusive semantics; a slice/
                // `Vec` (see `is_known_list_receiver`) needs `List.subList` instead
                // — both require an explicit end, so an omitted one becomes `.size`.
                if let ExprKind::Range(r) = idx.index.kind() {
                    let is_list = is_known_list_receiver(&idx.obj, self);
                    let obj = self.render_expr(&idx.obj)?;
                    let start = match &r.start {
                        Some(s) => self.render_expr(s)?,
                        None => "0".to_string(),
                    };
                    if !is_list && r.end.is_none() {
                        return Ok(format!("{}.substring({})", obj, start));
                    }
                    let end = match &r.end {
                        Some(end) => {
                            let end = self.render_expr(end)?;
                            if matches!(r.limit, fp_core::ast::ExprRangeLimit::Inclusive) {
                                format!("({} + 1)", end)
                            } else {
                                end
                            }
                        }
                        None => format!("{}.size", obj),
                    };
                    return if is_list {
                        Ok(format!("{}.subList({}, {})", obj, start, end))
                    } else {
                        Ok(format!("{}.substring({}, {})", obj, start, end))
                    };
                }
                Ok(format!(
                    "{}[{}]",
                    self.render_expr(&idx.obj)?,
                    self.render_expr(&idx.index)?
                ))
            }

            ExprKind::BinOp(bin) => {
                let mut lhs = self.render_expr(&bin.lhs)?;
                let mut rhs = self.render_expr(&bin.rhs)?;
                // Kotlin's `==`/`!=` (unlike `<`/`>`, which have cross-type
                // `compareTo` overloads) require matching numeric types — a
                // `.len()`-derived `Long` (see `is_len_on_list`'s `.toLong()`)
                // compared against a bare `Int` literal needs the literal
                // suffixed to match.
                if matches!(bin.kind, BinOpKind::Eq | BinOpKind::Ne) {
                    if lhs.ends_with(".toLong()") {
                        if let Some(suffixed) = int_literal_as_long(&rhs) {
                            rhs = suffixed;
                        }
                    } else if rhs.ends_with(".toLong()") {
                        if let Some(suffixed) = int_literal_as_long(&lhs) {
                            lhs = suffixed;
                        }
                    }
                }
                Ok(format!("({} {} {})", lhs, kotlin_bin_op(&bin.kind), rhs))
            }

            ExprKind::UnOp(un) => Ok(format!(
                "{}({})",
                kotlin_un_op(&un.op),
                self.render_expr(&un.val)?
            )),

            ExprKind::If(if_expr) => {
                let cond = self.render_expr(&if_expr.cond)?;
                // Always brace-wrap: `then`/`elze` can be a multi-statement block
                // (render_expr_single/render_expr on an ExprKind::Block renders
                // just the inner statements, no braces of its own), and Kotlin
                // accepts `if (c) { x } else { y }` as an expression too, so
                // wrapping unconditionally is safe even for the single-expression
                // case.
                let then_val = render_expr_single(&if_expr.then, self)?;
                if let Some(elze) = &if_expr.elze {
                    Ok(format!(
                        "if ({}) {{ {} }} else {{ {} }}",
                        cond,
                        then_val,
                        render_expr_single(elze, self)?
                    ))
                } else {
                    Ok(format!("if ({}) {{ {} }}", cond, then_val))
                }
            }

            ExprKind::Match(mt) => {
                let tmp = self.fresh_var("__m");
                self.writer.write_line(&format!("var {}: Any? = null", tmp));
                self.emit_match_stmt(mt, Tail::Assign(&tmp))?;
                Ok(tmp)
            }

            ExprKind::Block(block) => {
                let tmp = self.fresh_var("__b");
                self.writer.write_line(&format!("var {}: Any? = null", tmp));
                self.push_scope();
                let len = block.stmts.len();
                for (i, stmt) in block.stmts.iter().enumerate() {
                    let stmt_tail = if i == len - 1 {
                        Tail::Assign(&tmp)
                    } else {
                        Tail::None
                    };
                    self.emit_stmt(stmt, stmt_tail)?;
                }
                self.pop_scope();
                Ok(tmp)
            }

            ExprKind::Assign(assign) => Ok(format!(
                "{} = {}",
                self.render_expr(&assign.target)?,
                self.render_expr(&assign.value)?
            )),

            ExprKind::Struct(st) => {
                // `st.name` is an `ExprKind::Name` for every real enum-variant
                // constructor call — `render_expr`'s own `Name` arm already
                // resolves it fully (typed registry lookup, then the plain
                // structural join for anything else), so no separate
                // post-processing is needed here at all.
                let variant_name = self.render_expr(&st.name)?;
                let fields: Vec<String> = st
                    .fields
                    .iter()
                    .map(|f| {
                        // `None` means Rust field-init shorthand (`Field { name }` ≡ `Field { name: name }`),
                        // not an explicit null value.
                        let val = match &f.value {
                            Some(v) => self.render_expr(v)?,
                            None => f.name.name.clone(),
                        };
                        Ok(format!("{} = {}", f.name.name, val))
                    })
                    .collect::<Result<Vec<_>>>()?;
                Ok(format!("{}({})", variant_name, fields.join(", ")))
            }

            ExprKind::Array(arr) => {
                let items: Vec<String> = arr
                    .values
                    .iter()
                    .map(|v| self.render_expr(v))
                    .collect::<Result<Vec<_>>>()?;
                Ok(format!("listOf({})", items.join(", ")))
            }

            ExprKind::Tuple(tup) => {
                let items: Vec<String> = tup
                    .values
                    .iter()
                    .map(|v| self.render_expr(v))
                    .collect::<Result<Vec<_>>>()?;
                // Kotlin's built-in tuple constructors only go up to 3 elements.
                let ctor = match items.len() {
                    3 => "Triple",
                    _ => "Pair",
                };
                Ok(format!("{}({})", ctor, items.join(", ")))
            }

            ExprKind::Reference(r) => self.render_expr(&r.referee),
            ExprKind::Dereference(d) => self.render_expr(&d.referee),
            ExprKind::Cast(c) => {
                let inner = self.render_expr(&c.expr)?;
                let conv = match self.kotlin_type_from_ty(&c.ty).as_str() {
                    "Byte" => Some("toByte()"),
                    "Short" => Some("toShort()"),
                    "Int" => Some("toInt()"),
                    "Long" => Some("toLong()"),
                    "Float" => Some("toFloat()"),
                    "Double" => Some("toDouble()"),
                    "Char" => Some("toInt().toChar()"),
                    _ => None,
                };
                match conv {
                    Some(m) => Ok(format!("{}.{}", inner, m)),
                    None => Ok(inner),
                }
            }
            ExprKind::Paren(p) => Ok(format!("({})", self.render_expr(&p.expr)?)),

            ExprKind::Closure(cl) => {
                let params: Vec<String> = cl
                    .params
                    .iter()
                    .map(|p| {
                        let n = ident_from_pattern(p);
                        // An explicit `|c: char| ...` annotation, or a typechecker-resolved
                        // parameter type promoted by `HirToAstLifter`'s closure-lifting arm,
                        // both parse/lift as `PatternKind::Type` wrapping the ident pattern —
                        // there's no other `Pattern`-level type slot to fall back to anymore
                        // (the old ad hoc `Pattern.ty` cache field has been removed).
                        let ty_str = if let PatternKind::Type(pt) = &p.kind {
                            Some(self.kotlin_type_from_ty(&pt.ty))
                        } else {
                            None
                        };
                        if n.starts_with('(') {
                            // Destructuring lambda param (`{ (a, b) -> ... }`) — Kotlin doesn't
                            // support a blanket type annotation after the whole pattern here.
                            n
                        } else if n == "_" {
                            if let Some(ty) = ty_str {
                                format!("it: {}", ty)
                            } else {
                                "it: Any?".to_string()
                            }
                        } else if let Some(ty) = ty_str {
                            format!("{}: {}", n, ty)
                        } else {
                            format!("{}: Any?", n)
                        }
                    })
                    .collect();
                // `render_expr_single` on a `Block`/`Match` body has a side
                // effect: it writes hoisted `var __bN = ...` statements
                // straight to `self.writer`, the single shared output
                // stream, at whatever statement position is currently
                // open — normally correct for a body rendered directly
                // into the enclosing statement list, but wrong here, since
                // this closure's `{ params -> ... }` is built as an
                // in-memory string and spliced in later. Left alone, a
                // nested closure's own hoisted statements leak out into
                // the *enclosing* function/closure's statement stream,
                // landing outside this closure's braces entirely (and
                // ahead of the closure literal itself), rather than
                // inside them. Redirect the writer into a scratch buffer
                // for the duration of this one body's render so anything
                // hoisted stays scoped to this closure.
                self.writer.increase_indent();
                let saved = self.writer.swap_buffer(String::new());
                let value = render_expr_single(&cl.body, self);
                let hoisted = self.writer.swap_buffer(saved);
                self.writer.decrease_indent();
                let value = value?;
                let params = params.join(", ");
                if hoisted.trim().is_empty() {
                    Ok(format!("{{ {} -> {} }}", params, value))
                } else {
                    Ok(format!(
                        "{{ {} ->\n{}\n{} }}",
                        params,
                        hoisted.trim_end_matches('\n'),
                        value
                    ))
                }
            }

            ExprKind::Let(l) => Ok(format!(
                "val {} = {}",
                ident_from_pattern(&l.pat),
                self.render_expr(&l.expr)?
            )),

            ExprKind::Return(ret) => {
                if let Some(val) = &ret.value {
                    Ok(format!("return {}", self.render_expr(val)?))
                } else {
                    Ok("return".to_string())
                }
            }

            ExprKind::IntrinsicCall(ic) => {
                use fp_core::intrinsics::calls::IntrinsicKind;
                // Render all args first to avoid borrow conflicts
                let args: Vec<String> = ic
                    .args
                    .iter()
                    .map(|a| self.render_expr(a))
                    .collect::<Result<Vec<_>>>()?;

                match &ic.kind {
                    // A method-style intrinsic (`receiver.count()`, from self.g. a
                    // desugared `for` loop's length check) — NOT a plain
                    // function call. The generic fallback below (`name(args)`)
                    // would double up the parens `intrinsic_name` already
                    // includes for this one (`"count()"`), producing malformed
                    // `count()(receiver)`.
                    IntrinsicKind::Len => {
                        let receiver = args.first().cloned().unwrap_or_default();
                        Ok(format!("{}.count()", receiver))
                    }
                    // `OptionUnwrap`/`OptionSome`/`OptionNone`/`VecNew`/`AsRef`/
                    // `Iter`/`ToOwned`/`AsStr`/`Clone` never reach here:
                    // `KotlinMaterializer::materialize_call` (run over the
                    // lifted AST before serialization, see `compile_project`'s
                    // phase 2 in `fp-cli`) already rewrites those into their
                    // real Kotlin-shaped `Expr` upstream. The arms below stay
                    // here rather than in the materializer because they render
                    // straight to a Kotlin-specific string form
                    // (`?:`/`.toList()`/a string-template literal) that has no
                    // generic `ast::Expr` equivalent to return instead.
                    kind @ (IntrinsicKind::Format
                    | IntrinsicKind::Print
                    | IntrinsicKind::Println) => {
                        // Resolve each placeholder against its real argument and emit a
                        // genuine Kotlin string template, instead of a fake "arg" literal
                        // fed to `String.format(...)`.
                        let template = match ic.args.first().map(|a| a.kind()) {
                            Some(ExprKind::FormatString(fs)) => {
                                let value_args = &args[1..];
                                let mut next_implicit = 0usize;
                                let mut out = String::new();
                                for part in &fs.parts {
                                    match part {
                                        FormatTemplatePart::Literal(lit) => {
                                            out.push_str(&escape_str_for_kt(lit))
                                        }
                                        FormatTemplatePart::Placeholder(ph) => {
                                            match &ph.arg_ref {
                                                // `{name}` with no separate trailing
                                                // argument at all — Rust's inline-
                                                // captured-identifier format syntax,
                                                // which refers to a local variable
                                                // directly rather than indexing into
                                                // the macro's own argument list.
                                                FormatArgRef::Named(name)
                                                    if value_args.is_empty() =>
                                                {
                                                    out.push_str(&format!("${{{}}}", name));
                                                }
                                                FormatArgRef::Positional(i) => {
                                                    let val = value_args
                                                        .get(*i)
                                                        .cloned()
                                                        .unwrap_or_default();
                                                    out.push_str(&format!("${{{}}}", val));
                                                }
                                                FormatArgRef::Implicit | FormatArgRef::Named(_) => {
                                                    let i = next_implicit;
                                                    next_implicit += 1;
                                                    let val = value_args
                                                        .get(i)
                                                        .cloned()
                                                        .unwrap_or_default();
                                                    out.push_str(&format!("${{{}}}", val));
                                                }
                                            }
                                        }
                                    }
                                }
                                format!("\"{}\"", out)
                            }
                            _ => args.first().cloned().unwrap_or_default(),
                        };
                        match kind {
                            IntrinsicKind::Format => Ok(template),
                            IntrinsicKind::Print => Ok(format!("print({})", template)),
                            IntrinsicKind::Println => Ok(format!("println({})", template)),
                            _ => unreachable!(),
                        }
                    }
                    _ => {
                        let name = intrinsic_name(&ic.kind);
                        Ok(format!("{}({})", name, args.join(", ")))
                    }
                }
            }

            ExprKind::Range(r) => {
                let start = r.start.as_ref().map(|s| self.render_expr(s)).transpose()?;
                let end = r.end.as_ref().map(|s| self.render_expr(s)).transpose()?;
                Ok(match (start, end) {
                    (Some(s), Some(en)) => format!("{}..{}", s, en),
                    (Some(s), None) => format!("{}..", s),
                    (None, Some(en)) => format!("..{}", en),
                    (None, None) => "..".to_string(),
                })
            }

            ExprKind::FormatString(fs) => {
                let parts = fs
                    .parts
                    .iter()
                    .map(|p| match p {
                        FormatTemplatePart::Literal(lit) => Ok(escape_str_for_kt(lit)),
                        FormatTemplatePart::Placeholder(_ph) => {
                            let rendered = "arg".to_string();
                            Ok(format!("${{{}}}", rendered))
                        }
                    })
                    .collect::<Result<Vec<_>>>()?;
                Ok(format!("\"{}\"", parts.join("")))
            }

            ExprKind::Break(_) => Ok("break".to_string()),
            ExprKind::Continue(_) => Ok("continue".to_string()),

            ExprKind::Try(t) => {
                // `?` operator → just render inner expr (error handling is implicit)
                self.render_expr(&t.expr)
            }
            ExprKind::Macro(_m) => Ok("null".to_string()),
            ExprKind::ConstBlock(_) => Ok("null".to_string()),
            ExprKind::ArrayRepeat(ar) => {
                let elem = self.render_expr(&ar.elem)?;
                Ok(format!("listOf({})", elem))
            }
            ExprKind::Await(a) => self.render_expr(&a.base),

            _ => Ok(format!(
                "/* unreachable: {:?} */",
                std::mem::discriminant(expr.kind())
            )),
        }
    }
}

/// `render_expr` already handles `ExprKind::Block` correctly (via `emit_stmt`,
/// preserving every statement kind — `let`s, `for`-loops, everything, not
/// just bare `Expr` statements) — this is a thin alias kept for call-site
/// clarity at "this body must render as a single value" positions (an
/// if/else branch used as an expression), not a distinct implementation.
pub(super) fn render_expr_single(body: &BExpr, e: &mut KotlinEmitter) -> Result<String> {
    e.render_expr(body)
}

/// `Self` (constructor shorthand, `Self::other_fn`) or a leading `Self::`
/// path segment, swapped for the real class/enum name — see
/// `current_self_name`'s doc comment.
pub(super) fn substitute_self_prefix(raw: &str, self_name: &str) -> String {
    if raw == "Self" {
        self_name.to_string()
    } else if let Some(rest) = raw.strip_prefix("Self::") {
        format!("{self_name}::{rest}")
    } else {
        raw.to_string()
    }
}

/// The `name` extracted here is used purely to match against known
/// path-shaped special cases (e.g. `std::env::current_dir`, crate-prefix
/// stripping via `map_kt_path`) before falling through to a generic
/// `{name}({args})` call rendering — so a target this can't name has no
/// honest generic rendering either; erroring here is what stops that
/// fallthrough from silently emitting a callee-less `(args)`.
pub(super) fn invoke_name(target: &ExprInvokeTarget) -> Result<String> {
    match target {
        ExprInvokeTarget::Function(name) => Ok(name.to_string()),
        ExprInvokeTarget::Method(sel) => Ok(format!(".{}", sel.field.name)),
        other => Err(eyre::eyre!(
            "call target {other:?} is not yet supported in Kotlin output"
        )),
    }
}

/// Map a field/function name in a select expression to Kotlin equivalent.
pub(super) fn map_kt_field(name: &str) -> String {
    match name {
        "var" => "getenv".into(),
        "current_dir" => "getProperty".into(),
        _ => name.to_string(),
    }
}

/// Map a path-style function call (e.g., `PathBuf::from` or `std::path::PathBuf::from`)
/// to its Kotlin approximation by resolving type prefixes through KnownClass.
pub(super) fn map_kt_path(name: &str) -> String {
    // Type-qualified paths use `::` or `.` as separators
    if let Some(pos) = name.rfind("::") {
        let (prefix, method) = name.split_at(pos);
        let method = &method[2..];
        let normalized = prefix.replace("::", ".");

        // `Path::from`/`PathBuf::from`/`Path::new` → `java.nio.file.Path.of(...)` isn't
        // resolvable as a static-interface-method call in this position; use
        // `Paths.get(...)` instead (kt_import_for's StdPath arm imports both).
        let prefix_last = prefix.rsplit("::").next().unwrap_or(prefix);
        if matches!(method, "from" | "new")
            && KnownClass::from_source_type(prefix_last) == Some(KnownClass::Path)
        {
            return "Paths.get".to_string();
        }
        // `Vec::new()`/`HashSet::new()`/`HashMap::new()` — there's no Kotlin
        // class named `Vec`, and `HashSet`/`HashMap` don't have a portable
        // no-arg factory reachable this way (this falls through to generic
        // path resolution otherwise, producing unresolvable `HashSet.of()`);
        // the portable constructors are these top-level functions. (The
        // `"HashSet::new" => "mutableSetOf"` entry in `map_kt_method` below
        // is unreachable from here — this function always splits `::` before
        // calling it, so it only ever sees the bare method name.)
        if method == "new" && matches!(prefix_last, "Vec" | "HashSet" | "HashMap") {
            return match prefix_last {
                "Vec" => "mutableListOf",
                "HashSet" => "mutableSetOf",
                _ => "mutableMapOf",
            }
            .to_string();
        }

        let pkg = known_package(&normalized);
        // Drop prefix only for language-internal crates (not serialization libs)
        let skip_prefix = matches!(
            pkg,
            KnownPackage::ThisError
                | KnownPackage::Tracing
                | KnownPackage::AsyncTrait
                | KnownPackage::Anyhow
        );
        if skip_prefix {
            let kt_method = map_kt_method(method);
            if method.chars().next().map_or(false, |c| c.is_uppercase()) {
                return method.to_uppercase();
            }
            if kt_method.is_empty() {
                return String::new();
            }
            return kt_method;
        }
        let kt_prefix = map_name_to_kt(prefix);
        // `Type::new(...)` — a real constructor call (JDK type like
        // `ProcessBuilder`/`Arc`, or any user-defined struct's own `new`).
        // `Vec`/`HashSet`/`HashMap` (the only real static-factory-shaped
        // `new`s) are already intercepted above, so anything reaching here
        // is a genuine constructor — render it as `Type(args)`, not
        // `Type.new(args)`/`Type.of(args)` (neither of which exist).
        if method == "new" {
            return kt_prefix;
        }
        // Drop PascalCase type prefix for local-type static methods not in known mappings
        let is_local_type = prefix.chars().next().map_or(false, |c| c.is_uppercase())
            && !prefix.contains("::")
            && method.chars().next().map_or(false, |c| c.is_lowercase());
        let known_method = is_local_type && map_kt_method(method) == method;
        if is_local_type && known_method {
            return method.to_string();
        }
        if method.chars().next().map_or(false, |c| c.is_uppercase()) {
            // Enum-variant tuple-struct constructor (e.g.
            // `CoreError::GitCommand(...)`) — pass the variant name through
            // unchanged. This branch used to uppercase it, intending to
            // handle Rust unit-struct-as-constant calls, but that also fired
            // on ordinary enum-variant constructors, which have no all-caps
            // Kotlin convention.
            return format!("{}.{}", kt_prefix, method);
        }
        let kt_method = map_kt_method(method);
        return format!("{}.{}", kt_prefix, kt_method);
    }
    map_kt_method(name)
}

pub(super) fn map_kt_method(name: &str) -> String {
    // Portable method mappings (no Rust-specific names)
    match name {
        // Collecion constructors (portable). Note: a qualified `X::new` never
        // reaches here — `map_kt_path` always splits on `::` first and only
        // passes the bare method name — so this only matters for a bare
        // "Vec" (no `::`) reaching this function directly.
        "Vec" => "mutableListOf".into(),
        // Collection operations (portable names)
        "unwrap" | "expect" => "!!".into(),
        "is_empty" => "isEmpty()".into(),
        "push" => "add".into(),
        "pop" => "removeLast()".into(),
        "insert" => "add".into(),
        "len" => "length".into(),
        "lines" => "lines()".into(),
        "split" => "split".into(),
        "contains" => "contains".into(),
        "replace" => "replace".into(),
        "trim" => "trim()".into(),
        "to_uppercase" => "uppercase()".into(),
        "to_lowercase" => "lowercase()".into(),
        "starts_with" => "startsWith".into(),
        "ends_with" => "endsWith".into(),
        "rfind" => "lastIndexOf".into(),
        "clone" => "copy()".into(),
        "from" => "of".into(),
        "new" => "of".into(),
        "into" => "".into(),
        "var" => "getenv".into(),
        "current_dir" => "currentDir()".into(),
        "to_string_lossy" => "toString()".into(),
        "to_string" => "toString()".into(),
        "unwrap_or_else" => "let".into(),
        "split_once" => "split".into(),
        "or_else" => "run".into(),
        "display" => "toString()".into(),
        // Property access, not a method call — java.nio.file.Path has no
        // zero-arg `fileName()` member (only the synthetic `fileName` property).
        "file_name" => "fileName".into(),
        "to_str" => "toString()".into(),
        "join" => "resolve".into(),
        "strip_prefix" => "removePrefix".into(),
        "strip_suffix" => "removeSuffix".into(),
        "trim_end_matches" => "removeSuffix".into(),
        "unwrap_or" => "".into(),
        // `Option<T>::as_ref()` -> `Option<&T>` has no Kotlin equivalent step —
        // nullable types don't need one — so drop the call entirely.
        "as_ref" => "".into(),
        "as_bytes" => "toByteArray()".into(),
        "map" => "let".into(),
        "parse_next" => "parse".into(),
        "verify" => "also".into(),
        "take_while" => "filter".into(),
        "is_ascii_alphanumeric" => "isLetterOrDigit()".into(),
        "is_ascii_hexdigit" => "isDigit()".into(),
        "is_whitespace" => "isWhitespace()".into(),
        "all" => "all".into(),
        // Kotlin's `CharSequence` already has `.all { c: Char -> ... }`, `.map`, etc.
        // directly — no `.chars()` step needed (that maps to a Java `IntStream`).
        "chars" => "".into(),
        // Kotlin collections are already directly iterable/chainable — no explicit
        // iterator-producing step needed before `.map`/`.filter`/`.find`/etc.
        "iter" => "".into(),
        "iter_mut" => "".into(),
        "into_iter" => "".into(),
        // Kotlin's `.map`/`.filter`/etc. already return a concrete `List`, unlike
        // Rust's lazy iterators — no separate materializing `.collect()` step needed.
        "collect" => "".into(),
        // `Iterator::sum()` — a genuine zero-arg Kotlin function, needs real parens
        // (unlike the property-style zero-arg methods erased above).
        "sum" => "sum()".into(),
        "from_millis" => "ofMillis".into(),
        "from_secs" => "ofSeconds".into(),
        "is_ascii_alphabetic" => "isLetter()".into(),
        "is_ascii_digit" => "isDigit()".into(),
        "wrapping_mul" => "times".into(),
        "write_all" => "write".into(),
        "read_to_string" => "readText".into(),
        "remove_file" => "delete".into(),
        "is_alive" => "isAlive".into(),
        "kill_process" => "destroy".into(),
        "sleep" => "Thread.sleep".into(),
        "next" => "".into(),
        "clamp" => "coerceIn".into(),
        _ => name.replace("::", "."),
    }
}

/// Check if a pattern is a wildcard/default (else) arm.
pub(super) fn is_else_arm(pat: &Option<fp_core::ast::BPattern>) -> bool {
    match pat {
        None => true,
        Some(p) => match &p.kind {
            PatternKind::Wildcard(_) => true,
            // Err(_) is also a catch-all arm
            PatternKind::TupleStruct(ts) => {
                let raw = ts.name.to_string();
                let simple = raw.rsplit("::").next().unwrap_or(&raw);
                (simple == "Err" || simple == "None")
                    && ts
                        .patterns
                        .iter()
                        .all(|inner| matches!(&inner.kind, PatternKind::Wildcard(_)))
            }
            // `None` with no parens (a unit variant, not a tuple-struct
            // shape) parses as a bare `Variant` pattern instead.
            PatternKind::Variant(v) if v.pattern.is_none() => match v.name.kind() {
                ExprKind::Name(name) => {
                    let raw = name.to_string();
                    raw.rsplit("::").next().unwrap_or(&raw) == "None"
                }
                _ => false,
            },
            _ => false,
        },
    }
}

/// The enum's own bare declared name for an enum-variant VALUE expression,
/// derived from the expression's real resolved type (`Ty::Expr` wrapping the
/// real, `DefPath`-derived path `HirToAstLifter::def_id_to_ty` builds from
/// the type-checker's own resolved `DefId` — never derived from this
/// particular use's own path text). A `DefPath`'s own declaring segment is
/// structurally always last (module segments only ever precede it), so the
/// last path segment reliably names the real enum regardless of how many
/// module segments precede it — no position-counting/guessing needed.
pub(super) fn enum_name_from_ty(ty: Option<&Ty>) -> Option<String> {
    match ty? {
        Ty::Enum(en) => Some(en.name.name.clone()),
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Name(fp_core::ast::Name::Path(p)) => {
                p.segments.last().map(|s| s.name.clone())
            }
            ExprKind::Name(fp_core::ast::Name::Ident(id)) => Some(id.name.clone()),
            _ => None,
        },
        _ => None,
    }
}

/// Renders an ordinary (non-enum-variant) qualified name from its real
/// segments — no `::`/`.` text search-and-replace — swapping a literal
/// `Self` first segment for the real class/enum name (see
/// `current_self_name`'s doc comment) by checking/replacing that one real
/// `Ident`, not by substring-searching the joined text.
pub(super) fn qualified_name_with_self(
    name: &fp_core::ast::Name,
    self_name: Option<&str>,
) -> String {
    match name {
        fp_core::ast::Name::Ident(id) => {
            if id.name == "Self" {
                self_name.unwrap_or(id.name.as_str()).to_string()
            } else {
                id.name.clone()
            }
        }
        fp_core::ast::Name::Path(p) => {
            let mut segments: Vec<String> = p.segments.iter().map(|s| s.name.clone()).collect();
            if let (Some(first), Some(sn)) = (segments.first_mut(), self_name) {
                if first == "Self" {
                    *first = sn.to_string();
                }
            }
            segments.join(".")
        }
        _ => name_to_string(name),
    }
}

/// Resolves `raw_name` (a pattern's own qualified source path, e.g.
/// `"GitRefNode::Branch"` or a bare `"Branch"` when the variant was
/// brought into scope via `use`) to its real Kotlin sealed-subclass path,
/// by looking up `e.enum_variant_names` — the registry `emit_enum` itself
/// populated from the enum's own definition — instead of re-deriving a
/// name via string manipulation of the pattern text. Falls back to a
/// best-effort dotted-path guess only when the registry has no entry
/// (e.g. the defining enum lives in a package this file's workspace-wide
/// scan didn't cover), so a genuinely unresolvable case still degrades
/// gracefully rather than panicking.
pub(super) fn resolve_variant_kotlin_path(e: &KotlinEmitter, raw_name: &str) -> String {
    let segments: Vec<&str> = raw_name.split("::").flat_map(|s| s.split('.')).collect();
    let variant_name = segments.last().copied().unwrap_or(raw_name);
    let enum_name = if segments.len() >= 2 {
        segments[segments.len() - 2]
    } else {
        e.current_self_name.as_deref().unwrap_or("")
    };
    if let Some(kotlin_variant) = e
        .enum_variant_names
        .get(enum_name)
        .and_then(|variants| variants.get(variant_name))
    {
        return format!("{}.{}", enum_name, kotlin_variant);
    }
    // No registry entry — join the segments already split out above; no
    // separate re-derivation from the raw, `::`-joined string.
    segments.join(".")
}

pub(super) fn render_match_pat(pat: &Option<fp_core::ast::BPattern>, e: &KotlinEmitter) -> String {
    match pat {
        Some(p) => match &p.kind {
            PatternKind::Ident(id) => id.ident.name.clone(),
            PatternKind::Wildcard(_) => "else".to_string(),
            PatternKind::Struct(s) => s
                .fields
                .iter()
                .map(|f| f.name.name.clone())
                .collect::<Vec<_>>()
                .join(", "),
            PatternKind::Tuple(t) => t
                .patterns
                .iter()
                .map(|p| render_match_pat(&Some(Box::new(p.clone())), e))
                .collect::<Vec<_>>()
                .join(", "),
            PatternKind::TupleStruct(ts) => {
                let raw_name = ts.name.to_string();
                let simple_name = raw_name.rsplit("::").next().unwrap_or(&raw_name);
                // Portable monadic wrappers (Option/Result) — strip to just the binding
                if matches!(simple_name, "Ok" | "Err" | "Some" | "None") {
                    if ts.patterns.is_empty() {
                        return "null".to_string();
                    }
                    return ts
                        .patterns
                        .iter()
                        .map(|p| render_match_pat(&Some(Box::new(p.clone())), e))
                        .collect::<Vec<_>>()
                        .join(", ");
                }
                let variant_name = resolve_variant_kotlin_path(e, &raw_name);
                let inner = ts
                    .patterns
                    .iter()
                    .map(|p| render_match_pat(&Some(Box::new(p.clone())), e))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", variant_name, inner)
            }
            // A bare qualified path pattern (`ChangesLineKind::Add`, no `(...)`)
            // parses as a "literal" `Variant` pattern (see
            // `parse_literal_pattern_expr` in fp-lang), not `TupleStruct` —
            // render it the same way `ExprKind::Name` renders a path
            // expression elsewhere (dotted + uppercased last segment, e.g.
            // `ChangesLineKind.ADD`). Plain literal values (ints/strings/etc,
            // also routed through `Variant` by the same parser rule) render
            // via `render_value` instead.
            PatternKind::Variant(v) => {
                let variant = match v.name.kind() {
                    ExprKind::Name(name) => resolve_variant_kotlin_path(e, &name.to_string()),
                    ExprKind::Value(val) => render_value(val),
                    _ => return "else".to_string(),
                };
                match &v.pattern {
                    Some(inner) => {
                        format!("{}({})", variant, render_match_pat(&Some(inner.clone()), e))
                    }
                    None => variant,
                }
            }
            _ => "else".to_string(),
        },
        None => "else".to_string(),
    }
}

/// If this pattern is `Enum::Variant(binding)` for a single-field, NON-monadic
/// variant (not Some/Ok/Err/None — those get the null-check fast path instead),
/// return the Kotlin sealed-subclass path (`Enum.VARIANT`) and the binding name.
/// Requires the pattern to be written with its enum-qualified path.
pub(super) fn non_monadic_tuple_variant(
    e: &KotlinEmitter,
    pat: &Option<fp_core::ast::BPattern>,
) -> Option<(String, String)> {
    let p = pat.as_ref()?;
    let PatternKind::TupleStruct(ts) = &p.kind else {
        return None;
    };
    if ts.patterns.len() != 1 {
        return None;
    }
    let raw = ts.name.to_string();
    if !raw.contains("::") {
        return None;
    }
    let simple = raw.rsplit("::").next().unwrap_or(&raw);
    if matches!(simple, "Some" | "Ok" | "Err" | "None") {
        return None;
    }
    let binding = match &unwrap_ref_pattern(&ts.patterns[0]).kind {
        PatternKind::Ident(id) => id.ident.name.clone(),
        _ => return None,
    };
    let variant_path = resolve_variant_kotlin_path(e, &raw);
    Some((variant_path, binding))
}

/// Strip `ref`/`ref mut` wrapper patterns (`Some(ref mut file)`) to get at the
/// underlying binding — these carry no Kotlin-relevant information (there's no
/// by-reference-vs-by-value binding distinction), but the binding-extraction
/// helpers below need to see through them to find the `Ident`/`Struct` pattern.
pub(super) fn unwrap_ref_pattern(pat: &Pattern) -> &Pattern {
    match &pat.kind {
        PatternKind::Ref(r) => unwrap_ref_pattern(&r.pattern),
        _ => pat,
    }
}

/// Extract the inner binding name from a Some/Ok/Err TupleStruct pattern. A nested
/// tuple pattern (`Some((host, path))`) is returned as `"(host, path)"` — callers
/// distinguish this from a single binding by checking for a leading `(`.
pub(super) fn stripped_tuple_binding(pat: &Option<fp_core::ast::BPattern>) -> Option<String> {
    match pat {
        Some(p) => match &p.kind {
            PatternKind::TupleStruct(ts) => {
                if ts.patterns.len() == 1 {
                    match &unwrap_ref_pattern(&ts.patterns[0]).kind {
                        PatternKind::Ident(id) => Some(id.ident.name.clone()),
                        PatternKind::Tuple(t) => {
                            let names: Vec<String> = t
                                .patterns
                                .iter()
                                .map(|p| match &p.kind {
                                    PatternKind::Ident(id) => Some(id.ident.name.clone()),
                                    _ => None,
                                })
                                .collect::<Option<Vec<_>>>()?;
                            Some(format!("({})", names.join(", ")))
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            _ => None,
        },
        None => None,
    }
}

/// Extract a binding variable name from the first struct field of a match arm.
pub(super) fn match_case_binding(pat: &Option<fp_core::ast::BPattern>) -> Option<String> {
    match pat {
        Some(p) => match &unwrap_ref_pattern(p).kind {
            PatternKind::Ident(id) => Some(id.ident.name.clone()),
            PatternKind::Struct(s) => s.fields.first().map(|f| f.name.name.clone()),
            _ => None,
        },
        None => None,
    }
}

// ── Value ────────────────────────────────────────────────────────────────────

/// Render a u64 literal that may exceed `Long.MAX_VALUE`, reinterpreting the bit
/// pattern via Kotlin's unsigned-to-signed conversion (matches Rust's `as i64` cast
/// semantics used by hash/checksum constants).
/// If `s` is a bare integer literal (optionally negative), returns it with
/// a Kotlin `L` (`Long`) suffix — used to match a `.len()`-derived `Long`
/// on the other side of an `==`/`!=` comparison. Only a plain literal
/// qualifies; any other rendered expression is left alone.
pub(super) fn int_literal_as_long(s: &str) -> Option<String> {
    let digits = s.strip_prefix('-').unwrap_or(s);
    if !digits.is_empty() && digits.bytes().all(|b| b.is_ascii_digit()) {
        Some(format!("{s}L"))
    } else {
        None
    }
}

pub(super) fn render_int_literal_kt(v: u64) -> String {
    if v > i64::MAX as u64 {
        format!("{}UL.toLong()", v)
    } else {
        v.to_string()
    }
}

/// Rust's default `f64`/decimal formatting drops the fractional part for
/// whole-number floats (`0.0.to_string() == "0"`), which Kotlin then parses
/// as an `Int` literal instead of a `Double` — cascading into type
/// mismatches everywhere that value flows. Force a decimal point onto any
/// formatted value that would otherwise read as an integer literal.
pub(super) fn format_kt_decimal_literal(s: String) -> String {
    if s.contains('.')
        || s.contains('e')
        || s.contains('E')
        || s.contains("NaN")
        || s.contains("inf")
    {
        s
    } else {
        format!("{}.0", s)
    }
}

pub(super) fn render_value(val: &Value) -> String {
    match val {
        Value::Bool(v) => v.value.to_string(),
        Value::Int(v) => v.value.to_string(),
        Value::UInt(v) => render_int_literal_kt(v.value),
        Value::BigInt(v) => {
            let s = v.value.to_string();
            if !s.starts_with('-') && s.parse::<i64>().is_err() {
                format!("{}UL.toLong()", s)
            } else {
                s
            }
        }
        Value::Decimal(v) => format_kt_decimal_literal(v.value.to_string()),
        Value::BigDecimal(v) => format_kt_decimal_literal(v.value.to_string()),
        Value::Char(v) => format!("'{}'", escape_char_for_kt(v.value)),
        Value::String(v) => format!("\"{}\"", escape_str_for_kt(&v.value)),
        Value::Unit(_) | Value::Null(_) | Value::None(_) => "null".to_string(),
        Value::Some(v) => render_value(&v.value),
        Value::Option(v) => v
            .value
            .as_ref()
            .map(|i| render_value(i))
            .unwrap_or_else(|| "null".to_string()),
        Value::List(l) => {
            let items: Vec<String> = l.values.iter().map(|v| render_value(v)).collect();
            format!("listOf({})", items.join(", "))
        }
        Value::Map(m) => {
            let entries: Vec<String> = m
                .entries
                .iter()
                .map(|e| format!("{} to {}", render_value(&e.key), render_value(&e.value)))
                .collect();
            format!("mapOf({})", entries.join(", "))
        }
        Value::Tuple(t) => {
            let items: Vec<String> = t.values.iter().map(|v| render_value(v)).collect();
            format!("Pair({})", items.join(", "))
        }
        _ => "null".to_string(),
    }
}

pub(super) fn escape_char_for_kt(c: char) -> String {
    match c {
        '\'' => "\\'".to_string(),
        '\\' => "\\\\".to_string(),
        '\n' => "\\n".to_string(),
        '\r' => "\\r".to_string(),
        '\t' => "\\t".to_string(),
        c if c.is_ascii_graphic() || c == ' ' => c.to_string(),
        c => format!("\\u{:04X}", c as u32),
    }
}

pub(super) fn escape_str_for_kt(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            // Unicode escapes: convert raw unicode chars to Kotlin \\uXXXX
            c if !c.is_ascii_graphic() && c != ' ' => {
                out.push_str(&format!("\\u{:04X}", c as u32));
            }
            _ => out.push(c),
        }
    }
    out
}

// ── Operators ────────────────────────────────────────────────────────────────

pub(super) fn kotlin_bin_op(kind: &BinOpKind) -> &str {
    match kind {
        BinOpKind::Add => "+",
        BinOpKind::Sub => "-",
        BinOpKind::Mul => "*",
        BinOpKind::Div => "/",
        BinOpKind::Mod => "%",
        BinOpKind::And | BinOpKind::BitAnd => "&&",
        BinOpKind::Or | BinOpKind::BitOr => "||",
        BinOpKind::Eq => "==",
        BinOpKind::Ne => "!=",
        BinOpKind::Lt => "<",
        BinOpKind::Gt => ">",
        BinOpKind::Le => "<=",
        BinOpKind::Ge => ">=",
        BinOpKind::Shl => "shl",
        BinOpKind::Shr => "shr",
        BinOpKind::BitXor => "xor",
        _ => "?",
    }
}

pub(super) fn kotlin_un_op(kind: &UnOpKind) -> &str {
    match kind {
        UnOpKind::Not => "!",
        UnOpKind::Neg => "-",
        UnOpKind::Deref => "*",
        _ => "?",
    }
}

pub(super) fn intrinsic_name(kind: &fp_core::intrinsics::calls::CallKind) -> String {
    use fp_core::intrinsics::calls::IntrinsicKind;
    match kind {
        IntrinsicKind::Print => "print".into(),
        IntrinsicKind::Println => "println".into(),
        IntrinsicKind::Format => "String.format".into(),
        IntrinsicKind::Len => "count()".into(),
        IntrinsicKind::Panic => "error".into(),
        _ => format!("intr_{:?}", kind).to_lowercase(),
    }
}

// ── Type mapping ─────────────────────────────────────────────────────────────
