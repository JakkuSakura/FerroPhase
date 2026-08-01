use crate::*;
use fp_core::hir::*;
use fp_core::error::Result;
use fp_core::module::path::*;
use fp_core::span::Span;
use std::collections::{HashMap, HashSet};

impl HirTypeInferencer {
    pub(crate) fn heap_inner_ty<'a>(&self, ty: &'a Ty) -> Option<&'a Ty> {
        match ty {
            Ty::Reference(reference) => Some(&reference.ty),
            Ty::Vec(vec) => Some(&vec.ty),
            Ty::Expr(expr) => {
                let ExprKind::Name(Name::ParameterPath(path)) = expr.kind() else {
                    return None;
                };
                let segment = path.segments.last()?;
                if segment.args.len() != 1 {
                    return None;
                }
                match segment.ident.as_str() {
                    "Box" | "Arc" | "Rc" | "Weak" | "Vec" => Some(&segment.args[0]),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub(crate) fn struct_name_variants(&self, name: &str) -> Vec<QualifiedPath> {
        let parsed = parse_path(name).ok();
        let (segments, is_unqualified) = match parsed {
            Some(parsed) => {
                let is_unqualified =
                    parsed.prefix == PathPrefix::Plain && parsed.segments.len() == 1;
                (parsed.segments, is_unqualified)
            }
            None => (vec![name.to_string()], true),
        };
        let name_path = QualifiedPath::new(segments);
        self.struct_name_variants_for_path(&name_path, is_unqualified)
    }

    pub(crate) fn struct_name_variants_for_path(
        &self,
        name_path: &QualifiedPath,
        is_unqualified: bool,
    ) -> Vec<QualifiedPath> {
        // A single shared borrow for this whole (synchronous, no re-entrant
        // `self` calls) function -- simpler than repeating `self.inner
        // .borrow()` at each of the several `module_path`/`root_modules`
        // reads below.
        let inner = self.inner.borrow();
        let mut names = Vec::new();
        let mut seen = HashSet::new();
        let push = |value: QualifiedPath,
                    names: &mut Vec<QualifiedPath>,
                    seen: &mut HashSet<QualifiedPath>| {
            if seen.insert(value.clone()) {
                names.push(value);
            }
        };

        if !inner.module_path.is_empty() && is_unqualified {
            if let Some(head) = name_path.head() {
                push(
                    inner.module_path.with_segment(head.to_string()),
                    &mut names,
                    &mut seen,
                );
                if let Some(module_head) = inner.module_path.head() {
                    if inner.root_modules.contains(module_head) {
                        if inner.module_path.segments.len() > 1 {
                            let mut segments = Vec::with_capacity(inner.module_path.segments.len());
                            segments.extend(inner.module_path.segments.iter().skip(1).cloned());
                            segments.push(head.to_string());
                            push(QualifiedPath::new(segments), &mut names, &mut seen);
                        }
                    } else {
                        for root in &inner.root_modules {
                            let mut segments =
                                Vec::with_capacity(inner.module_path.segments.len() + 2);
                            segments.push(root.to_string());
                            segments.extend(inner.module_path.segments.iter().cloned());
                            segments.push(head.to_string());
                            push(QualifiedPath::new(segments), &mut names, &mut seen);
                        }
                    }
                }
            }
        }
        push(name_path.clone(), &mut names, &mut seen);

        if name_path.segments.len() > 1 {
            if let Some(tail) = name_path.tail() {
                push(
                    QualifiedPath::new(vec![tail.to_string()]),
                    &mut names,
                    &mut seen,
                );
            }
        }

        names
    }

    pub(crate) async fn lookup_struct_def_by_name(
        &self,
        name: &str,
    ) -> Option<(QualifiedPath, TypeStruct)> {
        if name == "TypeBuilder" && std::env::var("FP_DEBUG_TYPEBUILDER").is_ok() {
            let keys = self
                .own_struct_defs()
                .keys()
                .filter(|key| key.tail() == Some("TypeBuilder"))
                .cloned()
                .collect::<Vec<_>>();
            eprintln!(
                "debug TypeBuilder: module_path={:?} keys={:?}",
                self.inner.borrow().module_path,
                keys
            );
        }
        let parsed = parse_path(name).ok();
        let segments = parsed
            .map(|parsed| parsed.segments)
            .unwrap_or_else(|| vec![name.to_string()]);
        let name_path = QualifiedPath::new(segments.clone());

        if let Some(def) = self.own_struct_defs().get(&name_path).cloned() {
            return Some((name_path, def));
        }
        if let Some(def) = self.typing_ctx.env_ctx.find_struct(&name_path) {
            return Some((name_path, def));
        }
        if let Some(stripped) = Self::strip_std_prefix(&name_path) {
            if let Some(def) = self.own_struct_defs().get(&stripped).cloned() {
                return Some((stripped, def));
            }
        }
        if !self.inner.borrow().module_path.is_empty() && segments.len() == 1 {
            let qualified = self
                .inner
                .borrow()
                .module_path
                .with_segment(segments[0].clone());
            if let Some(def) = self.own_struct_defs().get(&qualified).cloned() {
                return Some((qualified, def));
            }
        }
        if segments.len() > 1 {
            return None;
        }
        let mut match_key = None;
        for key in self.own_struct_defs().keys() {
            if key.tail() == Some(name) {
                if match_key.is_some() {
                    return None;
                }
                match_key = Some(key.clone());
            }
        }
        if let Some(key) = match_key {
            return self
                .own_struct_defs()
                .get(&key)
                .cloned()
                .map(|def| (key, def));
        }
        let mut match_key = None;
        for (key, def) in self.own_struct_defs().iter() {
            if def.name.as_str() == name {
                if match_key.is_some() {
                    return None;
                }
                match_key = Some(key.clone());
            }
        }
        if let Some(key) = match_key {
            return self
                .own_struct_defs()
                .get(&key)
                .cloned()
                .map(|def| (key, def));
        }
        // Also check the workspace — a bare/ambiguous-locally name may still
        // resolve unambiguously to a single cross-crate struct.
        let mut match_key = None;
        for krate in self.typing_ctx.env_ctx.crates().values() {
            for key in krate.borrow().struct_defs.keys() {
                if key.tail() == Some(name) {
                    if match_key.is_some() {
                        return None;
                    }
                    match_key = Some(key.clone());
                }
            }
        }
        if let Some(key) = match_key {
            if let Some(def) = self.typing_ctx.env_ctx.find_struct(&key) {
                return Some((key, def));
            }
        }
        for candidate in self.struct_name_variants(name) {
            if let Some(var) = self.lookup_env_var(&candidate.to_key()).await {
                if let Ok(ty) = self.resolve_to_ty(var).await {
                    if let Ty::Struct(def) = ty {
                        return Some((candidate, def));
                    }
                }
            }
        }
        None
    }

    pub(crate) fn lookup_enum_def_by_name(&self, name: &str) -> Option<(QualifiedPath, TypeEnum)> {
        let parsed = parse_path(name).ok();
        let segments = parsed
            .map(|parsed| parsed.segments)
            .unwrap_or_else(|| vec![name.to_string()]);
        let name_path = QualifiedPath::new(segments.clone());

        if let Some(def) = self.own_enum_defs().get(&name_path).cloned() {
            return Some((name_path, def));
        }
        // Cross-crate counterpart to `lookup_struct_def_by_name`'s own
        // `env_ctx.find_struct` check -- without this, an enum defined in
        // another already-loaded crate (e.g. `std::option::Option`,
        // `std::result::Result`) can never be found by bare name, since
        // `own_enum_defs()` only holds the crate currently being typed.
        if let Some(def) = self.typing_ctx.env_ctx.find_enum(&name_path) {
            return Some((name_path, def));
        }
        if !self.inner.borrow().module_path.is_empty() && segments.len() == 1 {
            let qualified = self
                .inner
                .borrow()
                .module_path
                .with_segment(segments[0].clone());
            if let Some(def) = self.own_enum_defs().get(&qualified).cloned() {
                return Some((qualified, def));
            }
        }
        if segments.len() > 1 {
            return None;
        }
        let mut match_key = None;
        for key in self.own_enum_defs().keys() {
            if key.tail() == Some(name) {
                if match_key.is_some() {
                    return None;
                }
                match_key = Some(key.clone());
            }
        }
        if let Some(key) = match_key {
            return self
                .own_enum_defs()
                .get(&key)
                .cloned()
                .map(|def| (key, def));
        }
        let mut match_key = None;
        for (key, def) in self.own_enum_defs().iter() {
            if def.name.as_str() == name {
                if match_key.is_some() {
                    return None;
                }
                match_key = Some(key.clone());
            }
        }
        match_key.and_then(|key| {
            self.own_enum_defs()
                .get(&key)
                .cloned()
                .map(|def| (key, def))
        })
    }

    pub(crate) fn record_function_signature(
        &self,
        name: &Ident,
        sig: &FunctionSignature,
        item_id: fp_core::hir::ItemId,
    ) {
        let candidates = if self.inner.borrow().module_path.is_empty() {
            vec![QualifiedPath::new(vec![name.as_str().to_string()])]
        } else {
            vec![self
                .inner
                .borrow()
                .module_path
                .with_segment(name.as_str().to_string())]
        };
        for candidate in candidates {
            self.own_function_sigs_mut()
                .insert(candidate.clone(), sig.clone());
            self.own_function_item_ids_mut().insert(candidate, item_id);
        }
    }

    pub(crate) fn record_extern_function_signature(&self, name: &Ident, sig: &FunctionSignature) {
        let candidates = if self.inner.borrow().module_path.is_empty() {
            vec![QualifiedPath::new(vec![name.as_str().to_string()])]
        } else {
            vec![self
                .inner
                .borrow()
                .module_path
                .with_segment(name.as_str().to_string())]
        };
        for candidate in candidates {
            self.inner
                .borrow_mut()
                .extern_function_signatures
                .insert(candidate, sig.clone());
        }
    }

    pub(crate) fn record_unimplemented_symbol(&self, name: &Ident, attrs: &[Attribute]) {
        if !attrs_has_name(attrs, "unimplemented") {
            return;
        }
        let candidates = if self.inner.borrow().module_path.is_empty() {
            vec![QualifiedPath::new(vec![name.as_str().to_string()])]
        } else {
            vec![self
                .inner
                .borrow()
                .module_path
                .with_segment(name.as_str().to_string())]
        };
        for candidate in candidates {
            self.inner
                .borrow_mut()
                .unimplemented_symbols
                .insert(candidate);
        }
    }

    pub(crate) fn is_unimplemented_name(&self, name: &QualifiedPath) -> bool {
        self.inner.borrow().unimplemented_symbols.contains(name)
    }

    pub(crate) fn env_contains(&self, key: &str) -> bool {
        self.inner
            .borrow()
            .env
            .iter()
            .rev()
            .any(|scope| scope.contains_key(key))
    }

    pub(crate) fn scope_contains_non_module(&self, name: &str) -> bool {
        let inner = self.inner.borrow();
        let module_depth = *inner.module_scope_depths.last().unwrap_or(&0);
        inner
            .env
            .iter()
            .enumerate()
            .rev()
            .any(|(idx, scope)| idx > module_depth && scope.contains_key(name))
    }

    pub(crate) fn item_exists_path(&self, path: &QualifiedPath) -> bool {
        let key = path.to_key();
        self.own_struct_defs().contains_key(path)
            || self.own_enum_defs().contains_key(path)
            || self.own_function_sigs().contains_key(path)
            || self
                .inner
                .borrow()
                .extern_function_signatures
                .contains_key(path)
            || self.own_trait_defs().contains(path)
            || self.inner.borrow().unimplemented_symbols.contains(path)
            || self.env_contains(&key)
            || self.typing_ctx.env_ctx.find_struct(path).is_some()
            || self.typing_ctx.env_ctx.find_enum(path).is_some()
            || self.typing_ctx.env_ctx.find_function_sig(path).is_some()
    }

    pub(crate) fn resolve_name_key(&self, name: &Name) -> Option<QualifiedPath> {
        if let Some(qualified) = self.resolve_alias_name(name) {
            return Some(qualified);
        }
        let parsed = self.resolution_parsed_path(name)?;
        let found = {
            let inner = self.inner.borrow();
            resolve_item_path(
                &parsed,
                &inner.module_path,
                &inner.root_modules,
                &inner.extern_prelude,
                &inner.module_defs,
                |candidate| self.item_exists_path(candidate),
                |name| self.scope_contains_non_module(name),
            )
        };
        if let Some(qualified) = found {
            return Some(qualified);
        }
        // Fallback: try the raw segments as a fully-qualified path
        let raw = QualifiedPath::new(parsed.segments);
        if self.item_exists_path(&raw) {
            return Some(raw);
        }
        None
    }

    pub(crate) fn resolve_segments_key(
        &self,
        prefix: PathPrefix,
        segments: &[String],
    ) -> Option<QualifiedPath> {
        if segments.is_empty() && matches!(prefix, PathPrefix::Plain | PathPrefix::Root) {
            return None;
        }
        if matches!(prefix, PathPrefix::Plain) && !segments.is_empty() {
            if let Some(symbol_path) = self.lookup_symbol_alias(&segments[0]) {
                return Some(symbol_path.join(&segments[1..]));
            }
            if let Some(module_path) = self.lookup_module_alias(&segments[0]) {
                return Some(module_path.join(&segments[1..]));
            }
        }
        let parsed = ParsedPath {
            prefix,
            segments: segments.to_vec(),
        };
        let qualified = {
            let inner = self.inner.borrow();
            resolve_item_path(
                &parsed,
                &inner.module_path,
                &inner.root_modules,
                &inner.extern_prelude,
                &inner.module_defs,
                |candidate| self.item_exists_path(candidate),
                |name| self.scope_contains_non_module(name),
            )
        }?;
        Some(qualified)
    }

    pub(crate) fn check_unimplemented_name(&self, name: &Name) -> bool {
        if let Some(ident) = name.as_ident() {
            if !self.inner.borrow().module_path.is_empty() {
                let candidate = self
                    .inner
                    .borrow()
                    .module_path
                    .with_segment(ident.as_str().to_string());
                if self.is_unimplemented_name(&candidate) {
                    if !self.is_same_crate_path(&candidate) {
                        self.emit_warning(format!(
                            "use of unimplemented item: {}",
                            candidate.to_key()
                        ));
                    }
                    return false;
                }
            }
        }
        let Some(candidate) = self.resolve_name_key(name) else {
            return false;
        };
        if self.is_unimplemented_name(&candidate) {
            if !self.is_same_crate_path(&candidate) {
                self.emit_warning(format!("use of unimplemented item: {}", candidate.to_key()));
            }
            return false;
        }
        false
    }

    pub(crate) fn is_same_crate_path(&self, candidate: &QualifiedPath) -> bool {
        let Some(current_root) = self
            .inner
            .borrow()
            .module_path
            .head()
            .map(|s| s.to_string())
        else {
            return false;
        };
        candidate.head() == Some(current_root.as_str())
    }

    pub(crate) fn lookup_function_signature(&self, name: &Name) -> Option<FunctionSignature> {
        let candidate = self
            .resolve_name_key(name)
            .or_else(|| self.fallback_name_key(name))?;
        if let Some(sig) = self
            .inner
            .borrow()
            .extern_function_signatures
            .get(&candidate)
        {
            return Some(sig.clone());
        }
        if let Some(sig) = self.own_function_sigs().get(&candidate) {
            return Some(sig.clone());
        }
        self.lookup_stripped_function_signature(&candidate)
            .or_else(|| self.lookup_prefixed_function_signature(&candidate))
            .or_else(|| {
                self.name_tail(name)
                    .and_then(|name| self.lookup_function_signature_by_name(&name))
            })
    }

    /// Suspends once (via `await_package`) if the first attempt fails and
    /// the name's head names a registered-but-unloaded package, then
    /// retries the whole lookup -- mirrors `lookup_struct`'s suspend/retry
    /// shape for the function-signature case.
    pub(crate) async fn lookup_function_signature_with_path(
        &self,
        name: &Name,
    ) -> Option<(QualifiedPath, FunctionSignature)> {
        if let Some(found) = self.lookup_function_signature_with_path_once(name) {
            return Some(found);
        }
        let candidate = self
            .resolve_name_key(name)
            .or_else(|| self.fallback_name_key(name))?;
        let head = candidate.head()?;
        if !self.typing_ctx.env_ctx.is_registered(head) {
            return None;
        }
        self.await_package(head).await;
        self.lookup_function_signature_with_path_once(name)
    }

    pub(crate) fn lookup_function_signature_with_path_once(
        &self,
        name: &Name,
    ) -> Option<(QualifiedPath, FunctionSignature)> {
        let candidate = self
            .resolve_name_key(name)
            .or_else(|| self.fallback_name_key(name))?;
        if let Some(sig) = self
            .inner
            .borrow()
            .extern_function_signatures
            .get(&candidate)
        {
            return Some((candidate, sig.clone()));
        }
        if let Some(sig) = self.own_function_sigs().get(&candidate) {
            return Some((candidate, sig.clone()));
        }
        if let Some(sig) = self.typing_ctx.env_ctx.find_function_sig(&candidate) {
            // Resolved via a workspace crate, not the local one — if this is
            // a `Struct::method`-shaped path, the struct's owning crate's
            // `impl` block needs to be visible to HIR/MIR lowering too (see
            // `cross_crate_struct_refs`'s doc comment).
            if candidate.segments.len() >= 2 {
                if let Some(struct_path) = candidate.parent_n(1) {
                    self.inner
                        .borrow_mut()
                        .cross_crate_struct_refs
                        .insert(struct_path);
                }
            }
            return Some((candidate, sig.clone()));
        }
        if let Some(stripped) = Self::strip_std_prefix(&candidate) {
            if let Some(sig) = self.own_function_sigs().get(&stripped) {
                return Some((stripped, sig.clone()));
            }
        }
        if let Some((path, sig)) = self.lookup_prefixed_signature_with_path(&candidate, false) {
            return Some((path, sig));
        }
        if let Some(found) = self
            .name_tail(name)
            .and_then(|name| self.lookup_function_signature_by_name_with_path(&name))
        {
            return Some(found);
        }
        None
    }

    pub(crate) fn lookup_extern_function_signature_with_path(
        &self,
        name: &Name,
    ) -> Option<(QualifiedPath, FunctionSignature)> {
        let candidate = self
            .resolve_name_key(name)
            .or_else(|| self.fallback_name_key(name))?;
        if let Some(sig) = self
            .inner
            .borrow()
            .extern_function_signatures
            .get(&candidate)
        {
            return Some((candidate, sig.clone()));
        }
        if let Some(stripped) = Self::strip_std_prefix(&candidate) {
            if let Some(sig) = self
                .inner
                .borrow()
                .extern_function_signatures
                .get(&stripped)
            {
                return Some((stripped, sig.clone()));
            }
        }
        self.lookup_prefixed_signature_with_path(&candidate, true)
    }

    pub(crate) fn lookup_stripped_function_signature(
        &self,
        candidate: &QualifiedPath,
    ) -> Option<FunctionSignature> {
        let stripped = Self::strip_std_prefix(candidate)?;
        self.own_function_sigs().get(&stripped).cloned()
    }

    pub(crate) fn strip_std_prefix(candidate: &QualifiedPath) -> Option<QualifiedPath> {
        let first = candidate.segments.first()?;
        if (first == "std" || first == "core" || first == "alloc") && candidate.segments.len() > 1 {
            Some(QualifiedPath::new(
                candidate.segments.iter().skip(1).cloned().collect(),
            ))
        } else {
            None
        }
    }

    pub(crate) fn lookup_prefixed_function_signature(
        &self,
        candidate: &QualifiedPath,
    ) -> Option<FunctionSignature> {
        self.lookup_prefixed_signature(candidate, false)
    }

    pub(crate) fn fallback_name_key(&self, name: &Name) -> Option<QualifiedPath> {
        let (prefix, segments) = match name {
            Name::Path(path) => (
                path.prefix,
                path.segments
                    .iter()
                    .map(|seg| seg.as_str().to_string())
                    .collect::<Vec<_>>(),
            ),
            Name::ParameterPath(path) => (
                path.prefix,
                path.segments
                    .iter()
                    .map(|seg| seg.ident.as_str().to_string())
                    .collect::<Vec<_>>(),
            ),
            _ => return None,
        };
        if segments.is_empty() && matches!(prefix, PathPrefix::Plain | PathPrefix::Root) {
            return None;
        }
        match prefix {
            PathPrefix::Plain | PathPrefix::Root | PathPrefix::Crate => {
                Some(QualifiedPath::new(segments))
            }
            _ => None,
        }
    }

    pub(crate) fn lookup_function_signature_by_name(
        &self,
        name: &str,
    ) -> Option<FunctionSignature> {
        let mut found: Option<FunctionSignature> = None;
        for (key, sig) in self.own_function_sigs().iter() {
            if key.tail() == Some(name) {
                if found.is_some() {
                    return None;
                }
                found = Some(sig.clone());
            }
        }
        found
    }

    pub(crate) fn lookup_function_signature_by_name_with_path(
        &self,
        name: &str,
    ) -> Option<(QualifiedPath, FunctionSignature)> {
        let mut found: Option<(QualifiedPath, FunctionSignature)> = None;
        for (key, sig) in self.own_function_sigs().iter() {
            if key.tail() == Some(name) {
                if found.is_some() {
                    return None;
                }
                found = Some((key.clone(), sig.clone()));
            }
        }
        found
    }

    pub(crate) fn lookup_prefixed_signature(
        &self,
        candidate: &QualifiedPath,
        extern_only: bool,
    ) -> Option<FunctionSignature> {
        let first = candidate.segments.first()?;
        if first == "std" || first == "core" || first == "alloc" {
            return None;
        }
        for prefix in ["std", "core", "alloc"] {
            if !self.inner.borrow().root_modules.contains(prefix) {
                continue;
            }
            let base = QualifiedPath::new(vec![prefix.to_string()]);
            let qualified = base.join(&candidate.segments);
            if let Some(sig) = self
                .inner
                .borrow()
                .extern_function_signatures
                .get(&qualified)
            {
                return Some(sig.clone());
            }
            if !extern_only {
                if let Some(sig) = self.own_function_sigs().get(&qualified) {
                    return Some(sig.clone());
                }
            }
        }
        None
    }

    pub(crate) fn lookup_prefixed_signature_with_path(
        &self,
        candidate: &QualifiedPath,
        extern_only: bool,
    ) -> Option<(QualifiedPath, FunctionSignature)> {
        let first = candidate.segments.first()?;
        if first == "std" || first == "core" || first == "alloc" {
            return None;
        }
        for prefix in ["std", "core", "alloc"] {
            if !self.inner.borrow().root_modules.contains(prefix) {
                continue;
            }
            let base = QualifiedPath::new(vec![prefix.to_string()]);
            let qualified = base.join(&candidate.segments);
            if let Some(sig) = self
                .inner
                .borrow()
                .extern_function_signatures
                .get(&qualified)
            {
                return Some((qualified, sig.clone()));
            }
            if !extern_only {
                if let Some(sig) = self.own_function_sigs().get(&qualified) {
                    return Some((qualified, sig.clone()));
                }
            }
        }
        None
    }

    pub(crate) async fn lookup_associated_function(
        &self,
        name: &Name,
    ) -> Result<Option<TypeVarId>> {
        if let Name::Path(path) = name {
            if path.segments.len() >= 2 {
                if let Some(method_segment) = path.segments.last() {
                    let method_name = method_segment.as_str();
                    let struct_segments = path
                        .segments
                        .iter()
                        .take(path.segments.len() - 1)
                        .map(|seg| seg.as_str().to_string())
                        .collect::<Vec<_>>();
                    if let Some(struct_name) =
                        self.resolve_segments_key(path.prefix, &struct_segments)
                    {
                        for candidate in self.struct_name_variants_for_path(
                            &struct_name,
                            struct_name.segments.len() == 1,
                        ) {
                            let qualified = candidate.with_segment(method_name.to_string());
                            if let Some(var) = self.lookup_env_var(&qualified.to_key()).await {
                                return Ok(Some(var));
                            }
                            // Only clone the one matching method signature,
                            // not the whole method list -- the local case
                            // doesn't need to clone anything else (cross-crate
                            // lookups already clone internally via
                            // `find_method_sigs`, since crates now live
                            // behind a `RefCell` for on-demand loading).
                            let local = self.own_method_sigs().get(&candidate).map(|sigs| {
                                (
                                    false,
                                    sigs.iter()
                                        .find(|(n, _)| n == method_name)
                                        .map(|(_, sig)| sig.clone()),
                                )
                            });
                            let (is_cross_crate, found_sig) = if let Some(result) = local {
                                result
                            } else if let Some(sigs) =
                                self.typing_ctx.env_ctx.find_method_sigs(&candidate)
                            {
                                (
                                    true,
                                    sigs.into_iter()
                                        .find(|(n, _)| n == method_name)
                                        .map(|(_, sig)| sig),
                                )
                            } else {
                                let registered = candidate.head().is_some_and(|head| {
                                    self.typing_ctx.env_ctx.is_registered(head)
                                });
                                if registered {
                                    self.await_package(candidate.head().unwrap()).await;
                                    match self.typing_ctx.env_ctx.find_method_sigs(&candidate) {
                                        Some(sigs) => (
                                            true,
                                            sigs.into_iter()
                                                .find(|(n, _)| n == method_name)
                                                .map(|(_, sig)| sig),
                                        ),
                                        None => (false, None),
                                    }
                                } else {
                                    (false, None)
                                }
                            };
                            if is_cross_crate || self.own_method_sigs().contains_key(&candidate) {
                                if is_cross_crate {
                                    self.inner
                                        .borrow_mut()
                                        .cross_crate_struct_refs
                                        .insert(candidate.clone());
                                }
                                if let Some(sig) = found_sig {
                                    if !sig.impl_generics_params.is_empty()
                                        || !sig.sig.generics_params.is_empty()
                                    {
                                        if let Ok((receiver, params, ret)) =
                                            self.instantiate_method_signature(&sig).await
                                        {
                                            if sig.sig.receiver.is_some() {
                                                let fn_var = self.fresh_type_var();
                                                self.bind_function_term(fn_var, params, ret);
                                                let _ = receiver;
                                                return Ok(Some(fn_var));
                                            }
                                            let fn_var = self.fresh_type_var();
                                            self.bind_function_term(fn_var, params, ret);
                                            return Ok(Some(fn_var));
                                        }
                                    }
                                    if let Some(var) = self.lookup_env_var(method_name).await {
                                        return Ok(Some(var));
                                    }
                                    let fn_ty = self.ty_from_function_signature(&sig.sig)?;
                                    let fn_var = self.type_from_ast_ty(&fn_ty).await?;
                                    return Ok(Some(fn_var));
                                }
                            }
                        }

                        // Enum tuple variant constructors: `Enum::Variant(...)`.
                        // `lookup_enum` (not `own_enum_defs()` alone) so a
                        // cross-crate enum like `std::option::Option` -- not
                        // defined in whatever crate is currently being typed
                        // -- resolves here too.
                        let enum_def = self.lookup_enum(&struct_name).await;
                        if let Some(enum_def) = enum_def {
                            if let Some(variant) = enum_def
                                .variants
                                .iter()
                                .find(|v| v.name.as_str() == method_name)
                            {
                                self.enter_scope();
                                let mut generic_vars = Vec::new();
                                if !enum_def.generics_params.is_empty() {
                                    for param in &enum_def.generics_params {
                                        let var = self.register_generic_param(param.name.as_str());
                                        generic_vars.push((param.name.as_str().to_string(), var));
                                        let bounds = Self::extract_trait_bounds(&param.bounds);
                                        if !bounds.is_empty() {
                                            self.inner
                                                .borrow_mut()
                                                .generic_trait_bounds
                                                .insert(var, bounds);
                                        }
                                    }
                                }
                                let generic_mapping = generic_vars
                                    .iter()
                                    .map(|(name, var)| (name.clone(), Ty::infer_var(*var)))
                                    .collect::<HashMap<_, _>>();
                                let mut params = Vec::new();
                                match &variant.value {
                                    Ty::Unit(_) => {}
                                    Ty::Tuple(tuple_ty) => {
                                        params.extend(tuple_ty.types.iter().map(|ty| {
                                            self.substitute_generic_ty(ty, &generic_mapping)
                                        }))
                                    }
                                    other => params
                                        .push(self.substitute_generic_ty(other, &generic_mapping)),
                                }

                                let func_ty = Ty::Function(TypeFunction {
                                    params,
                                    generics_params: enum_def.generics_params.clone(),
                                    ret_ty: Some(Box::new(self.substitute_generic_ty(
                                        &Ty::Enum(enum_def.clone()),
                                        &generic_mapping,
                                    ))),
                                });
                                let func_var = self.type_from_ast_ty(&func_ty).await?;
                                self.exit_scope();
                                return Ok(Some(func_var));
                            }
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    pub(crate) async fn lookup_name(&self, name: &Name) -> Result<TypeVarId> {
        self.lookup_name_with_resolution(name)
            .await
            .map(|(var, _)| var)
    }

    pub(crate) async fn lookup_name_with_resolution(
        &self,
        name: &Name,
    ) -> Result<(TypeVarId, Option<ResolvedName>)> {
        if self.check_unimplemented_name(name) {
            return Ok((self.error_type_var(), None));
        }
        if let Name::Path(path) = name {
            if path.segments.len() >= 2 {
                let variant_name = path.segments.last().map(|seg| seg.as_str());
                let enum_segments = path
                    .segments
                    .iter()
                    .take(path.segments.len() - 1)
                    .map(|seg| seg.as_str().to_string())
                    .collect::<Vec<_>>();
                if let (Some(variant_name), Some(enum_key)) = (
                    variant_name,
                    self.resolve_segments_key(path.prefix, &enum_segments),
                ) {
                    let enum_def = self.lookup_enum(&enum_key).await;
                    if let Some(enum_def) = enum_def {
                        if enum_def
                            .variants
                            .iter()
                            .any(|v| v.name.as_str() == variant_name)
                        {
                            let var = self.fresh_type_var();
                            self.bind(var, Ty::Enum(enum_def));
                            let qualified = enum_key.with_segment(variant_name.to_string());
                            return Ok((
                                var,
                                Some(ResolvedName {
                                    namespace: ResolvedNameNamespace::Value,
                                    path: qualified,
                                }),
                            ));
                        }
                    }
                }
            }
        }
        if let Some(ident) = name.as_ident() {
            let name = ident.as_str();
            if let Some(var) = self.lookup_env_var(name).await {
                return Ok((var, None));
            }
            if !self.inner.borrow().module_path.is_empty() {
                let qualified = self
                    .inner
                    .borrow()
                    .module_path
                    .with_segment(name.to_string());
                if let Some(var) = self.lookup_env_var(&qualified.to_key()).await {
                    return Ok((
                        var,
                        Some(ResolvedName {
                            namespace: ResolvedNameNamespace::Value,
                            path: qualified,
                        }),
                    ));
                }
            }
        }
        let key = match self.resolve_name_key(name) {
            Some(key) => key,
            None => {
                // In value position, names like i64, bool, str, type
                // refer to types — bind them as type-level values.
                if let Some(ident) = name.as_ident() {
                    let name = ident.as_str();
                    if name == "type" {
                        let var = self.fresh_type_var();
                        self.bind(var, Ty::Type(TypeType::new(Span::null())));
                        return Ok((
                            var,
                            Some(ResolvedName {
                                namespace: ResolvedNameNamespace::Type,
                                path: QualifiedPath::new(vec![name.to_string()]),
                            }),
                        ));
                    }
                    if let Some(prim) = crate::unify::primitive_from_name(name) {
                        let var = self.fresh_type_var();
                        self.bind(
                            var,
                            Ty::Type(TypeType {
                                span: Span::null(),
                                inner: Some(Box::new(Ty::Primitive(prim))),
                            }),
                        );
                        return Ok((
                            var,
                            Some(ResolvedName {
                                namespace: ResolvedNameNamespace::Type,
                                path: QualifiedPath::new(vec![name.to_string()]),
                            }),
                        ));
                    }
                }
                self.emit_error(format!("unresolved symbol: {}", name));
                return Ok((self.error_type_var(), None));
            }
        };
        if self.own_struct_defs().contains_key(&key) || self.own_enum_defs().contains_key(&key) {
            let var = self.fresh_type_var();
            self.bind(var, Ty::Type(TypeType::new(Span::null())));
            return Ok((
                var,
                Some(ResolvedName {
                    namespace: ResolvedNameNamespace::Type,
                    path: key,
                }),
            ));
        }
        if let Some(var) = self.lookup_env_var(&key.to_key()).await {
            return Ok((
                var,
                Some(ResolvedName {
                    namespace: ResolvedNameNamespace::Value,
                    path: key,
                }),
            ));
        }
        // Fallback: workspace crates may have this function registered
        let workspace_sig = self.typing_ctx.env_ctx.find_function_sig(&key);
        if let Some(sig) = workspace_sig {
            let fn_ty = self.ty_from_function_signature(&sig)?;
            let var = self.type_from_ast_ty(&fn_ty).await?;
            return Ok((
                var,
                Some(ResolvedName {
                    namespace: ResolvedNameNamespace::Value,
                    path: key,
                }),
            ));
        }
        self.emit_error(format!("unresolved symbol: {}", key.to_key()));
        Ok((self.error_type_var(), None))
    }

    pub(crate) fn resolve_alias_name(&self, name: &Name) -> Option<QualifiedPath> {
        match name {
            Name::Ident(ident) => self.lookup_symbol_alias(ident.as_str()),
            Name::Path(path) => {
                if let Some(first) = path.segments.first() {
                    if let Some(module_path) = self.lookup_module_alias(first.as_str()) {
                        let extra = path
                            .segments
                            .iter()
                            .skip(1)
                            .map(|seg| seg.as_str().to_string())
                            .collect::<Vec<_>>();
                        return Some(module_path.join(&extra));
                    }
                    if let Some(symbol_path) = self.lookup_symbol_alias(first.as_str()) {
                        let extra = path
                            .segments
                            .iter()
                            .skip(1)
                            .map(|seg| seg.as_str().to_string())
                            .collect::<Vec<_>>();
                        return Some(symbol_path.join(&extra));
                    }
                }
                None
            }
            Name::ParameterPath(path) => {
                if let Some(first) = path.segments.first() {
                    if let Some(module_path) = self.lookup_module_alias(first.ident.as_str()) {
                        let extra = path
                            .segments
                            .iter()
                            .skip(1)
                            .map(|seg| seg.ident.as_str().to_string())
                            .collect::<Vec<_>>();
                        return Some(module_path.join(&extra));
                    }
                    if let Some(symbol_path) = self.lookup_symbol_alias(first.ident.as_str()) {
                        let extra = path
                            .segments
                            .iter()
                            .skip(1)
                            .map(|seg| seg.ident.as_str().to_string())
                            .collect::<Vec<_>>();
                        return Some(symbol_path.join(&extra));
                    }
                }
                None
            }
        }
    }

    pub(crate) fn lookup_symbol_alias(&self, name: &str) -> Option<QualifiedPath> {
        for scope in self.inner.borrow().symbol_aliases.iter().rev() {
            if let Some(target) = scope.get(name) {
                return Some(target.clone());
            }
        }
        None
    }

    pub(crate) fn lookup_module_alias(&self, name: &str) -> Option<QualifiedPath> {
        for scope in self.inner.borrow().module_aliases.iter().rev() {
            if let Some(path) = scope.get(name) {
                return Some(path.clone());
            }
        }
        None
    }

    pub(crate) async fn lookup_env_var(&self, name: &str) -> Option<TypeVarId> {
        if let Some(var) = self.lookup_env_var_direct(name).await {
            return Some(var);
        }
        let should_retry = self
            .inner
            .borrow_mut()
            .resolution_hook
            .as_mut()
            .map(|hook| hook.resolve_symbol(name))
            .unwrap_or(false);
        if should_retry {
            return self.lookup_env_var_direct(name).await;
        }
        None
    }

    pub(crate) async fn lookup_env_var_direct(&self, name: &str) -> Option<TypeVarId> {
        // The scope scan itself is confined to a single `Ref` borrow that
        // ends before returning -- iterating `self.env` directly as the
        // `for` loop's iterable would otherwise keep that borrow alive for
        // the whole loop (a `for` loop's iterable temporary lives for the
        // entire loop), including across the `Poly` branch's `.await` below.
        let found = {
            let inner = self.inner.borrow();
            inner
                .env
                .iter()
                .rev()
                .find_map(|scope| scope.get(name).cloned())
        };
        match found {
            Some(EnvEntry::Mono(var)) => Some(var),
            Some(EnvEntry::Poly(scheme)) => Some(self.instantiate_scheme(&scheme).await),
            None => None,
        }
    }

    pub(crate) async fn symbol_var(&self, name: &Ident) -> TypeVarId {
        let key = name.as_str().to_string();
        if let Some(var) = self.lookup_env_var(&key).await {
            return var;
        }
        let var = self.fresh_type_var();
        self.insert_env(key, EnvEntry::Mono(var));
        var
    }

    pub(crate) fn register_symbol(&self, name: &Ident) {
        let key = name.as_str().to_string();
        let var = self.fresh_type_var();
        if let Some(scope) = self.inner.borrow_mut().env.last_mut() {
            scope.entry(key).or_insert(EnvEntry::Mono(var));
        }
    }

    pub(crate) async fn generalize_symbol(&self, name: &str, var: TypeVarId) -> Result<()> {
        let scheme = self.generalize(var).await?;
        self.replace_env_entry(name, EnvEntry::Poly(scheme));
        Ok(())
    }
}
