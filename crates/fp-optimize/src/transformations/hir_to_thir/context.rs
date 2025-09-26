use super::*;
use std::collections::HashMap;

#[derive(Clone)]
pub(super) struct StructInfo {
    pub(super) adt_def: hir_types::AdtDef,
    pub(super) field_map: HashMap<String, (usize, hir_types::Ty)>,
}

pub(super) struct TypeContext {
    function_signatures: HashMap<hir_types::DefId, hir_types::FnSig>,
    method_signatures: HashMap<hir_types::DefId, HashMap<String, hir_types::FnSig>>,
    structs: HashMap<hir_types::DefId, StructInfo>,
    struct_names: HashMap<String, hir_types::DefId>,
    value_names: HashMap<String, hir_types::DefId>,
    const_types: HashMap<hir_types::DefId, hir_types::Ty>,
    next_def_id: hir_types::DefId,
}

impl TypeContext {
    pub(super) fn new() -> Self {
        Self {
            function_signatures: HashMap::new(),
            method_signatures: HashMap::new(),
            structs: HashMap::new(),
            struct_names: HashMap::new(),
            value_names: HashMap::new(),
            const_types: HashMap::new(),
            next_def_id: 1,
        }
    }

    pub(super) fn register_function(
        &mut self,
        def_id: hir_types::DefId,
        name: &str,
        sig: hir_types::FnSig,
    ) {
        self.function_signatures.insert(def_id, sig);
        self.insert_value_name(name, def_id);
    }

    pub(super) fn register_const(
        &mut self,
        def_id: hir_types::DefId,
        name: &str,
        ty: hir_types::Ty,
    ) {
        self.const_types.insert(def_id, ty);
        self.insert_value_name(name, def_id);
    }

    pub(super) fn declare_struct(&mut self, def_id: hir_types::DefId, name: &str) {
        self.insert_struct_name(name, def_id);
        if !self.structs.contains_key(&def_id) {
            let adt_def = self.build_adt_def(def_id, name, Vec::new());
            self.structs.insert(
                def_id,
                StructInfo {
                    adt_def,
                    field_map: HashMap::new(),
                },
            );
        }
        self.method_signatures.entry(def_id).or_default();
    }

    pub(super) fn init_struct_fields(
        &mut self,
        def_id: hir_types::DefId,
        fields: Vec<(String, hir_types::Ty)>,
    ) {
        if !self.structs.contains_key(&def_id) {
            let struct_name = self
                .struct_names
                .iter()
                .find(|(_, &id)| id == def_id)
                .map(|(name, _)| name.clone())
                .unwrap_or_else(|| format!("<anon:{}>", def_id));
            let adt_def = self.build_adt_def(def_id, &struct_name, Vec::new());
            self.structs.insert(
                def_id,
                StructInfo {
                    adt_def,
                    field_map: HashMap::new(),
                },
            );
        }

        let struct_name = self
            .structs
            .get(&def_id)
            .and_then(|info| info.adt_def.variants.first())
            .map(|variant| variant.ident.clone())
            .or_else(|| {
                self.struct_names
                    .iter()
                    .find(|(_, &id)| id == def_id)
                    .map(|(name, _)| name.clone())
            })
            .unwrap_or_else(|| format!("<anon:{}>", def_id));

        let mut field_defs = Vec::new();
        let mut field_map = HashMap::new();
        for (idx, (field_name, field_ty)) in fields.into_iter().enumerate() {
            let field_def = hir_types::FieldDef {
                did: self.allocate_def_id(),
                ident: field_name.clone(),
                vis: hir_types::Visibility::Public,
            };
            field_defs.push(field_def);
            field_map.insert(field_name, (idx, field_ty));
        }

        let new_adt = self.build_adt_def(def_id, &struct_name, field_defs);

        if let Some(info) = self.structs.get_mut(&def_id) {
            info.field_map = field_map;
            info.adt_def = new_adt;
        }
    }

    pub(super) fn register_method(
        &mut self,
        owner_def_id: hir_types::DefId,
        method: &str,
        sig: hir_types::FnSig,
    ) {
        self.method_signatures
            .entry(owner_def_id)
            .or_default()
            .insert(method.to_string(), sig);
    }

    pub(super) fn make_struct_ty_by_id(
        &self,
        def_id: hir_types::DefId,
        substs: hir_types::SubstsRef,
    ) -> Option<hir_types::Ty> {
        self.structs.get(&def_id).map(|info| hir_types::Ty {
            kind: hir_types::TyKind::Adt(info.adt_def.clone(), substs),
        })
    }

    pub(super) fn lookup_struct_def_id(&self, name: &str) -> Option<hir_types::DefId> {
        self.struct_names.get(name).copied()
    }

    pub(super) fn lookup_value_def_id(&self, name: &str) -> Option<hir_types::DefId> {
        self.value_names.get(name).copied()
    }

    pub(super) fn lookup_function_signature(
        &self,
        def_id: hir_types::DefId,
    ) -> Option<&hir_types::FnSig> {
        self.function_signatures.get(&def_id)
    }

    pub(super) fn lookup_const_type(&self, def_id: hir_types::DefId) -> Option<&hir_types::Ty> {
        self.const_types.get(&def_id)
    }

    pub(super) fn lookup_field_info(
        &self,
        ty: &hir_types::Ty,
        field_name: &str,
    ) -> Option<(usize, hir_types::Ty)> {
        match &ty.kind {
            hir_types::TyKind::Adt(adt_def, _) => self
                .structs
                .get(&adt_def.did)
                .and_then(|info| info.field_map.get(field_name).cloned()),
            hir_types::TyKind::Ref(_, inner, _) => self.lookup_field_info(inner, field_name),
            _ => None,
        }
    }

    pub(super) fn lookup_method_signature(
        &self,
        owner_ty: &hir_types::Ty,
        method_name: &str,
    ) -> Option<&hir_types::FnSig> {
        match &owner_ty.kind {
            hir_types::TyKind::Adt(adt_def, _) => self
                .method_signatures
                .get(&adt_def.did)
                .and_then(|methods| methods.get(method_name)),
            hir_types::TyKind::Ref(_, inner, _) => self.lookup_method_signature(inner, method_name),
            _ => None,
        }
    }

    pub(super) fn ensure_struct_stub(&mut self, name: &str) -> hir_types::DefId {
        if let Some(def_id) = self.struct_names.get(name) {
            *def_id
        } else {
            let def_id = self.allocate_def_id();
            self.insert_struct_name(name, def_id);
            self.structs.insert(
                def_id,
                StructInfo {
                    adt_def: self.build_adt_def(def_id, name, Vec::new()),
                    field_map: HashMap::new(),
                },
            );
            self.method_signatures.entry(def_id).or_default();
            def_id
        }
    }

    fn insert_value_name(&mut self, name: &str, def_id: hir_types::DefId) {
        self.value_names.insert(name.to_string(), def_id);
        if let Some(base) = name.rsplit("::").next() {
            self.value_names.entry(base.to_string()).or_insert(def_id);
        }
    }

    fn insert_struct_name(&mut self, name: &str, def_id: hir_types::DefId) {
        self.struct_names.insert(name.to_string(), def_id);
        if let Some(base) = name.rsplit("::").next() {
            self.struct_names.entry(base.to_string()).or_insert(def_id);
        }
    }

    fn build_adt_def(
        &self,
        def_id: hir_types::DefId,
        name: &str,
        field_defs: Vec<hir_types::FieldDef>,
    ) -> hir_types::AdtDef {
        let variant = hir_types::VariantDef {
            def_id,
            ctor_def_id: None,
            ident: name.to_string(),
            discr: hir_types::VariantDiscr::Relative(0),
            fields: field_defs,
            ctor_kind: hir_types::CtorKind::Const,
            is_recovered: false,
        };

        hir_types::AdtDef {
            did: def_id,
            variants: vec![variant],
            flags: hir_types::AdtFlags::IS_STRUCT,
            repr: hir_types::ReprOptions {
                int: None,
                align: None,
                pack: None,
                flags: hir_types::ReprFlags::empty(),
                field_shuffle_seed: 0,
            },
        }
    }

    fn allocate_def_id(&mut self) -> hir_types::DefId {
        let id = self.next_def_id;
        self.next_def_id += 1;
        id
    }
}
