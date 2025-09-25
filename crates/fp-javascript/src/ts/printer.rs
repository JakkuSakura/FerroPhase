use std::io::{Cursor, Write};
use std::sync::{Arc, Mutex};

use fp_core::{bail, Result};
use itertools::Itertools;
use swc_ecma_ast::{Script, TsEnumDecl, TsEnumMemberId};
use swc_ecma_codegen::text_writer::JsWriter;
use swc_ecma_codegen::Emitter;
use swc_ecma_quote::swc_common::sync::Lrc;
use swc_ecma_quote::swc_common::{SourceMap, DUMMY_SP};

use fp_core::ast::{AstSerializer, EnumTypeVariant, Ty, TypeEnum, TypeStruct};
use fp_core::id::Ident;
use fp_core::printer::AstSerializerConfig;

#[derive(Clone)]
struct SharedWriter {
    wr: Arc<Mutex<Cursor<Vec<u8>>>>,
}
impl SharedWriter {
    pub fn new() -> Self {
        Self {
            wr: Arc::new(Mutex::new(Cursor::new(Vec::new()))),
        }
    }
    pub fn take_string(&self) -> String {
        let vec = std::mem::take(self.wr.lock().unwrap().get_mut());
        String::from_utf8(vec).unwrap()
    }
}
impl Write for SharedWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.wr.lock().unwrap().write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.wr.lock().unwrap().flush()
    }
}
pub struct TsPrinter {
    emitter: Mutex<Emitter<'static, JsWriter<'static, SharedWriter>, SourceMap>>,
    writer: SharedWriter,
    config: AstSerializerConfig,
}
impl TsPrinter {
    pub fn new() -> Self {
        Self::with_config(AstSerializerConfig::compact())
    }

    pub fn with_config(config: AstSerializerConfig) -> Self {
        let writer = SharedWriter::new();
        let source_map = Lrc::new(SourceMap::default());

        let mut swc_config = swc_ecma_codegen::Config::default();
        swc_config.minify = config.minify;

        let line_ending = config.formatting.line_ending.as_str();

        Self {
            emitter: Mutex::new(Emitter {
                cfg: swc_config,
                cm: source_map.clone(),
                comments: None,
                wr: JsWriter::new(source_map, line_ending, writer.clone(), None),
            }),
            writer,
            config,
        }
    }

    pub fn config(&self) -> &AstSerializerConfig {
        &self.config
    }
    pub fn to_ident(&self, name: &Ident) -> swc_ecma_ast::Ident {
        swc_ecma_ast::Ident::new(name.name.as_str().into(), DUMMY_SP)
    }

    pub fn to_enum_member(&self, name: &EnumTypeVariant) -> Result<swc_ecma_ast::TsEnumMember> {
        Ok(swc_ecma_ast::TsEnumMember {
            span: Default::default(),
            id: TsEnumMemberId::Ident(self.to_ident(&name.name)),
            // TODO: deal with init
            init: None,
        })
    }
    pub fn to_enum(&self, decl: &TypeEnum) -> Result<TsEnumDecl> {
        Ok(TsEnumDecl {
            span: DUMMY_SP,
            declare: true,
            id: self.to_ident(&decl.name),
            is_const: true,
            members: decl
                .variants
                .iter()
                .map(|x| self.to_enum_member(x))
                .try_collect()?,
        })
    }

    pub fn to_struct_interface(
        &self,
        struct_def: &TypeStruct,
    ) -> Result<swc_ecma_ast::TsInterfaceDecl> {
        use swc_ecma_ast::*;

        Ok(TsInterfaceDecl {
            span: DUMMY_SP,
            id: self.to_ident(&struct_def.name),
            declare: false,
            type_params: None,
            extends: vec![],
            body: TsInterfaceBody {
                span: DUMMY_SP,
                body: struct_def
                    .fields
                    .iter()
                    .map(|field| {
                        let ts_type =
                            self.rust_type_to_typescript_type(&field.value.to_string())?;
                        Ok(TsTypeElement::TsPropertySignature(TsPropertySignature {
                            span: DUMMY_SP,
                            readonly: false,
                            key: Box::new(Expr::Ident(self.to_ident(&field.name))),
                            computed: false,
                            optional: false,
                            init: None,
                            params: vec![],
                            type_params: None,
                            type_ann: Some(Box::new(TsTypeAnn {
                                span: DUMMY_SP,
                                type_ann: Box::new(ts_type),
                            })),
                        }))
                    })
                    .collect::<Result<Vec<_>>>()?,
            },
        })
    }

    pub fn rust_type_to_typescript_type(&self, rust_type: &str) -> Result<swc_ecma_ast::TsType> {
        use swc_ecma_ast::*;

        let ts_type = match rust_type {
            "f64" | "f32" | "i64" | "i32" | "i16" | "i8" | "u64" | "u32" | "u16" | "u8"
            | "usize" | "isize" => TsType::TsKeywordType(TsKeywordType {
                span: DUMMY_SP,
                kind: TsKeywordTypeKind::TsNumberKeyword,
            }),
            "bool" => TsType::TsKeywordType(TsKeywordType {
                span: DUMMY_SP,
                kind: TsKeywordTypeKind::TsBooleanKeyword,
            }),
            "String" | "str" => TsType::TsKeywordType(TsKeywordType {
                span: DUMMY_SP,
                kind: TsKeywordTypeKind::TsStringKeyword,
            }),
            _ => {
                // Custom type reference
                TsType::TsTypeRef(TsTypeRef {
                    span: DUMMY_SP,
                    type_name: TsEntityName::Ident(swc_ecma_ast::Ident::new(
                        rust_type.into(),
                        DUMMY_SP,
                    )),
                    type_params: None,
                })
            }
        };

        Ok(ts_type)
    }
    pub fn print_script(&self, script: &Script) -> Result<String> {
        self.emitter.lock().unwrap().emit_script(script)?;
        Ok(self.writer.take_string())
    }

    pub fn print_multiple_scripts(&self, scripts: &[Script]) -> Result<String> {
        let mut output = String::new();
        for script in scripts {
            self.emitter.lock().unwrap().emit_script(script)?;
            output.push_str(&self.writer.take_string());
            output.push('\n');
        }
        Ok(output)
    }

    pub fn print_struct_interface(&self, struct_def: &TypeStruct) -> Result<String> {
        use swc_ecma_ast::*;

        let interface_decl = self.to_struct_interface(struct_def)?;

        let script = Script {
            span: DUMMY_SP,
            body: vec![Stmt::Decl(Decl::TsInterface(Box::new(interface_decl)))],
            shebang: None,
        };

        self.print_script(&script)
    }

    pub fn print_types_and_code(
        &self,
        structs: &[TypeStruct],
        enums: &[TypeEnum],
        main_code: &str,
    ) -> Result<String> {
        use swc_ecma_ast::*;

        let mut body = Vec::new();

        // Add enums first
        for enum_def in enums {
            let enum_decl = self.to_enum(enum_def)?;
            body.push(Stmt::Decl(Decl::TsEnum(Box::new(enum_decl))));
        }

        // Add interfaces
        for struct_def in structs {
            let interface_decl = self.to_struct_interface(struct_def)?;
            body.push(Stmt::Decl(Decl::TsInterface(Box::new(interface_decl))));
        }

        let script = Script {
            span: DUMMY_SP,
            body,
            shebang: None,
        };

        let types_output = self.print_script(&script)?;
        Ok(format!("{}\n{}", types_output, main_code))
    }
}
unsafe impl Send for TsPrinter {}
unsafe impl Sync for TsPrinter {}
impl AstSerializer for TsPrinter {
    fn serialize_type(&self, node: &Ty) -> Result<String> {
        match node {
            Ty::Enum(decl) => {
                let decl = self.to_enum(decl)?;

                self.print_script(&Script {
                    span: Default::default(),
                    body: vec![swc_ecma_ast::Stmt::Decl(swc_ecma_ast::Decl::TsEnum(
                        decl.into(),
                    ))],
                    shebang: None,
                })
            }
            Ty::Struct(struct_def) => {
                let interface_decl = self.to_struct_interface(struct_def)?;

                self.print_script(&Script {
                    span: Default::default(),
                    body: vec![swc_ecma_ast::Stmt::Decl(swc_ecma_ast::Decl::TsInterface(
                        interface_decl.into(),
                    ))],
                    shebang: None,
                })
            }
            _ => bail!("Type serialization not implemented for: {:?}", node),
        }
    }
}
