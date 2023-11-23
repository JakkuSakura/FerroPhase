use common::{bail, Result};
use common_lang::ast::Ident;
use common_lang::value::{EnumType, EnumTypeVariant, TypeValue};
use common_lang::Serializer;
use oxc::ast::ast::{BindingIdentifier, Modifiers, Program};
use oxc::codegen::CodegenOptions;
use oxc::span::{SourceType, Span};

pub struct TsPrinter {
    alloc: oxc::allocator::Allocator,
}
impl TsPrinter {
    pub fn new() -> Self {
        Self {
            alloc: Default::default(),
        }
    }
    pub fn builder(&self) -> oxc::ast::AstBuilder {
        oxc::ast::AstBuilder::new(&self.alloc)
    }
    pub fn codegen(&self) -> oxc::codegen::Codegen<false> {
        oxc::codegen::Codegen::new(0, CodegenOptions::default())
    }
    pub fn source_type(&self) -> SourceType {
        SourceType::default().with_typescript_definition(true)
    }
    pub fn to_program<'a>(&'a self, statement: oxc::ast::ast::Statement<'a>) -> Program<'a> {
        let builder = self.builder();
        let directives = builder.new_vec();
        let mut body = builder.new_vec();
        body.push(statement);
        Program {
            span: Default::default(),
            source_type: self.source_type(),
            directives,
            hashbang: None,
            body,
        }
    }
    pub fn to_id(&self, name: &Ident) -> BindingIdentifier {
        BindingIdentifier::new(Span::default(), name.name.as_str().into())
    }
    pub fn to_id_name(&self, name: &Ident) -> oxc::ast::ast::IdentifierName {
        oxc::ast::ast::IdentifierName::new(Span::default(), self.to_id(name).name)
    }
    pub fn to_enum_member(&self, variant: &EnumTypeVariant) -> Result<oxc::ast::ast::TSEnumMember> {
        let name = self.to_id_name(&variant.name);
        let init = oxc::ast::ast::TSEnumMember {
            span: Default::default(),
            id: oxc::ast::ast::TSEnumMemberName::Identifier(name),
            initializer: None,
        };

        Ok(init)
    }
    pub fn to_declaration(&self, def: &EnumType) -> Result<oxc::ast::ast::Declaration> {
        let builder = self.builder();
        let name = self.to_id(&def.name);
        let mut members = builder.new_vec();
        for variant in &def.variants {
            let member = self.to_enum_member(variant)?;
            members.push(member);
        }
        let decl =
            builder.ts_enum_declaration(Span::default(), name, members, Modifiers::default());
        Ok(decl)
    }
    pub fn print_statement<'a>(&'a self, statement: oxc::ast::ast::Statement<'a>) -> String {
        let s = self.codegen().build(&self.to_program(statement));
        s.replace("\t", "    ")
    }
}

impl Serializer for TsPrinter {
    fn serialize_type(&self, node: &TypeValue) -> Result<String> {
        match node {
            TypeValue::Enum(node) => {
                let decl = self.to_declaration(node)?;
                let s = self.print_statement(oxc::ast::ast::Statement::Declaration(decl));
                Ok(s)
            }
            _ => bail!("not implemented: serialize_type: {:?}", node),
        }
    }
}
