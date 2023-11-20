use crate::ast::{SourceSpan, Type, BinOp};

#[derive(Debug, Clone, PartialEq)]
pub enum TypedTopLvl {
    Import(String, SourceSpan),
    FuncDef(Type, String, Vec<(Type, String)>, Vec<TypedStmt>, SourceSpan),
    StructDef(String, Vec<(Type, String)>, SourceSpan),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedStmt {
    VarDecl(Type, String, Option<TypedExpr>, SourceSpan),
    Assign(String, TypedExpr, SourceSpan),
    If(TypedExpr, Vec<TypedStmt>, Option<Vec<TypedStmt>>, SourceSpan),
    While(TypedExpr, Vec<TypedStmt>, SourceSpan),
    For(TypedExpr, TypedExpr, TypedExpr, Vec<TypedStmt>, SourceSpan),
    Return(Option<TypedExpr>, SourceSpan),
    Call(String, Vec<TypedExpr>, SourceSpan),
    Err,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedExpr {
    Ident(String, SourceSpan),
    Int(i32, SourceSpan),
    Float(f32, SourceSpan),
    String(String, SourceSpan),
    Array(Vec<TypedExpr>, SourceSpan),
    Call(String, Vec<TypedExpr>, SourceSpan),
    Index(String, Box<TypedExpr>, SourceSpan),
    BinOp(Box<TypedExpr>, BinOp, Box<TypedExpr>, SourceSpan),
    Neg(Box<TypedExpr>, SourceSpan),
    Not(Box<TypedExpr>, SourceSpan),
    Err,
}

impl TypedExpr {
    pub fn get_span(&self) -> SourceSpan {
        match self {
            TypedExpr::Ident(_, span) => span.clone(),
            TypedExpr::Int(_, span) => span.clone(),
            TypedExpr::Float(_, span) => span.clone(),
            TypedExpr::String(_, span) => span.clone(),
            TypedExpr::Array(_, span) => span.clone(),
            TypedExpr::Call(_, _, span) => span.clone(),
            TypedExpr::Index(_, _, span) => span.clone(),
            TypedExpr::BinOp(_, _, _, span) => span.clone(),
            TypedExpr::Neg(_, span) => span.clone(),
            TypedExpr::Not(_, span) => span.clone(),
            TypedExpr::Err => SourceSpan {
                file: 0,
                span: 0..0,
            },
        }
    }
}

// func def -> mangle -> function hashmap
// fn call -> infer types -> infer what fn to call -> replace with mangled name
