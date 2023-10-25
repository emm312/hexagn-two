use std::{ops::Range, process::exit};

use codespan_reporting::diagnostic::Diagnostic;
use lalrpop_util::ParseError;

use crate::grammar;

#[derive(Debug, Clone, PartialEq)]
pub enum TopLvl {
    Import(String, SourceSpan),
    FuncDef(Type, String, Vec<(Type, String)>, Vec<Stmt>, SourceSpan),
    StructDef(String, Vec<(Type, String)>, SourceSpan),
    Err,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Named(String, SourceSpan),
    Array(Box<Type>, SourceSpan),
    Ptr(Box<Type>, SourceSpan),
    Const(Box<Type>, SourceSpan),
    Err,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    VarDecl(Type, String, Option<Expr>, SourceSpan),
    Assign(String, Expr, SourceSpan),
    If(Expr, Vec<Stmt>, Option<Vec<Stmt>>, SourceSpan),
    While(Expr, Vec<Stmt>, SourceSpan),
    For(Expr, Expr, Expr, Vec<Stmt>, SourceSpan),
    Return(Option<Expr>, SourceSpan),
    Call(String, Vec<Expr>, SourceSpan),
    Err,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(String, SourceSpan),
    Int(i32, SourceSpan),
    Float(f32, SourceSpan),
    String(String, SourceSpan),
    Array(Vec<Expr>, SourceSpan),
    Call(String, Vec<Expr>, SourceSpan),
    Index(String, Box<Expr>, SourceSpan),
    BinOp(Box<Expr>, BinOp, Box<Expr>, SourceSpan),
    Neg(Box<Expr>, SourceSpan),
    Not(Box<Expr>, SourceSpan),
    Err,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceSpan {
    pub file: usize,
    pub span: Range<usize>,
}

#[macro_export]
macro_rules! gen_sourcespan {
    ($file: expr, $span: expr) => {
        SourceSpan {
            file: $file,
            span: $span,
        }
    };
}

#[macro_export]
macro_rules! gen_err {
    ($errs:expr, $err_typ:expr, $err:expr) => {{
        $errs.push($err);
        $err_typ
    }};
}

pub fn parse(code: &str, file: usize) -> (Vec<TopLvl>, Vec<Diagnostic<usize>>) {
    let mut errs = Vec::new();
    let ast = grammar::HexagnParser::new()
        .parse(file, &mut errs, code)
        .unwrap_or_else(|e| {
            println!("{}", e);
            exit(-1);
        });
    let mut diags = Vec::new();
    for err in errs {
        let mut diagnostic = Diagnostic::error();
        match err {
            ParseError::ExtraToken { token } => {
                diagnostic = diagnostic.with_message("Extra token");
                diagnostic =
                    diagnostic.with_labels(vec![codespan_reporting::diagnostic::Label::primary(
                        0,
                        token.0..token.2,
                    )
                    .with_message("Extra token")]);
            }
            ParseError::InvalidToken { location } => {
                diagnostic = diagnostic.with_message("Invalid token");
                diagnostic =
                    diagnostic.with_labels(vec![codespan_reporting::diagnostic::Label::primary(
                        0,
                        location..location,
                    )
                    .with_message("Invalid token")]);
            }
            ParseError::UnrecognizedEof { location, expected } => {
                diagnostic = diagnostic.with_message("Unexpected EOF");
                diagnostic =
                    diagnostic.with_labels(vec![codespan_reporting::diagnostic::Label::primary(
                        0,
                        location..location,
                    )
                    .with_message(format!("Expected {}", expected.join(", ")))]);
            }
            ParseError::UnrecognizedToken {
                token: (start, _, end),
                expected,
            } => {
                diagnostic = diagnostic.with_message("Unrecognized token");
                diagnostic =
                    diagnostic.with_labels(vec![codespan_reporting::diagnostic::Label::primary(
                        0,
                        start..end,
                    )
                    .with_message(format!("Expected {}", expected.join(", ")))]);
            }
            _ => (),
        }
        diags.push(diagnostic);
    }
    (ast, diags)
}
