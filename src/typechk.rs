use std::{collections::HashMap, hash::Hash};

use codespan_reporting::diagnostic::{Diagnostic, Label, LabelStyle};

use crate::{
    ast::{BinOp, BuiltinType, Expr, Stmt, TopLvl, Type},
    func_mangling,
};

pub fn typecheck(ast: &Vec<TopLvl>) -> Result<(), Diagnostic<usize>> {
    let mut typechecker = Typechecker::new();
    typechecker.typecheck(ast)
}

struct Typechecker {
    funcs: Vec<String>, // mangling of the function
    vars: HashMap<String, Type>,
}

impl Typechecker {
    fn new() -> Self {
        Self {
            funcs: Vec::new(),
            vars: HashMap::new(),
        }
    }

    fn typecheck(&mut self, ast: &Vec<TopLvl>) -> Result<(), Diagnostic<usize>> {
        let mut bodies_to_check = Vec::new();
        for toplvl in ast {
            match toplvl {
                TopLvl::FuncDef(typ, name, args, body, _) => {
                    self.funcs
                        .push(func_mangling::mangle_function(name, args, typ));
                    bodies_to_check.push((body, args, typ));
                }
                _ => (),
            }
        }
        for (body, args, ret_t) in bodies_to_check {
            self.vars = args.iter().fold(HashMap::new(), |mut acc, (typ, name)| {
                acc.insert(name.clone(), typ.clone());
                acc
            });
            for stmt in body {
                match stmt {
                    Stmt::VarDecl(typ, name, expr, span) => {
                        self.vars.insert(name.clone(), typ.clone());
                        if let Some(expr) = expr {
                            let expr_t = self.infer_expr(expr.clone(), &Some(typ.clone()))?;
                            if typ != &expr_t {
                                return Err(Diagnostic::error()
                                    .with_message(format!("Type mismatch: {} and {}", typ, expr_t))
                                    .with_labels(vec![
                                        codespan_reporting::diagnostic::Label::primary(
                                            span.file,
                                            span.span.start..span.span.end,
                                        )
                                        .with_message("declared here"),
                                        codespan_reporting::diagnostic::Label::secondary(
                                            span.file,
                                            expr.get_span().span.start..expr.get_span().span.end,
                                        )
                                        .with_message("expression has this type"),
                                    ]));
                            }
                        }
                    }
                    Stmt::Return(expr, _) => {
                        if let Some(_) = expr {
                            self.infer_expr(expr.as_ref().unwrap().clone(), &Some(ret_t.clone()))?;
                        }
                    }
                    _ => todo!(),
                }
            }
        }
        println!("{:#?}", self.funcs);
        Ok(())
    }

    fn infer_expr(&self, expr: Expr, should_be: &Option<Type>) -> Result<Type, Diagnostic<usize>> {
        match expr {
            Expr::BinOp(lhs, _, rhs, _) => {
                let lhs_t = self.infer_expr(*lhs.clone(), should_be)?;
                let rhs_t = self.infer_expr(*rhs.clone(), should_be)?;
                if lhs_t != rhs_t {
                    Err(Diagnostic::error()
                        .with_message(format!("Type mismatch: {} and {}", lhs_t, rhs_t))
                        .with_labels(vec![
                            Label::new(LabelStyle::Secondary, lhs.clone().get_span().file, lhs.clone().get_span().span),
                            Label::new(LabelStyle::Secondary, rhs.clone().get_span().file, rhs.clone().get_span().span),
                        ]))
                } else {
                    Ok(lhs_t)
                }
            }
            Expr::Array(exprs, span) => {
                let mut typ = exprs
                    .iter()
                    .map(|expr| self.infer_expr(expr.clone(), &None))
                    .collect::<Result<Vec<Type>, Diagnostic<usize>>>()?;
                typ.dedup();
                if typ.len() == 1 {
                    Ok(Type::Array(Box::new(typ.pop().unwrap()), exprs.len(), span))
                } else {
                    Err(Diagnostic::error().with_message(format!("Type mismatch: {:?}", typ)))
                }
            }
            Expr::Float(_, span) => Ok(Type::Builtin(BuiltinType::Float32, span)),
            Expr::String(s, span) => Ok(Type::Array(
                Box::new(Type::Builtin(BuiltinType::Uint8, span.clone())),
                s.len(),
                span,
            )),
            Expr::Index(_, _, _span) => todo!("indexing"),
            Expr::Call(_, _, _span) => todo!("func calls"),
            Expr::Neg(expr, span) => {
                let typ = self.infer_expr(*expr, should_be)?;
                if typ == Type::Builtin(BuiltinType::Float32, span.clone()) {
                    Ok(typ)
                } else {
                    Err(Diagnostic::error().with_message(format!(
                        "Type mismatch: {} and {}",
                        typ,
                        Type::Builtin(BuiltinType::Float32, span)
                    )))
                }
            }
            Expr::Not(expr, span) => {
                let typ = self.infer_expr(*expr, should_be)?;
                if typ == Type::Builtin(BuiltinType::Bool, span.clone()) {
                    Ok(typ)
                } else {
                    Err(Diagnostic::error().with_message(format!(
                        "Type mismatch: {} and {}",
                        typ,
                        Type::Builtin(BuiltinType::Bool, span)
                    )))
                }
            }
            Expr::Int(..) => {
                return Ok(Type::Builtin(BuiltinType::Int32, expr.get_span()));
            }
            Expr::Err => Ok(Type::Err),
            _ => todo!("func calls {:#?}", expr),
        }
    }
}


pub enum BaseType {
    Integer,
    Float,
    Bool,
    Array(Box<BaseType>, usize),
    Struct(String)
}

impl PartialEq<Type> for BaseType {
    fn eq(&self, other: &Type) -> bool {
        match self {
            Self::Integer => {
                if let Type::Builtin(builtin, _) = other {
                    match builtin {
                        BuiltinType::Int8 | BuiltinType::Int16 | BuiltinType::Int32 | BuiltinType::Int64 | BuiltinType::Uint8 | BuiltinType::Uint16 | BuiltinType::Uint32 | BuiltinType::Uint64 => true,
                        _ => false
                    }
                } else {
                    false
                }
            }
            Self::Float => {
                if let Type::Builtin(builtin, _) = other {
                    match builtin {
                        BuiltinType::Float32 | BuiltinType::Float64 => true,
                        _ => false
                    }
                } else {
                    false
                }
            }
            Self::Bool => {
                if let Type::Builtin(builtin, _) = other {
                    match builtin {
                        BuiltinType::Bool => true,
                        _ => false
                    }
                } else {
                    false
                }
            }
            Self::Array(typ, len) => {
                if let Type::Array(typ2, len2, _) = other {
                    **typ == **typ2 && len == len2
                } else {
                    false
                }
            }
            Self::Struct(name) => {
                if let Type::Struct(name2, _) = other {
                    name == name2
                } else {
                    false
                }
            }
        }
    }
}