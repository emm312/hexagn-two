use std::{collections::HashMap, fmt::Display};

use codespan_reporting::diagnostic::{Diagnostic, Label, LabelStyle};

use crate::ast::{BuiltinType, Expr, SourceSpan, Stmt, TopLvl, Type};

pub fn typecheck(ast: &Vec<TopLvl>) -> Result<(), Diagnostic<usize>> {
    let mut typechecker = Typechecker::new();
    typechecker.typecheck(ast)
}

struct Typechecker {
    // mangled name along with return and arg types
    funcs: Vec<(String, Type, Vec<Type>)>,
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
            if let TopLvl::FuncDef(typ, name, args, body, _) = toplvl {
                self.funcs.push((
                    name.clone(),
                    typ.clone(),
                    args.iter().map(|(t, _)| t.clone()).collect(),
                ));
                bodies_to_check.push((body, args, typ));
            }
        }
        for (body, args, _) in bodies_to_check {
            self.vars = args.iter().fold(HashMap::new(), |mut acc, (typ, name)| {
                acc.insert(name.clone(), typ.clone());
                acc
            });
            for stmt in body {
                match stmt {
                    Stmt::VarDecl(typ, name, expr, span) => {
                        self.vars.insert(name.clone(), typ.clone());
                        if let Some(expr) = expr {
                            let expr_t = self.infer_expr(expr.clone())?;
                            if &expr_t != typ {
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
                        if expr.is_some() {
                            self.infer_expr(expr.as_ref().unwrap().clone())?;
                        }
                    }
                    Stmt::Call(name, args, span) => {
                        let mut args_t = Vec::new();
                        for arg in args {
                            args_t.push(self.infer_expr(arg.clone())?)
                        }
                        let possible_funcs = self
                            .funcs
                            .iter()
                            .filter(|(fname, _, _)| fname == name)
                            .collect::<Vec<_>>();
                        let func_ret = possible_funcs.iter().find_map(|(fname, ret, fargs)| {
                            if fname == name && &args_t == fargs {
                                Some(ret)
                            } else {
                                None
                            }
                        });

                        match func_ret {
                            Some(_) => return Ok(()),
                            None => {
                                return Err(Diagnostic::error()
                                    .with_message("Invalid arguments to function")
                                    .with_labels(vec![Label::new(
                                        LabelStyle::Primary,
                                        span.file,
                                        span.span.clone(),
                                    )
                                    .with_message(format!("Function arguments have types {}", {
                                        String::from("[")
                                            + &args_t
                                                .iter()
                                                .map(|t| format!("{}", t))
                                                .collect::<Vec<_>>()
                                                .join(", ")
                                            + "]"
                                    }))])
                                    .with_notes(vec![format!(
                                        "Possible argument types are {}",
                                        possible_funcs
                                            .iter()
                                            .map(|(_, _, args)| String::from("[")
                                                + &args
                                                    .iter()
                                                    .map(|t| format!("{}", t))
                                                    .collect::<Vec<_>>()
                                                    .join(", ")
                                                + "]")
                                            .collect::<Vec<_>>()
                                            .join(" or ")
                                    )]));
                            }
                        }
                    }

                    _ => todo!(),
                }
            }
        }
        Ok(())
    }

    fn infer_expr(&self, expr: Expr) -> Result<BaserBaseType, Diagnostic<usize>> {
        match expr {
            Expr::BinOp(lhs, _, rhs, _) => {
                let lhs_t = self.infer_expr(*lhs.clone())?;
                let rhs_t = self.infer_expr(*rhs.clone())?;
                if lhs_t != rhs_t {
                    Err(Diagnostic::error()
                        .with_message(format!("Type mismatch: {} and {}", lhs_t, rhs_t))
                        .with_labels(vec![
                            Label::new(
                                LabelStyle::Secondary,
                                lhs.clone().get_span().file,
                                lhs.clone().get_span().span,
                            ),
                            Label::new(
                                LabelStyle::Secondary,
                                rhs.clone().get_span().file,
                                rhs.clone().get_span().span,
                            ),
                        ]))
                } else {
                    Ok(lhs_t)
                }
            }
            Expr::Array(exprs, span) => {
                let mut typ = exprs
                    .iter()
                    .map(|expr| self.infer_expr(expr.clone()))
                    .collect::<Result<Vec<_>, Diagnostic<usize>>>()?;
                typ.dedup();
                if typ.len() == 1 {
                    Ok(BaserBaseType::WhateverIsNotAConcreteType(
                        BaseType::Array(Box::new(typ.pop().unwrap()), exprs.len()),
                        span,
                    ))
                } else {
                    Err(Diagnostic::error()
                        .with_message("Type mismatch")
                        .with_labels(vec![
                            Label::new(LabelStyle::Primary, span.file, span.span.clone())
                                .with_message(format!(
                                    "This has type: {}",
                                    String::from("[")
                                        + &typ
                                            .iter()
                                            .map(|t| format!("{}", t))
                                            .collect::<Vec<_>>()
                                            .join(", ")
                                        + "]"
                                )),
                            Label::new(LabelStyle::Secondary, span.file, span.span)
                                .with_message("Array cannot have mixed types"),
                        ]))
                }
            }
            Expr::Float(_, span) => Ok(BaserBaseType::WhateverIsNotAConcreteType(
                BaseType::Float,
                span,
            )),
            Expr::String(s, span) => Ok(BaserBaseType::Concrete(Type::Array(
                Box::new(Type::Builtin(BuiltinType::Uint8, span.clone())),
                s.len(),
                span,
            ))),
            Expr::Index(_, _, _span) => todo!("indexing"),
            Expr::Call(name, args, span) => {
                let mut args_t = Vec::new();
                for arg in args {
                    args_t.push(self.infer_expr(arg)?)
                }
                let possible_funcs = self
                    .funcs
                    .iter()
                    .filter(|(fname, _, _)| fname == &name)
                    .collect::<Vec<_>>();
                let func_ret = possible_funcs.iter().find_map(|(fname, ret, fargs)| {
                    if fname == &name && &args_t == fargs {
                        Some(ret)
                    } else {
                        None
                    }
                });

                match func_ret {
                    Some(typ) => Ok(BaserBaseType::Concrete(typ.clone())),
                    None => Err(Diagnostic::error()
                        .with_message("Invalid arguments to function")
                        .with_labels(vec![Label::new(
                            LabelStyle::Primary,
                            span.file,
                            span.span.clone(),
                        )
                        .with_message(format!(
                            "Function arguments have types {}",
                            {
                                String::from("[")
                                    + &args_t
                                        .iter()
                                        .map(|t| format!("{}", t))
                                        .collect::<Vec<_>>()
                                        .join(", ")
                                    + "]"
                            }
                        ))])
                        .with_notes(vec![format!(
                            "Possible argument types are {}",
                            possible_funcs
                                .iter()
                                .map(|(_, _, args)| String::from("[")
                                    + &args
                                        .iter()
                                        .map(|t| format!("{}", t))
                                        .collect::<Vec<_>>()
                                        .join(", ")
                                    + "]")
                                .collect::<Vec<_>>()
                                .join(" or ")
                        )])),
                }
            }
            Expr::Neg(expr, span) => {
                let typ = self.infer_expr(*expr)?;
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
                let typ = self.infer_expr(*expr)?;
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
            Expr::Int(..) => Ok(BaserBaseType::WhateverIsNotAConcreteType(
                BaseType::Integer,
                expr.get_span(),
            )),
            Expr::Err => Ok(BaserBaseType::Concrete(Type::Err)),

            _ => todo!("TODO: {:#?}", expr),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum BaseType {
    Integer,
    Float,
    Array(Box<BaserBaseType>, usize),
}

#[derive(Clone, PartialEq)]
pub enum BaserBaseType {
    Concrete(Type),
    WhateverIsNotAConcreteType(BaseType, SourceSpan),
}

impl PartialEq<Type> for BaseType {
    fn eq(&self, other: &Type) -> bool {
        match self {
            Self::Integer => {
                if let Type::Builtin(builtin, _) = other {
                    matches!(
                        builtin,
                        BuiltinType::Int8
                            | BuiltinType::Int16
                            | BuiltinType::Int32
                            | BuiltinType::Int64
                            | BuiltinType::Uint8
                            | BuiltinType::Uint16
                            | BuiltinType::Uint32
                            | BuiltinType::Uint64
                    )
                } else {
                    false
                }
            }
            Self::Float => {
                if let Type::Builtin(builtin, _) = other {
                    matches!(builtin, BuiltinType::Float32 | BuiltinType::Float64)
                } else {
                    false
                }
            }
            Self::Array(typ, size) => {
                if let Type::Array(othertyp, othersize, _) = other {
                    **typ == **othertyp && size == othersize
                } else {
                    false
                }
            }
        }
    }
}

impl PartialEq<Type> for BaserBaseType {
    fn eq(&self, other: &Type) -> bool {
        match self {
            Self::Concrete(typ) => typ == other,
            Self::WhateverIsNotAConcreteType(typ, _) => typ == other,
        }
    }
}

impl Display for BaseType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer => write!(f, "Int")?,
            Self::Float => write!(f, "Float")?,
            Self::Array(typ, size) => write!(f, "Array[{}; {}]", typ, size)?,
        }

        Ok(())
    }
}

impl Display for BaserBaseType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Concrete(typ) => write!(f, "{}", typ)?,
            Self::WhateverIsNotAConcreteType(base, _) => write!(f, "{}", base)?,
        }

        Ok(())
    }
}
