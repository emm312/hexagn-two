use std::{collections::HashMap, fmt::Display};

use codespan_reporting::diagnostic::{Diagnostic, Label, LabelStyle};

use crate::{
    ast::{BuiltinType, Expr, FuncAttribute, SourceSpan, Stmt, TopLvl, Type},
    typed_ast::{TypedExpr, TypedStmt, TypedTopLvl},
};

pub fn typecheck(ast: &Vec<TopLvl>) -> Result<Vec<TypedTopLvl>, Diagnostic<usize>> {
    let mut typechecker = Typechecker::new();
    typechecker.typecheck(ast)
}

struct Typechecker {
    // mangled name along with return and arg types
    funcs: Vec<(Vec<FuncAttribute>, String, Type, Vec<Type>)>,
    vars: HashMap<String, Type>,
}

impl Typechecker {
    fn new() -> Self {
        Self {
            funcs: Vec::new(),
            vars: HashMap::new(),
        }
    }

    fn typecheck(&mut self, ast: &Vec<TopLvl>) -> Result<Vec<TypedTopLvl>, Diagnostic<usize>> {
        let mut bodies_to_check = Vec::new();
        let mut typed_ast = Vec::new();
        for toplvl in ast {
            if let TopLvl::FuncDef(attrs, typ, name, args, body, span) = toplvl {
                self.funcs.push((
                    attrs.clone(),
                    name.clone(),
                    typ.clone(),
                    args.iter().map(|(t, _)| t.clone()).collect(),
                ));
                bodies_to_check.push((attrs, body, args, typ, name, span));
            } else if let TopLvl::StructDef(name, fields, span) = toplvl {
                typed_ast.push(TypedTopLvl::StructDef(
                    name.clone(),
                    fields.clone(),
                    span.clone(),
                ));
            } else if let TopLvl::Import(name, span) = toplvl {
                typed_ast.push(TypedTopLvl::Import(name.clone(), span.clone()));
            }
        }
        for (attrs, body, args, func_checked_ret_t, name, span) in bodies_to_check {
            let mut fn_body = Vec::new();
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
                        fn_body.push(TypedStmt::VarDecl(
                            typ.clone(),
                            name.to_string(),
                            expr.clone().map(|expr| self.to_typed_expr(&expr)),
                            span.clone(),
                        ));
                    }
                    Stmt::Return(expr, span) => {
                        if expr.is_some() {
                            self.infer_expr(expr.as_ref().unwrap().clone())?;
                        }
                        fn_body.push(TypedStmt::Return(
                            expr.clone().map(|e| self.to_typed_expr(&e)),
                            span.clone(),
                        ))
                    }
                    Stmt::Call(name, args, span) => {
                        let mut fargs_t = Vec::new();
                        let mut args_t = Vec::new();
                        for arg in args {
                            args_t.push(self.infer_expr(arg.clone())?)
                        }
                        let possible_funcs = self
                            .funcs
                            .iter()
                            .filter(|(_, fname, _, _)| fname == name)
                            .collect::<Vec<_>>();
                        let func_ret = possible_funcs.iter().find_map(|(_, fname, ret, fargs)| {
                            if fname == name && &args_t == fargs {
                                fargs_t = fargs.clone();
                                Some(ret)
                            } else {
                                None
                            }
                        });
                        let ret_t_call;
                        match func_ret {
                            Some(ret) => ret_t_call = ret,
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
                                            .map(|(_, _, _, args)| String::from("[")
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
                        fn_body.push(TypedStmt::Call(
                            ret_t_call.clone(),
                            name.clone(),
                            fargs_t
                                .into_iter()
                                .zip(args.into_iter().map(|e| self.to_typed_expr(e)))
                                .collect(),
                            span.clone(),
                        ))
                    }
                    Stmt::Assign(var, expr, _) => {
                        let var_t = self.vars.get(var).unwrap();
                        let expr_t = self.infer_expr(expr.clone())?;
                        if &expr_t == var_t {
                            return Err(Diagnostic::error()
                                .with_message(format!("Type mismatch: {} and {}", var_t, expr_t))
                                .with_labels(vec![
                                    Label::new(
                                        LabelStyle::Primary,
                                        span.file,
                                        span.span.start..span.span.end,
                                    )
                                    .with_message("declared here"),
                                    Label::new(
                                        LabelStyle::Secondary,
                                        expr.get_span().file,
                                        expr.get_span().span.start..expr.get_span().span.end,
                                    )
                                    .with_message("expression has this type"),
                                ]));
                        }
                        fn_body.push(TypedStmt::Assign(
                            var.clone(),
                            self.to_typed_expr(&expr),
                            span.clone(),
                        ))
                    }
                    //Stmt::If(cond, if_body, else_body, span) => {
                    //    let cond_t = self.infer_expr(cond.clone())?;
                    //    if cond_t != Type::Builtin(BuiltinType::Bool, span.clone()) {
                    //        return Err(Diagnostic::error()
                    //            .with_message(format!(
                    //                "Type mismatch: {} and {}",
                    //                cond_t,
                    //                Type::Builtin(BuiltinType::Bool, span.clone())
                    //            ))
                    //            .with_labels(vec![Label::new(
                    //                LabelStyle::Primary,
                    //                span.file,
                    //                span.span.clone(),
                    //            )
                    //            .with_message("Condition has type")]));
                    //    }
                    //    let mut typed_if_body = Vec::new();
                    //    for stmt in if_body {
                    //        typed_if_body.push(self.to_typed_stmt(stmt));
                    //    }
                    //    let mut typed_else_body = Vec::new();
                    //    if let Some(else_body) = else_body {
                    //        for stmt in else_body {
                    //            typed_else_body.push(self.to_typed_stmt(stmt));
                    //        }
                    //    }
                    //    fn_body.push(TypedStmt::If(
                    //        self.to_typed_expr(&cond),
                    //        typed_if_body,
                    //        Some(typed_else_body),
                    //        span.clone(),
                    //    ))
                    //}
                    _ => todo!(),
                }
            }
            typed_ast.push(TypedTopLvl::FuncDef(
                attrs.clone(),
                func_checked_ret_t.clone(),
                name.clone(),
                args.clone(),
                fn_body,
                span.clone(),
            ))
        }
        Ok(typed_ast)
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
                    .filter(|(_, fname, _, _)| fname == &name)
                    .collect::<Vec<_>>();
                let func_ret = possible_funcs.iter().find_map(|(_, fname, ret, fargs)| {
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
                                .map(|(_, _, _, args)| String::from("[")
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
            Expr::Ident(i, span) => {
                if let Some(typ) = self.vars.get(&i) {
                    Ok(BaserBaseType::Concrete(typ.clone()))
                } else {
                    Err({
                        Diagnostic::error()
                            .with_message("Variable does not exist")
                            .with_labels(vec![Label::new(
                                LabelStyle::Primary,
                                span.file,
                                span.span.clone(),
                            )])
                    })
                }
            }
        }
    }

    fn to_typed_expr(&self, expr: &Expr) -> TypedExpr {
        match expr {
            Expr::BinOp(lhs, op, rhs, span) => TypedExpr::BinOp(
                Box::new(self.to_typed_expr(&lhs)),
                *op,
                Box::new(self.to_typed_expr(&rhs)),
                span.clone(),
            ),
            Expr::Array(exprs, span) => {
                let typed_exprs = exprs
                    .iter()
                    .map(|elem| self.to_typed_expr(elem))
                    .collect::<Vec<TypedExpr>>();
                TypedExpr::Array(typed_exprs, span.clone())
            }
            Expr::Call(name, args, span) => {
                let mut args_t = Vec::new();
                for arg in args {
                    args_t.push(self.infer_expr(arg.clone()).unwrap())
                }

                let possible_funcs = self
                    .funcs
                    .iter()
                    .filter(|(_, fname, _, _)| fname == name)
                    .collect::<Vec<_>>();

                let mut fn_args = Vec::new();
                let func_ret = possible_funcs
                    .iter()
                    .find_map(|(_, fname, ret, fargs)| {
                        if fname == name && &args_t == fargs {
                            fn_args = fargs.clone();
                            Some(ret)
                        } else {
                            None
                        }
                    })
                    .unwrap();

                TypedExpr::Call(
                    func_ret.clone(),
                    name.clone(),
                    fn_args
                        .into_iter()
                        .zip(args.into_iter().map(|e| self.to_typed_expr(e)))
                        .collect(),
                    span.clone(),
                )
            }
            Expr::Ident(i, span) => TypedExpr::Ident(i.clone(), span.clone()),
            Expr::Int(i, span) => TypedExpr::Int(*i, span.clone()),
            Expr::Float(i, span) => TypedExpr::Float(*i, span.clone()),
            Expr::String(i, span) => TypedExpr::String(i.clone(), span.clone()),
            Expr::Index(var, e, span) => {
                TypedExpr::Index(var.clone(), Box::new(self.to_typed_expr(&e)), span.clone())
            }
            Expr::Neg(i, span) => TypedExpr::Neg(Box::new(self.to_typed_expr(&i)), span.clone()),
            Expr::Not(i, span) => TypedExpr::Not(Box::new(self.to_typed_expr(&i)), span.clone()),
            Expr::Err => unreachable!("should have been caught in typechecking"),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum BaseType {
    Integer,
    Float,
    Array(Box<BaserBaseType>, usize),
}

#[derive(Clone)]
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

impl PartialEq for BaserBaseType {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Concrete(typ) => other == typ,
            Self::WhateverIsNotAConcreteType(typ, _) => other == typ,
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

impl PartialEq<BaseType> for BaserBaseType {
    fn eq(&self, other: &BaseType) -> bool {
        match self {
            Self::Concrete(typ) => other == typ,
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
