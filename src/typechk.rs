use std::{collections::HashMap, hash::Hash};

use codespan_reporting::diagnostic::Diagnostic;

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
                let lhs = self.infer_expr(*lhs, should_be)?;
                let rhs = self.infer_expr(*rhs, should_be)?;
                if lhs != rhs {
                    Err(Diagnostic::error()
                        .with_message(format!("Type mismatch: {} and {}", lhs, rhs)))
                } else {
                    Ok(lhs)
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
