use std::{collections::HashMap, path::Path};

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, OptimizationLevel,
};

use crate::{
    ast::{BinOp, BuiltinType, FuncAttribute, Type},
    func_mangling,
    typed_ast::{TypedExpr, TypedStmt, TypedTopLvl},
};

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,

    cur_fn: Option<FunctionValue<'ctx>>,
    vars: HashMap<String, (PointerValue<'ctx>, Type)>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn compile(ast: Vec<TypedTopLvl>, filename: &str, output_path: &str) {
        let context = Context::create();
        let module = context.create_module(filename);
        let mut codegen = Codegen {
            context: &context,
            builder: context.create_builder(),
            module,
            cur_fn: None,
            vars: HashMap::new(),
        };
        codegen.compile_ast(ast);
        codegen.module.print_to_stderr();
        codegen.write_to_object(output_path);
    }

    fn write_to_object(&self, output_path: &str) {
        Target::initialize_all(&InitializationConfig::default());
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).unwrap();
        let cpu = TargetMachine::get_host_cpu_name();
        let features = TargetMachine::get_host_cpu_features();
        let reloc = RelocMode::Default;
        let model = CodeModel::Default;
        let opt = OptimizationLevel::Default;
        let target_machine = target
            .create_target_machine(
                &triple,
                cpu.to_str().unwrap(),
                features.to_str().unwrap(),
                opt,
                reloc,
                model,
            )
            .unwrap();

        target_machine
            .write_to_file(&self.module, FileType::Object, &Path::new(output_path))
            .unwrap();
    }

    fn compile_ast(&mut self, ast: Vec<TypedTopLvl>) {
        let _ = ast
            .iter()
            .map(|elem| match elem {
                TypedTopLvl::FuncDef(attrs, typ, name, args, _, _span) => {
                    let mname = if attrs.contains(&FuncAttribute::NoMangle) {
                        name.clone()
                    } else {
                        func_mangling::mangle_function(
                            &name,
                            &args.into_iter().map(|(e, _)| e.clone()).collect::<Vec<_>>(),
                            &typ,
                        )
                    };
                    let args = args
                        .iter()
                        .map(|(t, _)| self.to_llvm_t(t))
                        .collect::<Vec<_>>();
                    let fn_t = Self::as_fn_type(self.to_llvm_t(&typ), args);
                    self.module.add_function(&mname, fn_t, {
                        if attrs.contains(&FuncAttribute::External) {
                            Some(Linkage::External)
                        } else {
                            None
                        }
                    });
                }
                _ => (),
            })
            .collect::<Vec<()>>();

        for def in ast {
            match def {
                TypedTopLvl::FuncDef(attrs, typ, name, args, body, _span) => {
                    let mname = if attrs.contains(&FuncAttribute::NoMangle) {
                        name.clone()
                    } else {
                        func_mangling::mangle_function(
                            &name,
                            &args.iter().map(|(e, _)| e.clone()).collect::<Vec<_>>(),
                            &typ,
                        )
                    };
                    let fn_t = self.module.get_function(&mname).unwrap();
                    self.cur_fn = Some(fn_t);
                    if body.len() > 0 {
                        self.compile_stmts(body, args, typ, fn_t);
                    }
                }
                _ => todo!(),
            }
        }
    }

    fn compile_stmts(
        &mut self,
        stmts: Vec<TypedStmt>,
        args: Vec<(Type, String)>,
        _ret_t: Type,
        fn_t: FunctionValue<'ctx>,
    ) {
        let allocas = self.context.append_basic_block(fn_t, "allocas");
        self.builder.position_at_end(allocas);
        for (t, name) in args {
            let alloca = self
                .builder
                .build_alloca(BasicTypeEnum::try_from(self.to_llvm_t(&t)).unwrap(), &name)
                .unwrap();
            self.builder
                .build_store(alloca, fn_t.get_nth_param(0).unwrap())
                .unwrap();
            self.vars.insert(name, (alloca, t));
        }
        let entry = self.context.append_basic_block(fn_t, "entry");
        self.builder.position_at_end(entry);

        for stmt in stmts {
            match stmt {
                TypedStmt::VarDecl(typ, name, expr, _) => {
                    let return_to = self.builder.get_insert_block().unwrap();
                    self.builder.position_at_end(allocas);
                    let alloca = self
                        .builder
                        .build_alloca(
                            BasicTypeEnum::try_from(self.to_llvm_t(&typ)).unwrap(),
                            &name,
                        )
                        .unwrap();
                    self.vars.insert(name, (alloca, typ));
                    self.builder.position_at_end(return_to);
                    if let Some(expr) = expr {
                        let expr_c = self.compile_expr(expr);
                        self.builder.build_store(alloca, expr_c).unwrap();
                    }
                }
                TypedStmt::Assign(name, expr, _) => {
                    let expr_c = self.compile_expr(expr);
                    let ptr = self.vars.get(&name).unwrap();
                    self.builder.build_store(ptr.0, expr_c).unwrap();
                }
                TypedStmt::Call(ret_t, name, args, _) => {
                    let fn_t = self.module.get_function(&name).unwrap_or_else(|| {
                        self.module
                            .get_function(&func_mangling::mangle_function(
                                &name,
                                &args.iter().map(|(a, _)| a.clone()).collect::<Vec<Type>>(),
                                &ret_t,
                            ))
                            .unwrap()
                    });
                    let args = args
                        .iter()
                        .map(|(_, expr)| self.compile_expr(expr.clone()).try_into().unwrap())
                        .collect::<Vec<_>>();

                    self.builder
                        .build_call(fn_t, &args, &name)
                        .unwrap();
                }
                TypedStmt::Return(expr, _) => {
                    let expr_c = if let Some(expr) = expr {
                        self.compile_expr(expr)
                    } else {
                        BasicValueEnum::IntValue(self.context.i32_type().const_int(0, false))
                    };
                    self.builder.build_return(Some(&expr_c)).unwrap();
                }
                _ => todo!(),
            }
        }
        self.builder.position_at_end(allocas);
        self.builder.build_unconditional_branch(entry).unwrap();
    }

    fn as_fn_type(ret: AnyTypeEnum<'ctx>, args: Vec<AnyTypeEnum<'ctx>>) -> FunctionType<'ctx> {
        let args_n = args
            .into_iter()
            .map(|elem| BasicMetadataTypeEnum::try_from(elem).unwrap())
            .collect::<Vec<_>>();
        match BasicTypeEnum::try_from(ret) {
            Ok(v) => v.fn_type(&args_n, false),
            Err(_) => match ret {
                AnyTypeEnum::VoidType(v) => v.fn_type(args_n.as_slice(), false),
                AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::IntType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_) => unreachable!(),
                _ => todo!(),
            },
        }
    }

    fn to_llvm_t(&self, typ: &Type) -> AnyTypeEnum<'ctx> {
        match typ {
            Type::Array(t, len, _) => {
                let t = self.to_llvm_t(t);
                AnyTypeEnum::ArrayType(BasicTypeEnum::try_from(t).unwrap().array_type(*len as u32))
            }
            Type::Builtin(t, _) => self.builtin_to_llvm(t),
            Type::Const(t, _) => self.to_llvm_t(t),
            Type::Ptr(t, _) => {
                let t = self.to_llvm_t(t);
                AnyTypeEnum::PointerType(
                    BasicTypeEnum::try_from(t)
                        .unwrap()
                        .ptr_type(AddressSpace::default()),
                )
            }
            Type::Struct(name, _) => {
                let struct_t = self.module.get_struct_type(&name);
                if struct_t.is_some() {
                    AnyTypeEnum::StructType(struct_t.unwrap())
                } else {
                    panic!("Struct {} not found", name);
                }
            }
            _ => unreachable!(),
        }
    }

    fn builtin_to_llvm(&self, typ: &BuiltinType) -> AnyTypeEnum<'ctx> {
        match typ {
            BuiltinType::Int8 => self.context.i8_type().as_any_type_enum(),
            BuiltinType::Int16 => self.context.i16_type().as_any_type_enum(),
            BuiltinType::Int32 => self.context.i32_type().as_any_type_enum(),
            BuiltinType::Int64 => self.context.i64_type().as_any_type_enum(),
            BuiltinType::Uint8 => self.context.i8_type().as_any_type_enum(),
            BuiltinType::Uint16 => self.context.i16_type().as_any_type_enum(),
            BuiltinType::Uint32 => self.context.i32_type().as_any_type_enum(),
            BuiltinType::Uint64 => self.context.i64_type().as_any_type_enum(),
            BuiltinType::Float32 => self.context.f32_type().as_any_type_enum(),
            BuiltinType::Float64 => self.context.f64_type().as_any_type_enum(),
            BuiltinType::Bool => self.context.bool_type().as_any_type_enum(),
            BuiltinType::Void => self.context.void_type().as_any_type_enum(),
        }
    }

    pub fn compile_expr(&self, expr: TypedExpr) -> BasicValueEnum<'ctx> {
        match expr {
            TypedExpr::BinOp(lhs, op, rhs, _) => {
                let lhs_c = self.compile_expr(*lhs);
                let rhs_c = self.compile_expr(*rhs);
                match op {
                    BinOp::Add => match lhs_c {
                        BasicValueEnum::FloatValue(v) => BasicValueEnum::FloatValue(
                            self.builder
                                .build_float_add(v, rhs_c.into_float_value(), "addtmp")
                                .unwrap(),
                        ),
                        BasicValueEnum::IntValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_int_add(v, rhs_c.into_int_value(), "addtmp")
                                .unwrap(),
                        ),
                        _ => todo!(),
                    },
                    BinOp::Sub => match lhs_c {
                        BasicValueEnum::FloatValue(v) => BasicValueEnum::FloatValue(
                            self.builder
                                .build_float_sub(v, rhs_c.into_float_value(), "subtmp")
                                .unwrap(),
                        ),
                        BasicValueEnum::IntValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_int_sub(v, rhs_c.into_int_value(), "subtmp")
                                .unwrap(),
                        ),
                        _ => todo!(),
                    },
                    BinOp::Mul => match lhs_c {
                        BasicValueEnum::FloatValue(v) => BasicValueEnum::FloatValue(
                            self.builder
                                .build_float_mul(v, rhs_c.into_float_value(), "multmp")
                                .unwrap(),
                        ),
                        BasicValueEnum::IntValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_int_mul(v, rhs_c.into_int_value(), "multmp")
                                .unwrap(),
                        ),
                        _ => todo!(),
                    },
                    BinOp::Div => match lhs_c {
                        BasicValueEnum::FloatValue(v) => BasicValueEnum::FloatValue(
                            self.builder
                                .build_float_div(v, rhs_c.into_float_value(), "divtmp")
                                .unwrap(),
                        ),
                        BasicValueEnum::IntValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_int_signed_div(v, rhs_c.into_int_value(), "divtmp")
                                .unwrap(),
                        ),
                        _ => todo!(),
                    },
                    BinOp::Mod => match lhs_c {
                        BasicValueEnum::FloatValue(v) => BasicValueEnum::FloatValue(
                            self.builder
                                .build_float_rem(v, rhs_c.into_float_value(), "modtmp")
                                .unwrap(),
                        ),
                        BasicValueEnum::IntValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_int_signed_rem(v, rhs_c.into_int_value(), "modtmp")
                                .unwrap(),
                        ),
                        _ => todo!(),
                    },
                    BinOp::Eq => match lhs_c {
                        BasicValueEnum::FloatValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_float_compare(
                                    inkwell::FloatPredicate::OEQ,
                                    v,
                                    rhs_c.into_float_value(),
                                    "eqtmp",
                                )
                                .unwrap(),
                        ),
                        BasicValueEnum::IntValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_int_compare(
                                    inkwell::IntPredicate::EQ,
                                    v,
                                    rhs_c.into_int_value(),
                                    "eqtmp",
                                )
                                .unwrap(),
                        ),
                        _ => todo!(),
                    },
                    BinOp::Neq => match lhs_c {
                        BasicValueEnum::FloatValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_float_compare(
                                    inkwell::FloatPredicate::ONE,
                                    v,
                                    rhs_c.into_float_value(),
                                    "neqtmp",
                                )
                                .unwrap(),
                        ),
                        BasicValueEnum::IntValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_int_compare(
                                    inkwell::IntPredicate::NE,
                                    v,
                                    rhs_c.into_int_value(),
                                    "neqtmp",
                                )
                                .unwrap(),
                        ),
                        _ => todo!(),
                    },
                    BinOp::Lt => match lhs_c {
                        BasicValueEnum::FloatValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_float_compare(
                                    inkwell::FloatPredicate::OLT,
                                    v,
                                    rhs_c.into_float_value(),
                                    "lttmp",
                                )
                                .unwrap(),
                        ),
                        BasicValueEnum::IntValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_int_compare(
                                    inkwell::IntPredicate::SLT,
                                    v,
                                    rhs_c.into_int_value(),
                                    "lttmp",
                                )
                                .unwrap(),
                        ),
                        _ => todo!(),
                    },
                    BinOp::Gt => match lhs_c {
                        BasicValueEnum::FloatValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_float_compare(
                                    inkwell::FloatPredicate::OGT,
                                    v,
                                    rhs_c.into_float_value(),
                                    "gttmp",
                                )
                                .unwrap(),
                        ),
                        BasicValueEnum::IntValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_int_compare(
                                    inkwell::IntPredicate::SGT,
                                    v,
                                    rhs_c.into_int_value(),
                                    "gttmp",
                                )
                                .unwrap(),
                        ),
                        _ => todo!(),
                    },
                    BinOp::Leq => match lhs_c {
                        BasicValueEnum::FloatValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_float_compare(
                                    inkwell::FloatPredicate::OLE,
                                    v,
                                    rhs_c.into_float_value(),
                                    "leqtmp",
                                )
                                .unwrap(),
                        ),
                        BasicValueEnum::IntValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_int_compare(
                                    inkwell::IntPredicate::SLE,
                                    v,
                                    rhs_c.into_int_value(),
                                    "leqtmp",
                                )
                                .unwrap(),
                        ),
                        _ => todo!(),
                    },
                    BinOp::Geq => match lhs_c {
                        BasicValueEnum::FloatValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_float_compare(
                                    inkwell::FloatPredicate::OGE,
                                    v,
                                    rhs_c.into_float_value(),
                                    "geqtmp",
                                )
                                .unwrap(),
                        ),
                        BasicValueEnum::IntValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_int_compare(
                                    inkwell::IntPredicate::SGE,
                                    v,
                                    rhs_c.into_int_value(),
                                    "geqtmp",
                                )
                                .unwrap(),
                        ),
                        _ => todo!(),
                    },
                    BinOp::And => match lhs_c {
                        BasicValueEnum::IntValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_and(v, rhs_c.into_int_value(), "andtmp")
                                .unwrap(),
                        ),
                        _ => todo!(),
                    },
                    BinOp::Or => match lhs_c {
                        BasicValueEnum::IntValue(v) => BasicValueEnum::IntValue(
                            self.builder
                                .build_or(v, rhs_c.into_int_value(), "ortmp")
                                .unwrap(),
                        ),
                        _ => todo!(),
                    },
                }
            }
            TypedExpr::Array(_exprs, _) => {
                todo!()
            }
            TypedExpr::Call(typ, name, args, _) => {
                let fn_t = self.module.get_function(&name).unwrap_or_else(|| {
                    self.module
                        .get_function(&func_mangling::mangle_function(
                            &name,
                            &args.iter().map(|(a, _)| a.clone()).collect::<Vec<Type>>(),
                            &typ,
                        ))
                        .unwrap()
                });
                let args = args
                    .iter()
                    .map(|(_, expr)| self.compile_expr(expr.clone()).try_into().unwrap())
                    .collect::<Vec<_>>();

                self.builder
                    .build_call(fn_t, &args, &name)
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap()
            }
            TypedExpr::Float(val, _) => {
                BasicValueEnum::FloatValue(self.context.f32_type().const_float(val as f64))
            }
            TypedExpr::Ident(name, _) => {
                let ptr = self.vars.get(&name).unwrap();
                self.builder
                    .build_load(
                        BasicTypeEnum::try_from(self.to_llvm_t(&ptr.1)).unwrap(),
                        ptr.0,
                        "load",
                    )
                    .unwrap()
            }
            TypedExpr::Int(val, _) => {
                BasicValueEnum::IntValue(self.context.i32_type().const_int(val as u64, false))
            }
            TypedExpr::Index(name, expr, _) => {
                let ptr = self.vars.get(&name).unwrap();
                let idx = self.compile_expr(*expr);
                let ptr_l = unsafe {
                    self.builder.build_gep(
                        BasicTypeEnum::try_from(self.to_llvm_t(&ptr.1)).unwrap(),
                        ptr.0,
                        &[idx.into_int_value()],
                        "idx",
                    )
                }
                .unwrap();
                self.builder
                    .build_load(
                        BasicTypeEnum::try_from(self.to_llvm_t(&ptr.1)).unwrap(),
                        ptr_l,
                        "load",
                    )
                    .unwrap()
            }
            _ => todo!(),
        }
    }
}
