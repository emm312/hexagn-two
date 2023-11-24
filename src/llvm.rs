use std::{collections::HashMap, path::Path};

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{FunctionValue, PointerValue, BasicMetadataValueEnum, BasicValueEnum},
    AddressSpace, OptimizationLevel,
};

use crate::{
    ast::{BuiltinType, FuncAttribute, Type, BinOp},
    func_mangling,
    typed_ast::{TypedTopLvl, TypedExpr},
};

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,

    cur_fn: Option<FunctionValue<'ctx>>,
    vars: HashMap<String, PointerValue<'ctx>>,
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
        let _ = ast.iter().map(|elem| match elem {
            TypedTopLvl::FuncDef(attrs, typ, name, args, _, _span) => {
                let mname = if attrs.contains(&FuncAttribute::NoMangle) {
                    name.clone()
                } else {
                    func_mangling::mangle_function(&name, &args, &typ)
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
        }).collect::<Vec<()>>();

        for def in ast {
            match def {
                TypedTopLvl::FuncDef(attrs, typ, name, args, body, _span) => {
                    let mname = if attrs.contains(&FuncAttribute::NoMangle) {
                        name.clone()
                    } else {
                        func_mangling::mangle_function(&name, &args, &typ)
                    };
                    let fn_t = self.module.get_function(&mname).unwrap();
                    self.cur_fn = Some(fn_t);
                    if body.len() > 0 {
                        let entry = self.context.append_basic_block(fn_t, "entry");
                        self.builder.position_at_end(entry);
                    }
                }
                _ => todo!(),
            }
        }
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
                todo!()
            }
            _ => todo!()
        }
    }
}
