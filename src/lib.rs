use lalrpop_util::lalrpop_mod;
#[macro_use]
pub mod ast;
lalrpop_mod!(pub grammar);

pub mod func_mangling;
pub mod typechk;
pub mod typed_ast;

#[cfg(feature = "llvm")]
pub mod llvm;
