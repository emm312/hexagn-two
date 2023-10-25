use lalrpop_util::lalrpop_mod;
#[macro_use]
pub mod ast;
lalrpop_mod!(pub grammar);

pub mod gen;