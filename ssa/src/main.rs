use ssa::ir::{Function, Module, Type};

fn main() {
    let module = Module::new("hello", vec![Function::new("h", Type::Void, vec![], ssa::ir::Linkage::Public, vec![])]);
    println!("{}", module);
}
