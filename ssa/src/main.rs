use ssa::{ir::{Function, Module, Type, Linkage}, builder::ModuleBuilder};

fn main() {
    let mut builder = ModuleBuilder::new("main");
    let main_fn = builder.push_function("main", Type::Void, vec![], None);
    builder.switch_to_fn(main_fn);
    let entry_block = builder.push_block("entry");
    builder.switch_to_block(entry_block);

    builder.print_module();
}
