use crate::ir::{
    BasicBlock, BinOp, BlockId, Function, FunctionId, Instruction, Linkage, Module, Operation,
    Terminator, Type, Value, Variable, VariableId,
};

pub struct ModuleBuilder {
    module: Module,
    current_func: Option<FunctionId>,
    current_block: Option<BlockId>,
}

impl ModuleBuilder {
    pub fn new(name: &str) -> ModuleBuilder {
        ModuleBuilder {
            module: Module {
                functions: vec![],
                name: name.to_string(),
                analysed: false,
            },
            current_block: None,
            current_func: None,
        }
    }

    pub fn print_module(&self) {
        println!("{}", self.module);
    }

    pub fn push_function(
        &mut self,
        name: &str,
        ret_type: Type,
        args: Vec<(String, Type)>,
        linkage: Option<Linkage>,
    ) -> FunctionId {
        self.module.functions.push(Function::new(
            name,
            ret_type,
            args,
            linkage.unwrap_or(Linkage::Private),
            vec![],
            self.module.functions.len(),
        ));
        FunctionId(self.module.functions.len() - 1)
    }

    pub fn push_block(&mut self, name: &str) -> BlockId {
        self.module.functions[self.current_func.as_ref().unwrap().0]
            .blocks
            .push(BasicBlock {
                name: name.to_string(),
                instructions: vec![],
                terminator: Terminator::NoTerm,
            });
        BlockId(self.get_func(self.current_func.unwrap()).blocks.len() - 1)
    }

    pub fn switch_to_fn(&mut self, id: FunctionId) {
        self.current_func = Some(id);
    }

    pub fn switch_to_block(&mut self, id: BlockId) {
        self.current_block = Some(id);
    }

    pub fn build_binop(&mut self, op: BinOp, lhs: Value, rhs: Value) -> Value {
        let val = self.push_value();
        let block = self.get_block_mut(self.current_block.unwrap());
        block.instructions.push(Instruction {
            yielded: Some(val),
            operation: Operation::BinOp(op, lhs, rhs),
        });
        val
    }

    fn get_func(&self, id: FunctionId) -> &Function {
        &self.module.functions[id.0]
    }

    fn get_func_mut(&mut self, id: FunctionId) -> &mut Function {
        &mut self.module.functions[id.0]
    }

    fn get_block(&self, id: BlockId) -> &BasicBlock {
        &self.get_func(self.current_func.unwrap()).blocks[id.0]
    }

    fn get_block_mut(&mut self, id: BlockId) -> &mut BasicBlock {
        &mut self.get_func_mut(self.current_func.unwrap()).blocks[id.0]
    }

    pub fn push_variable(&mut self, name: &str, ty: Type) -> VariableId {
        let func = self.get_func_mut(self.current_func.unwrap());
        func.variables.push(Variable {
            name: name.to_string(),
            ty,
        });
        VariableId(func.variables.len() - 1)
    }

    pub fn build_integer(&mut self, value: i64) -> Value {
        let val = self.push_value();
        let block = self.get_block_mut(self.current_block.unwrap());
        block.instructions.push(Instruction {
            yielded: Some(val),
            operation: Operation::Integer(value),
        });
        val
    }

    // internal function to init values
    #[inline]
    fn push_value(&mut self) -> Value {
        self.get_func_mut(self.current_func.unwrap()).value_counter += 1;
        Value(self.get_func(self.current_func.unwrap()).value_counter - 1)
    }
}
