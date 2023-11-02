use std::fmt::Display;

pub struct Module {
    pub(crate) functions: Vec<Function>,
    pub name: String,
    pub(crate) analysed: bool,
}

impl Module {
    pub fn new(name: &str, functions: Vec<Function>) -> Module {
        Module {
            functions: functions,
            name: name.to_string(),
            analysed: false,
        }
    }
}

pub struct Function {
    pub name: String,
    pub(crate) ret_type: Type,
    pub(crate) args: Vec<(String, Type)>,
    pub(crate) blocks: Vec<BasicBlock>,
    pub(crate) linkage: Linkage,
    pub(crate) variables: Vec<Variable>,
    pub(crate) id: usize,
    pub(crate) value_counter: usize,
}

impl Function {
    pub(crate) fn new(
        name: &str,
        ret_type: Type,
        args: Vec<(String, Type)>,
        linkage: Linkage,
        variables: Vec<Variable>,
        id: usize,
    ) -> Self {
        Self {
            name: name.to_string(),
            ret_type,
            args,
            blocks: vec![],
            linkage,
            variables: variables,
            value_counter: 0,
            id,
        }
    }

    pub fn push_block(&mut self, block: BasicBlock) {
        self.blocks.push(block);
    }
}

pub struct Variable {
    pub(crate) name: String,
    pub(crate) ty: Type,
}

pub enum Type {
    Void,
    Integer(usize, bool),
    Pointer(Box<Type>),
}

pub struct BasicBlock {
    pub(crate) name: String,
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) terminator: Terminator,
}

pub enum Terminator {
    Return(Variable),
    Jump(BlockId),
    BranchCond(Variable, BlockId, BlockId),
    Branch(Variable, BlockId, BlockId),
    NoTerm,
}

pub enum Linkage {
    Public,
    Private,
    External,
}

pub struct Instruction {
    pub(crate) yielded: Option<Value>,
    pub(crate) operation: Operation,
}

pub enum Operation {
    Integer(i64),
    BinOp(BinOp, Value, Value),
    Call(FunctionId, Vec<Value>),
    LoadVar(VariableId),
    StoreVar(Value, VariableId),
}

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => write!(f, "add"),
            BinOp::Sub => write!(f, "sub"),
            BinOp::Mul => write!(f, "mul"),
            BinOp::Div => write!(f, "div"),
            BinOp::Mod => write!(f, "mod"),
            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),
            BinOp::Xor => write!(f, "xor"),
            BinOp::Shl => write!(f, "shl"),
            BinOp::Shr => write!(f, "shr"),
            BinOp::Eq => write!(f, "eq"),
            BinOp::Ne => write!(f, "ne"),
            BinOp::Lt => write!(f, "lt"),
            BinOp::Le => write!(f, "le"),
            BinOp::Gt => write!(f, "gt"),
            BinOp::Ge => write!(f, "ge"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub(crate) usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub(crate) usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableId(pub(crate) usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(pub(crate) usize);

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "/* {} module {} */",
            match self.analysed {
                true => "analyzed",
                false => "unanalyzed",
            },
            self.name
        )?;

        for func in &self.functions {
            write!(f, "{}", func)?;
        }

        Ok(())
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "${}: fn {}({}) {} {{",
            self.id,
            self.name,
            self.args
                .iter()
                .map(|e| format!("{}: {}", e.0, e.1))
                .collect::<Vec<String>>()
                .join(", "),
            self.ret_type
        )?;

        for block in &self.blocks {
            write!(f, "{}", block)?;
        }

        write!(f, "}}")?;
        Ok(())
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Void => write!(f, "void")?,
            Type::Integer(size, signed) => {
                write!(f, "{}i{}", size, if *signed { "s" } else { "u" })?
            }
            Type::Pointer(ty) => write!(f, "{}*", ty)?,
        }
        Ok(())
    }
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "${}:", self.name)?;
        for instr in &self.instructions {
            writeln!(f, "    {}", instr)?;
        }
        writeln!(f, "    {}", self.terminator)?;
        Ok(())
    }
}

impl Display for Terminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Terminator::Return(var) => write!(f, "ret %{}", var.name)?,
            Terminator::Jump(block) => write!(f, "jmp ${}", block.0)?,
            Terminator::BranchCond(var, block1, block2) => {
                write!(f, "br %{}, ${}, ${}", var.name, block1.0, block2.0)?
            }
            Terminator::Branch(var, block1, block2) => {
                write!(f, "br %{}, ${}, ${}", var.name, block1.0, block2.0)?
            }
            Terminator::NoTerm => write!(f, "noterm")?,
        }
        Ok(())
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(val) = self.yielded {
            write!(f, "{} = ", val)?;
        }
        write!(f, "{}", self.operation)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::BinOp(op, lhs, rhs) => write!(f, "{} {} {}", op, lhs, rhs)?,
            Operation::Call(func, args) => write!(
                f,
                "call ${}({})",
                func.0,
                args.iter()
                    .map(|e| format!("{}", e))
                    .collect::<Vec<String>>()
                    .join(", ")
            )?,
            Operation::LoadVar(var) => write!(f, "load %{}", var.0)?,
            Operation::StoreVar(val, var) => write!(f, "store {} %{}", val, var.0)?,
            Operation::Integer(val) => write!(f, "{}", val)?,
        }
        Ok(())
    }
}
