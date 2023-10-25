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
}

impl Function {
    pub fn new(name: &str, ret_type: Type, args: Vec<(String, Type)>, linkage: Linkage, variables: Vec<Variable>) -> Self {
        Self {
            name: name.to_string(),
            ret_type,
            args,
            blocks: vec![],
            linkage,
            variables: variables
        }
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
    pub(crate) terminator: Terminator
}

pub enum Terminator {
    Return(Value),
    Jump(BlockId),
    BranchCond(Value, BlockId, BlockId),
}

pub enum Linkage {
    Public,
    Private,
    External,
}

pub enum Instruction {}

pub struct BlockId(pub(crate) usize);
pub struct FunctionId(pub(crate) usize);
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
        write!(
            f,
            "fn {}({}) {} {{",
            self.name,
            self.args
                .iter()
                .map(|e| format!("{}: {}", e.0, e.1))
                .collect::<Vec<String>>()
                .join(", "),
            self.ret_type
        )?;

        writeln!(f, "}}")?;
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
        writeln!(f, "{}:", self.name)?;
        for instr in &self.instructions {
            writeln!(f, "  {}", instr)?;
        }

        Ok(())
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}