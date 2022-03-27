#![feature(adt_const_params)]
#![feature(generic_const_exprs)]
#![allow(incomplete_features)]
#![allow(dead_code)]

pub use error::{WResult, WasmError};
pub use interpreter::Interpreter;
pub use jit::WasmJit;
use opcode::Instruction;
use parse::ModuleParser;

mod error;
mod instance;
mod interpreter;
mod jit;
mod num;
mod opcode;
mod parse;
mod section;
mod stack;
mod store;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Module<'a> {
    pub(crate) types: Vec<FuncType>,
    pub(crate) funcs: Vec<Function>,
    pub(crate) tables: Vec<TableType>,
    pub(crate) mems: Vec<MemoryType>,
    pub(crate) globals: Vec<Global>,
    pub(crate) elems: Vec<Element>,
    pub(crate) data: Vec<DataSegment<'a>>,
    pub(crate) start: u32,
    pub(crate) imports: Vec<Import<'a>>,
    pub(crate) exports: Vec<Export<'a>>,
}

impl<'a> Module<'a> {
    pub fn new(buffer: &'a [u8]) -> WResult<Self> {
        ModuleParser::new(buffer).parse()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Element {
    ty: RefType,
    init: Vec<Expr>,
    mode: ElemMode,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ElemMode {
    Passive,
    Active { table_idx: u32, offset: Expr },
    Declarative,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType {
    I32,
    I64,
    F32,
    F64,
    FuncRef,
    ExternRef,
}

impl ValueType {
    pub fn zero(&self) -> Value {
        match self {
            Self::I32 => Value::I32(0),
            Self::I64 => Value::I64(0),
            Self::F32 => Value::F32(0.0),
            Self::F64 => Value::F64(0.0),
            Self::FuncRef | Self::ExternRef => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResultType(Vec<ValueType>);

#[derive(Debug, Clone, PartialEq)]
pub struct FuncType(ResultType, ResultType);

impl FuncType {
    pub fn number_of_args(&self) -> usize {
        (self.0).0.len()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import<'a> {
    mod_name: &'a str,
    name: &'a str,
    description: ImportDescription,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ImportDescription {
    Func(u32),
    Table(TableType),
    Mem(MemoryType),
    Global(GlobalType),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ExportDescription {
    FuncIdx(u32),
    TableIdx(u32),
    MemIdx(u32),
    GlobalIdx(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TableType {
    element: RefType,
    limit: Limit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GlobalType {
    val_type: ValueType,
    mutability: Mutability,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Limit {
    min: u32,
    max: Option<u32>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MemoryType {
    lim: Limit,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum RefType {
    FuncRef,
    ExternRef,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr(Vec<Instruction>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
    Const = 0x00,
    Var = 0x01,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Global {
    global_type: GlobalType,
    init: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Export<'a> {
    name: &'a str,
    description: ExportDescription,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    type_idx: u32,
    locals: Vec<ValueType>,
    expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataSegment<'a> {
    init: &'a [u8],
    mode: DataMode,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataMode {
    Passive,
    Active { mem_idx: u32, offset: Expr },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlockType {
    Empty,
    ValType(ValueType),
    TypeIndex(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MemoryOperand {
    align: u32,
    offset: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ref {
    Null,
    FuncAddr(u32),
    Extern(u32),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Ref(Ref),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl Value {
    pub fn assert_i32(self) -> WResult<i32> {
        match self {
            Self::I32(n) => Ok(n),
            _ => Err(WasmError::InvalidType),
        }
    }

    pub fn assert_i64(self) -> WResult<i64> {
        match self {
            Self::I64(n) => Ok(n),
            _ => Err(WasmError::InvalidType),
        }
    }

    pub fn assert_f32(self) -> WResult<f32> {
        match self {
            Self::F32(n) => Ok(n),
            _ => Err(WasmError::InvalidType),
        }
    }

    pub fn assert_f64(self) -> WResult<f64> {
        match self {
            Self::F64(n) => Ok(n),
            _ => Err(WasmError::InvalidType),
        }
    }

    pub fn width(&self) -> usize {
        match self {
            Self::I32(..) | Self::F32(..) => 32,
            Self::Ref(..) | Self::I64(..) | Self::F64(..) => 64,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExternValue {
    Func(usize),
    Table(usize),
    Mem(usize),
    Global(usize),
}

#[derive(Debug)]
pub enum ExternType {
    Func(FuncType),
    Table(TableType),
    Mem(MemoryType),
    Global(GlobalType),
}
