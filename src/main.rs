use error::WResult;
use opcode::Instruction;
use parse::ModuleParser;

mod error;
mod opcode;
mod parse;
mod section;

fn main() -> WResult<()> {
    let buffer = std::fs::read("./test.wasm").unwrap();

    let mut parser = ModuleParser::new(&buffer);

    parser.parse()?;

    Ok(())
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

#[derive(Debug)]
pub struct ResultType(Vec<ValueType>);

#[derive(Debug)]
pub struct FuncType(ResultType, ResultType);

#[derive(Debug)]
pub struct Import<'a> {
    mod_name: &'a str,
    name: &'a str,
    description: ImportDescription,
}

#[derive(Debug)]
enum ImportDescription {
    Idx(u32),
    Table(TableType),
    Mem(MemoryType),
    Global(GlobalType),
}

#[derive(Debug)]
pub enum ExportDescription {
    FuncIdx(u32),
    TableIdx(u32),
    MemIdx(u32),
    GlobalIdx(u32),
}

#[derive(Debug)]
pub struct TableType {
    element: RefType,
    limit: Limit,
}

#[derive(Debug)]
struct GlobalType {
    val_type: ValueType,
    mutability: Mutability,
}

#[derive(Debug)]
pub struct Limit {
    min: u32,
    max: Option<u32>,
}

#[derive(Debug)]
struct MemoryType {
    lim: Limit,
}

#[derive(Debug, PartialEq, Eq)]
pub enum RefType {
    FuncRef,
    ExternRef,
}

#[derive(Debug)]
pub struct Expr(Vec<Instruction>);

#[derive(Debug)]
pub enum Mutability {
    Const = 0x00,
    Var = 0x01,
}

#[derive(Debug)]
pub struct Global {
    global_type: GlobalType,
    expr: Expr,
}

#[derive(Debug)]
pub struct Export<'a> {
    name: &'a str,
    description: ExportDescription,
}

#[derive(Debug)]
pub struct Function {
    locals: Vec<ValueType>,
    expr: Expr,
}

#[derive(Debug)]
pub struct DataSegment<'a> {
    init: &'a [u8],
    mode: DataMode,
}

#[derive(Debug)]
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
