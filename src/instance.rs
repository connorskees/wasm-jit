use std::borrow::Cow;

use crate::{
    Expr, ExternValue, FuncType, Function, GlobalType, MemoryType, Ref, RefType, TableType,
};

#[derive(Debug)]
pub enum FuncInst {
    Local {
        ty: FuncType,
        module: u32,
        code: Function,
    },
    Host {
        ty: FuncType,
        host_code: Vec<u8>,
    },
}

#[derive(Debug)]
pub struct TableInst {
    pub(crate) ty: TableType,
    pub(crate) elem: Vec<Ref>,
}

#[derive(Debug)]
pub struct MemInst {
    pub(crate) ty: MemoryType,
    pub(crate) data: Vec<u8>,
}

#[derive(Debug)]
pub struct GlobalInst {
    pub(crate) ty: GlobalType,
    pub(crate) value: Expr,
}

#[derive(Debug)]
pub struct ElemInst {
    pub(crate) ty: RefType,
    pub(crate) elem: Vec<Ref>,
}

#[derive(Debug)]
pub struct DataInst<'a> {
    pub(crate) data: Cow<'a, [u8]>,
}

#[derive(Debug, Clone)]
pub struct ExportInst<'a> {
    pub(crate) name: &'a str,
    pub(crate) value: ExternValue,
}

#[derive(Debug, Clone)]
pub struct ModuleInst<'a> {
    pub(crate) types: Vec<FuncType>,
    pub(crate) func_addrs: Vec<usize>,
    pub(crate) table_addrs: Vec<usize>,
    pub(crate) mem_addrs: Vec<usize>,
    pub(crate) global_addrs: Vec<usize>,
    pub(crate) elem_addrs: Vec<usize>,
    pub(crate) data_addrs: Vec<usize>,
    pub(crate) exports: Vec<ExportInst<'a>>,
}

impl<'a> ModuleInst<'a> {
    pub const fn init() -> Self {
        Self {
            types: Vec::new(),
            func_addrs: Vec::new(),
            table_addrs: Vec::new(),
            mem_addrs: Vec::new(),
            global_addrs: Vec::new(),
            elem_addrs: Vec::new(),
            data_addrs: Vec::new(),
            exports: Vec::new(),
        }
    }
}
