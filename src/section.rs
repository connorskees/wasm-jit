use std::collections::HashMap;

use crate::{
    DataSegment, ElementSegment, Export, FuncType, Function, Global, Import, MemoryType, TableType,
};

#[derive(Debug)]
pub enum Section<'a> {
    Type(TypeSection),
    Import(ImportSection<'a>),
    Function(FunctionSection),
    Table(TableSection),
    Memory(MemorySection),
    Global(GlobalSection),
    Export(ExportSection<'a>),
    Code(CodeSection),
    Data(DataSection<'a>),
    DataCount(u32),
    Element(ElementSection),
    Custom(CustomSection<'a>),
}

#[derive(Debug)]
pub struct TypeSection {
    pub(crate) fn_types: Vec<FuncType>,
}

#[derive(Debug)]
pub struct ImportSection<'a> {
    pub(crate) imports: Vec<Import<'a>>,
}

#[derive(Debug)]
pub struct FunctionSection {
    pub(crate) type_idxs: Vec<u32>,
}

#[derive(Debug)]
pub struct TableSection {
    pub(crate) table_types: Vec<TableType>,
}

#[derive(Debug)]
pub struct MemorySection {
    pub(crate) mem_types: Vec<MemoryType>,
}

#[derive(Debug)]
pub struct GlobalSection {
    pub(crate) globals: Vec<Global>,
}

#[derive(Debug)]
pub struct ExportSection<'a> {
    pub(crate) exports: Vec<Export<'a>>,
}

#[derive(Debug)]
pub struct CodeSection {
    pub(crate) functions: Vec<Function>,
}

#[derive(Debug)]
pub struct DataSection<'a> {
    pub(crate) data_segments: Vec<DataSegment<'a>>,
}

#[derive(Debug)]
pub struct ElementSection {
    pub(crate) element_segments: Vec<ElementSegment>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CustomSection<'a> {
    Name(NameSection<'a>),
    Unknown { name: &'a str, contents: &'a [u8] },
}

pub type NameMap<'a> = HashMap<u32, &'a str>;

#[derive(Debug, Clone, PartialEq)]
pub struct NameSection<'a> {
    pub subsections: Vec<NameSubsection<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NameSubsection<'a> {
    Module(&'a str),
    Func(NameMap<'a>),
    Local(HashMap<u32, &'a str>),
    Label(NameMap<'a>),
    Table(NameMap<'a>),
    Memory(NameMap<'a>),
    Global(NameMap<'a>),
    Elem(NameMap<'a>),
    Data(NameMap<'a>),
    Tag(NameMap<'a>),
}
