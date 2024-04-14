use std::collections::VecDeque;

use crate::{
    error::{WResult, WasmError},
    opcode::{Instruction, OpCode},
    section::{
        CodeSection, CustomSection, DataSection, ElementSection, ExportSection, FunctionSection,
        GlobalSection, ImportSection, MemorySection, NameSection, Section, TableSection,
        TypeSection,
    },
    stack::Label,
    BlockType, DataMode, DataSegment, ElementMode, ElementSegment, Export, ExportDescription, Expr,
    FuncType, Function, Global, GlobalType, Import, ImportDescription, Limit, MemoryOperand,
    MemoryType, Module, Mutability, RefType, ResultType, TableType, ValueType,
};

const MAGIC: &[u8] = b"\0asm";
const VERSION: &[u8] = &[1, 0, 0, 0];

pub(crate) struct ModuleParser<'a> {
    cursor: usize,
    buffer: &'a [u8],
    func_types: VecDeque<u32>,
}

impl<'a> ModuleParser<'a> {
    pub fn new(buffer: &'a [u8]) -> Self {
        Self {
            buffer,
            cursor: 0,
            func_types: VecDeque::new(),
        }
    }

    pub fn parse(mut self) -> WResult<Module<'a>> {
        self.expect_magic()?;
        self.expect_version()?;

        let mut module = Module::default();

        while self.buffer.len() > self.cursor {
            match self.parse_section()? {
                Section::Custom(custom) => module.custom.push(custom),
                Section::Export(mut export) => module.exports.append(&mut export.exports),
                Section::Import(mut import) => module.imports.append(&mut import.imports),
                Section::Type(mut ty) => module.types.append(&mut ty.fn_types),
                Section::Function(function) => self.func_types.extend(function.type_idxs),
                Section::Table(mut table) => module.tables.append(&mut table.table_types),
                Section::Memory(mut memory) => module.mems.append(&mut memory.mem_types),
                Section::Global(mut global) => module.globals.append(&mut global.globals),
                Section::Code(mut code) => module.funcs.append(&mut code.functions),
                Section::Data(mut data) => module.data.append(&mut data.data_segments),
                Section::DataCount(..) => {}
                Section::Element(mut element) => module.elems.append(&mut element.element_segments),
            }
        }

        Ok(module)
    }

    fn expect_magic(&mut self) -> WResult<()> {
        let magic = self.read_4_bytes()?;

        if MAGIC == magic {
            Ok(())
        } else {
            anyhow::bail!(WasmError::InvalidMagicHeader)
        }
    }

    fn expect_version(&mut self) -> WResult<()> {
        let version = self.read_4_bytes()?;

        if VERSION == version {
            Ok(())
        } else {
            anyhow::bail!(WasmError::InvalidVersion)
        }
    }

    fn expect_byte(&mut self, expected: u8) -> WResult<()> {
        let found = self.next_byte()?;

        if found == expected {
            Ok(())
        } else {
            anyhow::bail!(WasmError::ExpectedByte { found, expected })
        }
    }

    fn next_byte(&mut self) -> WResult<u8> {
        self.buffer
            .get(self.cursor)
            .copied()
            .map(|b| {
                self.cursor += 1;

                b
            })
            .ok_or(anyhow::anyhow!(WasmError::UnexpectedEof {
                pos: self.cursor
            }))
    }

    fn read_u32(&mut self) -> WResult<u32> {
        let mut result: u32 = 0;
        let mut shift = 0;

        loop {
            let byte = self.next_byte()?;
            let low = u32::from(byte & 0b0111_1111);
            let high = byte & 0b1000_0000;

            result |= low << shift;

            if high == 0 {
                break;
            }

            shift += 7;
        }

        Ok(result)
    }

    fn read_i32(&mut self) -> WResult<i32> {
        let mut result = 0;
        let mut shift = 0;

        let size = i32::BITS;

        let mut byte;

        loop {
            byte = self.next_byte()?;
            let low = i32::from(byte & 0b0111_1111);
            let high = byte & 0b1000_0000;

            result |= low << shift;

            shift += 7;

            if high == 0 {
                break;
            }
        }

        if shift < size && (byte & 0x40) != 0 {
            result |= !0 << shift;
        }

        Ok(result)
    }

    fn read_i64(&mut self) -> WResult<i64> {
        let mut result = 0;
        let mut shift = 0;

        let size = i64::BITS;

        let mut byte;

        loop {
            byte = self.next_byte()?;
            let low = i64::from(byte & 0b0111_1111);
            let high = byte & 0b1000_0000;

            result |= low << shift;

            shift += 7;

            if high == 0 {
                break;
            }
        }

        if shift < size && (byte & 0x40) != 0 {
            result |= !0 << shift;
        }

        Ok(result)
    }

    fn read_f32(&mut self) -> WResult<f32> {
        Ok(f32::from_le_bytes(self.read_4_bytes()?))
    }

    fn read_f64(&mut self) -> WResult<f64> {
        Ok(f64::from_le_bytes(self.read_8_bytes()?))
    }

    fn read_4_bytes(&mut self) -> WResult<[u8; 4]> {
        let b1 = self.next_byte()?;
        let b2 = self.next_byte()?;
        let b3 = self.next_byte()?;
        let b4 = self.next_byte()?;

        Ok([b1, b2, b3, b4])
    }

    fn read_8_bytes(&mut self) -> WResult<[u8; 8]> {
        let b1 = self.next_byte()?;
        let b2 = self.next_byte()?;
        let b3 = self.next_byte()?;
        let b4 = self.next_byte()?;
        let b5 = self.next_byte()?;
        let b6 = self.next_byte()?;
        let b7 = self.next_byte()?;
        let b8 = self.next_byte()?;

        Ok([b1, b2, b3, b4, b5, b6, b7, b8])
    }

    fn read_range(&mut self, len: usize) -> WResult<&'a [u8]> {
        let start = self.cursor;

        self.cursor += len;

        self.buffer
            .get(start..self.cursor)
            .ok_or(anyhow::anyhow!(WasmError::UnexpectedEof {
                pos: self.cursor
            }))
    }

    fn parse_section(&mut self) -> WResult<Section<'a>> {
        let id = self.next_byte()?;
        let size = self.read_u32()?;

        Ok(match id {
            0 => Section::Custom(self.parse_custom_section(size)?),
            1 => Section::Type(self.parse_type_section()?),
            2 => Section::Import(self.parse_import_section()?),
            3 => Section::Function(self.parse_function_section()?),
            4 => Section::Table(self.parse_table_section()?),
            5 => Section::Memory(self.parse_memory_section()?),
            6 => Section::Global(self.parse_global_section()?),
            7 => Section::Export(self.parse_export_section()?),
            8 => todo!("start"),
            9 => Section::Element(self.parse_element_section()?),
            10 => Section::Code(self.parse_code_section()?),
            11 => Section::Data(self.parse_data_section()?),
            12 => Section::DataCount(self.parse_data_count_section()?),
            id => {
                anyhow::bail!(WasmError::UnrecognizedSection {
                    id,
                    pos: self.cursor,
                })
            }
        })
    }

    fn parse_data_count_section(&mut self) -> WResult<u32> {
        self.read_u32()
    }

    fn parse_custom_section(&mut self, size: u32) -> WResult<CustomSection<'a>> {
        let start = self.cursor;
        let name = self.parse_name()?;
        let num_range_bytes = self.cursor - start;

        let section = match name {
            "name" => CustomSection::Name(self.parse_name_section()?),
            _ => {
                let contents = self.read_range(size as usize - num_range_bytes)?;
                CustomSection::Unknown { name, contents }
            }
        };

        Ok(section)
    }

    fn parse_name_section(&mut self) -> WResult<NameSection> {
        let subsection_id = self.next_byte()?;

        match subsection_id {
            0 => todo!("module name"),
            1 => todo!("function name"),
            2 => todo!("local name"),
            _ => todo!("invalid name section subsection: {subsection_id}"),
        }
    }

    fn parse_element_section(&mut self) -> WResult<ElementSection> {
        let element_segments = self.parse_vec(Self::parse_element_segment)?;

        Ok(ElementSection { element_segments })
    }

    fn parse_element_segment(&mut self) -> WResult<ElementSegment> {
        let kind = self.read_u32()?;

        Ok(match kind {
            0 => {
                let e = self.parse_expr()?;
                let y = self.parse_vec(Self::read_u32)?;
                drop(y);

                ElementSegment {
                    ty: RefType::FuncRef,
                    // todo: what is init?
                    init: Vec::new(),
                    mode: ElementMode::Active {
                        table_idx: 0,
                        offset: e,
                    },
                }
            }
            1 => todo!(),
            2 => todo!(),
            3 => todo!(),
            4 => todo!(),
            5 => todo!(),
            6 => todo!(),
            7 => todo!(),
            n => anyhow::bail!("invalid element segment id: {n}"),
        })
    }

    fn parse_vec<T>(&mut self, parse: impl Fn(&mut Self) -> WResult<T>) -> WResult<Vec<T>> {
        let len = self.read_u32()?;

        let mut elems = Vec::with_capacity(len as usize);

        for _ in 0..len {
            elems.push(parse(self)?);
        }

        Ok(elems)
    }

    fn parse_type_section(&mut self) -> WResult<TypeSection> {
        let fn_types = self.parse_vec(Self::parse_func_type)?;

        Ok(TypeSection { fn_types })
    }

    fn parse_import_section(&mut self) -> WResult<ImportSection<'a>> {
        let imports = self.parse_vec(Self::parse_import)?;
        Ok(ImportSection { imports })
    }

    fn parse_function_section(&mut self) -> WResult<FunctionSection> {
        let type_idxs = self.parse_vec(Self::read_u32)?;
        Ok(FunctionSection { type_idxs })
    }

    fn parse_table_section(&mut self) -> WResult<TableSection> {
        let table_types = self.parse_vec(Self::parse_table_type)?;
        Ok(TableSection { table_types })
    }

    fn parse_memory_section(&mut self) -> WResult<MemorySection> {
        let mem_types = self.parse_vec(Self::parse_mem_type)?;
        Ok(MemorySection { mem_types })
    }

    fn parse_global_section(&mut self) -> WResult<GlobalSection> {
        let globals = self.parse_vec(Self::parse_global)?;
        Ok(GlobalSection { globals })
    }

    fn parse_export_section(&mut self) -> WResult<ExportSection<'a>> {
        let exports = self.parse_vec(Self::parse_export)?;
        Ok(ExportSection { exports })
    }

    fn parse_code_section(&mut self) -> WResult<CodeSection> {
        let functions = self.parse_vec(Self::parse_function)?;
        Ok(CodeSection { functions })
    }

    fn parse_data_section(&mut self) -> WResult<DataSection<'a>> {
        let data_segments = self.parse_vec(Self::parse_data_segment)?;
        Ok(DataSection { data_segments })
    }

    fn parse_data_segment(&mut self) -> WResult<DataSegment<'a>> {
        let mode = match self.next_byte()? {
            0x00 => DataMode::Active {
                mem_idx: 0,
                offset: self.parse_expr()?,
            },
            0x01 => DataMode::Passive,
            0x02 => DataMode::Active {
                mem_idx: self.read_u32()?,
                offset: self.parse_expr()?,
            },
            n => anyhow::bail!(WasmError::InvalidDataMode { n }),
        };

        let init_len = self.read_u32()?;

        let init = self.read_range(init_len as usize)?;

        Ok(DataSegment { init, mode })
    }

    fn parse_function(&mut self) -> WResult<Function> {
        let _size = self.read_u32()?;

        let type_idx = self.func_types.pop_front().unwrap();

        let num_of_locals = self.read_u32()?;

        let mut locals = Vec::with_capacity(num_of_locals as usize);

        for _ in 0..num_of_locals {
            let n = self.read_u32()?;
            let local_ty = self.parse_value_type()?;

            for _ in 0..n {
                locals.push(local_ty);
            }
        }

        let expr = self.parse_expr()?;

        Ok(Function {
            type_idx,
            locals,
            expr,
        })
    }

    fn parse_export(&mut self) -> WResult<Export<'a>> {
        let name = self.parse_name()?;
        let description = self.parse_export_description()?;

        Ok(Export { name, description })
    }

    fn parse_export_description(&mut self) -> WResult<ExportDescription> {
        let kind = self.next_byte()?;

        let value = self.read_u32()?;

        Ok(match kind {
            0x00 => ExportDescription::FuncIdx(value),
            0x01 => ExportDescription::TableIdx(value),
            0x02 => ExportDescription::MemIdx(value),
            0x03 => ExportDescription::GlobalIdx(value),
            _ => anyhow::bail!(WasmError::InvalidExportDescription { n: kind }),
        })
    }

    fn parse_global(&mut self) -> WResult<Global> {
        let global_type = self.parse_global_type()?;
        let init = self.parse_expr()?;

        Ok(Global { global_type, init })
    }

    fn parse_expr(&mut self) -> WResult<Expr> {
        let mut instructions: Vec<Instruction> = Vec::new();

        let mut nested = Vec::new();

        loop {
            let op_code = self.next_op_code()?;

            let inst = match op_code {
                OpCode::Block => {
                    let block_type = self.parse_block_type()?;
                    assert_eq!(block_type, BlockType::Empty);

                    let start = instructions.len();

                    let label = Label::incomplete_block(block_type);

                    nested.push(start);

                    Instruction::Block(label)
                }
                OpCode::Loop => {
                    let block_type = self.parse_block_type()?;
                    assert_eq!(block_type, BlockType::Empty);
                    let start = instructions.len();

                    let label = Label::new_loop(start, block_type);

                    nested.push(start);

                    Instruction::Loop(label)
                }
                OpCode::If => {
                    let block_type = self.parse_block_type()?;
                    assert_eq!(block_type, BlockType::Empty);

                    let start = instructions.len();

                    let label = Label::incomplete_block(block_type);

                    nested.push(start);

                    Instruction::If(label)
                }
                OpCode::End => {
                    let end = instructions.len();

                    instructions.push(Instruction::End);

                    if let Some(block_idx) = nested.pop() {
                        instructions[block_idx].set_label_end(end);
                        continue;
                    } else {
                        break;
                    }
                }
                _ => self.parse_immediates(op_code)?,
            };

            instructions.push(inst);
        }

        Ok(Expr(instructions))
    }

    fn parse_import_description(&mut self) -> WResult<ImportDescription> {
        Ok(match self.next_byte()? {
            0x00 => ImportDescription::Func(self.read_u32()?),
            0x01 => ImportDescription::Table(self.parse_table_type()?),
            0x02 => ImportDescription::Mem(self.parse_mem_type()?),
            0x03 => ImportDescription::Global(self.parse_global_type()?),
            n => anyhow::bail!(WasmError::InvalidImportDescription { n }),
        })
    }

    fn parse_import(&mut self) -> WResult<Import<'a>> {
        let mod_name = self.parse_name()?;
        let name = self.parse_name()?;

        let description = self.parse_import_description()?;

        Ok(Import {
            mod_name,
            name,
            description,
        })
    }

    fn parse_table_type(&mut self) -> WResult<TableType> {
        let element = self.parse_ref_type()?;
        let limit = self.parse_limit()?;

        Ok(TableType { element, limit })
    }

    fn parse_mem_type(&mut self) -> WResult<MemoryType> {
        Ok(MemoryType {
            lim: self.parse_limit()?,
        })
    }

    fn parse_global_type(&mut self) -> WResult<GlobalType> {
        let val_type = self.parse_value_type()?;

        let mutability = match self.next_byte()? {
            0x00 => Mutability::Const,
            0x01 => Mutability::Var,
            n => anyhow::bail!(WasmError::InvalidGlobalMutability { n }),
        };

        Ok(GlobalType {
            val_type,
            mutability,
        })
    }

    fn parse_limit(&mut self) -> WResult<Limit> {
        let limit_kind = self.next_byte()?;

        let min = self.read_u32()?;

        let max = match limit_kind {
            0x00 => None,
            0x01 => Some(self.read_u32()?),
            n => anyhow::bail!(WasmError::InvalidLimitFlag { n }),
        };

        Ok(Limit { min, max })
    }

    fn parse_name(&mut self) -> WResult<&'a str> {
        let len = self.read_u32()?;

        let buffer = self.read_range(len as usize)?;

        Ok(std::str::from_utf8(buffer)?)
    }

    fn parse_func_type(&mut self) -> WResult<FuncType> {
        self.expect_byte(0x60)?;

        let arg_type = self.parse_result_type()?;
        let return_type = self.parse_result_type()?;

        Ok(FuncType(arg_type, return_type))
    }

    fn parse_value_type(&mut self) -> WResult<ValueType> {
        Ok(match self.next_byte()? {
            0x7f => ValueType::I32,
            0x7e => ValueType::I64,
            0x7d => ValueType::F32,
            0x7c => ValueType::F64,
            0x70 => ValueType::FuncRef,
            0x6f => ValueType::ExternRef,
            n => anyhow::bail!("invalid value type: {n}"),
        })
    }

    fn parse_ref_type(&mut self) -> WResult<RefType> {
        Ok(match self.next_byte()? {
            0x70 => RefType::FuncRef,
            0x6f => RefType::ExternRef,
            n => anyhow::bail!(WasmError::InvalidRefType { n }),
        })
    }

    fn parse_result_type(&mut self) -> WResult<ResultType> {
        let types = self.parse_vec(Self::parse_value_type)?;

        Ok(ResultType(types))
    }

    fn next_op_code(&mut self) -> WResult<OpCode> {
        Ok(match self.next_byte()? {
            0x00 => OpCode::Unreachable,
            0x01 => OpCode::Nop,
            0x02 => OpCode::Block,
            0x03 => OpCode::Loop,
            0x04 => OpCode::If,
            0x05 => OpCode::Else,
            0x0b => OpCode::End,
            0x0c => OpCode::Branch,
            0x0d => OpCode::BranchIf,
            0x0e => OpCode::BranchTable,
            0x0f => OpCode::Return,
            0x10 => OpCode::Call,
            0x11 => OpCode::CallIndirect,
            0x1a => OpCode::Drop,
            0x1b => OpCode::Select,
            0x1c => OpCode::SelectT,
            0x20 => OpCode::LocalGet,
            0x21 => OpCode::LocalSet,
            0x22 => OpCode::LocalTee,
            0x23 => OpCode::GlobalGet,
            0x24 => OpCode::GlobalSet,
            0x25 => OpCode::TableGet,
            0x26 => OpCode::TableSet,
            0x28 => OpCode::i32Load,
            0x29 => OpCode::i64Load,
            0x2a => OpCode::f32Load,
            0x2b => OpCode::f64Load,
            0x2c => OpCode::i32Load8Signed,
            0x2d => OpCode::i32Load8Unsigned,
            0x2e => OpCode::i32Load16Signed,
            0x2f => OpCode::i32Load16Unsigned,
            0x30 => OpCode::i64Load8Signed,
            0x31 => OpCode::i64Load8Unsigned,
            0x32 => OpCode::i64Load16Signed,
            0x33 => OpCode::i64Load16Unsigned,
            0x34 => OpCode::i64Load32Signed,
            0x35 => OpCode::i64Load32Unsigned,
            0x36 => OpCode::i32Store,
            0x37 => OpCode::i64Store,
            0x38 => OpCode::f32Store,
            0x39 => OpCode::f64Store,
            0x3a => OpCode::i32Store8,
            0x3b => OpCode::i32Store16,
            0x3c => OpCode::i64Store8,
            0x3d => OpCode::i64Store16,
            0x3e => OpCode::i64Store32,
            0x3f => OpCode::MemorySize,
            0x40 => OpCode::MemoryGrow,
            0x41 => OpCode::i32Const,
            0x42 => OpCode::i64Const,
            0x43 => OpCode::f32Const,
            0x44 => OpCode::f64Const,
            0x45 => OpCode::i32EqZero,
            0x46 => OpCode::i32Eq,
            0x47 => OpCode::i32Ne,
            0x48 => OpCode::i32LtSigned,
            0x49 => OpCode::i32LtUnsigned,
            0x4a => OpCode::i32GtSigned,
            0x4b => OpCode::i32GtUnsigned,
            0x4c => OpCode::i32LeSigned,
            0x4d => OpCode::i32LeUnsigned,
            0x4e => OpCode::i32GeSigned,
            0x4f => OpCode::i32GeUnsigned,
            0x50 => OpCode::i64EqZero,
            0x51 => OpCode::i64Eq,
            0x52 => OpCode::i64Ne,
            0x53 => OpCode::i64LtSigned,
            0x54 => OpCode::i64LtUnsigned,
            0x55 => OpCode::i64GtSigned,
            0x56 => OpCode::i64GtUnsigned,
            0x57 => OpCode::i64LeSigned,
            0x58 => OpCode::i64LeUnsigned,
            0x59 => OpCode::i64GeSigned,
            0x5a => OpCode::i64GeUnsigned,
            0x5b => OpCode::f32Eq,
            0x5c => OpCode::f32Ne,
            0x5d => OpCode::f32Lt,
            0x5e => OpCode::f32Gt,
            0x5f => OpCode::f32Le,
            0x60 => OpCode::f32Ge,
            0x61 => OpCode::f64Eq,
            0x62 => OpCode::f64Ne,
            0x63 => OpCode::f64Lt,
            0x64 => OpCode::f64Gt,
            0x65 => OpCode::f64Le,
            0x66 => OpCode::f64Ge,
            0x67 => OpCode::i32CountLeadingZeros,
            0x68 => OpCode::i32CountTrailingZeros,
            0x69 => OpCode::i32PopulationCount,
            0x6a => OpCode::i32Add,
            0x6b => OpCode::i32Sub,
            0x6c => OpCode::i32Mul,
            0x6d => OpCode::i32SignedDiv,
            0x6e => OpCode::i32UnsignedDiv,
            0x6f => OpCode::i32SignedRem,
            0x70 => OpCode::i32UnsignedRem,
            0x71 => OpCode::i32BitwiseAnd,
            0x72 => OpCode::i32BitwiseOr,
            0x73 => OpCode::i32BitwiseXor,
            0x74 => OpCode::i32BitwiseShiftLeft,
            0x75 => OpCode::i32SignedBitwiseShiftRight,
            0x76 => OpCode::i32UnsignedBitwiseShiftRight,
            0x77 => OpCode::i32RotateLeft,
            0x78 => OpCode::i32RotateRight,
            0x79 => OpCode::i64CountLeadingZeros,
            0x7a => OpCode::i64CountTrailingZeros,
            0x7b => OpCode::i64PopulationCount,
            0x7c => OpCode::i64Add,
            0x7d => OpCode::i64Sub,
            0x7e => OpCode::i64Mul,
            0x7f => OpCode::i64SignedDiv,
            0x80 => OpCode::i64UnsignedDiv,
            0x81 => OpCode::i64SignedRem,
            0x82 => OpCode::i64UnsignedRem,
            0x83 => OpCode::i64BitwiseAnd,
            0x84 => OpCode::i64BitwiseOr,
            0x85 => OpCode::i64BitwiseXor,
            0x86 => OpCode::i64BitwiseShiftLeft,
            0x87 => OpCode::i64SignedBitwiseShiftRight,
            0x88 => OpCode::i64UnsignedBitwiseShiftRight,
            0x89 => OpCode::i64RotateLeft,
            0x8a => OpCode::i64RotateRight,
            0x8b => OpCode::f32Abs,
            0x8c => OpCode::f32Neg,
            0x8d => OpCode::f32Ceil,
            0x8e => OpCode::f32Floor,
            0x8f => OpCode::f32Trunc,
            0x90 => OpCode::f32Nearest,
            0x91 => OpCode::f32Sqrt,
            0x92 => OpCode::f32Add,
            0x93 => OpCode::f32Sub,
            0x94 => OpCode::f32Mul,
            0x95 => OpCode::f32Div,
            0x96 => OpCode::f32Min,
            0x97 => OpCode::f32Max,
            0x98 => OpCode::f32CopySign,
            0x99 => OpCode::f64Abs,
            0x9a => OpCode::f64Neg,
            0x9b => OpCode::f64Ceil,
            0x9c => OpCode::f64Floor,
            0x9d => OpCode::f64Trunc,
            0x9e => OpCode::f64Nearest,
            0x9f => OpCode::f64Sqrt,
            0xa0 => OpCode::f64Add,
            0xa1 => OpCode::f64Sub,
            0xa2 => OpCode::f64Mul,
            0xa3 => OpCode::f64Div,
            0xa4 => OpCode::f64Min,
            0xa5 => OpCode::f64Max,
            0xa6 => OpCode::f64CopySign,
            0xa7 => OpCode::i32WrapI64,
            0xa8 => OpCode::i32TruncF32Signed,
            0xa9 => OpCode::i32TruncF32Unsigned,
            0xaa => OpCode::i32TruncF64Signed,
            0xab => OpCode::i32TruncF64Unsigned,
            0xac => OpCode::i64Extendi32Signed,
            0xad => OpCode::i64Extendi32Unsigned,
            0xae => OpCode::i64TruncF32Signed,
            0xaf => OpCode::i64TruncF32Unsigned,
            0xb0 => OpCode::i64TruncF64Signed,
            0xb1 => OpCode::i64TruncF64Unsigned,
            0xb2 => OpCode::f32SignedConvertI32,
            0xb3 => OpCode::f32UnsignedConvertI32,
            0xb4 => OpCode::f32SignedConvertI64,
            0xb5 => OpCode::f32UnsignedConvertI64,
            0xb6 => OpCode::f32DemoteF64,
            0xb7 => OpCode::f64SignedConvertI32,
            0xb8 => OpCode::f64UnsignedConvertI32,
            0xb9 => OpCode::f64SignedConvertI64,
            0xba => OpCode::f64UnsignedConvertI64,
            0xbb => OpCode::f64PromoteF32,
            0xbc => OpCode::i32ReinterpretF32,
            0xbd => OpCode::i64ReinterpretF64,
            0xbe => OpCode::f32ReinterpretI32,
            0xbf => OpCode::f64ReinterpretI64,
            0xc0 => OpCode::i32Extend8Signed,
            0xc1 => OpCode::i32Extend16Signed,
            0xc2 => OpCode::i64Extend8Signed,
            0xc3 => OpCode::i64Extend16Signed,
            0xc4 => OpCode::i64Extend32Signed,
            0xd0 => OpCode::RefNull,
            0xd1 => OpCode::RefIsNull,
            0xd2 => OpCode::RefFunc,
            0xfc => match self.read_u32()? {
                0x00 => OpCode::i32TruncSatf32Signed,
                0x01 => OpCode::i32TruncSatf32Unsigned,
                0x02 => OpCode::i32TruncSatf64Signed,
                0x03 => OpCode::i32TruncSatf64Unsigned,
                0x04 => OpCode::i64TruncSatf32Signed,
                0x05 => OpCode::i64TruncSatf32Unsigned,
                0x06 => OpCode::i64TruncSatf64Signed,
                0x07 => OpCode::i64TruncSatf64Unsigned,
                0x08 => OpCode::MemoryInit,
                0x09 => OpCode::DataDrop,
                0x0a => OpCode::MemoryCopy,
                0x0b => OpCode::MemoryFill,
                0x0c => OpCode::TableInit,
                0x0d => OpCode::ElemDrop,
                0x0e => OpCode::TableCopy,
                0x0f => OpCode::TableGrow,
                0x10 => OpCode::TableSize,
                0x11 => OpCode::TableFill,
                op => {
                    anyhow::bail!(WasmError::InvalidOpCode {
                        op: op as u16 + 0xfc_00,
                    })
                }
            },
            op => anyhow::bail!(WasmError::InvalidOpCode { op: op as u16 }),
        })
    }

    fn parse_branch_table_immediate(&mut self) -> WResult<(Vec<u32>, u32)> {
        let l_star = self.parse_vec(Self::read_u32)?;
        let l_n = self.read_u32()?;

        Ok((l_star, l_n))
    }

    fn parse_select_immediate(&mut self) -> WResult<Vec<ValueType>> {
        self.parse_vec(Self::parse_value_type)
    }

    fn parse_memory_operand(&mut self) -> WResult<MemoryOperand> {
        let align = self.read_u32()?;
        let offset = self.read_u32()?;

        Ok(MemoryOperand { align, offset })
    }

    fn parse_immediates(&mut self, op_code: OpCode) -> WResult<Instruction> {
        Ok(match op_code {
            OpCode::Unreachable => Instruction::Unreachable,
            OpCode::Nop => Instruction::Nop,
            OpCode::Block | OpCode::Loop | OpCode::If => unreachable!(),
            OpCode::Else => Instruction::Else,
            OpCode::End => Instruction::End,
            OpCode::Branch => Instruction::Branch(self.read_u32()?),
            OpCode::BranchIf => Instruction::BranchIf(self.read_u32()?),
            OpCode::BranchTable => {
                let (a, b) = self.parse_branch_table_immediate()?;
                Instruction::BranchTable(a, b)
            }
            OpCode::Return => Instruction::Return,
            OpCode::Call => Instruction::Call(self.read_u32()?),
            OpCode::CallIndirect => Instruction::CallIndirect(self.read_u32()?, self.read_u32()?),
            OpCode::Drop => Instruction::Drop,
            OpCode::Select => Instruction::Select,
            OpCode::SelectT => Instruction::SelectT(self.parse_select_immediate()?),
            OpCode::LocalGet => Instruction::LocalGet(self.read_u32()?),
            OpCode::LocalSet => Instruction::LocalSet(self.read_u32()?),
            OpCode::LocalTee => Instruction::LocalTee(self.read_u32()?),
            OpCode::GlobalGet => Instruction::GlobalGet(self.read_u32()?),
            OpCode::GlobalSet => Instruction::GlobalSet(self.read_u32()?),
            OpCode::TableGet => Instruction::TableGet(self.read_u32()?),
            OpCode::TableSet => Instruction::TableSet(self.read_u32()?),
            OpCode::i32Load => Instruction::i32Load(self.parse_memory_operand()?),
            OpCode::i64Load => Instruction::i64Load(self.parse_memory_operand()?),
            OpCode::f32Load => Instruction::f32Load(self.parse_memory_operand()?),
            OpCode::f64Load => Instruction::f64Load(self.parse_memory_operand()?),
            OpCode::i32Load8Signed => Instruction::i32Load8Signed(self.parse_memory_operand()?),
            OpCode::i32Load8Unsigned => Instruction::i32Load8Unsigned(self.parse_memory_operand()?),
            OpCode::i32Load16Signed => Instruction::i32Load16Signed(self.parse_memory_operand()?),
            OpCode::i32Load16Unsigned => {
                Instruction::i32Load16Unsigned(self.parse_memory_operand()?)
            }
            OpCode::i64Load8Signed => Instruction::i64Load8Signed(self.parse_memory_operand()?),
            OpCode::i64Load8Unsigned => Instruction::i64Load8Unsigned(self.parse_memory_operand()?),
            OpCode::i64Load16Signed => Instruction::i64Load16Signed(self.parse_memory_operand()?),
            OpCode::i64Load16Unsigned => {
                Instruction::i64Load16Unsigned(self.parse_memory_operand()?)
            }
            OpCode::i64Load32Signed => Instruction::i64Load32Signed(self.parse_memory_operand()?),
            OpCode::i64Load32Unsigned => {
                Instruction::i64Load32Unsigned(self.parse_memory_operand()?)
            }
            OpCode::i32Store => Instruction::i32Store(self.parse_memory_operand()?),
            OpCode::i64Store => Instruction::i64Store(self.parse_memory_operand()?),
            OpCode::f32Store => Instruction::f32Store(self.parse_memory_operand()?),
            OpCode::f64Store => Instruction::f64Store(self.parse_memory_operand()?),
            OpCode::i32Store8 => Instruction::i32Store8(self.parse_memory_operand()?),
            OpCode::i32Store16 => Instruction::i32Store16(self.parse_memory_operand()?),
            OpCode::i64Store8 => Instruction::i64Store8(self.parse_memory_operand()?),
            OpCode::i64Store16 => Instruction::i64Store16(self.parse_memory_operand()?),
            OpCode::i64Store32 => Instruction::i64Store32(self.parse_memory_operand()?),
            OpCode::MemorySize => Instruction::MemorySize,
            OpCode::MemoryGrow => Instruction::MemoryGrow,
            OpCode::i32Const => Instruction::i32Const(self.read_i32()?),
            OpCode::i64Const => Instruction::i64Const(self.read_i64()?),
            OpCode::f32Const => Instruction::f32Const(self.read_f32()?),
            OpCode::f64Const => Instruction::f64Const(self.read_f64()?),
            OpCode::i32EqZero => Instruction::i32EqZero,
            OpCode::i32Eq => Instruction::i32Eq,
            OpCode::i32Ne => Instruction::i32Ne,
            OpCode::i32LtSigned => Instruction::i32LtSigned,
            OpCode::i32LtUnsigned => Instruction::i32LtUnsigned,
            OpCode::i32GtSigned => Instruction::i32GtSigned,
            OpCode::i32GtUnsigned => Instruction::i32GtUnsigned,
            OpCode::i32LeSigned => Instruction::i32LeSigned,
            OpCode::i32LeUnsigned => Instruction::i32LeUnsigned,
            OpCode::i32GeSigned => Instruction::i32GeSigned,
            OpCode::i32GeUnsigned => Instruction::i32GeUnsigned,
            OpCode::i64EqZero => Instruction::i64EqZero,
            OpCode::i64Eq => Instruction::i64Eq,
            OpCode::i64Ne => Instruction::i64Ne,
            OpCode::i64LtSigned => Instruction::i64LtSigned,
            OpCode::i64LtUnsigned => Instruction::i64LtUnsigned,
            OpCode::i64GtSigned => Instruction::i64GtSigned,
            OpCode::i64GtUnsigned => Instruction::i64GtUnsigned,
            OpCode::i64LeSigned => Instruction::i64LeSigned,
            OpCode::i64LeUnsigned => Instruction::i64LeUnsigned,
            OpCode::i64GeSigned => Instruction::i64GeSigned,
            OpCode::i64GeUnsigned => Instruction::i64GeUnsigned,
            OpCode::f32Eq => Instruction::f32Eq,
            OpCode::f32Ne => Instruction::f32Ne,
            OpCode::f32Lt => Instruction::f32Lt,
            OpCode::f32Gt => Instruction::f32Gt,
            OpCode::f32Le => Instruction::f32Le,
            OpCode::f32Ge => Instruction::f32Ge,
            OpCode::f64Eq => Instruction::f64Eq,
            OpCode::f64Ne => Instruction::f64Ne,
            OpCode::f64Lt => Instruction::f64Lt,
            OpCode::f64Gt => Instruction::f64Gt,
            OpCode::f64Le => Instruction::f64Le,
            OpCode::f64Ge => Instruction::f64Ge,
            OpCode::i32CountLeadingZeros => Instruction::i32CountLeadingZeros,
            OpCode::i32CountTrailingZeros => Instruction::i32CountTrailingZeros,
            OpCode::i32PopulationCount => Instruction::i32PopulationCount,
            OpCode::i32Add => Instruction::i32Add,
            OpCode::i32Sub => Instruction::i32Sub,
            OpCode::i32Mul => Instruction::i32Mul,
            OpCode::i32SignedDiv => Instruction::i32SignedDiv,
            OpCode::i32UnsignedDiv => Instruction::i32UnsignedDiv,
            OpCode::i32SignedRem => Instruction::i32SignedRem,
            OpCode::i32UnsignedRem => Instruction::i32UnsignedRem,
            OpCode::i32BitwiseAnd => Instruction::i32BitwiseAnd,
            OpCode::i32BitwiseOr => Instruction::i32BitwiseOr,
            OpCode::i32BitwiseXor => Instruction::i32BitwiseXor,
            OpCode::i32BitwiseShiftLeft => Instruction::i32BitwiseShiftLeft,
            OpCode::i32SignedBitwiseShiftRight => Instruction::i32SignedBitwiseShiftRight,
            OpCode::i32UnsignedBitwiseShiftRight => Instruction::i32UnsignedBitwiseShiftRight,
            OpCode::i32RotateLeft => Instruction::i32RotateLeft,
            OpCode::i32RotateRight => Instruction::i32RotateRight,
            OpCode::i64CountLeadingZeros => Instruction::i64CountLeadingZeros,
            OpCode::i64CountTrailingZeros => Instruction::i64CountTrailingZeros,
            OpCode::i64PopulationCount => Instruction::i64PopulationCount,
            OpCode::i64Add => Instruction::i64Add,
            OpCode::i64Sub => Instruction::i64Sub,
            OpCode::i64Mul => Instruction::i64Mul,
            OpCode::i64SignedDiv => Instruction::i64SignedDiv,
            OpCode::i64UnsignedDiv => Instruction::i64UnsignedDiv,
            OpCode::i64SignedRem => Instruction::i64SignedRem,
            OpCode::i64UnsignedRem => Instruction::i64UnsignedRem,
            OpCode::i64BitwiseAnd => Instruction::i64BitwiseAnd,
            OpCode::i64BitwiseOr => Instruction::i64BitwiseOr,
            OpCode::i64BitwiseXor => Instruction::i64BitwiseXor,
            OpCode::i64BitwiseShiftLeft => Instruction::i64BitwiseShiftLeft,
            OpCode::i64SignedBitwiseShiftRight => Instruction::i64SignedBitwiseShiftRight,
            OpCode::i64UnsignedBitwiseShiftRight => Instruction::i64UnsignedBitwiseShiftRight,
            OpCode::i64RotateLeft => Instruction::i64RotateLeft,
            OpCode::i64RotateRight => Instruction::i64RotateRight,
            OpCode::f32Abs => Instruction::f32Abs,
            OpCode::f32Neg => Instruction::f32Neg,
            OpCode::f32Ceil => Instruction::f32Ceil,
            OpCode::f32Floor => Instruction::f32Floor,
            OpCode::f32Trunc => Instruction::f32Trunc,
            OpCode::f32Nearest => Instruction::f32Nearest,
            OpCode::f32Sqrt => Instruction::f32Sqrt,
            OpCode::f32Add => Instruction::f32Add,
            OpCode::f32Sub => Instruction::f32Sub,
            OpCode::f32Mul => Instruction::f32Mul,
            OpCode::f32Div => Instruction::f32Div,
            OpCode::f32Min => Instruction::f32Min,
            OpCode::f32Max => Instruction::f32Max,
            OpCode::f32CopySign => Instruction::f32CopySign,
            OpCode::f64Abs => Instruction::f64Abs,
            OpCode::f64Neg => Instruction::f64Neg,
            OpCode::f64Ceil => Instruction::f64Ceil,
            OpCode::f64Floor => Instruction::f64Floor,
            OpCode::f64Trunc => Instruction::f64Trunc,
            OpCode::f64Nearest => Instruction::f64Nearest,
            OpCode::f64Sqrt => Instruction::f64Sqrt,
            OpCode::f64Add => Instruction::f64Add,
            OpCode::f64Sub => Instruction::f64Sub,
            OpCode::f64Mul => Instruction::f64Mul,
            OpCode::f64Div => Instruction::f64Div,
            OpCode::f64Min => Instruction::f64Min,
            OpCode::f64Max => Instruction::f64Max,
            OpCode::f64CopySign => Instruction::f64CopySign,
            OpCode::i32WrapI64 => Instruction::i32WrapI64,
            OpCode::i32TruncF32Signed => Instruction::i32TruncF32Signed,
            OpCode::i32TruncF32Unsigned => Instruction::i32TruncF32Unsigned,
            OpCode::i32TruncF64Signed => Instruction::i32TruncF64Signed,
            OpCode::i32TruncF64Unsigned => Instruction::i32TruncF64Unsigned,
            OpCode::i64Extendi32Signed => Instruction::i64Extendi32Signed,
            OpCode::i64Extendi32Unsigned => Instruction::i64Extendi32Unsigned,
            OpCode::i64TruncF32Signed => Instruction::i64TruncF32Signed,
            OpCode::i64TruncF32Unsigned => Instruction::i64TruncF32Unsigned,
            OpCode::i64TruncF64Signed => Instruction::i64TruncF64Signed,
            OpCode::i64TruncF64Unsigned => Instruction::i64TruncF64Unsigned,
            OpCode::f32SignedConvertI32 => Instruction::f32SignedConvertI32,
            OpCode::f32UnsignedConvertI32 => Instruction::f32UnsignedConvertI32,
            OpCode::f32SignedConvertI64 => Instruction::f32SignedConvertI64,
            OpCode::f32UnsignedConvertI64 => Instruction::f32UnsignedConvertI64,
            OpCode::f32DemoteF64 => Instruction::f32DemoteF64,
            OpCode::f64SignedConvertI32 => Instruction::f64SignedConvertI32,
            OpCode::f64UnsignedConvertI32 => Instruction::f64UnsignedConvertI32,
            OpCode::f64SignedConvertI64 => Instruction::f64SignedConvertI64,
            OpCode::f64UnsignedConvertI64 => Instruction::f64UnsignedConvertI64,
            OpCode::f64PromoteF32 => Instruction::f64PromoteF32,
            OpCode::i32ReinterpretF32 => Instruction::i32ReinterpretF32,
            OpCode::i64ReinterpretF64 => Instruction::i64ReinterpretF64,
            OpCode::f32ReinterpretI32 => Instruction::f32ReinterpretI32,
            OpCode::f64ReinterpretI64 => Instruction::f64ReinterpretI64,
            OpCode::i32Extend8Signed => Instruction::i32Extend8Signed,
            OpCode::i32Extend16Signed => Instruction::i32Extend16Signed,
            OpCode::i64Extend8Signed => Instruction::i64Extend8Signed,
            OpCode::i64Extend16Signed => Instruction::i64Extend16Signed,
            OpCode::i64Extend32Signed => Instruction::i64Extend32Signed,
            OpCode::RefNull => Instruction::RefNull(self.parse_ref_type()?),
            OpCode::RefIsNull => Instruction::RefIsNull,
            OpCode::RefFunc => Instruction::RefFunc(self.read_u32()?),
            OpCode::i32TruncSatf32Signed => Instruction::i32TruncSatf32Signed,
            OpCode::i32TruncSatf32Unsigned => Instruction::i32TruncSatf32Unsigned,
            OpCode::i32TruncSatf64Signed => Instruction::i32TruncSatf64Signed,
            OpCode::i32TruncSatf64Unsigned => Instruction::i32TruncSatf64Unsigned,
            OpCode::i64TruncSatf32Signed => Instruction::i64TruncSatf32Signed,
            OpCode::i64TruncSatf32Unsigned => Instruction::i64TruncSatf32Unsigned,
            OpCode::i64TruncSatf64Signed => Instruction::i64TruncSatf64Signed,
            OpCode::i64TruncSatf64Unsigned => Instruction::i64TruncSatf64Unsigned,
            OpCode::MemoryInit => Instruction::MemoryInit(self.read_u32()?),
            OpCode::DataDrop => Instruction::DataDrop(self.read_u32()?),
            OpCode::MemoryCopy => Instruction::MemoryCopy,
            OpCode::MemoryFill => Instruction::MemoryFill,
            OpCode::TableInit => Instruction::TableInit(self.read_u32()?, self.read_u32()?),
            OpCode::ElemDrop => Instruction::ElemDrop(self.read_u32()?),
            OpCode::TableCopy => Instruction::TableCopy(self.read_u32()?, self.read_u32()?),
            OpCode::TableGrow => Instruction::TableGrow(self.read_u32()?),
            OpCode::TableSize => Instruction::TableSize(self.read_u32()?),
            OpCode::TableFill => Instruction::TableFill(self.read_u32()?),
        })
    }

    fn parse_block_type(&mut self) -> WResult<BlockType> {
        Ok(match self.next_byte()? {
            0x40 => BlockType::Empty,
            0x7f => BlockType::ValType(ValueType::I32),
            0x7e => BlockType::ValType(ValueType::I64),
            0x7d => BlockType::ValType(ValueType::F32),
            0x7c => BlockType::ValType(ValueType::F64),
            0x70 => BlockType::ValType(ValueType::FuncRef),
            0x6f => BlockType::ValType(ValueType::ExternRef),
            _ => todo!(),
        })
    }
}
