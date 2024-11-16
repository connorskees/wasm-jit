use std::collections::{HashMap, VecDeque};

use crate::{
    error::{WResult, WasmError},
    opcode::Instruction,
    section::{
        CodeSection, CustomSection, DataSection, ElementSection, ExportSection, FunctionSection,
        GlobalSection, ImportSection, MemorySection, NameMap, NameSection, NameSubsection, Section,
        TableSection, TypeSection,
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

    pub fn read_n_bytes<const N: usize>(&mut self) -> WResult<[u8; N]> {
        Ok(self.read_range(N)?.try_into().unwrap())
    }

    fn expect_magic(&mut self) -> WResult<()> {
        let magic = self.read_4_bytes()?;
        anyhow::ensure!(MAGIC == magic, WasmError::InvalidMagicHeader);
        Ok(())
    }

    fn expect_version(&mut self) -> WResult<()> {
        let version = self.read_4_bytes()?;
        anyhow::ensure!(VERSION == version, WasmError::InvalidVersion);
        Ok(())
    }

    fn expect_byte(&mut self, expected: u8) -> WResult<()> {
        let found = self.next_byte()?;

        anyhow::ensure!(
            found == expected,
            WasmError::ExpectedByte { found, expected }
        );

        Ok(())
    }

    fn next_byte(&mut self) -> WResult<u8> {
        self.buffer
            .get(self.cursor)
            .copied()
            .inspect(|_| self.cursor += 1)
            .ok_or_else(|| anyhow::anyhow!(WasmError::UnexpectedEof { pos: self.cursor }))
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
        self.read_n_bytes::<4>()
    }

    fn read_8_bytes(&mut self) -> WResult<[u8; 8]> {
        self.read_n_bytes::<8>()
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

        let end = self.cursor + (size as usize - num_range_bytes);

        let section = match name {
            "name" => CustomSection::Name(self.parse_name_section(end)?),
            _ => {
                let contents = self.read_range(size as usize - num_range_bytes)?;
                CustomSection::Unknown { name, contents }
            }
        };

        Ok(section)
    }

    fn parse_name_subsection(&mut self) -> WResult<NameSubsection<'a>> {
        let subsection_id = self.next_byte()?;
        let _size = self.read_u32()?;

        Ok(match subsection_id {
            0 => NameSubsection::Module(self.parse_name()?),
            1 => NameSubsection::Func(self.parse_name_map()?),
            2 => todo!("local name"),
            // extended name section <https://github.com/WebAssembly/extended-name-section/blob/main/proposals/extended-name-section/Overview.md#global-names>
            5 => NameSubsection::Table(self.parse_name_map()?),
            6 => NameSubsection::Memory(self.parse_name_map()?),
            7 => NameSubsection::Global(self.parse_name_map()?),
            8 => NameSubsection::Elem(self.parse_name_map()?),
            9 => NameSubsection::Data(self.parse_name_map()?),
            11 => NameSubsection::Tag(self.parse_name_map()?),
            _ => todo!("invalid name section subsection: {subsection_id}"),
        })
    }

    fn parse_name_section(&mut self, end: usize) -> WResult<NameSection<'a>> {
        let mut subsections = Vec::new();

        while self.cursor < end {
            subsections.push(self.parse_name_subsection()?);
        }

        Ok(NameSection { subsections })
    }

    fn parse_name_assoc(&mut self) -> WResult<(u32, &'a str)> {
        let idx = self.read_u32()?;
        let name = self.parse_name()?;
        Ok((idx, name))
    }

    fn parse_name_map(&mut self) -> WResult<NameMap<'a>> {
        Ok(HashMap::from_iter(self.parse_vec(Self::parse_name_assoc)?))
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
            let inst = match self.next_instruction()? {
                Instruction::Block(..) => {
                    let block_type = self.parse_block_type()?;
                    assert_eq!(block_type, BlockType::Empty);

                    let start = instructions.len();

                    let label = Label::incomplete_block(block_type);

                    nested.push(start);

                    Instruction::Block(label)
                }
                Instruction::Loop(..) => {
                    let block_type = self.parse_block_type()?;
                    assert_eq!(block_type, BlockType::Empty);
                    let start = instructions.len();

                    let label = Label::new_loop(start, block_type);

                    nested.push(start);

                    Instruction::Loop(label)
                }
                Instruction::If(..) => {
                    let block_type = self.parse_block_type()?;
                    assert_eq!(block_type, BlockType::Empty);

                    let start = instructions.len();

                    let label = Label::incomplete_block(block_type);

                    nested.push(start);

                    Instruction::If(label)
                }
                Instruction::End => {
                    let end = instructions.len();

                    instructions.push(Instruction::End);

                    if let Some(block_idx) = nested.pop() {
                        instructions[block_idx].set_label_end(end);
                        continue;
                    } else {
                        break;
                    }
                }
                inst => inst,
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

    fn next_instruction(&mut self) -> WResult<Instruction> {
        Ok(match self.next_byte()? {
            0x00 => Instruction::Unreachable,
            0x01 => Instruction::Nop,
            0x02 => Instruction::Block(Label::incomplete_block(BlockType::Empty)),
            0x03 => Instruction::Loop(Label::incomplete_block(BlockType::Empty)),
            0x04 => Instruction::If(Label::incomplete_block(BlockType::Empty)),
            0x05 => Instruction::Else,
            0x0b => Instruction::End,
            0x0c => Instruction::Branch(self.read_u32()?),
            0x0d => Instruction::BranchIf(self.read_u32()?),
            0x0e => {
                let (a, b) = self.parse_branch_table_immediate()?;
                Instruction::BranchTable(a, b)
            }
            0x0f => Instruction::Return,
            0x10 => Instruction::Call(self.read_u32()?),
            0x11 => Instruction::CallIndirect(self.read_u32()?, self.read_u32()?),
            0x1a => Instruction::Drop,
            0x1b => Instruction::Select,
            0x1c => Instruction::SelectT(self.parse_select_immediate()?),
            0x20 => Instruction::LocalGet(self.read_u32()?),
            0x21 => Instruction::LocalSet(self.read_u32()?),
            0x22 => Instruction::LocalTee(self.read_u32()?),
            0x23 => Instruction::GlobalGet(self.read_u32()?),
            0x24 => Instruction::GlobalSet(self.read_u32()?),
            0x25 => Instruction::TableGet(self.read_u32()?),
            0x26 => Instruction::TableSet(self.read_u32()?),
            0x28 => Instruction::i32Load(self.parse_memory_operand()?),
            0x29 => Instruction::i64Load(self.parse_memory_operand()?),
            0x2a => Instruction::f32Load(self.parse_memory_operand()?),
            0x2b => Instruction::f64Load(self.parse_memory_operand()?),
            0x2c => Instruction::i32Load8Signed(self.parse_memory_operand()?),
            0x2d => Instruction::i32Load8Unsigned(self.parse_memory_operand()?),
            0x2e => Instruction::i32Load16Signed(self.parse_memory_operand()?),
            0x2f => Instruction::i32Load16Unsigned(self.parse_memory_operand()?),
            0x30 => Instruction::i64Load8Signed(self.parse_memory_operand()?),
            0x31 => Instruction::i64Load8Unsigned(self.parse_memory_operand()?),
            0x32 => Instruction::i64Load16Signed(self.parse_memory_operand()?),
            0x33 => Instruction::i64Load16Unsigned(self.parse_memory_operand()?),
            0x34 => Instruction::i64Load32Signed(self.parse_memory_operand()?),
            0x35 => Instruction::i64Load32Unsigned(self.parse_memory_operand()?),
            0x36 => Instruction::i32Store(self.parse_memory_operand()?),
            0x37 => Instruction::i64Store(self.parse_memory_operand()?),
            0x38 => Instruction::f32Store(self.parse_memory_operand()?),
            0x39 => Instruction::f64Store(self.parse_memory_operand()?),
            0x3a => Instruction::i32Store8(self.parse_memory_operand()?),
            0x3b => Instruction::i32Store16(self.parse_memory_operand()?),
            0x3c => Instruction::i64Store8(self.parse_memory_operand()?),
            0x3d => Instruction::i64Store16(self.parse_memory_operand()?),
            0x3e => Instruction::i64Store32(self.parse_memory_operand()?),
            0x3f => Instruction::MemorySize,
            0x40 => Instruction::MemoryGrow,
            0x41 => Instruction::i32Const(self.read_i32()?),
            0x42 => Instruction::i64Const(self.read_i64()?),
            0x43 => Instruction::f32Const(self.read_f32()?),
            0x44 => Instruction::f64Const(self.read_f64()?),
            0x45 => Instruction::i32EqZero,
            0x46 => Instruction::i32Eq,
            0x47 => Instruction::i32Ne,
            0x48 => Instruction::i32LtSigned,
            0x49 => Instruction::i32LtUnsigned,
            0x4a => Instruction::i32GtSigned,
            0x4b => Instruction::i32GtUnsigned,
            0x4c => Instruction::i32LeSigned,
            0x4d => Instruction::i32LeUnsigned,
            0x4e => Instruction::i32GeSigned,
            0x4f => Instruction::i32GeUnsigned,
            0x50 => Instruction::i64EqZero,
            0x51 => Instruction::i64Eq,
            0x52 => Instruction::i64Ne,
            0x53 => Instruction::i64LtSigned,
            0x54 => Instruction::i64LtUnsigned,
            0x55 => Instruction::i64GtSigned,
            0x56 => Instruction::i64GtUnsigned,
            0x57 => Instruction::i64LeSigned,
            0x58 => Instruction::i64LeUnsigned,
            0x59 => Instruction::i64GeSigned,
            0x5a => Instruction::i64GeUnsigned,
            0x5b => Instruction::f32Eq,
            0x5c => Instruction::f32Ne,
            0x5d => Instruction::f32Lt,
            0x5e => Instruction::f32Gt,
            0x5f => Instruction::f32Le,
            0x60 => Instruction::f32Ge,
            0x61 => Instruction::f64Eq,
            0x62 => Instruction::f64Ne,
            0x63 => Instruction::f64Lt,
            0x64 => Instruction::f64Gt,
            0x65 => Instruction::f64Le,
            0x66 => Instruction::f64Ge,
            0x67 => Instruction::i32CountLeadingZeros,
            0x68 => Instruction::i32CountTrailingZeros,
            0x69 => Instruction::i32PopulationCount,
            0x6a => Instruction::i32Add,
            0x6b => Instruction::i32Sub,
            0x6c => Instruction::i32Mul,
            0x6d => Instruction::i32SignedDiv,
            0x6e => Instruction::i32UnsignedDiv,
            0x6f => Instruction::i32SignedRem,
            0x70 => Instruction::i32UnsignedRem,
            0x71 => Instruction::i32BitwiseAnd,
            0x72 => Instruction::i32BitwiseOr,
            0x73 => Instruction::i32BitwiseXor,
            0x74 => Instruction::i32BitwiseShiftLeft,
            0x75 => Instruction::i32SignedBitwiseShiftRight,
            0x76 => Instruction::i32UnsignedBitwiseShiftRight,
            0x77 => Instruction::i32RotateLeft,
            0x78 => Instruction::i32RotateRight,
            0x79 => Instruction::i64CountLeadingZeros,
            0x7a => Instruction::i64CountTrailingZeros,
            0x7b => Instruction::i64PopulationCount,
            0x7c => Instruction::i64Add,
            0x7d => Instruction::i64Sub,
            0x7e => Instruction::i64Mul,
            0x7f => Instruction::i64SignedDiv,
            0x80 => Instruction::i64UnsignedDiv,
            0x81 => Instruction::i64SignedRem,
            0x82 => Instruction::i64UnsignedRem,
            0x83 => Instruction::i64BitwiseAnd,
            0x84 => Instruction::i64BitwiseOr,
            0x85 => Instruction::i64BitwiseXor,
            0x86 => Instruction::i64BitwiseShiftLeft,
            0x87 => Instruction::i64SignedBitwiseShiftRight,
            0x88 => Instruction::i64UnsignedBitwiseShiftRight,
            0x89 => Instruction::i64RotateLeft,
            0x8a => Instruction::i64RotateRight,
            0x8b => Instruction::f32Abs,
            0x8c => Instruction::f32Neg,
            0x8d => Instruction::f32Ceil,
            0x8e => Instruction::f32Floor,
            0x8f => Instruction::f32Trunc,
            0x90 => Instruction::f32Nearest,
            0x91 => Instruction::f32Sqrt,
            0x92 => Instruction::f32Add,
            0x93 => Instruction::f32Sub,
            0x94 => Instruction::f32Mul,
            0x95 => Instruction::f32Div,
            0x96 => Instruction::f32Min,
            0x97 => Instruction::f32Max,
            0x98 => Instruction::f32CopySign,
            0x99 => Instruction::f64Abs,
            0x9a => Instruction::f64Neg,
            0x9b => Instruction::f64Ceil,
            0x9c => Instruction::f64Floor,
            0x9d => Instruction::f64Trunc,
            0x9e => Instruction::f64Nearest,
            0x9f => Instruction::f64Sqrt,
            0xa0 => Instruction::f64Add,
            0xa1 => Instruction::f64Sub,
            0xa2 => Instruction::f64Mul,
            0xa3 => Instruction::f64Div,
            0xa4 => Instruction::f64Min,
            0xa5 => Instruction::f64Max,
            0xa6 => Instruction::f64CopySign,
            0xa7 => Instruction::i32WrapI64,
            0xa8 => Instruction::i32TruncF32Signed,
            0xa9 => Instruction::i32TruncF32Unsigned,
            0xaa => Instruction::i32TruncF64Signed,
            0xab => Instruction::i32TruncF64Unsigned,
            0xac => Instruction::i64Extendi32Signed,
            0xad => Instruction::i64Extendi32Unsigned,
            0xae => Instruction::i64TruncF32Signed,
            0xaf => Instruction::i64TruncF32Unsigned,
            0xb0 => Instruction::i64TruncF64Signed,
            0xb1 => Instruction::i64TruncF64Unsigned,
            0xb2 => Instruction::f32SignedConvertI32,
            0xb3 => Instruction::f32UnsignedConvertI32,
            0xb4 => Instruction::f32SignedConvertI64,
            0xb5 => Instruction::f32UnsignedConvertI64,
            0xb6 => Instruction::f32DemoteF64,
            0xb7 => Instruction::f64SignedConvertI32,
            0xb8 => Instruction::f64UnsignedConvertI32,
            0xb9 => Instruction::f64SignedConvertI64,
            0xba => Instruction::f64UnsignedConvertI64,
            0xbb => Instruction::f64PromoteF32,
            0xbc => Instruction::i32ReinterpretF32,
            0xbd => Instruction::i64ReinterpretF64,
            0xbe => Instruction::f32ReinterpretI32,
            0xbf => Instruction::f64ReinterpretI64,
            0xc0 => Instruction::i32Extend8Signed,
            0xc1 => Instruction::i32Extend16Signed,
            0xc2 => Instruction::i64Extend8Signed,
            0xc3 => Instruction::i64Extend16Signed,
            0xc4 => Instruction::i64Extend32Signed,
            0xd0 => Instruction::RefNull(self.parse_ref_type()?),
            0xd1 => Instruction::RefIsNull,
            0xd2 => Instruction::RefFunc(self.read_u32()?),
            0xfc => match self.read_u32()? {
                0x00 => Instruction::i32TruncSatf32Signed,
                0x01 => Instruction::i32TruncSatf32Unsigned,
                0x02 => Instruction::i32TruncSatf64Signed,
                0x03 => Instruction::i32TruncSatf64Unsigned,
                0x04 => Instruction::i64TruncSatf32Signed,
                0x05 => Instruction::i64TruncSatf32Unsigned,
                0x06 => Instruction::i64TruncSatf64Signed,
                0x07 => Instruction::i64TruncSatf64Unsigned,
                0x08 => Instruction::MemoryInit(self.read_u32()?),
                0x09 => Instruction::DataDrop(self.read_u32()?),
                0x0a => Instruction::MemoryCopy,
                0x0b => Instruction::MemoryFill,
                0x0c => Instruction::TableInit(self.read_u32()?, self.read_u32()?),
                0x0d => Instruction::ElemDrop(self.read_u32()?),
                0x0e => Instruction::TableCopy(self.read_u32()?, self.read_u32()?),
                0x0f => Instruction::TableGrow(self.read_u32()?),
                0x10 => Instruction::TableSize(self.read_u32()?),
                0x11 => Instruction::TableFill(self.read_u32()?),
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
