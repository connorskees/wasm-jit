use std::convert::TryInto;

use anyhow::Context;

use crate::{
    error::{WResult, WasmError},
    instance::{FuncInst, ModuleInst},
    num::{Num, Sign},
    opcode::Instruction,
    stack::{Frame, Label, Stack, StackEntry},
    store::Store,
    BlockType, Expr, ExternValue, ImportValue, MemoryOperand, Module, Value, ValueType,
};

pub struct Interpreter<'a> {
    module_insts: Vec<ModuleInst<'a>>,
    store: Store<'a>,
    stack: Stack,
    globals: Vec<Value>,
}

impl<'a> Interpreter<'a> {
    pub fn new(buffer: &'a [u8], imports: &[(&str, &str, ImportValue)]) -> WResult<Self> {
        let module = Module::new(buffer)?;
        let mut store = Store::default();
        let stack = Stack::new();

        let module_inst = store.alloc_module(module, imports)?;

        let globals = Vec::with_capacity(store.globals.len());

        let mut i = Self {
            module_insts: vec![module_inst],
            store,
            stack,
            globals,
        };

        i.initialize_globals()?;

        Ok(i)
    }

    fn initialize_globals(&mut self) -> WResult<()> {
        let globals: Vec<_> = self.store.globals.iter().map(|g| g.value.clone()).collect();

        for g in globals {
            self.invoke_expr(&g, &mut Frame::default())?;
            let val = self.stack.pop_value()?;
            self.globals.push(val)
        }

        Ok(())
    }

    pub fn invoke(&mut self, args: &[Value]) -> WResult<Option<Value>> {
        let func_addr = 0;

        for arg in args {
            self.stack.push_value(*arg);
        }

        self.invoke_function(func_addr)
    }

    /// Invoke function with `name` given `args`
    pub fn invoke_export(&mut self, name: &str, args: &[Value]) -> WResult<Option<Value>> {
        let func_addr = self.module_insts[0]
            .exports
            .iter()
            .find_map(|export| {
                if export.name != name {
                    return None;
                }

                match export.value {
                    ExternValue::Func(func) => Some(func),
                    _ => None,
                }
            })
            .ok_or(WasmError::UndefinedExport)?;

        for arg in args {
            self.stack.push_value(*arg);
        }

        self.invoke_function(func_addr)
    }

    fn invoke_function(&mut self, addr: usize) -> WResult<Option<Value>> {
        let func_inst = &self.store.funcs[addr];

        match func_inst {
            FuncInst::Host { .. } => {
                dbg!("host function");
                Ok(Some(Value::F32(0.0)))
            }
            FuncInst::Local { ty, code, .. } => {
                let mut args = self
                    .stack
                    .pop_n(ty.number_of_args())
                    .ok_or(WasmError::InvalidNumberOfArgs)?
                    .into_iter()
                    .map(StackEntry::assert_value)
                    .collect::<WResult<Vec<_>>>()?;

                args.extend(code.locals.iter().map(ValueType::zero));

                let mut frame = Frame {
                    module_idx: 0,
                    locals: args,
                };

                self.stack.push(StackEntry::Activation(vec![frame.clone()]));

                self.stack.push(StackEntry::Label(Label {
                    // todo: wrong
                    block_type: BlockType::Empty,
                    continuation: code.expr.0.len(),
                }));

                let expr = code.expr.clone();

                let has_return_value = !(ty.1).0.is_empty();

                self.invoke_expr(&expr, &mut frame)?;

                Ok(if has_return_value {
                    Some(self.stack.pop_value().context("return value")?)
                } else {
                    None
                })
            }
        }
    }

    fn invoke_expr(&mut self, expr: &Expr, frame: &mut Frame) -> WResult<()> {
        let mut idx = 0;

        while idx < expr.0.len() {
            match &expr.0[idx] {
                Instruction::If(label) => idx = self.if_op(idx, *label)?,
                Instruction::Else => todo!("if/else"),

                // ibinop
                Instruction::i32Add => self.bin_op::<i32>(&|a, b| a.wrapping_add(b))?,
                Instruction::i64Add => self.bin_op::<i64>(&|a, b| a.wrapping_add(b))?,
                Instruction::i32Sub => self.bin_op::<i32>(&|a, b| a.wrapping_sub(b))?,
                Instruction::i64Sub => self.bin_op::<i64>(&|a, b| a.wrapping_sub(b))?,
                Instruction::i32Mul => self.bin_op::<i32>(&|a, b| a.wrapping_mul(b))?,
                Instruction::i64Mul => self.bin_op::<i64>(&|a, b| a.wrapping_mul(b))?,
                Instruction::i32SignedDiv => self.bin_op::<i32>(&|a, b| a / b)?,
                Instruction::i64SignedDiv => self.bin_op::<i64>(&|a, b| a / b)?,
                Instruction::i32UnsignedDiv => self.bin_op::<u32>(&|a, b| a / b)?,
                Instruction::i64UnsignedDiv => self.bin_op::<u64>(&|a, b| a / b)?,
                Instruction::i32SignedRem => self.bin_op::<i32>(&|a, b| a % b)?,
                Instruction::i64SignedRem => self.bin_op::<i64>(&|a, b| a % b)?,
                Instruction::i32UnsignedRem => self.bin_op::<u32>(&|a, b| a % b)?,
                Instruction::i64UnsignedRem => self.bin_op::<u64>(&|a, b| a % b)?,
                Instruction::i32BitwiseAnd => self.bin_op::<i32>(&|a, b| a & b)?,
                Instruction::i64BitwiseAnd => self.bin_op::<i64>(&|a, b| a & b)?,
                Instruction::i32BitwiseOr => self.bin_op::<i32>(&|a, b| a | b)?,
                Instruction::i64BitwiseOr => self.bin_op::<i64>(&|a, b| a | b)?,
                Instruction::i32BitwiseXor => self.bin_op::<i32>(&|a, b| a ^ b)?,
                Instruction::i64BitwiseXor => self.bin_op::<i64>(&|a, b| a ^ b)?,
                Instruction::i32BitwiseShiftLeft => self.bin_op::<i32>(&|a, b| a << b)?,
                Instruction::i64BitwiseShiftLeft => self.bin_op::<i64>(&|a, b| a << b)?,
                Instruction::i32SignedBitwiseShiftRight => self.bin_op::<i32>(&|a, b| a >> b)?,
                Instruction::i64SignedBitwiseShiftRight => self.bin_op::<i64>(&|a, b| a >> b)?,
                Instruction::i32UnsignedBitwiseShiftRight => self.bin_op::<i32>(&|a, b| {
                    i32::reinterpret(u32::reinterpret(a) >> u32::reinterpret(b))
                })?,
                Instruction::i64UnsignedBitwiseShiftRight => self.bin_op::<i64>(&|a, b| {
                    i64::reinterpret(u64::reinterpret(a) >> u64::reinterpret(b))
                })?,
                Instruction::i32RotateLeft => {
                    self.bin_op::<i32>(&|a, b| a.rotate_left(u32::reinterpret(b & 31)))?
                }
                Instruction::i64RotateLeft => {
                    self.bin_op::<i64>(&|a, b| a.rotate_left(u64::reinterpret(b & 63) as u32))?
                }
                Instruction::i32RotateRight => {
                    self.bin_op::<i32>(&|a, b| a.rotate_right(u32::reinterpret(b & 31)))?
                }
                Instruction::i64RotateRight => {
                    self.bin_op::<i64>(&|a, b| a.rotate_right(u64::reinterpret(b & 63) as u32))?
                }

                // fbinop
                Instruction::f32Add => self.bin_op::<f32>(&|a, b| a + b)?,
                Instruction::f64Add => self.bin_op::<f64>(&|a, b| a + b)?,
                Instruction::f32Sub => self.bin_op::<f32>(&|a, b| a - b)?,
                Instruction::f64Sub => self.bin_op::<f64>(&|a, b| a - b)?,
                Instruction::f32Mul => self.bin_op::<f32>(&|a, b| a * b)?,
                Instruction::f64Mul => self.bin_op::<f64>(&|a, b| a * b)?,
                Instruction::f32Div => self.bin_op::<f32>(&|a, b| a / b)?,
                Instruction::f64Div => self.bin_op::<f64>(&|a, b| a / b)?,
                Instruction::f32Min => self.bin_op::<f32>(&|a, b| a.min(b))?,
                Instruction::f64Min => self.bin_op::<f64>(&|a, b| a.min(b))?,
                Instruction::f32Max => self.bin_op::<f32>(&|a, b| a.max(b))?,
                Instruction::f64Max => self.bin_op::<f64>(&|a, b| a.max(b))?,
                Instruction::f32CopySign => todo!(),
                Instruction::f64CopySign => todo!(),

                // iunop
                Instruction::i32CountLeadingZeros => {
                    self.unary_op::<i32>(&|a| a.leading_zeros() as i32)?
                }
                Instruction::i64CountLeadingZeros => {
                    self.unary_op::<i64>(&|a| a.leading_zeros() as i64)?
                }
                Instruction::i32CountTrailingZeros => {
                    self.unary_op::<i32>(&|a| a.trailing_zeros() as i32)?
                }
                Instruction::i64CountTrailingZeros => {
                    self.unary_op::<i64>(&|a| a.trailing_zeros() as i64)?
                }
                Instruction::i32PopulationCount => {
                    self.unary_op::<i32>(&|a| a.count_ones() as i32)?
                }
                Instruction::i64PopulationCount => {
                    self.unary_op::<i64>(&|a| a.count_ones() as i64)?
                }

                // funop
                Instruction::f32Abs => self.unary_op::<f32>(&|a| a.abs())?,
                Instruction::f64Abs => self.unary_op::<f64>(&|a| a.abs())?,
                Instruction::f32Neg => self.unary_op::<f32>(&|a| -a)?,
                Instruction::f64Neg => self.unary_op::<f64>(&|a| -a)?,
                Instruction::f32Sqrt => self.unary_op::<f32>(&|a| a.sqrt())?,
                Instruction::f64Sqrt => self.unary_op::<f64>(&|a| a.sqrt())?,
                Instruction::f32Ceil => self.unary_op::<f32>(&|a| a.ceil())?,
                Instruction::f64Ceil => self.unary_op::<f64>(&|a| a.ceil())?,
                Instruction::f32Floor => self.unary_op::<f32>(&|a| a.floor())?,
                Instruction::f64Floor => self.unary_op::<f64>(&|a| a.floor())?,
                Instruction::f32Trunc => self.unary_op::<f32>(&|a| a.trunc())?,
                Instruction::f64Trunc => self.unary_op::<f64>(&|a| a.trunc())?,
                Instruction::f32Nearest => todo!("unary op f nearest"),
                Instruction::f64Nearest => todo!("unary op f nearest"),

                // itestop
                Instruction::i32EqZero => self.test_op::<i32>(&|a| a == 0)?,
                Instruction::i64EqZero => self.test_op::<i64>(&|a| a == 0)?,

                // const
                Instruction::i32Const(n) => self.stack.push_value(Value::I32(*n)),
                Instruction::i64Const(n) => self.stack.push_value(Value::I64(*n)),
                Instruction::f32Const(n) => self.stack.push_value(Value::F32(*n)),
                Instruction::f64Const(n) => self.stack.push_value(Value::F64(*n)),

                // irelop
                Instruction::i32LtSigned => self.rel_op::<i32>(&|a, b| a < b)?,
                Instruction::i64LtSigned => self.rel_op::<i64>(&|a, b| a < b)?,
                Instruction::i32LeSigned => self.rel_op::<i32>(&|a, b| a <= b)?,
                Instruction::i64LeSigned => self.rel_op::<i64>(&|a, b| a <= b)?,
                Instruction::i32GtSigned => self.rel_op::<i32>(&|a, b| a > b)?,
                Instruction::i64GtSigned => self.rel_op::<i64>(&|a, b| a > b)?,
                Instruction::i32GeSigned => self.rel_op::<i32>(&|a, b| a >= b)?,
                Instruction::i64GeSigned => self.rel_op::<i64>(&|a, b| a >= b)?,
                Instruction::i32LtUnsigned => self.rel_op::<u32>(&|a, b| a < b)?,
                Instruction::i64LtUnsigned => self.rel_op::<u64>(&|a, b| a < b)?,
                Instruction::i32LeUnsigned => self.rel_op::<u32>(&|a, b| a <= b)?,
                Instruction::i64LeUnsigned => self.rel_op::<u64>(&|a, b| a <= b)?,
                Instruction::i32GtUnsigned => self.rel_op::<u32>(&|a, b| a > b)?,
                Instruction::i64GtUnsigned => self.rel_op::<u64>(&|a, b| a > b)?,
                Instruction::i32GeUnsigned => self.rel_op::<u32>(&|a, b| a >= b)?,
                Instruction::i64GeUnsigned => self.rel_op::<u64>(&|a, b| a >= b)?,
                Instruction::i32Eq => self.rel_op::<i32>(&|a, b| a == b)?,
                Instruction::i64Eq => self.rel_op::<i64>(&|a, b| a == b)?,
                Instruction::i32Ne => self.rel_op::<i32>(&|a, b| a != b)?,
                Instruction::i64Ne => self.rel_op::<i64>(&|a, b| a != b)?,

                // frelop
                Instruction::f32Eq => self.rel_op::<f32>(&|a, b| a == b)?,
                Instruction::f64Eq => self.rel_op::<f64>(&|a, b| a == b)?,
                Instruction::f32Ne => self.rel_op::<f32>(&|a, b| a != b)?,
                Instruction::f64Ne => self.rel_op::<f64>(&|a, b| a != b)?,
                Instruction::f32Lt => self.rel_op::<f32>(&|a, b| a < b)?,
                Instruction::f64Lt => self.rel_op::<f64>(&|a, b| a < b)?,
                Instruction::f32Gt => self.rel_op::<f32>(&|a, b| a > b)?,
                Instruction::f64Gt => self.rel_op::<f64>(&|a, b| a > b)?,
                Instruction::f32Le => self.rel_op::<f32>(&|a, b| a <= b)?,
                Instruction::f64Le => self.rel_op::<f64>(&|a, b| a <= b)?,
                Instruction::f32Ge => self.rel_op::<f32>(&|a, b| a >= b)?,
                Instruction::f64Ge => self.rel_op::<f64>(&|a, b| a >= b)?,

                // cvtop
                Instruction::f64SignedConvertI64 => self.cvt_op::<i64, f64>(&|a| a as f64)?,
                Instruction::f64PromoteF32 => self.cvt_op::<f32, f64>(&|a| a as f64)?,
                Instruction::f32DemoteF64 => self.cvt_op::<f64, f32>(&|a| a as f32)?,
                Instruction::i32ReinterpretF32 => self.reinterpret::<f32, i32>()?,
                Instruction::i64ReinterpretF64 => self.reinterpret::<f64, i64>()?,
                Instruction::f32ReinterpretI32 => self.reinterpret::<i32, f32>()?,
                Instruction::f64ReinterpretI64 => self.reinterpret::<i64, f64>()?,
                Instruction::i32WrapI64 => self.cvt_op::<i64, i32>(&|a| a as i32)?,
                Instruction::i64Extendi32Signed => self.cvt_op::<i32, i64>(&|a| a as i64)?,
                Instruction::i64Extendi32Unsigned => self.cvt_op::<u32, i64>(&|a| a as i64)?,
                Instruction::i32Extend8Signed => self.cvt_op::<i32, i32>(&|a| a as i8 as i32)?,
                Instruction::i32Extend16Signed => self.cvt_op::<i32, i32>(&|a| a as i16 as i32)?,
                Instruction::i64Extend8Signed => self.cvt_op::<i64, i64>(&|a| a as i8 as i64)?,
                Instruction::i64Extend16Signed => self.cvt_op::<i64, i64>(&|a| a as i16 as i64)?,
                Instruction::i64Extend32Signed => self.cvt_op::<i64, i64>(&|a| a as i32 as i64)?,

                Instruction::i32Store(mem_arg) => self.store::<i32>(*mem_arg, frame)?,
                Instruction::i64Store(mem_arg) => self.store::<i64>(*mem_arg, frame)?,
                Instruction::f32Store(mem_arg) => self.store::<f32>(*mem_arg, frame)?,
                Instruction::f64Store(mem_arg) => self.store::<f64>(*mem_arg, frame)?,

                Instruction::i32Store8(mem_arg) => self.store_n::<i32, 1>(*mem_arg, frame)?,
                Instruction::i32Store16(mem_arg) => self.store_n::<i32, 2>(*mem_arg, frame)?,
                Instruction::i64Store8(mem_arg) => self.store_n::<i64, 1>(*mem_arg, frame)?,
                Instruction::i64Store16(mem_arg) => self.store_n::<i64, 2>(*mem_arg, frame)?,
                Instruction::i64Store32(mem_arg) => self.store_n::<i64, 4>(*mem_arg, frame)?,

                Instruction::i32Load(mem_arg) => self.load::<i32>(*mem_arg, frame)?,
                Instruction::i64Load(mem_arg) => self.load::<i64>(*mem_arg, frame)?,
                Instruction::f32Load(mem_arg) => self.load::<f32>(*mem_arg, frame)?,
                Instruction::f64Load(mem_arg) => self.load::<f64>(*mem_arg, frame)?,

                Instruction::i32Load8Signed(mem_arg) => {
                    self.load_n::<i32, i8>(*mem_arg, frame, Sign::Signed)?
                }
                Instruction::i32Load8Unsigned(mem_arg) => {
                    self.load_n::<i32, u8>(*mem_arg, frame, Sign::Unsigned)?
                }
                Instruction::i32Load16Signed(mem_arg) => {
                    self.load_n::<i32, i16>(*mem_arg, frame, Sign::Signed)?
                }
                Instruction::i32Load16Unsigned(mem_arg) => {
                    self.load_n::<i32, u16>(*mem_arg, frame, Sign::Unsigned)?
                }
                Instruction::i64Load8Signed(mem_arg) => {
                    self.load_n::<i64, i8>(*mem_arg, frame, Sign::Signed)?
                }
                Instruction::i64Load8Unsigned(mem_arg) => {
                    self.load_n::<i64, u8>(*mem_arg, frame, Sign::Unsigned)?
                }
                Instruction::i64Load16Signed(mem_arg) => {
                    self.load_n::<i64, i16>(*mem_arg, frame, Sign::Signed)?
                }
                Instruction::i64Load16Unsigned(mem_arg) => {
                    self.load_n::<i64, u16>(*mem_arg, frame, Sign::Unsigned)?
                }
                Instruction::i64Load32Signed(mem_arg) => {
                    self.load_n::<i64, i32>(*mem_arg, frame, Sign::Signed)?
                }
                Instruction::i64Load32Unsigned(mem_arg) => {
                    self.load_n::<i64, u32>(*mem_arg, frame, Sign::Unsigned)?
                }

                Instruction::Nop => {}
                Instruction::LocalGet(n) => self.local_get(*n, frame)?,
                Instruction::LocalSet(n) => self.local_set(*n, frame)?,
                Instruction::LocalTee(n) => self.local_tee(*n, frame)?,

                Instruction::GlobalGet(n) => self.global_get(*n, frame)?,
                Instruction::GlobalSet(n) => self.global_set(*n, frame)?,

                Instruction::Drop => self.drop()?,

                Instruction::Block(label) => self.block(*label)?,
                Instruction::Loop(label) => self.do_loop(*label)?,
                Instruction::BranchIf(n) => {
                    if let Some(new_idx) = self.branch_if(*n)? {
                        idx = new_idx;
                    }
                }
                Instruction::Branch(n) => idx = self.branch(*n)?,
                Instruction::BranchTable(table, fallback) => {
                    idx = self.branch_table(table, *fallback)?
                }
                Instruction::End => self.end()?,
                Instruction::Call(fn_idx) => self.call(*fn_idx, frame)?,
                Instruction::Return => break,

                Instruction::Unreachable => todo!(),
                Instruction::CallIndirect(_, _) => todo!(),
                Instruction::Select => self.select()?,
                Instruction::SelectT(_) => todo!(),
                Instruction::TableGet(_) => todo!(),
                Instruction::TableSet(_) => todo!(),
                Instruction::MemorySize => todo!(),
                Instruction::MemoryGrow => todo!(),

                Instruction::i32TruncF32Signed => todo!(),
                Instruction::i32TruncF32Unsigned => todo!(),
                Instruction::i32TruncF64Signed => todo!(),
                Instruction::i32TruncF64Unsigned => todo!(),

                Instruction::i64TruncF32Signed => todo!(),
                Instruction::i64TruncF32Unsigned => todo!(),
                Instruction::i64TruncF64Signed => todo!(),
                Instruction::i64TruncF64Unsigned => todo!(),

                Instruction::f32SignedConvertI32 => todo!(),
                Instruction::f32UnsignedConvertI32 => todo!(),
                Instruction::f32SignedConvertI64 => todo!(),
                Instruction::f32UnsignedConvertI64 => todo!(),
                Instruction::f64SignedConvertI32 => todo!(),
                Instruction::f64UnsignedConvertI32 => todo!(),
                Instruction::f64UnsignedConvertI64 => todo!(),

                Instruction::RefNull(_) => todo!(),
                Instruction::RefIsNull => todo!(),
                Instruction::RefFunc(_) => todo!(),

                Instruction::i32TruncSatf32Signed => todo!(),
                Instruction::i32TruncSatf32Unsigned => todo!(),
                Instruction::i32TruncSatf64Signed => todo!(),
                Instruction::i32TruncSatf64Unsigned => todo!(),
                Instruction::i64TruncSatf32Signed => todo!(),
                Instruction::i64TruncSatf32Unsigned => todo!(),
                Instruction::i64TruncSatf64Signed => todo!(),
                Instruction::i64TruncSatf64Unsigned => todo!(),

                Instruction::MemoryInit(_) => todo!(),
                Instruction::DataDrop(_) => todo!(),
                Instruction::MemoryCopy => todo!(),
                Instruction::MemoryFill => todo!(),
                Instruction::TableInit(_, _) => todo!(),
                Instruction::ElemDrop(_) => todo!(),
                Instruction::TableCopy(_, _) => todo!(),
                Instruction::TableGrow(_) => todo!(),
                Instruction::TableSize(_) => todo!(),
                Instruction::TableFill(_) => todo!(),
            }

            idx += 1;
        }

        Ok(())
    }

    fn trap(&self, message: &str) -> ! {
        todo!("{message}")
    }
}

impl<'a> Interpreter<'a> {
    fn local_get(&mut self, n: u32, frame: &Frame) -> WResult<()> {
        let val = frame
            .locals
            .get(n as usize)
            .ok_or(WasmError::TooFewLocals)?;

        self.stack.push_value(*val);

        Ok(())
    }

    fn local_set(&mut self, n: u32, frame: &mut Frame) -> WResult<()> {
        let val = self.stack.pop_value()?;

        *frame
            .locals
            .get_mut(n as usize)
            .ok_or(WasmError::TooFewLocals)? = val;

        Ok(())
    }

    fn local_tee(&mut self, n: u32, frame: &mut Frame) -> WResult<()> {
        let val = self.stack.pop_value()?;

        self.stack.push_value(val);
        self.stack.push_value(val);

        self.local_set(n, frame)?;

        Ok(())
    }

    fn global_get(&mut self, n: u32, frame: &Frame) -> WResult<()> {
        let module = &self.module_insts[frame.module_idx as usize];
        let global_addr = module.global_addrs[n as usize];

        let val = self.globals[global_addr];

        self.stack.push_value(val);

        Ok(())
    }

    fn global_set(&mut self, n: u32, frame: &mut Frame) -> WResult<()> {
        let val = self.stack.pop_value()?;

        let module = &self.module_insts[frame.module_idx as usize];
        let global_addr = module.global_addrs[n as usize];

        self.globals[global_addr] = val;

        Ok(())
    }

    fn reinterpret<From: Num, To: Num>(&mut self) -> WResult<()>
    where
        // checking To::BYTES == From::BYTES
        [(); From::BYTES - To::BYTES]: Sized,
        [(); To::BYTES - From::BYTES]: Sized,
    {
        let a = From::from_value(self.stack.pop_value()?)?;

        self.stack.push_value(To::reinterpret(a).as_value());

        Ok(())
    }

    fn call(&mut self, fn_idx: u32, frame: &Frame) -> WResult<()> {
        let module = &self.module_insts[frame.module_idx as usize];

        let addr = module.func_addrs[fn_idx as usize];

        self.invoke_function(addr)?;

        Ok(())
    }

    fn cvt_op<N: Num, N2: Num>(&mut self, op: &dyn Fn(N) -> N2) -> WResult<()> {
        let a = N::from_value(self.stack.pop_value()?)?;

        let c = op(a);

        self.stack.push_value(c.as_value());

        Ok(())
    }

    fn branch_table(&mut self, table: &[u32], fallback: u32) -> WResult<usize> {
        let i = self.stack.pop_i32()? as usize;

        if i < table.len() {
            self.branch(table[i])
        } else {
            self.branch(fallback)
        }
    }

    fn select(&mut self) -> WResult<()> {
        let c = self.stack.pop_i32()?;

        let val2 = self.stack.pop_value()?;
        let val1 = self.stack.pop_value()?;

        if val1.value_type() != val2.value_type() {
            self.trap("mismatched select value types");
        }

        if c != 0 {
            self.stack.push_value(val1);
        } else {
            self.stack.push_value(val2);
        }

        Ok(())
    }

    fn drop(&mut self) -> WResult<()> {
        self.stack.pop_value()?;

        Ok(())
    }

    fn end(&mut self) -> WResult<()> {
        let mut vals = Vec::new();

        while let Some(entry) = self.stack.pop() {
            match entry {
                StackEntry::Activation(..) | StackEntry::Value(..) => vals.push(entry),
                StackEntry::Label(..) => break,
            }
        }

        for v in vals.into_iter().rev() {
            self.stack.push(v);
        }

        Ok(())
    }

    fn branch(&mut self, nth_label: u32) -> WResult<usize> {
        let label = self.stack.find_label(nth_label).unwrap();

        let vals = self.stack.pop_n(label.arity() as usize).unwrap();

        for _ in 0..=nth_label {
            while let Some(entry) = self.stack.pop() {
                match entry {
                    StackEntry::Activation(..) | StackEntry::Value(..) => continue,
                    StackEntry::Label(..) => break,
                }
            }
        }

        for val in vals {
            self.stack.push(val);
        }

        Ok(label.continuation)
    }

    fn branch_if(&mut self, nth_label: u32) -> WResult<Option<usize>> {
        let val = self.stack.pop_i32()?;

        if val == 0 {
            return Ok(None);
        }

        Ok(Some(self.branch(nth_label)?))
    }

    fn block(&mut self, label: Label) -> WResult<()> {
        assert_eq!(label.arity(), 0);

        self.stack.push(StackEntry::Label(label));

        Ok(())
    }

    fn do_loop(&mut self, label: Label) -> WResult<()> {
        assert_eq!(label.arity(), 0);

        self.stack.push(StackEntry::Label(label));

        Ok(())
    }

    fn if_op(&mut self, curr: usize, label: Label) -> WResult<usize> {
        let cond = self.stack.pop_i32()?;

        Ok(if cond == 0 { label.continuation } else { curr })
    }

    fn bin_op<N: Num>(&mut self, op: &dyn Fn(N, N) -> N) -> WResult<()> {
        let b = N::from_value(self.stack.pop_value()?)?;
        let a = N::from_value(self.stack.pop_value()?)?;

        let c = op(a, b);

        self.stack.push_value(c.as_value());

        Ok(())
    }

    fn test_op<N: Num>(&mut self, op: &dyn Fn(N) -> bool) -> WResult<()> {
        let a = N::from_value(self.stack.pop_value()?)?;

        let c = op(a);

        self.stack.push_value(Value::I32(c as i32));

        Ok(())
    }

    fn unary_op<N: Num>(&mut self, op: &dyn Fn(N) -> N) -> WResult<()> {
        let a = N::from_value(self.stack.pop_value()?)?;

        let c = op(a);

        self.stack.push_value(c.as_value());

        Ok(())
    }

    fn rel_op<N: Num>(&mut self, op: &dyn Fn(N, N) -> bool) -> WResult<()> {
        let b = N::from_value(self.stack.pop_value()?)?;
        let a = N::from_value(self.stack.pop_value()?)?;

        let c = op(a, b);

        self.stack.push_value(Value::I32(c as i32));

        Ok(())
    }

    fn store<N: Num>(&mut self, mem_arg: MemoryOperand, frame: &Frame) -> WResult<()>
    where
        [(); N::BYTES]: Sized,
    {
        self.store_n::<N, { N::BYTES }>(mem_arg, frame)
    }

    fn store_n<N: Num, const BYTES: usize>(
        &mut self,
        mem_arg: MemoryOperand,
        frame: &Frame,
    ) -> WResult<()>
    where
        [(); N::BYTES]: Sized,
    {
        let module = &self.module_insts[frame.module_idx as usize];
        let a = module.mem_addrs[0];

        let mem = &mut self.store.mems[a];

        let c = N::from_value(self.stack.pop_value()?)?;
        let i = u32::reinterpret(self.stack.pop_i32()?);

        let ea = (i + mem_arg.offset) as usize;

        if ea + BYTES > mem.data.len() {
            self.trap(&format!("oob write at index {ea}"));
        }

        let buffer = &c.to_le_bytes()[..BYTES];

        mem.data[ea..(ea + BYTES)].copy_from_slice(buffer);

        Ok(())
    }

    fn load_buffer(&mut self, mem_arg: MemoryOperand, frame: &Frame, n: usize) -> WResult<&[u8]> {
        assert_eq!(mem_arg.offset % 2_u32.pow(mem_arg.align), 0);

        let mem = {
            let module = &self.module_insts[frame.module_idx as usize];
            let addr = module.mem_addrs[0];
            &self.store.mems[addr]
        };

        let i = self.stack.pop_i32()?;

        let ea = i as usize + mem_arg.offset as usize;

        if ea + n > mem.data.len() {
            self.trap("oob read")
        }

        let buffer = &mem.data[ea..(ea + n)];

        Ok(buffer)
    }

    fn load_n<N: Num, N2: Num>(
        &mut self,
        mem_arg: MemoryOperand,
        frame: &Frame,
        sign: Sign,
    ) -> WResult<()>
    where
        [(); N::BYTES]: Sized,
        [(); N2::BYTES]: Sized,
        // convoluted way of determining that N::BYTES > N2::BYTES
        [(); -((N::BYTES < N2::BYTES) as isize) as usize + 1]: Sized,
    {
        let buffer = self.load_buffer(mem_arg, frame, N2::BYTES)?;
        let bytes: [u8; N2::BYTES] = buffer.try_into().unwrap();

        let n = N2::from_le_bytes(bytes);

        let c = match sign {
            Sign::Unsigned => n.extend_unsigned::<N>().as_value(),
            // todo: almost certainly wrong
            Sign::Signed => n.extend_signed::<N>().as_value(),
        };

        self.stack.push_value(c);

        Ok(())
    }

    fn load<N: Num>(&mut self, mem_arg: MemoryOperand, frame: &Frame) -> WResult<()>
    where
        [(); N::BYTES]: Sized,
    {
        let buffer = self.load_buffer(mem_arg, frame, N::BYTES)?;
        let bytes: [u8; N::BYTES] = buffer.try_into().unwrap();

        self.stack.push_value(N::from_le_bytes(bytes).as_value());

        Ok(())
    }
}
