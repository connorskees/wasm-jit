use std::convert::TryInto;

use crate::{
    error::{WResult, WasmError},
    instance::{FuncInst, ModuleInst},
    num::{Num, Sign},
    opcode::Instruction,
    stack::{Frame, Label, Stack, StackEntry},
    store::Store,
    BlockType, Expr, ExternValue, MemoryOperand, Module, Value, ValueType,
};

pub(crate) struct Interpreter<'a> {
    module_insts: Vec<ModuleInst<'a>>,
    store: Store<'a>,
    stack: Stack,
}

impl<'a> Interpreter<'a> {
    pub fn new(buffer: &'a [u8]) -> WResult<Self> {
        let module = Module::new(buffer)?;
        let mut store = Store::default();
        let stack = Stack::new();

        let module_inst = store.alloc_module(module)?;

        Ok(Self {
            module_insts: vec![module_inst],
            store,
            stack,
        })
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

        self.invoke_function(func_addr)?;

        Ok(if self.stack.is_empty() {
            None
        } else {
            Some(self.stack.pop_value()?)
        })
    }

    fn invoke_function(&mut self, addr: usize) -> WResult<()> {
        let func_inst = &self.store.funcs[addr];

        match func_inst {
            FuncInst::Host { .. } => todo!(),
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

                self.invoke_expr(&expr, &mut frame)?;
            }
        }

        Ok(())
    }

    fn invoke_expr(&mut self, expr: &Expr, frame: &mut Frame) -> WResult<()> {
        let mut idx = 0;

        while idx < expr.0.len() {
            match &expr.0[idx] {
                Instruction::Nop => {}
                Instruction::LocalGet(n) => self.local_get(*n, frame)?,
                Instruction::LocalSet(n) => self.local_set(*n, frame)?,
                Instruction::LocalTee(n) => self.local_tee(*n, frame)?,

                Instruction::i32Add => self.bin_op::<i32>(&|a, b| b.wrapping_add(a))?,
                Instruction::i64Add => self.bin_op::<i64>(&|a, b| b.wrapping_add(a))?,
                Instruction::i32Sub => self.bin_op::<i32>(&|a, b| b.wrapping_sub(a))?,
                Instruction::i64Sub => self.bin_op::<i64>(&|a, b| b.wrapping_sub(a))?,
                Instruction::i32Mul => self.bin_op::<i32>(&|a, b| b.wrapping_mul(a))?,
                Instruction::i64Mul => self.bin_op::<i64>(&|a, b| b.wrapping_mul(a))?,
                Instruction::f32Add => self.bin_op::<f32>(&|a, b| b + a)?,
                Instruction::f64Add => self.bin_op::<f64>(&|a, b| b + a)?,
                Instruction::f32Sub => self.bin_op::<f32>(&|a, b| b - a)?,
                Instruction::f64Sub => self.bin_op::<f64>(&|a, b| b - a)?,
                Instruction::f32Mul => self.bin_op::<f32>(&|a, b| b * a)?,
                Instruction::f64Mul => self.bin_op::<f64>(&|a, b| b * a)?,
                Instruction::f32Div => self.bin_op::<f32>(&|a, b| b / a)?,
                Instruction::f64Div => self.bin_op::<f64>(&|a, b| b / a)?,
                Instruction::i32BitwiseAnd => self.bin_op::<i32>(&|a, b| b & a)?,
                Instruction::i64BitwiseAnd => self.bin_op::<i64>(&|a, b| b & a)?,
                Instruction::i32BitwiseOr => self.bin_op::<i32>(&|a, b| b | a)?,
                Instruction::i64BitwiseOr => self.bin_op::<i64>(&|a, b| b | a)?,
                Instruction::i32BitwiseXor => self.bin_op::<i32>(&|a, b| b ^ a)?,
                Instruction::i64BitwiseXor => self.bin_op::<i64>(&|a, b| b ^ a)?,

                Instruction::i32Const(n) => self.stack.push_value(Value::I32(*n)),
                Instruction::i64Const(n) => self.stack.push_value(Value::I64(*n)),
                Instruction::f32Const(n) => self.stack.push_value(Value::F32(*n)),
                Instruction::f64Const(n) => self.stack.push_value(Value::F64(*n)),

                Instruction::i32Load(mem_arg) => self.load::<i32>(*mem_arg, frame)?,
                Instruction::i64Load(mem_arg) => self.load::<i64>(*mem_arg, frame)?,
                Instruction::f32Load(mem_arg) => self.load::<f32>(*mem_arg, frame)?,
                Instruction::f64Load(mem_arg) => self.load::<f64>(*mem_arg, frame)?,

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

                Instruction::f32Lt => self.rel_op::<f32>(&|a, b| a < b)?,
                Instruction::f64Lt => self.rel_op::<f64>(&|a, b| a < b)?,
                Instruction::f32Le => self.rel_op::<f32>(&|a, b| a <= b)?,
                Instruction::f64Le => self.rel_op::<f64>(&|a, b| a <= b)?,
                Instruction::f32Gt => self.rel_op::<f32>(&|a, b| a > b)?,
                Instruction::f64Gt => self.rel_op::<f64>(&|a, b| a > b)?,
                Instruction::f32Ge => self.rel_op::<f32>(&|a, b| a >= b)?,
                Instruction::f64Ge => self.rel_op::<f64>(&|a, b| a >= b)?,
                Instruction::f32Eq => self.rel_op::<f32>(&|a, b| a == b)?,
                Instruction::f64Eq => self.rel_op::<f64>(&|a, b| a == b)?,
                Instruction::f32Ne => self.rel_op::<f32>(&|a, b| a != b)?,
                Instruction::f64Ne => self.rel_op::<f64>(&|a, b| a != b)?,

                Instruction::i32Store(mem_arg) => self.store::<i32>(*mem_arg, frame)?,
                Instruction::i64Store(mem_arg) => self.store::<i64>(*mem_arg, frame)?,
                Instruction::f32Store(mem_arg) => self.store::<f32>(*mem_arg, frame)?,
                Instruction::f64Store(mem_arg) => self.store::<f64>(*mem_arg, frame)?,

                Instruction::i64Load32Signed(mem_arg) => {
                    self.load_n::<i64, i32>(*mem_arg, frame, Sign::Signed)?
                }

                Instruction::Drop => self.drop()?,

                Instruction::Block(label) => self.block(*label)?,
                Instruction::Loop(label) => self.do_loop(*label)?,
                Instruction::BranchIf(n) => {
                    if let Some(new_idx) = self.branch_if(*n)? {
                        idx = new_idx;
                    }
                }
                Instruction::Branch(n) => idx = self.branch(*n)?,
                Instruction::f64SignedConvertI64 => self.f64_signed_convert_i64()?,
                Instruction::f64PromoteF32 => self.f64_promote_f32()?,
                Instruction::f32DemoteF64 => self.f32_demote_f64()?,
                Instruction::i32ReinterpretF32 => self.i32_reinterpret_f32()?,
                Instruction::End => {}
                Instruction::Call(fn_idx) => self.call(*fn_idx, frame)?,
                Instruction::Return => break,
                inst => todo!("{:?}", inst),
            }

            idx += 1;
        }

        Ok(())
    }

    fn trap(&self) -> ! {
        todo!()
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

    fn f64_signed_convert_i64(&mut self) -> WResult<()> {
        let a = self.stack.pop_i64()?;

        self.stack.push_value(Value::F64(a as f64));

        Ok(())
    }

    fn f64_promote_f32(&mut self) -> WResult<()> {
        let a = self.stack.pop_f32()?;

        self.stack.push_value(Value::F64(a as f64));

        Ok(())
    }

    fn f32_demote_f64(&mut self) -> WResult<()> {
        let a = self.stack.pop_f64()?;

        self.stack.push_value(Value::F32(a as f32));

        Ok(())
    }

    fn i32_reinterpret_f32(&mut self) -> WResult<()> {
        let a = self.stack.pop_f32()?;

        self.stack
            .push_value(Value::I32(i32::from_le_bytes(a.to_le_bytes())));

        Ok(())
    }

    fn call(&mut self, fn_idx: u32, frame: &Frame) -> WResult<()> {
        let module = &self.module_insts[frame.module_idx as usize];

        let addr = module.func_addrs[fn_idx as usize];

        self.invoke_function(addr)?;

        Ok(())
    }

    fn drop(&mut self) -> WResult<()> {
        self.stack.pop_value()?;

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

        self.stack.push(StackEntry::Label(label));

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

    fn bin_op<N: Num>(&mut self, op: &dyn Fn(N, N) -> N) -> WResult<()> {
        let b = N::from_value(self.stack.pop_value()?)?;
        let a = N::from_value(self.stack.pop_value()?)?;

        let c = op(a, b);

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
        let module = &self.module_insts[frame.module_idx as usize];
        let a = module.mem_addrs[0];

        let mem = &mut self.store.mems[a];

        let c = N::from_value(self.stack.pop_value()?)?;

        let i = self.stack.pop_i32()?;

        let ea = (i + mem_arg.offset as i32) as usize;

        let n = N::BITS;

        if ea + n / 8 > mem.data.len() {
            self.trap();
        }

        let buffer = c.to_le_bytes();

        mem.data[(ea as usize)..(ea + (N::BITS / 8))].copy_from_slice(&buffer);

        Ok(())
    }

    fn load_buffer(&mut self, mem_arg: MemoryOperand, frame: &Frame, n: usize) -> WResult<&[u8]> {
        let mem = {
            let module = &self.module_insts[frame.module_idx as usize];
            let addr = module.mem_addrs[0];
            &self.store.mems[addr]
        };

        let i = self.stack.pop_i32()?;

        let ea = i as usize + mem_arg.offset as usize;

        if ea + n / 8 > mem.data.len() {
            self.trap()
        }

        let buffer = &mem.data[(ea as usize)..(ea + (n / 8))];

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
        [(); -((N::BYTES < N2::BYTES) as isize) as usize + 1]: Sized,
    {
        let buffer = self.load_buffer(mem_arg, frame, N2::BITS)?;
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
        let buffer = self.load_buffer(mem_arg, frame, N::BITS)?;
        let bytes: [u8; N::BYTES] = buffer.try_into().unwrap();

        self.stack.push_value(N::from_le_bytes(bytes).as_value());

        Ok(())
    }
}
