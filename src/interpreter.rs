use std::convert::TryInto;

use crate::{
    error::{WResult, WasmError},
    instance::{FuncInst, ModuleInst},
    num::Num,
    opcode::Instruction,
    stack::{Frame, Stack, StackEntry},
    store::Store,
    Expr, MemoryOperand, Module, Value, ValueType,
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

    pub fn invoke(mut self) -> WResult<()> {
        self.stack.push_value(Value::I32(5));

        dbg!(self.invoke_function(0)?);

        Ok(())
    }

    fn invoke_function(&mut self, addr: usize) -> WResult<Value> {
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

                let expr = code.expr.clone();

                return self.invoke_expr(&expr, &mut frame);
            }
        }
    }

    fn invoke_expr(&mut self, expr: &Expr, frame: &mut Frame) -> WResult<Value> {
        for inst in &expr.0 {
            match inst {
                Instruction::LocalGet(n) => self.local_get(*n, frame)?,
                Instruction::i32Add => self.i32_add()?,
                Instruction::i32Sub => self.i32_sub()?,
                Instruction::i32Const(n) => self.stack.push_value(Value::I32(*n)),
                Instruction::i64Const(n) => self.stack.push_value(Value::I64(*n)),
                Instruction::f32Const(n) => self.stack.push_value(Value::F32(*n)),
                Instruction::f64Const(n) => self.stack.push_value(Value::F64(*n)),
                Instruction::i32Load(mem_arg) => self.load::<i32, 4>(*mem_arg, frame)?,
                Instruction::i64Load(mem_arg) => self.load::<i64, 8>(*mem_arg, frame)?,
                Instruction::i32Store(mem_arg) => self.store::<i32, 4>(*mem_arg, frame)?,
                Instruction::i64Store(mem_arg) => self.store::<i64, 8>(*mem_arg, frame)?,
                Instruction::LocalTee(n) => self.local_tee(*n, frame)?,
                Instruction::LocalSet(n) => self.local_set(*n, frame)?,
                _ => todo!("{:?}", inst),
            }
        }

        self.stack.pop_value()
    }

    fn trap(&mut self) -> ! {
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

    fn i32_add(&mut self) -> WResult<()> {
        let a = self.stack.pop_i32()?;
        let b = self.stack.pop_i32()?;

        self.stack.push_value(Value::I32(a.wrapping_add(b)));

        Ok(())
    }

    fn i32_sub(&mut self) -> WResult<()> {
        let a = self.stack.pop_i32()?;
        let b = self.stack.pop_i32()?;

        self.stack.push_value(Value::I32(a.wrapping_sub(b)));

        Ok(())
    }

    fn store<N: Num<B>, const B: usize>(
        &mut self,
        mem_arg: MemoryOperand,
        frame: &Frame,
    ) -> WResult<()> {
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

    fn load<N: Num<B>, const B: usize>(
        &mut self,
        mem_arg: MemoryOperand,
        frame: &Frame,
    ) -> WResult<()> {
        let module = &self.module_insts[frame.module_idx as usize];
        let a = module.mem_addrs[0];

        let mem = &self.store.mems[a];

        let i = self.stack.pop_i32()?;

        let ea = i as usize + mem_arg.offset as usize;

        if ea + N::BITS / 8 > mem.data.len() {
            self.trap();
        }

        let buffer = &mem.data[(ea as usize)..(ea + (N::BITS / 8))];

        let bytes: [u8; B] = buffer.try_into().unwrap();

        self.stack.push_value(N::from_le_bytes(bytes).as_value());

        Ok(())
    }
}
