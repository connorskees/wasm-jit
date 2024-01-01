use std::mem;

use iced_x86::code_asm::*;

use crate::{
    error::{WResult, WasmError},
    instance::{FuncInst, ModuleInst},
    opcode::Instruction,
    stack::Stack,
    store::Store,
    Expr, ExternValue, Module, Value, ValueType,
};

pub struct WasmJit<'a> {
    module_insts: Vec<ModuleInst<'a>>,
    store: Store<'a>,
    stack: Stack,
}

struct Assembler {
    engine: CodeAssembler,
}

enum ArgInst {
    Mov(u8),
    Push,
}

impl Assembler {
    pub fn new() -> WResult<Self> {
        use iced_x86::code_asm::*;

        let engine = CodeAssembler::new(64).unwrap();
        let mut assembler = Self { engine };

        // function prologue
        assembler.engine.push(rbp)?;
        assembler.engine.mov(rbp, rsp)?;
        assembler.engine.sub(rsp, 1000)?;

        Ok(assembler)
    }

    pub fn with_args(args: &[Value]) -> WResult<Self> {
        let mut assembler = Self::new()?;
        let order_64 = [rdi, rsi, rdx, rcx, r8, r9];
        let order_32 = [edi, esi, edx, ecx, r8d, r9d];

        for (idx, &val) in args.iter().enumerate() {
            match val {
                Value::I64(v) if idx < 6 => assembler.engine.mov(order_64[idx], v)?,
                Value::I32(v) if idx < 6 => assembler.engine.mov(order_32[idx], v)?,
                Value::I64(v) => {
                    assembler.engine.mov(r10, v)?;
                    assembler.engine.push(r10)?;
                }
                Value::I32(v) => assembler.engine.push(v)?,
                _ => todo!(),
            }
        }

        Ok(assembler)
    }

    pub fn finish<T>(mut self) -> WResult<extern "C" fn() -> T> {
        // function epilogue
        self.engine.mov(rsp, rbp)?;
        self.engine.pop(rbp)?;
        self.engine.ret()?;

        let bytes = self.engine.assemble(0x0).unwrap();

        let mut mmap = memmap::MmapMut::map_anon(bytes.len()).unwrap();

        (&mut mmap[..bytes.len() as usize]).copy_from_slice(&bytes);
        let mmap = mmap.make_exec().unwrap();

        let ptr = mmap.as_ptr() as *const ();
        let code: extern "C" fn() -> T = unsafe { std::mem::transmute(ptr) };

        mem::forget(mmap);

        Ok(code)
    }
}

impl<'a> WasmJit<'a> {
    pub fn new(buffer: &'a [u8]) -> WResult<Self> {
        let module = Module::new(buffer)?;
        let mut store = Store::default();
        let stack = Stack::new();

        let module_inst = store.alloc_module(module)?;

        Self::test();

        Ok(Self {
            module_insts: vec![module_inst],
            store,
            stack,
        })
    }

    /// A function to test ad hoc assembly
    #[allow(warnings)]
    fn test() {
        let mut assembler = Assembler::with_args(&[Value::I32(1), Value::I32(2)]).unwrap();

        assembler.engine.mov(rax, 5_i64);

        let func = assembler.finish::<i32>().unwrap();

        dbg!(func());
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

        self.invoke_function(func_addr, args)?;

        Ok(if self.stack.is_empty() {
            None
        } else {
            Some(self.stack.pop_value()?)
        })
    }

    fn invoke_function(&mut self, addr: usize, args: &[Value]) -> WResult<()> {
        let func_inst = &self.store.funcs[addr];

        match func_inst {
            FuncInst::Host { .. } => todo!(),
            FuncInst::Local { ty, code, .. } => {
                assert_eq!(ty.number_of_args(), args.len());

                let expr = code.expr.clone();

                let return_type = (ty.1).0[0];

                self.jit_expr(expr, args, return_type).unwrap();
            }
        }

        Ok(())
    }

    fn jit_expr(&mut self, expr: Expr, args: &[Value], return_type: ValueType) -> WResult<()> {
        let mut assembler = Assembler::with_args(args)?;

        for inst in expr.0 {
            match inst {
                Instruction::LocalGet(n) => {
                    assembler.engine.push(match n {
                        0 => rdi,
                        1 => rsi,
                        2 => rdx,
                        _ => todo!("{n}"),
                    })?;
                }
                Instruction::i32Add => {
                    assembler.engine.pop(r9)?;
                    assembler.engine.pop(r10)?;
                    assembler.engine.xor(r11, r11)?;
                    assembler.engine.add(r11, r9)?;
                    assembler.engine.add(r11, r10)?;
                    assembler.engine.push(r11)?;
                }
                Instruction::End => assembler.engine.pop(rax)?,
                i => todo!("{:?}", i),
            }
        }

        let ret_val = match return_type {
            ValueType::I32 => {
                let func = assembler.finish::<i32>()?;

                Value::I32(func())
            }
            ValueType::I64 => {
                let func = assembler.finish::<i64>()?;

                Value::I64(func())
            }
            ValueType::F32 => {
                let func = assembler.finish::<f32>()?;

                Value::F32(func())
            }
            ValueType::F64 => {
                let func = assembler.finish::<f64>()?;

                Value::F64(func())
            }
            _ => todo!(),
        };

        self.stack.push_value(ret_val);

        Ok(())
    }
}
