use std::mem;

use crate::{
    error::{WResult, WasmError},
    instance::{FuncInst, ModuleInst},
    opcode::Instruction,
    stack::Stack,
    store::Store,
    Expr, ExternValue, Module, Ref, Value, ValueType,
};

use arm_assembler::Register as ArmRegister;
use keystone::{Arch, Keystone};

pub struct WasmJit<'a> {
    module_insts: Vec<ModuleInst<'a>>,
    store: Store<'a>,
    stack: Stack,
}

struct Assembler {
    assembly: String,
    inst_count: usize,
    engine: Keystone,
}

enum ArgInst {
    Mov(u8),
    Push,
}

// calling convention:
//  args: x0-x7 passed in
//   rest: ldr x0, [sp, #0]
//  ret: x0
impl Assembler {
    pub fn new() -> Self {
        let mut assembler = Self {
            assembly: String::new(),
            inst_count: 0,
            engine: Keystone::from(Arch::Arm64, keystone::Mode::LittleEndian).unwrap(),
        };

        assembler.add_instruction("mov x27, sp");
        assembler.add_instruction("mov x28, sp");

        assembler
    }

    pub fn with_args(args: &[Value]) -> Self {
        let mut assembler = Self::new();

        for (idx, &val) in args.iter().enumerate() {
            let base = assembler.pass_arg(idx);

            let arg = match val {
                Value::I32(v) => format!("#{v}"),
                Value::I64(v) => format!("#{v}"),
                Value::F32(..) | Value::F64(..) => todo!(),
                Value::Ref(Ref::Null) => "#0".to_string(),
                Value::Ref(Ref::Extern(v) | Ref::FuncAddr(v)) => format!("#{v}"),
            };

            match base {
                ArgInst::Mov(reg) => assembler.add_instruction(&format!("mov x{reg}, {arg}")),
                ArgInst::Push => {
                    assembler.add_instruction(&format!("mov x8, {arg}"));
                    assembler.push(ArmRegister::X8);
                }
            }
        }

        assembler
    }

    fn pass_arg(&mut self, idx: usize) -> ArgInst {
        match idx {
            0..=7 => ArgInst::Mov(idx as u8),
            _ => ArgInst::Push,
        }
    }

    pub fn push(&mut self, register: ArmRegister) {
        self.add_instruction("sub sp, x28, #8");
        self.add_instruction(&format!("str {register}, [x28, #-8]!"));
    }

    pub fn pop(&mut self, register: ArmRegister) {
        self.add_instruction(&format!("ldr {register}, [x28], #8"));
    }

    pub fn add_instruction(&mut self, inst: &str) {
        self.assembly.push('\n');
        self.assembly.push_str(inst);
        self.inst_count += 1;
    }

    pub fn finish<T>(mut self) -> extern "C" fn() -> T {
        self.add_instruction("mov sp, x27");
        self.add_instruction("ret");

        let mut mmap = memmap::MmapMut::map_anon(self.inst_count * 4).unwrap();

        let res = self
            .engine
            .asm(&self.assembly, mmap.as_ptr() as u64)
            .unwrap();

        let bytes = res.encoding();
        (&mut mmap[..bytes.len() as usize]).copy_from_slice(bytes);
        let mmap = mmap.make_exec().unwrap();

        let ptr = mmap.as_ptr() as *const ();
        let code: extern "C" fn() -> T = unsafe { std::mem::transmute(ptr) };

        mem::forget(mmap);

        self.assembly.clear();

        code
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
        let mut assembler = Assembler::with_args(&[
            Value::I32(0),
            Value::I32(1),
            Value::I32(2),
            Value::I32(3),
            Value::I32(4),
            Value::I32(5),
            Value::I32(6),
            Value::I32(7),
            Value::I32(8),
            Value::I32(9),
            Value::I32(10),
            Value::I32(11),
            Value::I32(12),
            Value::I32(13),
        ]);

        // assembler.add_instruction("mov x28, sp");
        // assembler.add_instruction("sub sp, x28, #16");
        // assembler.add_instruction("str x2, [x28, #-8]!");
        // assembler.add_instruction("mov x0, x1");

        // assembler.add_instruction("add sp, sp, #16");

        // assembler.add_instruction("mov x0, x5");
        // assembler.add_instruction("ldr x0, [sp, #4]");
        // assembler.add_instruction("str #4, [sp, #0]");

        // dbg!(&assembler.assembly);

        // assembler.pop(0);
        // assembler.pop(0);
        // assembler.pop(0);

        let func = assembler.finish::<i32>();

        dbg!(func());

        // mov x1, #0
        // bytes.extend_from_slice(&0b11010010100000000000000000000001_u32.to_le_bytes());
        // mov x0, #0
        // bytes.extend_from_slice(&0b11010010100000000000000000000000_u32.to_le_bytes());
        // ret
        // bytes.extend_from_slice(&0b11010110010111110000001111000000_u32.to_le_bytes());
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
        let mut assembler = Assembler::with_args(args);

        for inst in expr.0 {
            match inst {
                Instruction::LocalGet(n) => {
                    assembler.push(ArmRegister::from_int(n));
                }
                Instruction::i32Add => {
                    assembler.pop(ArmRegister::X9);
                    assembler.pop(ArmRegister::X10);
                    assembler.add_instruction("add x11, x10, x9");
                    assembler.push(ArmRegister::X11);
                }
                Instruction::End => assembler.pop(ArmRegister::X0),
                i => todo!("{:?}", i),
            }
        }

        match return_type {
            ValueType::I32 => {
                let func = assembler.finish::<i32>();

                self.stack.push_value(Value::I32(func()));
            }
            ValueType::I64 => {
                let func = assembler.finish::<i64>();

                self.stack.push_value(Value::I64(func()));
            }
            ValueType::F32 => {
                let func = assembler.finish::<f32>();

                self.stack.push_value(Value::F32(func()));
            }
            ValueType::F64 => {
                let func = assembler.finish::<f64>();

                self.stack.push_value(Value::F64(func()));
            }
            _ => todo!(),
        }

        Ok(())
    }
}
