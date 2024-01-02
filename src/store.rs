use crate::{
    error::WResult,
    instance::{
        DataInst, ElemInst, ExportInst, FuncInst, GlobalInst, MemInst, ModuleInst, TableInst,
    },
    opcode::Instruction,
    ElementMode, ElementSegment, ExportDescription, Expr, ExternValue, Function, Global,
    ImportDescription, ImportValue, MemoryType, Module, Ref, TableType,
};

use std::borrow::Cow;

#[derive(Debug, Default)]
pub struct Store<'a> {
    pub funcs: Vec<FuncInst>,
    tables: Vec<TableInst>,
    pub mems: Vec<MemInst>,
    pub globals: Vec<GlobalInst>,
    elems: Vec<ElemInst>,
    data: Vec<DataInst<'a>>,
}

impl<'a> Store<'a> {
    fn alloc_fn(
        &mut self,
        func: Function,
        module_inst: &ModuleInst<'_>,
        module_idx: u32,
    ) -> WResult<usize> {
        let addr = self.next_free_fn_address();

        // todo: can we store ref to func type, or remove vec
        let func_type = module_inst.types[func.type_idx as usize].clone();

        let func_inst = FuncInst::Local {
            ty: func_type,
            module: module_idx,
            code: func,
        };

        self.funcs.push(func_inst);

        Ok(addr)
    }

    fn alloc_host_fn(
        &mut self,
        func_type_idx: u32,
        module_inst: &ModuleInst<'_>,
    ) -> WResult<usize> {
        let addr = self.next_free_fn_address();

        // todo: can we store ref to func type, or remove vec
        let func_type = module_inst.types[func_type_idx as usize].clone();

        let func_inst = FuncInst::Host {
            ty: func_type,
            host_code: Vec::new(),
        };

        self.funcs.push(func_inst);

        Ok(addr)
    }

    fn alloc_table(&mut self, table_type: TableType, elem_ref: Ref) -> usize {
        let addr = self.next_free_table_address();

        let table_inst = TableInst {
            ty: table_type,
            elem: vec![elem_ref; table_type.limit.min as usize],
        };

        self.tables.push(table_inst);

        addr
    }

    fn alloc_memory(&mut self, mem_type: MemoryType) -> usize {
        let addr = self.next_free_mem_address();

        let mem_inst = MemInst {
            ty: mem_type,
            data: vec![0; mem_type.lim.min as usize * 64 * 1024 * 1024],
        };

        self.mems.push(mem_inst);

        addr
    }

    fn alloc_global(&mut self, global: Global) -> usize {
        let addr = self.next_free_global_address();

        let global_inst = GlobalInst {
            ty: global.global_type,
            value: global.init,
        };

        self.globals.push(global_inst);

        addr
    }

    fn alloc_elem(&mut self, elem: ElementSegment) -> usize {
        let addr = self.next_free_global_address();

        let elem_inst = ElemInst {
            ty: elem.ty,
            elem: match elem.mode {
                ElementMode::Passive => todo!(),
                ElementMode::Active { table_idx, offset } => {
                    let _table = &self.tables[table_idx as usize];
                    drop(offset);
                    // todo: implement this
                    Vec::new()
                }
                ElementMode::Declarative => todo!(),
            },
        };

        self.elems.push(elem_inst);

        addr
    }

    fn alloc_data_segment(&mut self, bytes: Vec<u8>) -> usize {
        let addr = self.next_free_data_address();

        let data_inst = DataInst {
            data: Cow::Owned(bytes),
        };

        self.data.push(data_inst);

        addr
    }

    pub fn alloc_module(
        &mut self,
        module: Module<'a>,
        imports: &[(&str, &str, ImportValue)],
    ) -> WResult<ModuleInst<'a>> {
        let mut module_inst = ModuleInst {
            types: module.types,
            ..ModuleInst::init()
        };

        for import in module.imports {
            let value = imports
                .iter()
                .find(|(module, name, _)| import.mod_name == *module && import.name == *name)
                .map(|(_, _, val)| val)
                .ok_or_else(|| {
                    anyhow::anyhow!("missing import for {}.{}", import.mod_name, import.name)
                })?;

            match (import.description, value) {
                (ImportDescription::Func(func), ImportValue::Func(_)) => {
                    module_inst
                        .func_addrs
                        .push(self.alloc_host_fn(func, &module_inst)?);
                }
                (ImportDescription::Table(table_ty), ImportValue::Table(_)) => {
                    // todo: init table, ref?
                    module_inst
                        .table_addrs
                        .push(self.alloc_table(table_ty, Ref::Null));
                }
                (ImportDescription::Mem(mem), ImportValue::Mem(init)) => {
                    // todo: init memory
                    let idx = self.alloc_memory(mem);
                    assert!(mem.lim.max.unwrap_or(u32::MAX) >= init.len() as u32);

                    if init.len() > self.mems[idx].data.len() {
                        self.mems[idx].data = init.to_vec();
                    } else {
                        self.mems[idx].data[..init.len()].copy_from_slice(init);
                    }

                    module_inst.mem_addrs.push(idx);
                }
                (ImportDescription::Global(ty), ImportValue::Global(_)) => {
                    module_inst.global_addrs.push(self.alloc_global(Global {
                        global_type: ty,
                        // todo: initialize function here
                        init: Expr(vec![Instruction::i32Const(0), Instruction::End]),
                    }))
                }
                _ => anyhow::bail!(
                    "invalid import value: expected {:?} for {}.{}",
                    import.description,
                    import.mod_name,
                    import.name
                ),
            }
        }

        module_inst.func_addrs.append(
            &mut module
                .funcs
                .into_iter()
                .map(|func| self.alloc_fn(func, &module_inst, 0))
                .collect::<WResult<_>>()?,
        );

        module_inst.table_addrs.append(
            &mut module
                .tables
                .into_iter()
                .map(|table| self.alloc_table(table, Ref::Null))
                .collect(),
        );

        module_inst.mem_addrs.append(
            &mut module
                .mems
                .into_iter()
                .map(|mem| self.alloc_memory(mem))
                .collect(),
        );

        module_inst.global_addrs.append(
            &mut module
                .globals
                .into_iter()
                .map(|global| self.alloc_global(global))
                .collect(),
        );

        module_inst.elem_addrs.append(
            &mut module
                .elems
                .into_iter()
                .map(|elem| self.alloc_elem(elem))
                .collect(),
        );

        module_inst.data_addrs.append(
            &mut module
                .data
                .into_iter()
                // todo: do we need this clone
                .map(|data| self.alloc_data_segment(data.init.to_vec()))
                .collect(),
        );

        module_inst.exports.append(
            &mut module
                .exports
                .into_iter()
                .map(|export| ExportInst {
                    name: export.name,
                    value: match export.description {
                        ExportDescription::FuncIdx(idx) => {
                            ExternValue::Func(module_inst.func_addrs[idx as usize])
                        }
                        ExportDescription::MemIdx(idx) => {
                            ExternValue::Mem(module_inst.mem_addrs[idx as usize])
                        }
                        ExportDescription::GlobalIdx(idx) => {
                            ExternValue::Global(module_inst.global_addrs[idx as usize])
                        }
                        ExportDescription::TableIdx(idx) => {
                            ExternValue::Table(module_inst.table_addrs[idx as usize])
                        }
                    },
                })
                .collect(),
        );

        Ok(module_inst)
    }

    fn next_free_fn_address(&mut self) -> usize {
        self.funcs.len()
    }

    fn next_free_table_address(&mut self) -> usize {
        self.tables.len()
    }

    fn next_free_mem_address(&mut self) -> usize {
        self.mems.len()
    }

    fn next_free_global_address(&mut self) -> usize {
        self.globals.len()
    }

    fn next_free_data_address(&mut self) -> usize {
        self.data.len()
    }
}
