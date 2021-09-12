use crate::{
    error::WResult,
    instance::{
        DataInst, ElemInst, ExportInst, FuncInst, GlobalInst, MemInst, ModuleInst, TableInst,
    },
    ExportDescription, ExternValue, FuncType, Function, GlobalType, MemoryType, Module, Ref,
    TableType, Value,
};

use std::borrow::Cow;

#[derive(Debug, Default)]
pub struct Store<'a> {
    pub funcs: Vec<FuncInst>,
    tables: Vec<TableInst>,
    pub mems: Vec<MemInst>,
    globals: Vec<GlobalInst>,
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

    fn alloc_host_fn(&mut self, func_type: FuncType) -> WResult<usize> {
        let addr = self.next_free_fn_address();

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

    fn alloc_global(&mut self, global_type: GlobalType, val: Value) -> usize {
        let addr = self.next_free_global_address();

        let global_inst = GlobalInst {
            ty: global_type,
            value: val,
        };

        self.globals.push(global_inst);

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

    pub fn alloc_module(&mut self, module: Module<'a>) -> WResult<ModuleInst<'a>> {
        let mut module_inst = ModuleInst {
            types: module.types,
            ..ModuleInst::init()
        };

        module_inst.func_addrs = vec![0];

        module_inst.func_addrs.append(
            &mut module
                .funcs
                .into_iter()
                .map(|func| self.alloc_fn(func, &module_inst, 0))
                .collect::<WResult<_>>()?,
        );

        module_inst.table_addrs = module
            .tables
            .into_iter()
            .map(|table| self.alloc_table(table, Ref::Null))
            .collect();

        module_inst.mem_addrs = module
            .mems
            .into_iter()
            .map(|mem| self.alloc_memory(mem))
            .collect();

        module_inst.global_addrs = module
            .globals
            .into_iter()
            .map(|global| self.alloc_global(global.global_type, todo!()))
            .collect();

        assert!(module.elems.is_empty());

        module_inst.data_addrs = module
            .data
            .into_iter()
            // todo: do we need this clone
            .map(|data| self.alloc_data_segment(data.init.to_vec()))
            .collect();

        module_inst.exports = module
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
            .collect();

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
