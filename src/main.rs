use wasm_jit::{ImportDescription, ImportValue, Interpreter, Limit, Ref, Value, WResult, WasmJit};

fn main() -> WResult<()> {
    let buffer = std::fs::read("./foo.wasm").unwrap();

    // (import "env" "__linear_memory" (memory (;0;) 1))
    // (import "env" "__stack_pointer" (global (;0;) (mut i32)))
    // (import "env" "printf" (func (;0;) (type 0)))
    // (import "env" "__indirect_function_table" (table (;0;) 0 funcref))

    // let mut jit = WasmJit::new(&buffer)?;
    let mut interpreter = Interpreter::new(
        &buffer,
        &[
            (
                "env",
                "__linear_memory",
                ImportValue::Mem(vec![1, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0]),
            ),
            ("env", "__indirect_function_table", ImportValue::Table(())),
        ],
    )?;

    dbg!(interpreter.invoke_export("twoSum", &[Value::I32(0), Value::I32(4), Value::I32(11)]))?;
    // dbg!(interpreter.invoke_export("main", &[Value::I32(5)]))?;

    // let res_jit = jit.invoke_export("_Z3addii", &[Value::I32(45), Value::I32(6)])?;
    // let res_int = interpreter.invoke_export("_Z3addii", &[Value::I32(45), Value::I32(6)])?;

    // assert_eq!(res_jit, res_int);

    // dbg!(res_jit);

    Ok(())
}
