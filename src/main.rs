use wasm_jit::{Interpreter, Value, WResult, WasmJit};

fn main() -> WResult<()> {
    let buffer = std::fs::read("./files/add.wasm").unwrap();

    let mut jit = WasmJit::new(&buffer)?;
    let mut interpreter = Interpreter::new(&buffer)?;

    let res_jit = jit.invoke_export("_Z3addii", &[Value::I32(45), Value::I32(6)])?;
    let res_int = interpreter.invoke_export("_Z3addii", &[Value::I32(45), Value::I32(6)])?;

    assert_eq!(res_jit, res_int);

    dbg!(res_jit);

    Ok(())
}
