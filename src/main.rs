use wasm_jit::{Interpreter, Value, WResult, WasmJit};

fn main() -> WResult<()> {
    let buffer = std::fs::read("./files/7f0c27a069562e85579b.module.wasm").unwrap();

    let mut jit = WasmJit::new(&buffer)?;
    let mut interpreter = Interpreter::new(&buffer)?;

    dbg!(interpreter.invoke_export("d", &[Value::I32(0), Value::I32(0), Value::I32(0)]))?;

    // let res_jit = jit.invoke_export("_Z3addii", &[Value::I32(45), Value::I32(6)])?;
    // let res_int = interpreter.invoke_export("_Z3addii", &[Value::I32(45), Value::I32(6)])?;

    // assert_eq!(res_jit, res_int);

    // dbg!(res_jit);

    Ok(())
}
