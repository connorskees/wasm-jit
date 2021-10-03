use wasm_jit::{Value, WResult, WasmJit};

fn main() -> WResult<()> {
    let buffer = std::fs::read("./files/add.wasm").unwrap();

    let mut jit = WasmJit::new(&buffer)?;

    let res = jit.invoke_export("_Z3addii", &[Value::I32(5), Value::I32(6)])?;

    dbg!(res);

    Ok(())
}
