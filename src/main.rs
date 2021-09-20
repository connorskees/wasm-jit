use wasm_jit::{Interpreter, WResult};

fn main() -> WResult<()> {
    let buffer = std::fs::read("./null-deref.wasm").unwrap();

    let mut interpreter = Interpreter::new(&buffer)?;

    let res = interpreter.invoke_export("main", &[])?;

    dbg!(res);

    Ok(())
}
