use wasm_jit::{ImportValue, Interpreter, Value, WResult};

fn main() -> WResult<()> {
    let buffer = std::fs::read(
        // "/root/wasm-jit/files/array-contains.wasm",
        "/root/wasm-jit/test-project/target/wasm32-unknown-unknown/debug/test_project.wasm",
    )
    .unwrap();
    // let buffer = std::fs::read("./foo.wasm").unwrap();

    let mut interpreter = Interpreter::new(
        &buffer,
        &[
            ("env", "__linear_memory", ImportValue::Mem(vec![0; 5000])),
            ("env", "__indirect_function_table", ImportValue::Table(())),
        ],
    )?;

    assert_eq!(
        interpreter
            .invoke_export("nested_br", &[Value::I64(0)])
            .unwrap()
            .unwrap(),
        Value::I32(6),
    );

    // let res_jit = jit.invoke_export("_Z3addii", &[Value::I32(45), Value::I32(6)])?;
    // let res_int = interpreter.invoke_export("_Z3addii", &[Value::I32(45), Value::I32(6)])?;

    // assert_eq!(res_jit, res_int);

    // dbg!(res_jit);

    Ok(())
}
