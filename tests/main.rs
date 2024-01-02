use wasm_jit::Value;

#[test]
fn add() {
    let buffer = std::fs::read("./files/add.wasm").unwrap();
    let mut interpreter = wasm_jit::Interpreter::new(&buffer, &[]).unwrap();

    let result = interpreter
        .invoke_export("_Z3addii", &[Value::I32(5), Value::I32(7)])
        .unwrap()
        .unwrap();

    assert_eq!(Value::I32(12), result);

    let result = interpreter
        .invoke_export("_Z3addii", &[Value::I32(-5), Value::I32(7)])
        .unwrap()
        .unwrap();

    assert_eq!(Value::I32(2), result);
}

#[test]
fn factorial() {
    let buffer = std::fs::read("./files/factorial.wasm").unwrap();
    let mut interpreter = wasm_jit::Interpreter::new(&buffer, &[]).unwrap();

    let result = interpreter
        .invoke_export("fact", &[Value::I32(5)])
        .unwrap()
        .unwrap();

    assert_eq!(Value::F64(120.0), result);

    let result = interpreter
        .invoke_export("fact", &[Value::I32(10)])
        .unwrap()
        .unwrap();

    assert_eq!(Value::F64(3628800.0), result);
}

#[test]
fn fibonacci() {
    let buffer = std::fs::read("./files/fibonacci.wasm").unwrap();
    let mut interpreter = wasm_jit::Interpreter::new(&buffer, &[]).unwrap();

    let result = interpreter
        .invoke_export("fibonacci", &[Value::I32(10)])
        .unwrap()
        .unwrap();

    assert_eq!(Value::I32(55), result);

    let result = interpreter
        .invoke_export("fibonacci", &[Value::I32(20)])
        .unwrap()
        .unwrap();

    assert_eq!(Value::I32(6765), result);
}
