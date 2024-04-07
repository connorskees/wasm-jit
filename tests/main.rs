use wasm_jit::{ImportValue, Value};

macro_rules! test {
    ($name:ident, $file:literal, $export:literal, $($args:expr => $expected:expr),*,) => {
        #[test]
        fn $name() {
            let buffer = std::fs::read($file).expect("file not found");
            let mut interpreter = wasm_jit::Interpreter::new(&buffer, &[]).expect("failed to initialize interpreter");

            $(
                let result = interpreter
                    .invoke_export($export, $args)
                    .expect("error during execution")
                    .expect("missing return value");

                assert_eq!($expected, result);
            )*
        }
    };
    ($name:ident, $file:literal, $export:literal, $($args:expr => $expected:expr),*,) => {
        #[test]
        fn $name() {
            let buffer = std::fs::read($file).unwrap();
            let mut interpreter = wasm_jit::Interpreter::new(&buffer, &[]).unwrap();

            $(
                let result = interpreter
                    .invoke_export($export, $args)
                    .unwrap()
                    .unwrap();

                assert_eq!($expected, result);
            )*
        }
    };
}

test!(
    add,
    "./files/add.wasm",
    "_Z3addii",
    &[Value::I32(5), Value::I32(7)] => Value::I32(5 + 7),
    &[Value::I32(-5), Value::I32(7)] => Value::I32(-5 + 7),
);

// todo: these files come from emscripten. for some reason they start by reading bytes from memory?
// test!(
//     factorial,
//     "./files/factorial.wasm",
//     "fact",
//     &[Value::I32(5)] => Value::F64(120.0),
//     &[Value::I32(10)] => Value::F64(3628800.0),
// );
// test!(
//     fibonacci,
//     "./files/fibonacci.wasm",
//     "fibonacci",
//     &[Value::I32(10)] => Value::I32(55),
//     &[Value::I32(20)] => Value::I32(6765),
// );

#[test]
fn array_contains() {
    let buffer = std::fs::read("./files/array-contains.wasm").unwrap();
    let mut interpreter = wasm_jit::Interpreter::new(
        &buffer,
        &[
            ("env", "__indirect_function_table", ImportValue::Table(())),
            (
                "env",
                "__linear_memory",
                ImportValue::Mem(vec![1, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0]),
            ),
        ],
    )
    .unwrap();

    let result = interpreter
        .invoke_export("contains", &[Value::I32(0), Value::I32(4), Value::I32(10)])
        .unwrap()
        .unwrap();

    assert_eq!(Value::I32(0), result);

    let result = interpreter
        .invoke_export("contains", &[Value::I32(0), Value::I32(4), Value::I32(0)])
        .unwrap()
        .unwrap();

    assert_eq!(Value::I32(0), result);

    let result = interpreter
        .invoke_export("contains", &[Value::I32(0), Value::I32(4), Value::I32(9)])
        .unwrap()
        .unwrap();

    assert_eq!(Value::I32(1), result);
}

test!(
    i32_divs,
    "./files/signed_div.wasm",
    "i32_signed_div",
    &[Value::I32(5)] => Value::I32(5 / 2),
    &[Value::I32(10)] => Value::I32(10 / 2),
);

test!(
    i64_divs,
    "./files/signed_div.wasm",
    "i64_signed_div",
    &[Value::I64(5)] => Value::I64(5 / 2),
    &[Value::I64(10)] => Value::I64(10 / 2),
);

test!(
    u32_divu,
    "./files/unsigned_div.wasm",
    "u32_unsigned_div",
    &[Value::I32(5)] => Value::I32(5 / 3),
    &[Value::I32(10)] => Value::I32(10 / 3),
);

test!(
    u64_divu,
    "./files/unsigned_div.wasm",
    "u64_unsigned_div",
    &[Value::I64(5)] => Value::I64(5 / 3),
    &[Value::I64(10)] => Value::I64(10 / 3),
);

test!(
    u32_remu,
    "./files/unsigned_rem.wasm",
    "u32_unsigned_rem",
    &[Value::I32(5)] => Value::I32(5 % 3),
    &[Value::I32(10)] => Value::I32(10 % 3),
);

test!(
    u64_remu,
    "./files/unsigned_rem.wasm",
    "u64_unsigned_rem",
    &[Value::I64(5)] => Value::I64(5 % 3),
    &[Value::I64(10)] => Value::I64(10 % 3),
);

test!(
    i32_rems,
    "./files/signed_rem.wasm",
    "i32_signed_rem",
    &[Value::I32(5)] => Value::I32(5 % 3),
    &[Value::I32(10)] => Value::I32(10 % 3),
);

test!(
    i64_rems,
    "./files/signed_rem.wasm",
    "i64_signed_rem",
    &[Value::I64(5)] => Value::I64(5 % 3),
    &[Value::I64(10)] => Value::I64(10 % 3),
);

test!(
    i32_rotl,
    "./files/rotate_left.wasm",
    "i32_rotate_left",
    &[Value::I32(5)] => Value::I32(5_i32.rotate_left(5)),
    &[Value::I32(10)] => Value::I32(10_i32.rotate_left(5)),
);

test!(
    i64_rotl,
    "./files/rotate_left.wasm",
    "i64_rotate_left",
    &[Value::I64(5)] => Value::I64(5_i64.rotate_left(5)),
    &[Value::I64(10)] => Value::I64(10_i64.rotate_left(5)),
);

test!(
    i32_reinterpret_f32,
    "./files/reinterpret.wasm",
    "i32_reinterpret_f32",
    &[Value::F32(5.0)] => Value::I32((5.0_f32).to_bits() as i32),
    &[Value::F32(-99.5454)] => Value::I32((-99.5454_f32).to_bits() as i32),
);

test!(
    f32_reinterpret_i32,
    "./files/reinterpret.wasm",
    "f32_reinterpret_i32",
    &[Value::I32((5.0_f32).to_bits() as i32)] => Value::F32(5.0),
    &[Value::I32((-99.5454_f32).to_bits() as i32)] => Value::F32(-99.5454),
);

test!(
    i64_reinterpret_f64,
    "./files/reinterpret.wasm",
    "i64_reinterpret_f64",
    &[Value::F64(5.0)] => Value::I64((5.0_f64).to_bits() as i64),
    &[Value::F64(-99.5454)] => Value::I64((-99.5454_f64).to_bits() as i64),
);

test!(
    f64_reinterpret_i64,
    "./files/reinterpret.wasm",
    "f64_reinterpret_i64",
    &[Value::I64((5.0_f64).to_bits() as i64)] => Value::F64(5.0),
    &[Value::I64((-99.5454_f64).to_bits() as i64)] => Value::F64(-99.5454),
);

// todo: find a way to codegen rotr
// test!(
//     i32_rotr,
//     "./files/rotate_right.wasm",
//     "i32_rotate_right",
//     &[Value::I32(5)] => Value::I32(5_i32.rotate_right(5)),
//     &[Value::I32(10)] => Value::I32(10_i32.rotate_right(5)),
// );
// test!(
//     i64_rotr,
//     "./files/rotate_right.wasm",
//     "i64_rotate_right",
//     &[Value::I64(5)] => Value::I64(5_i64.rotate_right(5)),
//     &[Value::I64(10)] => Value::I64(10_i64.rotate_right(5)),
// );

test!(
    i32_sub,
    "./files/sub.wasm",
    "i32_sub",
    &[Value::I32(5), Value::I32(4)] => Value::I32(1),
    &[Value::I32(10), Value::I32(-10)] => Value::I32(20),
);
test!(
    i64_sub,
    "./files/sub.wasm",
    "i64_sub",
    &[Value::I64(5), Value::I64(4)] => Value::I64(1),
    &[Value::I64(10), Value::I64(-10)] => Value::I64(20),
);

test!(
    select,
    "./files/select.wasm",
    "select",
    &[Value::I32(0)] => Value::I32(5),
    &[Value::I32(1)] => Value::I32(6),
    &[Value::I32(-1)] => Value::I32(6),
);

test!(
    i32_extend8_s,
    "./files/icvt.wasm",
    "i32_extend8_s",
    &[Value::I32(0)] => Value::I32(0),
    &[Value::I32(i8::MAX as i32)] => Value::I32(i8::MAX as i32),
    &[Value::I32(i8::MIN as i32)] => Value::I32(i8::MIN as i32),
    &[Value::I32(-1)] => Value::I32(-1),
);

test!(
    i32_extend16_s,
    "./files/icvt.wasm",
    "i32_extend16_s",
    &[Value::I32(0)] => Value::I32(0),
    &[Value::I32(i16::MAX as i32)] => Value::I32(i16::MAX as i32),
    &[Value::I32(i16::MIN as i32)] => Value::I32(i16::MIN as i32),
    &[Value::I32(-1)] => Value::I32(-1),
);

test!(
    i64_extend8_s,
    "./files/icvt.wasm",
    "i64_extend8_s",
    &[Value::I32(0)] => Value::I64(0),
    &[Value::I32(i8::MAX as i32)] => Value::I64(i8::MAX as i64),
    &[Value::I32(i8::MIN as i32)] => Value::I64(i8::MIN as i64),
    &[Value::I32(-1)] => Value::I64(-1),
);

test!(
    i64_extend16_s,
    "./files/icvt.wasm",
    "i64_extend16_s",
    &[Value::I32(0)] => Value::I64(0),
    &[Value::I32(i16::MAX as i32)] => Value::I64(i16::MAX as i64),
    &[Value::I32(i16::MIN as i32)] => Value::I64(i16::MIN as i64),
    &[Value::I32(-1)] => Value::I64(-1),
);

test!(
    i64_extend32_s,
    "./files/icvt.wasm",
    "i64_extend32_s",
    &[Value::I32(0)] => Value::I64(0),
    &[Value::I32(i32::MAX as i32)] => Value::I64(i32::MAX as i64),
    &[Value::I32(i32::MIN as i32)] => Value::I64(i32::MIN as i64),
    &[Value::I32(-1)] => Value::I64(-1),
);

test!(
    i64_extend_i32_u,
    "./files/icvt.wasm",
    "i64_extend_i32_u",
    &[Value::I32(0)] => Value::I64(0),
    &[Value::I32(i32::MAX as i32)] => Value::I64(i32::MAX as i64),
    &[Value::I32(i32::MIN as i32)] => Value::I64(2147483648),
    &[Value::I32(u32::MAX as i32)] => Value::I64(u32::MAX as i64),
    &[Value::I32(u32::MIN as i32)] => Value::I64(u32::MIN as i64),
    &[Value::I32(-1)] => Value::I64(4294967295),
);

test!(
    i32_wrap_i64,
    "./files/icvt.wasm",
    "wrap",
    &[Value::I64(1)] => Value::I32(1),
    &[Value::I64(0)] => Value::I32(0),
    &[Value::I64(-1)] => Value::I32(-1),
    &[Value::I64(i64::MAX)] => Value::I32(-1),
    &[Value::I64(i64::MIN)] => Value::I32(0),
);
