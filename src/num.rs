use crate::{error::WResult, Value};

pub trait Num<const BYTES: usize> {
    const BITS: usize = BYTES * 8;
    const BYTES: usize = BYTES;

    // todo: use Self::BYTES to avoid superfluous generic
    fn from_le_bytes(bytes: [u8; BYTES]) -> Self;
    fn to_le_bytes(self) -> [u8; BYTES];

    fn as_value(self) -> Value;
    fn from_value(val: Value) -> WResult<Self>
    where
        Self: Sized;
}

impl Num<4> for i32 {
    fn from_le_bytes(bytes: [u8; 4]) -> Self {
        i32::from_le_bytes(bytes)
    }

    fn to_le_bytes(self) -> [u8; 4] {
        self.to_le_bytes()
    }

    fn as_value(self) -> Value {
        Value::I32(self)
    }

    fn from_value(val: Value) -> WResult<Self> {
        val.assert_i32()
    }
}

impl Num<4> for u32 {
    fn from_le_bytes(bytes: [u8; 4]) -> Self {
        u32::from_le_bytes(bytes)
    }

    fn to_le_bytes(self) -> [u8; 4] {
        self.to_le_bytes()
    }

    fn as_value(self) -> Value {
        Value::I32(self as i32)
    }

    fn from_value(val: Value) -> WResult<Self> {
        Ok(val.assert_i32()? as u32)
    }
}

impl Num<8> for i64 {
    fn from_le_bytes(bytes: [u8; 8]) -> Self {
        i64::from_le_bytes(bytes)
    }

    fn to_le_bytes(self) -> [u8; 8] {
        self.to_le_bytes()
    }

    fn as_value(self) -> Value {
        Value::I64(self)
    }

    fn from_value(val: Value) -> WResult<Self> {
        val.assert_i64()
    }
}

impl Num<8> for u64 {
    fn from_le_bytes(bytes: [u8; 8]) -> Self {
        u64::from_le_bytes(bytes)
    }

    fn to_le_bytes(self) -> [u8; 8] {
        self.to_le_bytes()
    }

    fn as_value(self) -> Value {
        Value::I64(self as i64)
    }

    fn from_value(val: Value) -> WResult<Self> {
        Ok(val.assert_i64()? as u64)
    }
}
