use crate::{error::WResult, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sign {
    Signed,
    Unsigned,
}

pub trait Num: Copy + std::fmt::Debug {
    const BYTES: usize;
    const BITS: usize = Self::BYTES * 8;
    type Signed: Num;

    fn from_le_bytes(bytes: [u8; Self::BYTES]) -> Self
    where
        [(); Self::BYTES]: Sized;

    fn to_le_bytes(self) -> [u8; Self::BYTES]
    where
        [(); Self::BYTES]: Sized;

    fn as_value(self) -> Value;
    fn from_value(val: Value) -> WResult<Self>
    where
        Self: Sized;

    fn signed(self) -> Self::Signed;

    /// Reinterprets `N` as `self` where `N` is the same size as `self`
    fn reinterpret<N: Num>(n: N) -> Self
    where
        // checking Self::BYTES == N::BYTES
        [(); Self::BYTES - N::BYTES]: Sized,
        [(); N::BYTES - Self::BYTES]: Sized,
    {
        debug_assert_eq!(Self::BYTES, N::BYTES);

        // todo: use faster algorithm here, we just want to reinterpret bytes
        // but it's hard to tell the compiler that
        let mut buffer = [0; Self::BYTES];

        buffer[..].copy_from_slice(&n.to_le_bytes());

        Self::from_le_bytes(buffer)
    }

    /// Reinterprets `self` as `N`
    fn extend_unsigned<N: Num>(&self) -> N
    where
        Self: Sized,
        // convoluted way of determining that N::BYTES > Self::BYTES
        [(); -((N::BYTES < Self::BYTES) as isize) as usize + 1]: Sized,
    {
        let mut buffer = [0; N::BYTES];

        buffer[..Self::BYTES].copy_from_slice(&self.to_le_bytes());

        N::from_le_bytes(buffer)
    }

    fn extend_signed<N: Num>(&self) -> N
    where
        Self: Sized,
        // convoluted way of determining that N::BYTES > Self::BYTES
        [(); -((N::BYTES < Self::BYTES) as isize) as usize + 1]: Sized,
    {
        let signed = self.signed();

        let mut buffer = [0; N::BYTES];

        buffer[..Self::BYTES].copy_from_slice(&self.to_le_bytes());

        N::from_le_bytes(buffer)
    }
}

macro_rules! impl_num {
    ($num:ty, $signed:ty, $actual:ty, $bytes:literal, $value:ident, $value_assert:ident) => {
        impl Num for $num {
            const BYTES: usize = $bytes;
            type Signed = $signed;

            fn from_le_bytes(bytes: [u8; Self::BYTES]) -> Self {
                <$num>::from_le_bytes(bytes)
            }

            fn to_le_bytes(self) -> [u8; Self::BYTES] {
                self.to_le_bytes()
            }

            fn as_value(self) -> Value {
                Value::$value(self as $actual)
            }

            fn from_value(val: Value) -> WResult<Self> {
                Ok(val.$value_assert()? as $num)
            }

            fn signed(self) -> Self::Signed {
                self as $signed
            }
        }
    };
}

impl_num!(i8, i8, i32, 1, I32, assert_i32);
impl_num!(u8, i8, i32, 1, I32, assert_i32);
impl_num!(i16, i16, i32, 2, I32, assert_i32);
impl_num!(u16, i16, i32, 2, I32, assert_i32);

impl_num!(i32, i32, i32, 4, I32, assert_i32);
impl_num!(u32, i32, i32, 4, I32, assert_i32);
impl_num!(f32, f32, f32, 4, F32, assert_f32);
impl_num!(i64, i64, i64, 8, I64, assert_i64);
impl_num!(u64, i64, i64, 8, I64, assert_i64);
impl_num!(f64, f64, f64, 8, F64, assert_f64);
