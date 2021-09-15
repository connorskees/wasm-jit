use std::{fmt, str::Utf8Error};

#[derive(Debug)]
pub enum WasmError {
    UnexpectedEof { pos: usize },
    UnrecognizedSection { id: u8, pos: usize },
    InvalidMagicHeader,
    InvalidVersion,
    InvalidValueType { n: u8 },
    InvalidRefType { n: u8 },
    ExpectedByte { expected: u8, found: u8 },
    Utf8Error(Utf8Error),
    InvalidImportDescription { n: u8 },
    InvalidExportDescription { n: u8 },
    InvalidDataMode { n: u8 },
    InvalidLimitFlag { n: u8 },
    InvalidGlobalMutability { n: u8 },
    InvalidOpCode { op: u16 },
    InvalidNumberOfArgs,
    InvalidType,
    StackUnderflow,
    TooFewLocals,
    UndefinedExport,
}

impl fmt::Display for WasmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl From<Utf8Error> for WasmError {
    fn from(err: Utf8Error) -> Self {
        Self::Utf8Error(err)
    }
}

impl std::error::Error for WasmError {}

pub type WResult<T> = Result<T, WasmError>;
