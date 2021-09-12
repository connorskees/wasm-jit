use crate::{
    error::{WResult, WasmError},
    Value,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Stack(Vec<StackEntry>);

impl Stack {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, entry: StackEntry) {
        self.0.push(entry);
    }

    pub fn push_value(&mut self, value: Value) {
        self.push(StackEntry::Value(value));
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn pop(&mut self) -> Option<StackEntry> {
        self.0.pop()
    }

    pub fn pop_value(&mut self) -> WResult<Value> {
        self.pop().ok_or(WasmError::StackUnderflow)?.assert_value()
    }

    pub fn pop_i32(&mut self) -> WResult<i32> {
        self.pop_value()?.assert_i32()
    }

    pub fn pop_n(&mut self, n: usize) -> Option<Vec<StackEntry>> {
        if self.len() < n {
            return None;
        }

        Some(self.0.split_off(self.len() - n))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StackEntry {
    /// The operand of instructions
    Value(Value),

    Label(u32),
    Activation(Vec<Frame>),
}

impl StackEntry {
    pub fn assert_value(self) -> WResult<Value> {
        match self {
            Self::Value(v) => Ok(v),
            _ => Err(WasmError::InvalidType),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Frame {
    pub(crate) locals: Vec<Value>,
    pub(crate) module_idx: u32,
}
