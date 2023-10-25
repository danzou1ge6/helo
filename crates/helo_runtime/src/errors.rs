use thiserror::Error;

#[derive(Error, Debug)]
pub enum RunTimeError {
    #[error("Out of Menory")]
    OutOfMemory,
    #[error("Program Paniced: {}", .0)]
    Panic(String),
    #[error("Bad Op Code in Program: {}", .0)]
    BadOpCode(u8),
    #[error("Zero Division")]
    ZeroDivision,
    #[error("Exponent Out of Range: Should be a u32, got {}", .0)]
    IntExponentOutOfRange(i64),
    #[error("Exponent Out of Range: Should be a i32, got {}", .0)]
    FloatExponentOutOfRange(i64),
    #[error("Called `string_head` or `string_tail` on an empty string")]
    EmptyString,
}
