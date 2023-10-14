use thiserror::Error;

#[derive(Error, Debug)]
pub enum RunTimeError {
    #[error("Out of Menory")]
    OutOfMemory,
    #[error("Program Paniced: {}", .0)]
    Panic(String),
    #[error("Bad Op Code in Program: {}", .0)]
    BadOpCode(u8),
}
