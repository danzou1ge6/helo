pub enum RunTimeError {
    OutOfMemory,
    Panic(String),
    BadOpCode(u8),
}
